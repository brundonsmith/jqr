use std::{cmp::Ordering, iter::Map, ops::Range};
use serde_json::{Number, Value};

#[derive(Debug,Clone,PartialEq)]
pub enum Filter<'a> {

    // basics
    Identity,
    ObjectIdentifierIndex { identifier: &'a str, optional: bool },
    ArrayIndex { index: usize },
    Slice { start: Option<usize>, end: Option<usize> },
    AllValues,
    Literal(Value),

    // combinators
    Comma(Vec<Filter<'a>>),
    Pipe(Vec<Filter<'a>>),

    // operators
    Add { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    Subtract { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    Multiply { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    Divide { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    Modulo { left: Box<Filter<'a>>, right: Box<Filter<'a>> },

    // comparison
    Equal { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    NotEqual  { left: Box<Filter<'a>>, right: Box<Filter<'a>> },

    LessThan { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    LessThanOrEqual { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    GreaterThan { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    GreaterThanOrEqual { left: Box<Filter<'a>>, right: Box<Filter<'a>> },

    And { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    Or { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    Not,

    // functions
    Length,
    Keys,
    KeysUnsorted,
    Map(Box<Filter<'a>>),
    Select(Box<Filter<'a>>),
}


pub fn apply_filter<'a>(filter: &'a Filter<'a>, values: impl 'a + Iterator<Item=Value>) -> Box<dyn 'a + Iterator<Item=Value>> {
    match filter {
        Filter::Identity => Box::new(values),
        Filter::ObjectIdentifierIndex { identifier, optional } => 
            Box::new(values.map(move |val| -> Value {
                if let Value::Object(contents) = val {
                    return contents.get(*identifier).unwrap().clone();
                } else if *optional && val == Value::Null {
                    return Value::Null;
                } else {
                    panic!(format!("Error: Object identifier index can only be used on values of type Object; got {}", val));
                }
            })),
        Filter::ArrayIndex { index } => 
            Box::new(values.map(move |val| -> Value {
                if let Value::Array(contents) = val {
                    return contents[*index].clone();
                } else {
                    panic!(format!("Error: Array index can only be used on values of type Array; got {}", val));
                }
        })),
        Filter::Slice { start, end } => Box::new(values.map(move |val| -> Box<dyn 'a + Iterator<Item=Value>> {
                if let Some(s) = start {
                    if let Some(e) = end {
                        match val {
                            Value::Array(contents) => Box::new(std::iter::once(Value::Array(contents.into_iter().skip(*s).take(*e - *s).collect()))),
                            Value::String(string) => Box::new(std::iter::once(Value::String(String::from(&string[*s..*e])))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    } else {
                        match val {
                            Value::Array(contents) => Box::new(std::iter::once(Value::Array(contents.into_iter().skip(*s).collect()))),
                            Value::String(string) => Box::new(std::iter::once(Value::String(String::from(&string[*s..])))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    }
                } else {
                    if let Some(e) = end {
                        match val {
                            Value::Array(contents) => Box::new(std::iter::once(Value::Array(contents.into_iter().take(*e).collect()))),
                            Value::String(string) => Box::new(std::iter::once(Value::String(String::from(&string[..*e])))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    } else {
                        match val {
                            Value::Array(contents) => Box::new(std::iter::once(Value::Array(contents.clone()))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    }
                }
            }).flatten()),
        Filter::AllValues => Box::new(values.map(move |val| -> Box<dyn 'a + Iterator<Item=Value>> {
                match val {
                    Value::Object(contents) => Box::new(contents.into_iter().map(|(_, v)| v)),
                    Value::Array(contents) => Box::new(contents.into_iter()),
                    _ => panic!(format!("Cannot get values of {}", val)),
                }
            }).flatten()),
        Filter::Literal(v) => Box::new(std::iter::once(v.clone())),

        Filter::Comma(filters) => {
            let vals = values.collect::<Vec<Value>>();

            Box::new(filters.iter()
                .map(move |f| apply_filter(f, vals.clone().into_iter()))
                .flatten())
        },
        Filter::Pipe(filters) => 
            Box::new(values.map(move |val| -> Box<dyn 'a + Iterator<Item=Value>> {
                let mut result: Box<dyn 'a + Iterator<Item=Value>> = Box::new(std::iter::once(val));

                for f in filters {
                    result = apply_filter(f, result);
                }

                return result;
            }).flatten()),

        Filter::Add { left, right } => applied_to_combinations(values, left, right, &add),
        Filter::Subtract { left, right } => applied_to_combinations(values, left, right, &subtract),
        Filter::Multiply { left, right } => applied_to_combinations(values, left, right, &multiply),
        Filter::Divide { left, right } => applied_to_combinations(values, left, right, &divide),
        Filter::Modulo { left, right } => applied_to_combinations(values, left, right, &modulo),

        Filter::Equal { left, right } => applied_to_combinations(values, left, right, &equal),
        Filter::NotEqual { left, right } => applied_to_combinations(values, left, right, &not_equal),

        Filter::LessThan { left, right } => applied_to_combinations(values, left, right, &less_than),
        Filter::LessThanOrEqual { left, right } => applied_to_combinations(values, left, right, &less_than_or_equal),
        Filter::GreaterThan { left, right } => applied_to_combinations(values, left, right, &greater_than),
        Filter::GreaterThanOrEqual { left, right } => applied_to_combinations(values, left, right, &greater_than_or_equal),

        Filter::And { left, right } => applied_to_combinations(values, left, right, &and),
        Filter::Or { left, right } => applied_to_combinations(values, left, right, &or),
        Filter::Not => Box::new(values.map(move |val| -> Value {
            if let Value::Bool(v) = val {
                return Value::Bool(!v);
            } else {
                panic!(format!("Error: 'not' can only be used on values of type Boolean; got '{}'", val));
            }
        })),

        Filter::Length => Box::new(values.map(move |val| {
            match val {
                Value::Array(x) => Value::Number(Number::from(x.len())),
                Value::String(x) => Value::Number(Number::from(x.len())),
                Value::Null => Value::Number(Number::from(0)),
                Value::Object(x) => Value::Number(Number::from(x.keys().len())),
                _ => panic!(format!("Cannot get the length of {}", val)),
            }
        })),
        Filter::Keys => {
            let mut unsorted_keys = values.map(keys).flatten().collect::<Vec<Value>>();

            unsorted_keys.sort_by(cmp);

            return Box::new(unsorted_keys.into_iter());
        },
        Filter::KeysUnsorted => Box::new(values.map(keys).flatten()),
        Filter::Map(pred) => Box::new(values.map(move |val| {
            match val {
                Value::Array(arr) => 
                    Value::Array(arr.into_iter().map(|v| apply_filter(pred, std::iter::once(v))).flatten().collect()),
                _ => panic!(format!("Cannot map over value {}", val))
            }
        })),
        Filter::Select(pred) => Box::new(values.filter(move |val| {
            let mut inner_vals = apply_filter(pred, std::iter::once(val.clone()));

            inner_vals.all(|v| v == Value::Bool(true))
        }))
    }
}

fn cmp(a: &Value, b: &Value) -> Ordering {
    let cmp_key_a = type_cmp_key(a);
    let cmp_key_b = type_cmp_key(b);
    if cmp_key_a != cmp_key_b {
        return cmp_key_a.cmp(&cmp_key_b);
    } else if let Value::String(a) = a {
        if let Value::String(b) = b {
            return a.cmp(b);
        }
    } else if let Value::Number(a) = a {
        if let Value::Number(b) = b {
            return a.as_f64().partial_cmp(&b.as_f64()).unwrap_or(Ordering::Equal);
        }
    } else if let Value::Array(a) = a {
        if let Value::Array(b) = b {
            todo!()
        }
    } else if let Value::Object(a) = a {
        if let Value::Object(b) = b {
            todo!()
        }
    }

    Ordering::Equal
}

fn type_cmp_key(val: &Value) -> u8 {
    match val {
        Value::Object(_) => 6,
        Value::Array(_) => 5,
        Value::String(_) => 4,
        Value::Number(_) => 3,
        Value::Bool(true) => 2,
        Value::Bool(false) => 1,
        Value::Null => 0,
    }
}

fn combinations<'a>(a: impl Iterator<Item=Value>, b: impl Iterator<Item=Value>) -> impl Iterator<Item=(Value,Value)> {
    let vec_a = a.collect::<Vec<Value>>();
    let vec_b = b.collect::<Vec<Value>>();

    let mut index_a = 0;
    let mut index_b = 0;

    return std::iter::from_fn(move || {
        if index_a < vec_a.len() && index_b < vec_b.len() {
            let next = (vec_a[index_a].clone(), vec_b[index_b].clone());

            if index_b < vec_b.len() - 1 {
                index_b += 1;
            } else {
                index_a += 1;
                index_b = 0;
            }

            return Some(next);
        } else {
            return None;
        }
    });
}

fn applied_to_combinations<'a>(values: impl 'a + Iterator<Item=Value>, left: &'a Filter<'a>, right: &'a Filter<'a>, func: &'static impl Fn((Value, Value)) -> Value) -> Box<dyn 'a + Iterator<Item=Value>> {
    Box::new(values.map(move |val| {
        let left_vals = apply_filter(left, std::iter::once(val.clone()));
        let right_vals = apply_filter(right, std::iter::once(val));

        combinations(left_vals, right_vals).map(func)
    }).flatten())
}



enum KeysIterator {
    Object(Map<serde_json::map::IntoIter, fn((String, Value)) -> Value>),
    Array(Map<Range<usize>, fn(usize) -> Value>),
}

impl Iterator for KeysIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            KeysIterator::Object(i) => i.next(),
            KeysIterator::Array(i) => i.next(),
        }
    }
}

fn keys(val: Value) -> KeysIterator {
    match val {
        Value::Object(map) => 
            KeysIterator::Object(map.into_iter().map(|(key, _)| Value::String(key))),
        Value::Array(arr) => 
            KeysIterator::Array((0..arr.len()).map(|i| Value::Number(Number::from(i)))),
        _ => panic!(format!("Cannot get keys from value {}", val))
    }
}

enum NumberPair {
    F64(f64, f64),
    I64(i64, i64),
    U64(u64, u64),
}

impl NumberPair {
    pub fn from_values(a: &Value, b: &Value) -> Option<NumberPair> {
        if let Value::Number(a) = a {
            if let Value::Number(b) = b {
                
                if let Some(a) = a.as_i64() {
                    if let Some(b) = b.as_i64() {
                        return Some(NumberPair::I64(a, b));
                    } else if let Some(b) = b.as_u64() {
                        return Some(NumberPair::I64(a, b as i64));
                    } else if let Some(b) = b.as_f64() {
                        return Some(NumberPair::F64(a as f64, b));
                    }
                }
    
                if let Some(a) = a.as_u64() {
                    if let Some(b) = b.as_i64() {
                        return Some(NumberPair::I64(a as i64, b));
                    } else if let Some(b) = b.as_u64() {
                        return Some(NumberPair::U64(a, b));
                    } else if let Some(b) = b.as_f64() {
                        return Some(NumberPair::F64(a as f64, b));
                    }
                }

                if let Some(a) = a.as_f64() {
                    if let Some(b) = b.as_i64() {
                        return Some(NumberPair::F64(a, b as f64));
                    } else if let Some(b) = b.as_u64() {
                        return Some(NumberPair::F64(a, b as f64));
                    } else if let Some(b) = b.as_f64() {
                        return Some(NumberPair::F64(a, b));
                    }
                }
            }
        }

        None
    }
}

macro_rules! numeric_operation {
    ($a:ident, $b:ident, $op:tt) => {
        if let Some(pair) = NumberPair::from_values(&$a, &$b) {
            return Value::Number(match pair {
                NumberPair::F64(a, b) => Number::from_f64(a $op b).unwrap(),
                NumberPair::I64(a, b) => Number::from(a $op b),
                NumberPair::U64(a, b) => Number::from(a $op b),
            })
        }
    };
}

fn add<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    numeric_operation!(a, b, +);
    
    if a == Value::Null {
        return b;
    }

    if b == Value::Null {
        return a;
    }

    if let Value::String(a) = a {
        if let Value::String(b) = b {
            return Value::String(a + &b);
        }

        panic!(format!("Cannot add values {} and {}", Value::String(a), b));
    }
    
    if let Value::Array(mut a) = a {
        if let Value::Array(b) = b {
            for v in b {
                a.push(v);
            }
            return Value::Array(a);
        }

        panic!(format!("Cannot add values {} and {}", Value::Array(a), b));
    }

    // if let Value::Object(a) = a.clone() {
    //     if let Value::Object(b) = b {
    //         let mut res = a;
    //         for (key, value) in b {
    //             res.insert(key, value);
    //         }
    //         return Value::Object(res);
    //     }
    // }

    panic!(format!("Cannot add values {} and {}", a, b));
}

fn subtract<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    numeric_operation!(a, b, -);

    if let Value::String(a) = a {
        if let Value::String(b) = b {
            todo!()
            // return Value::MultiSliceString(new_str.replace(b, ""));
        }
        if let Value::String(b) = b {
            todo!()
            // return Value::MultiSliceString(new_str.replace(b.as_str(), ""));
        }

        panic!(format!("Cannot subtract values {} and {}", Value::String(a), b));
    } else if let Value::Array(a) = a {
        if let Value::Array(b) = b {
            return Value::Array(
                a.iter()
                    .filter(|v| b.iter().any(|other| other == *v))
                    .cloned()
                    .collect()
            );
        }

        panic!(format!("Cannot subtract values {} and {}", Value::Array(a), b));
    }

    panic!(format!("Cannot subtract values {} and {}", a, b));
}

fn multiply<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    numeric_operation!(a, b, *);

    panic!(format!("Cannot multiply values {} and {}", a, b));
}

fn divide<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    numeric_operation!(a, b, /);

    panic!(format!("Cannot divide values {} and {}", a, b));
}

fn modulo<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    numeric_operation!(a, b, %);

    panic!(format!("Cannot get the modulus of values {} and {}", a, b));
}

fn equal<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    Value::Bool(a == b)
}

fn not_equal<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    Value::Bool(a != b)
}

fn less_than<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    let ord = cmp(&a, &b);
    Value::Bool(ord == Ordering::Less)
}

fn less_than_or_equal<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    let ord = cmp(&a, &b);
    Value::Bool(ord == Ordering::Less || ord == Ordering::Equal)
}

fn greater_than<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    let ord = cmp(&a, &b);
    Value::Bool(ord == Ordering::Greater)
}

fn greater_than_or_equal<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    let ord = cmp(&a, &b);
    Value::Bool(ord == Ordering::Greater || ord == Ordering::Equal)
}

fn and<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    if let Value::Bool(a) = a {
        if let Value::Bool(b) = b {
            return Value::Bool(a && b);
        }
    }

    panic!(format!("Cannot apply 'and' to values {} and {}", a, b));
}

fn or<'a>(vals: (Value, Value)) -> Value {
    let (a, b) = vals;

    if let Value::Bool(a) = a {
        if let Value::Bool(b) = b {
            return Value::Bool(a || b);
        }
    }

    panic!(format!("Cannot apply 'or' to values {} and {}", a, b));
}


#[cfg(test)]
mod tests {

        use serde_json::Value;

use crate::{filter_parser, json_parser};

    use super::apply_filter;

    #[test]
    fn test_1() {
        test_filter(
            "{ \"foo\": 12, \"bar\": \"hello\" }",
            ".foo",
            "12"
        );
    }
    
    #[test]
    fn test_2() {
        test_filter(
            "{ 
                \"foo\": 12, 
                \"bar\": {
                    \"stuff\": false
                }
            }",
            ".bar.stuff",
            "false"
        );
    }

    #[test]
    fn test_3() {
        test_filter(
            "{ 
                \"foo\": 12, 
                \"bar\": [ 0, 1, 2 ]
            }",
            ".bar.[1]",
            "1"
        );
    }
    
    #[test]
    fn test_4() {
        test_filter(
            "{ 
                \"foo\": 12, 
                \"bar\": [ 0, 1, 2 ]
            }",
            ".bar.[]",
            "0 1 2"
        );
    }
    
    #[test]
    fn test_5() {
        test_filter(
            "{ 
                \"foo\": 12, 
                \"bar\": [ 0, 1, 2 ]
            }",
            ".bar[1:]",
            "[1, 2]"
        );
    }
    
    #[test]
    fn test_6() {
        test_filter(
            "{ 
                \"foo\": 12, 
                \"bar\": [ 0, 1, 2, 3, 4, 5, 6 ]
            }",
            ".bar[2:4]",
            "[2, 3]"
        );
    }
    
    #[test]
    fn test_7() {
        test_filter(
            "{ 
                \"foo\": 12, 
                \"bar\": [ 0, 1, 2, 3, 4, 5, 6 ]
            }",
            ".bar[:4]",
            "[0, 1, 2, 3]"
        );
    }
    
    #[test]
    fn test_8() {
        test_filter(
            "{ 
                \"foo\": 12, 
                \"bar\": [ 0, 1, 2, 3, 4, 5, 6 ]
            }",
            ".bar[4]",
            "4"
        );
    }

    #[test]
    fn test_9() {
        test_filter(
            "\"abcdefghi\"",
            ".[2:4]",
            "\"cd\""
        );
    }

    #[test]
    fn test_10() {
        test_filter(
            "[
                { \"foo\": 1 },
                { \"foo\": 2 },
                { \"foo\": 3 }
            ]",
            "map(.foo)",
            "[1, 2, 3]"
        );
    }

    #[test]
    fn test_11() {
        test_filter(
            "[1, 2, 3]",
            "map(.+1)",
            "[2, 3, 4]"
        );
    }

    #[test]
    fn test_12() {
        test_filter(
            "[\"a\", \"b\", \"c\"]",
            "map(. + \"d\")",
            "[\"ad\", \"bd\", \"cd\"]"
        );
    }

    fn test_filter(input_json: &str, filter: &str, output_json: &str) {
        let input = json_parser::parse(input_json).map(|r| r.unwrap());
        let filter = filter_parser::parse(filter).unwrap();
        let expected = json_parser::parse(output_json).map(|r| r.unwrap());

        apply_filter(&filter, input).zip(expected).for_each(|(i, o)| {
            assert_eq!(i, o)
        });
    }
}
