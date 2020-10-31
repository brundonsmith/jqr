use std::{cmp::Ordering, rc::Rc};

use crate::json_model::{JSONValue, apply_escapes};

#[derive(Debug,Clone,PartialEq)]
pub enum Filter<'a> {

    // basics
    Identity,
    ObjectIdentifierIndex { identifier: &'a str, optional: bool },
    ArrayIndex { index: usize },
    Slice { start: Option<usize>, end: Option<usize> },
    AllValues,
    Literal(JSONValue<'a>),

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

    // _MapSelect(Box<Filter<'a>>),
    // _PropertyChain(Vec<(&'a str, bool)>),
}


pub fn apply_filter<'a>(filter: &'a Filter<'a>, values: impl 'a + Iterator<Item=JSONValue<'a>>) -> Box<dyn 'a + Iterator<Item=JSONValue<'a>>> {
    match filter {
        Filter::Identity => Box::new(values),
        Filter::ObjectIdentifierIndex { identifier, optional } => 
            Box::new(values.map(move |val| -> JSONValue {
                if let JSONValue::Object(contents) = val {
                    contents.as_ref().0.get(&JSONValue::String { s: identifier, needs_escaping: false }).unwrap().clone()
                } else if val == JSONValue::Null && *optional {
                    JSONValue::Null
                } else {
                    panic!(format!("Error: Object identifier index can only be used on values of type Object; got {}", val))
                }
            })),
        Filter::ArrayIndex { index } => 
            Box::new(values.map(move |val| -> JSONValue {
                if let JSONValue::Array(contents) = val {
                    contents[*index].clone()
                } else {
                    panic!(format!("Error: Array index can only be used on values of type Array; got {}", val))
                }
            })),
        Filter::Slice { start, end } => 
            Box::new(values.map(move |val| -> Box<dyn 'a + Iterator<Item=JSONValue>> {
                Box::new(std::iter::once(
                    match val {
                        JSONValue::Array(contents) => {
                            if let Some(s) = start {
                                if let Some(e) = end {
                                    JSONValue::Array(Rc::new(contents.iter().skip(*s).take(*e - *s).map(|v| v.clone()).collect()))
                                } else {
                                    JSONValue::Array(Rc::new(contents.iter().skip(*s).map(|v| v.clone()).collect()))
                                }
                            } else {
                                if let Some(e) = end {
                                    JSONValue::Array(Rc::new(contents.iter().take(*e).map(|v| v.clone()).collect()))
                                } else {
                                    JSONValue::Array(contents.clone())
                                }
                            }
                        },
                        JSONValue::String { s: string, needs_escaping } => {
                            if needs_escaping {
                                let string = apply_escapes(string);

                                JSONValue::AllocatedString(Rc::new(
                                    slice(&string, start, end)
                                        .expect(&format!("Cannot get values of {}", val))
                                ))
                            } else {
                                JSONValue::AllocatedString(Rc::new(
                                    slice(&string, start, end)
                                        .expect(&format!("Cannot get values of {}", val))
                                ))
                            }
                        },
                        _ => panic!(format!("Cannot get values of {}", val)),
                    }
                ))
            }).flatten()),
        Filter::AllValues => 
            Box::new(values.map(move |val| -> Box<dyn 'a + Iterator<Item=JSONValue<'a>>> {
                let vals: Vec<JSONValue> = match val {
                    JSONValue::Object(contents) => contents.as_ref().0.values().cloned().collect(),
                    JSONValue::Array(contents) => contents.iter().cloned().collect(),
                    _ => panic!(format!("Cannot get values of {}", val)),
                };

                Box::new(vals.into_iter())
            }).flatten()),
        Filter::Literal(v) => Box::new(std::iter::once(v.clone())),

        Filter::Comma(filters) => {
            let vals = values.collect::<Vec<JSONValue<'a>>>();

            Box::new(filters.iter()
                .map(move |f| {
                    let vals: Vec<JSONValue> = vals.iter().cloned().collect();

                    apply_filter(f, vals.into_iter())
                })
                .flatten())
        },
        Filter::Pipe(filters) => 
            Box::new(values.map(move |val| {
                let mut result: Box<dyn 'a + Iterator<Item=JSONValue<'a>>> = Box::new(std::iter::once(val));

                for f in filters {
                    result = apply_filter(f, result);
                }

                return result;
            }).flatten()),

        Filter::Add { left, right } => applied_to_combinations(values, left, right, add),
        Filter::Subtract { left, right } => applied_to_combinations(values, left, right, subtract),
        Filter::Multiply { left, right } => applied_to_combinations(values, left, right, multiply),
        Filter::Divide { left, right } => applied_to_combinations(values, left, right, divide),
        Filter::Modulo { left, right } => applied_to_combinations(values, left, right, modulo),

        Filter::Equal { left, right } => applied_to_combinations(values, left, right, equal),
        Filter::NotEqual { left, right } => applied_to_combinations(values, left, right, not_equal),

        Filter::LessThan { left, right } => applied_to_combinations(values, left, right, less_than),
        Filter::LessThanOrEqual { left, right } => applied_to_combinations(values, left, right, less_than_or_equal),
        Filter::GreaterThan { left, right } => applied_to_combinations(values, left, right, greater_than),
        Filter::GreaterThanOrEqual { left, right } => applied_to_combinations(values, left, right, greater_than_or_equal),

        Filter::And { left, right } => applied_to_combinations(values, left, right, and),
        Filter::Or { left, right } => applied_to_combinations(values, left, right, or),
        Filter::Not => Box::new(values.map(move |val| -> JSONValue<'a> {
            if let JSONValue::Bool(v) = val {
                return JSONValue::Bool(!v);
            } else {
                panic!(format!("Error: 'not' can only be used on values of type Boolean; got '{}'", val));
            }
        })),

        Filter::Length => Box::new(values.map(move |val| -> JSONValue<'a> {
            JSONValue::Integer(
                match val {
                    JSONValue::Array(x) => x.len(),
                    JSONValue::String { s, needs_escaping } => {
                        if needs_escaping {
                            apply_escapes(s).chars().count() // TODO: Optimize by making non-allocating escaped length function?
                        } else {
                            s.chars().count()
                        }
                    },
                    JSONValue::Null => 0,
                    JSONValue::Object(x) => x.as_ref().0.keys().len(),
                    _ => panic!(format!("Cannot get the length of {}", val)),
                } as i32
            )
        })),
        Filter::Keys => Box::new(values.map(move |val| {
            let mut keys = keys(val);

            keys.sort();

            JSONValue::Array(Rc::new(keys))
        })),
        Filter::KeysUnsorted => Box::new(values.map(keys).flatten()),
        Filter::Map(pred) => Box::new(values.map(move |val| -> JSONValue {
            match val {
                JSONValue::Array(arr) => 
                    JSONValue::Array(Rc::new(
                        arr.iter()
                            .map(|v| apply_filter(pred, std::iter::once(v.clone())))
                            .flatten()
                            .collect())),
                _ => panic!(format!("Cannot map over value {}", val))
            }
        })),
        Filter::Select(pred) => Box::new(values.filter(move |val|
            apply_filter(pred, std::iter::once(val.clone()))
                .all(|v| v == JSONValue::Bool(true))
        )),

        // Filter::_PropertyChain(props) => Box::new(values.map(move |val| -> JSONValue {
        //     let mut next = val.as_ref();

        //     for (identifier, optional) in props {
        //         if let JSONValue::Object(contents) = next {
        //             next = contents.get(*identifier).unwrap();
        //         } else if *optional && next == &JSONValue::Null {
        //             return Rc::new(JSONValue::Null);
        //         } else {
        //             panic!(format!("Error: Object identifier index can only be used on values of type Object; got {}", next))
        //         }
        //     }

        //     Rc::new(next.clone())
        // })),
    }
}

fn slice(string: &str, start: &Option<usize>, end: &Option<usize>) -> Result<String, ()> {
    if let Some(s) = start {
        if let Some(e) = end {
            Ok(string.chars().skip(*s).take(*e - *s).collect())
        } else {
            Ok(string.chars().skip(*s).collect())
        }
    } else {
        if let Some(e) = end {
            Ok(string.chars().take(*e).collect())
        } else {
            Err(())
        }
    }
}

fn applied_to_combinations<'a>(values: impl 'a + Iterator<Item=JSONValue<'a>>, left: &'a Filter<'a>, right: &'a Filter<'a>, func: fn((JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a>) -> Box<dyn 'a + Iterator<Item=JSONValue<'a>>> {
    Box::new(values.map(move |val| {
        let left_vals = apply_filter(left, std::iter::once(val.clone()));
        let right_vals = apply_filter(right, std::iter::once(val.clone()));

        combinations(left_vals, right_vals).map(func)
    }).flatten())
}

fn combinations<'a>(a: impl Iterator<Item=JSONValue<'a>>, b: impl Iterator<Item=JSONValue<'a>>) -> impl Iterator<Item=(JSONValue<'a>, JSONValue<'a>)> {
    let vec_a = a.collect::<Vec<JSONValue>>();
    let vec_b = b.collect::<Vec<JSONValue>>();

    let mut index_a = 0;
    let mut index_b = 0;

    std::iter::from_fn(move || {
        if index_a < vec_a.len() && index_b < vec_b.len() {
            let next = (vec_a[index_a].clone(), vec_b[index_b].clone());

            if index_b < vec_b.len() - 1 {
                index_b += 1;
            } else {
                index_a += 1;
                index_b = 0;
            }

            Some(next)
        } else {
            None
        }
    })
}

fn keys<'a>(val: JSONValue<'a>) -> Vec<JSONValue<'a>> {
    match val {
        JSONValue::Object(map) => {
            map.as_ref().0.keys().cloned().collect()
        },
        JSONValue::Array(arr) => 
            (0..arr.len()).map(|i| JSONValue::Integer(i as i32)).collect(),
        _ => panic!(format!("Cannot get keys from value {}", val))
    }
}

enum NumberPair {
    Floats(f32, f32),
    Ints(i32, i32),
}

impl NumberPair {
    pub fn from_values(a: &JSONValue, b: &JSONValue) -> Option<NumberPair> {
        match a {
            JSONValue::Float(a) => {
                match b {
                    JSONValue::Float(b) => Some(NumberPair::Floats(*a, *b)),
                    JSONValue::Integer(b) => Some(NumberPair::Floats(*a, *b as f32)),
                    _ => None,
                }
            },
            JSONValue::Integer(a) => {
                match b {
                    JSONValue::Float(b) => Some(NumberPair::Floats(*a as f32, *b)),
                    JSONValue::Integer(b) => Some(NumberPair::Ints(*a, *b)),
                    _ => None,
                }
            },

            _ => None,
        }
    }
}

macro_rules! numeric_operation {
    ($a:ident, $b:ident, $op:tt) => {
        if let Some(pair) = NumberPair::from_values(&$a, &$b) {
            return match pair {
                NumberPair::Floats(a, b) => JSONValue::Float(a $op b),
                NumberPair::Ints(a, b) => JSONValue::Integer(a $op b),
            }
        }
    };
}

fn add<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    numeric_operation!(a, b, +);
    
    if a == JSONValue::Null {
        return b.clone();
    }

    if b == JSONValue::Null {
        return a.clone();
    }

    if let JSONValue::String { s: a, needs_escaping: _ } = a {
        if let JSONValue::String { s: b, needs_escaping: _ } = b {
            return JSONValue::AllocatedString(Rc::new(String::from(a) + b));
        }

        panic!(format!("Cannot add values {} and {}", JSONValue::String { s: a.clone(), needs_escaping: false }, b));
    }
    
    if let JSONValue::Array(a) = a {
        if let JSONValue::Array(b) = b {
            let mut result = a.as_ref().clone();
            for v in b.iter() {
                result.push(v.clone());
            }
            return JSONValue::Array(Rc::new(result));
        }

        panic!(format!("Cannot add values {} and {}", JSONValue::Array(a.clone()), b));
    }

    if let JSONValue::Object(a) = &a {
        if let JSONValue::Object(b) = &b {
            return JSONValue::Object(Rc::new((
                a.as_ref().0.iter().chain(b.as_ref().0.iter())
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect()
            , None)));
        }
    }

    panic!(format!("Cannot add values {} and {}", a, b));
}

fn subtract<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    numeric_operation!(a, b, -);
    
    if let JSONValue::Array(a) = &a {
        if let JSONValue::Array(b) = b {
            return JSONValue::Array(Rc::new(
                a.as_ref().iter()
                    .filter(|v| !b.as_ref().iter().any(|other| other == *v))
                    .cloned()
                    .collect()
            ));
        }
    }

    panic!(format!("Cannot subtract values {} and {}", a, b));
}

fn multiply<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    numeric_operation!(a, b, *);

    if let JSONValue::String { s: a, needs_escaping: _ } = &a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::AllocatedString(Rc::new(repeated_str(a, b)));
        }
    } else if let JSONValue::AllocatedString(a) = &a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::AllocatedString(Rc::new(repeated_str(a, b)));
        }
    } else if let JSONValue::Integer(a) = &a {
        if let JSONValue::String { s: b, needs_escaping: _ } = b {
            return JSONValue::AllocatedString(Rc::new(repeated_str(b, *a)));
        } else if let JSONValue::AllocatedString(b) = b {
            return JSONValue::AllocatedString(Rc::new(repeated_str(&b, *a)));
        }
    }

    panic!(format!("Cannot multiply values {} and {}", a, b));
}

fn repeated_str(s: &str, n: i32) -> String {
    let mut res = String::with_capacity(s.len() * n as usize);

    for _ in 0..n {
        res.push_str(s);
    }

    return res;
}

fn divide<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    numeric_operation!(a, b, /);

    panic!(format!("Cannot divide values {} and {}", a, b));
}

fn modulo<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    numeric_operation!(a, b, %);

    panic!(format!("Cannot get the modulus of values {} and {}", a, b));
}

fn equal<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    JSONValue::Bool(a == b)
}

fn not_equal<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    JSONValue::Bool(a != b)
}

fn less_than<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    let ord = a.cmp(&b);
    JSONValue::Bool(ord == Ordering::Less)
}

fn less_than_or_equal<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    let ord = a.cmp(&b);
    JSONValue::Bool(ord == Ordering::Less || ord == Ordering::Equal)
}

fn greater_than<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    let ord = a.cmp(&b);
    JSONValue::Bool(ord == Ordering::Greater)
}

fn greater_than_or_equal<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    let ord = a.cmp(&b);
    JSONValue::Bool(ord == Ordering::Greater || ord == Ordering::Equal)
}

fn and<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    if let JSONValue::Bool(a) = a {
        if let JSONValue::Bool(b) = b {
            return JSONValue::Bool(a && b);
        }
    }

    panic!(format!("Cannot apply 'and' to values {} and {}", a, b));
}

fn or<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    if let JSONValue::Bool(a) = a {
        if let JSONValue::Bool(b) = b {
            return JSONValue::Bool(a || b);
        }
    }

    panic!(format!("Cannot apply 'or' to values {} and {}", a, b));
}


#[cfg(test)]
mod tests {

    use std::rc::Rc;
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

    #[test]
    fn test_13() {
        test_filter(
            "{ 
                \"foo\": 12, 
                \"bar\": [ 0, 1, 2 ]
            }",
            "keys",
            "[ \"bar\", \"foo\" ]"
        );
    }

    #[test]
    fn test_14() {
        test_filter(
            "\"\\u0001hello world\"", 
            ".[1:]", 
            "\"hello world\""
        );
    }

    #[test]
    fn test_15() {
        test_filter("3", ". + 5 * 2", "13")
    }

    #[test]
    fn test_16() {
        test_filter("5", "10 / . * 3", "6")
    }

    #[test]
    fn test_17() {
        test_filter(
            "
                { \"foo\": 1 }
                { \"foo\": 2 }
                { \"foo\": 3 }
            ",
            "keys",
            "
                [ \"foo\" ]
                [ \"foo\" ]
                [ \"foo\" ]
            "
        );
    }

    #[test]
    fn test_18() {
        test_filter(
            "{
                \"a\": { \"foo\": 12 },
                \"b\": { \"bar\": 13 } 
            }",
            ".a + .b",
            "{
                \"foo\": 12,
                \"bar\": 13
            }")
    }

    #[test]
    fn test_19() {
        test_filter("[ 5, 6, 7, 8 ]", ". - .[2:]", "[ 5, 6 ]")
    }

    #[test]
    fn test_20() {
        test_filter("\"foo\"", "3 * .", "\"foofoofoo\"")
    }

    fn test_filter(input_json: &str, filter: &str, output_json: &str) {
        let input = json_parser::parse(input_json, false).map(|r| r.unwrap());
        let filter = filter_parser::parse(filter).unwrap();
        println!("{:?}", filter);
        let expected = json_parser::parse(output_json, false).map(|r| r.unwrap());
        apply_filter(&filter, input).zip(expected).for_each(|(r, e)| {
            assert_eq!(r, e)
        });
    }
}
