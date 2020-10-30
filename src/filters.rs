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
    Literal(Rc<JSONValue<'a>>),

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


pub fn apply_filter<'a>(filter: &'a Filter<'a>, values: impl 'a + Iterator<Item=Rc<JSONValue<'a>>>) -> Box<dyn 'a + Iterator<Item=Rc<JSONValue<'a>>>> {
    // println!("\n{:?}", filter);
    match filter {
        Filter::Identity => Box::new(values),
        Filter::ObjectIdentifierIndex { identifier, optional } => 
            Box::new(values.map(move |val| -> Rc<JSONValue> {
                if let JSONValue::Object(contents) = val.as_ref() {
                    contents.get(&JSONValue::String { s: identifier, needs_escaping: false }).unwrap().clone()
                } else if val.as_ref() == &JSONValue::Null && *optional {
                    Rc::new(JSONValue::Null)
                } else {
                    panic!(format!("Error: Object identifier index can only be used on values of type Object; got {}", val))
                }
            })),
        Filter::ArrayIndex { index } => 
            Box::new(values.map(move |val| -> Rc<JSONValue> {
                if let JSONValue::Array(contents) = val.as_ref() {
                    contents[*index].clone()
                } else {
                    panic!(format!("Error: Array index can only be used on values of type Array; got {}", val))
                }
            })),
        Filter::Slice { start, end } => 
            Box::new(values.map(move |val| -> Box<dyn 'a + Iterator<Item=Rc<JSONValue>>> {
                Box::new(std::iter::once(
                    match val.as_ref() {
                        JSONValue::Array(contents) => {
                            if let Some(s) = start {
                                if let Some(e) = end {
                                    Rc::new(JSONValue::Array(contents.into_iter().skip(*s).take(*e - *s).cloned().collect()))
                                } else {
                                    Rc::new(JSONValue::Array(contents.into_iter().skip(*s).cloned().collect()))
                                }
                            } else {
                                if let Some(e) = end {
                                    Rc::new(JSONValue::Array(contents.into_iter().take(*e).cloned().collect()))
                                } else {
                                    val.clone()
                                }
                            }
                        },
                        JSONValue::String { s: string, needs_escaping } => {
                            if *needs_escaping {
                                let string = apply_escapes(string);

                                Rc::new(JSONValue::AllocatedString(
                                    slice(&string, start, end)
                                        .expect(&format!("Cannot get values of {}", val))
                                ))
                            } else {
                                Rc::new(JSONValue::AllocatedString(
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
            Box::new(values.map(move |val| -> Box<dyn 'a + Iterator<Item=Rc<JSONValue<'a>>>> {
                let vals: Vec<Rc<JSONValue>> = match val.as_ref() {
                    JSONValue::Object(contents) => contents.values().cloned().collect(),
                    JSONValue::Array(contents) => contents.iter().cloned().collect(),
                    _ => panic!(format!("Cannot get values of {}", val)),
                };

                Box::new(vals.into_iter())
            }).flatten()),
        Filter::Literal(v) => Box::new(std::iter::once(v.clone())),

        Filter::Comma(filters) => {
            let vals = values.collect::<Vec<Rc<JSONValue<'a>>>>();

            Box::new(filters.iter()
                .map(move |f| {
                    let vals: Vec<Rc<JSONValue>> = vals.iter().cloned().collect();

                    apply_filter(f, vals.into_iter())
                })
                .flatten())
        },
        Filter::Pipe(filters) => 
            Box::new(values.map(move |val| {
                let mut result: Box<dyn 'a + Iterator<Item=Rc<JSONValue<'a>>>> = Box::new(std::iter::once(val));

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
        Filter::Not => Box::new(values.map(move |val| -> Rc<JSONValue<'a>> {
            if let JSONValue::Bool(v) = val.as_ref() {
                return Rc::new(JSONValue::Bool(!v));
            } else {
                panic!(format!("Error: 'not' can only be used on values of type Boolean; got '{}'", val));
            }
        })),

        Filter::Length => Box::new(values.map(move |val| -> Rc<JSONValue<'a>> {
            Rc::new(JSONValue::Integer(
                match val.as_ref() {
                    JSONValue::Array(x) => x.len(),
                    JSONValue::String { s, needs_escaping } => {
                        if *needs_escaping {
                            apply_escapes(s).chars().count() // TODO: Optimize by making non-allocating escaped length function?
                        } else {
                            s.chars().count()
                        }
                    },
                    JSONValue::Null => 0,
                    JSONValue::Object(x) => x.keys().len(),
                    _ => panic!(format!("Cannot get the length of {}", val)),
                } as i32
            ))
        })),
        Filter::Keys => {
            let mut unsorted_keys = values.map(keys).flatten().collect::<Vec<Rc<JSONValue<'a>>>>();

            unsorted_keys.sort();

            return Box::new(std::iter::once(Rc::new(JSONValue::Array(unsorted_keys))));
        },
        Filter::KeysUnsorted => Box::new(values.map(keys).flatten()),
        Filter::Map(pred) => Box::new(values.map(move |val| -> Rc<JSONValue> {
            match val.as_ref() {
                JSONValue::Array(arr) => 
                    Rc::new(JSONValue::Array(
                        arr.iter()
                            .map(|v| apply_filter(pred, std::iter::once(v.clone())))
                            .flatten()
                            .collect())),
                _ => panic!(format!("Cannot map over value {}", val))
            }
        })),
        Filter::Select(pred) => Box::new(values.filter(move |val|
            apply_filter(pred, std::iter::once(val.clone()))
                .all(|v| v.as_ref() == &JSONValue::Bool(true))
        )),

        // Filter::_PropertyChain(props) => Box::new(values.map(move |val| -> Rc<JSONValue> {
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

fn applied_to_combinations<'a>(values: impl 'a + Iterator<Item=Rc<JSONValue<'a>>>, left: &'a Filter<'a>, right: &'a Filter<'a>, func: fn((Rc<JSONValue<'a>>, Rc<JSONValue<'a>>)) -> Rc<JSONValue<'a>>) -> Box<dyn 'a + Iterator<Item=Rc<JSONValue<'a>>>> {
    Box::new(values.map(move |val| {
        let left_vals = apply_filter(left, std::iter::once(val.clone()));
        let right_vals = apply_filter(right, std::iter::once(val.clone()));

        combinations(left_vals, right_vals).map(func)
    }).flatten())
}

fn combinations<'a>(a: impl Iterator<Item=Rc<JSONValue<'a>>>, b: impl Iterator<Item=Rc<JSONValue<'a>>>) -> impl Iterator<Item=(Rc<JSONValue<'a>>, Rc<JSONValue<'a>>)> {
    let vec_a = a.collect::<Vec<Rc<JSONValue>>>();
    let vec_b = b.collect::<Vec<Rc<JSONValue>>>();

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

fn keys<'a>(val: Rc<JSONValue<'a>>) -> Vec<Rc<JSONValue<'a>>> {
    match val.as_ref() {
        JSONValue::Object(map) => {
            map.keys().cloned().collect()
        },
        JSONValue::Array(arr) => 
            (0..arr.len()).map(|i| JSONValue::Integer(i as i32)).map(Rc::new).collect(),
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
            return Rc::new(match pair {
                NumberPair::Floats(a, b) => JSONValue::Float(a $op b),
                NumberPair::Ints(a, b) => JSONValue::Integer(a $op b),
            })
        }
    };
}

fn add<'a>(vals: (Rc<JSONValue<'a>>, Rc<JSONValue<'a>>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    numeric_operation!(a, b, +);
    
    if a.as_ref() == &JSONValue::Null {
        return b.clone();
    }

    if b.as_ref() == &JSONValue::Null {
        return a.clone();
    }

    if let JSONValue::String { s: a, needs_escaping: _ } = a.as_ref() {
        if let JSONValue::String { s: b, needs_escaping: _ } = b.as_ref() {
            return Rc::new(JSONValue::AllocatedString(String::from(*a) + b.as_ref()));
        }

        panic!(format!("Cannot add values {} and {}", JSONValue::String { s: a.clone(), needs_escaping: false }, b));
    }
    
    if let JSONValue::Array(a) = a.as_ref() {
        if let JSONValue::Array(b) = b.as_ref() {
            let mut result = a.clone();
            for v in b {
                result.push(v.clone());
            }
            return Rc::new(JSONValue::Array(result));
        }

        panic!(format!("Cannot add values {} and {}", JSONValue::Array(a.clone()), b));
    }

    // if let JSONValue::Object(a) = a.clone() {
    //     if let JSONValue::Object(b) = b {
    //         let mut res = a;
    //         for (key, value) in b {
    //             res.insert(key, value);
    //         }
    //         return JSONValue::Object(res);
    //     }
    // }

    panic!(format!("Cannot add values {} and {}", a, b));
}

fn subtract<'a>(vals: (Rc<JSONValue<'a>>, Rc<JSONValue<'a>>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    numeric_operation!(a, b, -);

    if let JSONValue::String { s: a, needs_escaping: _ } = a.as_ref() {
        if let JSONValue::String { s: b , needs_escaping: _ } = b.as_ref() {
            todo!()
        }

        panic!(format!("Cannot subtract values {} and {}", JSONValue::String { s: a.clone(), needs_escaping: false }, b));
    } else if let JSONValue::Array(a) = a.as_ref() {
        if let JSONValue::Array(b) = b.as_ref() {
            return Rc::new(JSONValue::Array(
                a.iter()
                    .filter(|v| b.iter().any(|other| other == *v))
                    .cloned()
                    .collect()
            ));
        }

        panic!(format!("Cannot subtract values {} and {}", JSONValue::Array(a.clone()), b));
    }

    panic!(format!("Cannot subtract values {} and {}", a, b));
}

fn multiply<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    numeric_operation!(a, b, *);

    panic!(format!("Cannot multiply values {} and {}", a, b));
}

fn divide<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    numeric_operation!(a, b, /);

    panic!(format!("Cannot divide values {} and {}", a, b));
}

fn modulo<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    numeric_operation!(a, b, %);

    panic!(format!("Cannot get the modulus of values {} and {}", a, b));
}

fn equal<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    Rc::new(JSONValue::Bool(a == b))
}

fn not_equal<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    Rc::new(JSONValue::Bool(a != b))
}

fn less_than<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    let ord = a.cmp(&b);
    Rc::new(JSONValue::Bool(ord == Ordering::Less))
}

fn less_than_or_equal<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    let ord = a.cmp(&b);
    Rc::new(JSONValue::Bool(ord == Ordering::Less || ord == Ordering::Equal))
}

fn greater_than<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    let ord = a.cmp(&b);
    Rc::new(JSONValue::Bool(ord == Ordering::Greater))
}

fn greater_than_or_equal<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    let ord = a.cmp(&b);
    Rc::new(JSONValue::Bool(ord == Ordering::Greater || ord == Ordering::Equal))
}

fn and<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    if let JSONValue::Bool(a) = a.as_ref() {
        if let JSONValue::Bool(b) = b.as_ref() {
            return Rc::new(JSONValue::Bool(*a && *b));
        }
    }

    panic!(format!("Cannot apply 'and' to values {} and {}", a, b));
}

fn or<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    if let JSONValue::Bool(a) = a.as_ref() {
        if let JSONValue::Bool(b) = b.as_ref() {
            return Rc::new(JSONValue::Bool(*a || *b));
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

    // #[test]
    // fn test_15() {
    //     test_filter("5", "10 / . * 3", "6")
    // }

    fn test_filter(input_json: &str, filter: &str, output_json: &str) {
        let input = json_parser::parse(input_json, false).map(|r| r.unwrap()).map(Rc::new);
        let filter = filter_parser::parse(filter).unwrap();
        let expected = json_parser::parse(output_json, false).map(|r| r.unwrap()).map(Rc::new);

        apply_filter(&filter, input).zip(expected).for_each(|(r, e)| {
            assert_eq!(r, e)
        });
    }
}
