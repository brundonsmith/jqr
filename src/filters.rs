use std::{cmp::Ordering, rc::Rc};

use crate::json_parser::JSONValue;

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

// map(select(.base.Attack > 100)) | map(.name.english)
// pub fn apply_filter_hardcoded<'a>(values: impl Iterator<Item=Rc<JSONValue<'a>>>) -> impl Iterator<Item=Rc<JSONValue<'a>>> {
//     values.map(|val| {
//         if let JSONValue::Array(vec) = val.as_ref() {
//             let vals: Vec<Rc<JSONValue>> = vec.iter()
//                 .filter(|val| {
//                     if let JSONValue::Object(contents) = val.as_ref() {
//                         let val = contents.get(&JSONValue::String("base")).unwrap();

//                         if let JSONValue::Object(contents) = val.as_ref() {
//                             let val = contents.get(&JSONValue::String("Attack")).unwrap();

//                             if let JSONValue::Integer(attack) = val.as_ref() {
//                                 return *attack > 100;
//                             }
//                         }
//                     }
    
//                     panic!("failed");
//                 })
//                 .map(|val| {
//                     if let JSONValue::Object(contents) = val.as_ref() {
//                         let val = contents.get(&JSONValue::String("name")).unwrap();
        
//                         if let JSONValue::Object(contents) = val.as_ref() {
//                             let val = contents.get(&JSONValue::String("english")).unwrap();

//                             return val;
//                         }
//                     }
    
//                     panic!("failed");
//                 })
//                 .cloned()
//                 .collect();

//             return vals.into_iter();
//         }

//         panic!("failed");
//     }).flatten()
// }


pub fn apply_filter<'a>(filter: &'a Filter<'a>, values: impl 'a + Iterator<Item=Rc<JSONValue<'a>>>) -> Box<dyn 'a + Iterator<Item=Rc<JSONValue<'a>>>> {
    // println!("\n{:?}", filter);
    match filter {
        Filter::Identity => Box::new(values),
        Filter::ObjectIdentifierIndex { identifier, optional } => 
            Box::new(values.map(move |val| -> Rc<JSONValue> {
                if let JSONValue::Object(contents) = val.as_ref() {
                    let result = contents.get(&JSONValue::String(identifier)).unwrap();
                    result.clone()
                } else if *optional && val.as_ref() == &JSONValue::Null {
                    Rc::new(JSONValue::Null)
                } else {
                    panic!(format!("Error: Object identifier index can only be used on values of type Object; got {}", val))
                }
            })),
        Filter::ArrayIndex { index } => 
            Box::new(values.map(move |val| -> Rc<JSONValue> {
                if let JSONValue::Array(contents) = val.as_ref() {
                    return contents[*index].clone(); // TODO: Remove from Vec here as in Map above, to avoid clone()
                } else {
                    panic!(format!("Error: Array index can only be used on values of type Array; got {}", val));
                }
            })),
        Filter::Slice { start, end } => 
            Box::new(values.map(move |val| -> Box<dyn 'a + Iterator<Item=Rc<JSONValue>>> {
                if let Some(s) = start {
                    if let Some(e) = end {
                        match val.as_ref() {
                            JSONValue::Array(contents) => Box::new(std::iter::once(Rc::new(JSONValue::Array(contents.iter().skip(*s).take(*e - *s).cloned().collect())))),
                            JSONValue::String(string) => Box::new(std::iter::once(Rc::new(JSONValue::String(&string[*s..*e])))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    } else {
                        match val.as_ref() {
                            JSONValue::Array(contents) => Box::new(std::iter::once(Rc::new(JSONValue::Array(contents.into_iter().skip(*s).cloned().collect())))),
                            JSONValue::String(string) => Box::new(std::iter::once(Rc::new(JSONValue::String(&string[*s..])))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    }
                } else {
                    if let Some(e) = end {
                        match val.as_ref() {
                            JSONValue::Array(contents) => Box::new(std::iter::once(Rc::new(JSONValue::Array(contents.into_iter().take(*e).cloned().collect())))),
                            JSONValue::String(string) => Box::new(std::iter::once(Rc::new(JSONValue::String(&string[..*e])))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    } else {
                        match val.as_ref() {
                            JSONValue::Array(contents) => Box::new(std::iter::once(val.clone())),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    }
                }
            }).flatten()),
        Filter::AllValues => 
            Box::new(values.map(move |val| -> Box<dyn 'a + Iterator<Item=Rc<JSONValue<'a>>>> {
                match val.as_ref() {
                    JSONValue::Object(contents) => {
                        let vals: Vec<Rc<JSONValue>> = contents.values().cloned().collect();

                        Box::new(vals.into_iter())
                    },
                    JSONValue::Array(contents) => {
                        let vals: Vec<Rc<JSONValue>> = contents.iter().cloned().collect();

                        Box::new(vals.into_iter())
                    },
                    _ => panic!(format!("Cannot get values of {}", val)),
                }
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
            match val.as_ref() {
                JSONValue::Array(x) => Rc::new(JSONValue::Integer(x.len() as i32)),
                JSONValue::String(x) => Rc::new(JSONValue::Integer(x.len() as i32)),
                JSONValue::Null => Rc::new(JSONValue::Integer(0)),
                JSONValue::Object(x) => Rc::new(JSONValue::Integer(x.keys().len() as i32)),
                _ => panic!(format!("Cannot get the length of {}", val)),
            }
        })),
        Filter::Keys => {
            let mut unsorted_keys = values.map(keys).flatten().collect::<Vec<Rc<JSONValue<'a>>>>();

            unsorted_keys.sort_by(|a, b| cmp(a.as_ref(), b.as_ref()));

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
                            .map(|v| v.clone())
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

fn cmp(a: &JSONValue, b: &JSONValue) -> Ordering {
    let cmp_key_a = type_cmp_key(a);
    let cmp_key_b = type_cmp_key(b);
    if cmp_key_a != cmp_key_b {
        return cmp_key_a.cmp(&cmp_key_b);
    } else if let JSONValue::String(a) = a {
        if let JSONValue::String(b) = b {
            return a.cmp(b);
        }
    } else if let JSONValue::Array(a) = a {
        if let JSONValue::Array(b) = b {
            todo!()
        }
    } else if let JSONValue::Object(a) = a {
        if let JSONValue::Object(b) = b {
            todo!()
        }
    }

    let float_a = match a {
        JSONValue::Integer(x) => Some(*x as f32),
        JSONValue::Float(x) => Some(*x),
        _ => None,
    };
    let float_b = match b {
        JSONValue::Integer(x) => Some(*x as f32),
        JSONValue::Float(x) => Some(*x),
        _ => None,
    };

    if let Some(a) = float_a {
        if let Some(b) = float_b {
            return a.partial_cmp(&b).unwrap();
        }
    }

    Ordering::Equal
}

fn type_cmp_key(val: &JSONValue) -> u8 {
    match val {
        JSONValue::Object(_) => 6,
        JSONValue::Array(_) => 5,
        JSONValue::String(_) => 4,
        JSONValue::AllocatedString(_) => 4,
        JSONValue::Integer(_) => 3,
        JSONValue::Float(_) => 3,
        JSONValue::Bool(true) => 2,
        JSONValue::Bool(false) => 1,
        JSONValue::Null => 0,
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
            (0..arr.len()).map(|i| Rc::new(JSONValue::Integer(i as i32))).collect(),
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

    if let JSONValue::String(a) = a.as_ref() {
        if let JSONValue::String(b) = b.as_ref() {
            return Rc::new(JSONValue::AllocatedString(String::from(*a) + b.as_ref()));
        }

        panic!(format!("Cannot add values {} and {}", JSONValue::String(a.clone()), b));
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

    if let JSONValue::String(a) = a.as_ref() {
        if let JSONValue::String(b) = b.as_ref() {
            todo!()
            // return JSONValue::MultiSliceString(new_str.replace(b, ""));
        }
        if let JSONValue::String(b) = b.as_ref() {
            todo!()
            // return JSONValue::MultiSliceString(new_str.replace(b.as_str(), ""));
        }

        panic!(format!("Cannot subtract values {} and {}", JSONValue::String(a.clone()), b));
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

    let ord = cmp(&a, &b);
    Rc::new(JSONValue::Bool(ord == Ordering::Less))
}

fn less_than_or_equal<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    let ord = cmp(&a, &b);
    Rc::new(JSONValue::Bool(ord == Ordering::Less || ord == Ordering::Equal))
}

fn greater_than<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    let ord = cmp(&a, &b);
    Rc::new(JSONValue::Bool(ord == Ordering::Greater))
}

fn greater_than_or_equal<'a>(vals: (Rc<JSONValue>, Rc<JSONValue>)) -> Rc<JSONValue<'a>> {
    let (a, b) = vals;

    let ord = cmp(&a, &b);
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

    fn test_filter(input_json: &str, filter: &str, output_json: &str) {
        let input = json_parser::parse(input_json, false).map(|r| r.unwrap()).map(Rc::new);
        let filter = filter_parser::parse(filter).unwrap();
        let expected = json_parser::parse(output_json, false).map(|r| r.unwrap()).map(Rc::new);

        apply_filter(&filter, input).zip(expected).for_each(|(r, e)| {
            assert_eq!(r, e)
        });
    }
}
