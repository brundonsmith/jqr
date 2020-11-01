use std::{collections::HashMap, cmp::Ordering, rc::Rc};

use crate::json_model::{JSONValue, apply_escapes};
use crate::filter_model::Filter;

pub fn apply_filter<'a>(filter: &'a Filter<'a>, values: impl 'a + Iterator<Item=JSONValue<'a>>) -> Box<dyn 'a + Iterator<Item=JSONValue<'a>>> {
    match filter {
        Filter::Identity => Box::new(values),
        Filter::ObjectIdentifierIndex { identifier, optional } => 
            Box::new(values.filter_map(move |val| {
                if let JSONValue::Object(contents) = val {
                    Some(contents.as_ref().0.get(&JSONValue::String { s: identifier, needs_escaping: false }).unwrap().clone())
                } else if val == JSONValue::Null && *optional {
                    None
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
        Filter::Slice { start, end, optional } => 
            Box::new(values.filter_map(move |val| -> Option<Box<dyn Iterator<Item=JSONValue>>> {
                if JSONValue::Null == val && *optional {
                    None
                } else if *start == None && *end == None {
                    if let JSONValue::Object(contents) = val {
                        let vals: Vec<JSONValue> = contents.as_ref().0.values().cloned().collect();
                        Some(Box::new(vals.into_iter()))
                    } else if let JSONValue::Array(contents) = val {
                        let vals: Vec<JSONValue> = contents.iter().cloned().collect();
                        Some(Box::new(vals.into_iter()))
                    } else if val == JSONValue::Null && *optional {
                        None
                    } else {
                        panic!(format!("Cannot get values of {}", val));
                    }
                } else {
                    Some(Box::new(std::iter::once(
                        if let JSONValue::Array(contents) = val {
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
                        } else if let JSONValue::String { s: string, needs_escaping } = val {
                            if needs_escaping {
                                let string = apply_escapes(string);

                                JSONValue::AllocatedString(Rc::new(
                                    slice(&string, start, end)
                                        .expect(&format!("Cannot get slice of {}", val))
                                ))
                            } else {
                                JSONValue::AllocatedString(Rc::new(
                                    slice(&string, start, end)
                                        .expect(&format!("Cannot get slice of {}", val))
                                ))
                            }
                        } else { 
                            panic!(format!("Cannot get slice of {}", val));
                        }
                    )))
                }
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
        Filter::Alternative { left, right } => applied_to_combinations(values, left, right, alternative),

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
                    _ => panic!("Cannot get the length of {}", val),
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
                _ => panic!("Cannot map over value {}", val),
            }
        })),
        Filter::Select(pred) => Box::new(values.filter(move |val|
            apply_filter(pred, std::iter::once(val.clone()))
                .all(|v| v == JSONValue::Bool(true))
        )),
        Filter::Sort => Box::new(values.map(move |val| {
            match val {
                JSONValue::Array(arr) => {
                    let mut res = arr.as_ref().clone();

                    res.sort();

                    JSONValue::Array(Rc::new(res))
                },
                _ => panic!("Cannot sort {}", val),
            }
        })),
        Filter::SortBy(inner) => Box::new(values.map(move |val| {
            match val {
                JSONValue::Array(arr) => {
                    let mut res = arr.as_ref().clone();

                    res.sort_by(|a, b| {
                        let a = apply_filter(inner, std::iter::once(a.clone())).next().unwrap();
                        let b = apply_filter(inner, std::iter::once(b.clone())).next().unwrap();

                        a.cmp(&b)
                    });

                    JSONValue::Array(Rc::new(res))
                },
                _ => panic!("Cannot sort {}", val),
            }
        })),
        Filter::Has(key) => Box::new(values.map(move |val| {
            if let Some((key, _)) = key.as_str() {
                if let JSONValue::Object(contents) = val {
                    return JSONValue::Bool(contents.as_ref().0.contains_key(&JSONValue::String { s: key, needs_escaping: false }));
                }
            }
            
            if let JSONValue::Integer(key) = key {
                if let JSONValue::Array(contents) = val {
                    return JSONValue::Bool((*key as usize) < contents.len());
                }
            }

            panic!("Can't call has({}) on {}", key, val);
        })),
        Filter::Type => Box::new(values.map(|val| JSONValue::String { s: val.type_name(), needs_escaping: false })),
        Filter::Min => Box::new(values.map(|val| {
            if let JSONValue::Array(contents) = &val {
                return contents.iter().min().cloned().unwrap_or(JSONValue::Null);
            }

            panic!("Cannot get the min of {}", val);
        })),
        Filter::Max => Box::new(values.map(|val| {
            if let JSONValue::Array(contents) = &val {
                return contents.iter().max().cloned().unwrap_or(JSONValue::Null);
            }

            panic!("Cannot get the max of {}", val);
        })),
        Filter::Flatten => Box::new(values.map(|val| {
            if let JSONValue::Array(contents) = val {
                let mut res = Vec::new();

                flatten_recursive(contents.as_ref(), &mut res);

                return JSONValue::Array(Rc::new(res));
            }

            panic!("Cannot flatten {}", val);
        })),

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

fn flatten_recursive<'a: 'b, 'b>(vals: &'b Vec<JSONValue<'a>>, result: &mut Vec<JSONValue<'a>>) {
    for val in vals {
        match val {
            JSONValue::Array(contents) => flatten_recursive(contents, result),
            _ => result.push(val.clone()),
        }
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


fn add<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    if let JSONValue::Integer(a) = a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::Integer(a + b);
        }
    }

    if let Some(a) = a.as_float() {
        if let Some(b) = b.as_float() {
            return JSONValue::Float(a + b);
        }
    }
    
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

    if let JSONValue::Integer(a) = a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::Integer(a - b);
        }
    }

    if let Some(a) = a.as_float() {
        if let Some(b) = b.as_float() {
            return JSONValue::Float(a - b);
        }
    }
    
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

    if let JSONValue::Integer(a) = a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::Integer(a * b);
        }
    }

    if let Some(a) = a.as_float() {
        if let Some(b) = b.as_float() {
            return JSONValue::Float(a * b);
        }
    }

    if let JSONValue::String { s: a, needs_escaping: _ } = &a {
        if let JSONValue::Integer(b) = b {
            return repeated_str(a, b);
        }
    } else if let JSONValue::AllocatedString(a) = &a {
        if let JSONValue::Integer(b) = b {
            return repeated_str(a, b);
        }
    } else if let JSONValue::Integer(a) = &a {
        if let JSONValue::String { s: b, needs_escaping: _ } = b {
            return repeated_str(b, *a);
        } else if let JSONValue::AllocatedString(b) = b {
            return repeated_str(&b, *a);
        }
    }

    if let JSONValue::Object(a) = &a {
        if let JSONValue::Object(b) = b {
            return merge_objects_recursive(&a.as_ref().0, &b.as_ref().0);
        }
    }

    panic!(format!("Cannot multiply values {} and {}", a, b));
}

fn repeated_str<'a, 'b>(s: &'a str, n: i32) -> JSONValue<'b> {
    if n == 0 {
        return JSONValue::Null;
    } else {
        let mut res = String::with_capacity(s.len() * n as usize);

        for _ in 0..n {
            res.push_str(s);
        }

        return JSONValue::AllocatedString(Rc::new(res));
    }
}

fn merge_objects_recursive<'a: 'b, 'b>(a: &'b HashMap<JSONValue<'a>,JSONValue<'a>>, b: &'b HashMap<JSONValue<'a>, JSONValue<'a>>) -> JSONValue<'a> {
    let mut result_map = HashMap::new();

    for (key, value) in a.iter() {
        result_map.insert(key.clone(), value.clone());
    }

    for (key, value) in b.iter() {
        if let Some(JSONValue::Object(contents_a)) = result_map.get(key) {
            if let JSONValue::Object(contents_b) = value {
                result_map.insert(key.clone(), merge_objects_recursive(&contents_a.as_ref().0, &contents_b.as_ref().0));
                continue;
            }
        }

        result_map.insert(key.clone(), value.clone());
    }

    JSONValue::Object(Rc::new((result_map, None)))
}

// fn flatten_recursive<'a: 'b, 'b>(vals: &'b Vec<JSONValue<'a>>, result: &mut Vec<JSONValue<'a>>) {

fn divide<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    if let JSONValue::Integer(a) = a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::Integer(a / b);
        }
    }

    if let Some(a) = a.as_float() {
        if let Some(b) = b.as_float() {
            return JSONValue::Float(a / b);
        }
    }

    if let Some((a, ane)) = a.as_str() {
        if let Some((b, bne)) = b.as_str() {
            todo!()
            // return JSONValue::Array(Rc::new(
            //     a.split(b)
            //         .map(|s| JSONValue::String { s, needs_escaping: ane || bne })
            //         .collect()
            // ));
        }
    }

    panic!(format!("Cannot divide values {} and {}", a, b));
}

fn modulo<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    if let JSONValue::Integer(a) = a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::Integer(a % b);
        }
    }

    if let Some(a) = a.as_float() {
        if let Some(b) = b.as_float() {
            return JSONValue::Float(a % b);
        }
    }

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

fn alternative<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    if a == JSONValue::Null || a == JSONValue::Bool(false) {
        return b;
    } else {
        return a;
    }
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

    #[test]
    fn test_21() {
        test_filter(
            "[ null, 12, 0, \"\", false ]", 
            "map(. // \"ALTERNATIVE\")", 
            "[ \"ALTERNATIVE\", 12, 0, \"\", \"ALTERNATIVE\" ]")
    }

    #[test]
    fn test_22() {
        let input = json_parser::parse("null", false).map(|r| r.unwrap());
        let filter = filter_parser::parse(".[]?").unwrap();
        println!("{:?}", filter);

        let res = apply_filter(&filter, input).next();

        assert!(res == None);
    }

    #[test]
    fn test_23() {
        test_filter("23", "(. + 3) * 2", "52")
    }

    #[test]
    fn test_24() {
        test_filter("{ \"foo\": 12 }", "has(\"foo\")", "true")
    }

    #[test]
    fn test_25() {
        test_filter("{ \"foo\": 12 }", "has(\"bar\")", "false")
    }

    #[test]
    fn test_26() {
        test_filter("[ 55, 44, 33 ]", "has(33)", "false")
    }

    #[test]
    fn test_27() {
        test_filter("[ 55, 44, 33 ]", "has(2)", "true")
    }

    #[test]
    fn test_28() {
        test_filter("[ false, null, {}, [], 12 ]", "map(type)", "[ \"boolean\", \"null\", \"object\", \"array\", \"number\" ]")
    }

    #[test]
    fn test_29() {
        test_filter("[ 5, 2, 9, 8, 3 ]", "min", "2")
    }

    #[test]
    fn test_30() {
        test_filter("[ ]", "min", "null")
    }

    #[test]
    fn test_31() {
        test_filter("[ 5, 2, 9, 8, 3 ]", "max", "9")
    }

    #[test]
    fn test_32() {
        test_filter("[1, [2], [[3], 4]]", "flatten", "[1, 2, 3, 4]")
    }

    #[test]
    fn test_33() {
        test_filter(
            "[[1, 2, 4], [1, 2, 3], [2, 3, 4]]",
            "sort",
            "[[1, 2, 3], [1, 2, 4], [2, 3, 4]]")
    }

    #[test]
    fn test_34() {
        test_filter(
            "[{\"foo\":4, \"bar\":10}, {\"foo\":3, \"bar\":100}, {\"foo\":2, \"bar\":1}]",
            "sort",
            "[{\"foo\":2, \"bar\":1}, {\"foo\":4, \"bar\":10}, {\"foo\":3, \"bar\":100}]")
    }

    #[test]
    fn test_35() {
        test_filter(
            "[ {\"k\": {\"a\": 1, \"b\": 2}}, {\"k\": {\"a\": 0,\"c\": 3}} ]",
            ".[0] * .[1]",
            "{\"k\": {\"a\": 0, \"b\": 2, \"c\": 3}}")
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
