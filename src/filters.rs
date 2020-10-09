use crate::model::{Filter, JSONValue};


pub fn apply_filter<'a>(filter: &'a Filter<'a>, values: impl 'a + Iterator<Item=JSONValue<'a>>) -> Box<dyn 'a + Iterator<Item=JSONValue<'a>>> {
    match filter {
        Filter::Identity => Box::new(values),
        Filter::ObjectIdentifierIndex { identifier, optional } => 
            Box::new(values.map(move |val| -> JSONValue<'a> {
                if let JSONValue::Object(contents) = val {
                    return contents.get(identifier).unwrap().clone();
                } else if *optional && val == JSONValue::Null {
                    return JSONValue::Null;
                } else {
                    panic!(format!("Error: Object identifier index can only be used on values of type Object; got {}", val));
                }
            })),
        Filter::ArrayIndex { index } => 
            Box::new(values.map(move |val| -> JSONValue<'a> {
                if let JSONValue::Array(contents) = val {
                    return contents[*index].clone();
                } else {
                    panic!(format!("Error: Array index can only be used on values of type Array; got {}", val));
                }
            })),
        Filter::Slice { start, end } => Box::new(values.map(move |val| -> Box<dyn 'a + Iterator<Item=JSONValue<'a>>> {
                if let Some(s) = start {
                    if let Some(e) = end {
                        match val {
                            JSONValue::Array(contents) => Box::new(std::iter::once(JSONValue::Array(contents.into_iter().skip(*s).take(*e - *s).collect()))),
                            JSONValue::String(string) => Box::new(std::iter::once(JSONValue::String(&string[*s..*e]))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    } else {
                        match val {
                            JSONValue::Array(contents) => Box::new(std::iter::once(JSONValue::Array(contents.into_iter().skip(*s).collect()))),
                            JSONValue::String(string) => Box::new(std::iter::once(JSONValue::String(&string[*s..]))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    }
                } else {
                    if let Some(e) = end {
                        match val {
                            JSONValue::Array(contents) => Box::new(std::iter::once(JSONValue::Array(contents.into_iter().take(*e).collect()))),
                            JSONValue::String(string) => Box::new(std::iter::once(JSONValue::String(&string[..*e]))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    } else {
                        match val {
                            JSONValue::Array(contents) => Box::new(std::iter::once(JSONValue::Array(contents.clone()))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    }
                }
            }).flatten()),
        Filter::AllValues => Box::new(values.map(move |val| -> Box<dyn 'a + Iterator<Item=JSONValue<'a>>> {
                match val {
                    JSONValue::Object(contents) => Box::new(contents.into_iter().map(|(_, v)| v)),
                    JSONValue::Array(contents) => Box::new(contents.into_iter()),
                    _ => panic!(format!("Cannot get values of {}", val)),
                }
            }).flatten()),
        Filter::Literal(v) => Box::new(std::iter::once(v.clone())),

        Filter::Comma(filters) => {
            let vals = values.collect::<Vec<JSONValue<'a>>>();

            Box::new(filters.iter()
                .map(move |f| apply_filter(f, vals.clone().into_iter()))
                .flatten())
        },
        Filter::Pipe(filters) => 
            Box::new(values.map(move |val| -> Box<dyn 'a + Iterator<Item=JSONValue<'a>>> {
                let mut result: Box<dyn 'a + Iterator<Item=JSONValue<'a>>> = Box::new(std::iter::once(val));

                for f in filters {
                    result = apply_filter(f, result);
                }

                return result;
            }).flatten()),

        Filter::Add { left, right } => Box::new(values.map(move |val| {
            let left_vals = apply_filter(left, std::iter::once(val.clone()));
            let right_vals = apply_filter(right, std::iter::once(val));

            combinations(left_vals, right_vals).map(add)
        }).flatten()),
        Filter::Subtract { left, right } => Box::new(values.map(move |val| {
            let left_vals = apply_filter(left, std::iter::once(val.clone()));
            let right_vals = apply_filter(right, std::iter::once(val));

            combinations(left_vals, right_vals).map(subtract)
        }).flatten()),
        Filter::Multiply { left, right } => Box::new(values.map(move |val| {
            let left_vals = apply_filter(left, std::iter::once(val.clone()));
            let right_vals = apply_filter(right, std::iter::once(val));

            combinations(left_vals, right_vals).map(multiply)
        }).flatten()),
        Filter::Divide { left, right } => Box::new(values.map(move |val| {
            let left_vals = apply_filter(left, std::iter::once(val.clone()));
            let right_vals = apply_filter(right, std::iter::once(val));

            combinations(left_vals, right_vals).map(divide)
        }).flatten()),
        Filter::Modulo { left, right } => Box::new(values.map(move |val| {
            let left_vals = apply_filter(left, std::iter::once(val.clone()));
            let right_vals = apply_filter(right, std::iter::once(val));

            combinations(left_vals, right_vals).map(modulo)
        }).flatten()),

        Filter::Length => Box::new(values.map(move |val| {
            match val {
                JSONValue::Array(x) => JSONValue::Integer(x.len() as i32),
                JSONValue::String(x) => JSONValue::Integer(x.len() as i32),
                JSONValue::Null => JSONValue::Integer(0),
                JSONValue::Object(x) => JSONValue::Integer(x.keys().len() as i32),
                _ => panic!(format!("Cannot get the length of a value of type {}", val.type_name())),
            }
        })),
        Filter::Keys => {
            let mut unsorted_keys = values.map(keys).flatten().collect::<Vec<JSONValue<'a>>>();

            unsorted_keys.sort_by(|a, b| {
                if let JSONValue::String(a) = a {
                    if let JSONValue::String(b) = b {
                        return a.cmp(b);
                    }
                }

                if let JSONValue::Integer(a) = a {
                    if let JSONValue::Integer(b) = b {
                        return a.cmp(b);
                    }
                }

                unreachable!();
            });

            return Box::new(unsorted_keys.into_iter());
        },
        Filter::KeysUnsorted => Box::new(values.map(keys).flatten()),
        Filter::Map(pred) => Box::new(values.map(move |val| {
            match val {
                JSONValue::Array(arr) => 
                    JSONValue::Array(arr.into_iter().map(|v| apply_filter(pred, std::iter::once(v))).flatten().collect()),
                _ => panic!(format!("Cannot map over a value of type {}", val.type_name()))
            }
        }))
    }
}

fn combinations<'a>(a: impl Iterator<Item=JSONValue<'a>>, b: impl Iterator<Item=JSONValue<'a>>) -> impl Iterator<Item=(JSONValue<'a>,JSONValue<'a>)> {
    let vec_a = a.collect::<Vec<JSONValue<'a>>>();
    let vec_b = b.collect::<Vec<JSONValue<'a>>>();

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

fn keys<'a>(val: JSONValue<'a>) -> Box<dyn 'a + Iterator<Item=JSONValue<'a>>> {
    if let JSONValue::Object(map) = val {
        return Box::new(map.into_iter().map(|(key, _)| JSONValue::String(key)));
    }
    
    if let JSONValue::Array(arr) = val {
        return Box::new((0..arr.len()).map(|i| JSONValue::Integer(i as i32)));
    }

    panic!(format!("Cannot get keys from a value of type {}", val.type_name()));
}

fn add<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    if let JSONValue::Integer(a) = a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::Integer(a + b);
        }
        if let JSONValue::Float(b) = b {
            return JSONValue::Float(a as f32 + b);
        }
    }
    
    if let JSONValue::Float(a) = a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::Float(a + b as f32);
        }
        if let JSONValue::Float(b) = b {
            return JSONValue::Float(a + b);
        }
    }
    
    if a == JSONValue::Null {
        return b;
    }

    if b == JSONValue::Null {
        return a;
    }

    let a_type_name = a.type_name();

    // if let JSONValue::String(a) = a {
    //     if let JSONValue::String(b) = b {
    //         return JSONValue::String(a + b);
    //     }
    // }
    
    if let JSONValue::Array(a) = a {
        if let JSONValue::Array(b) = b {
            let mut res = a.clone();
            for v in b {
                res.push(v);
            }
            return JSONValue::Array(res);
        }
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

    panic!(format!("Cannot add values of type {} and {}", a_type_name, b.type_name()));
}

fn subtract<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    if let JSONValue::Integer(a) = a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::Integer(a - b);
        }
    }
    
    if let JSONValue::Float(a) = a {
        if let JSONValue::Float(b) = b {
            return JSONValue::Float(a - b);
        }
    }
    
    let a_type_name = a.type_name();

    if let JSONValue::Array(a) = a {
        if let JSONValue::Array(b) = b {
            return JSONValue::Array(
                a.iter()
                    .filter(|v| b.iter().any(|other| other == *v))
                    .cloned()
                    .collect()
            );
        }
    }

    panic!(format!("Cannot subtract values of type {} and {}", a_type_name, b.type_name()));
}

fn multiply<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    if let JSONValue::Integer(a) = a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::Integer(a * b);
        }
        // if let JSONValue::String(b) = b {
        //     let mut new_string = String::from(b);

        //     for _ in 0..a {
        //         new_string += b;
        //     }

        //     return JSONValue::String(new_string.as_str());
        // }
    }
    
    if let JSONValue::Float(a) = a {
        if let JSONValue::Float(b) = b {
            return JSONValue::Float(a * b);
        }
    }

    panic!(format!("Cannot multiply values of type {} and {}", a.type_name(), b.type_name()));
}

fn divide<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    if let JSONValue::Integer(a) = a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::Integer(a / b);
        }
    }
    
    if let JSONValue::Float(a) = a {
        if let JSONValue::Float(b) = b {
            return JSONValue::Float(a / b);
        }
    }

    panic!(format!("Cannot divide values of type {} and {}", a.type_name(), b.type_name()));
}

fn modulo<'a>(vals: (JSONValue<'a>, JSONValue<'a>)) -> JSONValue<'a> {
    let (a, b) = vals;

    if let JSONValue::Integer(a) = a {
        if let JSONValue::Integer(b) = b {
            return JSONValue::Integer(a % b);
        }
    }
    
    if let JSONValue::Float(a) = a {
        if let JSONValue::Float(b) = b {
            return JSONValue::Float(a % b);
        }
    }

    panic!(format!("Cannot get the modulus of values of type {} and {}", a.type_name(), b.type_name()));
}



#[cfg(test)]
mod tests {

    use crate::filter_parser;
    use crate::json_parser;

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

    // #[test]
    // fn test_11() {
    //     test_filter(
    //         "[1, 2, 3]",
    //         "map(.+1)",
    //         "[2, 3, 4]"
    //     );
    // }

    fn test_filter(input_json: &str, filter: &str, output_json: &str) {
        let input = json_parser::parse(input_json).map(|r| r.unwrap());
        let filter = filter_parser::parse(filter).unwrap();
        let expected = json_parser::parse(output_json).map(|r| r.unwrap());

        apply_filter(&filter, input).zip(expected).for_each(|(i, o)| {
            assert_eq!(i, o)
        });
    }
}
