use crate::model::{Filter, JSONValue};


pub fn apply_filter<'a>(filter: &'a Filter<'a>, values: impl 'a + Iterator<Item=JSONValue<'a>>) -> Box<dyn 'a + Iterator<Item=JSONValue<'a>>> {
    match filter {
        Filter::Identity => Box::new(values),
        Filter::ObjectIdentifierIndex { identifier, optional } => 
            Box::new(values.map(move |val| -> JSONValue<'a> {
                if let JSONValue::Object(contents) = val {
                    return contents.get(identifier).unwrap().clone();
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
                            JSONValue::Array(contents) => Box::new(contents.into_iter().skip(*s).take(*e - *s)),
                            JSONValue::String(string) => Box::new(std::iter::once(JSONValue::String(&string[*s..*e]))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    } else {
                        match val {
                            JSONValue::Array(contents) => Box::new(contents.into_iter().skip(*s)),
                            JSONValue::String(string) => Box::new(std::iter::once(JSONValue::String(&string[*s..]))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    }
                } else {
                    if let Some(e) = end {
                        match val {
                            JSONValue::Array(contents) => Box::new(contents.into_iter().take(*e)),
                            JSONValue::String(string) => Box::new(std::iter::once(JSONValue::String(&string[..*e]))),
                            _ => panic!(format!("Cannot get values of {}", val)),
                        }
                    } else {
                        match val {
                            JSONValue::Array(contents) => Box::new(contents.into_iter()),
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

        Filter::Add { left, right } => Box::new(values),
        Filter::Subtract { left, right } => Box::new(values),
        Filter::Multiply { left, right } => Box::new(values),
        Filter::Divide { left, right } => Box::new(values),
        Filter::Modulo { left, right } => Box::new(values),

        Filter::Length => Box::new(values),
        Filter::Keys => Box::new(values),
        Filter::KeysUnsorted => Box::new(values),
    }
}

#[cfg(test)]
mod tests {

    use crate::{filter_parser, model::JSONValue};
    use crate::json_parser;

    use super::apply_filter;

    #[test]
    fn test_1() {
        let filter = filter_parser::parse(".foo").unwrap();
        let json = json_parser::parse("{ \"foo\": 12, \"bar\": \"hello\" }").unwrap();
    
        assert_eq!(apply_filter(&filter, std::iter::once(json)).collect::<Vec<JSONValue>>(), vec![ JSONValue::Integer(12) ]);
    }
    
    #[test]
    fn test_2() {
        let filter = filter_parser::parse(".bar.stuff").unwrap();
        let json = json_parser::parse("
        { 
            \"foo\": 12, 
            \"bar\": {
                \"stuff\": false
            }
        }").unwrap();
    
        assert_eq!(apply_filter(&filter, std::iter::once(json)).collect::<Vec<JSONValue>>(), vec![ JSONValue::Boolean(false) ]);
    }

    #[test]
    fn test_3() {
        let filter = filter_parser::parse(".bar.[1]").unwrap();
        let json = json_parser::parse("
        { 
            \"foo\": 12, 
            \"bar\": [ 0, 1, 2 ]
        }").unwrap();
    
        assert_eq!(apply_filter(&filter, std::iter::once(json)).collect::<Vec<JSONValue>>(), vec![ JSONValue::Integer(1) ]);
    }
    
    #[test]
    fn test_4() {
        let filter = filter_parser::parse(".bar.[]").unwrap();
        let json = json_parser::parse("
        { 
            \"foo\": 12, 
            \"bar\": [ 0, 1, 2 ]
        }").unwrap();
    
        assert_eq!(apply_filter(&filter, std::iter::once(json)).collect::<Vec<JSONValue>>(), vec![ JSONValue::Integer(0), JSONValue::Integer(1), JSONValue::Integer(2) ]);
    }
    
    #[test]
    fn test_5() {
        let filter = filter_parser::parse(".bar.[1:]").unwrap();
        let json = json_parser::parse("
        { 
            \"foo\": 12, 
            \"bar\": [ 0, 1, 2 ]
        }").unwrap();
    
        assert_eq!(apply_filter(&filter, std::iter::once(json)).collect::<Vec<JSONValue>>(), vec![ JSONValue::Integer(1), JSONValue::Integer(2) ]);
    }
    
    #[test]
    fn test_6() {
        let filter = filter_parser::parse(".bar.[2:4]").unwrap();
        let json = json_parser::parse("
        { 
            \"foo\": 12, 
            \"bar\": [ 0, 1, 2, 3, 4, 5, 6 ]
        }").unwrap();
    
        assert_eq!(apply_filter(&filter, std::iter::once(json)).collect::<Vec<JSONValue>>(), vec![ JSONValue::Integer(2), JSONValue::Integer(3) ]);
    }
    
    #[test]
    fn test_7() {
        let filter = filter_parser::parse(".bar.[:4]").unwrap();
        let json = json_parser::parse("
        { 
            \"foo\": 12, 
            \"bar\": [ 0, 1, 2, 3, 4, 5, 6 ]
        }").unwrap();
    
        assert_eq!(apply_filter(&filter, std::iter::once(json)).collect::<Vec<JSONValue>>(), vec![ JSONValue::Integer(0), JSONValue::Integer(1), JSONValue::Integer(2), JSONValue::Integer(3) ]);
    }
}
