
use std::{collections::HashMap};

use crate::model::{JSONValue, StrOrString};



#[derive(Debug,PartialEq)]
pub struct ParseError {
    pub msg: String,
    pub index: usize,
}

const TRUE_TOKEN: &str = "true";
const FALSE_TOKEN: &str = "false";
const NULL_TOKEN: &str = "null";


// parsing
pub fn parse<'a>(code: &'a str) -> impl Iterator<Item=Result<JSONValue<'a>,ParseError>> {
    let mut index = 0;

    consume_whitespace(code, &mut index);
    std::iter::from_fn(move || {
        if index < code.len() {
            let res = Some(expression(code, &mut index));
            consume_whitespace(code, &mut index);
            res
        } else {
            None
        }
    })
}

fn expression<'a>(code: &'a str, index: &mut usize) -> Result<JSONValue<'a>, ParseError> {
    consume_whitespace(code, index);
    let ch = code[*index..].chars().next();

    if ch == Some('"') {
        string(code, index)
    } else if ch == Some('{') {
        object(code, index)
    } else if ch == Some('[') {
        array(code, index)
    } else if ch.map(|c| c.is_numeric()).unwrap_or(false) {
        number(code, index)
    } else if try_match_front(code, index, TRUE_TOKEN) {
        Ok(JSONValue::Boolean(true))
    } else if try_match_front(code, index, FALSE_TOKEN) {
        Ok(JSONValue::Boolean(false))
    } else if try_match_front(code, index, NULL_TOKEN) {
        Ok(JSONValue::Null)
    } else {
        Err(ParseError {
            msg: String::from("Expected JSON value"),
            index: *index
        })
    }
}

fn object<'a>(code: &'a str, index: &mut usize) -> Result<JSONValue<'a>, ParseError> {
    try_eat(code, index, &'{')?;

    if try_eat(code, index, &'}').is_ok() {
        return Ok(JSONValue::Object(HashMap::new()));
    }

    let mut contents = HashMap::new();

    let key = expression(code, index);

    if let Some(prop) = as_str_or_string(key) {
        try_eat(code, index, &':')?;
        let value = expression(code, index)?;

        contents.insert(prop, value);

        while try_eat(code, index, &',').is_ok() {
            let key = expression(code, index);

            if let Some(prop) = as_str_or_string(key) {
                try_eat(code, index, &':')?;
                let value = expression(code, index)?;

                contents.insert(prop, value);
            } else {
                return Err(ParseError {
                    msg: String::from("Expected object property after ','"),
                    index: *index,
                });
            }
        }
    }
    try_eat(code, index, &'}')?;
    
    return Ok(JSONValue::Object(contents));
}

fn as_str_or_string<'a>(next: Result<JSONValue<'a>, ParseError>) -> Option<StrOrString<'a>> {
    if let Ok(JSONValue::String(prop)) = next {
        return Some(StrOrString::Str(prop));
    } else if let Ok(JSONValue::AllocatedString(prop)) = next {
        return Some(StrOrString::String(prop));
    } else {
        return None;
    }
}

fn array<'a>(code: &'a str, index: &mut usize) -> Result<JSONValue<'a>, ParseError> {
    try_eat(code, index, &'[')?;

    if try_eat(code, index, &']').is_ok() {
        return Ok(JSONValue::Array(vec![]));
    }

    let mut contents = Vec::new();

    if let Ok(value) = expression(code, index) {
        contents.push(value);

        while try_eat(code, index, &',').is_ok() {
            let value = expression(code, index)?;
            contents.push(value);
        }
    }
    try_eat(code, index, &']')?;

    return Ok(JSONValue::Array(contents));
}

fn string<'a>(code: &'a str, index: &mut usize) -> Result<JSONValue<'a>, ParseError> {
    let mut escape_next = false;
    let mut allocated_string: Option<String> = None;
    let contents_start = *index + 1;
    let mut end = contents_start;
    for (i, c) in code[*index + 1..].char_indices() {
        let current_index = contents_start + i;
        if c == '\\' && !escape_next {
            escape_next = true;
            if allocated_string.is_none() {
                allocated_string = Some(String::from(&code[contents_start..current_index]));
            }
        } else {
            if c == '"' && !escape_next {
                end = contents_start + i;
                *index = current_index + 1;
                break;
            }
            
            match &mut allocated_string {
                Some(s) => s.push(c),
                None => {},
            };

            escape_next = false;
        }
    }

    Ok(match allocated_string {
        Some(s) => JSONValue::AllocatedString(s),
        None => JSONValue::String(&code[contents_start..end]),
    })
}

fn number<'a>(code: &'a str, index: &mut usize) -> Result<JSONValue<'a>, ParseError> {
    let start = *index;
    let front_end = *index + code[*index..].char_indices()
        .take_while(|(_, c)| c.is_numeric())
        .last()
        .map(|(index, ch)| index + ch.len_utf8())
        .unwrap_or(0);

    if code[front_end..].chars().next() == Some('.') {
        let back_end_start = front_end + 1;
        let back_end = back_end_start + code[back_end_start..].char_indices()
            .take_while(|(_, c)| c.is_numeric())
            .last()
            .map(|(index, ch)| index + ch.len_utf8())
            .unwrap_or(0);

        *index = back_end;
        return Ok(JSONValue::Float(code[start..back_end].parse().unwrap()));
    } else {
        *index = front_end;
        return Ok(JSONValue::Integer(code[start..front_end].parse().unwrap()));
    }
}

fn try_eat<'a>(code: &'a str, index: &mut usize, expected: &char) -> Result<(), ParseError> {
    consume_whitespace(code, index);
    if let Some(found) = code[*index..].chars().next() {
        if &found == expected {
            *index += expected.len_utf8();
            Ok(())
        } else {
            return Err(ParseError { 
                msg: format!("Expected '{}', found '{}'", expected, found), 
                index: *index
            });
        }
    } else {
        return Err(ParseError { 
            msg: format!("Expected '{}'", expected), 
            index: *index
        });
    }
}

fn try_match_front(code: &str, index: &mut usize, segment: &str) -> bool {
    consume_whitespace(code, index);
    if *index + segment.len() <= code.len() && segment.chars().zip(code[*index..].chars()).all(|(a, b)| a == b) {
        *index += segment.len();
        return true;
    } else {
        return false;
    }
}

fn consume_whitespace<'a>(code: &'a str, index: &mut usize) {
    if let Some(length) = code[*index..]
            .char_indices()
            .take_while(|(_, ch)| ch.is_whitespace())
            .last()
            .map(|(i, ch)| i + ch.len_utf8()) {
        *index += length;
    }
}

#[cfg(test)]
mod parser_tests {
    use std::collections::HashMap;
    use crate::model::{JSONValue, StrOrString};
    use super::{ParseError, parse};

    #[test]
    fn test_1() {
        assert_eq!(
            parse("[1, 2, 3]").collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![ Ok(JSONValue::Array(vec![ JSONValue::Integer(1), JSONValue::Integer(2), JSONValue::Integer(3) ])) ]
        )
    }

    #[test]
    fn test_2() {
        let mut target_hashmap = HashMap::new();
        target_hashmap.insert(StrOrString::Str("foo"), JSONValue::Array(vec![
            JSONValue::Integer(1),
            JSONValue::Float(2.3),
            JSONValue::Boolean(false),
            JSONValue::Null,
            JSONValue::String(""),
        ]));

        assert_eq!(
            parse("{ 
                \"foo\": [ 
                    1, 
                    2.3, 
                    false, 
                    null, 
                    \"\" 
                ] 
            }").collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::Object(target_hashmap))
            ]
        )
    }

    #[test]
    fn test_3() {
        let mut map_1 = HashMap::new();
        map_1.insert(StrOrString::Str("foo"), JSONValue::Integer(1));
        
        let mut map_2 = HashMap::new();
        map_2.insert(StrOrString::Str("foo"), JSONValue::Integer(2));
        
        let mut map_3 = HashMap::new();
        map_3.insert(StrOrString::Str("foo"), JSONValue::Integer(3));

        assert_eq!(
            parse("
                { \"foo\": 1 }
                { \"foo\": 2 }
                { \"foo\": 3 }
            ").collect::<Vec<Result<JSONValue,ParseError>>>(),
            vec![
                Ok(JSONValue::Object(map_1)),
                Ok(JSONValue::Object(map_2)),
                Ok(JSONValue::Object(map_3)),
            ]
        )
    }

    #[test]
    fn test_4() {
        assert_eq!(
            parse("12").collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::Integer(12))
            ]
        )
    }

    #[test]
    fn test_5() {
        assert_eq!(
            parse("\"hello \\\"world\\\"\"").collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::AllocatedString(String::from("hello \"world\"")))
            ]
        )
    }

    #[test]
    fn test_6() {
        assert_eq!(
            parse("\"hello \\\\ \\\"world\\\"\"").collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::AllocatedString(String::from("hello \\ \"world\"")))
            ]
        )
    }

    #[test]
    fn test_7() {
        assert_eq!(
            parse("{}").collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::Object(HashMap::new()))
            ]
        )
    }

    #[test]
    fn test_8() {
        assert_eq!(
            parse("[]").collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::Array(vec![]))
            ]
        )
    }
}