
use std::{collections::HashMap};

use crate::model::{JSONValue, StrOrString};


pub fn parse<'a>(code: &'a str) -> impl Iterator<Item=Result<JSONValue<'a>,ParseError>> {
    let tokens: Vec<Token> = tokenize(code).collect();
    let mut index = 0;

    std::iter::from_fn(move || {
        if index < tokens.len() {
            Some(expression(&tokens, &mut index))
        } else {
            None
        }
    })
}

#[derive(Debug,PartialEq)]
pub struct ParseError<'a> {
    pub msg: String,
    pub token: Token<'a>,
}



// tokenizing 
#[derive(Debug,Clone,PartialEq)]
pub struct Token<'a> {
    pub index: usize,
    pub lexeme: JSONLexeme<'a>,
}

#[derive(Debug,Clone,PartialEq)]
pub enum JSONLexeme<'a> {
    Special(&'static char),
    String(&'a str),
    AllocatedString(String),
    Integer(i32),
    Float(f32),
    Boolean(bool),
    Null
}

fn tokenize<'a>(code: &'a str) -> impl Iterator<Item = Token<'a>> {
    let mut skip_to: Option<usize> = None;

    code.char_indices().filter_map(move |(index, ch)| {
        if skip_to
            .map(|destination| destination > index)
            .unwrap_or(false)
        {
            return None;
        } else {
            skip_to = None;
        }

        // whitespace
        if ch.is_whitespace() {
            return None;
        }

        // strings
        if ch == '"' {
            let mut escape_next = false;
            let mut allocated_string: Option<String> = None;
            let contents_start = index + 1;
            let mut end = contents_start;
            for (i, c) in code[index + 1..].char_indices() {
                let current_index = contents_start + i;
                if c == '\\' && !escape_next {
                    escape_next = true;
                    if allocated_string.is_none() {
                        allocated_string = Some(String::from(&code[contents_start..current_index]));
                    }
                } else {
                    if c == '"' && !escape_next {
                        end = contents_start + i;
                        skip_to = Some(current_index + 1);
                        break;
                    }
                    
                    match &mut allocated_string {
                        Some(s) => s.push(c),
                        None => {},
                    };

                    escape_next = false;
                }
            }

            return Some(Token { 
                index,
                lexeme: match allocated_string {
                    Some(s) => JSONLexeme::AllocatedString(s),
                    None => JSONLexeme::String(&code[index + 1..end]),
                }
            });
        }

        // special tokens
        for special in &SPECIAL_TOKENS {
            if code[index..].chars().nth(0).as_ref() == Some(special) {
                skip_to = Some(index + 1);
                return Some(Token { index, lexeme: JSONLexeme::Special(special) });
            }
        }

        if match_front(&code[index..], TRUE_TOKEN) {
            skip_to = Some(index + TRUE_TOKE_LEN);
            return Some(Token { index, lexeme: JSONLexeme::Boolean(true) });
        }

        if match_front(&code[index..], FALSE_TOKEN) {
            skip_to = Some(index + FALSE_TOKE_LEN);
            return Some(Token { index, lexeme: JSONLexeme::Boolean(false) });
        }

        if match_front(&code[index..], NULL_TOKEN) {
            skip_to = Some(index + NULL_TOKE_LEN);
            return Some(Token { index, lexeme: JSONLexeme::Null });
        }

        // numbers
        if ch.is_numeric() {
            let front_end = index + code[index..].char_indices()
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

                skip_to = Some(back_end);
                return Some(Token {
                    index,
                    lexeme: JSONLexeme::Float(code[index..back_end].parse().unwrap())
                });
            } else {
                skip_to = Some(front_end);
                return Some(Token {
                    index,
                    lexeme: JSONLexeme::Integer(code[index..front_end].parse().unwrap())
                });
            }
        }

        return None;
    })
}

fn match_front(code: &str, segment: &str) -> bool {
    segment.chars().zip(code.chars()).all(|(a, b)| a == b)
}

const SPECIAL_TOKENS: [char; 6] = ['{', '}', '[', ']', ',', ':'];
const TRUE_TOKEN: &str = "true";
const TRUE_TOKE_LEN: usize = TRUE_TOKEN.len();
const FALSE_TOKEN: &str = "false";
const FALSE_TOKE_LEN: usize = FALSE_TOKEN.len();
const NULL_TOKEN: &str = "null";
const NULL_TOKE_LEN: usize = NULL_TOKEN.len();


// parsing
fn expression<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<JSONValue<'a>, ParseError<'a>> {
    let next = &tokens[*index];
    *index += 1;

    match &next.lexeme {
        JSONLexeme::String(s) => Ok(JSONValue::String(s)),
        JSONLexeme::AllocatedString(s) => Ok(JSONValue::AllocatedString(s.clone())),
        JSONLexeme::Integer(n) => Ok(JSONValue::Integer(*n)),
        JSONLexeme::Float(n) => Ok(JSONValue::Float(*n)),
        JSONLexeme::Boolean(b) => Ok(JSONValue::Boolean(*b)),
        JSONLexeme::Null => Ok(JSONValue::Null),
        JSONLexeme::Special('{') => object_body(tokens, index),
        JSONLexeme::Special('[') => array_body(tokens, index),
        JSONLexeme::Special(t) => Err(ParseError { msg: format!("Unexpected token '{}'", t), token: next.clone() }),
    }
}

fn object_body<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<JSONValue<'a>, ParseError<'a>> {
    if try_eat(tokens, index, &'}').is_ok() {
        return Ok(JSONValue::Object(HashMap::new()));
    }

    let mut contents = HashMap::new();

    let key = expression(tokens, index);

    if let Some(prop) = consume_str_or_string(key) {
        try_eat(tokens, index, &':')?;
        let value = expression(tokens, index)?;

        contents.insert(prop, value);

        while try_eat(tokens, index, &',').is_ok() {
            let key = expression(tokens, index);

            if let Some(prop) = consume_str_or_string(key) {
                try_eat(tokens, index, &':')?;
                let value = expression(tokens, index)?;

                contents.insert(prop, value);
            } else {
                return Err(ParseError {
                    msg: String::from("Expected object property after ','"),
                    token: tokens[*index].clone(),
                });
            }
        }
    }
    try_eat(tokens, index, &'}')?;
    
    return Ok(JSONValue::Object(contents));
}

fn consume_str_or_string<'a>(next: Result<JSONValue<'a>, ParseError>) -> Option<StrOrString<'a>> {
    if let Ok(JSONValue::String(prop)) = next {
        return Some(StrOrString::Str(prop));
    } else if let Ok(JSONValue::AllocatedString(prop)) = next {
        return Some(StrOrString::String(prop));
    } else {
        return None;
    }
}

fn array_body<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<JSONValue<'a>, ParseError<'a>> {
    if try_eat(tokens, index, &']').is_ok() {
        return Ok(JSONValue::Array(vec![]));
    }

    let mut contents = Vec::new();

    if let Ok(value) = expression(tokens, index) {
        contents.push(value);

        while try_eat(tokens, index, &',').is_ok() {
            let value = expression(tokens, index)?;
            contents.push(value);
        }
    }
    try_eat(tokens, index, &']')?;

    return Ok(JSONValue::Array(contents));
}

fn property_name<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<&'a str, ParseError<'a>> {
    if let JSONLexeme::String(prop) = tokens[*index].lexeme {
        *index += 1;
        return Ok(prop);
    } else {
        return Err(ParseError { msg: format!("Expected object property name"), token: tokens[*index].clone() });
    }
}

fn try_eat<'a>(tokens: &Vec<Token<'a>>, index: &mut usize, expected: &char) -> Result<(), ParseError<'a>> {
    if let JSONLexeme::Special(found) = tokens[*index].lexeme {
        if found == expected {
            *index += 1;
            Ok(())
        } else {
            return Err(ParseError { msg: format!("Expected '{}', found '{}'", expected, found), token: tokens[*index].clone() });
        }
    } else {
        return Err(ParseError { msg: format!("Expected '{}'", expected), token: tokens[*index].clone() });
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