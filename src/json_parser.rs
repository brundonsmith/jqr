
use std::{collections::HashMap, fmt::Display};

use crate::model::JSONValue;


pub fn parse<'a>(code: &'a str) -> Result<JSONValue<'a>,ParseError> {
    let mut index = 0;
    expression(&tokenize(code).collect(), &mut index)
}

#[derive(Debug,PartialEq)]
pub struct ParseError<'a> {
    msg: String,
    token: Token<'a>,
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("ERROR {}:{}, {}", self.token.line_number, self.token.column, self.msg))
    }
}


#[derive(Debug,Clone,PartialEq)]
pub struct Token<'a> {
    pub line_number: i32,
    pub column: usize,
    pub lexeme: JSONLexeme<'a>,
}

#[derive(Debug,Clone,PartialEq)]
pub enum JSONLexeme<'a> {
    Special(&'a str),
    String(&'a str),
    Integer(i32),
    Float(f32),
    True,
    False,
    Null
}

fn tokenize<'a>(code: &'a str) -> impl Iterator<Item = Token<'a>> {
    let mut skip_to: Option<usize> = None;
    let mut line_number = 0;
    let mut line_start = 0;

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
            if ch == '\n' {
                line_number += 1;
                line_start = index;
            }

            return None;
        }

        // special tokens
        for special in &SPECIAL_TOKENS {
            if match_front(&code[index..], special) {
                skip_to = Some(index + special.len());
                return Some(Token { line_number, column: index - line_start, lexeme: JSONLexeme::Special(*special) });
            }
        }

        if match_front(&code[index..], "true") {
            skip_to = Some(index + "true".len());
            return Some(Token { line_number, column: index - line_start, lexeme: JSONLexeme::True });
        }

        if match_front(&code[index..], "false") {
            skip_to = Some(index + "false".len());
            return Some(Token { line_number, column: index - line_start, lexeme: JSONLexeme::False });
        }

        if match_front(&code[index..], "null") {
            skip_to = Some(index + "null".len());
            return Some(Token { line_number, column: index - line_start, lexeme: JSONLexeme::Null });
        }

        // strings
        if ch == '"' {
            let content_length = match_pred(&code[index + 1..], |c| c != '"');
            let string_end_index = index + content_length + 1;

            skip_to = Some(string_end_index + 1);
            let string_content = &code[index + 1..string_end_index];
            return Some(Token { 
                line_number, 
                column: index - line_start, 
                lexeme: JSONLexeme::String(string_content) 
            });
        }

        // comments
        // if ch == ';' && code[index+1..].chars().next().map_or(false, |c| c == ';') {
        //   let comment_end = index + 2 + match_pred(&code[index+2..], |c| c != '\n').unwrap_or(0);

        //   skip_to = Some(comment_end + 1);
        //   return None;
        // }

        // numbers
        if ch.is_numeric() {
            let front_end = index + match_pred(&code[index..], |c| c.is_numeric());

            let column = index - line_start;

            if front_end < code.len() - 1 && &code[front_end..front_end + 1] == "." {
                let back_end =
                    front_end + 1 + match_pred(&code[front_end + 1..], |c| c.is_numeric());

                skip_to = Some(back_end);
                return Some(Token {
                    line_number,
                    column,
                    lexeme: JSONLexeme::Float(code[index..back_end].parse().unwrap())
                });
            } else {
                skip_to = Some(front_end);
                return Some(Token {
                    line_number,
                    column,
                    lexeme: JSONLexeme::Integer(code[index..front_end].parse().unwrap())
                });
            }
        }

        return None;
    })
}

fn is_symbol(c: char) -> bool {
    !c.is_numeric()
        && !c.is_whitespace()
        && !SPECIAL_TOKENS
            .iter()
            .any(|t| t.chars().any(|other| other == c))
}

fn match_front(code: &str, segment: &str) -> bool {
    segment.chars().zip(code.chars()).all(|(a, b)| a == b)
}

fn match_pred<F: Fn(char) -> bool>(code: &str, pred: F) -> usize {
    code.char_indices()
        .take_while(|(_, c)| pred(*c))
        .last()
        .map(|(index, _)| index + 1)
        .unwrap_or(0)
}

#[test]
fn match_pred_test_1() {
    assert_eq!(match_pred("foobar", |c| c != 'b'), 3);
}
#[test]
fn match_pred_test_2() {
    assert_eq!(match_pred("foobar", |c| c != 'f'), 0);
}

const SPECIAL_TOKENS: [&str; 6] = ["{", "}", "[", "]", ",", ":"];

#[test]
fn tokenize_test() {
    for token in tokenize("{ \"foo\": [ 1, 2.3, false, null, \"\" ] }") {
        println!("{:?}", &token);
    }
}

fn expression<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<JSONValue<'a>, ParseError<'a>> {
    let next = &tokens[*index];
    *index += 1;

    match next.lexeme {
        JSONLexeme::String(s) => Ok(JSONValue::String(s)),
        JSONLexeme::Integer(n) => Ok(JSONValue::Integer(n)),
        JSONLexeme::Float(n) => Ok(JSONValue::Float(n)),
        JSONLexeme::True => Ok(JSONValue::Boolean(true)),
        JSONLexeme::False => Ok(JSONValue::Boolean(false)),
        JSONLexeme::Null => Ok(JSONValue::Null),
        JSONLexeme::Special("{") => object_body(tokens, index),
        JSONLexeme::Special("[") => array_body(tokens, index),
        JSONLexeme::Special(t) => Err(ParseError { msg: format!("Unexpected token '{}'", t), token: next.clone() }),//panic!(format!("ERROR: Unexpected token {}", t)),
    }
}

fn object_body<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<JSONValue<'a>, ParseError<'a>> {
    let mut contents = HashMap::new();

    if let Ok(JSONValue::String(prop)) = expression(tokens, index) {
        eat(tokens, index, ":")?;
        let value = expression(tokens, index)?;

        contents.insert(prop, value);

        while let Ok(_) = eat(tokens, index, ",") {
            if let Ok(JSONValue::String(prop)) = expression(tokens, index) {
                eat(tokens, index, ":")?;
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
    eat(tokens, index, "}")?;
    
    return Ok(JSONValue::Object(contents));
}

fn array_body<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<JSONValue<'a>, ParseError<'a>> {
    let mut contents = Vec::new();

    if let Ok(value) = expression(tokens, index) {
        contents.push(value);

        while let Ok(_) = eat(tokens, index, ",") {
            let value = expression(tokens, index)?;
            contents.push(value);
        }
    }
    eat(tokens, index, "]")?;

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

fn eat<'a>(tokens: &Vec<Token<'a>>, index: &mut usize, expected: &str) -> Result<(), ParseError<'a>> {
    if let JSONLexeme::Special(found) = tokens[*index].lexeme {
        if found == expected {
            *index += 1;
            Ok(())
        } else {
            return Err(ParseError { msg: format!("Expected '{}', found {}", expected, found), token: tokens[*index].clone() });
        }
    } else {
        return Err(ParseError { msg: format!("Expected '{}'", expected), token: tokens[*index].clone() });
    }
}

#[test]
fn test_1() {
    assert_eq!(parse("[1, 2, 3]"), Ok(JSONValue::Array(vec![ JSONValue::Integer(1), JSONValue::Integer(2), JSONValue::Integer(3) ])))
}

#[test]
fn test_2() {
    let mut target_hashmap = HashMap::new();
    target_hashmap.insert("foo", JSONValue::Array(vec![
        JSONValue::Integer(1),
        JSONValue::Float(2.3),
        JSONValue::Boolean(false),
        JSONValue::Null,
        JSONValue::String(""),
    ]));

    println!("{}", &parse("{ 
        \"foo\": [ 
            1, 
            2.3, 
            false, 
            null, 
            \"\" 
        ] 
    }").unwrap());

    assert_eq!(parse("{ 
        \"foo\": [ 
            1, 
            2.3, 
            false, 
            null, 
            \"\" 
        ] 
    }"), Ok(JSONValue::Object(target_hashmap)))
}