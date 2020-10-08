use std::fmt::Display;

use crate::model::Filter;



pub fn parse<'a>(code: &'a str) -> Result<Filter,ParseError> {
    let mut index = 0;
    filter(&tokenize(code).collect(), &mut index)
}


#[derive(Debug,PartialEq)]
pub struct ParseError<'a> {
    msg: String,
    token: Option<Token<'a>>,
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let location = match &self.token {
            Some(t) => format!("{}:{}", t.line_number, t.column),
            None => String::from("<EOF>"),
        };

        f.write_str(&format!(
            "ERROR {}, {}", 
            location, 
            self.msg
        ))
    }
}


#[derive(Debug,Clone,PartialEq)]
pub struct Token<'a> {
    pub line_number: i32,
    pub column: usize,
    pub lexeme: FilterLexeme<'a>,
}

#[derive(Debug,Clone,PartialEq)]
pub enum FilterLexeme<'a> {
    Special(&'a str),
    Identifier(&'a str),
    Index(usize),
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
                return Some(Token { line_number, column: index - line_start, lexeme: FilterLexeme::Special(*special) });
            }
        }

        // identifiers
        if ch == '"' {
            let content_length = match_pred(&code[index + 1..], |c| c != '"');
            let string_end_index = index + content_length + 1;

            skip_to = Some(string_end_index + 1);
            let string_content = &code[index + 1..string_end_index];
            return Some(Token { 
                line_number, 
                column: index - line_start, 
                lexeme: FilterLexeme::Identifier(string_content)
            });
        }

        if ch.is_alphabetic() || ch == '_' {
            let content_length = match_pred(&code[index..], |c| c.is_alphanumeric() || c == '_');
            let string_end_index = index + content_length;

            skip_to = Some(string_end_index);
            let string_content = &code[index..string_end_index];
            return Some(Token { 
                line_number, 
                column: index - line_start, 
                lexeme: FilterLexeme::Identifier(string_content)
            });
        }

        // indices
        if ch.is_numeric() {
            let front_end = index + match_pred(&code[index..], |c| c.is_numeric());

            skip_to = Some(front_end);
            return Some(Token { 
                line_number,
                column: index - line_start,
                lexeme: FilterLexeme::Index(code[index..front_end].parse().unwrap())
            });
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

const SPECIAL_TOKENS: [&str; 9] = ["{", "}", "[", "]", ",", "|", ":", ".", "?"];



fn filter<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {
   pipe(tokens, index)
}

fn pipe<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {
    let first = comma(tokens, index)?;

    if try_eat(tokens, index, "|").is_ok() {
        let second = comma(tokens, index)?;

        let mut sequence = vec![ first, second ];

        while try_eat(tokens, index, "|").is_ok() {
            let next = comma(tokens, index)?;

            sequence.push(next);
        }

        return Ok(Filter::Pipe(sequence));
    }

    return Ok(first);
}

fn comma<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {
    let first = identifier_chain(tokens, index)?;

    if try_eat(tokens, index, ",").is_ok() {
        let second = identifier_chain(tokens, index)?;

        let mut sequence = vec![ first, second ];

        while try_eat(tokens, index, ",").is_ok() {
            let next = identifier_chain(tokens, index)?;

            sequence.push(next);
        }

        return Ok(Filter::Comma(sequence));
    }

    return Ok(first);
}

fn identifier_chain<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {
    if try_eat(tokens, index, ".").is_ok() {
        if let Ok(first_identifier) = indexer(tokens, index) {
            if tokens.get(*index).map(|t| &t.lexeme) == Some(&FilterLexeme::Special("[")) || try_eat(tokens, index, ".").is_ok() {
                if let Ok(second_identifier) = indexer(tokens, index) {
                    let mut path = vec![ first_identifier, second_identifier ];

                    while tokens.get(*index).map(|t| &t.lexeme) == Some(&FilterLexeme::Special("[")) || try_eat(tokens, index, ".").is_ok() {
                        if let Ok(next) = indexer(tokens, index) {
                            path.push(next);
                        } else {
                            return Err(ParseError {
                                token: tokens.get(*index).cloned(),
                                msg: format!("Expected identifier after '.'"),
                            });
                        }
                    }

                    return Ok(Filter::Pipe(path));
                }
            } else {
                return Ok(first_identifier);
            }
        } else {
            return Ok(Filter::Identity);
        }
    }

    return json_literal(tokens, index);
}

fn json_literal<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {

    todo!();
    
    return Err(ParseError {
        msg: String::from("Expected filter expression"),
        token: None,
    });
}

fn indexer<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {
    if let Some(Token { lexeme: FilterLexeme::Identifier(s), column: _, line_number: _ }) = tokens.get(*index) {
        *index += 1;

        return Ok(Filter::ObjectIdentifierIndex { 
            identifier: s,
            optional: try_eat(tokens, index, "?").is_ok()
        });
    } else if try_eat(tokens, index, "[").is_ok() {
        let filter = match &tokens.get(*index) {
            Some(Token { lexeme: FilterLexeme::Identifier(s), column: _, line_number: _ }) => {
                *index += 1;

                Ok(Filter::ObjectIdentifierIndex { 
                    identifier: s,
                    optional: try_eat(tokens, index, "?").is_ok()
                })
            },
            Some(Token { lexeme: FilterLexeme::Index(start), column: _, line_number: _ }) => {
                *index += 1;

                if try_eat(tokens, index, ":").is_ok() {
                    let end = tokens.get(*index).map(|t| {
                        if let FilterLexeme::Index(i) = t.lexeme {
                            *index += 1;
                            Some(i)
                        } else {
                            None
                        }
                    }).flatten();
                    
                    Ok(Filter::Slice {
                        start: Some(*start),
                        end,
                    })
                } else {
                    Ok(Filter::ArrayIndex { 
                        index: *start
                    })
                }
            },
            Some(Token { lexeme: FilterLexeme::Special(":"), column: _, line_number: _ }) => {
                *index += 1;

                let end = tokens.get(*index).map(|t| {
                    if let FilterLexeme::Index(i) = t.lexeme {
                        *index += 1;
                        Some(i)
                    } else {
                        None
                    }
                }).flatten();
                
                Ok(Filter::Slice {
                    start: None,
                    end,
                })
            },
            Some(Token { lexeme: FilterLexeme::Special("]"), column: _, line_number: _ }) => {
                *index += 1;

                return Ok(Filter::AllValues); // return early!
            },
            token => Err(ParseError {
                msg: format!("Unexpected token: {:?}", &token),
                token: token.cloned(),
            })
        };

        try_eat(tokens, index, "]")?;

        return filter;
    } else {
        let token = tokens.get(*index).cloned();
        let found_str = match &token {
            Some(t) => format!("{:?}", t.lexeme),
            None => String::from("<EOF>"),
        };

        return Err(ParseError {
            msg: format!("Expected identifier, found {}", found_str),
            token,
        });
    }
}

fn try_eat<'a>(tokens: &Vec<Token<'a>>, index: &mut usize, expected: &str) -> Result<(), ParseError<'a>> {
    if *index > tokens.len() - 1 {
        return Err(ParseError { msg: format!("Expected '{}'", expected), token: tokens.get(*index - 1).cloned() });
    }

    if let Some(FilterLexeme::Special(found)) = tokens.get(*index).map(|t| &t.lexeme) {
        if *found == expected {
            *index += 1;
            Ok(())
        } else {
            return Err(ParseError { msg: format!("Expected '{}', found {}", expected, found), token: tokens.get(*index).cloned() });
        }
    } else {
        return Err(ParseError { msg: format!("Expected '{}'", expected), token: tokens.get(*index).cloned() });
    }
}


#[test]
fn test_1() {
    assert_eq!(parse(".foo"), Ok(Filter::ObjectIdentifierIndex { identifier: "foo", optional: false }));
}

#[test]
fn test_2() {
    assert_eq!(parse(".foo.bar"), Ok(Filter::Pipe(vec![ 
        Filter::ObjectIdentifierIndex { identifier: "foo", optional: false }, 
        Filter::ObjectIdentifierIndex { identifier: "bar", optional: false } 
    ])));
}

#[test]
fn test_3() {
    assert_eq!(parse(".foo.[]"), Ok(Filter::Pipe(vec![ 
        Filter::ObjectIdentifierIndex { identifier: "foo", optional: false }, 
        Filter::AllValues,
    ])));
}

#[test]
fn test_4() {
    assert_eq!(parse(".foo?.bar.blah"), Ok(Filter::Pipe(vec![ 
        Filter::ObjectIdentifierIndex { identifier: "foo", optional: true }, 
        Filter::ObjectIdentifierIndex { identifier: "bar", optional: false }, 
        Filter::ObjectIdentifierIndex { identifier: "blah", optional: false }, 
    ])));
}

#[test]
fn test_5() {
    assert_eq!(parse(".a | . | .b"), Ok(Filter::Pipe(vec![ 
        Filter::ObjectIdentifierIndex { identifier: "a", optional: false }, 
        Filter::Identity, 
        Filter::ObjectIdentifierIndex { identifier: "b", optional: false }, 
    ])));
}

#[test]
fn test_6() {
    assert_eq!(parse(". , .[] , .b"), Ok(Filter::Comma(vec![ 
        Filter::Identity, 
        Filter::AllValues, 
        Filter::ObjectIdentifierIndex { identifier: "b", optional: false }, 
    ])));
}

#[test]
fn test_7() {
    assert_eq!(parse(".[12] , .[1:5] , .[24:], .[:763], .[:]"), Ok(Filter::Comma(vec![
        Filter::ArrayIndex { index: 12 },
        Filter::Slice { start: Some(1), end: Some(5) },
        Filter::Slice { start: Some(24), end: None },
        Filter::Slice { start: None, end: Some(763) },
        Filter::Slice { start: None, end: None },
    ])));
}

#[test]
fn test_8() {
    assert_eq!(parse(".foo[2]"), Ok(Filter::Pipe(vec![
        Filter::ObjectIdentifierIndex { identifier: "foo", optional: false }, 
        Filter::ArrayIndex { index: 2 },
    ])));
}
