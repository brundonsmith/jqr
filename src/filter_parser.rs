use std::fmt::Display;

use crate::{filter_model::Filter, json_model::JSONValue};



pub fn parse<'a>(code: &'a str) -> Result<Filter,ParseError> {
    let mut index = 0;
    filter(&tokenize(code).collect(), &mut index).map(|f| optimize(&f))
}


#[derive(Debug,PartialEq)]
pub struct ParseError<'a> {
    msg: String,
    token: Option<Token<'a>>,
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let location = match &self.token {
            Some(t) => format!("{}:{}", t.line_number, t.column + 1),
            None => String::from("<EOF>"),
        };

        f.write_str(&format!(
            "Error parsing JSON at {} - {}", 
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
    Identifier { s: &'a str, quoted: bool },
    Number(usize),
}

fn tokenize<'a>(code: &'a str) -> impl Iterator<Item = Token<'a>> {
    let mut skip_to: Option<usize> = None;
    let mut line_number = 1;
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
                lexeme: FilterLexeme::Identifier { s: string_content, quoted: true }
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
                lexeme: FilterLexeme::Identifier { s: string_content, quoted: false }
            });
        }

        // indices
        if ch.is_numeric() {
            let front_end = index + match_pred(&code[index..], |c| c.is_numeric());

            skip_to = Some(front_end);
            return Some(Token { 
                line_number,
                column: index - line_start,
                lexeme: FilterLexeme::Number(code[index..front_end].parse().unwrap())
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

const SPECIAL_TOKENS: [&str; 26] = ["{", "}", "[", "]", "(", ")", ",", "|", ":", 
".", "?", "+", "-", "*", "//", "/", "%", "==", "!=", ">", ">=", "<", "<=", "and", 
"or", "not"];



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
    let first = operation_tier_1(tokens, index)?;

    if try_eat(tokens, index, ",").is_ok() {
        let second = operation_tier_1(tokens, index)?;

        let mut sequence = vec![ first, second ];

        while try_eat(tokens, index, ",").is_ok() {
            let next = operation_tier_1(tokens, index)?;

            sequence.push(next);
        }

        return Ok(Filter::Comma(sequence));
    }

    return Ok(first);
}


macro_rules! match_one {
    ($alltokens:ident, $index:ident, $first_token:literal, $($tokens:literal),*) => {
        $alltokens.get(*$index).map(|t| &t.lexeme) == Some(&FilterLexeme::Special($first_token))
        $(|| $alltokens.get(*$index).map(|t| &t.lexeme) == Some(&FilterLexeme::Special($tokens)))*
    };
}

fn operation_tier_1<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {
    let mut left = operation_tier_2(tokens, index)?;

    while match_one!(tokens, index, "and", "or", "//") {
        if let Some(FilterLexeme::Special(special)) = tokens.get(*index).map(|t| &t.lexeme) { // always true
            *index += 1;
            let right = operation_tier_2(tokens, index)?;
            left = op_filter_from_special(special, left, right);
        }
    }

    return Ok(left);
}

fn operation_tier_2<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {
    let mut left = operation_tier_3(tokens, index)?;

    while match_one!(tokens, index, "<", "<=", ">", ">=") {
        if let Some(FilterLexeme::Special(special)) = tokens.get(*index).map(|t| &t.lexeme) { // always true
            *index += 1;
            let right = operation_tier_3(tokens, index)?;
            left = op_filter_from_special(special, left, right);
        }
    }

    return Ok(left);
}

fn operation_tier_3<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {
    let mut left = operation_tier_4(tokens, index)?;

    while match_one!(tokens, index, "+", "-") {
        if let Some(FilterLexeme::Special(special)) = tokens.get(*index).map(|t| &t.lexeme) { // always true
            *index += 1;
            let right = operation_tier_4(tokens, index)?;
            left = op_filter_from_special(special, left, right);
        }
    }

    return Ok(left);
}

fn operation_tier_4<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {
    let mut left = function(tokens, index)?;

    while match_one!(tokens, index, "*", "/", "%") {
        if let Some(FilterLexeme::Special(special)) = tokens.get(*index).map(|t| &t.lexeme) { // always true
            *index += 1;
            let right = function(tokens, index)?;
            left = op_filter_from_special(special, left, right);
        }
    }

    return Ok(left);
}

fn op_filter_from_special<'a>(special: &'a str, left: Filter<'a>, right: Filter<'a>) -> Filter<'a> {
    match special {
        "and" => Filter::And { left: Box::new(left), right: Box::new(right) },
        "or" => Filter::Or { left: Box::new(left), right: Box::new(right) },
        "<" => Filter::LessThan { left: Box::new(left), right: Box::new(right) },
        "<=" => Filter::LessThanOrEqual { left: Box::new(left), right: Box::new(right) },
        ">" => Filter::GreaterThan { left: Box::new(left), right: Box::new(right) },
        ">=" => Filter::GreaterThanOrEqual { left: Box::new(left), right: Box::new(right) },
        "+" => Filter::Add { left: Box::new(left), right: Box::new(right) },
        "-" => Filter::Subtract { left: Box::new(left), right: Box::new(right) },
        "*" => Filter::Multiply { left: Box::new(left), right: Box::new(right) },
        "/" => Filter::Divide { left: Box::new(left), right: Box::new(right) },
        "%" => Filter::Modulo { left: Box::new(left), right: Box::new(right) },
        "//" => Filter::Alternative { left: Box::new(left), right: Box::new(right) },
        _ => unreachable!(),
    }
}

fn function<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {
    if let Some(FilterLexeme::Identifier { s: func, quoted: false }) = tokens.get(*index).map(|t| &t.lexeme) {
        *index += 1;

        return match *func {
            "length" => Ok(Filter::Length),
            "keys" => Ok(Filter::Keys),
            "keys_unsorted" => Ok(Filter::KeysUnsorted),
            "not" => Ok(Filter::Not),
            "sort" => Ok(Filter::Sort),
            _ => {
                try_eat(tokens, index, "(")?;

                let inner = filter(tokens, index)?;

                try_eat(tokens, index, ")")?;

                match *func {
                    "map" => Ok(Filter::Map(Box::new(inner))),
                    "select" => Ok(Filter::Select(Box::new(inner))),
                    "sort_by" => Ok(Filter::SortBy(Box::new(inner))),
                    "has" => {
                        if let Filter::Literal(key) = inner {
                            Ok(Filter::Has(key))
                        } else {
                            Err(ParseError {
                                token: tokens.get(*index - 1).cloned(),
                                msg: format!("Expected string or number inside has(); got '{:?}'", inner),
                            })
                        }
                    },
                    _ => Err(ParseError {
                        token: tokens.get(*index - 1).cloned(),
                        msg: format!("Function '{}' is unknown", func),
                    })
                }
            }
        }
    }

    return identifier_chain(tokens, index);
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

fn indexer<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {
    if let Some(FilterLexeme::Identifier { s, quoted: _ }) = tokens.get(*index).map(|t| &t.lexeme) {
        *index += 1;

        return Ok(Filter::ObjectIdentifierIndex { 
            identifier: s,
            optional: try_eat(tokens, index, "?").is_ok()
        });
    } else if try_eat(tokens, index, "[").is_ok() {
        let filter = match &tokens.get(*index).map(|t| &t.lexeme) {
            Some(FilterLexeme::Identifier { s, quoted: _ }) => {
                *index += 1;

                Ok(Filter::ObjectIdentifierIndex {
                    identifier: s,
                    optional: try_eat(tokens, index, "?").is_ok()
                })
            },
            Some(FilterLexeme::Number(start)) => {
                *index += 1;

                if try_eat(tokens, index, ":").is_ok() {
                    let end = tokens.get(*index).map(|t| {
                        if let FilterLexeme::Number(i) = t.lexeme {
                            *index += 1;
                            Some(i)
                        } else {
                            None
                        }
                    }).flatten();
                    
                    Ok(Filter::Slice {
                        start: Some(*start),
                        end,
                        optional: try_eat(tokens, index, "?").is_ok(),
                    })
                } else {
                    Ok(Filter::ArrayIndex { 
                        index: *start
                    })
                }
            },
            Some(FilterLexeme::Special(":")) => {
                *index += 1;

                let end = tokens.get(*index).map(|t| {
                    if let FilterLexeme::Number(i) = t.lexeme {
                        *index += 1;
                        Some(i)
                    } else {
                        None
                    }
                }).flatten();
                
                Ok(Filter::Slice {
                    start: None,
                    end,
                    optional: try_eat(tokens, index, "?").is_ok(),
                })
            },
            Some(FilterLexeme::Special("]")) => {
                *index += 1;

                return Ok(Filter::Slice {
                    start: None,
                    end: None,
                    optional: try_eat(tokens, index, "?").is_ok(),
                }); // return early!
            },
            token => Err(ParseError {
                msg: format!("Unexpected token: {:?}", &token),
                token: tokens.get(*index).cloned(),
            })
        };

        try_eat(tokens, index, "]")?;

        return filter;
    }

    let token = tokens.get(*index).cloned();
    let found_str = match &token {
        Some(t) => format!("{:?}", t.lexeme),
        None => String::from("<EOF>"),
    };

    return Err(ParseError {
        msg: format!("Expected identifier, found '{}'", found_str),
        token,
    });
}

fn json_literal<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {

    if let Some(FilterLexeme::Number(n)) = tokens.get(*index).map(|t| &t.lexeme) {
        *index += 1;
        return Ok(Filter::Literal(JSONValue::Integer(*n as i32)));
    }

    if let Some(FilterLexeme::Identifier { s, quoted: true}) = tokens.get(*index).map(|t| &t.lexeme) {
        *index += 1;
        return Ok(Filter::Literal(JSONValue::String { s: *s, needs_escaping: false }));
    }
    
    parenthesis(tokens, index)
}

fn parenthesis<'a>(tokens: &Vec<Token<'a>>, index: &mut usize) -> Result<Filter<'a>, ParseError<'a>> {

    if try_eat(tokens, index, "(").is_ok() {
        let exp = filter(tokens, index);
        try_eat(tokens, index, ")")?;

        return exp;
    }

    return Err(ParseError {
        msg: String::from("Expected filter expression"),
        token: None,
    });
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
            return Err(ParseError { msg: format!("Expected '{}', found '{}'", expected, found), token: tokens.get(*index).cloned() });
        }
    } else {
        return Err(ParseError { msg: format!("Expected '{}'", expected), token: tokens.get(*index).cloned() });
    }
}


fn optimize<'a>(filter: &Filter<'a>) -> Filter<'a> {
    match filter {
        Filter::Map(inner) => Filter::Map(Box::new(optimize(inner))),
        Filter::Select(inner) => Filter::Select(Box::new(optimize(inner))),
        Filter::Comma(vec) => Filter::Comma(vec.iter().map(optimize).collect()),
        Filter::GreaterThan { left, right } => Filter::GreaterThan { left: Box::new(optimize(left)), right: Box::new(optimize(right)) },

        // Filter::Pipe(vec) => {
        //     if vec.iter().all(|f| matches!(f, Filter::ObjectIdentifierIndex { identifier: _, optional: _ })) {
        //         Filter::_PropertyChain(vec.into_iter().map(|f| {
        //             if let Filter::ObjectIdentifierIndex { identifier, optional } = f {
        //                 (*identifier, *optional)
        //             } else {
        //                 unreachable!()
        //             }
        //         }).collect())
        //     } else {
        //         Filter::Pipe(vec.iter().map(optimize).collect())
        //     }
        // },

        // Filter::Map(inner) => 
        //     match inner.as_ref() {
        //         Filter::Select(pred) => {
        //             println!("_MapSelect added");
        //             Filter::_MapSelect(Box::new(optimize(pred)))
        //         },
        //         _ => filter.clone()
        //     },

        _ => filter.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::filter_model::Filter;
    use crate::filter_parser::parse;
    use crate::json_model::JSONValue;

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
            Filter::Slice { start: None, end: None, optional: false },
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
            Filter::Slice { start: None, end: None, optional: false }, 
            Filter::ObjectIdentifierIndex { identifier: "b", optional: false }, 
        ])));
    }

    #[test]
    fn test_7() {
        assert_eq!(parse(".[12] , .[1:5] , .[24:], .[:763], .[:]"), Ok(Filter::Comma(vec![
            Filter::ArrayIndex { index: 12 },
            Filter::Slice { start: Some(1), end: Some(5), optional: false },
            Filter::Slice { start: Some(24), end: None, optional: false },
            Filter::Slice { start: None, end: Some(763), optional: false },
            Filter::Slice { start: None, end: None, optional: false },
        ])));
    }

    #[test]
    fn test_8() {
        assert_eq!(parse(".foo[2]"), Ok(Filter::Pipe(vec![
            Filter::ObjectIdentifierIndex { identifier: "foo", optional: false }, 
            Filter::ArrayIndex { index: 2 },
        ])));
    }
    
    #[test]
    fn test_9() {
        assert_eq!(
            parse("map(.foo)"), 
            Ok(Filter::Map(Box::new(Filter::ObjectIdentifierIndex { identifier: "foo", optional: false })))
        );
    }

    #[test]
    fn test_10() {
        assert_eq!(
            parse(".second.store.books | map(.price + 10)"),
            Ok(Filter::Pipe(vec![
                Filter::Pipe(vec![
                    Filter::ObjectIdentifierIndex { identifier: "second", optional: false }, 
                    Filter::ObjectIdentifierIndex { identifier: "store", optional: false }, 
                    Filter::ObjectIdentifierIndex { identifier: "books", optional: false }
                ]), 
                Filter::Map(
                    Box::new(Filter::Add { 
                        left: Box::new(Filter::ObjectIdentifierIndex { identifier: "price", optional: false }),
                        right: Box::new(Filter::Literal(JSONValue::Integer(10)))
                    })
                )
            ]))
        );
    }
}