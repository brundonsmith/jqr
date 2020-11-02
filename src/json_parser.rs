
use std::{collections::HashMap, rc::Rc};
use crate::json_model::JSONValue;

#[derive(Debug,PartialEq)]
pub struct ParseError {
    pub msg: String,
    pub index: usize,
}

const TRUE_TOKEN: &str = "true";
const FALSE_TOKEN: &str = "false";
const NULL_TOKEN: &str = "null";


// parsing
pub fn parse<'a>(code: &'a str, no_free: bool) -> impl Iterator<Item=Result<JSONValue<'a>,ParseError>> {
    let mut index = 0;

    consume_whitespace(code, &mut index);
    std::iter::from_fn(move || {
        if index < code.len() {
            let res = Some(expression(code, &mut index, no_free));
            consume_whitespace(code, &mut index);
            res
        } else {
            None
        }
    })
}

fn expression<'a>(code: &'a str, index: &mut usize, no_free: bool) -> Result<JSONValue<'a>, ParseError> {
    consume_whitespace(code, index);
    let ch = code[*index..].chars().next();

    if ch == Some('"') {
        string(code, index)
    } else if ch == Some('{') {
        object(code, index, no_free)
    } else if ch == Some('[') {
        array(code, index, no_free)
    } else if ch.map(|c| c.is_numeric()).unwrap_or(false) {
        number(code, index)
    } else if try_match_front(code, index, TRUE_TOKEN) {
        Ok(JSONValue::Bool(true))
    } else if try_match_front(code, index, FALSE_TOKEN) {
        Ok(JSONValue::Bool(false))
    } else if try_match_front(code, index, NULL_TOKEN) {
        Ok(JSONValue::Null)
    } else {
        Err(ParseError {
            msg: String::from("Expected JSON value"),
            index: *index
        })
    }
}

fn object<'a>(code: &'a str, index: &mut usize, no_free: bool) -> Result<JSONValue<'a>, ParseError> {
    let str_start = *index;

    try_eat(code, index, &'{')?;

    let mut contents = HashMap::new();

    if try_eat(code, index, &'}').is_ok() {
        return Ok(JSONValue::Object(Rc::new((contents, Some(&code[str_start..*index])))));
    } else {
        let key = expression(code, index, no_free);

        if let Ok(prop) = key {
            try_eat(code, index, &':')?;
            let value = expression(code, index, no_free)?;

            if no_free {
                std::mem::forget(prop.clone());
                std::mem::forget(value.clone());
            }

            contents.insert(prop, value);

            while try_eat(code, index, &',').is_ok() {
                let key = expression(code, index, no_free);

                if let Ok(prop) = key {
                    try_eat(code, index, &':')?;
                    let value = expression(code, index, no_free)?;

                    if no_free {
                        std::mem::forget(prop.clone());
                        std::mem::forget(value.clone());
                    }

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
        
        return Ok(JSONValue::Object(Rc::new((contents, Some(&code[str_start..*index])))));
    }
}

/// Parsing into a HashMap is more efficient for lookup, but when writing an 
/// object back out as a string, we need to maintain the original property
/// order, which is lost in a HashMap. So what we do is hang on to a reference
/// to the original JSON code, and re-parse it as a sequence of key/value pairs
/// when we need to reserialize to a string.
pub fn object_entries<'a>(code: &'a str) -> Result<Vec<(JSONValue<'a>, JSONValue<'a>)>, ParseError> {
    // TODO: Come up with a way to share most of this logic with json_parser::object()

    let mut _index = 0;
    let index = &mut _index;

    try_eat(code, index, &'{')?;

    let mut entries = Vec::new();

    if try_eat(code, index, &'}').is_ok() {
        return Ok(entries);
    } else {
        let key = expression(code, index, false);

        if let Ok(prop) = key {
            try_eat(code, index, &':')?;
            let value = expression(code, index, false)?;

            entries.push((prop, value));

            while try_eat(code, index, &',').is_ok() {
                let key = expression(code, index, false);

                if let Ok(prop) = key {
                    try_eat(code, index, &':')?;
                    let value = expression(code, index, false)?;

                    entries.push((prop, value));
                } else {
                    return Err(ParseError {
                        msg: String::from("Expected object property after ','"),
                        index: *index,
                    });
                }
            }
        }
        try_eat(code, index, &'}')?;
        
        return Ok(entries);
    }
}

fn array<'a>(code: &'a str, index: &mut usize, no_free: bool) -> Result<JSONValue<'a>, ParseError> {
    try_eat(code, index, &'[')?;

    let mut contents = Vec::new();

    if try_eat(code, index, &']').is_ok() {
        return Ok(JSONValue::Array(Rc::new(contents)));
    } else {
        let value = expression(code, index, no_free)?;

        if no_free {
            std::mem::forget(value.clone());
        }

        contents.push(value);

        while try_eat(code, index, &',').is_ok() {

            let value = expression(code, index, no_free)?;

            if no_free {
                std::mem::forget(value.clone());
            }

            contents.push(value);
        }

        try_eat(code, index, &']')?;

        return Ok(JSONValue::Array(Rc::new(contents)));
    }
}

fn string<'a>(code: &'a str, index: &mut usize) -> Result<JSONValue<'a>, ParseError> {
    let string_contents_start = *index + 1;

    let mut needs_escaping = false;
    let mut escape_next = false;
    let mut end = string_contents_start;
    for (i, c) in code[string_contents_start..].char_indices() {
        if !escape_next {
            if c == '\\' {
                escape_next = true;
                needs_escaping = true;
            } else if c == '"' {
                end = string_contents_start + i;
                *index = end + 1;
                break;
            }
        } else {
            escape_next = false;
        }
    }

    Ok(JSONValue::String { s: &code[string_contents_start..end], needs_escaping })
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
    if Some(*expected) == code[*index..].chars().next() {
        *index += expected.len_utf8();
        Ok(())
    } else {
        return Err(ParseError { 
            msg: format!("Expected '{}'", expected), 
            index: *index
        });
    }
}

fn try_match_front(code: &str, index: &mut usize, segment: &str) -> bool {
    consume_whitespace(code, index);
    let rest = &code[*index..];
    if segment.len() <= rest.len() && segment.chars().zip(rest.chars()).all(|(a, b)| a == b) {
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
    use std::{collections::HashMap, rc::Rc};
    use crate::json_model::{JSONValue};
    use crate::json_parser::{ParseError, parse};

    #[test]
    fn test_1() {
        assert_eq!(
            parse("[1, 2, 3]", false).collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![ Ok(JSONValue::Array(Rc::new(vec![ JSONValue::Integer(1), JSONValue::Integer(2), JSONValue::Integer(3) ]))) ]
        )
    }

    #[test]
    fn test_2() {
        let mut target_hashmap = HashMap::new();
        target_hashmap.insert(JSONValue::String { s: "foo", needs_escaping: false }, JSONValue::Array(Rc::new(vec![
            JSONValue::Integer(1),
            JSONValue::Float(2.3),
            JSONValue::Bool(false),
            JSONValue::Null,
            JSONValue::String { s: "", needs_escaping: false },
        ])));

        let json = "{ 
            \"foo\": [ 
                1, 
                2.3, 
                false, 
                null, 
                \"\" 
            ] 
        }";

        assert_eq!(
            parse(json, false).collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::Object(Rc::new((target_hashmap, Some(json)))))
            ]
        )
    }

    #[test]
    fn test_3() {
        let mut map_1 = HashMap::new();
        map_1.insert(JSONValue::String { s: "foo", needs_escaping: false }, JSONValue::Integer(1));
        
        let mut map_2 = HashMap::new();
        map_2.insert(JSONValue::String { s: "foo", needs_escaping: false }, JSONValue::Integer(2));
        
        let mut map_3 = HashMap::new();
        map_3.insert(JSONValue::String { s: "foo", needs_escaping: false }, JSONValue::Integer(3));

        assert_eq!(
            parse("
                { \"foo\": 1 }
                { \"foo\": 2 }
                { \"foo\": 3 }
            ", false).collect::<Vec<Result<JSONValue,ParseError>>>(),
            vec![
                Ok(JSONValue::Object(Rc::new((map_1, Some("{ \"foo\": 1 }"))))),
                Ok(JSONValue::Object(Rc::new((map_2, Some("{ \"foo\": 2 }"))))),
                Ok(JSONValue::Object(Rc::new((map_3, Some("{ \"foo\": 3 }"))))),
            ]
        )
    }

    #[test]
    fn test_4() {
        assert_eq!(
            parse("12", false).collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::Integer(12))
            ]
        )
    }

    #[test]
    fn test_5() {
        assert_eq!(
            parse("\"hello \\\"world\\\"\"", false).collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::String { s: "hello \\\"world\\\"", needs_escaping: true })
            ]
        )
    }

    #[test]
    fn test_6() {
        assert_eq!(
            parse("\"hello \\\\ \\\"world\\\"\"", false).collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::String { s: "hello \\\\ \\\"world\\\"", needs_escaping: true })
            ]
        )
    }
    #[test]
    fn test_7() {
        assert_eq!(
            parse("{}", false).collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::Object(Rc::new((HashMap::new(), Some("{}")))))
            ]
        )
    }

    #[test]
    fn test_8() {
        assert_eq!(
            parse("[]", false).collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::Array(Rc::new(vec![])))
            ]
        )
    }

    #[test]
    fn test_9() {
        assert_reversible("{
  \"foo\": [
    1,
    2.3,
    false,
    null,
    \"\"
  ]
}");
    }

    #[test]
    fn test_10() {
        assert_reversible("{
  \"id\": 809,
  \"name\": {
    \"english\": \"Melmetal\",
    \"japanese\": \"メルメタル\",
    \"chinese\": \"美录梅塔\",
    \"french\": \"\"
  },
  \"type\": [
    \"Steel\"
  ],
  \"base\": {
    \"HP\": 135,
    \"Attack\": 143,
    \"Defense\": 143,
    \"Sp. Attack\": 80,
    \"Sp. Defense\": 65,
    \"Speed\": 34
  }
}")
    }

    fn assert_reversible(json: &str) {
        let parse_result = parse(json, false).next().unwrap().unwrap();
        assert_eq!(format!("{}", parse_result), json);
    }
}

