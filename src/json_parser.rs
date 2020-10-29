
use std::{collections::HashMap, fmt::Display, hash::Hash, rc::Rc};


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
    try_eat(code, index, &'{')?;

    let mut contents = HashMap::new();

    if try_eat(code, index, &'}').is_ok() {
        return Ok(JSONValue::Object(contents));
    } else {
        let key = expression(code, index, no_free);

        if let Ok(prop) = key {
            try_eat(code, index, &':')?;
            let value = expression(code, index, no_free)?;

            let prop = Rc::new(prop);
            let value = Rc::new(value);

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

                    let prop = Rc::new(prop);        
                    let value = Rc::new(value);

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
        
        return Ok(JSONValue::Object(contents));
    }
}

fn array<'a>(code: &'a str, index: &mut usize, no_free: bool) -> Result<JSONValue<'a>, ParseError> {
    try_eat(code, index, &'[')?;

    let mut contents = Vec::new();

    if try_eat(code, index, &']').is_ok() {
        return Ok(JSONValue::Array(contents));
    } else {
        if let Ok(value) = expression(code, index, no_free) {

            let value = Rc::new(value);

            if no_free {
                std::mem::forget(value.clone());
            }

            contents.push(value);

            while try_eat(code, index, &',').is_ok() {

                let value = Rc::new(expression(code, index, no_free)?);

                if no_free {
                    std::mem::forget(value.clone());
                }

                contents.push(value);
            }
        }
        try_eat(code, index, &']')?;

        return Ok(JSONValue::Array(contents));
    }
}

fn string<'a>(code: &'a str, index: &mut usize) -> Result<JSONValue<'a>, ParseError> {
    let string_contents_start = *index + 1;

    let mut escape_next = false;
    let mut allocated_string: Option<String> = None;
    let mut end = string_contents_start;
    for (i, c) in code[string_contents_start..].char_indices() {
        if c == '\\' && !escape_next {
            escape_next = true;
            if allocated_string.is_none() {
                // println!("Allocating string because of {}: {}", c, &code[string_contents_start..string_contents_start + i]);
                allocated_string = Some(String::from(&code[string_contents_start..string_contents_start + i]));
            }
            // TODO: Handle escapes other than \ and "
        } else {
            if c == '"' && !escape_next {
                end = string_contents_start + i;
                *index = end + 1;
                break;
            }
            
            if let Some(s) = &mut allocated_string {
                s.push(c);
            }
            
            escape_next = false;
        }
    }

    Ok(match allocated_string {
        Some(s) => JSONValue::AllocatedString(s),
        None => JSONValue::String(&code[string_contents_start..end]),
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

#[derive(Debug,Clone)]
pub enum JSONValue<'a> {
    Object(HashMap<Rc<JSONValue<'a>>, Rc<JSONValue<'a>>>),
    Array(Vec<Rc<JSONValue<'a>>>),
    String(&'a str),
    AllocatedString(String),
    Integer(i32),
    Float(f32),
    Bool(bool),
    Null,
}

impl<'a> JSONValue<'a> {
    pub fn type_name(&self) -> &'static str {
        match self {
            JSONValue::Object(_) => "object",
            JSONValue::Array(_) => "array",
            JSONValue::String(_) => "string",
            JSONValue::AllocatedString(_) => "string",
            JSONValue::Integer(_) => "number",
            JSONValue::Float(_) => "float",
            JSONValue::Bool(_) => "bool",
            JSONValue::Null => "null",
        }
    }
}

impl<'a> Hash for JSONValue<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            // JSONValue::Object(x) => x.hash(state),
            JSONValue::Array(x) => x.hash(state),
            JSONValue::String(x) => x.hash(state),
            JSONValue::AllocatedString(x) => x.hash(state),
            JSONValue::Integer(x) => x.hash(state),
            // JSONValue::Float(x) => x.hash(state),
            JSONValue::Bool(x) => x.hash(state),
            // JSONValue::Null => x.hash(state),
            _ => unimplemented!(),
        }
    }
}

impl<'a> PartialEq for JSONValue<'a> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            JSONValue::Object(x1) => match other { JSONValue::Object(x2) => x1.eq(&x2), _ => false },
            JSONValue::Array(x1) => match other { JSONValue::Array(x2) => x1.iter().enumerate().all(|(index, el)| Some(el).eq(&x2.get(index))), _ => false },
            JSONValue::String(x1) => match other { JSONValue::String(x2) => x1.eq(x2), JSONValue::AllocatedString(x2) => x1.eq(&x2), _ => false },
            JSONValue::AllocatedString(x1) => match other { JSONValue::String(x2) => x1.eq(x2), JSONValue::AllocatedString(x2) => x1.eq(x2), _ => false },
            JSONValue::Integer(x1) => match *other { JSONValue::Integer(x2) => x1.eq(&x2), _ => false },
            JSONValue::Float(x1) => match *other { JSONValue::Float(x2) => x1.eq(&x2), _ => false },
            JSONValue::Bool(x1) => match *other { JSONValue::Bool(x2) => x1.eq(&x2), _ => false },
            JSONValue::Null => match *other { JSONValue::Null => true, _ => false },
        }
    }
}

impl<'a> Eq for JSONValue<'a> { }

#[test]
fn test_1() {
    assert_eq!(JSONValue::String("foo"), JSONValue::AllocatedString(String::from("foo")));
}

impl<'a> PartialOrd for JSONValue<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let cmp_key_self = type_cmp_key(self);
        let cmp_key_other = type_cmp_key(other);
        if cmp_key_self != cmp_key_other {
            return cmp_key_self.partial_cmp(&cmp_key_other);
        }


        if let JSONValue::String(s) = self {
            if let JSONValue::String(other) = other {
                return s.partial_cmp(other);
            }
            if let JSONValue::AllocatedString(other) = other {
                return s.partial_cmp(&other.as_str());
            }
        } else if let JSONValue::AllocatedString(s) = self {
            if let JSONValue::String(other) = other {
                return s.as_str().partial_cmp(other);
            }
            if let JSONValue::AllocatedString(other) = other {
                return s.partial_cmp(other);
            }
        }

        if let JSONValue::Integer(s) = self {
            if let JSONValue::Integer(other) = other {
                return s.partial_cmp(other);
            }
            if let JSONValue::Float(other) = other {
                return (*s as f32).partial_cmp(other);
            }
        }
        
        if let JSONValue::Float(s) = self {
            if let JSONValue::Integer(other) = other {
                return s.partial_cmp(&(*other as f32));
            }
            if let JSONValue::Float(other) = other {
                return s.partial_cmp(other);
            }
        }

        if let JSONValue::Array(s) = self {
            if let JSONValue::Array(other) = other {
                todo!()
            }
        }

        if let JSONValue::Object(s) = self {
            if let JSONValue::Object(other) = other {
                todo!()
            }
        }

        None
    }
}

impl<'a> Ord for JSONValue<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
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


impl<'a> Display for JSONValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_inner(self, 0, f)
    }
}

fn fmt_inner<'a>(val: &JSONValue<'a>, indentation: i32, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match val {
        JSONValue::Object(contents) => {
            f.write_str("{")?;
            let mut first = true;

            for (key, value) in contents {
                if !first {
                    f.write_str(",")?;
                } else {
                    first = false;
                }
                
                write_newline_and_indentation(f, indentation + 1)?;

                f.write_str("\"")?;
                f.write_str(match key.as_ref() {
                    JSONValue::String(s) => s,
                    JSONValue::AllocatedString(s) => s,
                    _ => unimplemented!(),
                })?;
                f.write_str("\"")?;
                f.write_str(": ")?;
                fmt_inner(value, indentation + 1, f)?;
            }
            
            write_newline_and_indentation(f, indentation)?;
            f.write_str("}")
        },
        JSONValue::Array(contents) => {
            f.write_str("[")?;
            let mut first = true;

            for value in contents {
                if !first {
                    f.write_str(",")?;
                } else {
                    first = false;
                }

                write_newline_and_indentation(f, indentation + 1)?;
                fmt_inner(value, indentation + 1, f)?;
            }

            write_newline_and_indentation(f, indentation)?;
            f.write_str("]")
        },
        JSONValue::String(s) => f.write_str(&format!("\"{}\"", s)),
        JSONValue::AllocatedString(s) => f.write_str(&format!("\"{}\"", s)),
        JSONValue::Integer(n) => f.write_str(&format!("{}", n)),
        JSONValue::Float(n) => f.write_str(&format!("{}", n)),
        JSONValue::Bool(b) => f.write_str(&format!("{}", b)),
        JSONValue::Null => f.write_str("null"),
    }
}

fn write_newline_and_indentation(f: &mut std::fmt::Formatter<'_>, indentation: i32) -> std::fmt::Result {

    f.write_str("\n")?;

    for _ in 0..indentation {
        f.write_str(INDENTATION_STR)?;
    }

    Ok(())
}

const INDENTATION_STR: &str = "  ";


#[cfg(test)]
mod parser_tests {
    use std::{collections::HashMap, rc::Rc};
    use super::{JSONValue, ParseError, parse};

    #[test]
    fn test_1() {
        assert_eq!(
            parse("[1, 2, 3]", false).collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![ Ok(JSONValue::Array(vec![ Rc::new(JSONValue::Integer(1)), Rc::new(JSONValue::Integer(2)), Rc::new(JSONValue::Integer(3)) ])) ]
        )
    }

    #[test]
    fn test_2() {
        let mut target_hashmap = HashMap::new();
        target_hashmap.insert(Rc::new(JSONValue::String("foo")), Rc::new(JSONValue::Array(vec![
            Rc::new(JSONValue::Integer(1)),
            Rc::new(JSONValue::Float(2.3)),
            Rc::new(JSONValue::Bool(false)),
            Rc::new(JSONValue::Null),
            Rc::new(JSONValue::String("")),
        ])));

        assert_eq!(
            parse("{ 
                \"foo\": [ 
                    1, 
                    2.3, 
                    false, 
                    null, 
                    \"\" 
                ] 
            }", false).collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::Object(target_hashmap))
            ]
        )
    }

    #[test]
    fn test_3() {
        let mut map_1 = HashMap::new();
        map_1.insert(Rc::new(JSONValue::String("foo")), Rc::new(JSONValue::Integer(1)));
        
        let mut map_2 = HashMap::new();
        map_2.insert(Rc::new(JSONValue::String("foo")), Rc::new(JSONValue::Integer(2)));
        
        let mut map_3 = HashMap::new();
        map_3.insert(Rc::new(JSONValue::String("foo")), Rc::new(JSONValue::Integer(3)));

        assert_eq!(
            parse("
                { \"foo\": 1 }
                { \"foo\": 2 }
                { \"foo\": 3 }
            ", false).collect::<Vec<Result<JSONValue,ParseError>>>(),
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
                Ok(JSONValue::AllocatedString(String::from("hello \"world\"")))
            ]
        )
    }

    #[test]
    fn test_6() {
        assert_eq!(
            parse("\"hello \\\\ \\\"world\\\"\"", false).collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::AllocatedString(String::from("hello \\ \"world\"")))
            ]
        )
    }

    #[test]
    fn test_7() {
        assert_eq!(
            parse("{}", false).collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::Object(HashMap::new()))
            ]
        )
    }

    #[test]
    fn test_8() {
        assert_eq!(
            parse("[]", false).collect::<Vec<Result<JSONValue,ParseError>>>(), 
            vec![
                Ok(JSONValue::Array(vec![]))
            ]
        )
    }
}