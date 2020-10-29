
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

#[derive(Debug,Clone)]
pub enum JSONValue<'a> {
    Object(HashMap<Rc<JSONValue<'a>>, Rc<JSONValue<'a>>>),
    Array(Vec<Rc<JSONValue<'a>>>),
    String { s: &'a str, needs_escaping: bool },
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
            JSONValue::String { s: _, needs_escaping: _ } => "string",
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
            JSONValue::String { s: x, needs_escaping: _ } => x.hash(state),
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
            JSONValue::String { s: x1, needs_escaping: y1 } => match other { JSONValue::String { s: x2, needs_escaping: y2 } => x1.eq(x2) && y1 == y2, JSONValue::AllocatedString(x2) => x1.eq(&x2), _ => false },
            JSONValue::AllocatedString(x1) => match other { JSONValue::String { s: x2, needs_escaping: _ } => x1.eq(x2), JSONValue::AllocatedString(x2) => x1.eq(x2), _ => false },
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
    assert_eq!(JSONValue::String { s: "foo", needs_escaping: false }, JSONValue::AllocatedString(String::from("foo")));
}


pub fn apply_escapes<'a>(raw: &'a str) -> String {
    let mut new_str = String::with_capacity(raw.len());
    let new_str_ref = &mut new_str;
    let mut escape_next = false;
    let mut escape_to = 0;

    raw.char_indices().for_each(move |(i, c)| {
        if escape_to > i {
            // do nothing
        } else if !escape_next {
            if c == '\\' {
                escape_next = true;
            } else {
                new_str_ref.push(c);
            }
        } else {
            if c == 'u' {
                let decoded = std::char::from_u32(u32::from_str_radix(&raw[i + 1..i + 5], 16).unwrap()).unwrap();
                new_str_ref.push(decoded);
                escape_to = i + 5;
            } else {
                for (ie, e) in ESCAPE_DIRECTIVES.iter().enumerate() {
                    if c == *e {
                        new_str_ref.push(ESCAPE_CHAR_VALUES[ie]);
                        break;
                    }
                }
            }

            escape_next = false;
        }
    });

    new_str
}

#[test]
fn test_2() {
    assert_eq!(apply_escapes("\\n \\t \\f \\u1234"), "\n \t \u{000c} \u{1234}")
}

#[test]
fn test_3() {
    assert_eq!(apply_escapes("\\u1234\\n\\t\\\\").chars().count(), 4)
}

#[test]
fn test_4() {
    assert_eq!(encode_escapes("\n \t \u{000c} \u{1234}"), "\\n \\t \\f \\u1234")
}

pub fn encode_escapes<'a>(raw: &'a str) -> String {
    let mut new_str = String::with_capacity(raw.len());
    let new_str_ref = &mut new_str;

    raw.chars().for_each(move |c| {
        for (ie, e) in ESCAPE_CHAR_VALUES.iter().enumerate() {
            if c == *e {
                new_str_ref.push('\\');
                new_str_ref.push(ESCAPE_DIRECTIVES[ie]);
                return;
            }
        }

        if !c.is_ascii() {
            new_str_ref.push('\\');
            new_str_ref.push('u');
            new_str_ref.push_str(&format_radix(c as u32, 16));
            return;
        }

        // no escape found
        new_str_ref.push(c);
    });

    new_str
}

fn format_radix(mut x: u32, radix: u32) -> String {
    let mut result = vec![];

    loop {
        let m = x % radix;
        x = x / radix;

        // will panic if you use a bad radix (< 2 or > 36).
        result.push(std::char::from_digit(m, radix).unwrap());
        if x == 0 {
            break;
        }
    }
    result.into_iter().rev().collect()
}

const ESCAPE_DIRECTIVES: [char;7] = [ '\\', '"', 't', 'r', 'n', 'f', 'b' ];
const ESCAPE_CHAR_VALUES: [char;7] = [ '\\', '"', '\t', '\r', '\n', '\u{000c}', '\u{0008}' ];

impl<'a> PartialOrd for JSONValue<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let cmp_key_self = type_cmp_key(self);
        let cmp_key_other = type_cmp_key(other);
        if cmp_key_self != cmp_key_other {
            return cmp_key_self.partial_cmp(&cmp_key_other);
        }


        if let JSONValue::String { s, needs_escaping: _ } = self {
            if let JSONValue::String { s: other, needs_escaping: _ } = other {
                return s.partial_cmp(other);
            }
            if let JSONValue::AllocatedString(other) = other {
                return s.partial_cmp(&other.as_str());
            }
        } else if let JSONValue::AllocatedString(s) = self {
            if let JSONValue::String { s: other, needs_escaping: _ } = other {
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
        JSONValue::String { s: _, needs_escaping: _ } => 4,
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
        write_json(self, 0, 2, false, false, &mut |s| f.write_str(s))
    }
}

const WHITE: &str = "\u{1b}[37m";
const BLUE: &str = "\u{1b}[34m";
const GREEN: &str = "\u{1b}[32m";
const BLACK: &str = "\u{1b}[30m";

pub fn write_json<'a, E, W: FnMut(&str) -> Result<(),E>>(val: &JSONValue<'a>, indentation: i32, indentation_step: u8, tab_indentation: bool, colored: bool, write_str: &mut W) -> Result<(),E> {
    match val {
        JSONValue::Object(contents) => {
            write_str("{")?;
            let mut first = true;

            for (key, value) in contents {
                if !first {
                    write_str(",")?;
                } else {
                    first = false;
                }
                
                write_newline_and_indentation(write_str, indentation + 1, indentation_step, tab_indentation)?;

                if colored {
                    write_str(BLUE)?;
                }

                write_str("\"")?;
                match key.as_ref() {
                    JSONValue::String { s, needs_escaping: _ } => write_str(s)?,
                    JSONValue::AllocatedString(s) => write_str(&encode_escapes(s))?,
                    _ => unimplemented!(),
                };
                write_str("\"")?;

                if colored {
                    write_str(WHITE)?;
                }

                write_str(": ")?;
                write_json(value, indentation + 1, indentation_step, tab_indentation, colored, write_str)?;
            }
            
            write_newline_and_indentation(write_str, indentation, indentation_step, tab_indentation)?;
            write_str("}")
        },
        JSONValue::Array(contents) => {
            write_str("[")?;
            let mut first = true;

            for value in contents {
                if !first {
                    write_str(",")?;
                } else {
                    first = false;
                }

                write_newline_and_indentation(write_str, indentation + 1, indentation_step, tab_indentation)?;
                write_json(value, indentation + 1, indentation_step, tab_indentation, colored, write_str)?;
            }

            write_newline_and_indentation(write_str, indentation, indentation_step, tab_indentation)?;
            write_str("]")
        },
        JSONValue::String { s, needs_escaping: _ } => {
            if colored {
                write_str(GREEN)?;
            }
            
            write_str(&format!("\"{}\"", s))?;

            if colored {
                write_str(WHITE)?;
            }

            Ok(())
        },
        JSONValue::AllocatedString(s) => {
            if colored {
                write_str(GREEN)?;
            }

            write_str(&format!("\"{}\"", encode_escapes(s)))?;

            if colored {
                write_str(WHITE)?;
            }

            Ok(())
        },
        JSONValue::Integer(n) => write_str(&format!("{}", n)),
        JSONValue::Float(n) => write_str(&format!("{}", n)),
        JSONValue::Bool(b) => write_str(&format!("{}", b)),
        JSONValue::Null => {
            if colored {
                write_str(BLACK)?;
            }
            
            write_str("null")?;

            if colored {
                write_str(WHITE)?;
            }

            Ok(())
        },
    }
}

// const BLUE = "\033[94m";


fn write_newline_and_indentation<E, W: FnMut(&str) -> Result<(),E>>(write_str: &mut W, indentation: i32, indentation_step: u8, tab_indentation: bool) -> Result<(), E> {

    write_str("\n")?;

    if tab_indentation {
        for _ in 0..indentation {
            write_str("\t")?;
        }
    } else {
        for _ in 0..indentation {
            for _ in 0..indentation_step {
                write_str(" ")?;
            }
        }
    }

    Ok(())
}


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
        target_hashmap.insert(Rc::new(JSONValue::String { s: "foo", needs_escaping: false }), Rc::new(JSONValue::Array(vec![
            Rc::new(JSONValue::Integer(1)),
            Rc::new(JSONValue::Float(2.3)),
            Rc::new(JSONValue::Bool(false)),
            Rc::new(JSONValue::Null),
            Rc::new(JSONValue::String { s: "", needs_escaping: false }),
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
        map_1.insert(Rc::new(JSONValue::String { s: "foo", needs_escaping: false }), Rc::new(JSONValue::Integer(1)));
        
        let mut map_2 = HashMap::new();
        map_2.insert(Rc::new(JSONValue::String { s: "foo", needs_escaping: false }), Rc::new(JSONValue::Integer(2)));
        
        let mut map_3 = HashMap::new();
        map_3.insert(Rc::new(JSONValue::String { s: "foo", needs_escaping: false }), Rc::new(JSONValue::Integer(3)));

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

    #[test]
    fn test_9() {
        assert_reversible("\"\\n \\t \\f \\u1234\"");
    }

    #[test]
    fn test_10() {
        assert_reversible("\"hello \\\\ \\\"world\\\"\"");
    }

    #[test]
    fn test_11() {
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

    fn assert_reversible(json: &str) {
        let parse_result = parse(json, false).next().unwrap().unwrap();
        assert_eq!(format!("{}", parse_result), json);
    }
}

