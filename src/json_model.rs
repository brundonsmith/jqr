use std::{collections::HashMap, fmt::Display, hash::Hash, cmp::Ordering, rc::Rc};

use crate::json_parser::object_entries;

#[derive(Debug, Clone)]
pub enum JSONValue<'a> {
    Object(Rc<(HashMap<JSONValue<'a>, JSONValue<'a>>, Option<&'a str>)>),
    Array(Rc<Vec<JSONValue<'a>>>),
    AllocatedString(Rc<String>),
    String { s: &'a str, needs_escaping: bool },
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
            JSONValue::AllocatedString(_) => "string",
            JSONValue::String { s: _, needs_escaping: _, } => "string",
            JSONValue::Integer(_) => "number",
            JSONValue::Float(_) => "number",
            JSONValue::Bool(_) => "boolean",
            JSONValue::Null => "null",
        }
    }

    pub fn as_str(&'a self) -> Option<(&'a str, bool)> {
        match self {
            JSONValue::String { s, needs_escaping } => Some((s, *needs_escaping)),
            JSONValue::AllocatedString(s) => Some((s.as_str(), false)),
            _ => None,
        }
    }

    pub fn as_float(&self) -> Option<f32> {
        match self {
            JSONValue::Float(n) => Some(*n),
            JSONValue::Integer(n) => Some(*n as f32),
            _ => None,
        }
    }
}

impl<'a> Hash for JSONValue<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            // JSONValue::Object(x) => x.hash(state),
            JSONValue::Array(x) => x.hash(state),
            JSONValue::AllocatedString(x) => x.hash(state),
            JSONValue::String {
                s: x,
                needs_escaping: _,
            } => x.hash(state),
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
            JSONValue::Object(x1) => match other {
                JSONValue::Object(x2) => x1.as_ref().0.eq(&x2.as_ref().0),
                _ => false,
            },
            JSONValue::Array(x1) => match other {
                JSONValue::Array(x2) => x1.as_ref().eq(x2.as_ref()),
                _ => false,
            },
            JSONValue::AllocatedString( x1) => match other {
                JSONValue::String {
                    s: x2,
                    needs_escaping: _,
                } => x1.as_ref().eq(x2),
                JSONValue::AllocatedString(x2) => x1.eq(x2),
                _ => false,
            },
            JSONValue::String {
                s: x1,
                needs_escaping: y1,
            } => match other {
                JSONValue::String {
                    s: x2,
                    needs_escaping: y2,
                } => x1.eq(x2) && y1 == y2,
                JSONValue::AllocatedString(x2) => x1.eq(x2.as_ref()),
                _ => false,
            },
            JSONValue::Integer(x1) => match *other {
                JSONValue::Integer(x2) => x1.eq(&x2),
                _ => false,
            },
            JSONValue::Float(x1) => match *other {
                JSONValue::Float(x2) => x1.eq(&x2),
                _ => false,
            },
            JSONValue::Bool(x1) => match *other {
                JSONValue::Bool(x2) => x1.eq(&x2),
                _ => false,
            },
            JSONValue::Null => match *other {
                JSONValue::Null => true,
                _ => false,
            },
        }
    }
}

impl<'a> Eq for JSONValue<'a> {}

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
                let decoded =
                    std::char::from_u32(u32::from_str_radix(&raw[i + 1..i + 5], 16).unwrap())
                        .unwrap();
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

const ESCAPE_DIRECTIVES: [char; 7] = ['\\', '"', 't', 'r', 'n', 'f', 'b'];
const ESCAPE_CHAR_VALUES: [char; 7] = ['\\', '"', '\t', '\r', '\n', '\u{000c}', '\u{0008}'];

impl<'a> PartialOrd for JSONValue<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let cmp_key_self = type_cmp_key(self);
        let cmp_key_other = type_cmp_key(other);
        if cmp_key_self != cmp_key_other {
            return cmp_key_self.partial_cmp(&cmp_key_other);
        }

        if let Some((s, sne)) = self.as_str() {
            if let Some((other, one)) = other.as_str() {
                // TODO: Handle escapes
                return s.partial_cmp(other);
            }
        }

        if let Some(s) = self.as_float() {
            if let Some(other) = other.as_float() {
                return s.partial_cmp(&other);
            }
        }

        if let JSONValue::Array(s) = self {
            if let JSONValue::Array(other) = other {
                for (a, b) in s.iter().zip(other.iter()) {
                    let ord = a.partial_cmp(b).unwrap();

                    if ord != Ordering::Equal {
                        return Some(ord);
                    }
                }
            }
        }

        if let JSONValue::Object(s) = self {
            if let JSONValue::Object(other) = other {
                todo!()
            }
        }

        unreachable!()
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
        JSONValue::String {
            s: _,
            needs_escaping: _,
        } => 4,
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
        let mut buffer = String::new();
        write_json(self, 0, "  ", false, &mut buffer);
        f.write_str(&buffer)
    }
}

const WHITE: &str = "\u{1b}[37m";
const BLUE: &str = "\u{1b}[34m";
const GREEN: &str = "\u{1b}[32m";
const BLACK: &str = "\u{1b}[30m";

pub fn write_json<'a>(
    val: &JSONValue<'a>,
    indentation: i32,
    indentation_string: &str,
    colored: bool,
    buffer: &mut String,
) {
    match val {
        JSONValue::Object(contents) => {
            buffer.push('{');
            let mut first = true;

            let entries = contents.as_ref().1
                .map(|json| object_entries(json).unwrap())
                .unwrap_or(contents.as_ref().0.iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect());

            for (key, value) in entries.into_iter() {
                if !first {
                    buffer.push(',');
                } else {
                    first = false;
                }

                write_newline_and_indentation(
                    buffer,
                    indentation + 1,
                    indentation_string,
                );

                if colored {
                    buffer.push_str(BLUE);
                }

                buffer.push('\"');
                match key {
                    JSONValue::String {
                        s,
                        needs_escaping: _,
                    } => buffer.push_str(s),
                    JSONValue::AllocatedString(s) => buffer.push_str(&encode_escapes(s.as_str())),
                    _ => unimplemented!(),
                };
                buffer.push('\"');

                if colored {
                    buffer.push_str(WHITE);
                }

                buffer.push_str(": ");
                write_json(
                    &value,
                    indentation + 1,
                    indentation_string,
                    colored,
                    buffer,
                );
            }

            write_newline_and_indentation(
                buffer,
                indentation,
                indentation_string,
            );
            buffer.push('}')
        }
        JSONValue::Array(contents) => {
            buffer.push('[');
            let mut first = true;

            for value in contents.as_ref() {
                if !first {
                    buffer.push(',');
                } else {
                    first = false;
                }

                write_newline_and_indentation(
                    buffer,
                    indentation + 1,
                    indentation_string,
                );
                write_json(
                    value,
                    indentation + 1,
                    indentation_string,
                    colored,
                    buffer,
                );
            }

            write_newline_and_indentation(
                buffer,
                indentation,
                indentation_string,
            );
            buffer.push(']');
        }
        JSONValue::String {
            s,
            needs_escaping: _,
        } => {
            if colored {
                buffer.push_str(GREEN);
            }

            buffer.push('\"');
            buffer.push_str(s);
            buffer.push('\"');

            if colored {
                buffer.push_str(WHITE);
            }
        }
        JSONValue::AllocatedString(s) => {
            if colored {
                buffer.push_str(GREEN);
            }

            buffer.push('\"');
            buffer.push_str(&encode_escapes(s));
            buffer.push('\"');

            if colored {
                buffer.push_str(WHITE);
            }
        }
        JSONValue::Integer(n) => buffer.push_str(&n.to_string()),
        JSONValue::Float(n) => buffer.push_str(&n.to_string()),
        JSONValue::Bool(b) => buffer.push_str(match b {
            true => "true",
            false => "false",
        }),
        JSONValue::Null => {
            if colored {
                buffer.push_str(BLACK);
            }

            buffer.push_str("null");

            if colored {
                buffer.push_str(WHITE);
            }
        }
    }
}

fn write_newline_and_indentation(
    buffer: &mut String,
    indentation: i32,
    indentation_string: &str,
) {
    buffer.push('\n');

    for _ in 0..indentation {
        buffer.push_str(indentation_string);
    }
}

pub fn create_indentation_string(
    indentation_step: u8,
    tab_indentation: bool,
) -> String {
    if tab_indentation {
        String::from("\t")
    } else {
        let mut s = String::new();

        for _ in 0..indentation_step {
            s.push(' ');
        }

        s
    }
}

#[cfg(test)]
mod tests {
    use super::{apply_escapes, encode_escapes, JSONValue};
    use std::rc::Rc;

    #[test]
    fn test_1() {
        assert_eq!(
            JSONValue::String {
                s: "foo",
                needs_escaping: false
            },
            JSONValue::AllocatedString(Rc::new(String::from("foo")))
        );
    }

    #[test]
    fn test_2() {
        assert_eq!(
            apply_escapes("\\n \\t \\f \\u1234"),
            "\n \t \u{000c} \u{1234}"
        )
    }

    #[test]
    fn test_3() {
        assert_eq!(apply_escapes("\\u1234\\n\\t\\\\").chars().count(), 4)
    }

    #[test]
    fn test_4() {
        assert_eq!(
            encode_escapes("\n \t \u{000c} \u{1234}"),
            "\\n \\t \\f \\u1234"
        )
    }
}
