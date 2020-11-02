use std::{ops::Deref, cmp::Ordering, collections::HashMap, fmt::Display, hash::Hash, rc::Rc};

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
            JSONValue::Object(x) => {
                let mut keys: Vec<&JSONValue> = x.0.keys().collect();
                keys.sort();

                for key in keys {
                    key.hash(state);
                    x.0.get(key).hash(state);
                }
            },
            JSONValue::Array(x) => x.hash(state),
            JSONValue::AllocatedString(x) => x.hash(state),
            JSONValue::String { s, needs_escaping, } => {
                if *needs_escaping {
                    for (_, c) in decoded_char_indices_iter(s) {
                        c.hash(state);
                    }
                } else {
                    s.hash(state);
                }
            },
            JSONValue::Integer(x) => x.hash(state),
            // JSONValue::Float(x) => x.hash(state),
            JSONValue::Bool(x) => x.hash(state),
            JSONValue::Null => NULL_HASH.hash(state),
            _ => unimplemented!(),
        }
    }
}

// HACK
const NULL_HASH: Option<bool> = None;

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
                JSONValue::String { s: _, needs_escaping: _ } => self.partial_cmp(other) == Some(Ordering::Equal),
                JSONValue::AllocatedString(x2) => x1.eq(x2),
                _ => false,
            },
            JSONValue::String { s: x1, needs_escaping: _ } => match other {
                JSONValue::String { s: x2, needs_escaping: _ } => x1.eq(x2),
                JSONValue::AllocatedString(_) => self.partial_cmp(other) == Some(Ordering::Equal),
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

pub fn decoded_length(raw: &str) -> usize {
    decoded_char_indices_iter(raw).count()
}

pub fn decoded_slice(raw: &str, start: Option<usize>, end: Option<usize>) -> &str {
    let indices: Vec<usize> = decoded_char_indices_iter(raw).map(|(i, _)| i).collect();

    if let Some(start) = start {
        if let Some(end) = end {
            &raw[indices[start]..indices[end]]
        } else {
            &raw[indices[start]..]
        }
    } else {
        if let Some(end) = end {
            &raw[..indices[end]]
        } else {
            raw
        }
    }
}

fn decode_unicode(digits: &str) -> char {
    std::char::from_u32(u32::from_str_radix(digits, 16).unwrap()).unwrap()
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

pub fn decoded_char_indices_iter<'a>(raw: &'a str) -> impl 'a + Iterator<Item=(usize, char)> {
    let mut index = 0;

    std::iter::from_fn(move || {
        if index >= raw.len() {
            None
        } else {
            let current_index = index;
            let c = raw[index..index+1].chars().next().unwrap();

            if c == '\\' {
                let directive = raw[index+1..index+2].chars().next().unwrap();
                if let Some((i, _)) = ESCAPE_DIRECTIVES.iter().enumerate().filter(|(_, c)| directive == **c).next() {
                    index += 2;
                    Some((current_index, ESCAPE_CHAR_VALUES[i]))
                } else if directive == 'u' {
                    index += 6;
                    Some((current_index, decode_unicode(&raw[current_index+2..current_index+6])))
                } else {
                    panic!("Encountered unexpected character escape \\{}", directive)
                }
            } else {
                index += 1;
                Some((current_index, c))
            }
        }
    })
}

const ESCAPE_DIRECTIVES: [char; 8] = ['\\', '"', 't', 'r', 'n', 'f', 'b', '/'];
const ESCAPE_CHAR_VALUES: [char; 8] = ['\\', '"', '\t', '\r', '\n', '\u{000c}', '\u{0008}', '/'];

macro_rules! compare_lex {
    ($iter_a:expr, $iter_b:expr) => {
        {
            let iter_a = $iter_a;
            let mut iter_b = $iter_b;

            for a in iter_a {
                let b = iter_b.next();
        
                if let Some(b) = b {
                    let ord = a.partial_cmp(&b).unwrap();
            
                    if ord != Ordering::Equal {
                        return Some(ord);
                    }
                } else {
                    // a is longer than b
                    return Some(Ordering::Greater);
                } 
            }
        
            if iter_b.next().is_some() {
                // b is longer than a
               Some(Ordering::Less)
            } else {
                Some(Ordering::Equal)
            }
        }
    };
}

impl<'a> PartialOrd for JSONValue<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let cmp_key_self = type_cmp_key(self);
        let cmp_key_other = type_cmp_key(other);
        if cmp_key_self != cmp_key_other {
            return cmp_key_self.partial_cmp(&cmp_key_other);
        }

        if let Some((s, sne)) = self.as_str() {
            if let Some((other, one)) = other.as_str() {
                return 
                    if sne {
                        if one {
                            compare_lex!(
                                decoded_char_indices_iter(s).map(|(_, c)| c),
                                decoded_char_indices_iter(other).map(|(_, c)| c)
                            )
                        } else {
                            compare_lex!(
                                decoded_char_indices_iter(s).map(|(_, c)| c),
                                other.chars()
                            )
                        }
                    } else {
                        if one {
                            compare_lex!(
                                s.chars(), 
                                decoded_char_indices_iter(other).map(|(_, c)| c)
                            )
                        } else {
                            s.partial_cmp(other)
                        }
                    }
                ;
            }
        }

        if let Some(s) = self.as_float() {
            if let Some(other) = other.as_float() {
                return s.partial_cmp(&other);
            }
        }

        if let JSONValue::Array(s) = self {
            if let JSONValue::Array(other) = other {
                return compare_lex!(s.iter(), other.iter());
            }
        }

        if let JSONValue::Object(s) = self {
            if let JSONValue::Object(other) = other {
                let mut self_keys: Vec<&JSONValue> = s.as_ref().0.keys().collect();
                let mut other_keys: Vec<&JSONValue> = other.as_ref().0.keys().collect();

                self_keys.sort();
                other_keys.sort();

                for (a, b) in self_keys.iter().zip(other_keys.iter()) {
                    let ord = a.partial_cmp(b).unwrap();

                    if ord != Ordering::Equal {
                        return Some(ord);
                    }
                }

                for (a, b) in self_keys.iter().map(|k| s.as_ref().0.get(k).unwrap()).zip(other_keys.iter().map(|k| other.as_ref().0.get(k).unwrap())) {
                    let ord = a.partial_cmp(b).unwrap();

                    if ord != Ordering::Equal {
                        return Some(ord);
                    }
                }

                return Some(Ordering::Equal);
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
        JSONValue::String { s: _, needs_escaping: _, } => 4,
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

                write_json(&key, 0, indentation_string, colored, buffer);

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
        JSONValue::String { s, needs_escaping } => {
            if colored {
                buffer.push_str(GREEN);
            }

            buffer.push('\"');
            
            if *needs_escaping {
                decoded_char_indices_iter(s).for_each(|(_, c)| buffer.push(c));
            } else {
                buffer.push_str(s);
            }

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
            buffer.push_str(s);
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
    use super::{JSONValue, decoded_char_indices_iter, decoded_length, decoded_slice};
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
        assert_eq!(decoded_length("abcde"), 5)
    }

    #[test]
    fn test_3() {
        assert_eq!(decoded_length("a\\tbcd\\u1234e"), 7)
    }

    #[test]
    fn test_4() {
        assert_eq!(decoded_slice("absde", Some(1), Some(3)), "bs")
    }

    #[test]
    fn test_5() {
        assert_eq!(decoded_slice("a\\tbcd\\u1234e", Some(2), Some(6)), "bcd\\u1234")
    }

    #[test]
    fn test_6() {
        assert_eq!(decoded_slice("a\\tbcd\\u1234e", Some(1), Some(3)), "\\tb")
    }

    #[test]
    fn test_7() {
        assert_eq!(decoded_char_indices_iter("\\u1234\\u1234\\u1234").collect::<Vec<(usize, char)>>(), vec![ (0, '\u{1234}'), (6, '\u{1234}'), (12, '\u{1234}') ])
    }
}
