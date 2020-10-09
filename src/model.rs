
use std::{collections::HashMap, fmt::Display};

#[derive(Debug,Clone,PartialEq,Eq,Hash)]
pub enum StrOrString<'a> {
    Str(&'a str),
    String(String),
}

#[derive(Debug,Clone)]
pub enum JSONValue<'a> {
    Object(HashMap<StrOrString<'a>, JSONValue<'a>>),
    Array(Vec<JSONValue<'a>>),
    String(&'a str),
    AllocatedString(String),
    Integer(i32),
    Float(f32),
    Boolean(bool),
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
            JSONValue::Boolean(_) => "boolean",
            JSONValue::Null => "null",
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
            JSONValue::Boolean(x1) => match *other { JSONValue::Boolean(x2) => x1.eq(&x2), _ => false },
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
        JSONValue::Boolean(true) => 2,
        JSONValue::Boolean(false) => 1,
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
                f.write_str(match key {
                    StrOrString::Str(s) => s,
                    StrOrString::String(s) => s,
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
        JSONValue::Boolean(b) => f.write_str(&format!("{}", b)),
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


#[derive(Debug,Clone,PartialEq)]
pub enum Filter<'a> {

    // basics
    Identity,
    ObjectIdentifierIndex { identifier: &'a str, optional: bool },
    ArrayIndex { index: usize },
    Slice { start: Option<usize>, end: Option<usize> },
    AllValues,
    Literal(JSONValue<'a>),

    // combinators
    Comma(Vec<Filter<'a>>),
    Pipe(Vec<Filter<'a>>),

    // operators
    Add { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    Subtract { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    Multiply { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    Divide { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    Modulo { left: Box<Filter<'a>>, right: Box<Filter<'a>> },

    // comparison
    Equal { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    NotEqual  { left: Box<Filter<'a>>, right: Box<Filter<'a>> },

    LessThan { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    LessThanOrEqual { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    GreaterThan { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    GreaterThanOrEqual { left: Box<Filter<'a>>, right: Box<Filter<'a>> },

    And { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    Or { left: Box<Filter<'a>>, right: Box<Filter<'a>> },
    Not,

    // functions
    Length,
    Keys,
    KeysUnsorted,
    Map(Box<Filter<'a>>),
    Select(Box<Filter<'a>>),
}
