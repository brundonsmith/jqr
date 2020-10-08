
use std::{collections::HashMap, fmt::Display};

#[derive(Debug,Clone,PartialEq)]
pub enum JSONValue<'a> {
    Object(HashMap<&'a str, JSONValue<'a>>),
    Array(Vec<JSONValue<'a>>),
    String(&'a str),
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
            JSONValue::Integer(_) => "number",
            JSONValue::Float(_) => "float",
            JSONValue::Boolean(_) => "boolean",
            JSONValue::Null => "null",
        }
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
                f.write_str(key)?;
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
        JSONValue::String(s) => {
            f.write_str(&format!("\"{}\"", s))
        },
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

const INDENTATION_STR: &str = "\t";


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

    // functions
    Length,
    Keys,
    KeysUnsorted,
}
