use std::fmt::Display;

use crate::json_model::JSONValue;


#[derive(Debug,Clone,PartialEq)]
pub enum Filter<'a> {

    // basics
    Identity,
    ObjectIdentifierIndex { identifier: &'a str, optional: bool },
    ArrayIndex { index: usize },
    Slice { start: Option<usize>, end: Option<usize>, optional: bool },
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
    Alternative { left: Box<Filter<'a>>, right: Box<Filter<'a>> },

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
    Sort,
    SortBy(Box<Filter<'a>>),
    Has(JSONValue<'a>),
    Type,
    Min,
    Max,
    Flatten,
    Reverse,

    // _MapSelect(Box<Filter<'a>>),
    // _PropertyChain(Vec<(&'a str, bool)>),
}


macro_rules! fmt_binary_op {
    ($f:ident, $left:ident, $symbol:literal, $right:ident) => {
        {
            $left.fmt($f)?;
            $f.write_str(" ")?;
            $f.write_str($symbol)?;
            $f.write_str(" ")?;
            $right.fmt($f)?;
            Ok(())
        }
    };
}

macro_rules! fmt_function {
    ($f:ident, $name:literal, $inner:ident) => {
        {   
            $f.write_str($name)?;
            $f.write_str("(")?;
            $inner.fmt($f)?;
            $f.write_str(")")?;
            Ok(())
        }
    };
}

impl<'a> Display for Filter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Filter::Identity => f.write_str("."),
            Filter::ObjectIdentifierIndex { identifier, optional } => {
                f.write_str(".")?;

                if identifier.chars().next().unwrap().is_alphabetic() && identifier.chars().all(|c| c.is_alphanumeric()) {
                    f.write_str(identifier)?;
                } else {
                    f.write_str("[\"")?;
                    f.write_str(identifier)?;
                    f.write_str("\"]")?;
                }

                if *optional {
                    f.write_str("?")?;
                }

                Ok(())
            },
            Filter::ArrayIndex { index } => {
                f.write_str(".")?;

                f.write_str("[")?;
                f.write_str(&index.to_string())?;
                f.write_str("]")?;

                Ok(())
            },
            Filter::Slice { start, end, optional } => {
                f.write_str(".")?;

                f.write_str("[")?;
                if let Some(start) = start {
                    f.write_str(&start.to_string())?;
                }
                f.write_str(":")?;
                if let Some(end) = end {
                    f.write_str(&end.to_string())?;
                }
                f.write_str("]")?;

                if *optional {
                    f.write_str("?")?;
                }

                Ok(())
            },
            Filter::Literal(val) => f.write_str(&format!("{}", val)),
            Filter::Comma(inner) => {
                let mut first = true;
                for segment in inner {
                    if !first {
                        f.write_str(", ")?;
                    } else {
                        first = false;
                    }

                    f.write_str(&format!("{}", segment))?;
                }

                Ok(())
            },
            Filter::Pipe(inner) => {
                if inner.iter().all(|f| matches!(f, Filter::ObjectIdentifierIndex { identifier: _, optional: _ }) || matches!(f, Filter::ArrayIndex { index: _ }) ||  matches!(f, Filter::Slice { start: None, end: None, optional: _ })) {
                    for segment in inner {
                        f.write_str(&format!("{}", segment))?;
                    }

                    Ok(())
                } else {
                    let mut first = true;
                    for segment in inner {
                        if !first {
                            f.write_str(" | ")?;
                        } else {
                            first = false;
                        }

                        f.write_str(&format!("{}", segment))?;
                    }

                    Ok(())
                }
            },
            Filter::Add { left, right } => fmt_binary_op!(f, left, "+", right),
            Filter::Subtract { left, right } => fmt_binary_op!(f, left, "-", right),
            Filter::Multiply { left, right } => fmt_binary_op!(f, left, "*", right),
            Filter::Divide { left, right } => fmt_binary_op!(f, left, "/", right),
            Filter::Modulo { left, right } => fmt_binary_op!(f, left, "%", right),
            Filter::Alternative { left, right } => fmt_binary_op!(f, left, "//", right),
            Filter::Equal { left, right } => fmt_binary_op!(f, left, "==", right),
            Filter::NotEqual { left, right } => fmt_binary_op!(f, left, "!=", right),
            Filter::LessThan { left, right } => fmt_binary_op!(f, left, "<", right),
            Filter::LessThanOrEqual { left, right } => fmt_binary_op!(f, left, "<=", right),
            Filter::GreaterThan { left, right } => fmt_binary_op!(f, left, ">", right),
            Filter::GreaterThanOrEqual { left, right } => fmt_binary_op!(f, left, ">=", right),
            Filter::And { left, right } => fmt_binary_op!(f, left, "and", right),
            Filter::Or { left, right } => fmt_binary_op!(f, left, "or", right),
            Filter::Not => f.write_str("not"),
            Filter::Length => f.write_str("length"),
            Filter::Keys => f.write_str("keys"),
            Filter::KeysUnsorted => f.write_str("keys_unsorted"),
            Filter::Map(inner) => fmt_function!(f, "map", inner),
            Filter::Select(inner) => fmt_function!(f, "select", inner),
            Filter::Sort => f.write_str("sort"),
            Filter::SortBy(inner) => fmt_function!(f, "sort_by", inner),
            Filter::Has(inner) => fmt_function!(f, "has", inner),
            Filter::Type => f.write_str("type"),
            Filter::Min => f.write_str("min"),
            Filter::Max => f.write_str("max"),
            Filter::Flatten => f.write_str("flatten"),
            Filter::Reverse => f.write_str("reverse"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::filter_parser;

    #[test]
    fn test_1() {
        let parsed = filter_parser::parse("map(select(.base.Attack > 100)) | map(.name.english)").unwrap();
        assert_eq!(format!("{}", parsed), "map(select(.base.Attack > 100)) | map(.name.english)")
    }
}