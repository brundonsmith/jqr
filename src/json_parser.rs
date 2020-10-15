use serde_json::Value;


pub fn parse<'a>(json: &'a str) -> impl 'a + Iterator<Item=Result<Value,serde_json::Error>> {
    delimit_values(json).map(serde_json::from_str)
}

macro_rules! end_of {
    ($slice:expr, $pred:expr) => {
        $slice
            .char_indices()
            .take_while($pred)
            .last()
            .map(|(i, c)| i + c.len_utf8())
            .unwrap_or(0)
    }
}

macro_rules! slice_of {
    ($json:ident, $index:ident, $start:ident, $skip:literal, $pred:expr) => {
        {
            let end = $start + $skip + end_of!($json[$start+$skip..], $pred);
            
            $index = end;

            if $start == end {
                None
            } else {
                Some(&$json[$start..end])
            }
        }
    }
}

macro_rules! slice_within_brackets {
    ($json:ident, $index:ident, $start:ident, $open:literal, $close:literal) => {
        {
            let mut bracket_depth = 1;

            slice_of!($json, $index, $start, 1, move |(_, c)| {
                let res = bracket_depth > 0;

                if c == &$close {
                    bracket_depth -= 1;
                } else if c == &$open {
                    bracket_depth += 1;
                }

                return res;
            })
        }
    };
}

pub fn delimit_values<'a>(json: &'a str) -> impl Iterator<Item=&'a str> {
    let mut index = 0;

    std::iter::from_fn(move || {
        if index >= json.len() {
            return None;
        } else {
            let start = index + end_of!(json[index..], |(_, c)| c.is_whitespace());

            match json[start..].chars().next() {
                Some('{') => slice_within_brackets!(json, index, start, '{','}'), // object
                Some('[') => slice_within_brackets!(json, index, start, '[',']'), // array
                Some('"') => { // string
                    let mut escape_count = 0;

                    slice_of!(json, index, start, 1, move |(_, c)| {
                        let res = c != &'"' || escape_count % 2 == 0;

                        if c == &'\\' {
                            escape_count += 1;
                        } else {
                            escape_count = 0;
                        }

                        return res;
                    })
                }
                _ => { // number, boolean, or null
                    slice_of!(json, index, start, 0, move |(_, c)| 
                        c != &'{' && c != &'[' && c != &'"' && !c.is_whitespace())
                }
            }
        }
    })
}

#[test]
fn test_1() {
    let vals: Vec<&str> = delimit_values("{\"a\": 12} {\"b\": 13}").collect();
    assert_eq!(vals, vec!["{\"a\": 12}", "{\"b\": 13}"]);
}
