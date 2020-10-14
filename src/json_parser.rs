use serde_json::Value;


pub fn parse<'a>(json: &'a str) -> impl 'a + Iterator<Item=Result<Value,serde_json::Error>> {
    delimit_values(json).map(serde_json::from_str)
}

fn delimit_values<'a>(json: &'a str) -> impl Iterator<Item=&'a str> {
    let mut index = 0;

    std::iter::from_fn(move || {
        if index >= json.len() {
            return None;
        } else {
            let start = index + json[index..].char_indices().take_while(|(_, c)| c.is_whitespace()).last().map(|(i, c)| i + c.len_utf8()).unwrap_or(0);

            match json[start..].chars().next() {
                Some('{') => {
                    let mut bracket_depth = 1;

                    let end = start + 1 + json[start+1..].char_indices()
                        .take_while(move |(_, c)| {
                            let res = bracket_depth > 0;

                            if c == &'}' {
                                bracket_depth -= 1;
                            } else if c == &'{' {
                                bracket_depth += 1;
                            }

                            return res;
                        })
                        .last()
                        .map(|(i, c)| i + c.len_utf8())
                        .unwrap_or(0);
                    
                    index = end;

                    if start == end {
                        None
                    } else {
                        Some(&json[start..end])
                    }
                },
                Some('[') => {
                    let mut bracket_depth = 1;

                    let end = start + 1 + json[start+1..].char_indices()
                        .take_while(move |(_, c)| {
                            let res = bracket_depth > 0;

                            if c == &']' {
                                bracket_depth -= 1;
                            } else if c == &'[' {
                                bracket_depth += 1;
                            }

                            return res;
                        })
                        .last()
                        .map(|(i, c)| i + c.len_utf8())
                        .unwrap_or(0);
                    
                    index = end;

                    if start == end {
                        None
                    } else {
                        Some(&json[start..end])
                    }
                },
                Some('"') => {
                    let mut escape_count = 0;

                    let end = start + 1 + json[start+1..].char_indices()
                        .take_while(move |(_, c)| {
                            let res = c != &'"' || escape_count % 2 == 0;

                            if c == &'\\' {
                                escape_count += 1;
                            } else {
                                escape_count = 0;
                            }

                            return res;
                        })
                        .last()
                        .map(|(i, c)| i + c.len_utf8())
                        .unwrap_or(0);
                    
                    index = end;

                    if start == end {
                        None
                    } else {
                        Some(&json[start..end])
                    }
                }
                _ => {
                    let end = start + json[start..].char_indices()
                        .take_while(move |(_, c)| c != &'{' && c != &'[' && c != &'"' && !c.is_whitespace())
                        .last()
                        .map(|(i, c)| i + c.len_utf8())
                        .unwrap_or(0);

                    index = end;

                    if start == end {
                        None
                    } else {
                        Some(&json[start..end])
                    }
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
