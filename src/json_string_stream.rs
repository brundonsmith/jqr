use std::{io::Read, collections::VecDeque};


pub struct CharQueue<R: Read> {
    reader: R,
    deque: VecDeque<u8>,
    eof: bool,
}

impl<R: Read> CharQueue<R> {

    pub fn new(reader: R) -> Self {
        Self {
            reader,
            deque: VecDeque::new(),
            eof: false,
        }
    }

    fn add_to_queue(&mut self) {
        if !self.eof {
            let mut buf: [u8;128] = [0;128];
            loop {
                let bytes_read = self.reader.read(&mut buf);
    
                if let Ok(bytes_read) = bytes_read {
                    if bytes_read == 0 {
                        self.eof = true;
                    }
    
                    self.deque.reserve_exact(bytes_read);
                    for i in 0..bytes_read {
                        self.deque.push_back(buf[i]);
                    }
    
                    break;
                }
            }
        }
    }

    pub fn peek(&mut self) -> Option<u8> {
        if self.deque.is_empty() && !self.eof {
            self.add_to_queue();
        }
        
        self.deque.front().map(|c| *c)
    }

    pub fn pop(&mut self) -> Option<u8> {
        if self.deque.is_empty() && !self.eof {
            self.add_to_queue();
        }

       self.deque.pop_front()
    }
}

impl<R: Read> Iterator for CharQueue<R> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        self.pop()
    }
}

pub fn delimit_values<C: Iterator<Item=u8>>(mut bytes: C) -> impl Iterator<Item=String> {

    std::iter::from_fn(move || {
        let mut next_json_bytes: Vec<u8> = Vec::new();
        let mut first_byte = bytes.next();

        // skip whitespace
        while first_byte.map(|b| b.is_ascii_whitespace()).unwrap_or(false) {
            first_byte = bytes.next();
        }

        if let Some(first_byte) = first_byte {

            next_json_bytes.push(first_byte);

            match first_byte {
                b'{' => {
                    let mut bracket_depth = 1;

                    while let Some(byte) = bytes.next() {
                        next_json_bytes.push(byte);

                        if byte == b'}' {
                            bracket_depth -= 1;
                        } else if byte == b'{' {
                            bracket_depth += 1;
                        }
                        
                        if bracket_depth == 0 {
                            return Some(String::from_utf8(next_json_bytes).unwrap());
                        }
                    }

                    return None;
                },
                b'[' => {
                    let mut bracket_depth = 1;

                    while let Some(byte) = bytes.next() {
                        next_json_bytes.push(byte);

                        if byte == b']' {
                            bracket_depth -= 1;
                        } else if byte == b'[' {
                            bracket_depth += 1;
                        }
                        
                        if bracket_depth == 0 {
                            return Some(String::from_utf8(next_json_bytes).unwrap());
                        }
                    }

                    return None;
                },
                b'"' => { // string
                    let mut escape_count = 0;

                    while let Some(byte) = bytes.next() {
                        next_json_bytes.push(byte);

                        if byte == b'"' && escape_count % 2 == 1 {
                            return Some(String::from_utf8(next_json_bytes).unwrap());
                        }

                        if byte == b'\\' {
                            escape_count += 1;
                        } else {
                            escape_count = 0;
                        }
                    }

                    return None;
                },
                _ => { // number, boolean, or null
                    while let Some(byte) = bytes.next() {
                        if byte.is_ascii_whitespace() {
                            return Some(String::from_utf8(next_json_bytes).unwrap());
                        } else {
                            next_json_bytes.push(byte);
                        }
                    }

                    return None;
                }
            }
        } else {
            return None;
        }
    })
}