use std::io::Read;

pub struct CharQueue<R: Read> {
    reader: R,
    eof: bool,
    buf: [u8; 65536],
    bytes_read: usize,
    cursor: usize,
}

impl<R: Read> CharQueue<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            buf: [0; 65536],
            bytes_read: 0,
            cursor: 0,
            eof: false,
        }
    }

    fn read_if_needed(&mut self) {
        if self.is_empty() && !self.eof {
            loop {
                let bytes_read = self.reader.read(&mut self.buf);

                if let Ok(bytes_read) = bytes_read {
                    if bytes_read == 0 {
                        self.eof = true;
                    }

                    self.bytes_read = bytes_read;
                    self.cursor = 0;
                    break;
                }
            }
        }
    }

    fn is_empty(&self) -> bool {
        self.cursor >= self.bytes_read
    }

    pub fn peek(&mut self) -> Option<u8> {
        self.read_if_needed();

        if self.is_empty() {
            None
        } else {
            Some(self.buf[self.cursor])
        }
    }

    pub fn pop(&mut self) -> Option<u8> {
        let val = self.peek();
        if self.cursor < self.bytes_read {
            self.cursor += 1;
        }
        val
    }
}

impl<R: Read> Iterator for CharQueue<R> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        self.pop()
    }
}

pub fn delimit_values<C: Iterator<Item = u8>>(
    mut bytes: C,
    elide_root_array: bool,
) -> impl Iterator<Item = Vec<u8>> {
    if elide_root_array {
        let mut first_byte = bytes.next();

        // skip whitespace
        while first_byte.map(|b| b.is_ascii_whitespace()).unwrap_or(false) {
            first_byte = bytes.next();
        }

        // skip open bracket
        if first_byte == Some(b'[') {
            bytes.next();
        } else {
            panic!("--elide-root-array was passed, but root value is not an array");
        }
    }

    let skip_fn = if elide_root_array {
        |b: u8| b.is_ascii_whitespace() || b == b','
    } else {
        |b: u8| b.is_ascii_whitespace()
    };

    let mut previous_sement_size = 0;
    std::iter::from_fn(move || {
        let mut next_json_bytes: Vec<u8> = Vec::with_capacity(previous_sement_size);
        let mut first_byte = bytes.next();

        // skip whitespace
        while first_byte.map(skip_fn).unwrap_or(false) {
            first_byte = bytes.next();
        }

        if let Some(first_byte) = first_byte {
            if elide_root_array && first_byte == b']' {
                bytes.next();
                return None;
            }

            next_json_bytes.push(first_byte);

            match first_byte {
                b'{' => {
                    let mut bracket_depth = 1;

                    while let Some(byte) = bytes.next() {
                        next_json_bytes.push(byte);

                        if byte == b'"' {
                            traverse_and_push_string(&mut bytes, &mut next_json_bytes).ok();
                        } else if byte == b'}' {
                            bracket_depth -= 1;

                            if bracket_depth == 0 {
                                previous_sement_size = next_json_bytes.len();
                                return Some(next_json_bytes);
                            }
                        } else if byte == b'{' {
                            bracket_depth += 1;
                        }
                    }

                    return None;
                }
                b'[' => {
                    let mut bracket_depth = 1;

                    while let Some(byte) = bytes.next() {
                        next_json_bytes.push(byte);

                        if byte == b'"' {
                            traverse_and_push_string(&mut bytes, &mut next_json_bytes).ok();
                        } else if byte == b']' {
                            bracket_depth -= 1;

                            if bracket_depth == 0 {
                                previous_sement_size = next_json_bytes.len();
                                return Some(next_json_bytes);
                            }
                        } else if byte == b'[' {
                            bracket_depth += 1;
                        }
                    }

                    return None;
                }
                b'"' => {
                    // string
                    return traverse_and_push_string(&mut bytes, &mut next_json_bytes)
                        .ok()
                        .map(|_| {
                            previous_sement_size = next_json_bytes.len();
                            next_json_bytes
                        });
                }
                _ => {
                    // number, boolean, or null
                    while let Some(byte) = bytes.next() {
                        if byte.is_ascii_whitespace() {
                            previous_sement_size = next_json_bytes.len();
                            return Some(next_json_bytes);
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

fn traverse_and_push_string<C: Iterator<Item = u8>>(
    bytes: &mut C,
    result_bytes: &mut Vec<u8>,
) -> Result<(), ()> {
    let mut escape_count = 0;

    while let Some(byte) = bytes.next() {
        result_bytes.push(byte);

        if byte == b'"' && escape_count % 2 == 0 {
            return Ok(());
        }

        if byte == b'\\' {
            escape_count += 1;
        } else {
            escape_count = 0;
        }
    }

    Err(())
}
