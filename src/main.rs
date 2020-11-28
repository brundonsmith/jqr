#![allow(dead_code)]

extern crate clap;
extern crate atty;
extern crate fxhash;

mod json_model;
mod json_string_stream;
mod json_parser;
mod filter_model;
mod filter_parser;
mod filter_interpreter;

use std::{fs::File, io::Read, io::Write};

use clap::ArgMatches;
use filter_interpreter::apply_filter;//, apply_filter_hardcoded};
use filter_model::Filter;
use json_model::{create_indentation_string, write_json};
use json_parser::ParseError;
use json_string_stream::{CharQueue, delimit_values};
use flate2::read::MultiGzDecoder;


fn main() -> Result<(),String> {

    let matches = clap::App::new("jqr")
        .version("0.1")
        .author("Brandon Smith <mail@brandonsmith.ninja>")
        .about("Partial Rust implementation of jq")
        .arg(
            clap::Arg::with_name("PATTERN")
                .help("The query pattern")
                .required(true)
        )
        .arg(
            clap::Arg::with_name("JSON")
                .help("File name or inlined JSON string")
                .required(false)
        )
        .arg(
            clap::Arg::with_name("kind")
                .long("kind")
                .help("Type of input")
                .required(false)
                .default_value("file")
                .possible_values(&["file", "inline"]),
        )
        .arg(
            clap::Arg::with_name("indent")
                .long("indent")
                .help("Number of spaces to indent by")
                .default_value("2")
                .required(false)
        )
        .arg(
            clap::Arg::with_name("tab")
                .long("tab")
                .help("Indent with tabs instead of spaces (--indent value is ignored)")
                .required(false)
                .takes_value(false)
        )
        .arg(
            clap::Arg::with_name("color-output")
                .long("color-output")
                .short("C")
                .help("Force colored output")
                .required(false)
                .takes_value(false)
        )
        .arg(
            clap::Arg::with_name("monochrome-output")
                .long("monochrome-output")
                .short("M")
                .help("Force monochrome output")
                .required(false)
                .takes_value(false)
        )
        .arg(
            clap::Arg::with_name("stream")
                .long("stream")
                .help(&normalize_help_text(
                    "Attempt to parse and process input in a streaming fashion. 
                    Whitespace-separated JSON values will be parsed and filtered 
                    (and their results printed) one at a time. NOTE: This can be 
                    considerably slower, but it will allow processing of very 
                    large JSON inputs that can't fit into memory."))
                .required(false)
                .takes_value(false)
        )
        .arg(
            clap::Arg::with_name("elide-root-array")
                .long("elide-root-array")
                .short("era")
                .help(&normalize_help_text(
                    "If this flag is passed and your
                    input JSON has at its root an array, the base array will
                    be ignored and its contents treated as if they were 
                    whitespace-separated values. This is useful when combined 
                    with --stream, as it allows the values to be 
                    processed one at a time, which is not otherwise possible for
                    a single atomic root value (even with --stream enabled).
                    
                    This will produce the same output as adding \".[] |\" to the 
                    beginning of your filter, and in fact this is how it's 
                    implemented for the non-streaming case, but changing the
                    filter does not afford the streaming benefits of passing the 
                    flag."))
                .required(false)
                .takes_value(false)
        )
        .arg(
            clap::Arg::with_name("gzipped")
                .long("gzipped")
                .help(&normalize_help_text(
                    "Set this flag to signal that the input file or stdin data 
                    is gzipped. The compressed data will be decompressed before 
                    processing (works with or without --stream)."))
                .required(false)
                .takes_value(false)
        )
        .arg(
            clap::Arg::with_name("no-free")
                .long("no-free")
                .help(&normalize_help_text(
                    "Direct the program to skip de-allocation of memory where 
                    possible, intentionally leaking objects (until the process 
                    ends) but saving time on system calls. In testing this 
                    tends to yield a 5%-10% performance improvement, at the 
                    expense of strictly-increasing memory usage."))
                .required(false)
                .takes_value(false)
        )
        .get_matches();

    let options = Options::from(&matches);

    if !options.stream {
        do_regular(&options)?;
    } else {
        do_streaming(&options)?;
    }

    Ok(())
}

struct Options<'a> {
    kind: &'a str,
    json: Option<&'a str>,
    indentation_step: u8,
    tab_indentation: bool,
    colored: bool,
    stream: bool,
    elide_root_array: bool,
    gzipped: bool,
    no_free: bool,

    indentation_string: Vec<u8>,
    filter_parsed: Filter<'a>,
}

impl<'a> Options<'a> {
    pub fn from(matches: &'a ArgMatches) -> Self {
        let pattern = matches.value_of("PATTERN").unwrap();
        let indentation_step = matches.value_of("indent").unwrap().parse().unwrap();
        let tab_indentation = matches.is_present("tab");

        Options {
            kind: matches.value_of("kind").unwrap(),
            json: matches.value_of("JSON"),
            indentation_step,
            tab_indentation,
            colored: !matches.is_present("monochrome-output") && (matches.is_present("color-output") || atty::is(atty::Stream::Stdout)),
            stream: matches.is_present("stream"),
            elide_root_array: matches.is_present("elide-root-array"),
            gzipped: matches.is_present("gzipped"),
            no_free: matches.is_present("no-free"),

            indentation_string: create_indentation_string(indentation_step, tab_indentation),
            filter_parsed: filter_parser::parse(pattern).map_err(|e| e.to_string()).unwrap_or_else(|e| panic!(e))
        }
    }
}


fn do_regular(options: &Options) -> Result<(),String> {

    let mut json_buffer = Vec::new();
    let json_slice: &[u8] = if let Some(json) = options.json {
        match options.kind {
            "file" => {
                let mut file = File::open(json).map_err(|e| e.to_string())?;

                if options.gzipped {
                    MultiGzDecoder::new(file).read_to_end(&mut json_buffer).map_err(|e| e.to_string())?;
                } else {
                    file.read_to_end(&mut json_buffer).map_err(|e| e.to_string())?;
                }
    
                &json_buffer
            },
            "inline" => json.as_bytes(),
            _ => unreachable!()
        }
    } else {
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();

        if options.gzipped {
            MultiGzDecoder::new(handle).read_to_end(&mut json_buffer).map_err(|e| e.to_string())?;
        } else {
            handle.read_to_end(&mut json_buffer).map_err(|e| e.to_string())?;
        }
        
        &json_buffer
    };

    let json_parsed = json_parser::parse(json_slice, options.no_free).map(|r| {
        match r {
            Ok(val) => val,
            Err(e) => {
                println!("{}", create_parse_error_string(json_slice, e));
                panic!();
            }
        }
    });

    if options.no_free {
        std::mem::forget(json_slice);
    }

    let filter: Filter = if options.elide_root_array {
        if let Filter::Pipe(vec) = &options.filter_parsed {
            let mut stages = vec.clone();
            stages.insert(0, Filter::Slice { start: None, end: None, optional: false });
            Filter::Pipe(stages)
        } else {
            Filter::Pipe(vec![ Filter::Slice { start: None, end: None, optional: false }, options.filter_parsed.clone() ])
        }
    } else {
        options.filter_parsed.clone()
    };

    let filtered = apply_filter(&filter, json_parsed);

    let mut out = Vec::new();
    for val in filtered {
        write_json(&val, 0, &options.indentation_string, options.colored, &mut out);
        out.push(b'\n');

        if options.no_free {
            std::mem::forget(val);
        }
    }

    std::io::stdout().write_all(&out).map_err(|e| e.to_string())?;

    Ok(())
}


fn do_streaming(options: &Options) -> Result<(),String> {

    if let Some(json) = options.json {
        match options.kind {
            "file" => {
                let reader = File::open(json).map_err(|e| e.to_string())?;

                if options.gzipped {
                    filter_and_print(MultiGzDecoder::new(reader), options)?;
                } else {
                    filter_and_print(reader, options)?;
                }
            },
            "inline" => {
                let reader = json.as_bytes();

                if options.gzipped {
                    filter_and_print(MultiGzDecoder::new(reader), options)?;
                } else {
                    filter_and_print(reader, options)?;
                }
            },
            _ => unreachable!()
        }
    } else {
        let stdin = std::io::stdin();
        let reader = stdin.lock();

        if options.gzipped {
            filter_and_print(MultiGzDecoder::new(reader), options)?;
        } else {
            filter_and_print(reader, options)?;
        }
    };

    Ok(())
}

fn filter_and_print<R: Read>(reader: R, options: &Options) -> Result<(),String> {
    let bytes = CharQueue::new(reader);

    // println!("reached loop");
    for json_bytes in delimit_values(bytes, options.elide_root_array) {
        // let end = usize::min(30, json_bytes.len());
        // println!("{}", &json_str[0..end]);

        let json_parsed = json_parser::parse_one(&json_bytes, options.no_free)
            .map_err(|e| create_parse_error_string(&json_bytes, e))?;

        let mut out = Vec::new();
        for val in apply_filter(&options.filter_parsed, std::iter::once(json_parsed)) {
            write_json(&val, 0, &options.indentation_string, options.colored, &mut out);
            out.push(b'\n');
    
            if options.no_free {
                std::mem::forget(val);
            }
        }

        std::io::stdout().write_all(&out).map_err(|e| e.to_string())?;

        if options.no_free {
            std::mem::forget(json_bytes);
        }
    }

    Ok(())
}

fn create_parse_error_string(json_slice: &[u8], e: ParseError) -> String {
    let json_str = std::str::from_utf8(json_slice).unwrap();
    let mut line = 1;
    let mut column = 1;

    for c in json_str.char_indices().take_while(|(i, _)| *i + 1 < e.index).map(|(_, c)| c) {
        if c == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    let preview_end = usize::min(e.index+30, json_str.len());
    format!("Error parsing JSON at {}:{} - {}\nNear here:\n{}", line, column, e.msg, &json_str[e.index..preview_end])
}

fn normalize_help_text(s: &str) -> String {
    let mut first = true;
    s.split_whitespace().fold(String::new(), |mut acc, word| {
        if !first {
            acc += " ";
        } else {
            first = false;
        }

        acc + word
    })
}