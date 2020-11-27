#![allow(dead_code)]

extern crate clap;
extern crate atty;

mod json_model;
mod json_string_stream;
mod json_parser;
mod filter_model;
mod filter_parser;
mod filter_interpreter;

use std::{fs::File, io::Read, io::Write, time::Instant};

use clap::ArgMatches;
use filter_interpreter::apply_filter;//, apply_filter_hardcoded};
use filter_model::Filter;
use json_model::{create_indentation_string, write_json};
use json_parser::ParseError;
use json_string_stream::{CharQueue, delimit_values};
use flate2::read::GzDecoder;


fn main() -> Result<(),String> {

    let matches = clap::App::new("jqr")
        .version("0.1")
        .author("Brandon Smith <mail@brandonsmith.ninja>")
        .about("Partial Rust implementation of jq")
        .arg(
            clap::Arg::with_name("PATTERN")
                .help("The query pattern")
                .required(true)
                // .default_value(".")
        )
        .arg(
            clap::Arg::with_name("JSON")
                .help("File name or inlined JSON string")
                .required(false)
//                 .default_value("[
//   {
//     \"id\": 1,")
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
                .help("Attempt to parse and process input in a streaming fashion. Whitespace-separated JSON values will be parsed and filtered (and their results printed) one at a time. NOTE: This can be considerably slower, but it will allow processing of very large JSON inputs that can't fit into memory.")
                .required(false)
                .takes_value(false)
        )
        .arg(
            clap::Arg::with_name("gzipped")
                .long("gzipped")
                .help("Set this flag to signal that the input file or stdin data is gzipped. The compressed data will be decompressed before processing (works with or without --stream).")
                .required(false)
                .takes_value(false)
        )
        .arg(
            clap::Arg::with_name("no-free")
                .long("no-free")
                .help("Direct the program to skip de-allocation of memory where possible, intentionally leaking objects (until the process ends) but saving time on system calls. In testing this tends to yield a 5%-10% performance improvement, at the expense of strictly-increasing memory usage.")
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
    gzipped: bool,
    no_free: bool,

    indentation_string: String,
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
            gzipped: matches.is_present("gzipped"),
            no_free: matches.is_present("no-free"),

            indentation_string: create_indentation_string(indentation_step, tab_indentation),
            filter_parsed: filter_parser::parse(pattern).map_err(|e| e.to_string()).unwrap_or_else(|e| panic!(e))
        }
    }
}


fn do_regular(options: &Options) -> Result<(),String> {

    let mut json_buffer = String::new();
    let json_str = if let Some(json) = options.json {
        match options.kind {
            "file" => {
                let mut file = File::open(json).map_err(|e| e.to_string())?;

                if options.gzipped {
                    GzDecoder::new(file).read_to_string(&mut json_buffer).map_err(|e| e.to_string())?;
                } else {
                    file.read_to_string(&mut json_buffer).map_err(|e| e.to_string())?;
                }
    
                json_buffer.as_str()
            },
            "inline" => json,
            _ => unreachable!()
        }
    } else {
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();

        if options.gzipped {
            GzDecoder::new(handle).read_to_string(&mut json_buffer).map_err(|e| e.to_string())?;
        } else {
            handle.read_to_string(&mut json_buffer).map_err(|e| e.to_string())?;
        }
        
        json_buffer.as_str()
    };

    let json_parsed = json_parser::parse(json_str, options.no_free).map(|r| {
        match r {
            Ok(val) => val,
            Err(e) => {
                panic!(create_parse_error_string(json_str, e));
            }
        }
    });

    if options.no_free {
        std::mem::forget(json_str);
    }

    let filtered = apply_filter(&options.filter_parsed, json_parsed);

    let mut out = String::new();
    for val in filtered {
        write_json(&val, 0, &options.indentation_string, options.colored, &mut out);
        out.push('\n');

        if options.no_free {
            std::mem::forget(val);
        }
    }

    std::io::stdout().write_all(out.as_bytes()).map_err(|e| e.to_string())?;

    Ok(())
}


fn do_streaming(options: &Options) -> Result<(),String> {

    if let Some(json) = options.json {
        match options.kind {
            "file" => {
                let reader = File::open(json).map_err(|e| e.to_string())?;

                if options.gzipped {
                    filter_and_print(CharQueue::new(GzDecoder::new(reader)), options)?;
                } else {
                    filter_and_print(CharQueue::new(reader), options)?;
                }
            },
            "inline" => {
                let reader = json.as_bytes();

                if options.gzipped {
                    filter_and_print(CharQueue::new(GzDecoder::new(reader)), options)?;
                } else {
                    filter_and_print(CharQueue::new(reader), options)?;
                }
            },
            _ => unreachable!()
        }
    } else {
        let stdin = std::io::stdin();
        let reader = stdin.lock();

        if options.gzipped {
            filter_and_print(CharQueue::new(GzDecoder::new(reader)), options)?;
        } else {
            filter_and_print(CharQueue::new(reader), options)?;
        }
    };

    Ok(())
}

fn filter_and_print<C: Iterator<Item=u8>>(bytes: C, options: &Options) -> Result<(),String> {

    for json_str in delimit_values(bytes) {
        println!("{}", json_str);

        let json_parsed = json_parser::parse_one(&json_str, options.no_free)
            .map_err(|e| create_parse_error_string(&json_str, e))?;

        let mut out = String::new();
        for val in apply_filter(&options.filter_parsed, std::iter::once(json_parsed)) {
            write_json(&val, 0, &options.indentation_string, options.colored, &mut out);
            out.push('\n');
    
            if options.no_free {
                std::mem::forget(val);
            }
        }

        std::io::stdout().write_all(out.as_bytes()).map_err(|e| e.to_string())?;

        if options.no_free {
            std::mem::forget(json_str);
        }
    }

    Ok(())
}

fn create_parse_error_string(json_str: &str, e: ParseError) -> String {
    let mut line = 1;
    let mut column = 1;

    for c in json_str.char_indices().take_while(|(i, _)| *i < e.index - 1).map(|(_, c)| c) {
        if c == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    format!("Error parsing JSON at {}:{} - {}", line, column, e.msg)
}