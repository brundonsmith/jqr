#![allow(dead_code)]

use std::{fs::File, io::Read, io::Write};

use filters::apply_filter;

extern crate clap;

mod model;
mod json_parser;
mod filter_parser;
mod filters;

fn main() -> Result<(),()> {
    let matches = clap::App::new("jqr")
        .version("0.1")
        .author("Brandon Smith <mail@brandonsmith.ninja>")
        .about("Partial Rust implementation of jq")
        .arg(
            clap::Arg::with_name("PATTERN")
                .help("The query pattern")
                .required(true)
                .display_order(0),
        )
        .arg(
            clap::Arg::with_name("JSON")
                .help("File name or inlined JSON string")
                .required(false)
                .display_order(1),
        )
        .arg(
            clap::Arg::with_name("kind")
                .help("Type of input")
                .required(false)
                .display_order(0)
                .default_value("file")
                .possible_values(&["file", "inline"]),
        )
        .get_matches();

    let mut json_buffer = String::new();
    let json_str = if matches.is_present("JSON") {
        match matches.value_of("kind").unwrap() {
            "file" => {
                let mut file = File::open(matches.value_of("JSON").unwrap()).map_err(|_| ())?;
                file.read_to_string(&mut json_buffer).map_err(|_| ())?;
    
                json_buffer.as_str()
            },
            "inline" => matches.value_of("JSON").unwrap(),
            _ => unreachable!()
        }
    } else {
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();

        handle.read_to_string(&mut json_buffer).map_err(|_| ())?;
        
        json_buffer.as_str()
    };
    
    let json_parsed = json_parser::parse(json_str).map(|r| r.unwrap());

    let filter_str = matches.value_of("PATTERN").unwrap();
    let filter_parsed = filter_parser::parse(filter_str).unwrap();

    let filtered = apply_filter(&filter_parsed, json_parsed);

    for val in filtered {
        std::io::stdout().write(format!("{}", val).as_bytes()).map_err(|_| ())?;
    }

    Ok(())
}

