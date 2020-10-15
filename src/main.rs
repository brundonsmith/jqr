#![allow(dead_code)]

use std::{fs::File, io::BufReader, io::Read, io::Write, time::Instant};

use filters::apply_filter;
use json_parser::delimit_values;
use serde_json::Value;

extern crate clap;

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
                let mark = Instant::now();
                let mut file = File::open(matches.value_of("JSON").unwrap()).map_err(|_| ())?;
                file.read_to_string(&mut json_buffer).map_err(|_| ())?;
                println!("File read took: {}ms", mark.elapsed().as_millis());
    
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

    // let mark = Instant::now();
    // let json_delimited: Vec<&str> = json_parser::delimit_values(json_str).collect();
    // println!("JSON delimiting took: {}ms", mark.elapsed().as_millis());

    // let mark = Instant::now();
    // let json_parsed: Vec<Value> = json_parser::parse(json_str).map(|r| {
    //     match r {
    //         Ok(val) => val,
    //         Err(e) => panic!(format!("Error parsing JSON at {}:{}\t{:?}", e.line(), e.column(), e.classify()))
    //     }
    // }).collect();
    // println!("JSON parse took: {}ms", mark.elapsed().as_millis());

    // let filter_str = matches.value_of("PATTERN").unwrap();
    // let filter_parsed = filter_parser::parse(filter_str).unwrap();

    // let json_parsed_a = json_parsed.clone();
    // let mark = Instant::now();
    // json_parsed_a.into_iter().for_each(std::mem::drop);
    // println!("Collecting took: {}ms", mark.elapsed().as_millis());

    // let json_parsed_b = json_parsed.clone();
    // let mark = Instant::now();
    // Box::new(json_parsed_b.into_iter()).for_each(std::mem::drop);
    // println!("Boxed collecting took: {}ms", mark.elapsed().as_millis());

    // let mark = Instant::now();
    // apply_filter(&filter_parsed, json_parsed.into_iter()).map(std::mem::forget);
    // println!("Filtering took: {}ms", mark.elapsed().as_millis());

    let json_parsed = json_parser::parse(json_str).map(|r| {
        match r {
            Ok(val) => val,
            Err(e) => panic!(format!("Error parsing JSON at {}:{}\t{:?}", e.line(), e.column(), e.classify()))
        }
    });

    let filter_str = matches.value_of("PATTERN").unwrap();
    let filter_parsed = filter_parser::parse(filter_str).unwrap();

    let filtered = apply_filter(&filter_parsed, json_parsed);

    for val in filtered {
        std::io::stdout().write(serde_json::to_string_pretty(&val).unwrap().as_bytes()).map_err(|_| ())?;
        std::io::stdout().write("\n".as_bytes()).map_err(|_| ())?;
        std::mem::forget(val);
    }

    Ok(())
}
