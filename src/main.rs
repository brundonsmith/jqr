#![allow(dead_code)]

extern crate clap;
extern crate atty;

mod json_parser;
mod filter_parser;
mod filters;
mod json_model;

use std::{fs::File, io::BufReader, io::Read, io::Write, rc::Rc, time::Instant};

use filters::apply_filter;//, apply_filter_hardcoded};
use json_model::{JSONValue, create_indentation_string, write_json};


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
            clap::Arg::with_name("no-free")
                .long("no-free")
                .help("Direct the program to skip de-allocation of memory where possible, intentionally leaking objects (until the process ends) but saving time on system calls. In testing this tends to yield a 5%-10% performance improvement, at the expense of strictly-increasing memory usage.")
                .required(false)
                .takes_value(false)
        )
        .get_matches();
    
    let kind = matches.value_of("kind").unwrap();
    let json = matches.value_of("JSON");
    let pattern = matches.value_of("PATTERN").unwrap();
    let indentation_step: u8 = matches.value_of("indent").unwrap().parse().unwrap();
    let tab_indentation = matches.is_present("tab");
    let colored = !matches.is_present("monochrome-output") && (matches.is_present("color-output") || atty::is(atty::Stream::Stdout));
    let no_free = matches.is_present("no-free");

    let mut json_buffer = String::new();
    let json_str = if let Some(json) = json {
        match kind {
            "file" => {
                let mark = Instant::now();
                let mut file = File::open(json).map_err(|e| e.to_string())?;
                file.read_to_string(&mut json_buffer).map_err(|e| e.to_string())?;
                println!("File read took: {}ms", mark.elapsed().as_millis());
    
                json_buffer.as_str()
            },
            "inline" => json,
            _ => unreachable!()
        }
    } else {
        let stdin = std::io::stdin();
        let mut handle = stdin.lock();

        handle.read_to_string(&mut json_buffer).map_err(|e| e.to_string())?;
        
        json_buffer.as_str()
    };

    let json_parsed = json_parser::parse(json_str, no_free).map(|r| {
        match r {
            Ok(val) => val,
            Err(e) => {
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

                panic!("Error parsing JSON at {}:{} - {}", line, column, e.msg);
            }
        }
    });

    if no_free {
        std::mem::forget(json_str);
    }

    let filter_parsed = filter_parser::parse(pattern).unwrap();




    // let mark = Instant::now();
    // let json_parsed: Vec<JSONValue> = json_parsed.collect();
    // println!("JSON parse took: {}ms", mark.elapsed().as_millis());

    // let mark = Instant::now();
    // let filtered: Vec<JSONValue> = apply_filter(&filter_parsed, json_parsed.into_iter()).collect();
    // println!("Filtering took: {}ms", mark.elapsed().as_millis());



    let filtered = apply_filter(&filter_parsed, json_parsed);



    // let mark = Instant::now();
    let indentation_string = create_indentation_string(indentation_step, tab_indentation);
    let mut out = String::new();
    for val in filtered {
        write_json(&val, 0, &indentation_string, colored, &mut out);
        out.push('\n');

        if no_free {
            std::mem::forget(val);
        }
    }

    std::io::stdout().write_all(out.as_bytes()).map_err(|e| e.to_string())?;
    // println!("Writing out took: {}ms", mark.elapsed().as_millis());

    Ok(())
}


