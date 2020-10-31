#![allow(dead_code)]

extern crate clap;
extern crate atty;

mod json_parser;
mod filter_parser;
mod filters;
mod json_model;

use std::{fs::File, io::BufReader, io::Read, io::Write, rc::Rc, time::Instant};

use filters::apply_filter;//, apply_filter_hardcoded};
use json_model::{JSONValue, write_json};


fn main() -> Result<(),()> {

    let matches = clap::App::new("jqr")
        .version("0.1")
        .author("Brandon Smith <mail@brandonsmith.ninja>")
        .about("Partial Rust implementation of jq")
        .arg(
            clap::Arg::with_name("PATTERN")
                .help("The query pattern")
                .required(true)
                // .default_value("map(select(.base.Attack > 100)) | map(.name.english)")
        )
        .arg(
            clap::Arg::with_name("JSON")
                .help("File name or inlined JSON string")
                .required(false)
                // .default_value("/Users/brundolf/Downloads/query-json-master/benchmarks/big.json")
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
                .help("DANGER")
                .required(false)
                .takes_value(false)
        )
        .get_matches();

    // monochrome defined -> false
    // else colored defined -> true
    // else is terminal -> true
    // else -> false

    let indentation_step: u8 = matches.value_of("indent").unwrap().parse().unwrap();
    let tab_indentation = matches.is_present("tab");
    let colored = !matches.is_present("monochrome-output") && (matches.is_present("color-output") || atty::is(atty::Stream::Stdout));
    let no_free = matches.is_present("no-free");

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

    let json_parsed = json_parser::parse(json_str, no_free).map(|r| {
        match r {
            Ok(val) => val,
            Err(e) => panic!(format!("Error parsing JSON at"))// {}:{}\t{:?}", e.line(), e.column(), e.classify()))
        }
    });

    if no_free {
        std::mem::forget(json_str);
    }



    // let mark = Instant::now();
    // let json_parsed: Vec<JSONValue> = json_parsed.collect();

    // println!("JSON parse took: {}ms", mark.elapsed().as_millis());

    // let filter_str = matches.value_of("PATTERN").unwrap();
    // let filter_parsed = filter_parser::parse(filter_str).unwrap();

    // let mark = Instant::now();
    // let filtered: Vec<JSONValue> = apply_filter(&filter_parsed, json_parsed.into_iter()).collect();
    // println!("Filtering took: {}ms", mark.elapsed().as_millis());

    // let mut out_string = String::with_capacity(89000000);
    // let mut write_stdout = |s: &str| {
    //     out_string.push_str(s);

    //     Ok(())
    // };



    let filter_str = matches.value_of("PATTERN").unwrap();
    let filter_parsed = filter_parser::parse(filter_str).unwrap();

    let filtered = apply_filter(&filter_parsed, json_parsed);

    let mut stdout = std::io::stdout();
    let mut write_stdout = move |s: &str| stdout.write_all(s.as_bytes()).map(|_| ()).map_err(|_| ());



    // let mark = Instant::now();
    for val in filtered {
        write_json(&val, 0, indentation_step, tab_indentation, colored, &mut write_stdout)?;
        write_stdout("\n")?;

        if no_free {
            std::mem::forget(val);
        }
    }
    // println!("Writing out took: {}ms", mark.elapsed().as_millis());

    Ok(())
}


