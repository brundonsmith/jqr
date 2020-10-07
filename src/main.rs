#![allow(dead_code)]

use filters::apply_filter;
use model::JSONValue;

extern crate clap;

mod model;
mod json_parser;
mod filter_parser;
mod filters;

fn main() {
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
        //     .arg(clap::Arg::with_name("path_pattern")
        //         .short("p")
        //         .long("path_pattern")
        //         .value_name("PAT")
        //         .help("A basic pattern string to filter which files will be searched. Asterisks ('*') will match any substring.")
        //         .default_value("*")
        //         .takes_value(true))
        //     .arg(clap::Arg::with_name("line_delimiter")
        //         .short("d")
        //         .long("line_delimiter")
        //         .value_name("CHAR")
        //         .help("The character that delimits 'lines'. Can be used, for example, to search a natural-language file by passing '.' to split on sentences. [default: \\n]")
        //         .takes_value(true))
        //     .arg(clap::Arg::with_name("line_pattern")
        //         .short("lp")
        //         .long("line_pattern")
        //         .value_name("PAT")
        //         .help("A basic pattern string to filter which lines will show up in results. Asterisks ('*') will match any substring.")
        //         .default_value("*")
        //         .takes_value(true))
        //     .arg(clap::Arg::with_name("trim_whitespace")
        //         .short("t")
        //         .long("trim_whitespace")
        //         .help("Trim whitespace from the start and end of each line before comparing."))
        //     .arg(clap::Arg::with_name("squash_chars")
        //         .short("s")
        //         .long("squash_chars")
        //         .help("Characters that should be 'squashed' when processing a line. When a character is 'squashed', any continuous sequence of that character will be treated as a single instance. This cen be used to, for example, normalize indentation.")
        //         .default_value("false")
        //         .multiple(true))
        .get_matches();
    
    let json_str = "";
    let json_parsed = json_parser::parse(json_str).unwrap();

    let filter_str = "";
    let filter_parsed = filter_parser::parse(filter_str).unwrap();

    let filtered = apply_filter(&filter_parsed, vec![json_parsed].into_iter()).collect::<Vec<JSONValue>>();

    println!("{:?}", &filtered);
}

