![Rust](https://github.com/brundonsmith/jqr/workflows/Rust/badge.svg)

Partial re-implementation of the `jq` command line utility in Rust. The 
supported features can generally be described as "the things most people use, 
without the hyper-complicated or the hyper-niche". Details can be found below.

`jqr` is significantly faster than `jq` in all non-streaming cases that have 
been tested.

## Streaming
`jq`'s streaming syntax is notoriously inscrutable, but some kind of streaming 
support is necessary when you want to process more JSON than can fit in memory 
at one time.

To this end, `jqr` implements a much more intuitive (if possibly less 
performant) form of streaming. There is no special syntax: you use regular 
filter expressions the regular way (and enable with `--stream`). The only 
stipulations are that, to take advantage of streaming, the root of your JSON 
must either be:

1) Whitespace-separated values. If streaming is enabled, each value will be 
parsed and processed one at a time.

2) A single array. For this case, to process as a stream, you must also pass 
the `--elide-root-array` flag. This will instruct the parser to ignore the root
array and treat its values **as if they were whitespace-separated values**. Note
that this will change the filter output slightly: it's equivalent to prepending
`.[] | ` at the beginning of your filter expression.


```
USAGE:
    jqr [FLAGS] [OPTIONS] <PATTERN> [JSON]

FLAGS:
    -C, --color-output         Force colored output
    -e, --elide-root-array     If this flag is passed and your input JSON has at its root an array, the base array will
                               be ignored and its contents treated as if they were whitespace-separated values. This is
                               useful when combined with --stream, as it allows the values to be processed one at a
                               time, which is not otherwise possible for a single atomic root value (even with --stream
                               enabled). This will produce the same output as adding ".[] |" to the beginning of your
                               filter, and in fact this is how it's implemented for the non-streaming case, but changing
                               the filter does not afford the streaming benefits of passing the flag.
        --gzipped              Set this flag to signal that the input file or stdin data is gzipped. The compressed data
                               will be decompressed before processing (works with or without --stream).
    -h, --help                 Prints help information
    -M, --monochrome-output    Force monochrome output
        --no-free              Direct the program to skip de-allocation of memory where possible, intentionally leaking
                               objects (until the process ends) but saving time on system calls. In testing this tends
                               to yield a 5%-10% performance improvement, at the expense of strictly-increasing memory
                               usage.
        --stream               Attempt to parse and process input in a streaming fashion. Whitespace-separated JSON
                               values will be parsed and filtered (and their results printed) one at a time. NOTE: This
                               can be considerably slower, but it will allow processing of very large JSON inputs that
                               can't fit into memory.
        --tab                  Indent with tabs instead of spaces (--indent value is ignored)
    -V, --version              Prints version information

OPTIONS:
        --indent <indent>    Number of spaces to indent by [default: 2]
        --kind <kind>        Type of input [default: file]  [possible values: file, inline]

ARGS:
    <PATTERN>    The query pattern
    <JSON>       File name or inlined JSON string
```


## Currently supported feature set:

| Badge | Meaning             |
| ----- | ------------------- |
| ✅    | Implemented         |
| ⚠️    | Not implemented yet |
| 🔴    | Won't implement     |

##### Based on jq 1.6

- #### [CLI: Invoking jq](https://stedolan.github.io/jq/manual/v1.6/#Invokingjq)
  - `--version` ✅
  <!-- - `--kind`. This is different than jq ✅
    - `--kind=file` and the 2nd argument can be a json file
    - `--kind=inline` and the 2nd argument can be a json as a string -->
  - `--tab` ✅
  - `--indent n` ✅
  - `--color-output / -C` and `--monochrome-output / -M` ✅
  - Whitespace-separated JSON values as input ✅
  - ...rest ⚠️

- #### [Basic filters](https://stedolan.github.io/jq/manual/v1.6/#Basicfilters)
  - Identity: `.` ✅
  - Object Identifier-Index: `.foo`, `.foo.bar` ✅
  - Optional Object Identifier-Index: `.foo?` ✅
  - Generic Object Index: `.[<string>]` ✅
  - Array Index: `.[2]` ✅
  - Array/String Slice: `.[10:15]` ✅
  - Array/Object Value Iterator: `.[]` ✅
  - `.[]?` ✅
  - Comma: `,` ✅
  - Pipe: `|` ✅
  - Parenthesis: `()` ✅

- #### [Types and Values](https://stedolan.github.io/jq/manual/v1.6/#TypesandValues)
  - Array construction: `[]` ⚠️
  - Object construction: `{}` ⚠️
  - Recursive Descent: `..` ⚠️

- #### [Builtin operators and functions](https://stedolan.github.io/jq/manual/v1.6/#Builtinoperatorsandfunctions)

  - Addition: `+` ✅
  - Subtraction: `-` ✅
  - Multiplication, division, modulo: `*`, `/`, and `%` ✅
  - `length` ✅
  - `keys`, `key_unsorted` ✅
  - `map` ✅
  - `select` ✅
  - `has(key)` ✅
  - `in` ⚠️
  - `flatten` ✅
  - `map_values` ⚠️
  - `del` ⚠️
  - `type` ✅
  - `sort`, `sort_by(path_expression)` ✅
  - `min`, `max` ✅
  - `reverse` ✅
  - `contains` ⚠️
  - `tojson`/`fromjson` ⚠️
  - Format strings and escaping: `@text`, `@csv`, etc.. 🔴

#### [Conditionals and Comparisons](https://stedolan.github.io/jq/manual/v1.6/#ConditionalsandComparisons)
  - `==`, `!=` ✅
  - `if-then-else` ⚠️
  - `>`, `>=`, `<=`, `<` ✅
  - `and`, `or`, `not` ✅
  - `//` ✅
  - `break` 🔴

#### [Regular expressions (PCRE)](https://stedolan.github.io/jq/manual/v1.6/#RegularexpressionsPCRE) 🔴

#### [Advanced features](https://stedolan.github.io/jq/manual/v1.6/#Advancedfeatures) 🔴

#### [Assignment](https://stedolan.github.io/jq/manual/v1.6/#Assignment) 🔴

#### [Modules](https://stedolan.github.io/jq/manual/v1.6/#Modules) 🔴