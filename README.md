![Rust](https://github.com/brundonsmith/jqr/workflows/Rust/badge.svg)

Partial re-implementation of the `jq` command line utility in Rust


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
  - `--tab`
  - `--indent n`
  - `--color-output / -C` and `--monochrome-output / -M` ✅
  - ...rest ⚠️

- #### [Basic filters](https://stedolan.github.io/jq/manual/v1.6/#Basicfilters)
  - Identity: `.` ✅
  - Object Identifier-Index: `.foo`, `.foo.bar` ✅
  - Optional Object Identifier-Index: `.foo?` ✅
  - Generic Object Index: `.[<string>]` ✅
  - Array Index: `.[2]` ✅
  - Array/String Slice: `.[10:15]` ✅
  - Array/Object Value Iterator: `.[]` ✅
  - `.[]?` ⚠️
  - Comma: `,` ✅
  - Pipe: `|` ✅
  - Parenthesis: `()` ⚠️

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
  - `has(key)` ⚠️
  - `in` ⚠️
  - `map` ✅
  - `map_values` ⚠️
  - `del` ⚠️
  - `select` ✅
  - `flatten` ⚠️
  - `type` ⚠️
  - `sort`, `sort_by(path_expression)` ⚠️
  - `min`, `max` ⚠️
  - `reverse` ⚠️
  - `contains` ⚠️
  - `tojson`/`fromjson` ⚠️
  - Format strings and escaping: `@text`, `@csv`, etc.. 🔴

#### [Conditionals and Comparisons](https://stedolan.github.io/jq/manual/v1.6/#ConditionalsandComparisons)
  - `==`, `!=` ✅
  - `if-then-else` ⚠️
  - `>`, `>=`, `<=`, `<` ✅
  - `and`, `or`, `not` ✅
  - `//`
  - `break` 🔴

#### [Regular expressions (PCRE)](https://stedolan.github.io/jq/manual/v1.6/#RegularexpressionsPCRE) ⚠️

#### [Advanced features](https://stedolan.github.io/jq/manual/v1.6/#Advancedfeatures) ⚠️

#### [Assignment](https://stedolan.github.io/jq/manual/v1.6/#Assignment) ⚠️

#### [Modules](https://stedolan.github.io/jq/manual/v1.6/#Modules) ⚠️