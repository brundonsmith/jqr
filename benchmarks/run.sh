#!/usr/bin/env bash

title () {
  echo ""
  echo ""
  echo "$1"
  echo "$2"
}

title "Running benchmark tests..."
echo "query-json: $(query-json --version)"
echo "jq: $(jq --version)"
echo "jqr: $(jqr --version)"
echo ""

test () {
  /usr/bin/time -f "%Us user | %Ss system | %es real" "$@"
}

title "### Pipe a json to stdin"
echo "-- jq --"
test cat benchmarks/big.json | jq '.' > /dev/null
echo "-- query-json --"
test cat benchmarks/big.json | query-json --kind=inline '.' > /dev/null
echo "-- jqr (regular, no-free, streaming) --"
test cat benchmarks/big.json | jqr '.' > /dev/null
test cat benchmarks/big.json | jqr '.' > /dev/null --no-free
test cat benchmarks/big.json | jqr '.' > /dev/null --stream

title "### Select an attribute on a small (4kb) JSON file" ".first.id"
echo "-- jq --"
test jq '.first.id' benchmarks/small.json > /dev/null
echo "-- query-json --"
test query-json '.first.id' benchmarks/small.json > /dev/null
echo "-- jqr (regular, no-free, streaming) --"
test jqr '.first.id' benchmarks/small.json > /dev/null
test jqr '.first.id' benchmarks/small.json > /dev/null --no-free
test jqr '.first.id' benchmarks/small.json > /dev/null --stream

title "### Select an attribute on a medium (132k) JSON file" "."
echo "-- jq --"
test jq '.' benchmarks/medium.json > /dev/null
echo "-- query-json --"
test query-json '.' benchmarks/medium.json > /dev/null
echo "-- jqr (regular, no-free, streaming) --"
test jqr '.' benchmarks/medium.json > /dev/null
test jqr '.' benchmarks/medium.json > /dev/null --no-free
test jqr '.' benchmarks/medium.json > /dev/null --stream

title "### Select an attribute on a big JSON (604k) file" "map(.)"
echo "-- jq --"
test jq 'map(.)' benchmarks/big.json > /dev/null
echo "-- query-json --"
test query-json 'map(.)' benchmarks/big.json > /dev/null
echo "-- jqr (regular, no-free, streaming) --"
test jqr 'map(.)' benchmarks/big.json > /dev/null
test jqr 'map(.)' benchmarks/big.json > /dev/null --no-free
test jqr 'map(.)' benchmarks/big.json > /dev/null --stream

title "### Simple operation on a small (4kb) JSON file" ".second.store.books | map(.price + 10)"
echo "-- jq --"
test jq '.second.store.books | map(.price + 10)' benchmarks/small.json > /dev/null
echo "-- query-json --"
test query-json '.second.store.books | map(.price + 10)' benchmarks/small.json > /dev/null
echo "-- jqr (regular, no-free, streaming) --"
test jqr '.second.store.books | map(.price + 10)' benchmarks/small.json > /dev/null
test jqr '.second.store.books | map(.price + 10)' benchmarks/small.json > /dev/null --no-free
test jqr '.second.store.books | map(.price + 10)' benchmarks/small.json > /dev/null --stream

title "### Simple operation on a medium (132k) JSON file" "map(.time)"
echo "-- jq --"
test jq 'map(.time)' benchmarks/medium.json > /dev/null
echo "-- query-json --"
test query-json 'map(.time)' benchmarks/medium.json > /dev/null
echo "-- jqr (regular, no-free, streaming) --"
test jqr 'map(.time)' benchmarks/medium.json > /dev/null
test jqr 'map(.time)' benchmarks/medium.json > /dev/null --no-free
test jqr 'map(.time)' benchmarks/medium.json > /dev/null --stream

title "### Simple operation an attribute on a big JSON (604k) file" "map(select(.base.Attack > 100)) | map(.name.english)"
echo "-- jq --"
test jq 'map(select(.base.Attack > 100)) | map(.name.english)' benchmarks/big.json > /dev/null
echo "-- query-json --"
test query-json 'filter(.base."Attack" > 100) | map(.name.english)' benchmarks/big.json > /dev/null
echo "-- jqr (regular, no-free, streaming) --"
test jqr 'map(select(.base.Attack > 100)) | map(.name.english)' benchmarks/big.json > /dev/null
test jqr 'map(select(.base.Attack > 100)) | map(.name.english)' benchmarks/big.json > /dev/null --no-free
test jqr 'map(select(.base.Attack > 100)) | map(.name.english)' benchmarks/big.json > /dev/null --stream

title "### Simple operation an attribute on a huge JSON (110M) file" "keys"
echo "-- jq --"
test jq 'keys' benchmarks/huge.json > /dev/null
echo "-- query-json --"
test query-json 'keys' benchmarks/huge.json > /dev/null
echo "-- jqr (regular, no-free, streaming) --"
test jqr 'keys' benchmarks/huge.json > /dev/null
test jqr 'keys' benchmarks/huge.json > /dev/null --no-free
test jqr 'keys' benchmarks/huge.json > /dev/null --stream

# title "### Simple operation an attribute on a huge JSON (74.9MB) file with many sequential values" "keys"
# test jq 'keys' /Users/brundolf/json-generator/manyobjects-1000000.json > /dev/null
# echo "        <not supported>"
# test jqr 'keys' /Users/brundolf/json-generator/manyobjects-1000000.json > /dev/null
# test jqr 'keys' /Users/brundolf/json-generator/manyobjects-1000000.json > /dev/null --no-free
# test jqr 'keys' /Users/brundolf/json-generator/manyobjects-1000000.json > /dev/null --stream

# title "### Simple operation an attribute on a gigantic JSON (1.14GB) file with many sequential values" "keys"
# test jq 'keys' /Users/brundolf/json-generator/manyobjects-15000000.json > /dev/null
# echo "        <not supported>"
# test jqr 'keys' /Users/brundolf/json-generator/manyobjects-15000000.json > /dev/null
# test jqr 'keys' /Users/brundolf/json-generator/manyobjects-15000000.json > /dev/null --no-free
