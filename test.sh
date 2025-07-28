#!/bin/bash
assert_execution() {
  expected="$1"
  input="$2"

  ./build/bin/AliceCompiler "$input" > tmp.s || exit

  gcc -static -o tmp tmp.s
  ./tmp
  actual="$?"

  rm -f tmp.s tmp

	if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert_execution 0 0
assert_execution 42 42
assert_execution 123 123

echo OK
