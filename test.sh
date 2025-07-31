#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  ./build/debug/bin/AliceCompiler "$input" > tmp.s || exit

  gcc -static -o tmp tmp.s
  ./tmp
  actual="$?"

  rm -f tmp.s tmp

	if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
		rm -f tmp
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 0
assert 42 42
assert 21 '5+20-4'
assert 41 ' 12 + 34 - 5 '

echo OK
