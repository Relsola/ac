#!/bin/bash
ac="./build/debug/bin/ac"

tmp=`mktemp -d /tmp/ac-test-XXXXXX`
trap 'rm -rf $tmp' INT TERM HUP EXIT
echo > $tmp/empty.c

check() {
    if [ $? -eq 0 ]; then
        echo "testing $1 ... passed"
    else
        echo "testing $1 ... failed"
        exit 1
    fi
}

# -o
rm -f $tmp/out
$ac -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check -o

# --help
$ac --help 2>&1 | grep -q ac
check --help

# -S
echo 'int main() {}' | $ac -S -o - - | grep -q 'main:'
check -S

# Default output file
rm -f $tmp/out.o $tmp/out.s
echo 'int main() {}' > $tmp/out.c
(cd $tmp; $OLDPWD/$ac out.c)
[ -f $tmp/out.o ]
check 'default output file'

(cd $tmp; $OLDPWD/$ac -S out.c)
[ -f $tmp/out.s ]
check 'default output file'

echo OK
