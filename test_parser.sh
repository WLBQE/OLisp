#!/bin/bash

OLISP="./olisp.native"
LOG="test_parser.log"
TESTDIR="tests/"

tests=(
	parser_p1
	parser_p2
	parser_p3
	parser_p4
	parser_p5
	parser_p6
)

fails=(
	parser_n1
	parser_n2
	parser_n3
	parser_n4
	parser_n5
)

total=0
passed=0

rm -f "$LOG"
printf "\\n"

for pass in "${tests[@]}"; do
	((++total))
	printf "testing %s..." "$pass"
	printf "test %s\\n" "$pass" >> "$LOG"
	if "$OLISP" -a "$TESTDIR$pass.olisp" > /dev/null 2>> "$LOG"; then
		printf "passed\\n"
		((++passed))
	else
		printf "failed\\n"
	fi
	printf "\\n" >> "$LOG"
done

printf "\\n"

for fail in "${fails[@]}"; do
	((++total))
	printf "testing %s..." "$fail"
	printf "test %s\\n" "$fail" >> "$LOG"
	if "$OLISP" -a "$TESTDIR$fail.olisp" 2> /dev/null >> "$LOG"; then
		printf "failed\\n"
	else
		printf "passed\\n"
		((++passed))
	fi
	printf "\\n" >> "$LOG"
done

printf "\\n%d of %d tests passed\\n\\n" $((passed)) $((total))

exit $((total - passed))
