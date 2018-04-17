#!/bin/bash

OLISP="./olisp.native"
LOG="test_semant.log"
TESTDIR="tests/"

tests=(
	parser_p1
	parser_p2
	parser_p3
	parser_p4
	parser_p5
)

total=0
passed=0

rm -f "$LOG"
printf "\\n"

for pass in "${tests[@]}"; do
	((++total))
	printf "testing %s..." "$pass"
	printf "test %s\\n" "$pass" >> "$LOG"
	if "$OLISP" -s "$TESTDIR$pass.olisp" > /dev/null 2>> "$LOG"; then
		printf "passed\\n"
		((++passed))
	else
		printf "failed\\n"
	fi
	printf "\\n" >> "$LOG"
done

printf "\\n%d of %d tests passed\\n\\n" $((passed)) $((total))

exit $((total - passed))
