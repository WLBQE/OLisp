#!/bin/bash

pos_tests=5
neg_tests=5
passed=0

OLISP="./olisp.native"

for (( i = 0; i < pos_tests; i++ )); do
	filename="parser_p$((i + 1)).olisp"
	printf "\\nPositive test: %s\\n" "$filename"
	if "$OLISP" -a "tests/$filename"; then
		printf "Accepted! Test passed.\\n"
		((++passed))
	else
		printf "Rejected! Test failed.\\n"
	fi
done

for (( i = 0; i < neg_tests; i++ )); do
	filename="parser_n$((i + 1)).olisp"
	printf "\\nNegative test: %s\\n" "$filename"
	if "$OLISP" -a "tests/$filename"; then
		printf "Accepted! Test failed.\\n"
	else
		printf "Rejected! Test passed.\\n"
		((++passed))
	fi
done

printf "\\n%d of %d tests passed!\\n\\n" $((passed)) $((pos_tests + neg_tests))

exit $((pos_tests + neg_tests - passed))
