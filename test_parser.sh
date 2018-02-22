#!/bin/bash

pos_tests=5
neg_tests=5
passed=0

for (( i = 0; i < pos_tests; i++ )); do
	filename="parser_p$((i + 1)).olisp"
	printf "\nPositive test: $filename\n"
	./olisp.native "tests/$filename"
	if [[ $? -eq 0 ]]; then
		printf "Accepted! Test passed.\n"
		((passed++))
	else
		printf "Rejected! Test failed.\n"
	fi
done

for (( i = 0; i < neg_tests; i++ )); do
	filename="parser_n$((i + 1)).olisp"
	printf "\nNegative test: $filename\n"
	./olisp.native "tests/$filename"
	if [[ $? -ne 0 ]]; then
		printf "Rejected! Test passed.\n"
		((passed++))
	else
		printf "Accepted! Test failed.\n"
	fi
done

printf "\n$passed of $((pos_tests + neg_tests)) tests passed!\n\n"

exit $((pos_tests + neg_tests - passed))
