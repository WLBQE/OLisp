#!/bin/sh

pos_tests=5
neg_tests=5
passed=0

for (( i = 0; i < pos_tests; i++ )); do
	filename="parser_p$((i + 1)).olisp"
	echo "\nPositive test: $filename"
	./olisp.native "tests/$filename"
	if [[ $? -eq 0 ]]; then
		echo "Accepted! Test passed."
		((passed++))
	else
		echo "Rejected! Test failed."
	fi
done

for (( i = 0; i < neg_tests; i++ )); do
	filename="parser_n$((i + 1)).olisp"
	echo "\nNegative test: $filename"
	./olisp.native "tests/$filename"
	if [[ $? -ne 0 ]]; then
		echo "Rejected! Test passed."
		((passed++))
	else
		echo "Accepted! Test failed."
	fi
done

echo "\n$passed of $((pos_tests + neg_tests)) tests passed!\n"
