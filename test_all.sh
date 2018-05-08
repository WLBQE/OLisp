#!/bin/bash

CC="clang"
LLC="llc"

OLISP="./olisp.native"
LOG="test_all.log"
TESTDIR="tests/"

tests=(
	test_print
	test_define
	test_begin
	test_arithmetic
	test_logical
	test_if
	test_lazy
	test_lambda
	test_class
	test_list
	int_iter
	int_map
	int_filter
	int_fold
	len
	sublist_from_head
	sublist_from_tail
	merge_sort
	reverse
	nth
)

fails=(
	fail_add
	fail_call
	fail_define1
  fail_define2
	fail_define3
	fail_class
)

total=0
passed=0

trap : SIGSEGV

rm -f "$LOG"
printf "\\n"

for pass in "${tests[@]}"; do
	((++total))
	printf "testing %s..." "$pass"
	printf "test %s\\n" "$pass" >> "$LOG"
	if "$OLISP" "$TESTDIR$pass.olisp" > "$TESTDIR/$pass.ll" 2>> "$LOG"; then
		if "$LLC" "$TESTDIR$pass.ll" "-o=$TESTDIR$pass.s" 2>> "$LOG"; then
			if "$CC" -o "$TESTDIR$pass.exe" "$TESTDIR$pass.s" list.o 2>> "$LOG"; then
				if "$TESTDIR$pass.exe" > "$TESTDIR$pass.result" 2>&1 &&
					diff "$TESTDIR$pass.out" "$TESTDIR$pass.result" >> "$LOG" 2>&1; then
					printf "passed\\n"
					((++passed))
				else
					printf "failed\\n"
				fi
				rm -f "$TESTDIR$pass.result"
			else
				printf "failed\\n"
			fi
			rm -f "$TESTDIR$pass.exe"
		else
			printf "failed\\n"
		fi
		rm -f "$TESTDIR$pass.s"
	else
		printf "failed\\n"
	fi
	rm -f "$TESTDIR$pass.ll"
	printf "\\n" >> "$LOG"
done

printf "\\n"

for fail in "${fails[@]}"; do
	((++total))
	printf "testing %s..." "$fail"
	printf "test %s\\n" "$fail" >> "$LOG"
	if "$OLISP" "$TESTDIR$fail.olisp" 2> "$TESTDIR$fail.result" >> "$LOG"; then
		printf "failed\\n"
	else
		if diff "$TESTDIR$fail.err" "$TESTDIR$fail.result" >> "$LOG" 2>&1; then
			printf "passed\\n"
			((++passed))
		else
			printf "failed\\n"
		fi
	fi
	rm -f "$TESTDIR$fail.result"
	printf "\\n" >> "$LOG"
done

printf "\\n%d of %d tests passed\\n\\n" $((passed)) $((total))

exit $((total - passed))
