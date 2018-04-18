#!/bin/bash

CC="clang"
LLC="llc"

OLISP="./olisp.native"
LOG="test_all.log"
TESTDIR="tests/"

tests=(
	test_print_1
	test_print_2
	test_define
	test_cast
	test_begin
	test_arithmetic
)

fails=(
	fail_add
	fail_call
	fail_define
)

total=0
passed=0

rm -f "$LOG"
printf "\\n"

for pass in "${tests[@]}"; do
	((++total))
	printf "testing %s..." "$pass"
	printf "test %s\\n" "$pass" >> "$LOG"
	if "$OLISP" "$TESTDIR$pass.olisp" > "$TESTDIR/$pass.ll" 2>> "$LOG"; then
		if "$LLC" "$TESTDIR$pass.ll" > "$TESTDIR$pass.s" 2>> "$LOG"; then
			if "$CC" -o "$TESTDIR$pass.exe" "$TESTDIR$pass.s" 2>> "$LOG"; then
				"$TESTDIR$pass.exe" > "$TESTDIR$pass.result" 2>> "$LOG"
				if diff "$TESTDIR$pass.out" "$TESTDIR$pass.result" >> "$LOG" 2>&1; then
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
