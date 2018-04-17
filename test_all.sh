#!/bin/bash

CC="clang"
LLC="llc"

OLISP="./olisp.native"
LOG="test_all.log"
TESTDIR="tests/"

tests=(
	test_print_1
)

fails=(
	fail_add
	fail_define
)

failed=0

rm -f "$LOG"

for pass in "${tests[@]}"; do
	printf "testing %s..." "$pass"
	printf "test %s\\n" "$pass" >> "$LOG"
	if "$OLISP" "$TESTDIR$pass.olisp" > "$TESTDIR/$pass.ll" 2>> "$LOG"; then
		if "$LLC" "$TESTDIR$pass.ll" > "$TESTDIR$pass.s" 2>> "$LOG"; then
			if "$CC" -o "$TESTDIR$pass.exe" "$TESTDIR$pass.s" 2>> "$LOG"; then
				"$TESTDIR$pass.exe" > "$TESTDIR$pass.result" 2>> "$LOG"
				if diff "$TESTDIR$pass.out" "$TESTDIR$pass.result" >> "$LOG" 2>&1; then
					printf "passed\\n"
				else
					printf "failed1\\n"
					((++failed))
				fi
				rm -f "$TESTDIR$pass.result"
			else
				printf "failed\\n"
				((++failed))
			fi
			rm -f "$TESTDIR$pass.exe"
		else
			printf "failed\\n"
			((++failed))
		fi
		rm -f "$TESTDIR$pass.s"
	else
		printf "failed\\n"
		((++failed))
	fi
	rm -f "$TESTDIR$pass.ll"
	printf "\\n" >> "$LOG"
done

for fail in "${fails[@]}"; do
	printf "testing %s..." "$fail"
	printf "test %s\\n" "$fail" >> "$LOG"
	if "$OLISP" "$TESTDIR$fail.olisp" 2> "$TESTDIR$fail.result" >> "$LOG"; then
		printf "failed\\n"
		((++failed))
	else
		if diff "$TESTDIR$fail.err" "$TESTDIR$fail.result" >> "$LOG" 2>&1; then
			printf "passed\\n"
		else
			printf "failed\\n"
			((++failed))
		fi
	fi
	rm -f "$TESTDIR$fail.result"
	printf "\\n" >> "$LOG"
done

exit $((failed))
