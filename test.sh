#!/bin/bash

# Die upon any errors
set -e

if [[ ! -f nasmjf ]]
then
    echo "I need to be run from the nasmjf directory."
    exit 1
fi

for testfile in jonesforth/test_*.f
do
#    testname=${testfile%%.f}
    echo "Testing: $testfile"

    # Pipe the following into nasmjf:
    #   1. Some Forth to print a message
    #   2. The test forth file (defines the word TEST)
    #   3. Some Forth to call the word TEST
    # Then pipe the output to sed which:
    #   1. Skips welcome message lines
    #   2. Removes unique "DSP=nnnnnn" from stack trace test.
    cat <(echo 'CR ." BEEP BOOP. Test mode activated." CR ') \
        $testfile <(echo ' TEST') \
        | ./nasmjf  2>&1 | \
        sed '1,4d;s/DSP=[0-9]*//g' > $testfile.myout
    # See if we match the expected output:
    diff -u $testfile.myout $testfile.out
done
