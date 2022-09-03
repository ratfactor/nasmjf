#!/bin/bash

# Die upon any errors (especially the cd)
set -e
cd jonesforth/
#if [[ ! -f nasmjf ]]
#then
#    echo "I need to be run from the nasmjf directory."
#    exit 1
#fi

ls -l

for testfile in test_*.f
do
    testname=${testfile%%.f}
    echo "testfile: $testfile"

    # pipe the following into nasmjf:
    #   1. Some Forth to print a message
    #   2. the test forth file (defines the word TEST)
    #   3. Some Forth to call the word TEST
    # then pipe the output to sed which
    #   1. Skips welcome message lines
    #   2. Strips DSP??? from output
    cat <(echo 'CR ." BEEP BOOP. Test mode activated." CR ') \
        $testfile <(echo ' TEST CR ') # | ../nasmjf 2>&1 | \#
        #sed '1,4d'
    #diff -u 
done

#                @rm -f .$@
#                @cat <(echo ': TEST-MODE ;') jonesforth.f $< <(echo 'TEST') | \
#                  ./jonesforth 2>&1 | \
#                  sed 's/DSP=[0-9]*//g' > .$@
#                @diff -u .$@ $<.out
