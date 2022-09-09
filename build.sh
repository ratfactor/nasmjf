#!/bin/bash

# Usage:
#
#   build.sh        Assemble, link
#   build.sh gdb    Assemble, link, and debug
#   build.sh test   Assemble, link, and test
#   build.sh run    Assemble, link, and run

set -e # quit on errors

F=nasmjf

# assemble and link! (-g enables debugging symbols)
nasm -f elf32 -g -o $F.o $F.asm
ld $F.o -o $F
rm $F.o

if [[ $1 == 'gdb' ]]
then
    # -q         - skips the verbiage at the beginning
    # --command  - debug it with a script to set everything up
    gdb $F -q --command=gdb.script
    exit
fi

if [[ $1 == 'test' ]]
then
    ./test.sh
    exit
fi

if [[ $1 == 'run' ]]
then
    ./$F
    exit
fi
