#!/bin/bash

# Usage:
#
#   r        Assemble, link, and run
#   r db     Assemble, link, and debug
#

-e # die on error

F=nasmjf

# assemble! (-g enables debugging symbols)
nasm -f elf32 -g -o $F.o $F.asm
ld $F.o -o $F

if [[ $1 == 'db' ]]
then
    # debug it with a script to set everything up
    gdb $F --command=gdb.script
else
    # run it
    ./$F
fi

