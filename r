#!/bin/bash

# Usage:
#
#   r        Assemble, link, and run
#   r db     Assemble, link, and debug
#

set -e # quit on errors

F=nasmjf

# assemble! (-g enables debugging symbols)
nasm -f elf32 -g -o $F.o -l $F.listing -L+ $F.asm
ld $F.o -o $F
rm $F.o

if [[ $1 == 'db' ]]
then
    # -q         - skips the verbiage at the beginning
    # --command  - debug it with a script to set everything up
    gdb $F -q --command=gdb.script
else
    # run it
    ./$F
fi

