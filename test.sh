#!/bin/bash

nasm -O1 -f elf -g -F dwarf output/Basic/output/__main.s
nasm -O1 -f elf -g -F dwarf output/Basic/output/src_main_resources_test_codegen_Basic.s
ld -melf_i386 -o output/Basic/main output/Basic/output/*.o
./output/Basic/main
echo "$?"
