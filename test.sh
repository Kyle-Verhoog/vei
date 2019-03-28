#!/bin/bash

/home/kyle/cs444-w19-vei/bin/nasm -O1 -f elf -g -F dwarf output/Basic/output/__main.s
/home/kyle/cs444-w19-vei/bin/nasm -O1 -f elf -g -F dwarf output/Basic/output/src_main_resources_test_codegen_Basic.s
ld -melf_i386 -o output/Basic/main output/Basic/output/*.o
./output/Basic/main
echo "$?"
