#!/bin/bash

./bin/nasm -O1 -f elf -g -F dwarf output/Basic/output/__main.s
./bin/nasm -O1 -f elf -g -F dwarf output/Basic/output/src_main_resources_test_codegen_Basic.s
./bin/nasm -O1 -f elf -g -F dwarf output/Basic/output/src_main_resources_test_marmoset_lib_5_java_lang_String.s
ld -melf_i386 -o output/Basic/main output/Basic/output/*.o
./output/Basic/main
echo "$?"
