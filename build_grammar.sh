#!/bin/bash

rm -r build/*

bnfc --functor -o ./build Instant.cf 

cp Makefile Compiler.hs JVM.hs LLVM.hs ./build

cd build
make

#cd ..
#rm -r build




