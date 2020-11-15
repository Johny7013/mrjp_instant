#!/bin/bash

rm -r build/*

bnfc --functor --makefile -o build/ Instant.cf 

cd build
make





