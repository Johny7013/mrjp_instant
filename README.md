# mrjp_instant
Intsant complier to llvm and jvm written in Haskell

To complie programs run 'make'. This will generate Complier executable which is used by
insc_jvm and insc_llvm to generate complied programs.

insc_jvm path/baz.ins generates baz.j and baz.class in path dir 
insc_llvm path/baz.ins generates baz.ll and baz.bc in path dir

Run 'make clean' to remove build dir and Compiler executable. 
