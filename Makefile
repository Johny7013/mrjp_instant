all:
	mkdir -p build
	bnfc --functor -o ./build ./src/Instant.cf 
	happy -gca ./build/ParInstant.y
	alex -g ./build/LexInstant.x
	cp ./src/Compiler.hs ./src/JVM.hs ./src/LLVM.hs ./aux/Makefile ./build
	make -C build

clean:
	-rm Compiler
	-rm -rf build
	
