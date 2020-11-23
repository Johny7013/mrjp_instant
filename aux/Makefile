all:
	happy -gca ParInstant.y
	alex -g LexInstant.x
	ghc --make TestInstant.hs -o TestInstant
	ghc --make Compiler.hs JVM.hs LLVM.hs -o ../Compiler

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* ComposOp.* Instant.dtd XMLInstant.* Compiler.*
	
