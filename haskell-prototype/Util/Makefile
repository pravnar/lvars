
ifeq ($GHC,)
  GHC=ghc
endif

OPTS= -keep-tmp-files -dsuppress-module-prefixes -ddump-to-file -ddump-core-stats -ddump-simpl-stats -dcore-lint \
 -dcmm-lint -ddump-ds -ddump-simpl -ddump-stg -ddump-asm -ddump-bcos -ddump-cmm -ddump-opt-cmm -ddump-inlinings \
 -rtsopts -fforce-recomp -O2 -threaded

all:
	rm -f *.o *.hi bugged.exe unbugged.exe ./bugged_dumps/* ./unbugged_dumps/*
	$(GHC) -DACTIVATE_BUG $(OPTS) PBBS.hs -main-is Util.PBBS.t4 -o bugged.exe
	mkdir -p ./bugged_dumps/
	mv *.dump* ./bugged_dumps/
	$(GHC)                $(OPTS) PBBS.hs -main-is Util.PBBS.t4 -o unbugged.exe
	mkdir -p ./unbugged_dumps/
	mv *.dump* ./unbugged_dumps/
	time ./unbugged.exe
	time ./bugged.exe

    ghc-options: 
    ghc-options: 
