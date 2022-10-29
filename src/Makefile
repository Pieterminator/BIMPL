all: pgf top

pgf:
	gf -make -output-format=haskell --haskell=gadt PropEng.gf PropGGC.gf
top:
	ghc --make -XGADTs Trans.hs
