all: performance

# -optl-w is there to suppress the warnings about text relocs
verifier: GeometricVerifier.hs
	ghc -rtsopts -odir ../../binaries -hidir ../../binaries -i=../../ --make GeometricVerifier.hs -optl-w

performance: TPTPPerformanceLogger.hs
	ghc -rtsopts -odir ../../binaries -hidir ../../binaries -i=../../ --make TPTPPerformanceLogger.hs -optl-w

waverifier: WAVerifier.hs
	ghc -odir ../../binaries -hidir ../../binaries -i=../../ --make WAVerifier.hs -optl-w

clean:
	-rm ../binaries/*.hi ../binaries/*.o

realclean: clean
	-rm GeometricVerifier TPTPPerformanceLogger WAVerifier

rc: realclean
