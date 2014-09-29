# -optl-w is there to suppress the warnings about text relocs
verifier: GeometricVerifier.hs
	ghc -rtsopts -odir ../../binaries -hidir ../../binaries -i=../../ --make GeometricVerifier.hs -optl-w

performance: TPTPPerformanceLogger.hs
	ghc -rtsopts -odir ../../binaries -hidir ../../binaries -i=../../ --make TPTPPerformanceLogger.hs -optl-w

profile: TPTPPerformanceLogger.hs
	ghc -O2 -odir ../../binaries -hidir ../../binaries -i=../../ -prof -auto-all -caf-all -fforce-recomp -optl-w --make TPTPPerformanceLogger.hs

waverifier: WAVerifier.hs
	ghc -odir ../../binaries -hidir ../../binaries -i=../../ --make WAVerifier.hs -optl-w

clean:
	-rm ../binaries/*.hi ../binaries/*.o

realclean: clean
	-rm GeometricVerifier TPTPPerformanceLogger WAVerifier

rc: realclean
