# Time-stamp: "2013-02-23 12:46:57 Salman Saghafi"

# -optl-w is there to suppress the warnings about text relocs
build:  PerformanceLogger.hs
	ghc -odir ../../binaries -hidir ../../binaries -i=../../ --make PerformanceLogger.hs -optl-w

clean:
	-rm *.hi *.o

realclean: clean
	-rm PerformanceLogger

rc: realclean