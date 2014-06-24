default: 

windows: directories
	ghc --make -odir binaries -hidir binaries -no-hs-main -shared -static -fno-shared-implib -lHSrts-ghc7.6.3 -optl-Wl,-rpath,"/cygdrive/c/Program Files (x86)/Haskell Platform/2013.2.0.0/lib" ffi.hs -o bin/ffi.dll

directories:
	-mkdir bin

clean:
	-rm -f binaries/*.hi binaries/*.o *.h

realclean: clean
	-rm -f bin/*.dll bin/*.so

rc: realclean
