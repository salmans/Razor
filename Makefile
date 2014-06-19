default: 

xml-test: directories
	ghc -rtsopts -odir binaries -hidir binaries --make xml-test.hs -optl-w -o bin/xml-test

windows: directories
	ghc --make -odir binaries -hidir binaries -no-hs-main -shared -static -fno-shared-implib -lHSrts-ghc7.6.3 -optl-Wl,-rpath,"/cygdrive/c/Program Files (x86)/Haskell Platform/2013.2.0.0/lib" ffi.hs -o bin/ffi.dll

directories:
	-mkdir bin

clean:
	-rm -f binaries/*.hi binaries/*.o *.h

realclean: clean
	-rm -rf bin

rc: realclean

profile: Main.hs
	ghc -O2 -odir binaries -hidir binaries -prof -auto-all -caf-all -fforce-recomp -optl-w --make Main.hs

# -o is ignored for now

# to profile it:
# ./go -i whatever  +RTS -p
# creates go.prof 
#
# ./go -i whatever  +RTS -sstderr
## shows info on command line
#
# To extract a standard heap profile run it with the -hc runtime flag:
# ... +RTS -hc -p -K100M
# then "hp2ps go.hp" will make go.ps
