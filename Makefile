
# -optl-w is there to suppress the warnings about text relocs
chase:  .PHONY chase.hs
	ghc -odir binaries -hidir binaries --make chase.hs -optl-w -o bin/chase

all: .PHONY main chase

main:   Main.hs
	ghc -odir binaries -hidir binaries --make Main.hs -optl-w

.PHONY: directories

directories:
	-mkdir bin



clean:
	-rm binaries/*.hi binaries/*.o

realclean: clean
	-rm Main runchase
	-rm -r bin

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









