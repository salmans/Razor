default: ui
# -optl-w is there to suppress the warnings about text relocs
ui: .PHONY
	ghc -rtsopts -odir binaries -hidir binaries --make UI.hs -optl-w -o bin/UI

chase:  .PHONY
	ghc -rtsopts -odir binaries -hidir binaries --make atlas.hs -optl-w -o bin/chase

main:   Main.hs
	ghc -odir binaries -hidir binaries --make Main.hs -optl-w

.PHONY: directories

directories:
	-mkdir bin

clean:
	-rm -f binaries/*.hi binaries/*.o

realclean: clean
	-rm -f Main runchase
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
