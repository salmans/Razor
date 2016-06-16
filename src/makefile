.PHONY: default

default:
	git submodule update --init
	cabal sandbox init
	(cd ../smtlib2; cabal --sandbox-config-file=../src/cabal.sandbox.config install)
	cabal install --bindir=$(CURDIR)/../bin
