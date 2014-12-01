#!/bin/bash

SBV="SBV"
SMTLib2="SMTLib2"

while true; do
    read -e -p "Please select the SMT library (default = SBV): [SBV/SMTLib2] `echo $'\n> '`" smtlib
    
    if [ "$smtlib" = "smtlib" -o "$smtlib" = "SBV" ]
    then
	echo "Installing Razor using SBV"
	cabal clean
	cabal install
	break
    elif [ "$smtlib" = "SMTLib2" ]; then       
	echo "Installing Razor using SMTLib2..."

	while true; do
	    read -e -p "Delete the existing sandbox? [Y/N] `echo $'\n> '`" yn
	    case $yn in
		[Yy]* ) cabal sandbox delete; break;;
		[Nn]* ) break;;
		* ) echo "Please answer yes or no.";;
	    esac
	done
	
	read -e -p "Please specify the sandbox location (default = './razor-sandbox'):  `echo $'\n> '`" sandbox
	if [ "$sandbox" = "" ]; then
	    cabal sandbox init --sandbox=razor-sandbox
	else
	    cabal sandbox init --sandbox=$sandbox
	fi

	read -e -p "Please specify the path to the smtlib2 library (default = './smtlib2/'):  `echo $'\n> '`" smtlib2
	if [ "$smtlib2" = "" ]; then
	    cabal sandbox add-source smtlib2
	else
	    cabal sandbox add-source $smtlib2
	fi
	cabal clean
	cabal install --flags=smtlib2
	break
    else
	echo "Invalid SMT library."
    fi
done       
