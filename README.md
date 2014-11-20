Razor
=====
Razor is a model-finder for first-order theories presented geometric form; geometric logic is a variant of first-order logic that focuses on *observable* properties. An important guiding principle of Razor is that it be accessible to users who are not necessarily expert in formal methods; application areas include software design, analysis of security protocols and policies, and configuration management. 
A core functionality of the tool is that it supports exploration of the space of models of a given input theory, as well as presentation of provenance information about the elements and facts of a model. The crucial mathematical tool is the ordering relation on models determined by homomorphism, and Razor generates models that are minimal with respect to this homomorphism ordering.

# Installation
## Haskell and Cabal
In order to build Razor from, you need to install the latest version of the Haskell platform (https://www.haskell.org) and Cabal (https://www.haskell.org/cabal/) on your machine.
## Z3
Razor utilizes an SMT solver for constructing models. In order to run the current version of Razor, you need to install Z3 (http://z3.codeplex.com) on your machine and make sure that Z3 is in the system path.
## Razor
1. Download Razor’s source code available at https://github.com/salmans/atlas/tree/razor to a local source directory.
2. Open a Terminal window and change directory to the local `/src` directory:
<br> `cd /src`
3. Install Razor using Cabal:
<br>`cabal install`
<br> The previous command installs Razor in Cabal’s default install directory. Alternatively, you may install Razor in a custom directory `DIR` by running the next command:
<br> `cabal install —bindir=DIR`
