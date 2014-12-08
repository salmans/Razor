Razor 0.9.0 (beta)
==================
Razor is a model-finder for first-order theories presented in geometric form. Geometric logic is a variant of first-order logic that focuses on *observable* properties. A central guiding principle of Razor is that it can be accessible to users who lack expertise in formal methods. Application areas include software design, analysis of security protocols and policies, and configuration management. 
A core function of the tool is that it supports exploration of the space of models of a given input theory, as well as presents provenance information about the elements and facts of a model. The crucial mathematical tool is the ordering relation on models determined by homomorphism. Razor generates models that are minimal with respect to this homomorphism ordering.

# Installation
## Prerequisites
1. **Haskell and Cabal**. In order to build Razor, you need to install the latest version of the Haskell platform (https://www.haskell.org) and Cabal (https://www.haskell.org/cabal/) on your machine.
2. **Z3**. Razor utilizes an SMT solver for constructing models. In order to run the current version of Razor, you need to install Z3 (http://z3.codeplex.com) on your machine and make sure that `z3` is in the system path.

## Razor
1. Download Razor’s source code available at https://github.com/salmans/Razor to a local directory `RAZOR`.
2. Open a Terminal window and change directory to the local `RAZOR/src` directory:
<br> `cd RAZOR/src`
3. Install Razor using Cabal:
<br>`cabal install`