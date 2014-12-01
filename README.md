Razor
=====
Razor is a model-finder for first-order theories presented in geometric form. Geometric logic is a variant of first-order logic that focuses on *observable* properties. A central guiding principle of Razor is that it can be accessible to users who lack expertise in formal methods. Application areas include software design, analysis of security protocols and policies, and configuration management. 
A core function of the tool is that it supports exploration of the space of models of a given input theory, as well as presents provenance information about the elements and facts of a model. The crucial mathematical tool is the ordering relation on models determined by homomorphism. Razor generates models that are minimal with respect to this homomorphism ordering.

# Installation
## Prerequisites
1. **Haskell and Cabal**. In order to build Razor, you need to install the latest version of the Haskell platform (https://www.haskell.org) and Cabal (https://www.haskell.org/cabal/) on your machine.
2. **Z3**. Razor utilizes an SMT solver for constructing models. In order to run the current version of Razor, you need to install Z3 (http://z3.codeplex.com) on your machine and make sure that `z3` is in the system path.

## Razor
Razor utilizes an SMT solver *incrementally* for constructing models and exploring the space of all models through augmentation. The earlier implementation of Razor used the SBV library (http://hackage.haskell.org/package/sbv) to interact with the SMT solver. The latest implementation of Razor can work with the SMTLib2 library (https://github.com/hguenther/smtlib2), which supports incremental interaction with the SMT solver. You can install Razor in both incremental and non-incremental modes. 
Razor runs significantly faster in the incremental mode, thus, installing Razor in the incremental mode is recommended.

### Razor with Non-Incremental SMT solving
1. Download Razor’s source code available at https://github.com/salmans/atlas/tree/razor to a local directory `RAZOR`.
2. Open a Terminal window and change directory to the local `RAZOR/src` directory:
<br> `cd RAZOR/src`
3. Install Razor using Cabal:
<br>`cabal install`

### Razor with Incremental SMT-solving
Because SMTLib2 is not currently available on Hackage, the library must be manually downloaded and installed using the `install.sh` script. The shell script utilizes Cabal’s sandboxing feature for installing SMTLib2, which requires Cabal version 1.18 and earlier.

1. Download Razor’s source code available at https://github.com/salmans/atlas/tree/razor to a local directory `RAZOR`.
2. Download SMTLib2 from https://github.com/hguenther/smtlib2 in a local folder `SMTLIB2DIR`.
3. Open a Terminal window and change directory to the local `RAZOR/src` directory:
<br> `cd RAZOR/src`
4. Run the install script:
<br> `sudo ./install.sh`
- The install script asks you to select the underlying SMT library. Enter “SMTLib2” for the incremental mode.
- If you have previously installed Razor in a Cabal sandbox, the install script can delete the previous sandbox for a clean install.
- The install script asks you to locate a sandbox directory `SANDBOX-DIR`. The install script will put Razor in `SANDBOX-DIR/bin`. The default location is “razor-sandbox” in the current directory.
- The install script asks you to provide the path to the local folder `SMTLib2DIR`.
