Razor 0.9.0 (beta)
==================
Razor is a model-finder for first-order theories. A central guiding principle of Razor is that it can be accessible to users who lack expertise in formal methods. Application areas include software design, analysis of security protocols and policies, and configuration management. 
A core function of the tool is that it supports exploration of the space of models of a given input theory, as well as presents provenance information about the elements and facts of a model. The crucial mathematical tool is the ordering relation on models determined by homomorphism. Razor generates models that are minimal with respect to this homomorphism ordering.

# Installation
## Prerequisites
1. **Haskell and Cabal.** In order to build Razor, you need to install the latest version of the Haskell platform (https://www.haskell.org) and Cabal (https://www.haskell.org/cabal/) on your machine.
2. **Z3.** Razor utilizes an SMT solver for constructing models. In order to run the current version of Razor, you need to install Z3 (http://z3.codeplex.com) on your machine and make sure that `z3` is in the system path.

## Razor
1. Download Razor’s source code available at https://github.com/salmans/Razor to a local directory `RAZOR`.
2. Open a Terminal window and change directory to the local `RAZOR/src` directory:
<br> `cd RAZOR/src`
3. Install Razor using Cabal:
<br>`cabal install`

# Using the REPL

## Help
There is overall help (use `help`), as well as help for each specific mode (use `?`).

## Modes
There are several modes which logically separate the functions of the REPL into different categories. You will have to execute the respective commands (included below) to enter each mode. *Note:* In future releases, we plan to make the transition between modes automatic, so any command can be executed at any time.
- `@theory` Starting mode; turn the various knobs of Razor, and load an input theory.
- `@explore` Available once a theory is loaded; explore the models of the given theory, and augment them with additional facts.
- `@explain` Available once a theory is loaded; query the model's various provenance information for elements and facts.

## Command Examples

### Theory Mode
- `debug` Toggles debug mode
- `pure` Toggles pure minimality.  By default, models produced in relaxed mode may have "accidental" equalities between definable elements; in pure mode, definable elements are distinct unless provably equal.
- `depth 5` Sets the Skolem depth to 5. By default the depth is -1, meaning unbounded search. 
- `load /path/to/razor/theory` Loads the given Razor theory
- `tptp /path/to/tptp/problem` Loads the given TPTP problem

### Exploration Mode
- `current` Displays the current model
- `next` Get the next model available
- `aug R(e^2, e^100)` Augment the model with the given fact; for fresh elements not in the model, use elements not in the domain, like e^100
- `aug e^2=e^3` Augment the model with the equality of two elements in the model.
- `undo` Undo the previous augmentation; next cannot be undone. 

### Explanation Mode
- `origin e^0` Display a single origin of element e^0. That is, the instance of the theory sentence that caused this element to exist in the model. 
- `origins e^0` Display the possibly multiple origins of element e^0. 
- `origin* e^7` Display the single origin of element e^7; also, recursively display the origins of the elements that caused e^7 to exist, and soforth. 
- `origins* e^7` Display all origins of this element and it's recursive dependents.
- `blame R(e^2, e^3)` Display the sentence blamed for making R(e^2, e^3) true. That is, the instance of the theory sentence that caused the given fact to be true in the model. 
