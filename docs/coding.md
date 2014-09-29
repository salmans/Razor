# Module System
## Units
A unit is a pair of Haskell modules, the interface and the **internal** implementation of the unit. The unit interface hides the internal implementation of the unit from the outside world: unit interfaces do not implement the functions they are exposing; their sole purpose is to make the *public* functions of the internal implementations available. Various functions of a unit may only be accessed by other units via importing the unit interface.

- __NOTE__: exceptionally, test modules are allowed access import the internal implementation of a unit in order to test the private functions of the unit.

## Hierarchies
A hierarchical module contains a set of interrelated modules that deliver a single feature or service.

- __NOTE__: Hierarchical modules may contain other submodules. The rules of encapsulation also applies to the submodules of a module.

# Naming Conventions and Documentation
## Hierarchical Modules
The directory containing the modules of a hierarchical module with a **CamelCased** name is the same as the corresponding module name.
## Units
* For a unit with a **CamelCased** name <UNIT_NAME>, the interface file is <UNIT_NAME>.hs and the internal implementation file is I<UNIT_NAME>.hs.
* Every unit provides a description of the service/feature offered by the unit at the beginning of its interface module. The description has to be compatible with Haddock.
## Functions
* Function names follow a **headlessCamelCase** pattern. Clearly, the name of a function has to be indicative of the functionality it provides.
* The helper function of a function <FUNC_NAME> is named by the convention <FUNC_NAME>Helper. Various Helpers of a function are identified as <FUNC_NAME>Helper, <FUNC_NAME>Helper', <FUNC_NAME>Helper'' and ... (up to four helpers).
* Functions must be decorated with type information in standard Haskell format.
* Functions provide a description, explaining the main functionality of the function, together with a description for any of the input parameters. If the function is exposed by the interface of its unit, the description must be Haddock-compatible.
* When relevant, a mathematical description for the function in LaTeX format may be provided.

## Constants
* Constants (in the definition of a function) follow a **headlessCamelCase** pattern. 
	- __NOTE__: However, the constant names may follow the conventional Haskell naming patterns when the names are understood in their context. Examples: "(x:xs)" for the head and the rest of a list, "l" for an instance of a List, "m" for an instance of a Map, etc.
* Various constants corresponding to different versions of the same piece of data <DATA> through a procedure may be identified as <DATA>', <DATA>'' and <DATA>''' (up to three instances).

## Types
Type names are **CamelCased**.

## Line Breaks
The maximum limit for the number of characters in a line is 80. 

# Unit Tests
Every unit <UNIT_NAME> is accompanied by a test module named T<UNIT_NAME>.hs that is located in hierarchical module (directory) "Test", located in the same directory as the unit.
The test module of a unit provides unit-test functions for every function of the unit.

## Test Functions
The name of a unit-test function for a function <FUNC_NAME> follows the pattern test_<FUNC_NAME>_<IDEN>[INDEX]. <IDEN> is a **CamelCased** identifier that serves as a short description for the property that the unit-test function is testing. Various variations of tests for the same identifier may follow an index [INDEX].


** Error Management
** Data Management for Tests