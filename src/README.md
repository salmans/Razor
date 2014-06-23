Current Choices
---------------
Pipe = JNI/FFI just sending strings
	> Works well for strings; sending data structures shoved into byte arrays is just as much work as string serialization with less portability
	> Cannot get around the explicit free call after creating a new Cstring in Haskell
	> Dependency: Java Native Access; allows us to avoid writing any C glue code
	> Haskell Choice = Built in FFI
	> Java Choice = JNI with the JNA library to avoid writing any c code
Package = XML
	> JSON does not support by reference types or schemas natively
	> XML's other heavyweight features may be useful in the future
	> YAML may be a better choice, but I've never used it and there isn't a good library available for it. Doesn't seem as prolific.
	> Dependency: HXT (Haskell XML Toolbox); unlike java, haskell has no built in XML library; this one seems to be active and well vetted
	> Haskell Choice = HXT
	> Java Choice = JAXB (built in)
Environment Stuff
	> The makefile may need some tweaks to be portable to other OSs; there's a ghc article on shared library management; this is due to the current pipe choice and needing to make a shared library
	> Currently the haskell shared library is statically linked, except for ghc; ghc library installed and location must be updated when switching environments

Useful Links
------------
JNA - https://github.com/twall/jna
HXT - http://www.fh-wedel.de/~si/HXmlToolbox/
