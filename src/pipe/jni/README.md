What we have out of the box from both languages
-----------------------------------------------
Haskell FFI >>> C/(kind of C++)
	exposes functions via a stub.h C-header file
Java JNI >>> C/C++/Assembly
	java can load a library and invoke native methods according to the specified header information. some libraries create interfaces instead of manual checking for I/O correctness.
	http://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/jniTOC.html

Possible ways to get a full Java >>> Haskell native pipe
--------------------------------------------------------
Manual way: JNI >>> C middleware for converting types >>> Haskell
	Actually not that bad. Just lots of things going on. Fine if it compiles without issue.
JNA (https://twall.github.io/jna/4.1.0/)
	Adds a bunch of interface and class types to avoid having to write any JNI native code. Seems like a promising way to avoid some of the major complications in the java code. This third-party library is definitely worth the extra weight. It also allows you to avoid writing any glue code in C. The glue can now be written using the provided java interfaces, classes and methods. 
JavaCPP: JNI >>> C++ annotations in java >>> Haskell
	Immature library with minimal support. Allows you to write C++ code with annotations. Getting it to compile is a nightmare. It's actually more complicated than the manual way. It would be easier just to write a middleware C file and incorporate that into the haskell shared library. 

Questions
---------
Can we avoid writting C code with some really fancy JNI work or possibly using JNA?
	> Not with straight JNI. The native code (the C) either needs to 
		1. take in a jobject and populate it 
		2. create a jobject
		This would be impossible to do in Haskell. JNI has no way to get a C struct as output and deal with it in Java. Unless what was returned was a jlong pointer to the struct. Again, Haskell can't do that.
	> JNA can communicate directly the Haskell FFI header file and call the Haskell shared library without issue... at least for primitive types.
Can we pass data structures through the pipe? (Java Classes <<< C Structs <<< Haskell Data)
	> JNA will automatically translate C Structs into Haskell classes if you implement their Structure interface
	> For the Haskell FFI to allocate a Haskell data structure and pass it to C (in this case the JNI), you need to define a Storable type that translates a Haskell Data Structure into an unmangaged byte array allocation of data. This is effectively mapping a Haskell data strucutre into a C struct in a block of allocated memory. 
So, instead of comming up with a request/response/string parsing architecture, we wrote a native/in-memory-conversion architecture. Is it really worth all the trouble just to avoid making a socket passing around some formatted strings? No not really. Converting to bytes of data is just as hard as formatting to strings, it has a lot more gotchas / segfaulting situations.
Can we avoid allocating things in Haskell so we don't have to deal with memory management?
	> I think the only possible way is to allocate space in the other language, and pass a reference to haskell for it to fill.
	> This is complicated and is impossible if the data is of variable length.
	> Short answer; no.

Other Notes
-----------
The C middleware that has to be written seems only important for type conversion / the only common language Haskell and Java know is C Types. 
MissingPy does not allow you to call Haskell from Python
The FFI only supports C at the moment. There doesn't seem to be a way of avoiding C as a middle ground. That's why things such as Python, Java, and C/C++ are the most promising integrating languages: they all support interoperability with C in some way. 

THE TL;DR
---------
JNI/FFI offers a tight, synchronous coupling between two seperate languages. That a blessing and a curse.
The more complex the data going through this pipe, the more complex the conversion is. Ints are easy. Strings are not too bad. Structs are a nightmare with memory management and byte array conversions. 