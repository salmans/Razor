default: directories ffi java notes
	
ffi: 
	@ printf \
	"\n***THIS MAKEFILE REQUIRES THE PATH OF YOUR GHC SHARED LIBRARIES***\n\
	THESE ARE A PART OF GHC... HERE ARE SOME PATH EXAMPLES...\n\
	WINDOWS: /cygdrive/c/Program Files (x86)/Haskell Platform/2013.2.0.0/lib\n\
	LINUX: /usr/lib/ghc/\n\
	MAC: ???ask dd/salman???\n\
	\n\
	Please enter your path: ";\
	read GHC_LIB_PATH;\
	printf \
	"\nPlease enter the common shared library extension for this OS (dll, so, ...): ";\
	read SHARED_LIB_EXT;\
	ghc --make -odir binaries -hidir binaries -no-hs-main -shared -static -fno-shared-implib -optl-Wl,-rpath,$GHC_LIB_PATH ffi.hs -o lib/haskell.$$SHARED_LIB_EXT

java:
	javac src/app/GUI.java -sourcepath src/ -cp lib/jna-4.1.0.jar -d bin/
	cd bin/; \
	jar xf ../lib/jna-4.1.0.jar; \
	jar cfve GUI.jar app.GUI *

notes:
	@ printf "\n***MAKE SURE THE JAR AND THE SHARED LIBRARY ARE IN THE SAME DIRECTORY***\n"

directories:
	-mkdir bin

clean:
	-rm -f binaries/*.hi binaries/*.o *.h

realclean: clean
	-rm -f lib/*.dll lib/*.so bin/*.jar

rc: realclean
