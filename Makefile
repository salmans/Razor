default: directories ffi java
	
ffi: 
	@printf \
	"\t\n***THIS MAKEFILE REQUIRES THE PATH OF YOUR GHC SHARED LIBRARIES***\n\
	THESE ARE A PART OF GHC... HERE ARE SOME PATH EXAMPLES...\n\
	WINDOWS: /cygdrive/c/Program Files (x86)/Haskell Platform/2013.2.0.0/lib\n\
	LINUX: /usr/lib/ghc/\n\
	MAC: ???ask dd/salman???\n\
	\n\
	Please enter your path: ";\
	read GHC_LIB_PATH; \
	ghc --make -odir binaries -hidir binaries -no-hs-main -shared -static -fno-shared-implib -optl-Wl,-rpath,$GHC_LIB_PATH ffi.hs -o bin/ffi.dll

java:
	javac src/app/GUI.java -sourcepath src/ -cp lib/jna-4.1.0.jar
	cd bin/; \
	jar xf ../lib/jna-4.1.0.jar; \
	jar cfve GUI.jar app.GUI *

directories:
	-mkdir bin

clean:
	-rm -f binaries/*.hi binaries/*.o *.h

realclean: clean
	-rm -f bin/*.dll bin/*.so bin/*.jar

rc: realclean
