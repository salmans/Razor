package pipe.jni;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * Represents the shared haskell library (ffi.hs) in the bin folder
 * @author Ryan Danas
 * 
 */
interface HaskellFFI extends Library
{
	HaskellFFI INSTANCE = (HaskellFFI) Native.loadLibrary("ffi", HaskellFFI.class);
	/**
	 * Initializes the haskell FFI environment
	 * Must be called before any other method
	 * @param argc Must be of ctype int*
	 * @param argv Must be of ctype char***
	 */
	void hs_init(IntByReference argc, PointerByReference argv);
	/**
	 * Prints out the models found for the given file
	 * @param file	Location of the theory file
	 */
	void hs_getmodels(String file);
	/**
	 * Exits the haskell FFI environment
	 */
	void hs_exit();
}