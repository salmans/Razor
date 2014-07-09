package pipe.jni;

import com.sun.jna.Library;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * Represents the shared haskell library (ffi.hs) in the bin folder
 * @author Ryan Danas
 * 
 */
interface HaskellFFI extends Library
{
	/**
	 * Initializes the haskell FFI environment
	 * Must be called before any other method
	 * @param argc Must be of ctype int*
	 * @param argv Must be of ctype char***
	 */
	void hs_init(IntByReference argc, PointerByReference argv);
	/**
	 * Prints out the models found for the given theory
	 * @param theory	Geometric theory
	 * @return The XML representation of the models generated
	 */
	Pointer hs_getmodels(String theory);
	/**
	 * Frees a cstring originally passed to java from haskell
	 * @param haskellOriginatedString Must be a reference to an original string passed back to java from haskell
	 */
	void hs_free_cstring(Pointer haskellOriginatedString);
	/**
	 * Exits the haskell FFI environment
	 */
	void hs_exit();
}