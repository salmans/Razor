package pipe.jni;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.StringArray;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.PointerByReference;

/**
 * A JNI <-> FFI native pipe for java <-> haskell communication
 * @author Ryan Danas
 *
 */
public class NativePipe 
{
	static HaskellFFI INSTANCE;
	/**
	 * Initializes the native pipe by calling hs_init
	 */
	public NativePipe(String libpath)
	{
		INSTANCE = (HaskellFFI) Native.synchronizedLibrary((Library)Native.loadLibrary(libpath, HaskellFFI.class));
		// int* argc
		IntByReference pArgc = new IntByReference();
		pArgc.setValue(1);
		// char* argv[]
		String[] noargs = {""};
		StringArray argv = new StringArray(noargs);
		// char*** pArgv
		PointerByReference pArgv = new PointerByReference();
		pArgv.setPointer(argv);
		INSTANCE.hs_init(pArgc, pArgv);
	}
	
	/**
	 * Prints out the models found for the given theory
	 * @param theory	Geometric theory as a string
	 * @return The XML representation of the models generated
	 */
	public String getModels(String theory)
	{
		// get pointer to cstring from haskell ffi call
		Pointer cstring = INSTANCE.hs_getmodels(theory);
		// get a java string from the memory allocated in haskell
		String xmlModels = cstring.getString(0);
		// free the haskell allocation now that we've copied the data
		INSTANCE.hs_free_cstring(cstring);
		// return the models
		return xmlModels;
	}
	
	/**
	 * Closes the native pipe
	 */
	public void close()
	{
		INSTANCE.hs_exit();
	}
}