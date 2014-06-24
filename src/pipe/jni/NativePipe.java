package pipe.jni;

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
	/**
	 * Initializes the native pipe by calling hs_init
	 */
	public NativePipe()
	{
		// int* argc
		IntByReference pArgc = new IntByReference();
		pArgc.setValue(1);
		// char* argv[]
		String[] noargs = {""};
		StringArray argv = new StringArray(noargs);
		// char*** pArgv
		PointerByReference pArgv = new PointerByReference();
		pArgv.setPointer(argv);
		HaskellFFI.INSTANCE.hs_init(pArgc, pArgv);
	}
	
	/**
	 * Prints out the models found for the given file
	 * @param file	Location of the theory file
	 * @return The XML representation of the models generated
	 */
	public String getModels(String file)
	{
		// get pointer to cstring from haskell ffi call
		Pointer cstring = HaskellFFI.INSTANCE.hs_getmodels(file);
		// get a java string from the memory allocated in haskell
		String xmlModels = cstring.getString(0);
		// free the haskell allocation now that we've copied the data
		HaskellFFI.INSTANCE.hs_free_cstring(cstring);
		// return the models
		return xmlModels;
	}
	
	/**
	 * Closes the native pipe
	 */
	public void close()
	{
		HaskellFFI.INSTANCE.hs_exit();
	}
}