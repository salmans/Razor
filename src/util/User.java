package util;

import java.io.File;
import java.io.IOException;
import java.util.Collection;

import javafx.stage.FileChooser;
import javafx.stage.FileChooser.ExtensionFilter;
import javafx.stage.Window;

public class User
{
	/**
	 * Gets a file path as a string through a file browsing popup
	 * @param prompt Prompt to show the user
	 * @param allowedExtensions A list of extensions that the user is allowed to choose from for a file
	 * @param lockparent Parent window to pause until file is selected
	 * @return The path for the file chosen
	 * @throws IOException If the file path could not be retrieved
	 */
	public static String getFile(String prompt, Collection<ExtensionFilter> allowedExtensions, Window lockparent) throws IOException
	{
		String absoluteFilePath = "";
		FileChooser fc = new FileChooser();
		fc.setTitle(prompt);
		fc.getExtensionFilters().addAll(allowedExtensions);
		// what window should be passed in and why
		File selected = fc.showOpenDialog(lockparent);
		if(selected == null)
		{
			throw new IOException("Chosen file is null");
		}
		else
		{
			absoluteFilePath = selected.getAbsolutePath();
		}
		return absoluteFilePath;
	}
}
