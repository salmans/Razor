package app.controller;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import app.GUI;
import javafx.scene.web.WebEngine;
import javafx.stage.FileChooser;
import javafx.stage.FileChooser.ExtensionFilter;

// TODO Controller exception handling; where should the logic exist? java or JS?
// Is JS just for presentation or is it starting to take control of application logic; where is the line drawn?
public class TheoryController
{
	WebEngine view;

	public TheoryController(WebEngine engine)
	{
		this.view = engine;
	}

	/**
	 * Load the theory file
	 * @throws IOException
	 */
	public void load() throws IOException 
	{
		// get the file path from the suer
		List<ExtensionFilter> acceptable = new ArrayList<ExtensionFilter>();
		acceptable.add(new ExtensionFilter("Geometric Theory Files", "*.gl"));
		String theoryFilePath = getFile("Load Theory File", acceptable);
		// read the theory file
		String theory = new String(Files.readAllBytes(Paths.get(theoryFilePath)));
		// update the model
		GUI.theModel.setTheory(theory);
		// update the view
		String js = "setTheoryText('"+GUI.theModel.getTheory()+"');";
		// FIXME the execute script does not like newlines? 
		// convert to js acceptable format (java webview did this automatically with return values; should be easy)
		view.executeScript("setTheoryText('theory loaded; not displayed for now');");
	}

	static String getFile(String prompt, Collection<ExtensionFilter> allowedExtensions) throws IOException
	{
		String absoluteFilePath = "";
		FileChooser fc = new FileChooser();
		fc.setTitle(prompt);
		fc.getExtensionFilters().addAll(allowedExtensions);
		// what window should be passed in and why
		File selected = fc.showOpenDialog(null);
		if(selected == null)
		{
			throw new IOException("Could not open file selected");
		}
		else
		{
			absoluteFilePath = selected.getAbsolutePath();
		}
		return absoluteFilePath;
	}
}
