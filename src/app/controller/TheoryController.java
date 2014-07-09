package app.controller;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import util.User;
import app.GUI;
import javafx.scene.web.WebEngine;
import javafx.stage.FileChooser.ExtensionFilter;

// TODO Controller exception handling; where should the logic exist? java or JS?
// Is JS just for presentation or is it starting to take control of application logic; where is the line drawn?
/**
 * Controller for anything related to theory file / text manipulation
 * @author Ryan Danas
 *
 */
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
		String theoryFilePath = User.getFile("Load Theory File", acceptable, null);
		// read the theory file
		String theory = new String(Files.readAllBytes(Paths.get(theoryFilePath)));
		// update the model
		GUI.theModel.setTheory(theory);
		// update the view
		view.executeScript("setTheoryText();");
	}
}
