package app.controller;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import app.GUI;
import data.xml.Models;
import data.xml.XmlProcessor;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.text.Text;
import javafx.stage.FileChooser;
import javafx.stage.FileChooser.ExtensionFilter;

public class ExampleController
{
	@FXML
	Text modelText;
	
	@FXML 
	void loadTheory(ActionEvent event) 
	{
		List<ExtensionFilter> acceptable = new ArrayList<ExtensionFilter>();
		acceptable.add(new ExtensionFilter("Geometric Theory Files", "*.gl"));
		String theoryFile = getFile("Load Theory File", acceptable);
		// Try processing the XML into the corresponding java classes
		try
		{
			// TODO change this static pipe setup
			String rawxml = GUI.thePipe.getModels(theoryFile);
			XmlProcessor<Models> processor = new XmlProcessor<Models>(Models.class);
			Models xmlModels = processor.unwrap(rawxml);
			modelText.setText(xmlModels.toString());
		}
		catch(Exception e)
		{
			modelText.setText("Unable to load theory file and generate models!");
		}
	}
	
	// TODO move out of here
	// TODO throw an exception on failure
	static String getFile(String prompt, Collection<ExtensionFilter> allowedExtensions)
	{
		String absoluteFilePath = "";
		FileChooser fc = new FileChooser();
		fc.setTitle(prompt);
		fc.getExtensionFilters().addAll(allowedExtensions);
		// what window should be passed in and why
		File selected = fc.showOpenDialog(null);
		if(selected != null)
		{
			absoluteFilePath = selected.getAbsolutePath();
		}
		return absoluteFilePath;
	}
}
