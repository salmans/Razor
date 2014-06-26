package app.controller;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import app.GUI;
import app.view.Primary;
import data.xml.Models;
import data.xml.XmlProcessor;
import javafx.stage.FileChooser;
import javafx.stage.FileChooser.ExtensionFilter;

public class TheoryController
{
	Primary view;
	
	public TheoryController(Primary view)
	{
		this.view = view;
	}
	
	public String load() 
	{
		List<ExtensionFilter> acceptable = new ArrayList<ExtensionFilter>();
		acceptable.add(new ExtensionFilter("Geometric Theory Files", "*.gl"));
		String theoryFile = getFile("Load Theory File", acceptable);
		String modelText;
		// Try processing the XML into the corresponding java classes
		try
		{
			String rawxml = GUI.thePipe.getModels(theoryFile);
			XmlProcessor<Models> processor = new XmlProcessor<Models>(Models.class);
			Models xmlModels = processor.unwrap(rawxml);
			//modelText.setText(xmlModels.toString());
			modelText = xmlModels.toString();
		}
		catch(Exception e)
		{
			modelText = "Unable to load theory file and generate models!";
		}
		return modelText;
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
