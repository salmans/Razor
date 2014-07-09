package app.controller;

import javafx.scene.web.WebEngine;
import app.GUI;
import data.xml.Models;
import data.xml.XmlProcessor;

public class ModelController
{
	WebEngine view;

	public ModelController(WebEngine engine)
	{
		this.view = engine;
	}
	
	public void generate() throws Exception
	{
		// Get the models generated from the theory as raw xml
		String rawxml = GUI.thePipe.getModels(GUI.theModel.getTheory());
		// Process the xml
		XmlProcessor<Models> processor = new XmlProcessor<Models>(Models.class);
		Models xmlModels = processor.unwrap(rawxml);
		// Update the model
		GUI.theModel.addModels(xmlModels.getModels());
		// update the view
		String js = "startModels("+GUI.theModel.getNumModels()+");";
		view.executeScript(js);
	}
}
