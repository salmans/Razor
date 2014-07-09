package app.controller;

import app.GUI;

/**
 * Controller in charge of the view's model access
 * @author Ryan Danas
 *
 */
public class ModelController
{
	public String getTheoryText()
	{
		return GUI.theModel.getTheory();
	}
	
	public int getNumModels()
	{
		return GUI.theModel.getNumModels();
	}
	
	public int getCurrentNum()
	{
		return GUI.theModel.getCurrentNum();
	}
	
	public String getCurrentModel()
	{
		if(GUI.theModel.isModelAvailable())
		{
			return GUI.theModel.getCurrentModel();
		}
		else
		{
			return "No model available";
		}
	}
}
