package app.controller;

import app.GUI;

public class ElementHistoryController
{
	public void getHistory(String element)
	{
		// TODO show information in some sort of overlay toast window
		System.out.println(GUI.theModel.getHistory(element));
	}
}
