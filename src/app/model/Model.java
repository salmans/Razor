package app.model;

import java.util.ArrayList;
import java.util.List;

import data.xml.ModelXml;

public class Model
{
	String theory;
	List<ModelXml> models;

	public Model()
	{
		this.models = new ArrayList<ModelXml>();
	}
	
	public void setTheory(String theory)
	{
		this.theory = theory;
	}
	
	public String getTheory()
	{
		return theory;
	}
	
	public void addModels(List<ModelXml> newModels)
	{
		this.models.addAll(newModels);
	}
	
	public int getNumModels()
	{
		return this.models.size();
	}
}
