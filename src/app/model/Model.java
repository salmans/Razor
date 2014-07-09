package app.model;

import java.util.ArrayList;
import java.util.List;
import data.xml.ModelXml;

public class Model
{
	String theory;
	List<ModelXml> models;
	// human countable (1 to size)
	int currentNum = 1;

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

	public int getCurrentNum()
	{
		return this.currentNum;
	}

	public boolean isModelAvailable()
	{
		try
		{
			this.models.get(currentNum-1).toString();
			return true;
		}
		catch(Exception e)
		{
			return false;
		}
	}
	
	public String getCurrentModel()
	{
		return this.models.get(currentNum-1).toString();
	}

	public boolean prev()
	{
		if(this.currentNum > 1)
		{
			this.currentNum--;
			return true;
		}
		else
		{
			return false;
		}
	}

	public boolean next()
	{
		if(this.currentNum < this.models.size())
		{
			this.currentNum++;
			return true;
		}
		else
		{
			return false;
		}
	}
}
