package data.xml;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.*;

import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
public class Model
{
	@XmlElement(name = "TABLE")
	List<Table> tables;
	
	public Model()
	{
		this.tables = new ArrayList<Table>();
	}
	
	@Override
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		// actual model data
		String out = Pretty.tabString(tabDepth, "MODEL DATA TABLES");
		for(Table t : this.tables)
		{
			out += "\n" + t.toString(tabDepth+1);
		}
		// provenance information
		
		// element history
		
		// done
		return out;
	}
}