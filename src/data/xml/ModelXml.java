package data.xml;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.*;

import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
public class ModelXml
{
	@XmlElement(name = "TABLE")
	List<Table> tables;
	@XmlElement(name = "PROVENANCEINFORMATION")
	ProvenanceInformation provenanceInformation;
	@XmlElement(name = "ELEMENTHISTORIES")
	ElementHistories elementHistories;

	ModelXml()
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
		out += Pretty.tabString(tabDepth, "\n" + provenanceInformation.toString(tabDepth));
		// element history
		out += Pretty.tabString(tabDepth, "\n" + elementHistories.toString(tabDepth));
		// done
		return out;
	}

	public String toHtml()
	{
		String out = "";
		for(Table t : this.tables)
		{
			out += t.toHtml() + "\n";
		}
		return out;
	}

	public String getElementHistory(String elt)
	{
		for(ElementHistory eh : elementHistories.histories)
		{
			if(eh.elt.name.equals(elt))
			{
				return eh.toString();
			}
		}
		return "none";
	}
}