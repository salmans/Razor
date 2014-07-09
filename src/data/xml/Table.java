package data.xml;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.*;
import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
class Table
{
	@XmlAttribute(name = "TYPE")
	String type;
	@XmlAttribute(name = "NAME")
	String name;
	@XmlElement(name = "RECORD")
	List<Record> records;

	Table()
	{
		this.records = new ArrayList<Record>();
	}

	@Override
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		String out = Pretty.tabString(tabDepth, this.type + "(" + this.name + ")");
		for(Record r : records)
		{
			out += "\n" +  r.toString(tabDepth+1);
		}
		return out;
	}

	String toHtml()
	{
		String out = "";
		if(this.type.equals("Relation"))
		{
			if(!this.name.startsWith("@"))
			{
				out += "<div><p>" + this.name + "= ";
				for(Record r : records)
				{
					out += "(";
					for(Element e : r.elements)
					{
						out += e.toHtml() + ",";
					}
					out += ") , ";
				}
				out += "</p></div>";
			}
		}
		else if(this.type.equals("Function"))
		{
			out += "<div><p>" + this.name + "= ";
			for(Record r : records)
			{
				for(Element e : r.elements)
				{
					out += e.toHtml() + "->";
				}
				out += " , ";
			}
			out += "</p></div>";
		}
		else if(this.type.equals("Domain"))
		{
			out += "<div><p>" + this.type + ": ";
			for(Record r : records)
			{
				for(Element e : r.elements)
				{
					out += e.toHtml() + ",";
				}
			}
			out += "</p></div>";
		}
		else
		{
			System.err.println("ModelXml.toHtml: Unsupported table type=" + this.type);
		}
		return out;
	}
}
