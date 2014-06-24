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
}
