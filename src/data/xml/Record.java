package data.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.*;

import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
public class Record
{
	@XmlElement(name = "ELT")
	List<Element> elements;
	
	public Record()
	{
		this.elements = new ArrayList<Element>();
	}
	
	@Override
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		return Pretty.tabString(tabDepth, elements.toString());
	}
}
