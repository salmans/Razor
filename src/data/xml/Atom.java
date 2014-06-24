package data.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.*;

import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
class Atom
{
	@XmlAttribute(name = "TYPE")
	String type;
	@XmlAttribute(name = "NAME")
	String name;
	@XmlElement(name = "TERM")
	List<Term> terms;
	
	Atom()
	{
		this.terms = new ArrayList<Term>();
	}
	
	@Override
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		String out = Pretty.tabString(tabDepth, this.type + "(" + this.name + ")");
		for(Term t : this.terms)
		{
			out += "\n" + t.toString(tabDepth+1);
		}
		return out;
	}
}
