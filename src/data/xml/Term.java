package data.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.*;

import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
class Term
{
	@XmlAttribute(name = "TYPE")
	String type;
	@XmlAttribute(name = "VALUE")
	String value;
	@XmlElement(name = "TERM")
	List<Term> terms;
	
	Term()
	{
		this.terms = new ArrayList<Term>();
	}
	
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		String out = Pretty.tabString(tabDepth, this.type + " " + this.value);
		for(Term t : this.terms)
		{
			out += "\n" + t.toString(tabDepth+1);
		}
		return out;
	}
}
