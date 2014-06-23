package data.xml;

import javax.xml.bind.annotation.XmlElement;

import util.Pretty;

public class ElementHistory
{
	@XmlElement(name = "ELT")
	Element elt;
	@XmlElement(name = "TERM")
	Term skolemTerm;
	
	@Override
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		String out = Pretty.tabString(tabDepth, "HISTORY OF " + this.elt.toString());
		out += "\n" + this.skolemTerm.toString(tabDepth+1);
		return out;
	}
}
