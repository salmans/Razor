package data.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
class Substitution
{
	@XmlAttribute(name = "VAR")
	String variable;
	@XmlElement(name = "TERM")
	Term term;

	@Override
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		String out = Pretty.tabString(tabDepth, "Substitute " + this.variable + " with...");
		out += "\n" + this.term.toString(tabDepth+1);
		return out;
	}
}
