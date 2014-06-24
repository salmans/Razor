package data.xml;

import javax.xml.bind.annotation.*;

import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
class Observation
{
	@XmlAttribute(name = "TYPE")
	String type;
	/** OPTIONAL FIELD */
	@XmlElement(name = "ATOM")
	Atom truth;
	/** OPTIONAL FIELD */
	@XmlElement(name = "TERM")
	Term t1;
	/** OPTIONAL FIELD */
	@XmlElement(name = "TERM")
	Term t2;
	
	@Override
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		String out = Pretty.tabString(tabDepth, "OBSERVATION(" + this.type + ")");
		if(truth != null) out += "\n" + truth.toString(tabDepth+1);
		if(t1 != null) out += "\n" + t1.toString(tabDepth+1);
		if(t2 != null) out += "\n" + t2.toString(tabDepth+1);
		return out;
	}
}
