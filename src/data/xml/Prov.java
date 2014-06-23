package data.xml;

import javax.xml.bind.annotation.*;

import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
public class Prov
{
	@XmlAttribute(name = "TYPE")
	String type;
	/** OPTIONAL FIELD */
	@XmlAttribute(name = "TAG")
	Integer tag;
	/** OPTIONAL FIELD */
	@XmlAttribute(name = "ID")
	Integer id;
	/** OPTIONAL FIELD */
	@XmlElement(name = "SUB")
	Substitution sub;

	@Override
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		String out = Pretty.tabString(tabDepth, this.type + " PROVENANCE " + "(TAG=" + this.tag + " ID=" + this.id + ")");
		if(this.sub != null) out += "\n" + this.sub.toString(tabDepth+1);
		return out;
	}
}
