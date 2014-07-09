package data.xml;

import javax.xml.bind.annotation.*;

import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
class Element
{
	@XmlAttribute(name = "NAME")
	String name;
	
	@Override
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		return Pretty.tabString(tabDepth, this.name);
	}
	
	String toHtml()
	{
		return "<div class=\"element\" id=\""+this.name+"\"onclick=\"elementHistory('"+this.name+"');\">"+this.name+"</div>";
	}
}
