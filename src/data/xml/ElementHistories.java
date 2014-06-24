package data.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.*;

import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
class ElementHistories
{
	@XmlElement(name = "HISTORY")
	List<ElementHistory> histories;
	
	ElementHistories()
	{
		this.histories = new ArrayList<ElementHistory>();
	}
	
	@Override
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		String out = Pretty.tabString(tabDepth, "ELEMENT HISTORIES");
		for(ElementHistory eh : this.histories)
		{
			out += "\n" + eh.toString(tabDepth+1);
		}
		return out;
	}
}
