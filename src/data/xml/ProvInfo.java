package data.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.*;

import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
class ProvInfo
{
	@XmlElement(name = "OBS")
	Observation obs;
	@XmlElement(name = "PROV")
	List<Prov> provs;
	
	ProvInfo()
	{
		this.provs = new ArrayList<Prov>();
	}
	
	@Override
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		String out = Pretty.tabString(tabDepth, "PROVINFO");
		out += "\n" + obs.toString(tabDepth+1);
		for(Prov p : this.provs)
		{
			out += "\n" + p.toString(tabDepth+1);
		}
		return out;
	}
}
