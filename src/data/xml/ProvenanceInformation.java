package data.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.*;

import util.Pretty;

@XmlAccessorType(XmlAccessType.FIELD)
public class ProvenanceInformation
{
	@XmlAttribute(name = "TOTAL")
	int total;
	@XmlElement(name = "PROVINFO")
	List<ProvInfo> provinfos;
	
	public ProvenanceInformation()
	{
		this.provinfos = new ArrayList<ProvInfo>();
	}
	
	@Override
	public String toString()
	{
		return this.toString(0);
	}
	String toString(int tabDepth)
	{
		String out = Pretty.tabString(tabDepth, "PROVENANCE INFORMATION (TOTAL = " + this.total + ")");
		for(ProvInfo pi : this.provinfos)
		{
			out += "\n" + pi.toString(tabDepth+1);
		}
		return out;
	}

}
