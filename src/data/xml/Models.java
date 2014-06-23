package data.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.*;

@XmlRootElement(name = "MODELS")
@XmlAccessorType(XmlAccessType.FIELD)
public class Models
{
	@XmlElement(name = "MODEL")
	List<Model> models;
	
	public Models()
	{
		this.models = new ArrayList<Model>();
	}
	
	@Override
	public String toString()
	{
		String out = "";
		out += "================================================================================";
		out += "\n" + "Total of " + models.size() + " Models";
		for(Model m : this.models)
		{
			out += "\n" + "--------------------------------------------------------------------------------";
			out += "\n" + m.toString();
		}
		out += "\n================================================================================";
		return out;
	}
}