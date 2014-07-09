package data.xml;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.annotation.*;

/**
 * Current XML representation of a model
 * XXX This doesn't really match how a model should be organized / represented in the ui
 * Build an app.model and hope the next iteration of the haskell program can match / agree upon a better structure
 * @author Ryan Danas
 *
 */
@XmlRootElement(name = "MODELS")
@XmlAccessorType(XmlAccessType.FIELD)
public class Models
{
	@XmlElement(name = "MODEL")
	List<ModelXml> models;
	
	Models()
	{
		this.models = new ArrayList<ModelXml>();
	}
	
	public List<ModelXml> getModels()
	{
		ModelXml[] m = {};
		return Arrays.asList(models.toArray(m));
	}
	
	@Override
	public String toString()
	{
		String out = "";
		out += "================================================================================\n";
		out += "Total of " + models.size() + " Models";
		for(ModelXml m : this.models)
		{
			out += "\n" + "--------------------------------------------------------------------------------";
			out += "\n" + m.toString(0);
		}
		out += "\n================================================================================";
		return out;
	}
}