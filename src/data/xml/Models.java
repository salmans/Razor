package data.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.*;

/**
 * Current XML representation of a model
 * TODO This doesn't really match how a model should be organized / represented in the ui
 * Build an app.model and hope the next iteration of the haskell program can match / agree upon a better structure
 * @author Ryan Danas
 *
 */
@XmlRootElement(name = "MODELS")
@XmlAccessorType(XmlAccessType.FIELD)
public class Models
{
	@XmlElement(name = "MODEL")
	List<Model> models;
	
	Models()
	{
		this.models = new ArrayList<Model>();
	}
	
	@Override
	public String toString()
	{
		String out = "";
		out += "================================================================================\n";
		out += "Total of " + models.size() + " Models";
		for(Model m : this.models)
		{
			out += "\n" + "--------------------------------------------------------------------------------";
			out += "\n" + m.toString(0);
		}
		out += "\n================================================================================";
		return out;
	}
}