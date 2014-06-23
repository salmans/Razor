package util;

public class Pretty
{
	public static String tabString(int numtabs, String str)
	{
		String out = "";
		for(int i=0; i < numtabs; i++) out += "\t";
		out += str;
		return out;
	}
}
