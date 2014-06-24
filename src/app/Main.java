package app;

import java.util.Scanner;

import pipe.jni.NativePipe;
import data.xml.Models;
import data.xml.XmlProcessor;

public class Main
{
	public static void main(String[] args)
	{
		NativePipe pipe = new NativePipe();
		// TODO if the haskell library throws an error, there's no way to handle it in java
		String rawxml = pipe.getModels(getFile());
		System.out.println("Raw XML = " + rawxml);
		// Try processing the XML into the corresponding java classes
		try
		{
			XmlProcessor<Models> processor = new XmlProcessor<Models>(Models.class);
			Models m = processor.unwrap(rawxml);
			System.out.println(m.toString());
		}
		catch(Exception e)
		{
			System.err.println("Unable to process the xml returned from haskell");
			e.printStackTrace();
		}
		System.out.println("DONE");
	}
	
	// Example input = C:\\cygwin64\\home\\Ryan Danas\\RazorExamples\\Razor\\grandpa.gl
	public static String getFile()
	{
		Scanner keyboard = new Scanner(System.in);
		System.out.print("Enter the path of the theory file: ");
		String file = keyboard.nextLine();
		keyboard.close();
		return file;
	}
}
