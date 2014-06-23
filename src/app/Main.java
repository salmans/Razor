package app;

import java.util.Scanner;

import pipe.jni.NativePipe;

public class Main
{
	public static void main(String[] args)
	{
		NativePipe pipe = new NativePipe();
		// TODO if the haskell library throws an error, there's no way to handle it in java
		String rawxml = pipe.getModels(getFile());
		System.out.println("Models = " + rawxml);
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
