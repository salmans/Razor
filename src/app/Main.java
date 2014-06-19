package app;

import java.util.Scanner;

import pipe.jni.NativePipe;

public class Main
{
	public static void main(String[] args)
	{
		NativePipe pipe = new NativePipe();
		pipe.getModels("C:\\cygwin64\\home\\Ryan Danas\\RazorExamples\\Razor\\grandpa.gl");
	}
	
	public static String getFile()
	{
		Scanner keyboard = new Scanner(System.in);
		System.out.print("Enter the path of the theory file: ");
		String file = keyboard.next();
		keyboard.close();
		return file;
	}
}
