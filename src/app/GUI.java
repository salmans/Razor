package app;

import java.io.IOException;

import pipe.jni.NativePipe;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class GUI extends Application 
{
	public static NativePipe thePipe;
	
	public static void main(String[] args) 
	{
		System.out.println("Starting...");
		// init environment
		try
		{
			
			thePipe = new NativePipe();
		}
		catch(Exception e)
		{
			System.err.println("Failed to initalize the application!");
			e.printStackTrace();
		}
		// launch app
		try
		{
			launch(args);
		}
		catch(Exception e)
		{
			System.err.println("Application failure!");
			e.printStackTrace();
		}
		// shut down
		try
		{
			thePipe.close();
			System.out.println("Shutdown Successful");
		}
		catch(Exception e)
		{
			System.err.println("Failed to properly shutdown the application!");
			e.printStackTrace();
		}
	}

	@Override
	public void start(Stage primaryStage) throws IOException 
	{
		// TODO is FXML the best way to make these views?
		Parent root = FXMLLoader.load(getClass().getResource("view/primary.fxml"));
        Scene scene = new Scene(root);
        primaryStage.setTitle("");
        primaryStage.setScene(scene);
        primaryStage.setMinHeight(480);
        primaryStage.setMinWidth(640);
        primaryStage.show();
	}
}
