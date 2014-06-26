package app;

import java.io.IOException;

import app.view.Primary;
import pipe.jni.NativePipe;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class GUI extends Application 
{
	// TODO change this static pipe setup
	public static NativePipe thePipe;
	Stage mainStage;
	Scene mainScene;
	
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
	public void start(Stage stage) throws IOException 
	{
		this.mainStage = stage;
		this.mainScene = new Scene(new Primary());
		this.mainStage.setScene(this.mainScene);
		this.mainStage.show();
	}
}
