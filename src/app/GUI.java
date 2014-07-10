package app;

import java.io.File;

import app.model.Model;
import app.view.Primary;
import pipe.jni.NativePipe;
import javafx.application.Application;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class GUI extends Application 
{
	// XXX change this static setup?
	public static NativePipe thePipe;
	public static Model theModel;
	public Stage mainStage;
	Scene mainScene;
	Primary mainView;

	public static void main(String[] args) 
	{
		try
		{
			// init environment
			System.out.println("Initializing...");
			// give the native pipe the directory of this running code (bin dir) or jar (jar dir)
			File lib = new File(GUI.class.getProtectionDomain().getCodeSource().getLocation().toURI());
			if(lib.isFile())
			{
				lib = lib.getParentFile();
			}
			thePipe = new NativePipe(lib.getAbsolutePath());
			theModel = new Model();
			// launch app
			System.out.println("Starting...");
			try
			{
				launch(args);
			}
			catch(Exception e)
			{
				System.err.println("!!! APPLICATION FAILURE !!!");
				e.printStackTrace();
			}
		}
		catch(Exception e)
		{
			System.err.println("!!! FAILED TO START APPLICATION !!!");
			e.printStackTrace();
		}
		// shut down
		System.out.println("Exiting...");
		try
		{
			thePipe.close();
		}
		catch(Exception e)
		{
			System.err.println("!!! SHUTDOWN FAILURE !!!");
			e.printStackTrace();
		}
		System.exit(0);
	}

	@Override
	public void start(Stage stage)  
	{
		// init gui 
		this.mainStage = stage;
		this.mainStage.setTitle("Razor GUI Proof-of-Concept");
		this.mainView = new Primary();
		this.mainScene = new Scene(this.mainView);
		this.mainStage.setScene(this.mainScene);
		ChangeListener<? super Number> widthListener = 
				(ObservableValue<? extends Number> ov, Number oldW, Number newW) -> 
		{
			this.mainView.setWidth(newW.doubleValue());
			this.mainView.setHeight(this.mainStage.getHeight());
		};
		ChangeListener<? super Number> heightListener = 
				(ObservableValue<? extends Number> ov, Number oldH, Number newH) -> 
		{
			this.mainView.setHeight(newH.doubleValue());
			this.mainView.setWidth(this.mainStage.getWidth());
		};
		this.mainScene.widthProperty().addListener(widthListener);
		this.mainScene.heightProperty().addListener(heightListener);
		this.mainStage.setMinWidth(640);
		this.mainStage.setMinHeight(480);
		this.mainStage.show();
	}
}
