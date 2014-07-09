package app;

import java.io.IOException;

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
	Primary mainView;
	Stage mainStage;
	Scene mainScene;

	public static void main(String[] args) 
	{
		System.out.println("Starting...");
		// init environment
		try
		{
			thePipe = new NativePipe();
			theModel = new Model();
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
