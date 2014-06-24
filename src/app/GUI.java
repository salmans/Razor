package app;

import java.io.IOException;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class GUI extends Application 
{
	static final int startH = 320;
	static final int startW = 240;
	
	public static void main(String[] args) 
	{
		launch(args);
	}

	@Override
	public void start(Stage primaryStage) throws IOException 
	{
		Parent root = FXMLLoader.load(getClass().getResource("view/primary.fxml"));
        Scene scene = new Scene(root, startH, startW);
        primaryStage.setTitle("FXML Welcome");
        primaryStage.setScene(scene);
        primaryStage.show();
	}
}
