package app.controller;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.text.Text;

public class ExampleController
{
	@FXML private Text actiontarget;

	@FXML protected void handleSubmitButtonAction(ActionEvent event) 
	{
		actiontarget.setText("Sign in button pressed");
	}
}
