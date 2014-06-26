package app.view;

import java.net.URL;

import app.controller.TheoryController;
import netscape.javascript.JSObject;
import javafx.beans.value.ObservableValue;
import javafx.concurrent.Worker.State;
import javafx.scene.layout.Region;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;

public class Primary extends Region
{
	// TODO come up with a standard format for urls so this process can be made generic? 
	static final URL url = Primary.class.getResource(Primary.class.getSimpleName()+".html");
	final WebView browser;
	final WebEngine engine; 

	public Primary()
	{
		browser = new WebView();
		this.getChildren().add(browser);
		engine = browser.getEngine();
		if(url != null)
		{
			// load the page
			engine.load(url.toExternalForm());
			// load up any java interactive elements for this page
			engine.getLoadWorker().stateProperty().addListener(
					(ObservableValue<? extends State> ov, State oldState, State newState) -> 
					{
						// When the page loads fully
						if (newState == State.SUCCEEDED) 
						{
							// Get the window
							JSObject win = (JSObject) engine.executeScript("window");
							// Add the controllers
							Object c1 = new TheoryController(this);
							win.setMember(c1.getClass().getSimpleName(), c1);
						}
					});
		}
	}
}
