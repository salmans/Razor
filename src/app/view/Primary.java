package app.view;

import java.net.URL;



import app.controller.GlobalController;
import app.controller.ModelController;
import app.controller.TheoryController;
import netscape.javascript.JSObject;
import javafx.beans.value.ObservableValue;
import javafx.concurrent.Worker.State;
import javafx.scene.layout.Region;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;

/**
 * Primary view; could possibly become just the webbrowser class
 * @author Ryan Danas
 *
 */
public class Primary extends Region
{ 
	static final URL url = Primary.class.getResource(Primary.class.getSimpleName()+".html");
	final WebView browser;
	final WebEngine engine; 

	public Primary()
	{
		// TODO make the brower resize with the main app window
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
							Object c0 = new ModelController();
							Object c1 = new TheoryController(engine);
							Object c2 = new GlobalController(engine);
							win.setMember(c0.getClass().getSimpleName(), c0);
							win.setMember(c1.getClass().getSimpleName(), c1);
							win.setMember(c2.getClass().getSimpleName(), c2);
						}
					});
		}
	}
}
