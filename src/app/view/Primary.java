package app.view;

import java.net.URL;

import app.controller.GlobalController;
import app.controller.ModelController;
import app.controller.TheoryController;
import netscape.javascript.JSObject;
import javafx.beans.value.ChangeListener;
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
	static final URL url = Primary.class.getResource(Primary.class.getSimpleName().toLowerCase()+".html");
	final WebView browser;
	final WebEngine engine; 

	public Primary()
	{
		System.out.println();
		browser = new WebView();
		this.getChildren().add(browser);
		engine = browser.getEngine();
		// load the page
		if(url != null)
		{
			engine.load(url.toExternalForm());
			// load up any java interactive elements for this page
			ChangeListener<? super State> stateListener = 
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
			};
			engine.getLoadWorker().stateProperty().addListener(stateListener);
		}
	}

	public void setWidth(double w)
	{
		this.setPrefWidth(w);
		this.browser.setPrefWidth(w);
	}
	public void setHeight(double h)
	{
		this.setPrefHeight(h);
		this.browser.setPrefHeight(h);
	}
}
