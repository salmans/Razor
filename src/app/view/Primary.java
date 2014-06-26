package app.view;

import javafx.scene.layout.Region;
import javafx.scene.web.WebView;

public class Primary extends Region
{
	final WebView browser;
	
	public Primary()
	{
		browser = new WebView();
		browser.getEngine().load("http://google.com/index.html");
		this.getChildren().add(browser);
	}
}
