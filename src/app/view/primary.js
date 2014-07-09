// ######################
// # view -> controller #
// ######################
function loadTheory()
{
	TheoryController.load();
}

function generateModels()
{
	GlobalController.generate();
}

function prevModel()
{
	GlobalController.prev();
}

function nextModel()
{
	GlobalController.next();
}

function showElemHist(e, elem)
{
	remElemHist();
	var x = (window.Event) ? e.pageX : event.clientX + (document.documentElement.scrollLeft ? document.documentElement.scrollLeft : document.body.scrollLeft);
	var y = (window.Event) ? e.pageY : event.clientY + (document.documentElement.scrollTop ? document.documentElement.scrollTop : document.body.scrollTop);
	var overlay = document.createElement("textarea");
	overlay.setAttribute("id", "overlay");
	overlay.setAttribute("onclick", "remElemHist();");
	overlay.innerHTML = ModelController.getHistory(elem);
	overlay.style.top = y;
	overlay.style.left = x;
	document.body.appendChild(overlay);
}

function remElemHist()
{
	var overlay = document.getElementById("overlay");
	if(overlay) overlay.parentNode.removeChild(overlay);
}

// ######################
// # controller -> view #
// ######################
function setTheoryText()
{
	var thytxt = ModelController.getTheoryText();
	// remove the option to load a theory
	document.getElementById("theoryLoader").style.display = "none";
	// set the theory text and display it
	var theoryText = document.getElementById("theoryText");
	theoryText.innerHTML = thytxt;
	theoryText.style.display = "block";
	// add the option to generate models
	document.getElementById("modelGenerator").style.display = "block";
}

function startModels() 
{
	// remove the option to generate models
	document.getElementById("modelGenerator").style.display = "none";
	// add the model controls
	document.getElementById("modelControls").style.display = "block";
	// make the model text visible
	document.getElementById("modelText").style.display = "block";
}

function updateModel(curmdl, nummdls, mdltxt)
{
	var curmdl = ModelController.getCurrentNum();
	var nummdls = ModelController.getNumModels();
	var mdltxt = ModelController.getCurrentModel();
	// update the model count
	document.getElementById("count").value = curmdl+" of "+nummdls;
	// update the model text
	document.getElementById("modelText").innerHTML = mdltxt;
}