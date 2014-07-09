// ######################
// # view -> controller #
// ######################
function loadTheory()
{
	TheoryController.load();
}

function generateModels()
{
	ModelController.generate();
}

// ######################
// # controller -> view #
// ######################
function setTheoryText(thytxt)
{
	// remove the option to load a theory
	document.getElementById("theoryLoader").style.display = "none";
	// set the theory text and display it
	var theoryText = document.getElementById("theoryText");
	theoryText.innerHTML = thytxt;
	theoryText.style.display = "block";
	// add the option to generate models
	document.getElementById("modelGenerator").style.display = "block";
}

function startModels(nummdls) 
{
	// remove the option to generate models
	document.getElementById("modelGenerator").style.display = "none";
	// add the model controls
	document.getElementById("modelControls").style.display = "block";
	// update the model count
	document.getElementById("modelCount").innerHTML = "0/"+nummdls;
	// make the model text visible
	document.getElementById("modelText").style.display = "block";
}