function loadTheory()
{
	setModelText(TheoryController.load());
}

function setModelText(mdltxt) 
{
	var e = document.getElementById("modelText");
	e.innerHTML = mdltxt;
}