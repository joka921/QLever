var retJson;


var myStyle = [];

var numTriples = 1;
var nextIndexTriple = 1;





$(document).ready(function() {
  $("#query").keyup(getEntitySearchResults);
  $("#searchmodeButtons").change(getEntitySearchResults);
})

// ______________________________________________________________________________________________________
function getEntitySearchResults() {
    var query = $("#query").val();

    var host = window.location.host;
    var port = window.location.port;
    var searchtype = $('input[name="searchtype"]:checked').val();
    console.log(searchtype);
    var url = "http://" + host + "/?t=" + searchtype + "?q=" + query;
    console.log("URL: " + url);
    $.getJSON(url, function(data) {retJson = data
      console.log(retJson);
      showEntitiesInResline(retJson, "searchRes");
    });
  }


// ___________________________________________________________________________
function addTriple() {
  var i = nextIndexTriple;
  var end = "\" ondrop=\"drop(event)\" ondragover=\"allowDrop(event)\"" +
            "ondragenter=\"markPossibleDragTarget(event)\"" +
            "onclick=\"showDetails(this)\"" +
            "ondragleave=\"unmarkPossibleDragTarget(event)\" > </div>"
  $("#triples").append("<div class=\"triple\" id=\"triple" + nextIndexTriple +"\" >" +
        "<div class=\"subject\" id=\"subject" + i + end +
        "<div class=\"property\" id=\"property" + i + end +
        "<div class=\"object\" id=\"object" + i + end +
        "<button class=\"deleteTripleButton\" onclick=\"removeTriple(" + nextIndexTriple + ")\"> - </button>" +
      "</div>"
      );
  numTriples = numTriples + 1;
  nextIndexTriple = nextIndexTriple + 1;
}

// ________________________________________________________________________
function removeTriple(idx) {
 $("#triple" + idx).remove();
 numTriples -= 1;
 if (numTriples <= 0) {
   addTriple();
 }
}

// __________________________________________________________________________
function allowDrop(ev) {
    ev.preventDefault();
}

// __________________________________________________________________________
function drag(ev) {
    var crt = ev.target.cloneNode(true);
        crt.id = crt.id + "dummy"
        crt.style.backgroundColor = "#C2E0EF"
        
	crt.style.position = "absolute"; crt.style.top = "0px"; crt.style.right = "0px";
        crt.style.zIndex = "-2";
        crt.innerText=ev.target.getAttribute("readableName");
	document.body.appendChild(crt);
	ev.dataTransfer.setDragImage(crt, 0, 0);
    markPossibleDragTarget(ev);
    ev.dataTransfer.setData("text", ev.target.getAttribute("readableName"));
    ev.dataTransfer.setData("dummyId", crt.id);
    ev.dataTransfer.setData("wdName", ev.target.getAttribute("wdName"));
    ev.dataTransfer.setData("description", ev.target.getAttribute("description"));

}

// ___________________________________________________________________________-
function drop(ev) {
    ev.preventDefault();
    unmarkPossibleDragTarget(ev);

    var data = ev.dataTransfer.getData("text");
    var wdName = ev.dataTransfer.getData("wdName");
    var desc = ev.dataTransfer.getData("description");
    ev.target.setAttribute("wdName", wdName);
    ev.target.setAttribute("description", desc);
    ev.target.innerHTML="";
    ev.target.innerHTML = data;
    removeDummyElement(ev)
}

// _________________________________________________________________________
function endDrag(ev) {
  unmarkPossibleDragTarget(ev);
  removeDummyElement(ev);
}

// ___________________________________________________________________
function removeDummyElement(ev) {
  $("#" + ev.dataTransfer.getData("dummyId")).remove();
}

// ____________________________________________________________
function showDetails(el) {
  if (el.innerText=="") {
    return;
  }

  $("#detailRes").empty();
  showDetailedEntity("detailRes", el.getAttribute("wdName"), el.innerText, 
                     el.getAttribute("description"), 0)
}

// _____________________________________________________________
function showDetailedEntity(target, wd, name, desc, i) {
    var cssClass = "resLinePredicate";
    var basename = target;
    $("#" + basename).append("<div class =\"" + cssClass +"\" id=res" + basename + i + " >");
    $("#res"+ basename + i).append("<div class = \"wdName\" id=wdName" + basename + i +
                                   " draggable=\"true\" ondragstart=\"drag(event)\"" +
                                   "ondragend=\"endDrag(event)\""  +
                                   "wdName=\"" + wd + "\" readableName=\"" + name + "\" >");

    var text = wd + "\n" + name + "\n" + desc;
    $("#wdName" + basename + i).text(text);
}
