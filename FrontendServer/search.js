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
  $("#triples").append("<div class=\"triple\" id=\"triple" + nextIndexTriple +"\" >" +
        "<div class=\"subject\" ondrop=\"drop(event)\" ondragover=\"allowDrop(event)\"> </div>" +
        "<div class=\"property\" ondrop=\"drop(event)\" ondragover=\"allowDrop(event)\"> </div>" +
        "<div class=\"object\" ondrop=\"drop(event)\" ondragover=\"allowDrop(event)\"> </div>" +
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
    ev.dataTransfer.setData("text", ev.target.getAttribute("readableName"));
    ev.dataTransfer.setData("wdName", ev.target.getAttribute("wdName"));

}

// ___________________________________________________________________________-
function drop(ev) {
    ev.preventDefault();
    var data = ev.dataTransfer.getData("text");
    var wdName = ev.dataTransfer.getData("wdName");
    ev.target.setAttribute("wdName", wdName);
    ev.target.innerHTML="";
    ev.target.innerHTML = data;
}
