var retJson;


var myStyle = [];

var numTriples = 1;
var nextIndexTriple = 1;





$(document).ready(function() {
  $("#query").keyup(function() {
    var query = $("#query").val();

    var host = window.location.host;
    var port = window.location.port;
    var url = "http://" + host + "/?q=" + query;
    console.log("URL: " + url);
    $.getJSON(url, function(data) {retJson = data
      console.log(retJson);
      var items = "<br />";
      $("#searchRes").empty();
      for (var i = 0; i < retJson.length; i++) {
        var cssClass = retJson[i]["type"] == "P" ? "resLinePredicate" : "resLineSubject";
        $("#searchRes").append("<div class =\"" + cssClass +"\" id=res" + i + " >");
        $("#res" + i).append("<div class = \"wdName\" id=wdName" + i + " draggable=\"true\" ondragstart=\"drag(event)\" > ");
        $("#res" + i).append("<div class = \"wdDesc\" id=wdDesc" + i + " > ");
        
        $("#wdName" + i).text(retJson[i]["name"]);
        $("#wdDesc" + i).text(retJson[i]["desc"]);
      }
        $("#searchRes").append("<div class =\"resLine\" id=\"target\" ondrop=\"drop(event)\" ondragover=\"allowDrop(event)\" >");
        $("#target").text("target for drops")


      console.log(items);
    });
  })
})

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

function allowDrop(ev) {
    ev.preventDefault();
}

function drag(ev) {
    ev.dataTransfer.setData("text", ev.target.innerHTML);
}

function drop(ev) {
    ev.preventDefault();
    var data = ev.dataTransfer.getData("text");
    ev.target.innerHTML="";
    ev.target.innerHTML = data;
}

function viewSource(){
    var source = "<html>";
    source += document.getElementsByTagName('html')[0].innerHTML;
    source += "</html>";
    source = source.replace(/</g, "&lt;").replace(/>/g, "&gt;");
    source = "<pre>"+source+"</pre>";
    sourceWindow = window.open('','Source of page','height=800,width=800,scrollbars=1,resizable=1');
    sourceWindow.document.write(source);
    sourceWindow.document.close(); 
    if(window.focus) sourceWindow.focus();
}  
