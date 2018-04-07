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
      /*$("#searchRes").empty();
      for (var i = 0; i < retJson.length; i++) {
        var cssClass = retJson[i]["type"] == "P" ? "resLinePredicate" : "resLineSubject";
        $("#searchRes").append("<div class =\"" + cssClass +"\" id=res" + i + " >");
        $("#res" + i).append("<div class = \"wdName\" id=wdName" + i + " draggable=\"true\" ondragstart=\"drag(event)\" wdName=\"" + retJson[i]["wdName"] + "\"> ");
        $("#res" + i).append("<div class = \"wdDesc\" id=wdDesc" + i + " > ");
        
        $("#wdName" + i).text(retJson[i]["name"]);
        $("#wdDesc" + i).text(retJson[i]["desc"]);
      }
      */

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

// ___________________________________________________________________________________
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

// ____________________________________________________________________________________
function removeEmptyTriples() {
   triples = document.getElementById("triples").children;
   var triplesToBeDeleted = [];
   var invalidTripleFound = 0;
   var sparql ="SELECT ?x0 ?x1 ?x2 ?x3 WHERE \{ ";
  for (var i = 0; i < triples.length; i++) {
    var name = triples[i].getAttribute("id");
    if (!name.startsWith("triple")) {
      continue;
    }
    var id = parseInt(name.substring(6));
    var els = triples[i].children;
    var oneDefined = 0;
    var oneUndefined = 0;
    for (var k = 0; k < els.length; k++) {
      if (els[k].getAttribute("class").startsWith("deleteTr")) {
        continue;
      }
      console.log(els[k]);
      var wdName = els[k].getAttribute("wdName");
      if (els[k].getAttribute("wdName")) {
        oneDefined = 1;
        sparql = sparql +" " + wdName + "";
      } else {
        oneUndefined = 1;
        els[k].style.backgroundColor="FF0000";
        console.log(els[k])
      }
    }
    if (oneDefined == 0) {
      triplesToBeDeleted.push(id);
    } else if (oneDefined == 1 && oneUndefined == 1) {
      // mark incomplete already done above, TODO: remember original color
      invalidTripleFound = 1;
    } else {
      sparql = sparql +" . ";
    }
  }

  for (var j = 0; j < triplesToBeDeleted.length; j++) {
    removeTriple(triplesToBeDeleted[j]);
  }
  sparql += "\}";
  if (!invalidTripleFound) {
    // TODO: also filter empty queries!!!
    executeSparqlQuery(sparql);
  }
}

// ______________________________________________________
function executeSparqlQuery(query) {
    var host = window.location.host;
    // TODO: not- hardcoded port (what is the easiest way ??)
    var port = window.location.port - 1;
    var url = "http://titan.informatik.privat:9998" + "/?query=" + query;
    console.log("URL: " + url);
    $.getJSON(url, function(data) {retJson = data
      console.log(retJson);
    if (retJson["status"] == "OK") {
      var flattend = [].concat.apply([], retJson["res"]);
      url = "http://" + host + "/?c=" + flattend.join(' ');
      $.getJSON(url, function(data) {retJson = data
        console.log(retJson);
        showEntitiesInResline(retJson, "queryRes");
      });

    }
    });
}

// ______________________________________________________
function showEntitiesInResline(retJson, basename) {
  $("#" + basename).empty();
  for (var i = 0; i < retJson.length; i++) {
    var cssClass = retJson[i]["type"] == "P" ? "resLinePredicate" : "resLineSubject";
    $("#" + basename).append("<div class =\"" + cssClass +"\" id=res" + basename + i + " >");
    //$("#res"+ basename + i).append("<div class = \"wdName\" id=wdId" + basename + i + " draggable=\"true\" ondragstart=\"drag(event)\" wdName=\"" + retJson[i]["wdName"] + "\ readableName=\"" + retJson[i]["name"] + "\" >");
    $("#res"+ basename + i).append("<div class = \"wdName\" id=wdName" + basename + i + " draggable=\"true\" ondragstart=\"drag(event)\" wdName=\"" + retJson[i]["wdName"] + "\" readableName=\"" + retJson[i]["name"] + "\" >");
    //$("#res" + basename + i).append("<div class = \"wdDesc\" id=wdDesc" + basename + i + " > ");
    var text = retJson[i]["wdName"] + "\n" + retJson[i]["name"] + "\n" + retJson[i]["desc"]
    $("#wdName" + basename + i).text(text);
    //$("#wdDesc" + basename + i).text(retJson[i]["desc"]);
  }
}


