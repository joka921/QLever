  var resultForPassing=""
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
   var sparql = ""
  for (var i = 0; i < triples.length; i++) {
    var name = triples[i].getAttribute("id");
    if (!name.startsWith("triple")) {
      continue;
    }
    var id = parseInt(name.substring(6));
    var els = triples[i].children;
    var oneDefined = 0;
    var oneUndefined = 0;
    var usedVariables= {}
    for (var k = 0; k < els.length; k++) {
      if (els[k].getAttribute("class").startsWith("deleteTr")) {
        continue;
      }
      console.log(els[k]);
      var wdName = els[k].getAttribute("wdName");
      if (els[k].getAttribute("wdName")) {
        oneDefined = 1;
        sparql = sparql +" " + wdName + "";
        if (wdName.startsWith("?")) {
          usedVariables[wdName] = true;
        }
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
  sparqlHead = "SELECT";
  selectedVars = {}
  checkboxes = $('input[id^="selectedX"]');
  var selectedVarFound = false;
  for (var c in checkboxes) {
    c = checkboxes[c]
    if (c.checked == true &&usedVariables[c.value] == true) {
      sparqlHead += " "  + c.value;
      selectedVarFound = true;

    }
  }
  sparqlHead += " WHERE \{ ";
  sparql = sparqlHead + sparql;
  sparql += "\}";
  if (invalidTripleFound) {
    showErrorInResline("Incomplete Triples were found (see red markings", "queryRes");
  } else if (!selectedVarFound) {
    showErrorInResline("You have to select at least one variable which also occurs in triples", "queryRes");
  } else {
    // TODO: also filter empty queries!!!
    executeSparqlQuery(sparql);
  }
}

// ______________________________________________________
function executeSparqlQuery(query) {
    var host = window.location.host;
    // TODO: not- hardcoded port (what is the easiest way ??)
    var port = window.location.port - 1;
    var url = "http://" + host +  "?r=" + query;
    console.log("URL: " + url);
    $.getJSON(url, function(data) {retJson = data
      console.log(retJson);
      showResults(retJson, "queryRes");
    });
}

// ______________________________________________________
function showEntitiesInResline(json, basename) {
  $("#" + basename).empty();
  retJson = json["entities"];
  for (var i = 0; i < retJson.length; i++) {

    var cssClass = retJson[i]["type"] == "1" ? "resLinePredicate" : "resLineSubject";
    $("#" + basename).append("<div class =\"" + cssClass +"\" id=res" + basename + i + " >");
    $("#res"+ basename + i).append("<div class = \"wdName\" id=wdName" + basename + i +
                                   " draggable=\"true\" ondragstart=\"drag(event)\"" +

                                   "ondragend=\"endDrag(event)\""  +
                                   "wdName=\"" + retJson[i]["wdName"] + "\" readableName=\"" + retJson[i]["name"] + "\"" +
                                   "description=\"" + retJson[i]["description"] +"\">");
    var text = retJson[i]["wdName"] + "\n" + retJson[i]["name"] + "\n" + retJson[i]["description"];
      console.log(text);
    $("#wdName" + basename + i).text(text);
    //$("#wdDesc" + basename + i).text(retJson[i]["desc"]);
  }
}

// ______________________________________________________
function showResults(json, basename) {
  $("#" + basename).empty();
  retJson = json["entities"];
  var tableId = "restable";
  $("#" + basename).append("<table id=\"" + tableId + "\"></table>");
  var nRows = 0;
  retJson = json["entities"];
  for (var i = 0; i < retJson.length; i++) {
    var rId = tableId + i
    $("#" + tableId).append("<tr id=\"" + rId +"\"></td>");
    for (var j = 0; j < retJson[i].length; j++) {
      var eId = rId + "c" + j;
      var el = retJson[i][j];


      var cssClass = el["type"] == "1" ? "resLinePredicate" : "resLineSubject";
      $("#" + rId).append("<td id=\"" + eId + "\"></td>");
      var text = el["wdName"] + "\n" + el["name"] + "\n" + el["description"];
        console.log(text);
      $("#" + eId).text(text);
      //$("#wdDesc" + basename + i).text(retJson[i]["desc"]);
     }
  }
}

// _________________________________________________________
function showErrorInResline(error, basename) {
  $("#" + basename).empty();
  var cssClass = "resLinePredicate"
  $("#" + basename).append("<div class =\"" + cssClass +"\" id=\"error" + basename +"\" >");
  $("#error" + basename).text(error);
}

// _________________________________________________________
function markPossibleDragTarget(event) {
  $("#" + event.target.id).addClass("marked");
}


// ________________________________________________________
function unmarkPossibleDragTarget(event) {
  $("#" + event.target.id).removeClass("marked");
}
