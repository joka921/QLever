// COPYRIGHT 2016 University of Freiburg
// Author Johannes Kalmbach <johannes.kalmbach@gmail.com>


// Global variables

// the number of triples currently displayed in the gui
var numTriples = 1;

// the unique index the next triple which will be added to gui will get.
// Can differ from numTriples after triples have been deleted
var nextIndexTriple = 1;

// stores information which variables are used in which triples
// e.g. "object1" in variableUsages["?x0"] means that the object field of triple
// 1 currently holds the variable value of ?x0. Necessary for correct renaming
// of variables which are already in use
var variableUsages = {};




// TODO: maybe register all the ondragstart etc functions here to clean up the
// html
$(document).ready(function() {
  $("#query").keyup(getEntitySearchResults);
  $("#searchmodeButtons").change(getEntitySearchResults);
})

/* get the wikidata entities corresponding to the value in textfield #query from
 * server and show them in the #searchRes field
 */
function getEntitySearchResults() {
    var query = $("#query").val();
    var host = window.location.host;
    var port = window.location.port;
    var searchtype = $('input[name="searchtype"]:checked').val();
    console.log(searchtype);
    var url = "http://" + host + "/?t=" + searchtype + "?q=" + query;
    console.log("URL: " + url);
    $.getJSON(url, function(data) {
      showEntitiesInResline(data, "searchRes");
    });
  }


/* add a triple to the gui which can be filled via drag and drop */
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

/* remove the Triple with index "idx" if this was the last triple, make sure
 * that there remains at least on triple in the gui*/
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

/* handle the start of a drag&drop operation*/
function drag(ev) {
    // custom drag drop image which has size of a triple element and shows only
    // the name of the dragged entity
    // for details see https://kryogenix.org/code/browser/custom-drag-image.html
    var crt = ev.target.cloneNode(true);
    crt.id = crt.id + "dummy"
    crt.style.backgroundColor = "#C2E0EF"

    var rect = $(".subject")[0];
    crt.style.width = rect.clientWidth;
    crt.style.height = rect.clientHeight;
    crt.style.position = "absolute"; crt.style.top = "0px"; crt.style.right = "0px";
    crt.style.zIndex = "-2";
    crt.innerText=ev.target.getAttribute("readableName");
    document.body.appendChild(crt);
    ev.dataTransfer.setDragImage(crt, 0, 0);

    // mark the origin of the drag
    markPossibleDragTarget(ev);

    // set all the data of the entity to be transmitted
    ev.dataTransfer.setData("text", ev.target.getAttribute("readableName"));
    ev.dataTransfer.setData("dummyId", crt.id);
    ev.dataTransfer.setData("wdName", ev.target.getAttribute("wdName"));
    ev.dataTransfer.setData("description", ev.target.getAttribute("description"));

}

/* handle the end of an entity or variable drag&drop operation*/
function drop(ev) {
    ev.preventDefault();
    // no need to keep the marking after finishing of op.
    unmarkPossibleDragTarget(ev);

    var data = ev.dataTransfer.getData("text");
    var wdName = ev.dataTransfer.getData("wdName");
    var desc = ev.dataTransfer.getData("description");
    var wdNameOld = ev.target.getAttribute("wdName");

    // if the target used to hold a variable, unregister this
    if (wdNameOld && wdNameOld.startsWith("?")) {
      removeVariableUsage(wdNameOld, ev.target.id)
    }
    ev.target.setAttribute("wdName", wdName);
    ev.target.setAttribute("description", desc);
    ev.target.innerHTML = data;

    // element which was only there for the drag image
    removeDummyElement(ev);
    // keep track of variableUsages in case of renamings
    if (wdName.slice(0,1) == "?") {
      addVariableUsage(wdName, ev.target.id)
    }


}

// handle unfinished drag&drop operations
function endDrag(ev) {
  unmarkPossibleDragTarget(ev);
  removeDummyElement(ev);
}

// remove the element which was used for the drag&drop image
function removeDummyElement(ev) {
  $("#" + ev.dataTransfer.getData("dummyId")).remove();
}

/* each triple field shows only the name of entities but also saves descriptions
 * etc. This function shows the details of a given triple element "el" in the
 * tag with id "detailRes"*/
function showDetails(el) {
  if (el.innerText=="") {
    return;
  }

  $("#detailRes").empty();
  showDetailedEntity("detailRes", el.getAttribute("wdName"), el.innerText,
                     el.getAttribute("description"), 0)
}

/* show the detailed description of a single entity
 * Arguments: target -> id of the tag in which to show
 *            wd, name, desc -> information about the entity
 *            i -> an index which has to be unique over all entities shown in
 *                 target
 */ 
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
