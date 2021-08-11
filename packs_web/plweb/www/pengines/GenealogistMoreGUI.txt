# The Genealogist Part 4: Improving the GUI

In this part of the tutorial we will add GUI parts for the db updating functionality that we added in [part 3](http://www.swi-prolog.org/pengines/GenealogistUpdate.html). From now on we are also going to try to adhere to the so called [principle of separation of concerns](http://en.wikipedia.org/wiki/Separation_of_concerns) and split our HTML, CSS and JavaScript up into separate files. Here is how the apps directory looks like in the final version of our web application:
  ~~~~
apps/
   genealogist/
      genealogist.pl
      index.html
      js/
         genealogist.js
      css/
         genealogist.css
   swish\
   scratchpad\
  ~~~~
The file =|genealogist.pl|= has not changed since part 3, so we will not say more about it here. The CSS in =|genealogist.css|= is fairly uninteresting so we keep silent about that too. Let us begin by looking at the HTML:

  ~~~~
  <!DOCTYPE html>
<html lang="en">   
    <head>
        <title>Genealogist</title>
        <meta name="author" content="Torbjörn Lager">
        <link rel="stylesheet" href="css/genealogist.css">
    </head>
    <body>
        <h1>The Genealogist</h1>
        <!-- Here starts the part of the GUI that was already in part 2 -->
        <h3>Query</h3>
        <div>
            <select id="sample-queries">
                <option selected disabled>Sample queries</option>
                <option>ancestor_descendant(X, Y)</option>
                <option>siblings(X, Y)</option>
                <option>parent_child(X, Y)</option>
                <option>father_child(X, Y)</option>
                <option>mother_child(X, Y)</option>
            </select>
        </div>
        <div>
            <input id="query" type="text" placeholder="Query">
        </div>
        <div>
            <button id="ask-btn">Ask</button>
            <button id="next-btn" disabled>Next</button>
            <button id="stop-btn" disabled>Stop</button>
            <button id="abort-btn" disabled>Abort</button>
            <button id="clear-btn">Clear</button>
        </div>
        <!-- Here start the new part for updating the application db -->
        <h3>Update</h3>
        <div>
            <input id="X" class="arg" type="text" placeholder="Parent">
            <input id="Y" class="arg" type="text" placeholder="Child">
        </div>
        <div>
            Parent is: 
            <input class="radio" type="radio" name="sex" value="father_child" checked>Male
            <input class="radio" type="radio" name="sex" value="mother_child">Female
        </div>
        <div>
            <button id="assert-btn">Assert</button>
            <button id="retract-btn">Retract</button>
        </div>
        <h3>Output</h3>
        <div id="output"></div>
        <!-- Placed at the end of the document so that page loads faster -->
        <script src="/vendor/jquery/jquery-2.0.3.min.js"></script>
        <script src="/pengine/pengines.js"></script>
        <script src="js/genealogist.js"></script>
    </body>
</html>
  ~~~~
Stripped from JavaScript and styling information it does a pretty good job at describing the structure of our GUI. 

Here is the JavaScript file:
  ~~~~
/* JavaScript for the Genealogist application */

var pengine;

function ask() {
    var query = $("#query").val();
    if (query) {
        pengine = new Pengine({
            application: 'genealogist',
            ask: query,
            onsuccess: function() {
                writeln(JSON.stringify(this.data));
                if (this.more) {
                    disableButtons(true, false, false, false);
                } else {
                    writeln("No more solutions");
                    disableButtons(false, true, true, true);                        
                }
            },
            onfailure: function() {
                writeln("Failure");
                disableButtons(false, true, true, true);
            },
            onstop: function() {
                writeln("Stopped");
                disableButtons(false, true, true, true);
            },
            onabort: function() {
                writeln("Aborted");
                disableButtons(false, true, true, true);
            },
            onerror: function() {
                writeln("Error: " + this.data);
                disableButtons(false, true, true, true);
            }
        });
    }
}

function next() {
    pengine.next();
}

function stop() {
    pengine.stop();
}

function abort() {
    pengine.abort();
}


function update(op) {
    var pred = op + $("input[name=sex]:checked").val(),
        X = $("#X").val().toLowerCase() || '_',
        Y = $("#Y").val().toLowerCase() || '_',
        command = pred + '(' + X + ',' + Y + ')';
    Pengine({
        application: 'genealogist',
        ask: command,
        onsuccess: function() {
            writeln(command);
            $("#X,#Y").val("");
        },
        onerror: function() {
            writeln("Error: " + this.data);
        }
    });
}


function writeln(string) {
    $('#output').append(string + "<br />");
}

function disableButtons(ask, next, stop, abort) {
    $("#ask-btn").prop("disabled", ask);
    $("#next-btn").prop("disabled", next);
    $("#stop-btn").prop("disabled", stop);
    $("#abort-btn").prop("disabled", abort);
}


$(document).ready(function() {
    $("#sample-queries").on("change", function() {
        $("#query").val($("#sample-queries option:selected").text());
    });
    $("#ask-btn").on("click", ask);
    $("#next-btn").on("click", next);
    $("#stop-btn").on("click", stop);
    $("#abort-btn").on("click", abort);
    $("#assert-btn").on("click", function() {
        update('assert_');
    });
    $("#retract-btn").on("click", function() {
        update('retract_');
    });
    $("#clear-btn").on("click", function() {
        $('#output').html('');
    });
});

  ~~~~

So, where are we? Well, the Genealogist should now allow us to:
  - Query the database from more than one client at a time.
  - Update the database of parent-child relationships incrementally and from more that one client at a time. 
  - All this without compromising the safety of the pengine server

There are still some problems left to be solved. For example, if you shut down the server and start it again, the database will turn out to be empty. Here, we will only point you to library(persistency) which may be able to help. 
