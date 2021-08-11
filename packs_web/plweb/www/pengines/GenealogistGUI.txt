# The Genealogist Part 2: Giving it a web-based GUI

In [part 1 of this tutorial](http://www.swi-prolog.org/pengines/Genealogist.html) we built a Pengines based web-service capable of serving up solutions to queries about family relations over HTTP. In this part we shall create a simple web GUI for our Genealogist application.

Here is the HTML we shall work with:

  ~~~~
  <html lang="en">   
    <head>
        <script src="/vendor/jquery/jquery-2.0.3.min.js"></script>
        <script src="/pengine/pengines.js"></script>
        <style>
            body, input, textarea {
                font-family: monospace;
                font-size: 14px;
            }
            textarea, input {
                width: 300px;
                padding: 4px;
            }
        </style>
        <script type="text/javascript">
            var pengine;
            function query() {
                pengine = new Pengine({
                    application: 'genealogist',
                    ask: $("#query").val(),
                    onsuccess: handleSuccess,
                    onfailure: handleFailure,
                    onerror:   handleError
                });
            }
            function handleSuccess() {
                var solution = JSON.stringify(this.data);
                writeln(solution);
                if (!this.more) {
                    writeln("No more solutions");
                }
            }
            function handleFailure() {
                writeln("No more solutions");
            }
            function handleError() {
                writeln(this.data);
            }
            function writeln(string) {
                $('#output').append(string + "<br />")
            }
        </script>
    </head>
    <body>
        <h1>The Genealogist</h1>
        <h3>Query</h3>
        <input type="text" id="query" value="ancestor_descendant(X,Y)">
        <button onclick="query()">Ask</button>
        <button onclick="pengine.next()">Next</button>
        <button onclick="pengine.stop()">Stop</button>
        <h3>Output</h3>
        <div id="output"></div>
    </body>
</html>
  ~~~~
We then need to make this HTML file available to the server. It can be placed anywhere from where it can be retrieved and served to an external browser, but we think that it sits most comfortably in the =|./apps/genealogist|= directory, along with the Prolog file.

  ~~~~
  apps/
     genealogist/
        genealogist.pl
        index.html
     swish\
     scratchpad\
  ~~~~

The application can now be tested. We don't even have to restart the server. All we need need to know is the URL pointing to the application. If you are running your Pengines installation as the default =|localhost:3030|= that would be:

  ~~~~
  http://localhost:3030/apps/genealogist/index.html
  ~~~~

In [the next part of the tutorial](http://www.swi-prolog.org/pengines/GenealogistUpdate.html) we are going to implement support for updating the database of parent-child relationships.
