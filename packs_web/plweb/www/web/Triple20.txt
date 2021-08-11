---+ Triple20 -- An RDF/RDFS/OWL visualization and editing tool

Triple20 is a both an XPCE/Prolog library and a stand alone application
for visualizing and editing RDF ontologies. The library builds on top of
the [[semantic web][</package/semweb.html>]] package for storing RDF
models and can be used together with this package for analysing and
debugging the RDF model.

				[[triple20.gif]]

---++ Key features

    $ Unrestricted model :
    Triple20 uses the model-view-controller paradigm where the model
    is the raw RDF triple model. This allows it to provide multiple low-
    and high-level consistent views on the same data and ensures it can
    express anything that can be expressed by RDF. A plugin interface to
    extend and add views is under development.

    $ Scalable :
    Triple20 requires about 80Mb memory per million triples which can be
    loaded in aprox. 3 seconds from its proprietary cached data format
    or 75 seconds from RDF/XML (AMD 1600+).

    $ Multiple ontologies :
    Triple20 can load ontologies from multiple files, present them in a
    unified model. This model can be edited and changes can be written
    to the appropriate files.

    $ Unrestricted undo/redo :
    Editing uses mostly direct manipulation (_|drag and drop|_) and
    any edit operation can be reverted, providing safe and intuitive
    access to the model.


---++ Requirements

Triple20 often requires SWI-Prolog with the semweb library and XPCE for
graphics. These components are default components of the SWI-Prolog
source and binary releases.


---++ Downloading Triple20

Triple20 can be downloaded from its [[GIT
repository][http://eculture.cs.vu.nl/git/public/?p=eculture/triple20.git]] through
[[GIT][<git:>]] or your browser.  For GIT (preferred), use

==
git clone git://eculture.cs.vu.nl/home/git/eculture/triple20.git
==

If you wish to use your browser for a one-time download, go to the
[[repository][http://eculture.cs.vu.nl/git/eculture/triple20.git]] and
download the desired (typically most recent) _snapshot_ link at the
right of the *shortlog* table. The downloaded archive is in Unix tar
format, which is also understood by WinZip.
