---+ SWI-Prolog for the (semantic) web

The (semantic) web is one of the most promising application areas for
SWI-Prolog. Prolog handles the semantic web Resource Description Framework (RDF) model naturally, where
RDF provides a stable model for knowledge representation with shared
semantics. Prolog is also quite capable of providing
web services (HTTP), especially where it comes to generating HTML pages dynamically and providing data for Javascript web applications using the
[[JSON][<json:>]] serialisation.

---++ Standard packages

Below are the most vital packages for programming web services and RDF
processing.

    $ [[Web server and client library][</pldoc/package/http.html>]] :
    Core web package. Provides an HTTP server and client, session
    handling, authorization, logging, etc.  Libraries are also included for generating
    HTML pages and JSON (JavaScript Object Notation).

    $ [[RDF storage and query library][</pldoc/package/semweb.html>]] :
    Load/save RDF and query it.  The library loads and saves XML/RDF
    and Turtle.  Its database scales to approx. 20M triples on 32-bit
    hardware and 300M triples on a 64-bit machine with 64 Gb memory.
    Also provides simple RDFS and OWL support.

    [[new.gif]] The GIT version in the development tree has been updated
    to [[version 3][semweb3.txt]].

    $ [[Spatial indexing library][</pldoc/package/space.html>]] :
    SWI-Prolog interface to Spatial Index and GEOS libraries, providing
    spatial indexing of URIs. Supports import and export to GML, KML,
    and RDF with GeoRSS Simple, GeoRSS GML, and W3C WGS84 vocabulary
    properties.

    [[alert.gif]] This package is provided in the source, but not
    (yet) in the binary distributions. The source contains
    instructions for building the dependencies and the indexer itself.


---++ Separately distributed packages

Some packages are distributed separately.


    $ [[The ClioPatria semantic search web server][http://cliopatria.swi-prolog.org]] :
    This library provides a ready-to-run web server that includes a
    SPARQL endpoint, user rights management and RDF development tools.
    ClioPatria is an RDF-application development framework.

    $ [[Thea, an OWL library for SWI-Prolog][<thea:>]] :
    Provides access to OWL ontologies at the level of the abstract
    syntax. Can interact with external DL reasoner using
    [[DIG][<dig:>]].  The project is [[hosted on
    GitHUB][http://wiki.github.com/vangelisv/thea]].  See also
    [[Blip Blog, Logic Programming in Biology][http://blipkit.wordpress.com/category/web/semantic-web-web/]]

    $ [[The Triple20 ontology editor][Triple20.txt]] :
    Graphical tool for browsing and editing ontologies.  Not very well
    maintained, but useful for browsing loaded RDF and it has two nice
    features: it scales well and it can handle multiple ontologies and
    allows for editing them in a unified view.

@see [[Publications][</Publications.html>]]
@see [[documentation overview][</pldoc/index.html>]]
@see [[Topos: A Semantic Web, Linked Data application][http://www.semanticweb.gr/topos/]]
@see [[DBTune blogs tagged SWI-Prolog][http://blog.dbtune.org/tag/swi-prolog]]
