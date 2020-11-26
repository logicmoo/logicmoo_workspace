:-use_module(library(trill)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(graphviz).
:- endif.

:- trill. % or :- trillp. or :- tornado.

/** <examples>

?- instanceOf(person,john,Expl).

*/

owl_rdf('<?xml version="1.0"?>
<rdf:RDF xmlns="http://example.foo#"
     xml:base="http://example.foo"
     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
     xmlns:owl="http://www.w3.org/2002/07/owl#"
     xmlns:xml="http://www.w3.org/XML/1998/namespace"
     xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#">
    <owl:Ontology rdf:about="http://example.foo"/>

    <!-- Classes -->
    <owl:Class rdf:about="http://example.foo#worker">
        <rdfs:subClassOf rdf:resource="http://example.foo#person"/>
    </owl:Class>

</rdf:RDF>').
subClassOf('employee','worker').
owl_rdf('<?xml version="1.0"?>
<rdf:RDF xmlns="http://example.foo#"
     xml:base="http://example.foo"
     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
     xmlns:owl="http://www.w3.org/2002/07/owl#"
     xmlns:xml="http://www.w3.org/XML/1998/namespace"
     xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#">
    <owl:Ontology rdf:about="http://example.foo"/>
    
    <!-- Individuals -->
    <owl:NamedIndividual rdf:about="http://example.foo#john">
        <rdf:type rdf:resource="http://example.foo#employee"/>
    </owl:NamedIndividual>
</rdf:RDF>').
