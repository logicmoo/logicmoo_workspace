% TRILL loaded with OWL/RDF KB initialization
:- use_module(library(trill)).

:- trill. % or :- trillp. or :- tornado.

owl_rdf('<?xml version="1.0"?>

<!--

/** <examples>

 Here examples of the form
 ?- prob_instanceOf(\'className\',\'indName\',Prob).

*/
-->

<!DOCTYPE rdf:RDF [
    <!ENTITY owl "http://www.w3.org/2002/07/owl#" >
    <!ENTITY xsd "http://www.w3.org/2001/XMLSchema#" >
    <!ENTITY rdfs "http://www.w3.org/2000/01/rdf-schema#" >
    <!ENTITY rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#" >
    <!ENTITY disponte "https://sites.google.com/a/unife.it/ml/disponte#" >
]>


<rdf:RDF xmlns="http://here.the.IRI.of.your.ontology#"
     xml:base="http://here.the.IRI.of.your.ontology"
     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
     xmlns:owl="http://www.w3.org/2002/07/owl#"
     xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
     xmlns:disponte="https://sites.google.com/a/unife.it/ml/disponte#">
    <owl:Ontology rdf:about="http://here.the.IRI.of.your.ontology"/>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // Annotation properties
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#probability -->

    <owl:AnnotationProperty rdf:about="&disponte;probability"/>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // Other Axioms
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

    
</rdf:RDF>').

/****************************
 * Other axioms here
 ****************************/
