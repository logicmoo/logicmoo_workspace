:-use_module(library(trill)).

:- trill. % or :- trillp. or :- tornado.

/*
Model of metabolic pathways.
http://www.biopax.org/
*/

/** <examples>

?- prob_sub_class('TransportWithBiochemicalReaction','Entity',Prob).
?- sub_class('TransportWithBiochemicalReaction','Entity',ListExpl).

*/

owl_rdf('<?xml version="1.0"?>
<rdf:RDF xmlns="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#"
     xml:base="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10"
     xmlns:owl="http://www.w3.org/2002/07/owl#"
     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
     xmlns:xml="http://www.w3.org/XML/1998/namespace"
     xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
     xmlns:disponte="https://sites.google.com/a/unife.it/ml/disponte#"
     xmlns:untitled-ontology-10="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#">
    <owl:Ontology rdf:about="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10"/>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // Annotation properties
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#probability -->

    <owl:AnnotationProperty rdf:about="https://sites.google.com/a/unife.it/ml/disponte#probability"/>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // Object Properties
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#objprop10 -->

    <owl:ObjectProperty rdf:about="https://sites.google.com/a/unife.it/ml/disponte#objprop10">
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#InverseFunctionalProperty"/>
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#AsymmetricProperty"/>
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#IrreflexiveProperty"/>
    </owl:ObjectProperty>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#objprop4 -->

    <owl:ObjectProperty rdf:about="https://sites.google.com/a/unife.it/ml/disponte#objprop4">
        <owl:equivalentProperty rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2"/>
        <owl:inverseOf rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#objprop5"/>
    </owl:ObjectProperty>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#objprop5 -->

    <owl:ObjectProperty rdf:about="https://sites.google.com/a/unife.it/ml/disponte#objprop5">
        <rdfs:domain rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#class5"/>
        <rdfs:domain rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#class6"/>
        <rdfs:range rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#class9"/>
        <owl:propertyDisjointWith rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1"/>
    </owl:ObjectProperty>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#objprop6 -->

    <owl:ObjectProperty rdf:about="https://sites.google.com/a/unife.it/ml/disponte#objprop6">
        <owl:propertyChainAxiom rdf:parseType="Collection">
            <rdf:Description rdf:about="https://sites.google.com/a/unife.it/ml/disponte#objprop7"/>
            <rdf:Description rdf:about="https://sites.google.com/a/unife.it/ml/disponte#objprop8"/>
        </owl:propertyChainAxiom>
    </owl:ObjectProperty>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#objprop7 -->

    <owl:ObjectProperty rdf:about="https://sites.google.com/a/unife.it/ml/disponte#objprop7"/>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#objprop8 -->

    <owl:ObjectProperty rdf:about="https://sites.google.com/a/unife.it/ml/disponte#objprop8"/>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#objprop9 -->

    <owl:ObjectProperty rdf:about="https://sites.google.com/a/unife.it/ml/disponte#objprop9">
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#FunctionalProperty"/>
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#SymmetricProperty"/>
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#TransitiveProperty"/>
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#ReflexiveProperty"/>
    </owl:ObjectProperty>
    


    <!-- http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1 -->

    <owl:ObjectProperty rdf:about="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1"/>
    


    <!-- http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2 -->

    <owl:ObjectProperty rdf:about="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2"/>
    


    <!-- http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop3 -->

    <owl:ObjectProperty rdf:about="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop3">
        <rdfs:subPropertyOf rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1"/>
    </owl:ObjectProperty>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // Data properties
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

    


    <!-- http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#dataprop1 -->

    <owl:DatatypeProperty rdf:about="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#dataprop1"/>
    


    <!-- http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#dataprop2 -->

    <owl:DatatypeProperty rdf:about="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#dataprop2"/>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // Classes
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#class10 -->

    <owl:Class rdf:about="https://sites.google.com/a/unife.it/ml/disponte#class10">
        <rdfs:subClassOf>
            <owl:Restriction>
                <owl:onProperty rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1"/>
                <owl:allValuesFrom rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#class9"/>
            </owl:Restriction>
        </rdfs:subClassOf>
        <rdfs:subClassOf>
            <owl:Restriction>
                <owl:onProperty rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2"/>
                <owl:allValuesFrom rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#class9"/>
            </owl:Restriction>
        </rdfs:subClassOf>
    </owl:Class>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#class11 -->

    <owl:Class rdf:about="https://sites.google.com/a/unife.it/ml/disponte#class11">
        <rdfs:subClassOf>
            <owl:Restriction>
                <owl:onProperty rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1"/>
                <owl:minQualifiedCardinality rdf:datatype="http://www.w3.org/2001/XMLSchema#nonNegativeInteger">2</owl:minQualifiedCardinality>
                <owl:onClass rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#class9"/>
            </owl:Restriction>
        </rdfs:subClassOf>
        <rdfs:subClassOf>
            <owl:Restriction>
                <owl:onProperty rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2"/>
                <owl:qualifiedCardinality rdf:datatype="http://www.w3.org/2001/XMLSchema#nonNegativeInteger">3</owl:qualifiedCardinality>
                <owl:onClass rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#class9"/>
            </owl:Restriction>
        </rdfs:subClassOf>
        <rdfs:subClassOf>
            <owl:Restriction>
                <owl:onProperty rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop1"/>
                <owl:maxQualifiedCardinality rdf:datatype="http://www.w3.org/2001/XMLSchema#nonNegativeInteger">4</owl:maxQualifiedCardinality>
                <owl:onClass rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#class9"/>
            </owl:Restriction>
        </rdfs:subClassOf>
    </owl:Class>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#class5 -->

    <owl:Class rdf:about="https://sites.google.com/a/unife.it/ml/disponte#class5"/>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#class6 -->

    <owl:Class rdf:about="https://sites.google.com/a/unife.it/ml/disponte#class6"/>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#class7 -->

    <owl:Class rdf:about="https://sites.google.com/a/unife.it/ml/disponte#class7">
        <owl:disjointWith>
            <owl:Class>
                <owl:unionOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="https://sites.google.com/a/unife.it/ml/disponte#class5"/>
                    <rdf:Description rdf:about="https://sites.google.com/a/unife.it/ml/disponte#class6"/>
                </owl:unionOf>
            </owl:Class>
        </owl:disjointWith>
    </owl:Class>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#class8 -->

    <owl:Class rdf:about="https://sites.google.com/a/unife.it/ml/disponte#class8">
        <owl:disjointUnionOf rdf:parseType="Collection">
            <rdf:Description rdf:about="https://sites.google.com/a/unife.it/ml/disponte#class5"/>
            <rdf:Description rdf:about="https://sites.google.com/a/unife.it/ml/disponte#class6"/>
        </owl:disjointUnionOf>
    </owl:Class>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#class9 -->

    <owl:Class rdf:about="https://sites.google.com/a/unife.it/ml/disponte#class9"/>
    


    <!-- http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1 -->

    <owl:Class rdf:about="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1"/>
    


    <!-- http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class2 -->

    <owl:Class rdf:about="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class2">
        <rdfs:subClassOf rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1"/>
    </owl:Class>
    <owl:Axiom>
        <owl:annotatedSource rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class2"/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2000/01/rdf-schema#subClassOf"/>
        <owl:annotatedTarget rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1"/>
        <disponte:probability rdf:datatype="http://www.w3.org/2001/XMLSchema#decimal">0.7</disponte:probability>
    </owl:Axiom>
    


    <!-- http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class3 -->

    <owl:Class rdf:about="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class3"/>
    


    <!-- http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class4 -->

    <owl:Class rdf:about="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class4">
        <owl:equivalentClass>
            <owl:Class>
                <owl:intersectionOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1"/>
                    <rdf:Description rdf:about="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class3"/>
                </owl:intersectionOf>
            </owl:Class>
        </owl:equivalentClass>
    </owl:Class>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // Individuals
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#ind1 -->

    <owl:NamedIndividual rdf:about="https://sites.google.com/a/unife.it/ml/disponte#ind1">
        <rdf:type rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1"/>
        <rdf:type>
            <owl:Restriction>
                <owl:onProperty rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#objprop2"/>
                <owl:someValuesFrom rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#class5"/>
            </owl:Restriction>
        </rdf:type>
        <owl:sameAs rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#ind2"/>
        <objprop1 rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#ind2"/>
        <dataprop1 rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">3</dataprop1>
    </owl:NamedIndividual>
    <owl:Axiom>
        <owl:annotatedSource rdf:resource="https://sites.google.com/a/unife.it/ml/disponte#ind1"/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#type"/>
        <owl:annotatedTarget rdf:resource="http://www.semanticweb.org/riccardo/ontologies/2019/10/untitled-ontology-10#class1"/>
        <disponte:probability rdf:datatype="http://www.w3.org/2001/XMLSchema#decimal">0.3</disponte:probability>
    </owl:Axiom>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#ind2 -->

    <owl:NamedIndividual rdf:about="https://sites.google.com/a/unife.it/ml/disponte#ind2"/>
    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#ind3 -->

    <owl:NamedIndividual rdf:about="https://sites.google.com/a/unife.it/ml/disponte#ind3"/>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // General axioms
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

    <rdf:Description>
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#AllDifferent"/>
        <owl:distinctMembers rdf:parseType="Collection">
            <rdf:Description rdf:about="https://sites.google.com/a/unife.it/ml/disponte#ind1"/>
            <rdf:Description rdf:about="https://sites.google.com/a/unife.it/ml/disponte#ind3"/>
        </owl:distinctMembers>
    </rdf:Description>
</rdf:RDF>



<!-- Generated by the OWL API (version 4.5.9.2019-02-01T07:24:44Z) https://github.com/owlcs/owlapi -->').

