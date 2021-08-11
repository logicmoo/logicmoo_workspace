---+ How to deal with prefixes in RDF?

The SWI-Prolog RDF library allows you to write explicit resources using
prefix abbreviations, e.g., =|rdf:subClassOf|= or =|rdfs:'Class'|=. The
prefix abbreviations are known through rdf_current_prefix/2.  New prefixes
can be added using rdf_register_prefix/2, as in

    ==
    ?- use_module(library(semweb/rdf_db)).

    ?- rdf_register_prefix(foaf, 'http://xmlns.com/foaf/0.1/').
    ?- rdf_register_prefix(ex,   'http://example.com/person/').
    ==

Now we can assert new facts using the above abbreviations:

    ==
    ?- rdf_assert(ex:bob, rdf:type, foaf:'Person').
    ==

If we query the database, we can see that the actual stored resources
are full URIs represented by atoms:

    ==
    ?- rdf(S,P,O).
    S = 'http://example.com/person/bob',
    P = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
    O = 'http://xmlns.com/foaf/0.1/Person'.
    ==

We can exploit Prolog's `portray' mechanism to show the resources in
a human-readable way:

    ==
    ?- use_module(library(semweb/rdf_portray)).

    ?-rdf(S,P,O).
    S = ex:bob,
    P = rdf:type,
    O = foaf:'Person'.
    ==


---++ Using prefixes in your program

Remember: _|Internally, all resources are atoms|_. The transformations
above are realised at *|compile-time|* using rules for goal_expansion/2
provided by the rdf_db library. If we want to use prefixes in programs,
we have to ensure two things:

    * All prefixes must be declared (using a directive with
    rdf_register_prefix/2) _before_ loading any code that refers to them.
    E.g.:

	==
	:- use_module(library(semweb/rdf_db)).
	:- use_module(library(semweb/rdfs)).
	:- rdf_load(library(semweb/rdfs)).

	:- rdf_register_prefix(foaf, 'http://xmlns.com/foaf/0.1/').

	person(X) :-
	    rdfs_individual_of(X, foaf:'Person').
	==

    * If you want to use the namespace abbreviation to pass arguments to
    your own predicates (e.g., person/1 above), you must declare these
    with rdf_meta/1 as below.  The argument *r* means that the predicate
    expects a resource.  Other argument indicators are described with
    rdf_meta/1.

	==
	:- rdf_meta
	    person(r).
	==

    Now, we can ask the query below.  Without the rdf_meta/1 declaration
    above, the query below results in a type error.

        ==
	?- person(ex:bob).
	true.
	==

    Note that the rdf_meta/1 declaration is only needed for predicates
    that we want to call from the Prolog source using the
    <alias>:<name> notation.

---+++ Using prefixes in the heads of clauses

Sometimes, one wishes to write clauses where head arguments must match a
resource. For example, suppose we want to state which predicates specify a
`label'.  Here, too, we can use the rdf_meta/1 declaration:

==
:- use_module(library(semweb/rdf_db)).

:- rdf_meta
	label_predicate(r).

label_predicate(rdfs:label).
label_predicate(skos:prefLabel).
label_predicate(skos:altLabel).
==


