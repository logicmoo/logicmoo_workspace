:- module(allie_sflf,
 [
    in_cluster1/2,
    in_cluster2/2,
    get_longRef/2,
    get_longForm/2,
    get_shortForm/2,
    get_longForms/3
 ]).

:- sparql_endpoint(allie, 'https://data.allie.dbcls.jp/sparql').

:- rdf_register_prefix(allie, 'http://purl.org/allie/ontology/201108#').

in_cluster1(A, B) :- rdf(A, allie:contains, B).
in_cluster2(A, B) :- rdf(A, allie:hasMemberOf, B).

get_longRef(A, B) :- rdf(A, allie:hasLongFormRepresentationOf, B).
get_longForm(A, B) :- rdf(A, allie:hasLongFormOf, B).
get_shortForm(A, B) :- rdf(A, allie:hasShortFormOf, B).

get_longForms(S, L1, L2) :-
  in_cluster1(A, C),
  in_cluster2(C, B),
  get_longRef(A, D),
  rdf(D,rdfs:label,L1),
  get_longForm(B, E),
  rdf(E,rdfs:label,L2),
  get_shortForm(B, F),
  label_of(S, F, "en").
