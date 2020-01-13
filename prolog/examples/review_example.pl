:- use_module(library(trill)).

:- trill.

subClassOf(a,f).
subClassOf(f,b).

subClassOf(a,unionOf([b,c,d])).
subClassOf(a,minCardinality(5,r)).
subClassOf(c,maxCardinality(3,r)).
subClassOf(d,maxCardinality(2,r)).
 
% query: sub_class(a,b).