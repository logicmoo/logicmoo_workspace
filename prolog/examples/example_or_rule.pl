:- use_module(library(trill)).

:- trill.


%subClassOf(a,unionOf([b,c])).
%subClassOf(a,complementOf(e)).
%subClassOf(b,e).
%subClassOf(c,e).

subClassOf(a,intersectionOf([b,someValuesFrom(r,e)])).
subClassOf(a,unionOf([f,allValuesFrom(r,b)])).
subClassOf(b,intersectionOf([c,d])).
subClassOf(c,intersectionOf([minCardinality(1,r),e])).
subClassOf(b,complementOf(e)).

subClassOf(b,complementOf(f)).

subClassOf(a,unionOf([intersectionOf([c,complementOf(c)]),complementOf(f)])).
subClassOf(a, unionOf([complementOf(c),complementOf(f)])).
subClassOf(a, unionOf([complementOf(c),complementOf(d)])).

%classAssertion(a,x).
%classAssertion(complementOf(a),x).



annotationAssertion('disponte:probability',subClassOf(a,intersectionOf([b,someValuesFrom(r,e)])),literal('0.1')).
annotationAssertion('disponte:probability',subClassOf(a,unionOf([f,allValuesFrom(r,b)])),literal('0.2')).
annotationAssertion('disponte:probability',subClassOf(b,intersectionOf([c,d])),literal('0.3')).
annotationAssertion('disponte:probability',subClassOf(c,intersectionOf([minCardinality(1,r),e])),literal('0.4')).
annotationAssertion('disponte:probability',subClassOf(b,complementOf(e)),literal('0.5')).

annotationAssertion('disponte:probability',subClassOf(b,complementOf(f)),literal('0.6')).

annotationAssertion('disponte:probability',subClassOf(a,unionOf([intersectionOf([c,complementOf(c)]),complementOf(f)])),literal('0.7')).
annotationAssertion('disponte:probability',subClassOf(a, unionOf([complementOf(c),complementOf(f)])),literal('0.8')).  % TODO  da testare con Protege
annotationAssertion('disponte:probability',subClassOf(a, unionOf([complementOf(c),complementOf(d)])),literal('0.9')).
