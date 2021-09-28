:-use_module(library(trill)).

:- trill. % or :- trillp. or :- tornado.

/*
An toy KB to test local consistency.
This KB is inconsistent due to individuals ind1 and ind2.
*/

/** <examples>

?- instanceOf(b,ind1,E). % locally inconsistent
?- inconsistent_theory(E).
E = [classAssertion(a, ind1), 
     classAssertion(complementOf(x), ind2),
     subClassOf(a, allValuesFrom(r, x)),
     propertyAssertion(r, ind1, ind2)
    ].
?- property_value(r,ind3,ind4,E). % locally consistent
?- instanceOf(x,ind4,E). % locally consistent


*/

% Axioms 
classAssertion(a,ind1).
subClassOf(a,allValuesFrom(r,x)).
propertyAssertion(r,ind1,ind2).
%classAssertion(complementOf(x),ind2). %TODO uncomment
subClassOf(a,b).

propertyAssertion(u,ind3,ind4).
subPropertyOf(u,s).
subPropertyOf(s,t).
subPropertyOf(t,r).
classAssertion(a,ind3).