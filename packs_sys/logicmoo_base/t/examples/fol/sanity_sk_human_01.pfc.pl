:- include(test_header).



:- include('sanity_fi_human.pfc').

:- begin_pfc. 

/*


% Basicly is this.. forall(C, forall(G, grandparent(C,G) => exists(P, (parent(P,G) & parent(C,P))))).
:- call(trace).
:- rtrace(ain(clif(forall(C, forall(G, exists(P,  grandparent(C,G) => (parent(C,P) & (parent(P,G))))))))).

grandparent(douglas,trudy).

:- must(parent(douglas,_X)).

*/
