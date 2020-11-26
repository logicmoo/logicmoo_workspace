/** <examples>

?- prob(has(4),P).
P = 0.192146176.

?- prob(has(4),has(2),P).
P = 0.380740705882353.

?- prob(has(4),do(has(2)),P).
P = 0.29964160000000006.

?- prob(has(4),(do(apriori(2)),do(viral(2,1))),P).
P = 0.29964160000000006.

?- prob(has(4),(apriori(2),viral(2,1)),P).
P = 0.2996416.

?- prob(has(4),\+ has(2),P).
P = 0.16246000000000002.

?- prob(has(4),(\+ apriori(2),\+ viral(2,1)),P).
P = 0.17833600000000002.
*/

:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(graphviz).
:- endif.
graph(digraph([rankdir="LR"|G])):-
    findall(edge(A -> B,[]),
      clause(trusts(A,B,_,_),_),
      G).

:- pita.

:- begin_lpad.

:- action apriori/1,viral/2,has/1.

has(P):-apriori(P).

has(P):- trusts(P, Q), has(Q),viral(P,Q).

viral(_,_):0.4.
apriori(_):0.1.

trusts(2,1).
trusts(3,1).
trusts(3,2).
trusts(4,1).
trusts(4,3).
:-end_lpad.
