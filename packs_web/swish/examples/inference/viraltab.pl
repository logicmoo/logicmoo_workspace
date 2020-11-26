/** <examples>
?- prob(has(4),P).
P = 0.192146176.

?- prob(has(4),has(2),P).
P = 0.380740705882353.

?- prob(has(4),do(has(2)),P).
P = 0.29964160000000006.

?- prob(has(4),(\+ has(2)),P).
P = 0.16246000000000002.

?- prob(has(4),(do(\+ has(2))),P).
P = 0.192146176.


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

:-table has/1,path/2.

:- begin_lpad.

:- action has/1.

has(_):0.1.

has(P) :0.4 :- mutual_trusts(P, Q), has(Q).



mutual_trusts(A,B):-
  trusts(A,B).

mutual_trusts(A,B):-
  trusts(B,A).

trusts(2,1).
trusts(3,1).
trusts(3,2).
trusts(4,1).
trusts(4,3).
:-end_lpad.
