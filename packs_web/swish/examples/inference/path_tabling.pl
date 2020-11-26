/** <examples>

?- prob(path(a,e),Prob).
?- prob(path(a,e),Prob),bar(Prob,C).
?- graph(G).

*/
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- endif.

:- pita.

:- table path/2.

:- begin_lpad.
path(X,X).

path(X,Y):-
  path(X,Z),edge(Z,Y).

edge(X,Y):-arc(X,Y).
edge(X,Y):-arc(Y,X).
arc(a,b):0.2.
arc(b,e):0.5.
arc(a,c):0.3.
arc(c,d):0.4.
arc(d,e):0.4.
arc(a,e):0.1.
:- end_lpad.
graph(digraph([rankdir='LR'|G])):-
    findall(edge(A - B,[label=P,dir=none]),
      clause(arc(A,B,_,_),(get_var_n(_,_,_,_,[P|_],_),_)),
      G).
