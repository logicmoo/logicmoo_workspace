
:- include(training).

:-dynamic(used_cl/1).

map_sent(_,Sent):- ground(Sent),!.
map_sent(Loc,Sent):- var(Sent), length(Sent,9),map_sent(Loc,Sent).
map_sent(Loc,[A,B,C,D|More]):-
  some_ngram(Loc,A,B,C,D,_Fire),
  map_sent(Loc,[C,D|More]).
map_sent(Loc,[A,B,C,D|More]):-
  some_ngram(Loc,A,B,C,_,_Fire),
  map_sent(Loc,[B,C,D|More]).
map_sent(Loc,List):- ABCDO=[_,_,_,_,_Occurs],append(List,_,ABCDO), 
  apply(some_ngram,[Loc|ABCDO]).

some_ngram(_PrevLoc,A,B,C,D,N):- ngram(Loc,A,B,C,D,N), may_use(Loc,A,B,C,D,N).


:- style_check(- singleton).

may_use(Loc,_,B,C,D,_):- \+ used_cl(ngram(A,B,C,D)), assert(used_cl(ngram(A,B,C,D)),Cl2), undo(erase(Cl2)), !.


gen6([A,B,C,D,E,F,G,H]=N):-
  ngram(Loc1,E,F,G,H,Z), ngram(Loc2,C,D,E,F,Y), ngram(Loc3,A,B,C,D,X), N is X+Y+Z.

:- fixup_exports.


