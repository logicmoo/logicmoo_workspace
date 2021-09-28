:- use_module(library(sldnfdraw)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(sldnf).
:- endif.

:- sldnf.

:- begin_program.

member(X ,[X|_T]).
member(X ,[_H|T]):-
  member(X,T).

:-end_program.

:-begin_query.

member(X,[1,2]), \+ member(X,[1,3]).

:-end_query.

/** <examples>

?- draw_goal(Tree).
?- draw_goal(Tree),format("~s",[Tree]).

*/
