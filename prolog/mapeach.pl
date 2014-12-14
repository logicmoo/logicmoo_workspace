:- module(mapeach, [mapeach/4]).

:- meta_predicate mapeach(0,2,?,?).

mapeach(Goal0, Goal, Head, Tail) :-
    S = s(Head),
    forall(Goal0,
	   ( S = s(H),
	     call(Goal, H, T),
	     nb_setarg(1, S, T)
	   )),
    S = s(Tail).
