:- module(xcoverage, [xcover/1]).

:- meta_predicate xcover(0).

xcover(Goal) :-
    setup_call_cleanup(setup_trace(State),
		       Goal,
		       cleanup_trace(State)).
