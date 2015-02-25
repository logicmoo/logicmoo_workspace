:- module(commited_retract, [commited_retract/1]).

:- meta_predicate commited_retract(0).
commited_retract(Fact) :-
    repeat,
    ( retract(Fact)
    ->true
    ; !,
      fail
    ).
