:- include(test_header).


%  was_module(adb_pfc,[]).



 ~P,P ==> contrradiction.

bird(X), \+ ~fly(X) ==> fly(X).

penguin(X) ==> bird(X).

penguin(X) ==> ~fly(X).

bird(X), injured(X) ==> ~fly(X).

bird(X), dead(X) ==> ~fly(X).
