


:-export(demo_nb_linkval/1).
demo_nb_linkval(T) :-
        T = nice(N),
        (   N = world,
            nb_linkval(myvar, T),
            fail
        ;   nb_getval(myvar, V),
            writeln(V)
        ).

:- dynamic(sk_out/1).
         

q :- q(X), writeln(X).
q(X) :- '$depth_of_var'(X, D), format('Depth = ~w~n', [D]), D < 5, q(X),notail.

notail.

/*
Running this says:

1 ?- q.
Depth = 1
Depth = 2
Depth = 3
Depth = 4
Depth = 5

*/

