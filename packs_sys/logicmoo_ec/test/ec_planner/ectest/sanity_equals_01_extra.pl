
axiom(equals(X, Y), [call(X = Y)]).
axiom(not(equals(X, Y)), [call(X \= Y)]).
%axiom((G ; _), [G]).
%axiom((_ ; G), [G]).
axiom(neighbor(1, 2), []).
axiom(neighbor(X, Y), [call(X>Y), neighbor(Y, X)]).
% axiom(neighbor(X, Y), [ neighbor(Y, X)]).
/*
Most animals can compare natural numbers. But counting is a "behavoviour script" that comes from playing a game with words. 
That game requires using language.  
Until such a game is played it is very unlikely to expect these numbers belong in a "numerical order".
Most animals (including bees) can see the numerical difference between the natural numbers 
(though bees are born with language with syntax and smeantic rules).
*/
%"Language" is a system that allows thoughts to be turned into a low mental bandwith immediate thoughtform.  Buffered in suchb a way that when echo'd a new unrelated thoughtform can take place  




% From /opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/ec_planner/ectest/sanity_equals_01.e:34
%; Prolog code starts with ;:-
do_test(all):-  maplist(call, 
 [ 
    assert(( test_neighbor(X, Y) :- must(ec_prove(neighbor(X, Y))), must(ec_prove(neighbor(Y, X))) )),

    assert(( test_not_neighbor(X, Y) :- must(ec_prove(not(neighbor(X, Y)))), must(ec_prove(not(neighbor(Y, X)))) )),

    test_neighbor(1, 2),
    test_neighbor(1, 3),
    test_neighbor(1, 4),
    test_neighbor(2, 3),
    test_neighbor(2, 4),
    test_neighbor(3, 4),
    test_neighbor(4, 7),
    test_not_neighbor(4, 8),
    test_neighbor(5, 6),
    test_neighbor(5, 7),
    test_neighbor(5, 8),
    test_neighbor(6, 7),
    test_neighbor(6, 8),
    test_neighbor(7, 8) ]).

:- listing([ec_current_domain_db, axiom]).


