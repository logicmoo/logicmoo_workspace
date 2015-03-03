:- module(tabling, [tabling/1,
		    cleanup_tabling_db/0,
		    cleanup_tabling_db/1
		   ]).

% Implementation of tabling using a separated thread for the goal continuation,
% while its fetched results are memoized, they can be accesed in further calls
% of the tabled goal.

:- dynamic
    tabling_db/2,
    tabled_db/1,
    table_db/3.

cleanup_tabling_db :- cleanup_tabling_db(_).

cleanup_tabling_db(Hash) :-
    retractall(tabling_db(Hash, _)),
    retractall(tabled_db(Hash)),
    retractall(table_db(Hash, _, _)).

:- meta_predicate tabling(0).

tabling(MGoal) :-
    variant_sha1(MGoal, Hash),
    ( tabled_db(Hash)
    ->table_db(Hash, _, MGoal)
    ; \+ tabling_db(Hash, _)
    ->install_goal_fetcher(MGoal, Hash)
    ; fetch_goal_result(MGoal, Hash)
    ).

install_goal_fetcher(MGoal, Hash) :-
    thread_create(do_install_goal_fetcher(MGoal, Hash), Id, []),
    assertz(tabling_db(Hash, Id)),
    fetch_goal_result(MGoal, Hash).

do_install_goal_fetcher(MGoal, Hash) :-
    setup_call_cleanup(M=m(_),
		       setup_goal_fetcher(MGoal, Hash, M),
		       ( retractall(tabling_db(Hash, _)),
			 assertz(tabled_db(Hash)),
			 M = m(CId1),
			 thread_send_message(CId1, end))).

mini_shell(MGoal, M) :-
    thread_get_message(c(Action, CId)),
    ( Action = c
    ->nb_setarg(1, M, CId),
      MGoal
    ; Action = a
    ->thread_exit(abort)
    ).

setup_goal_fetcher(MGoal, Hash, M) :-
    S = s(1),
    mini_shell(MGoal, M),
    ( S = s(N),
      M = m(CId),
      assertz(table_db(Hash, N, MGoal)),
      thread_send_message(CId, done),
      succ(N, N2),
      nb_setarg(1, S, N2)
    ),
    mini_shell(fail, M).

count(1).
count(N) :- count(N1), N is N1 + 1.

fetch_goal_result(MGoal, Hash) :-
    count(N),
    ( table_db(Hash, N, MGoal)
    ->true
    ; tabling_db(Hash, Id)
    ->thread_self(CId),
      thread_send_message(Id, c(c, CId)),
      thread_get_message(Result),
      ( Result = end
      ->!
      %% thread_join(Id, _Status)
      ; true
      ),
      table_db(Hash, N, MGoal)
    ; !,
      fail
    ).
