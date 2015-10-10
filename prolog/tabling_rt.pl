/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(tabling_rt, [(table)/2,
		       abolish_table/1,
		       abolish_all_tables/0
		      ]).

% Implementation of tabling using a separated thread for the goal continuation,
% while its fetched results are memoized, they can be accessed in further calls
% of the tabled goal.

% Tabling should not support dynamic predicates, because is inconsistent to what
% it is intended for.

% While we can not save a suspended goal into the saved state, we can only have
% volatile facts to store the results.

% TODO: implement yap interface:
% show_table(+P)
% table_statistics(+P)
% tabling_statistics/0

:- use_module(xlibrary(implementation_module)).

:- dynamic
    goal_table_db/3,
    tabling_db/2,
    tabled_db/1,
    table_db/4.

:- volatile
    goal_table_db/3,
    tabling_db/2,
    tabled_db/1,
    table_db/4.

cleanup_tabling_db(Hash) :-
    retractall(goal_table_db(_, _, Hash)),
    retractall(tabling_db(Hash, _)),
    retractall(tabled_db(Hash)),
    retractall(table_db(Hash, _, _, _)).

strip_goal_module(CM:Goal0, _, Goal, M) :- !,
    strip_goal_module(Goal0, CM, Goal, M).
strip_goal_module(Goal, CM, Goal, M) :-
    implementation_module(CM:Goal, IM),
    ( predicate_property(CM:Goal, transparent)
    ->M = CM
    ; M = IM
    ).

:- meta_predicate abolish_table(0).
abolish_table(CM:H) :-
    strip_goal_module(H, CM, Goal, M),
    forall(retract(goal_table_db(Goal, M, Hash)),
	   ( abort_fetcher(Hash),
	     cleanup_tabling_db(Hash)
	   )).

abort_fetcher(Hash) :-
    forall(tabling_db(Hash, Id),
	   thread_send_message(Id, c(a, Id))).
    
abolish_all_tables :-
    abort_fetcher(_),
    cleanup_tabling_db(_).

:- meta_predicate table(+, 0).

table(HKey, CM:H) :-
    strip_goal_module(H, CM, Goal, M),
    '$expand':variant_sha1_nat(M:HKey, Hash),
    ( tabled_db(Hash)
    ->table_db(Hash, _, HKey, M)
    ; ( \+ tabling_db(Hash, _)
      ->setup_fetcher(HKey, Goal, M, Hash)
      ; true
      ),
      fetch_result(HKey, M, Hash)
    ).

setup_fetcher(HKey, Goal, M, Hash) :-
    assertz(goal_table_db(HKey, M, Hash)),
    thread_create(do_setup_fetcher(HKey, Goal, M, Hash), Id, []),
    assertz(tabling_db(Hash, Id)).

do_setup_fetcher(HKey, Goal, M, Hash) :-
    setup_call_cleanup(IdH=m(_),
		       run_fetcher(HKey, Goal, M, Hash, IdH),
		       ( retractall(tabling_db(Hash, _)),
			 assertz(tabled_db(Hash)),
			 IdH = m(Id),
			 thread_send_message(Id, end))).

mini_shell(MGoal, IdH) :-
    thread_get_message(c(Action, Id)),
    ( Action = c
    ->nb_setarg(1, IdH, Id),
      MGoal
    ; Action = a
    ->thread_exit(abort)
    ).

run_fetcher(HKey, Goal, M, Hash, IdH) :-
    S = s(1),
    mini_shell(M:Goal, IdH),
    ( S = s(N),
      IdH = m(Id),
      assertz(table_db(Hash, N, HKey, M)),
      thread_send_message(Id, done),
      succ(N, N2),
      nb_setarg(1, S, N2)
    ),
    mini_shell(fail, IdH).

count(1).
count(N) :- count(N1), N is N1 + 1.

fetch_result(HKey, M, Hash) :-
    count(N),
    ( table_db(Hash, N, HKey, M)
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
      table_db(Hash, N, HKey, M)
    ; !,
      fail
    ).
