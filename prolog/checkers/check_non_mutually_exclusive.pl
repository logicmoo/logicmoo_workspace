/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

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

:- module(check_non_mutually_exclusive, []).

:- use_module(library(apply)).
:- use_module(library(check), []).
:- use_module(library(clambda)).
:- use_module(library(normalize_head)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(referenced_by)).

:- multifile
    prolog:message//1,
    mutually_exclusive_predicate/2,
    mutually_exclusive_predicate_key/3.

:- dynamic mutually_exclusive_db/1.

option_allmchk(OptionL0, OptionL, option_utils:call_2(FileGen, File)) :-
    option_allchk(_M, File, FileGen-OptionL0, true-OptionL).

checker:check(non_mutually_exclusive, Result, OptionL0 ) :-
    option_allmchk(OptionL0, OptionL1, FileChk),
    select_option(predicate(Ref), OptionL1, _, Ref),
    findall(Pairs, check_non_mutually_exclusive(from_chk(FileChk), Ref, Pairs), Result).

check_non_mutually_exclusive(FromChk, Ref, warning-(Ref-LocIdx)) :-
    normalize_head(Ref, M:H),
    mutually_exclusive_predicate(H, M),
    collect_non_mutually_exclusive(FromChk, H, M, LocIdxL),
    member(LocIdx, LocIdxL).

cleanup_redundant_groups([], _, []).
cleanup_redundant_groups([Key-Clause-ClauseNME|ClauseKeyU], ClauseKeyI, ClauseKeyR) :-
       ( \+ ( member(Key2-Clause2-ClauseNME2, ClauseKeyU),
	      subset([Key-Clause|ClauseNME], [Key2-Clause2|ClauseNME2]),
	      \+subset([Key2-Clause2|ClauseNME2], [Key-Clause|ClauseNME])
	    ),
	 \+ ( member(Key2-Clause2-ClauseNME2, ClauseKeyI),
	      subset([Key-Clause|ClauseNME], [Key2-Clause2|ClauseNME2])
	    )
       ->ClauseKeyR=[Key-Clause-ClauseNME|ClauseKeyR2],
	 ClauseKeyI2=[Key-Clause-ClauseNME|ClauseKeyI]
       ; ClauseKeyR=ClauseKeyR2,
	 ClauseKeyI2=ClauseKeyI
       ),
       cleanup_redundant_groups(ClauseKeyU, ClauseKeyI2, ClauseKeyR2).

collect_non_mutually_exclusive(FromChk, H, M, LocPL) :-
    findall(I-(Key-Clause),
	    ( nth_clause(M:H, I, Clause),
	      From = clause(Clause),
	      call(FromChk, From),
	      clause(M:P, _, Clause),
	      ( mutually_exclusive_predicate_key(P, M, Key)
	      ->true
	      ; Key = M:P
	      )
	    ),
	    ClauseKeyU),
    ClauseKeyU \= [],
    list_to_ord_set(ClauseKeyU, ClauseKeyL),
    findall(Key-Clause-ClauseNME,
	    [Clause, Key, ClauseNME, ClauseKeyL] +\
	    ( select(_-(Key-Clause), ClauseKeyL, ClauseKeyS),
	      exclude([Key] +\ (_-(SKey-_)) ^ (SKey\=Key), ClauseKeyS, ClauseKeyNME),
	      ClauseKeyNME \= [],
	      pairs_values(ClauseKeyNME, ClauseNME)
	    ),
	    ClausePR),
    cleanup_redundant_groups(ClausePR, [], ClausePL),
    maplist(\ (Key1-Clause1-ClauseNME1)^((Loc1-Idx1/Key1)/LocL)
	   ^( nth_clause(_, Idx1, Clause1),
	      from_location(clause(Clause1), Loc1),
	      maplist(\ (Key2-Clause2)^(Loc2-Idx2/Key2)
		     ^( nth_clause(_, Idx2, Clause2),
			from_location(clause(Clause2), Loc2)
		      ),
		      ClauseNME1,
		      LocU),
	      sort(LocU, LocL)
	    ), ClausePL, LocPU),
    sort(LocPU, LocPL).

prolog:message(acheck(non_mutually_exclusive)) -->
    ['---------------------------------',nl,
     'Non Mutually Exclusive Predicates',nl,
     '---------------------------------',nl,
     'The predicates below are marked as mutually_exclusive, but they have', nl,
     'non mutually exclusive clauses. You can resolve the ambiguity unifying', nl,
     'the non mutual exclusive clauses or changing the specification of such', nl,
     'predicates.', nl, nl].
prolog:message(acheck(non_mutually_exclusive, PI-LocCIs)) -->
    check:predicate(PI),
    [' has non mutually exclusive clauses:', nl],
    foldl(group_non_mut_ex, LocCIs).

locindex_index(_-I/_, I).

locindex_loccl(Loc-I/K, Loc/[' clause ~w'-[I, K]]).

group_non_mut_ex((Loc-Idx/Key)/LocIdxL) -->
    {maplist(locindex_index, LocIdxL, Idxs)},
    {maplist(locindex_loccl, LocIdxL, LCIs)},
    Loc, ['Clause ~w (key ~w) match with clauses ~w at'-[Idx, Key, Idxs], nl],
    referenced_by(LCIs).
