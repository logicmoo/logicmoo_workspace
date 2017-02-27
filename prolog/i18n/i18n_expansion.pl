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

:- module(i18n_expansion, []).

:- reexport(library(i18n/i18n_op)).
:- use_module(library(compound_expand)).
:- use_module(library(i18n/i18n_support)).

translate_args(Meta, M, Goal0, Goal) :-
    functor(Meta, F, A),
    functor(Goal, F, A),
    translate_arg(1, Meta, M, Goal0, Goal).

translate_arg(N, Meta, M, Goal0, Goal) :-
    arg(N, Meta, Spec), !,
    arg(N, Goal0, Term0),
    arg(N, Goal,  Term),
    translate_non_goal_term(Spec, M, Term0, Term),
    succ(N, N1),
    translate_arg(N1, Meta, M, Goal0, Goal).
translate_arg(_, _, _, _, _).

translate_non_goal_term(S, _, Term, Term) :-
    '$expand':direct_call_meta_arg(S), !.
translate_non_goal_term(S, M, Term0, Term) :-
    translate_term(S, M, Term0, Term).

translate_term(S, M, Term0, Term) :-
    '$expand':meta_arg(S), !,
    translate_term_meta(M, Term0, Term).
translate_term(_, M, Term0, Term) :-
    translate_term(M, Term0, Term).

translate_term_meta(_, Term0, M:Term) :-
    nonvar(Term0),
    Term0 = M:Term1, !,
    translate_term_meta(M, Term1, Term).
translate_term_meta(M, Term0, Term) :-
    translate_term(M, Term0, Term).

gtabling(Elem, Goal) :-
    ( \+ Elem->
      ( Goal,
        compile_aux_clauses(Elem),
        fail
      ; true
      )
    ; true
    ).

tabulate_i18n_records(M) :-
    gtabling(i18n_support:i18n_record(M, L, I, S),
             current_i18n_record(M, L, I, S)).

translate_term(Term0, Term) :-
    '$current_source_module'(M),
    translate_term(M, Term0, Term),
    Term0 \== Term.

translate_term(M, Term0, Term) :-
    tabulate_i18n_records(M),
    expand_i18n_term(i18n_entry_expander, M, Term0, Term).

goal_expansion(V:Goal0, M, V:Goal) :-
    var(M), !,
    translate_term(M, Goal0, Goal).
goal_expansion(M:Goal0, _, M:Goal) :- !,
    goal_expansion(Goal0, M, Goal).
goal_expansion(Goal0, M, Goal) :-
    ( predicate_property(M:Goal0, meta_predicate(Meta))
    ->translate_args(Meta, M, Goal0, Goal)
    ; translate_term(M, Goal0, Goal)
    ), !,
    Goal0 \== Goal.

goal_expansion(Goal0, Goal) :-
    callable(Goal0),
    '$current_source_module'(M),
    goal_expansion(Goal0, M, Goal).

/* Commented out due to this cause cyclic terms when clause_info is
 * executed over this predicate itself:

goal_expansion(A=~B0, A=B) :-
    nonvar(B0), !,
    translate_term(~B0, B).
goal_expansion(A=~~B0, A=B) :-
    nonvar(B0), !,
    translate_term(~~B0, B).
goal_expansion(A = IB, A=~B) :-
    nonvar(IB),
    IB = ~(B), !.
goal_expansion(A = IB, G) :- % A bit complex due to static optimizations:
    nonvar(IB),
    IB = ~~(B),
    ( language(en),
      dictionary(Dict),
      en \= Dict
    ->G = (A~=B)
    ; language(Lang),
      dictionary(Lang),
      \+ ((dictionary(Dict2), Dict2 \= Lang))
    ->G = (A = B)
    ; language(en),
      \+ dictionary(_)
    ->G = (A = B)
    ; G = (A=~~B)
    ).
*/

term_expansion((:- i18n_resource(PoAlias)),
               i18n_support:i18n_resource(M, PoAlias)) :- !,
    '$current_source_module'(M).
term_expansion((:- resourceterm(Term)),
               i18n_support:i18n_resourceterm(M, Term)) :- !,
    '$current_source_module'(M).
term_expansion((:- init_i18n),
               []) :- !,
    '$current_source_module'(M),
    tabulate_i18n_records(M).
term_expansion((:- M:init_i18n), [])  :- !, tabulate_i18n_records(M).
term_expansion((:- init_i18n(M)), []) :- !, tabulate_i18n_records(M).
term_expansion((:- _), _) :- !, fail. % Skip declarations
term_expansion((Term0 :- Body), (Term :- Body)) :- !,
    translate_term(Term0, Term).
term_expansion((Term0 --> Body), (Term --> Body)) :- !,
    translate_term(Term0, Term).
term_expansion(Term0, Term) :-
    translate_term(Term0, Term).
