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

:- module(clambda, [op(201,xfx,+\)]).

/** <module> Lambda expressions

This library is semantically equivalent to the lambda library implemented by
Ulrich Neumerkel, but it performs static expansion of the expressions to improve
performance.
  
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(occurs)).
:- use_module(xlibrary(compound_expand)).

remove_hats(^(H, G0), G) -->
    [H], !,
    remove_hats(G0, G).
remove_hats(G, G) --> [].

remove_hats(G0, G, EL) :-
    remove_hats(G0, G1, EL, T),
    '$expand':extend_arg_pos(G1, _, _, T, G, _).
    
cgoal_args(G0, G, AL, EL) :-
    G0 =.. [F|Args],
    cgoal_args(F, Args, G, Fr, EL),
    term_variables(Fr, AL).

cgoal_args(\,  [G1|EL],    G, [], EL) :- remove_hats(G1, G, EL).
cgoal_args(+\, [Fr,G1|EL], G, Fr, EL) :- remove_hats(G1, G, EL).

singleton(T, Name=V) :-
    occurrences_of_var(V, T, 1),
    \+ sub_atom(Name, _, _, _, '_').

have_name(VarL, _=Value) :-
    member(Var, VarL),
    Var==Value, !.

bind_name(Name=_, Name).

check_singletons(Goal, Term) :-
    term_variables(Term, VarL),
    ( b_getval('$variable_names', Bindings)
    ->true
    ; Bindings = []
    ),
    include(have_name(VarL), Bindings, VarN),
    include(singleton(Term), VarN, VarSN),
    ( VarSN \= []
    ->maplist(bind_name, VarSN, Names),
      print_message(warning, local_variables_outside(Names, Goal, Bindings))
    ; true
    ).

prolog:message(local_variables_outside(Names, Goal, Bindings)) -->
    [ 'Local variables ~w should not occurs outside lambda expression: ~W'
    -[Names, Goal, [variable_names(Bindings)]] ].

lambdaize_args(G, A0, M, VL, Ex, A) :-
    check_singletons(G, h(VL, Ex, A0 )),
    ( ( Ex==[]
      ; '$member'(E1, Ex),
	'$member'(E2, VL),
	E1==E2
      )
    ->'$expand':wrap_meta_arguments(A0, M, VL, Ex, A)
    ; '$expand':remove_arg_pos(A0, _, M, VL, Ex, A, _)
    ).

goal_expansion(G0, G) :-
    callable(G0),
    cgoal_args(G0, G1, AL, EL),
    '$set_source_module'(M, M),
    expand_goal(G1, G2),
    lambdaize_args(G0, G2, M, AL, EL, G3),
    % '$expand':wrap_meta_arguments(G2, M, AL, EL, G3), % Use this to debug
    G3 =.. [AuxName|VL],
    append(VL, EL, AV),
    G =.. [AuxName|AV].
