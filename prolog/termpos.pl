/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2019, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(termpos, [op(1150, fx, (add_termpos))]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- reexport(library(compound_expand)).
:- use_module(library(subpos_utils)).
:- use_module(library(transpose)).

:- multifile '$add_termpos'/4.

atp_expand_args(+, A, [ [], [A], [P],  [],  []]) --> [A, P]. % Input
atp_expand_args(-, A, [ [],  [],  [], [A], [P]]) --> [A, P]. % Output
atp_expand_args(?, A, [[A],  [],  [],  [],  []]) --> [A].    % Bypass

subterm_location_eq(Find, Term, Loc, Loc) :- Find==Term.
subterm_location_eq(Find, Term, PPos, Loc) :-
    nonvar(PPos),
    PPos = parentheses_term_position(_, _, Pos),
    !,
    subterm_location_eq(Find, Term, Pos, Loc).
subterm_location_eq(Find, Term, list_position(F, T, [EPos|LPos], TPos), Loc) :-
    compound(Term),
    Term = [E|L],
    ( subterm_location_eq(Find, E, EPos, Loc)
    ; subterm_location_eq(Find, L, list_position(F, T, LPos, TPos), Loc)
    ).
subterm_location_eq(Find, Term, term_position(_, _, _, _, PosL), Loc) :-
    compound(Term),
    arg(N, Term, Arg),
    nth1(N, PosL, Pos),
    subterm_location_eq(Find, Arg, Pos, Loc).
subterm_location_eq(Find, Term, brace_term_position(_, _, Pos), Loc) :-
    compound(Term),
    Term = {Arg},
    subterm_location_eq(Find, Arg, Pos, Loc).

var_location(Term, Pos, Var, Loc) :-
    ( subterm_location_eq(Var, Term, Pos, Loc)
    ->true
    ; true
    ).

collect_argpos([_, InL, InPosL, OutL, OutPosL], ArgPosLL) :-
    maplist(transpose, [[InL, InPosL], [OutL, OutPosL]], ArgPosLLL),
    append(ArgPosLLL, ArgPosLL).

link_headpos(GArgPosLL, HArgPosLL) :-
    foldl(link_argpos(HArgPosLL), GArgPosLL, NewArgPosLL, HArgPosLL),
    b_setval('$argpos', NewArgPosLL).

link_argpos(HArgPosLL, [Arg, Pos]) -->
    ( { member([HArg, HPos], HArgPosLL),
        HArg == Arg
      }
    ->{HPos = Pos}
    ; [[Arg, Pos]]
    ).

term_expansion((:- add_termpos Spec),
               termpos:'$add_termpos'(M, Head, NewHead, InOutArgPosLL)) :-
    '$current_source_module'(M),
    functor(Spec, F, A),
    functor(Head, F, A),
    Spec =.. [F|SArgL],
    Head =.. [F|HArgL],
    foldl(atp_expand_args, SArgL, HArgL, InOutArgPosLLL, NewArgL, []),
    transpose(InOutArgPosLLL, InOutArgPosLLT),
    maplist(append, InOutArgPosLLT, InOutArgPosLL),
    NewHead =.. [F|NewArgL].
term_expansion((Head :- Body), (NewHead :- NewBody)) :-
    nonvar(Head),
    '$current_source_module'(M),
    '$add_termpos'(M, Head, NewHead, LL),
    LL = [_, InL, InPosL, OutL, OutPosL],
    maplist(var_location(InL, list_position(_, _, InPosL, _)), OutL, OutPosL),
    collect_argpos(LL, APL),
    b_setval('$termpos', LL),
    b_setval('$argpos', APL),
    expand_goal(Body, NewBody),
    nb_delete('$termpos'),
    nb_delete('$argpos').
term_expansion(Head, NewHead) :-
    '$current_source_module'(M),
    '$add_termpos'(M, Head, NewHead, [_, InL, InPosL, OutL, OutPosL]),
    maplist(var_location(InL, list_position(_, _, InPosL, _)), OutL, OutPosL).

goal_expansion(Goal, NewGoal) :-
    '$current_source_module'(M),
    '$add_termpos'(M, Goal, NewGoal, GLL),
    GLL = [_, InL, InPosL, OutL, OutPosL],
    nb_current('$termpos', [_, HInL, HInPosL, _, _]),
    maplist(var_location(HInL, list_position(_, _, HInPosL, _)), InL, InPosL),
    maplist(var_location(HInL, list_position(_, _, HInPosL, _)), OutL, OutPosL),
    maplist(var_location(InL,  list_position(_, _, InPosL, _)), OutL, OutPosL),
    maplist(var_location(InL,  list_position(_, _, InPosL, _)), HInL, HInPosL),
    collect_argpos(GLL, GAPL),
    nb_current('$argpos', HAPL),
    link_headpos(GAPL, HAPL).
