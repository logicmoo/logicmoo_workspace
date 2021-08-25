%% front-end for Aleph and prodlr
%% 
%% Author: Angelos Charalambidis <acharal@users.sourceforge.net>
%%         Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%% Created: 20 Jun 2012
%% 
%% Copyright (C) 2006-2012 Stasinos Konstantopoulos
%% Copyright (C) 2007-2012 Angelos Charalambidis
%%
%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 2 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along
%% with this program; if not, write to the Free Software Foundation, Inc.,
%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


:- use_module(library(lists)).

% settings
% set(refinethreadvar, vid).
%

progol_engine(aleph).


:- multifile legitimate_literal/1.

set_def(refinethreadvar, refineop,
        'Description',
        templatevar, 'thread', 
        show). 

set_def(refinelitgen, refineop,
        'Description',
        [modes, user], modes,
        show).

lith(Head) :-
    progol_engine(Engine),
    lith(Engine, Head).

litb(Lit) :-
    progol_engine(Engine),
    litb(Engine, Lit).


thread_type(Type) :-
    setting(refinethreadvar, Type).

lith(aleph, Head) :-
    '$aleph_global'(modeh, modeh(_, Head)).

litb(aleph, Lit) :- 
    '$aleph_global'(modeb, modeb(_, Lit)).

refine(A, B) :-
    setting(construct_bottom, saturation) 
    ->  refine_bot(A, B) 
      ; refine_lazy(A, B).

refine_lazy(nil, Clause) :-
   lith(HeadTemplate),
   functor(HeadTemplate, Name, Arity),
   functor(Head, Name, Arity),  
   Clause = (Head).

refine_lazy((Head:-Body), (Head:-Body2)):- !,
   generate_lit(Lit),
   copy_term(Body, Body1),
   append_thread(Lit, Body1, Body2),
   connect_thread((Head:-Body2)).

refine_lazy(Head, Clause) :- !,
   refine_lazy((Head:-true), Clause).


generate_lit(Lit) :-
   setting(refinelitgen, user)
   -> legitimate_literal(Lit)
   ; generate_lit_modes(Lit). 

generate_lit_modes(Lit) :-
   find_mode(modeb, Name/Arity, Mode),
   functor(Lit, Name, Arity),
   split_args(Lit, Mode, _Inputs, _Outputs, Constants),
   generate_constants(Lit, Constants).

generate_constants(Lit, []).
generate_constants(Lit, [Type/Place|Z]) :-
   arg(Lit, Place, ConArg),
   call(Type,ConArg),
   generate_constants(Lit, Z).


subset([], []).
subset([X|L], [X|S]) :- subset(L, S).
subset(L, [_|S]) :- subset(L, S).

unordered_subset(Set, SubSet):-
  length(Set, LSet),
  between(0,LSet, LSubSet),
  length(NSubSet, LSubSet),
%  permutation(SubSet, NSubSet),
%  SubSet = NSubSet,
  subset(NSubSet,Set),
  permutation(NSubSet, SubSet).

refine_bot(nil, Clause) :-
    bottom(BottomClause),
    BottomClause = (Head:-Body),
    has_pieces(Body, BottomClauseList),
    unordered_subset(BottomClauseList, SelectedLits),
    has_pieces(Body2, SelectedLits),
    Clause = (Head :- Body2),
    connect_thread(Clause).


% arg places
argp(Term, InputPlaces, OutputPlaces) :- 
    split_args(Term, _Mode, Inputs, Outputs, _Constants),
    thread_type(Type),
    member(InputPlaces/Type, Inputs),
    member(OutputPlaces/Type, Outputs).

inout_lit(Term, Input, Output) :-
    functor(Term, _, Arity),
    once(argp(Term, [InArg], [OutArg])),
    arg(InArg,  Term, Input),
    arg(OutArg, Term, Output).

inout_thread(','(A, B), Input, Output) :-
    inout_lit(A, Input, FirstOut),
    inout_thread(B, FirstOut, Output), !.

inout_thread(Atom, Input, Output) :- inout_lit(Atom, Input, Output), !.

% has_pieces(+Body, -AtomList).
% returns the body as a list of atoms
has_pieces(','(A, R), [A|Z]) :- Z \== [], has_pieces(R, Z), !.
has_pieces(A, [A]) :- A \== true, !.
has_pieces(true, []) :- !.

% concat_thread(+List, -Body)
% concat_thread: concat a list of literals as a thread.
concat_thread([A], A) :- !.
concat_thread([A|Z], ','(A, Z1)) :- 
    concat_thread(Z, Z1),
    inout_lit(A, _, Out),
    inout_thread(Z1, Out, _).

% count_literals(+Literals, -Count)
count_literals(Lits, Count) :-
    has_pieces(Lits, LitList), 
    length(LitList, Count).

% connect_thread(+Clause)
% connect_thread: Connect the head variables with the body variables.
connect_thread((Head:-Body)) :-
    inout_lit(Head, Input, Output),
    inout_thread(Body, Input, Output), !.

% append_thread(+Literal, +Body, -BodyWithLit)
% append and connect the Literal to the Body resulting to BodyWithLit.
append_thread(Lit, Body, BodyWith) :-
    has_pieces(Body, Atoms),
    append(Atoms, [Lit], Atoms2),
    concat_thread(Atoms2, BodyWith), !.
