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


% yadlr alias
:-prolog_load_context(directory,Dir),asserta(user:file_search_path(yadlr,Dir)).

% aleph alias must resolve to the directory where aleph.pl exists.
% you can download aleph from http://www.comlab.ox.ac.uk/oucl/research/areas/machlearn/Aleph/aleph.pl
user:file_search_path(aleph, X) :- getenv('LOGICMOO_HOME',MOO),atom_concat(MOO,'/src_modules/aleph/', X).


:- use_module(yadlr(prodlr)).

:- consult(aleph(aleph)).

:- consult(refine).

:- assert_if_new(('$aleph_sat_terms'(v,v,v,v) :- fail) ).
:- assert_if_new(('$aleph_sat_atom'(v,v) :- fail) ).
:- assert_if_new(('$aleph_sat_varscopy'(v,v,v):- fail) ).
:- assert((nil :- fail)).

aleph_settings :-
   set(nodes, 100000),
   set(i, 3),
   set(depth, 50),
   set(clauselength, 3),
   set(verbosity, 100),
   set(check_useless, true),
   set(refinelitgen, user), 
   set(refinethreadvar, list), 
   !.

set_determinations(Pred, []).
set_determinations(Pred, [L|R]) :- 
   determination(Pred, L), 
   set_determinations(Pred, R).

set_predicate(Pred) :-
   Arity = 2,
   functor(Head, Pred, Arity),
   (setting(construct_bottom, reduction), 
    setting(refine, user)
   ->  setting(refinethreadvar, Template),
       arg(1, Head, +Template),
       arg(2, Head, -Template)
   ;   arg(1, Head, +inlist),
       arg(2, Head, -outlist)
   ),
   modeh( *, Head),
   findall(X, thread_itemfunctor(X), Determinations),
   set_determinations(Pred/Arity, Determinations).

set_body :-
   modeb( *, concept_select(+inlist, #concept_name_or_not, +outlist) ),
   modeb( *, forall_select(+inlist, #relation_path, #concept_name_or_not, +outlist) ),
   modeb( *, atleast_select(+inlist, #relation_path, #concept_name_or_not, #num, +outlist) ),
   modeb( *, atmost_select(+inlist, #relation_path, #concept_name_or_not, #num, +outlist) ),
   modeb( *, self_select(+inlist, #relation_path, +outlist) ),
   modeb( *, concept_select(+inlist, #concept_name_or_not, -list) ),
   modeb( *, forall_select(+inlist, #relation_path, #concept_name_or_not, -list) ),
   modeb( *, atleast_select(+inlist, #relation_path, #concept_name_or_not, #num, -list) ),
   modeb( *, atmost_select(+inlist, #relation_path, #concept_name_or_not, #num, -list) ),
   modeb( *, self_select(+inlist, #relation_path, -list) ),
   modeb( *, concept_select(+list, #concept_name_or_not, +outlist) ),
   modeb( *, forall_select(+list, #relation_path, #concept_name_or_not, +outlist) ),
   modeb( *, atleast_select(+list, #relation_path, #concept_name_or_not, #num, +outlist) ),
   modeb( *, atmost_select(+list, #relation_path, #concept_name_or_not, #num, +outlist) ),
   modeb( *, self_select(+list, #relation_path, +outlist) ),
   modeb( *, concept_select(+list, #concept_name_or_not, -list) ),
   modeb( *, forall_select(+list, #relation_path, #concept_name_or_not, -list) ),
   modeb( *, atleast_select(+list, #relation_path, #concept_name_or_not, #num, -list) ),
   modeb( *, atmost_select(+list, #relation_path, #concept_name_or_not, #num, -list) ),
   modeb( *, self_select(+list, #relation_path, -list) ).


config(Pred,reduction) :- 
   set(construct_bottom, reduction),
   set(refine, auto),
   aleph_settings,
   set_predicate(Pred),
   set_body.

config(Pred,saturation) :-
   set(construct_bottom, saturation),
   aleph_settings,
   set_predicate(Pred),
   set_body.

init_learn(Pred,Example, Config) :-
   read_all(Example),
   config(Pred,Config).

learn(Pred,Example,Config) :-
   init_learn(Pred,Example, Config), !,
   %leash(off), 
   %trace,
   %sat(1). 
   induce.

learn(Pred,Example) :- learn(Pred,Example,reduction).

learn(Example) :- learn(target,Example).


nil :-
   hypothesis(Head, Body, _),
   numbervars((Head:-Body), 0, _),
   Head = target(A, B),
   \+ inout_thread(Body, A, B).

prune((Head:-Body)):-
   numbervars((Head:-Body), 0, _),
   Head = target(A, B),
   \+ inout_thread(Body, A, _), !.
   %fail.

