%% tableaux.pl  First-Order Tableau Prover
%% Implements tableaux algorithm from Smullyan's Logic Textbook
%%
%% Author: Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%% Created: 17 Jan 2006
%% Copyright (C) 2006-2007 Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
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


:- module( tableaux, [prove/5,
		      yadlr_concept/2, yadlr_relation/2, yadlr_instance/2,
		      yadlr_concept_name/2, yadlr_relation_name/2,
		      yadlr_instance_name/2,
		      yadlr_assert/3, yadlr_init/1,
		      yadlr_retrieve_concept/2, yadlr_retrieve_instance/2,
		      set_debug_msgs/1, set_depth_limit/1,
		      set_proof_tree_log/1, unset_proof_tree_log/0] ).


:- use_module(library(lists), [append/3,flatten/2]).


%%
%% Operators
%%

:-
%   op(400, fxy, all),   % for all
%   op(400, fxy, exi),   % exists
   op(400, fy,  -),    % negation
   op(500, xfy, &),   % conjunction
   op(600, xfy, v),   % disjunction
   op(650, xfy, =>),  % implication
   op(700, xfy, <=>). % equivalence


%%
%% Predicate Representation
%%

%  exists X s.t. p(a,X) is represented as pred(p,pos,a,X)
%  for all X,   ~p(a,X) is represented as pred(p,neg,a,all)

poslit( pred(pos,_,_,_) ). poslit( pred(pos,_,_) ).
neglit( pred(neg,_,_,_) ). neglit( pred(neg,_,_) ).

contradict( pred(pos,F,A),     pred(neg,F,A) ).
contradict( pred(neg,F,A),     pred(pos,F,A) ).
contradict( pred(pos,F,A1,A2), pred(neg,F,A1,A2) ).
contradict( pred(neg,F,A1,A2), pred(pos,F,A1,A2) ).


%%
%% Branch Operations
%%

% a branch is four lists of expressions,
% one with ground, atomic predicates,
% one with existentially quantified atomic predicates,
% one with other subformulas already dealt with,
% one with the TODO subformulas.

%
% get_next/3 ( +Branch, -SubF, -Branch )
%
% gets the next sub-formula that still has work to do be done,
% and returns the subformula and the Branch without the said
% subformula

get_next( branch(Ground,Exists,Rest,[SubF|TODO]), SubF,
          branch(Ground,Exists,[SubF|Rest],TODO) ).

add_next(_, [], _) :- !, fail.
add_next( branch(Ground1,Exists,Rest,TODO1), L, branch(Ground2,Exists,Rest,TODO2) ) :-
	!,
	ground_literals(L, Ground, NotGround),
	append(Ground,Ground1,Ground2),
	append(NotGround,TODO1,TODO2).

ground_literal(p, pred(pos,p) ).
ground_literal(q, pred(pos,q) ).
ground_literal(-p, pred(neg,p) ).
ground_literal(-q, pred(neg,q) ).

ground_literals([], [], []).
ground_literals([Head|Tail], Ground, Rest) :-
	ground_literals(Tail, Ground1, Rest1),
	(ground_literal(Head,Pred) -> 
	    Ground = [Pred|Ground1], Rest = Rest1
	;
	    Ground = Ground1, Rest = [Head|Rest1]
	).

expand(-(T1 <=> T2), [-(T1 => T2)],           [-(T2 => T1)] ) :- !.
expand( (T1 <=> T2), [(T1 => T2),(T2 => T1)], [] ) :- !.
expand(-(T1  => T2), [T1,-T2],                [] ) :- !.
expand( (T1  => T2), [-T1],                   [T2] ) :- !.
expand(-(T1  &  T2), [-T1],                   [-T2]) :- !.
expand( (T1  &  T2), [T1,T2],                 [] ) :- !.
expand(-(T1  v  T2), [(-T1),(-T2)],           []) :- !.
expand( (T1  v  T2), [T1],                    [T2] ) :- !.
expand( -T,          [-T],                    [] ) :- !.
expand(  T,          [T],                     [] ).


%
% recurse/2 ( +Branch, -Res )
%
% InBranch is a branch. OutBranches are brances, after one
% expansion step. NOTE: Disjunctions will output two branches from
% a single input branch.

try_one(F,_,F).
try_one(_,F,F).

recurse( branch(Ground, Exist, Rest, []), [] ) :- !,
	% DEBUG
	print(branch(Ground, Exist, Rest, [])), nl,
	print(closed), nl,
	fail.
recurse(Branch, Res) :-
	% DEBUG
	print(Branch), nl,
	get_next(Branch, SubF, Branch1),
	expand(SubF, F1, F2),
	!,
	try_one(F1, F2, F),
	add_next(Branch1, F, Branch2),
	recurse(Branch2, Res).

prove(F,Res) :- recurse( branch([],[],[],[-F]), Res1 ) -> Res=Res1 ; Res=valid.
