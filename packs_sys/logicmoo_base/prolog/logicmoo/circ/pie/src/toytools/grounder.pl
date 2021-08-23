%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2010, 2015 Christoph Wernhard
%%%%
%%%% This program is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% This program is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(grounder, [expand_form/2, expand_form/3, form_ground_facts/2]).
		       
:- use_module(nf(nf)).
:- use_module(swilib(pretty)).
:- use_module(swilib(err)).
:- use_module(auxiliary).

expand_form(F, G) :-
	form_ground_facts(F, GroundFacts),
	expand_form(F, GroundFacts, G).

%%%% 
%%%% expand_form(+F, +GroundFacts, -G)
%%%% 
%%%% 
%%%% F is a first-order formula. GroundFacts is a list (or conjunction) of
%%%% logic ground atoms.
%%%% 
%%%% G is a formula obtained from F by replacing first-order quantifiers
%%%% with ground instances of the quantified formulas. More precisely,
%%%% subformulas of one of the forms:
%%%%
%%%%   all(Var, (F1 -> F2))
%%%%   all(Vars, (F1 -> 2))
%%%%   ex(Var, (F1, F2))
%%%%   ex(Vars, (F1, F2))
%%%%
%%%% are replaced by the conjunction (disjunction for ex, resp.)
%%%% of all instances of F2 obtained by matching each conjunct of
%%%% F1 with a member of GroundFacts.
%%%%
%%%% Also handles some builtins...
%%%%   
expand_form(F, GroundFacts, G) :-
	( GroundFacts = [] ->
	  GroundFacts1 = GroundFacts
	; GroundFacts = [_|_] ->
	  GroundFacts1 = GroundFacts
	; conjunction_to_list(GroundFacts, GroundFacts1)
	),
	vars_to_prolog(F, G1),
	rewrite(G1, exp(GroundFacts1), G2),
	( \+ ground(G2) ->
          err('Unguarded variable in expanding ~q', [F])
	; true
	),
	rewrite(G2, builtin, G3),
	rewrite(G3, simp, G).
		
r(exp(GroundFacts), all(_, (Pre -> F)), F1) :-
	conjunction_to_list(Pre, Pre1),
	findall(F0, bind_copy(Pre1, GroundFacts, F, F0), F0s),
	list_to_conjunction(F0s, F1).

r(exp(GroundFacts), ex(_, (Pre, F)), F1) :-
	conjunction_to_list(Pre, Pre1),
	findall(F0, bind_copy(Pre1, GroundFacts, F, F0), F0s),
	list_to_disjunction(F0s, F1).

r(builtin, F, V) :-
	builtin(F),
	!,
	catch( (call(F) -> V = true ; V = false) , _E, fail ).

r(simp, (F,F), F).
r(simp, (F;F), F).
r(simp, (F,(F,H)), (F,H)).
r(simp, (F;(F;H)), (F;H)).
r(simp, (true,F), F).
r(simp, (false, _), false).
r(simp, (true;_), true).
r(simp, (false; F), F).
r(simp, (F,true), F).
r(simp, (_,false), false).
r(simp, (_;true), true).
r(simp, (F;false), F).
r(simp, ~(~F), F).
r(simp, ~true, false).
r(simp, ~false, true).
r(simp, (F<-F), true).
r(simp, (true<-_), true).
r(simp, (F<-true), F).
r(simp, (false<-F), ~F).
r(simp, (_<-false), true).
r(simp, (F->F), true).
r(simp, (true->F), F).
r(simp, (_->true), true).
r(simp, (false->_), true).
r(simp, (F->false), ~F).
r(simp, (F<->F), true).
r(simp, (true<->F), F).
r(simp, (F<->true), F).
r(simp, (false<->F), ~F).
r(simp, (F<->false), ~F).
r(simp, F, F1) :-
	conjunction_to_list(F, F2),
	sort(F2, F3),
	length(F2, L2),
	length(F3, L3),
	L3 < L2,
	list_to_conjunction(F3, F1).


bind_copy([], _, F, F).
bind_copy([Pre|Pres], GroundFacts, F, F1) :-
	builtin(Pre),
	!,
	call(Pre),
	bind_copy(Pres, GroundFacts, F, F1).
bind_copy([Pre|Pres], GroundFacts, F, F1) :-
	member(Pre, GroundFacts),
	bind_copy(Pres, GroundFacts, F, F1).

builtin(_ < _).
builtin(_ =< _).
builtin(_ > _).
builtin(_ >= _).
builtin(_ =:= _).
builtin(_ =\= _).

builtin(_ == _).
builtin(_ \== _).

%%%% 
%%%% Extract a list GroundFacts of ground facts that easily follow from a
%%%% formula F
%%%%
%%%% - GroundFacts is "correct" in the sense that each of its members is
%%%%   a consequence of F
%%%%
%%%% - But since only positive ground facts that are easy to detect
%%%%   syntactically are returned, it might be incomplete
%%%%
form_ground_facts(F, GroundFacts) :-
	vars_to_prolog(F, F1),
	conjunction_to_list(F1, L),
	( setof(A, (member(A, L), ground_fact(A)), GroundFacts) ->
	  true
	; GroundFacts = []
	).

ground_fact(F) :-
	ground(F),
	functor(F, Op, N),
	\+ logic_operator(Op,N).

logic_operator(true, 0).
logic_operator(false, 0).
logic_operator((~), 1).
logic_operator((+), 1).
logic_operator((-), 1).
logic_operator((','), 2).
logic_operator((';'), 2).
logic_operator((<-), 2).
logic_operator((->), 2).
logic_operator((<->), 2).
logic_operator(proj, 2).
logic_operator(circ, 2).
logic_operator(forg, 2).

gf([p(a),
    p(b),
    p(c),
    p(d),
    q(c),
    q(d),
    q(e)]).


% form_facts
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rewrite(F, P, F1) :-
	subexpression(F, S, F2, T),
	\+ var(S),
	r(P, S, T),
	!,
	rewrite(F2, P, F1).
rewrite(F, _, F).


