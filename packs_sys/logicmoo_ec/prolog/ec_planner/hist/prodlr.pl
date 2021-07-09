%% fuzzy reasoning front-end for Aleph
%% 
%% Author: Angelos Charalambidis <acharal@users.sourceforge.net>
%%         Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%% Created: 22 Aug 2006
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

:- module(prodlr, [declare_concept/1, declare_concept/2, add_to_concept/2,
                   declare_relation/3, add_to_relation/3,
                   concept_select/3, forall_select/4, atleast_select/5, atmost_select/5, self_select/3,
                   relation_path/3, relation_path/1, concept_name/1, concept_name_or_not/1, num/1,
                   concept/3,
                   legitimate_literal/1, thread_itemfunctor/1]).

:- assert_if_new(user:use_algebra(alg_lukasiewicz)).

:- use_module( fuzzyutils ).

:- use_module( library(lists) ).

:- dynamic concept_name/1,  concept_assert/3,  concept_sub/3, concept_sub/2.
:- dynamic relation_name/3, relation_assert/4, relation_sub/4.

%prolog_engine(swi) :- current_prolog_flag(argv, [pl|_]).
%prolog_engine(yap) :- predicate_property(yap_flag(_,_), built_in).

init_engine(swi) :-!.
%	assert_if_new((remove_duplicates([], []))),
%	assert_if_new((remove_duplicates([H|X], [H|Y]) :- delete(X, H, Z), remove_duplicates(Z,Y))).

init_engine(yap) :-
	use_module( library(dialect/swi) ).


%:- prolog_engine(Engine), init_engine(Engine).
:- init_engine(swi).

% concept_name(?concept)
% predicate returns all the declared concepts.
concept_name(thing).

%declare_instance(Instance) :-
%	atom(Instance),
%	assert_if_new(instance(Instance)).

declare_concept(Concept, Super) :-
	atom(Concept),
	atom(Super),
	assert_if_new(concept_name(Concept)),
	assert_if_new(concept_sub(Concept,Super)),
	assert_if_new((concept_sub(Super, I, Deg) :- concept(Concept, I, Deg))).

declare_concept(Concept) :- declare_concept(Concept, thing).

% db_record an assertion Concept(Instance) >= Deg
% if there is already an assertion about Instance, we keep the one with the greater degree.
% i don't think that's necessary.
assert_instance(Concept, Instance, Deg) :-
	atom(Concept), atom(Instance),
	concept_name(Concept), is_fuzzy_degree(Deg),
	% instance(Instance),
	retractall(concept_assert(Concept, Instance, _)),
	asserta(concept_assert(Concept, Instance, Deg)).


% also known as: assert_many_instances_to_concept predicate
add_to_concept(Concept, []) :- concept_name(Concept).

add_to_concept(Concept, [(Instance, Deg) | Rest]) :-
	!,
	assert_instance(Concept, Instance, Deg),
	add_to_concept(Concept, Rest),
	!.

add_to_concept(Concept, [Instance | Rest]) :-
	add_to_concept(Concept, [(Instance, 1.0) | Rest]).


% unused because intersection of concepts can be implemented using concept_selects
%concept_intersect(C, C, C).
%concept_intersect(C1, C2, intersect(C1, C2)).

% unused because union of concepts can be implement using multiple clauses.
%concept_union(C, C, C).
%concept_union(C1, C2, union(C1, C2)).

% complementary will be implemented directly to concept_select.
concept_complement(compl(C), C) :- !.
concept_complement(C, compl(C)).

% concept_degrees(#Concept, #Instance, ?Degrees).
concept_degrees(Concept, Instance, Degrees) :-
	findall(D, concept_assert(Concept, Instance, D), DegAsserted),
	findall(D, concept_sub(Concept, Instance, D), DegSubsumed),
	append(DegAsserted, DegSubsumed, Degrees).

is_instance_of_concept(Concept, Instance) :-
	concept_assert(Concept, Instance, _).
is_instance_of_concept(Concept, Instance) :-
	concept_sub(Concept, Instance, _).

% concept_degree(#Concept, #Instance, ?Deg)
concept_degree(Concept, Instance, Deg) :-
	atom(Concept), atom(Instance),
	(Concept = thing ->
		equally_fuzzy(Deg, 1.0) ;
		concept_degrees(Concept, Instance, Degrees),
		(Degrees = [] -> equally_fuzzy(Deg, 0.0) ; tnorms(disjunction, Degrees, Deg))).

% concept(#Concept, ?Instance, ?Deg).
concept(Concept, Instance, Deg) :-
	concept_degree(Concept, Instance, Deg).

concept(Concept, Instance, Deg) :-
	atom(Concept), var(Instance),
	is_instance_of_concept(Concept, Instance),
	concept_degree(Concept, Instance, Deg).

%concept(intersect(C1, C2), I, Deg) :-
%	concept(C1, I, Deg1),
%	concept(C2, I, Deg2),
%	tnorm( conjunction, Deg1, Deg2, Deg ).

%concept(union(C1, C2), I, Deg) :-
%	concept(C1, I, Deg1),
%	concept(C2, I, Deg2),
%	tnorm( disjunction, Deg1, Deg2, Deg ).

concept(compl(C), I, Deg) :-
	concept_name(C),	% restrict infinite construction of compl(compl(compl(...)))
	concept(C, I, Deg1),
	tnorm( complement, Deg1, Deg).

concept_name_or_not(C) :- concept_name(C).
concept_name_or_not(compl(C)) :- concept_name(C).

concept_subsumed(C,C) :- !.

concept_subsumed(C1, C2) :-
	concept_name_or_not(C1),
	concept_name_or_not(C2),
	findall( (I1, D1), concept(C1, I1, D1), C1List ),
	set_subsumed( C1List, C2 ).

set_subsumed( [], _ ).
set_subsumed( [(I,D)|Rest], C) :-
	concept(C, I, D1), !,
	tnorm( implication, D, D1, Result),
	equally_fuzzy( Result, 1.0 ),
	set_subsumed( Rest, C ).

relation_name(uni, thing, thing).
relation_name(Relation) :- relation_name(Relation, _, _).

declare_relation(Relation, Domain, Range) :- declare_relation(Relation, uni, Domain, Range).
declare_relation(Relation, Super, Domain, Range) :-
	atom(Relation), atom(Super),
	atom(Domain), atom(Range),
	assert_if_new(relation_name(Relation, Domain, Range)),
	assert_if_new((relation_sub(Super, X, Y, Deg) :- relation(Relation, X, Y, Deg))).


assert_role(Relation, Inst1, Inst2, Deg) :-
	atom(Relation), atom(Inst1), atom(Inst2),
	%instance(Inst1), instance(Inst2),
	relation_name(Relation),
	%concept(Domain, Inst1, _),
	%concept(Range, Inst2, _),
	%!,
	retractall(relation_assert(Relation, Inst1, Inst2, _)),
	asserta(relation_assert(Relation, Inst1, Inst2, Deg)).

% add_to_relation(+Relation, +X, +Ys)
add_to_relation(Relation, X, []) :- atom(Relation), atom(X).
add_to_relation(Relation, X, [(Y, Deg) | R]) :-
	assert_role(Relation, X, Y, Deg),
	add_to_relation(Relation, X, R).

relation_degrees(Rel, X, Y, Degrees) :-
	atom(Rel), atom(X), atom(Y), 
	findall(D, relation_assert(Rel, X, Y, D), DegAsserted),
	findall(D, relation_sub(Rel, X, Y, D), DegSubsumed),
	append(DegAsserted, DegSubsumed, Degrees).

relation_degree(Rel, X, Y, Deg) :-
	atom(Rel), atom(X), atom(Y), 
	(Rel = uni -> 
		equally_fuzzy(Deg, 1.0) ;
		relation_degrees(Rel, X, Y, Degrees),
		(Degrees = [] -> equally_fuzzy(Deg, 0.0); tnorms( disjunction, Degrees, Deg))).

is_relation_instance(Rel, X, Y) :-
	relation_assert(Rel, X, Y, _).
is_relation_instance(Rel, X, Y) :-
	relation_sub(Rel, X, Y, _).

relation(Rel, X, Y, Degree) :-
	relation_degree(Rel, X, Y, Degree).

relation(Rel, X, Y, Degree) :-
	atom(Rel), (var(X) | var(Y)), !, 
	is_relation_instance(Rel, X, Y),
	relation_degree(Rel, X, Y, Degree).

relation_composition([Relation], Inst1, Inst2, Deg) :-
	relation_name(Relation),
	relation(Relation, Inst1, Inst2, Deg).

relation_composition([Rel1, Rel2 | Rest], Inst1, Inst2, Deg) :-
	relation_path([Rel1, Rel2 | Rest], Inst1, Inst2, _), % in case that Inst1 and/or Inst2 are not bounded
	%concept(thing, Inst1, _), concept(thing, Inst2, _),
	findall(DegOut, relation_path([Rel1, Rel2|Rest], Inst1, Inst2, DegOut), Degrees),
	(Degrees = [] -> equally_fuzzy(Deg, 0.0) ; sup_degree(Degrees, Deg)).

relation_path([Rel], Inst1, Inst2, DegOut) :- relation_name(Rel), relation(Rel, Inst1, Inst2, DegOut).
relation_path([Rel1, Rel2 | Rest], Inst1, Inst2, DegOut):-
	relation_name(Rel1), relation_name(Rel2),
	relation(Rel1, Inst1, Y, Deg2),
	relation_composition([Rel2 | Rest], Y, Inst2, Deg3),
	tnorm( conjunction, Deg2, Deg3, DegOut ).


relation_path_arb([Rel], Domain, Range) :-
	relation_name(Rel, Domain, Range).

relation_path_arb([Rel1, Rel2 |Rest], Domain, Range) :-
	relation_name(Rel1, Domain, _),
	relation_name(Rel2, Domain2, _),
	relation_range([Rel1], Domain2),
	relation_path_arb([Rel2|Rest], Domain2, Range).

relation_path(Path, Domain, Range) :-
	var(Path), !,
	between(1, 4, N),
	length(Path, N),
	relation_path_arb(Path, Domain, Range),
	\+ member(uni, Path).

relation_path(Path, Domain, Range) :-
	relation_path_arb(Path, Domain, Range).


relation_range(RelationPath, Range) :-
	relation_path(RelationPath, _, Range).

relation_range(RelationPath, Range) :-
	relation_path(RelationPath, _, Range2),
	%concept_subsumed(Range2, Range).
	concept_sub(Range2, Range).

relation_path(Path) :- relation_path(Path, _, _).

%concept_select( ?InList, ?Concept, -OutList )
%

% concept_select( +In, ?Concept, +Complement, -Out).
concept_select( InList, Concept, OutList ) :-
	concept_name_or_not(Concept),
	concept_select_rec(InList, Concept, OutList).

concept_select_rec( [], _, [] ).
concept_select_rec( [(Instance, InDeg)|InList], Concept, [(Instance, OutDeg) | OutList] ) :-
	concept(Concept, Instance, Deg),
	tnorm( conjunction, InDeg, Deg, OutDeg1 ),
	(var(OutDeg) -> OutDeg = OutDeg1 ; check_less_fuzzy(OutDeg1, OutDeg)),
	concept_select_rec( InList, Concept, OutList ).


%% forall_select/4 ( +DomainIn, +Relation, +Range, -DomainOut )
%%
%% All elements of DomainIn that (a) have Relation with an instance 
%% of Range, or (b) don't have Relation, are copied to DomainOut

forall_select([], _, _, []).
forall_select([(A, D) | InList], Relation, Concept, [(A, OutDeg) | OutList]) :-
	relation_path(Relation),
	concept_name_or_not(Concept),
	forall(A, Relation, Concept, DegF),
	tnorm(conjunction, D, DegF, OutDeg1),
	(var(OutDeg) -> OutDeg = OutDeg1 ; check_less_fuzzy(OutDeg1, OutDeg)),
	forall_select(InList, Relation, Concept, OutList).
%forall_select(In, Relation, Concept, Out)

forall_each(A, Relation, Concept, Deg) :-
	relation_composition(Relation, A, B, DegRel),
	concept(Concept, B, DegC),
	tnorm(implication, DegRel, DegC, Deg).

forall(A, Relation, Concept, Deg) :-
	atom(A), !,
	findall(D, forall_each(A, Relation, Concept, D), Degrees),
	(Degrees = [] -> Deg = 0.0; inf_degree(Degrees, Deg)).

forall(A, Relation, Concept, Deg) :-
	var(A), !,
	concept(thing, A, _),
	forall(A, Relation, Concept, Deg).


% every P-ary subset of Degrees are conjuncted resulting Deg.
% TODO: give more inspired name to predicate
atleast_once(Degrees, P, Deg) :-
	length(ParyDegrees, P),
	comb(Degrees, ParyDegrees),
	tnorms(weakconjunction, ParyDegrees, Deg).

exists_each(A, B, Relation, Concept, Deg) :-
	relation_composition(Relation, A, B, DegRel),
	concept(Concept, B, DegC),
	tnorm(conjunction, DegRel, DegC, Deg).

exists_many(A, Relation, Concept, Degrees, Many) :-
	atom(A), !,
	findall((B,D), exists_each(A, B, Relation, Concept, D), OutList),
	% OutSet contains only distinct role fillers
	remove_duplicates(OutList, OutSet),
	unzip(OutSet, _, Degrees1),
	%remove_zeros(Degrees1, Degrees),
	Degrees = Degrees1,
	length(Degrees, Many),
	!.

exists_many(A, Relation, Concept, Degrees, Many) :-
	var(A), !,
	concept(thing, A, _),
	exists_many(A, Relation, Concept, Degrees, Many).

% atleast_select(+InList, ?Relation, ?Concept, +Min, -OutList)
% ASSUMPTION: if N < Min then the Deg is 0.0
atleast_select([], _, _, _, []).
atleast_select([(A, InDeg) | Rest], Relation, Concept, Min, [(A, OutDeg) | OutList]) :-
	integer(Min), !,
	relation_path(Relation),
	concept_name_or_not(Concept),
	exists_many(A, Relation, Concept, Degrees, N),
	(N >= Min ->
		findall(Deg, (atleast_once(Degrees, Min, Deg1), tnorm(conjunction, Deg1, 1.0, Deg)), CompDegrees),
		sup_degree(CompDegrees, CompDeg)
		;
		equally_fuzzy(CompDeg, 0.0)
	),
	tnorm(conjunction, InDeg, CompDeg, OutDeg1),
	(var(OutDeg) -> OutDeg = OutDeg1 ; check_less_fuzzy(OutDeg1, OutDeg)),
	atleast_select(Rest, Relation, Concept, Min, OutList).

atleast_select(InList, Relation, Concept, Min, OutList) :-
	var(Min), !,
	relation_path(Relation),
	concept_name_or_not(Concept),
	atleast_select_MaxMin(InList, Relation, Concept, MaxMin, OutList),
	MaxMin >= 1,
	between(1, MaxMin, Min),
	atleast_select(InList, Relation, Concept, Min, OutList).

atleast_select_MaxMin(InList, Relation, Concept, MaxMin, OutList) :- 
	atleast_select_MaxMin_rec(InList, Relation, Concept, OutList, MaxMin).

atleast_select_MaxMin_rec([(Instance, InDeg)], R, C, [(Instance, OutDeg)], Max) :-
	%float(InDeg), float(OutDeg), !,
	atleast_find_max([(Instance, InDeg)], R, C, [(Instance, OutDeg)], 1, Max).

atleast_select_MaxMin_rec([(Instance, InDeg)|InList], Relation, Concept, [(Instance, OutDeg)|OutList], MaxMin) :-
	%float(InDeg), float(OutDeg), !,
	atleast_find_max([(Instance, InDeg)], Relation, Concept, [(Instance, OutDeg)], 1, N),
	atleast_select_MaxMin_rec(InList, Relation, Concept, OutList, MaxMin2),
	( N >= MaxMin2 -> MaxMin = MaxMin2 ; MaxMin = N ).

atleast_find_max([(Instance, _)], Relation, Concept, [(Instance, OutDeg)], _, N) :-
	var(OutDeg), !,
	exists_many(Instance, Relation, Concept, _, N).

atleast_find_max([(Instance,InDeg)], Relation, Concept, [(Instance, _)], K, K2) :-
	K2 is K + 1,
	atleast_select( [(Instance,InDeg)], Relation, Concept, K2, [(Instance, OutDeg2)]),
	equally_fuzzy(OutDeg2,0.0),
	!.

atleast_find_max(InList, Relation, Concept, OutList, K, Max) :-
	K2 is K + 1,
	atleast_find_max(InList, Relation, Concept, OutList, K2, Max).


% atmost_select(+InList, ?Relation, ?Concept, +Max, -OutList) :-

atmost_select([], _, _,_, []).

atmost_select([(A, InDeg) | Rest], Relation, Concept, Max, [(A, OutDeg) | OutList]) :-
	integer(Max), !,
	relation_path(Relation),
	concept_name_or_not(Concept),
	exists_many(A, Relation, Concept, Degrees, N),
	MaxPlusOne is Max + 1,
	findall(Deg,
		(atleast_once(Degrees, MaxPlusOne, Deg1), tnorm(implication, Deg1, 0.0, Deg)),
		CompDegrees),
	inf_degree(CompDegrees, CompDeg),
	tnorm(conjunction, InDeg, CompDeg, OutDeg1),
	(var(OutDeg) -> OutDeg = OutDeg1 ; check_less_fuzzy(OutDeg1, OutDeg)),
	atmost_select(Rest, Relation, Concept, Max, OutList).

atmost_select(InList, Relation, Concept, Max, OutList) :-
	var(Max), !,
	relation_path(Relation),
	concept_name_or_not(Concept),
	atmost_select_MaxMin(InList, Relation, Concept, MaxMin, OutList),
	MaxMin >= 0,
	between(0, MaxMin, Max),
	atmost_select(InList, Relation, Concept, Max, OutList).

atmost_select_MaxMin(InList, Relation, Concept, MaxMin, OutList) :- 
	atmost_select_MaxMin_rec(InList, Relation, Concept, OutList, MaxMin).

atmost_select_MaxMin_rec([(Instance, InDeg)], R, C, [(Instance, OutDeg)], Max) :-
	%float(InDeg), float(OutDeg), !,
	atmost_find_max([(Instance, InDeg)], R, C, [(Instance, OutDeg)], 1, Max).

atmost_select_MaxMin_rec([(Instance, InDeg)|InList], Relation, Concept, [(Instance, OutDeg)|OutList], MaxMin) :-
	%float(InDeg), float(OutDeg), !,
	atmost_find_max([(Instance, InDeg)], Relation, Concept, [(Instance, OutDeg)], 1, N),
	atmost_select_MaxMin_rec(InList, Relation, Concept, OutList, MaxMin2),
	( N >= MaxMin2 -> MaxMin = N ; MaxMin = MaxMin2 ).

atmost_find_max([(Instance, _)], Relation, Concept, [(Instance, OutDeg)], _, N) :-
	var(OutDeg), !,
	exists_many(Instance, Relation, Concept, _, N).

atmost_find_max([(Instance, InDeg)], Relation, Concept, [(Instance, _)], K, K2) :-
	K2 is K + 1,
	atmost_select( [(Instance, InDeg)], Relation, Concept, K2, [(Instance, OutDeg2)]),
	equally_fuzzy(InDeg, OutDeg2),
	!.

atmost_find_max(InList, Relation, Concept, OutList, K, Max) :-
	K2 is K + 1,
	atmost_find_max(InList, Relation, Concept, OutList, K2, Max).


% self_select( +InList, ?RelationName, -OutList )

self_select( InList, Rel, OutList ) :-
	relation_path( Rel ),
	self_select_rec( InList, Rel, OutList ).
	
self_select_rec( [], _, [] ).

self_select_rec( [(Instance, InDeg)|InList], Rel, [(Instance, OutDeg) | OutList] ) :-
	relation_composition(Rel, Instance, Instance, Deg),
	tnorm( conjunction, InDeg, Deg, OutDeg1 ),
	(var(OutDeg) -> OutDeg = OutDeg1 ; check_less_fuzzy(OutDeg1, OutDeg)),
	self_select_rec( InList, Rel, OutList ).



%%%
%%% utilities
%%%

unzip([], [], []).
unzip([(A, B) | R], [A | RA], [B | RB]) :- unzip(R, RA, RB).


comb([], []).
comb([X|A], [X|B]) :- comb(A, B).
comb([_|A], B) :- comb(A, B).


remove_zeros([],[]).
remove_zeros([Degree|Rest], Rest2) :-
	check_equally_fuzzy(Degree, 0.0),
	remove_zeros(Rest, Rest2), !.
remove_zeros([Degree|Rest], [Degree|Rest2]):-
	remove_zeros(Rest, Rest2).

num(0).
num(1).
num(2).
num(3).
num(4).
num(5).

thread_itemfunctor(concept_select/3).
thread_itemfunctor(forall_select/4).
thread_itemfunctor(atleast_select/5).
thread_itemfunctor(atmost_select/5).
thread_itemfunctor(self_select/3).

inout_lit(Term, Input, Output) :-
    Term =.. List,
    length(List, N),
    N >= 3,
    List = [ _, Input | _ ],
    last(List, Output).

inout_thread(','(A, B), Input, Output) :-
    inout_lit(A, Input, FirstOut),
    inout_thread(B, FirstOut, Output), !.

inout_thread(Atom, Input, Output) :- inout_lit(Atom, Input, Output), !.

% has_pieces(+Body, -AtomList).
% returns the body as a list of atoms
has_pieces(true, []) :- !.
has_pieces(','(A, R), [A|Z]) :- has_pieces(R, Z), !.
has_pieces(A, [A]) :- !.

% concat_thread(+List, -Body)
% concat_thread: concat a list of literals as a thread.
concat_thread([A], A) :- !.
concat_thread([A|Z], ','(A, Z1)) :- 
    concat_thread(Z, Z1),
    inout_lit(A, _, Out),
    inout_thread(Z1, Out, _).

% count_literals(+Literals, -Count)
count_literals(Lits, Count) :- has_pieces(Lits, LitList), length(LitList, Count).

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

concept_r(C, C).
concept_r(C, R) :- concept_sub(C, R).

% legitimate_literal(?Lit)
% legitimate_literal: generates or validates if the literal is legidimate.
% In other words, if the concepts and the relation path arguments are filled
% with a value that make sense.
legitimate_literal(concept_select(_,C,_)) :- concept_name_or_not(C).

legitimate_literal(forall_select(_, R, C, _)) :- relation_path(R, _, Range), concept_r(C, Range).

legitimate_literal(atleast_select(_, R, C, N, _)) :- num(N), relation_path(R, _, Range), concept_r(C, Range).

legitimate_literal(atmost_select(_, R, C, N, _)) :- num(N), relation_path(R, _, Range), concept_r(C, Range).

legitimate_literal(self_select(_, R, B)) :- relation_path(R).

%%%
%%%  yadlr front-end
%%%

yadlr_init( KB ) :- 
	retractKB( KB ).

% TODO: check if it already exists, and fail
yadlr_concept( KB, ConceptName ) :-
	declare_concept(ConceptName),
	db_recordz( KB, yconcept(ConceptName), _ ).

yadlr_relation( KB, RelationName ) :-
	declare_relation(RelationName, thing, thing),
	db_recordz( KB, yrelation(RelationName), _ ).

yadlr_instance( KB, InstanceName ) :-
	db_recordz( KB, yinstance(InstanceName), _ ).

yadlr_concept_name( KB, ConceptName ) :-
	db_recorded( KB, yconcept(ConceptName), _ ).

yadlr_relation_name( KB, RelationName ) :-
	db_recorded( KB, yrelation(RelationName), _ ).

yadlr_instance_name( KB, InstanceName ) :-
	db_recorded( KB, yinstance(InstanceName), _ ).

yadlr_assert( KB, Formula, FuzzyDegree ) :-
	clausify_abstract( Formula, FuzzyDegree, Clauses ),
	yadlr_assert_many( KB, Clauses ).

% place grounds facts at the top
/*
yadlr_assert_one( KB, Clause ) :-
	yadlr_clause_body( Clause, [] ),
	!,
	db_recorda( KB, Clause, _ ).
yadlr_assert_one( KB, Clause ) :-
	db_recordz( KB, Clause, _ ).
*/

yadlr_assert_one(KB, dla(concept, Assertion, Degree)) :-
	is_concept(Assertion, Concept, Instance),
	yadlr_concept_name(KB, Concept),
	%yadlr_instance_name(KB, Instance),
	!,
	add_to_concept(Concept, [(Instance, Degree)]).

yadlr_assert_one(KB, dla(relation, Assertion, Degree)) :-
	is_relation(Assertion, Relation, X, Y),
	yadlr_relation_name(KB, Relation),
	%yadlr_instance_name(KB, X),
	%yadlr_instance_name(KB, Y),
	!,
	add_to_relation(Relation, X, [(Y, Degree)]).

yadlr_assert_one(_, DLClause) :-
	mkplclause(DLClause, Clause),
	assert_if_new(Clause),
	!.

yadlr_assert_many( _, []).
yadlr_assert_many( KB, [Head|Rest] ) :-
	yadlr_assert_one(KB, Head),
	yadlr_assert_many(KB, Rest).

retractKB( KB ) :-
	prolog_engine(swi),
	db_recorded( KB, _, Ref ),
	erase( Ref ),
	fail.
retractKB( _ ) :-
	prolog_engine(swi),
	!.
%retractKB( KB ) :-
%	eraseall( KB ).

yadlr_retrieve_concept( KB, ConceptName ) :-
	db_recorded( KB, yconcept(ConceptName), _ ).

yadlr_retrieve_relation( KB, RelationName ) :-
	db_recorded( KB, yrelation(RelationName), _ ).

yadlr_retrieve_instance( KB, InstanceName ) :-
	db_recorded( KB, yinstance(InstanceName), _ ).

% try to transform some not so arbitrary formula into acceptable horn clauses.

clausify_abstract(Formula, FuzzyDegree, Clauses ) :-
	clausify_concept(Formula, FuzzyDegree, Clauses).

clausify_abstract(Formula, FuzzyDegree, Clauses ) :-
	clausify_role(Formula, FuzzyDegree, Clauses).

clausify_abstract(Formula, FuzzyDegree, Clauses ) :-
	clausify_assertion(Formula, FuzzyDegree, Clauses).

clausify_concept(all(X, dlimplies(Body, Head)), FuzzyDegree, Clauses) :-
	is_concept(Head, H, X),
	norm_concept(X, Body, DLBody),
	dlclausify(concept, (H :- DLBody), FuzzyDegree, Clauses).

clausify_concept(all(X, dlequiv(B1, B2)), FuzzyDegree, Clauses) :-
	is_concept(B2, _, _) ->
		clausify_concept(all(X, dlimplies(B1, B2)), FuzzyDegree, Clauses);
		clausify_concept(all(X, dlimplies(B2, B1)), FuzzyDegree, Clauses).

clausify_role(all(X, all(Y, dlimplies(Body, Head))), FuzzyDegree, Clauses) :-
	is_relation(Head, R, X, Y),
	norm_relation(X, Y, Body, DLBody),
	dlclausify(relation, (R :- DLBody), FuzzyDegree, Clauses).

clausify_assertion(Fmt, FuzzyDegree, [dla(concept, Fmt, FuzzyDegree)]) :-
	is_concept(Fmt, C, X),
	atom(C), atom(X), !.

clausify_assertion(Fmt, FuzzyDegree, [dla(relation, Fmt, FuzzyDegree)]) :-
	is_relation(Fmt, R, X, Y),
	atom(R), atom(X), atom(Y),
	!.

dlclausify(T, (H :- DLBody), Degree, Clauses) :-
	dlnnf(DLBody, NNF),
	dldnf(NNF, DNF),
	dlor_to_list(DNF, ListOfBodies),
	dlclausify_attach(T, H, ListOfBodies, Degree, Clauses).

%dlclausify_attach(H, [],    Deg, [dlc(H,[],Deg)]).
dlclausify_attach(T, H, [B],   Deg, [dlc(T, H, B, Deg)]).
dlclausify_attach(T, H, [B|R], Deg, [dlc(T, H, B, Deg)|R0]) :- dlclausify_attach(T, H, R, Deg, R0).


dlor_to_list(dlor(A, B), List)  :- !,
	dlor_to_list(A, LA),
	dlor_to_list(B, LB),
	append(LA, LB, List).
dlor_to_list(X, [X]).

is_concept(Fmt, C, X) :-
	functor(Fmt, C, 1),
	arg(1, Fmt, X).

is_relation(R, Relation, X, Y) :- !,
	functor(R, Relation, 2),
	arg(1, R, X),
	arg(2, R, Y).

%makes a prolog clause that can be asserted
mkplclause(dlc(concept, Head, Body, _), (PlHead:-PlBody)) :-
	mkplhead(Head, (I,D), PlHead),
	mkplbody(Body, [(I,1.0)], [(I,D)], PlBody).

mkplhead(Head, (I,D), concept_sub(Head,I,D)).
mkplbody(dland(A, B), Input, Output, ','(P,Q)) :-
	mkplbody(A, Input, Pipe, P),
	mkplbody(B, Pipe,  Output, Q).
mkplbody(dlconcept(C),      I, O, concept_select(I, C0, O))        :- mkplconcept(dlconcept(C), C0).
mkplbody(dlnot(C),          I, O, concept_select(I, C0, O))        :- mkplconcept(dlnot(C), C0).
mkplbody(forall(Rp, C),     I, O, forall_select(I, Rp, C0, O))     :- mkplconcept(C, C0).
mkplbody(exists(Rp, C),     I, O, atleast_select(I, Rp, C0, 1, O)) :- mkplconcept(C, C0).
mkplbody(atleast(N, Rp, C), I, O, atleast_select(I, Rp, C0, N, O)) :- mkplconcept(C, C0).
mkplbody(atmost(N, Rp, C),  I, O, atmost_select(I, Rp, C0, N, O))  :- mkplconcept(C, C0).

mkplconcept(dlconcept(C), C).
mkplconcept(dlnot(dlconcept(C)), compl(C)).


% norm(+Formula, -NormFormula)
% Normalize the formula into a more managable form
% Syntax:
% NC = dlforall([R1, R2, ...], C)
%    | dlexists([R1, ...], C)
%    | dlatleast([R1, ..], N, C)
%    | dlatmost([R1, ..], N, C)
%    | dlcompl(C)
%    | C
%  C = dlconcept(NC)
%  NC1 = NC and NC1
%  NC2 = NC1 or NC2


%normalization of concept and relations
 
norm_concept(X, all(Y, dlimplies(R, F)), all(Rel, NC)) :- !,
	is_relation(R, Rel, X, Y),
	norm_concept(Y, F, NC),
	(Nrm = all(R1, NC1) -> Rel = [R0|R1], NC = NC1 ;
                               Rel = [R0], NC = Nrm).

norm_concept(X, exists(Y, dland(R, F)), exists(Rel, NC)) :- !,
	is_relation(R, R0, X, Y),
	norm_concept(Y, F, Nrm),
	(Nrm = exists(R1, NC1) -> Rel = [R0|R1], NC = NC1 ;
                                  Rel = [R0], NC = Nrm).

norm_concept(X, exists(Y, Fmt), atleast(N, [Relation], Concept)) :- !,
	collectexists(exists(Y, Fmt), [], Fmt1, Vars),
	length(Vars, N),
	groupN(X, Vars, Fmt1, dland(R, C)),
	is_relation(R, Relation, X, Y),
	norm_concept(Y, C, Concept).

norm_concept(X, dlnot(Fmt), dlnot(Fmt1)) :- norm_concept(X, Fmt, Fmt1).

norm_concept(X, Fmt, dlconcept(C)) :- is_concept(Fmt, C, X).

norm_concept(X, dland(F, G), dland(F0, G0)) :-
	norm_concept(X, F, F0), norm_concept(X, G, G0).

norm_concept(X, dlor(F, G), dlor(F0, G0)) :-
	norm_concept(X, F, F0), norm_concept(X, G, G0).


norm_relation(X, Y, dlor(F, G), dlor(F0, G0)) :- !,
	norm_relation(X, Y, F, F0),
	norm_relatin(X, Y, G, G0).

norm_relation(X, Y, dland(F, G), dland(F0, G0)) :- !,
	norm_relation(X, Y, F, F0),
	norm_relation(X, Y, G, G0).

norm_relation(X, Y, R, dlrel(Relname)) :- !,
	is_relation(R, Relname, X, Y).


%utilities for normalizing the rules

% some nasty grouping
groupN(_, [_], Fmt, Fmt) :- !.
groupN(X, Vars, Fmt, FmtGrp) :- groupN_rec(X, [], Vars, Fmt, _, FmtGrp).

groupN_rec(_, V, [], dlalldifferent(V), FmtGrp, FmtGrp).

groupN_rec(_, _, [_], Fmt, Fmt, Fmt).

groupN_rec(X, [], V, dland(Fmt, dlalldifferent(V)), FmtGrp, FmtGrp) :- !,
	groupN_rec(X, [], V, Fmt, _, FmtGrp).

groupN_rec(X, V, [Y|R], dland(Fmt, RFmt), FmtGrp, FmtGrp) :-
	groupN_rec(X, [Y|V], R, RFmt, Fmt, FmtGrp).


collectexists(exists(Y, Formula), VarsSoFar, FormulaWithoutExists, Vars) :-
	!, collectexists(Formula, [Y|VarsSoFar], FormulaWithoutExists, Vars).
collectexists(Formula, Vars, Formula, Vars).


% my variant of negation normal form implementation for description logics

dlnnf(X, Y) :- dlnnf(X, _, Y, _).

dlnnf(all(X,F),FreeV,all(X,NNF),Paths) :- !,
	dlnnf(F,[X|FreeV],NNF,Paths).

dlnnf(exists(X,Fml),FreeV,exists(X,NNF),Paths) :- !,
	dlnnf(Fml,[X|FreeV],NNF,Paths).

dlnnf(atleast(N, X,Fml),FreeV,atleast(N,X,NNF),Paths) :- !,
	dlnnf(Fml,[X|FreeV],NNF,Paths).

dlnnf(atmost(N, X,Fml),FreeV,atmost(N,X,NNF),Paths) :- !,
	dlnnf(Fml,[X|FreeV],NNF,Paths).

dlnnf(dland(A,B),FreeV,NNF,Paths) :- !,
	dlnnf(A,FreeV,NNF1,Paths1),
	dlnnf(B,FreeV,NNF2,Paths2),
	Paths is Paths1 * Paths2,
	(Paths1 > Paths2 -> NNF = dland(NNF2,NNF1);
		            NNF = dland(NNF1,NNF2)).

dlnnf(dlor(A,B),FreeV,NNF,Paths) :- !,
	dlnnf(A,FreeV,NNF1,Paths1),
	dlnnf(B,FreeV,NNF2,Paths2),
	Paths is Paths1 + Paths2,
	(Paths1 > Paths2 -> NNF = dlor(NNF2,NNF1);
		            NNF = dlor(NNF1,NNF2)).

dlnnf(Fml,FreeV,NNF,Paths) :- 
	(Fml = dlnot(dlnot(A))       -> Fml1 = A;
	 Fml = dlnot(all(X,F))       -> Fml1 = exists(X,dlnot(F));
	 Fml = dlnot(exists(X,F))    -> Fml1 = all(X,dlnot(F));
	 Fml = dlnot(atleast(N,X,F)) -> N1 is N - 1, Fml1 = atmost(N1,X,F);
	 Fml = dlnot(atmost(N,X,F))  -> N1 is N + 1, Fml1 = atleast(N1,X,F);
	 Fml = dlnot(dlor(A,B))      -> Fml1 = dland( dlnot(A), dlnot(B) );
	 Fml = dlnot(dland(A,B))     -> Fml1 = dlor( dlnot(A), dlnot(B) );
	 Fml = dlimplies(A,B)        -> Fml1 = dlor( dlnot(A), B );
	 Fml = dlnot(dlimplies(A,B)) -> Fml1 = dland( A, dlnot(B) );
	 Fml = dlequiv(A,B)          -> Fml1 = dlor( dland(A, B), dland(dlnot(A), dlnot(B)) );
	 Fml = dlnot(dlequiv(A,B))   -> Fml1 = dlor( dland(A, dlnot(B)) , dland(dlnot(A), B) )
	),!,
	dlnnf(Fml1,FreeV,NNF,Paths).

dlnnf(Lit,_,Lit,1).


% my variant of disjunctive normal form implementation for description logics
dldnf(A, B) :- dnf(A, B).

dnf( dlor(P,Q),  dlor(P1,Q1) ) :- !, dnf(P, P1), dnf(Q, Q1).
dnf( dland(P,Q), DNF) :- !, dnf(P, P1), dnf(Q, Q1), dnf1( dland(P1,Q1), DNF).
dnf(DNF,       DNF).

dnf1( dland(P, dlor(Q,R)),  dlor(P1,Q1) ):- !, dnf1( dland(P,Q), P1), dnf1( dland(P,R), Q1).
dnf1( dland( dlor(P,Q), R), dlor(P1,Q1) ):- !, dnf1( dland(P,R), P1), dnf1( dland(Q,R), Q1).
dnf1( DNF,                  DNF ).

%%% not prenex form (actually the opposite of prenex form. Push quantifiers inside conjunctions and disjunctions)
% npf(+Fmt ?NPF)

dlnpf(all(X, dland(A, B)), dland(all(X,A0), all(X,B0))) :- !, 
	dlnpf(A, A0), dlnpf(B, B0).

dlnpf(all(X, dlor(A, B)), dlor(all(X,A0), all(X,B0))) :- !,
	dlnpf(A, A0), dlnpf(B, B0).

dlnpf(exists(X, dland(A, B)), dland(exists(X, A0), exists(X, B0))) :- !, 
	dlnpf(A, A0), dlnpf(B, B0).

dlnpf(exists(X, dlor(A, B)), dlor(exists(X, A0), exists(X, B0))) :- !,
	dlnpf(A, A0), dlnpf(B, B0).

dlnpf(NPF, NPF).
