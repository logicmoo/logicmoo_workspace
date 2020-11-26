% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(condense_drs, [
			 condense_drs/2  % +DRSIn, -DRSOut
			]).

:- use_module('../op_defs').
:- use_module('../utils').
:- use_module('../list_utils').
:- use_module(group_predicates).

/** <module> DRS condenser

This module condenses and transforms the DRS in order to make the subsequent processing
simpler and faster. There are seven transformations:

1. Transformation of object-predicates.
2. Condensation of predicate declarations.
3. Condensation of of-constructs.
4. Processing of "be" as identity.
5. Transforming copula+adjective.
6. Transformation of modality constructs.

See condensation.txt for details.

@author Tobias Kuhn
@version 2008-11-24
*/


%% condense_drs(+DRSIn, -DRSOut)
%
% Transfroms DRSIn and stores the result in DRSOut.

condense_drs(drs(VarList, CondIn), drs([], CondOut)) :-
	transform_deep(CondIn, CondTemp1),
	transform(CondTemp1, VarList, CondTemp2),
	add_propername_conds(CondIn, CondTemp2, CondOut).


%% transform_deep(+CondIn, -CondOut)
%
% Accesses the deep structures of the conditions and calls transform/2 to apply
% the transformations.

transform_deep([], []).

transform_deep([Fact-X|RestIn], [Fact-X|RestOut]) :-
	transform_deep(RestIn, RestOut).

transform_deep([group(Group)|RestIn], [group(Group)|RestOut]) :-
	transform_deep(RestIn, RestOut).

transform_deep([-drs(VarList, CondIn)|RestIn], [-drs([], CondOut)|RestOut]) :-
    transform_deep(CondIn, CondTemp),  % needed for nested modality boxes
	transform(CondTemp, VarList, CondOut),
	transform_deep(RestIn, RestOut).

transform_deep([~drs([], [-drs(VarList, CondIn)])|RestIn], [~drs([], [-drs([], CondOut)])|RestOut]) :-
    !,
    transform_deep(CondIn, CondTemp),  % needed for nested modality boxes
	transform(CondTemp, VarList, CondOut),
	transform_deep(RestIn, RestOut).

transform_deep([~drs(VarList, CondIn)|RestIn], [~drs([], CondOut)|RestOut]) :-
    transform_deep(CondIn, CondTemp),  % needed for nested modality boxes
	transform(CondTemp, VarList, CondOut),
	transform_deep(RestIn, RestOut).

transform_deep([ImpNeg1|RestIn], [ImpNeg2|RestOut]) :-
	ImpNeg1 = drs(VarList1, CondIn1) => drs([], [-drs(VarList2, CondIn2)]),
	!,
	ImpNeg2 = drs([], CondOut1) => drs([], [-drs([], CondOut2)]),
	transform_deep(CondIn1, CondTemp1),  % needed for nested negation and modality boxes
	transform(CondTemp1, VarList1, CondOut1),
	transform_deep(CondIn2, CondTemp2),  % needed for nested modality boxes
	transform(CondTemp2, VarList2, CondOut2),
	transform_deep(RestIn, RestOut).

transform_deep([Imp1|RestIn], [Imp2|RestOut]) :-
	Imp1 = drs(VarList1, CondIn1) => drs(VarList2, CondIn2),
	!,
	Imp2 = drs([], CondOut1) => drs([], CondOut2),
	transform_deep(CondIn1, CondTemp1),  % needed for nested negation and modality boxes
	transform(CondTemp1, VarList1, CondOut1),
	transform_deep(CondIn2, CondTemp2),  % needed for nested modality boxes
	transform(CondTemp2, VarList2, CondOut2),
	transform_deep(RestIn, RestOut).

transform_deep([FirstIn|RestIn], [FirstOut|RestOut]) :-
	FirstIn =.. [Mod, drs(VarList, CondIn)],
	is_modal_operator(Mod),
	transform(CondIn, VarList, CondOut),
	FirstOut =.. [Mod, drs([], CondOut)],
	transform_deep(RestIn, RestOut).


%% transform(+CondIn, -CondOut)
%
% Transforms the conditions CondIn and stores the result in CondOut. Only the
% top level is processed. Deeper structures are not accessed.

transform(CondIn, VarList, CondOut) :-
	transform_objects(CondIn, CondTemp1),             % apply transformation #1
	condense_pred(CondTemp1, CondTemp2),              % apply transformation #2
	condense_of(CondTemp2, CondTemp3),                % apply transformation #3
	process_identity(CondTemp3, VarList, CondTemp4),  % apply transformation #4
	transform_be_adj(CondTemp4, CondTemp5),           % apply transformation #5
	transform_modality(CondTemp5, CondOut).           % apply transformation #6


%% transform_objects(+CondIn, -CondOut)

transform_objects(CondIn, CondOut) :-
	member(object(A,somebody,Q,na,eq,1)-X, CondIn),
	Q == countable,
	!,
	remove_exact(CondIn, object(A,somebody,countable,na,eq,1)-X, CondTemp1),
	CondTemp2 = [object(A,_,_,_,_,_)-X|CondTemp1],
	transform_objects(CondTemp2, CondOut).

transform_objects(CondIn, CondOut) :-
	member(object(A,something,Q,na,na,na)-X, CondIn),
	Q == dom,
	!,
	remove_exact(CondIn, object(A,something,dom,na,na,na)-X, CondTemp1),
	CondTemp2 = [object(A,_,_,_,_,_)-X|CondTemp1],
	transform_objects(CondTemp2, CondOut).

transform_objects(CondIn, CondOut) :-
    % (this transformation is no longer needed with the latest APE version)
	member(object(A,Name,named,_,_,_)-_, CondIn),
	var(A),
	\+ var(Name),
	!,
	A = v(Name),
	transform_objects(CondIn, CondOut).

transform_objects(Cond, Cond).


%% condense_pred(+CondIn, -CondOut)
%
% Merges the predicates that stand for predicates and their modifiers
% (transformation #3). See the introduction for further information.

condense_pred(CondIn, CondOut) :-
	member(predicate(P,Name,A)-X, CondIn),
	!,
	remove_exact(CondIn, predicate(P,Name,A)-X, CondTemp1),
	get_mods(P, CondIn, Mods),
	remove_all_exact(CondTemp1, Mods, CondTemp2),
	transform_mods(Mods, ModsT),
	CondTemp3 = [pred_mod(Name,A,ModsT)-X|CondTemp2],
	condense_pred(CondTemp3, CondOut).

condense_pred(CondIn, CondOut) :-
	member(predicate(P,Name,A,B)-X, CondIn),
	!,
	remove_exact(CondIn, predicate(P,Name,A,B)-X, CondTemp1),
	get_mods(P, CondIn, Mods),
	remove_all_exact(CondTemp1, Mods, CondTemp2),
	transform_mods(Mods, ModsT),
	CondTemp3 = [pred_mod(Name,A,B,ModsT)-X|CondTemp2],
	condense_pred(CondTemp3, CondOut).

condense_pred(CondIn, CondOut) :-
	member(predicate(P,Name,A,B,C)-X, CondIn),
	!,
	remove_exact(CondIn, predicate(P,Name,A,B,C)-X, CondTemp1),
	get_mods(P, CondIn, Mods),
	remove_all_exact(CondTemp1, Mods, CondTemp2),
	transform_mods(Mods, ModsT),
	CondTemp3 = [pred_mod(Name,A,B,C,ModsT)-X|CondTemp2],
	condense_pred(CondTemp3, CondOut).

condense_pred(Cond, Cond).


%% condense_of(+CondIn, -CondOut)
%
% Merges the object- and relation-predicate that are used for of-relations
% (transformation #4). See the introduction for further information.

condense_of(CondIn, CondOut) :-
	member(object(A1,Noun,Q,U,O,C)-X1, CondIn),
	member(relation(A2,of,B)-X2, CondIn),
	A1 == A2,
	!,
	remove_exact(CondIn, object(A1,Noun,Q,U,O,C)-X1, CondTemp1),
	remove_exact(CondTemp1, relation(A2,of,B)-X2, CondTemp2),
	CondTemp3 = [of_relation(object(A1,Noun,Q,U,O,C),B)-X2|CondTemp2],
	condense_of(CondTemp3, CondOut).

condense_of(Cond, Cond).


%% process_identity(+CondIn, +VarList, -CondOut)
%
% Processes the identity relations expressed with "be" (transformation #6). See
% the introduction for further information.

process_identity(CondIn, VarList, CondOut) :-
	member(pred_mod(be,A,B,[])-X, CondIn),
	!,
	remove_exact(CondIn, pred_mod(be,A,B,[])-X, CondTemp),
	unify_objects(VarList, A, B),
	process_identity(CondTemp, VarList, CondOut).

process_identity(Cond, _, Cond).


%% transform_be_adj(+CondIn, -CondOut)

transform_be_adj(CondIn, CondOut) :-
	member(pred_mod(be,A,B,Mods)-X1, CondIn),
	member(property(B,Adj,D)-X2, CondIn),
	\+ (
		member(object(O,_,_,_,_,_)-_, CondIn),
		O == B
	),
	!,
	remove_exact(CondIn, pred_mod(be,A,B,Mods)-X1, CondTemp1),
	remove_exact(CondTemp1, property(B,Adj,D)-X2, CondTemp2),
	CondTemp3 = [be_adj(A,Adj,D,Mods)-X1|CondTemp2],
	transform_be_adj(CondTemp3, CondOut).

transform_be_adj(Cond, Cond).


unify_objects(VarList, A, B) :-
    \+ is_exact_member(A, VarList),
    \+ is_exact_member(B, VarList),
    !,
    throw(ar_error('parser.condense-drs.InvalidIdentity', 'Invalid Identity.')).

unify_objects(_, X, X) :-
    !.

unify_objects(_, _, _) :-
    throw(ar_error('parser.condense-drs.InvalidIdentity', 'Invalid Identity.')).


%% transform_modality(+CondIn, -CondOut)
%
% Transforms the modality boxes (can, must, should and may).

transform_modality(CondIn, CondOut) :-
	member(In, CondIn),
	In =.. [Mod, drs(_,C)],
	is_modal_operator(Mod),
	!,
	remove_exact(CondIn, In, CondTemp1),
	remove_sentence_nr(C, CN),
	Out =.. [Mod, CN],
	CondTemp2 = [Out-0/0|CondTemp1],
	transform_modality(CondTemp2, CondOut).

transform_modality(Cond, Cond).


%% get_mods(-Var, +Conds, -Mods)
%
% Returns a list of all modifier-statements in the list Conds that refer to the
% predicate with the discourse referent Var.

get_mods(Var, Conds, Mods) :-
    get_mods_x(Var, Conds, ModsR),
    reverse(ModsR, Mods).  % APE returns the modifier in reversed order

get_mods_x(_Var, [], []).

get_mods_x(Var, [modifier_adv(R,Adv,D)-X|RestIn], [modifier_adv(R,Adv,D)-X|RestOut]) :-
	Var == R,
	!,
	get_mods_x(Var, RestIn, RestOut).

get_mods_x(Var, [modifier_pp(R1,Prep,R2)-X|RestIn], [modifier_pp(R1,Prep,R2)-X|RestOut]) :-
	Var == R1,
	!,
	get_mods_x(Var, RestIn, RestOut).

get_mods_x(Var, [_|RestIn], Out) :-
	get_mods_x(Var, RestIn, Out).


%% transform_mods(+ModListIn, -ModListOut)
%
% Removes the first argument of the modifier-statements. This argument stands
% for the discourse referent of the corresponding predicate. It is not needed
% anymore since we merge the predicates.

transform_mods([], []).

transform_mods([modifier_adv(_,Adv,D)-_|RestIn], [modifier_adv(Adv,D)|RestOut]) :-
	transform_mods(RestIn, RestOut).

transform_mods([modifier_pp(_,Prep,R2)-_|RestIn], [modifier_pp(Prep,R2)|RestOut]) :-
	transform_mods(RestIn, RestOut).


%% add_propername_conds(+Term, +CondsIn, -CondsOut)

add_propername_conds(Var, Conds, Conds) :-
	var(Var),
	!.

add_propername_conds(Atom, Conds, Conds) :-
	atomic(Atom),
	!.

add_propername_conds(named(PN), Conds, Conds) :-
	atom(PN),
	member(object(named(PN),PN,named,na,eq,1)-0/0, Conds),
	!.

add_propername_conds(named(PN), Conds, [object(named(PN),PN,named,na,eq,1)-0/0|Conds]) :-
	atom(PN),
	!.

add_propername_conds([], Conds, Conds) :-
	!.

add_propername_conds([First|Rest], CondsIn, CondsOut) :-
	!,
	add_propername_conds(First, CondsIn, CondsTemp),
	add_propername_conds(Rest, CondsTemp, CondsOut).

add_propername_conds(Term, CondsIn, CondsOut) :-
	Term =.. TermList,
	add_propername_conds(TermList, CondsIn, CondsOut).
