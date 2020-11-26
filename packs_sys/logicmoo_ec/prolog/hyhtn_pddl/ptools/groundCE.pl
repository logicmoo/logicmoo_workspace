/*
 * GIPO COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.
 *
 * Copyright 2001 - 2003 by R.M.Simpson W.Zhao T.L.McCLuskey D Liu D. Kitchin
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both the copyright notice and this permission notice and warranty
 * disclaimer appear in supporting documentation, and that the names of
 * the authors or their employers not be used in advertising or publicity 
 * pertaining to distribution of the software without specific, written 
 * prior permission.
 *
 * The authors and their employers disclaim all warranties with regard to 
 * this software, including all implied warranties of merchantability and 
 * fitness.  In no event shall the authors or their employers be liable 
 * for any special, indirect or consequential damages or any damages 
 * whatsoever resulting from loss of use, data or profits, whether in an 
 * action of contract, negligence or other tortious action, arising out of 
 * or in connection with the use or performance of this software.
 */
 
 
reverse(L,RL) :-
	revSlave(L,[],RL).

revSlave([],RL,RL).
revSlave([H|T],Sofar,Final) :-
	revSlave(T,[H|Sofar],Final).

% **********************************************************************
% Utility to produce all the ground operators from an OCL WORLDS spec
% This version adds all ground instances to the conditional
% effects section of the operator
% Assumes OCL ver 1.1
% October 1999
% Ron Simpson
% ***********************************************************************





















































%
%      TOP LEVEL
%      convert(<OCLFile>,<GroundFiler>).

:- dynamic
	opParent/2.  % Stores a numbered operator

:- dynamic
	gOperator/3. % Stores a ground operator

:- dynamic
	opCounter/1. % Used as a counter in enumerating operators


:- dynamic
        temp/1.      % Used to build conditional effect

:- op(950,xfy,=>).  % Used in OCL Specification

% ********************************************************************
% Number each operator in the OCL Spec
%
% enumerateOps


enumerateOps :-
	retractall(opCounter),
	assert(opCounter(1)),
	enumOps.

enumOps :-
	operator(Name,Prev,Nec,Cond),
	retract(opCounter(Count)),
	assert(opParent(Count,operator(Name,Prev,Nec,Cond))),
	Next is Count + 1,
	assert(opCounter(Next)),
	fail.

enumOps.


% utility to clean up asserted clauses

clean :-
	retractall(opCounter(_)),
	retractall(opParent(_,_)),
	retractall(gOperator(_,_,_)),
	retractall(temp(_)).


% *********************************************************************
% findVarsAndTypes - collect a list of all variables and their
%                    types as they occur in an operator
%                    also collect the list of "ne" constraints
%                    that apply to variables
%                    [<Type>,<Variable>|<Rest>]
%
% findVarsAndTypes(+Operator,-TypeVarList,-Nes)


findVarsAndTypes(operator(_,Pre,Nec,Cond),Vars,NEs,CondVars) :-
	vtPrevail(Pre,PreVars,PreNEs),
	vtEffects(Nec,NecVars,NecNEs),
	vtEffects(Cond,CondVars,CondNEs),
	append(PreVars,NecVars,Vars),
	append(NecNEs,CondNEs,IntNEs),
	append(PreNEs,IntNEs,NEs),
	!.

% collect all Vars and types in a changes clause
%vtEffects(+EffectsClause,-VarsTypes,-NEClauses).

vtEffects([],[],[]).

vtEffects([sc(Type,Obj1,Preds)|Rest],VT,NEs) :-
	vtPreds(Preds,Related,NEs1),
	append([Type,Obj1],Related,Obj1VT),
	vtEffects(Rest,RestVT,RestNEs),
	append(Obj1VT,RestVT,VT),
	append(NEs1,RestNEs,NEs).

% collect all Vars and types in a Prevail clause
%vtPrevail(+PrevailClause,-VarsTypes,-NEClauses).

vtPrevail([],[],[]).

vtPrevail([se(Type,Obj1,Preds)|Rest],VT,NEs) :-
	vtPLst(Preds,Related,NEs1),
	append([Type,Obj1],Related,Obj1VT),
	vtPrevail(Rest,RestVT,RestNEs),
	append(Obj1VT,RestVT,VT),
	append(NEs1,RestNEs,NEs).

% Deal with the change predicates in a changes clause
% vtPreds(+ChangeProps,-VarsTypes,-NEClauses).

vtPreds((Pre => Add),Res,NEs) :-
	vtPLst(Pre,VTPre,NEs1),
	vtPLst(Add,VTAdd,NEs2),
	append(VTPre,VTAdd,Res),
	append(NEs1,NEs2,NEs).

% Deal with a list of literals
% vtPLst(+Literals,-VarTypes,-NEClauses).

vtPLst([],[],[]).

vtPLst([ne(X,Y)|Rest],Res,[ne(X,Y)|RestNEs]) :-
	!,
	vtPLst(Rest,Res,RestNEs).

vtPLst([Pred|Preds],Res,NEs) :-
	functor(Pred,_,1),
	!,
	vtPLst(Preds,Res,NEs).

% here is the hard bit, Create a dummy literal - instantiate it with
% the OCL predicate list to find the types then
% match up the type with the original literal variables.

vtPLst([Pred|Preds],Res,NEs) :-
	functor(Pred,Name,Arity),
	Pred =.. [Name,Obj1|Rest],
	VNeeded is Arity - 1,
	createVarList(VNeeded,VN),
	DummyPred =.. [Name,X|VN],
	predicates(PList),
	member(DummyPred,PList),
	pair(VN,Rest,This),
	vtPLst(Preds,RestPre,NEs),
	append(This,RestPre,Res).

% Create a list of new uninstantiated variables
% createVarList(+NoOfVariablesNeeded, -ListOfvariables).

createVarList(1,[X]) :-
	!.

createVarList(N,[X|Rest]) :-
	Next is N - 1,
	createVarList(Next,Rest).

% merge the list of variables and the list of types
% pair(+TypeList,+VarList,-varTypeList).

pair([],[],[]).

pair([Type|Types],[Var|Vars],[Type,Var|Rest]) :-
	pair(Types,Vars,Rest).	



% **********************************************************************
% Top Level Routine to instantiate / ground operators in all legal ways
%
% instOps

instOps :-
	retract(opCounter(_)),
	assert(opCounter(1)),
	opParent(No,Operator),
	containsInvars(Operator,InVars,CondInVars), %Find the atomic_invariants
	findVarsAndTypes(Operator,VT,NEs,CondVT),
	chooseVals(VT,NEs,InVars,Vals),
	retract(opCounter(Count)),
	operator(Name,Prev,Nec,Cond) = Operator,
	append(InVars,CondInVars,AllInVars),
	collectAllConds(CondVT,NEs,AllInVars,CondVals,Cond,NewConds),
	filterSE(Prev,FPrev),
	filterSC(Nec,FNec),
	filterSC(NewConds,FNewConds),
	assert(gOperator(Count,No,operator(Name,FPrev,FNec,FNewConds))),
	Next is Count + 1,
	assert(opCounter(Next)),
	fail.

instOps.

% filterSE - remove ne and is_of_sort clauses

filterSE([],[]) :- !.
filterSE([se(Sort,Id,Preds)|Rest],[se(Sort,Id,FPreds)|FRest]) :-
	filterPreds(Preds,FPreds),!,
	filterSE(Rest,FRest).

% filterSC - remove ne and is_of_sort clauses

filterSC([],[]) :- !.
filterSC([sc(Sort,Id,(Pre => Post))|Rest],[sc(Sort,Id,(FPre => FPost))|FRest]) :-
	filterPreds(Pre,FPre),
	filterPreds(Post,FPost),
	!,
	filterSC(Rest,FRest).

% FilterPreds - remove ne and is_of_sort clauses

filterPreds([],[]).
filterPreds([ne(_,_)|Rest],FRest) :-
	!,
	filterPreds(Rest,FRest).
filterPreds([is_of_sort(_)|Rest],FRest) :-
	!,
	filterPreds(Rest,FRest).
filterPreds([H|T],[H|FT]) :-
	filterPreds(T,FT).


% Collect all possible ways of instantiating the conditional effects

collectAllConds(_,_,_,_,[],[]) :- !.

collectAllConds(CondVT,NEs,InVars,CondVals,Cond,_) :-
	retractall(temp(_)),
	chooseVals(CondVT,NEs,InVars,Vals),
	assertIndivConds(Cond),
	fail.

collectAllConds(_,_,_,_,_,NewConds) :-
	setof(Cond,temp(Cond),NewConds).

assertIndivConds([]) :- !.

assertIndivConds([H|T]) :-
	assert(temp(H)),
	assertIndivConds(T).

% Find the atomic_invariants in the Operator 

containsInvars(operator(Name,Prev,Nec,Cond),InVars,CondInVars) :-
	prevInvars(Prev,PInVars),
	necInvars(Nec,NecInVars),
	append(NecInVars,PInVars,InVars),
	necInvars(Cond,CondInVars),
	!.

prevInvars([],[]).
prevInvars([se(Type,Obj,Props)|Rest],InVars) :-
	   propsInvars(Props,PInvars),
	   prevInvars(Rest,RInVars),
	   append(PInVars,RInVars,InVars).

necInvars([],[]).
necInvars([sc(Type,Obj,(Props => Adds))|Rest],Invars) :-
	   propsInvars(Props,PInvars),
	   propsInvars(Adds,AInvars),
	   necInvars(Rest,RInvars),
	   append(AInvars,PInvars,Temp),
	   append(Temp,RInvars,Invars).

propsInvars([],[]).
propsInvars([Prop|Props],[Prop|Rest]) :-
	isInvariant(Prop),
	!,
	propsInvars(Props,Rest).
propsInvars([_|Props],Rest) :-
	propsInvars(Props,Rest).

isInvariant(Prop) :-
	atomic_invariants(Invars),
	functor(Prop,Name,Arity),
	createVarList(Arity,VN),
	Pred =.. [Name | VN],
	member(Pred,Invars).

% Select values for the variables in the operator
%
% chooseVals(+TypeVarList,+NEList,+Invariants,-VarValueList)

chooseVals([],_,_,[]).

chooseVals([Type,Var|TypeVars],NEs,InVars,[Var|Vals]) :-
	objects(Type,AllVals),
	member(Var,AllVals),
	chooseVals(TypeVars,NEs,InVars,Vals),
	obeysNEs(NEs),
	obeysInVars(InVars).

obeysNEs([]).

obeysNEs([ne(V1,V2)|Rest]) :-
	V1 \== V2,
	obeysNEs(Rest).

obeysInVars([]).
obeysInVars([Prop|Rest]) :-
	atomic_invariants(Invars),
	member(Prop,Invars),
	!.

% **********************************************************************
% prettyPrinting Routines for ground OCL operators 
% long and boring


% prettyPrintOp(+<Ground Operator>)

prettyPrintOp(gOperator(No,Par,Op)) :-
	write('gOperator('),
	write(No),write(','),
	write(Par),write(','),nl,
	writeOp(4,Op),
	!.

writeOp(TabVal,operator(Name,Prev,Nec,Cond)) :-
	tab(TabVal),
	write('operator('),write(Name),write(','),nl,
	tab(8),write('% Prevail'),nl,
        tab(8),write('['),nl,
        writePrevailLists(8,Prev),
	tab(8),write('],'),nl,
	tab(8),write('% Necessary'),nl,
        tab(8),write('['),nl,
	writeChangeLists(10,Nec),
	tab(8),write('],'),nl,
	tab(8),write('% Conditional'),nl,
        tab(8),write('['),nl,
	writeChangeLists(10,Cond),
	tab(8),write('])).'),nl.
	
writePropList(TabVal,[]) :-
	tab(TabVal),
	write('[]').

writePropList(TabVal,[ne(_,_)|Props]) :-
	!,
	writePropList(Indent,Props).

writePropList(TabVal,[Prop|Props]) :-
	atomic_invariants(Invars),
	member(Prop,Invars),
	writePropList(TabVal,Props).

writePropList(TabVal,[Prop|Props]) :-
	tab(TabVal),
	write('['),
	write(Prop),
	Indent is TabVal + 1,
	writePList(Indent,Props).

writePList(TabVal,[]) :-
	nl,
	tab(TabVal),
	write(']').

writePList(TabVal,[ne(_,_)]) :-
	!,
	nl,
	tab(TabVal),
	write(']').

writePList(TabVal,[Prop]) :-
	atomic_invariants(Invars),
	member(Prop,Invars),
	!,
	nl,
	tab(TabVal),
	write(']').

writePList(TabVal,[Prop]) :-
	write(','),
	nl,
	tab(TabVal),
	write(Prop),
	write(']').

writePList(TabVal,[ne(_,_),P2|Rest]) :-
	!,
	writePList(TabVal,[P2|Rest]).

writePList(TabVal,[Prop,P2|Rest]) :-
	atomic_invariants(Invars),
	member(Prop,Invars),
	!,
	writePList(TabVal,[P2|Rest]).

writePList(TabVal,[P1,P2|Rest]) :-
	write(','),
	nl,
	tab(TabVal),
	write(P1),
	writePList(TabVal,[P2|Rest]).

writeChangeLists(_,[]).

writeChangeLists(TabVal,[sc(Type,Obj,(Req => Add))|Rest]) :-
	tab(TabVal),
	write('sc('),write(Type),write(','),write(Obj),write(',('),nl,
	Indent is TabVal + 12,
	writePropList(Indent,Req),
	nl,
	tab(Indent),
	write('=>'),
	nl,
	writePropList(Indent,Add),
	write('))'),writeComma(Rest),
	nl,
	writeChangeLists(TabVal,Rest).

writeComma([]).
writeComma(_) :-
	write(',').

writePrevailLists(_,[]).

writePrevailLists(TabVal,[se(Type,Obj,Props)|Rest]) :-
	tab(TabVal),
	write('se('),write(Type),write(','),write(Obj),write(','),nl,
	Indent is TabVal + 12,
	writePropList(Indent,Props),
	write(')'),writeComma(Rest),
	nl,
	writePrevailLists(TabVal,Rest).
	
% ********************************************************************
% Routine to write all ground operators to file
% NOTE : the routine will  supress
%        ne clauses
%                + routines to write them to file
%
% writeOpsToFile(+FileName,+true/false)    

writeOpsToFile(FName) :-
	tell(FName),
	writeOps,
	told.


writeOps :-
	gOperator(No,Par,Op),
	prettyPrintOp(gOperator(No,Par,Op)),
	nl,nl,
	fail.

writeOps.

% ********************************************************************
% Convert OCL File to Ground Operators

convert(InFile,OutFile) :-
	consult(InFile),
	enumerateOps,
	instOps,
	writeOpsToFile(OutFile),
	clean.

% ------------------------------------------------------------------------
% Utilities for Sicstus Prolog

append([],Y,Y).
append([H|T],Y,[H|Rest]) :-
	append(T,Y,Rest).

member(Item,[Item|_]).
member(Item,[_|T]) :-
	member(Item,T).

not(X) :-
	call(X),
	!,
	fail.

not(_).


%--------------------------------------------------------------------------
% Utility for GIPO interface

groundForGipo :-
	enumerateOps,
	instOps,
	retractall(opCounter(_)),
	retractall(opParent(_,_)),
	retractall(temp(_)).

