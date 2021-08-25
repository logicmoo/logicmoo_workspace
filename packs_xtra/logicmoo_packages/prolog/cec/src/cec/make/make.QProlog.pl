/*
 *	file:		make.QProlog.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file is the main file for CEC.
 *
 *	history:
 *	891010	js	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/* Load member/2 and append/3 */
:- use_module(library(basics)).

:- op(500,fx,cd).
:- op(50,fx,[@]).       % Variable
:- op(600,xfx,[ofType]).
:- op(600,xfx,<-).
:- op(200,fx,[?]).
:- op(950,xfx,[=>]).
:- op(940,fx,[(or)]).
:- op(950,fx,[(op)]).
:- op(950,fx,[(cons)]).
:- op(900,fx,not).
:- op(850,xfy,and).
:- op(1100,fx,var).
:- op(400,xfx,for).
:- op(600,xfx,using).
:- op(500,fx,module).
:- op(500,fx,order).
:- op(910,fy,let).
:- op(935,xfy,in).
:- op(500,xfy,'|').
:- op(1100,fx,(parameter)).
:- op(1100,xfx,(generatedBy)).


:-
	setof((P,F,OP),current_op(P,F,OP),O),
	assert(systemDeclaredOps(O)).


predefinedNotationsForOps(OL):-
	setof1(Op,[P,F]^(systemDeclaredOps(O),member((P,F,Op),O)),OL).

doNotChangeNotation(Op) :-
    predefinedNotationsForOps(Ops),
    member(Op,Ops),
    !.


predefinedOps(
    [@,:,=,and,=>,<-]) :- !.
doNotUseAsOperator(Op) :-
    predefinedOps(Ops),
    !,
    member(Op,Ops).


nullPrec(0).

not(P) :-
	P,
	!,
	fail.
not(_).


totalRuntime(TinSec) :-
	statistics(runtime,[T,_]),
	TinSec is T / 1000,
	!.


integerDivision(A,B,C) :-
	C is A//B,
	!.




exists(F) :-
	concAtomNames('test -f ',F,C),
	unix(shell(C)),
	!.


succ(X,SX) :-
	var(SX),
	!,
	SX is X+1.
succ(X,SX) :-
	var(X),
	X is SX-1.


isSourceFile(F) :-
	name('.pl',SourceSuffix),
	name(F,FileNameChars),
	append(_,SourceSuffix,FileNameChars).

srcFileName(F,AFN) :-
	srcPath(SP),
	name(SP,SPChars),
	name('/',[Slash]),
	name(F,FileNameChars),
	append(SPChars,[Slash|FileNameChars],AFNChars),
	name(AFN,AFNChars).

compileA(F) :-
	(isSourceFile(F) ->
		srcFileName(F,AFN)
	;
		absolute_file_name(F,AFN)
	),
	rg_env:rg_compile(user:AFN).



%compileA(F) :-
%	absolute_file_name(F,A),
%	compile(A).


:- unknown(_,fail).


:- no_style_check(single_var).
:- no_style_check(discontiguous).


:- compileA('prolog/apply.pl').
:- compileA('prolog/arith.pl').
:- compileA('prolog/imp.pl').
:- compileA('prolog/io.pl').
:- compileA('prolog/utilities.pl').
:- compileA('prolog/lists.pl').
:- compileA('prolog/sets.pl').
:- compileA('prolog/strings.pl').
:- compileA('prolog/funtest.pl').
:- srcFileName('prolog/interrupt.pl',AF),compile(AF).

:- compileA('acUni/uniac.new.pl').

:- dynamic mapExpansion/1.
:- dynamic benchmark/1.



benchmark(contextualReduction).
benchmark(new).
benchmark(simplerProof/5).



:- dynamic pc/1.
pc(1).


mapExpansion(yes).

:- compileA('make/mapexpansion.pl').

:- compileL('acUni/achelp.new.pl').
:- compileL('acUni/gen.new.pl').
:- compileL('acUni/match.new.pl').

:- compileL('terms/terms.pl'). % needs to be compiled first, as expansion uses term predicates


:- compileL('tps/kns/kns.pl').     % kns predicates
:- compileL('tps/kns/rename.pl').  % operator renaming for kns
:- compileL('tps/kns/knsmods.pl'). % predicates used by kns and neqkns
:- compileL('tps/rename.pl').


:- compileL('tps/polyn/check.pl').
:- compileL('tps/polyn/arithmetic.pl').
:- compileL('tps/polyn/current.pl').
:- compileL('tps/polyn/inout.pl').
:- compileL('tps/polyn/interpret.pl').
:- compileL('tps/polyn/operators.pl').
:- compileL('tps/polyn/ordering.pl').
:- compileL('tps/polyn/suggest.pl').
:- compileL('tps/polyn/trace.pl').
:- compileL('tps/polyn/transform.pl').
:- compileL('tps/polyn/tuplelength.pl').
:- compileL('tps/polyn/utilities.pl').
:- compileL('tps/polyn/combine.pl').
:- compileL('tps/polyn/rename.pl').  % expects unique operator names
                                       % like kns
:- compileL('tps/polyn/addinterp.pl').
:- compileL('tps/polyn/polynomial.pl').
:- compileL('tps/polyn/polGreater.pl').
:- compileL('tps/interface.pl').

% :- compileA('panndas/ccallqp.pl').
% :- compileA('panndas/panConnect.pl').
% :- compileA('panndas/panEvaluate.pl').
% :- compileA('panndas/panGetEquation.pl').
% :- compileA('panndas/panInput.pl').
% :- compileA('panndas/panMiscell.pl').
% :- compileA('panndas/panOutput.pl').
% :- compileA('panndas/panPreprocess.pl').
% :- compileA('panndas/panSorts.pl').
% :- compileA('panndas/panTrace.pl').
% :- compileA('panndas/inTerms.pl').


:- style_check(single_var).

:- compileL('worlds/changable.pl').
:- compileL('worlds/stack.pl').
:- compileL('worlds/startup.pl').
:- compileL('worlds/state.pl').
:- compileL('worlds/worlds.pl').

:- compileL('completion/action.pl').
:- compileL('completion/complCom.pl').
:- compileL('completion/dbKit.pl').
:- compileL('completion/dbscheme.pl').
:- compileL('completion/eqCompletion.pl').
:- compileL('completion/order.pl').
:- compileL('completion/orderCom.pl').
:- compileL('completion/regular.pl').

:- compileL('terms/typing.pl').


:- ensure_loaded(library(change_arg)).
:- ensure_loaded(library(arg)).

:- compileL('compute/antecedent.pl').
:- compileL('compute/compile.pl').
:- compileL('compute/narrow.pl').
:- compileL('compute/norm.pl').
:- compileL('compute/prove.pl').
:- compileL('compute/reduce.pl').

:- compileL('cr/commands.pl').

:- compileL('display/portray.pl').
:- compileL('display/show.pl').

:- compileL('help/helptext.pl').

:- compileL('inout/in.pl').
:- compileL('inout/modExp.pl').
:- compileL('inout/orderIO.pl').
:- compileL('inout/path.pl').
:- compileL('inout/thawFreeze.pl').

:- compileL('specVars/combine.pl').
:- compileL('specVars/morphisms.pl').
:- compileL('specVars/rename.pl').
:- compileL('specVars/specVar.pl').
:- compileL('specVars/undo.pl').


:- compileL('prolog/genarg.pl').

:- assign1(maxCP,6).
:- storeOpPrec.
:- genNew2.

cec_language('q2.0').
