/*
 *	file:		rg_env.pl
 *	version:	1.0
 *	date:		May 22, 1990
 *	creation:	October 10, 1989
 *	author:		Juergen Stuber
 *
 *	description:
 *	This file contains a runtime environment for CEC running
 *	under the Quintus Prolog Runtime Generator. It emulates some
 *	of the predicates the Runtime Generator cannot handle
 *	(e.g. consult).
 *
 *	history:
 *	891010	js	Added this comment
 *	900522	js	Now using library(read) for toplevel read
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

:- module(rg_env,[]).

:- use_module( library(read)).

:- meta_predicate
	rg_compile(:),
	rg_consult(:),
	rg_reconsult(:),
	rg_ensure_loaded(:),
	rg_load_foreign_files(:,+),
	rg_spy(:),
	rg_nospy(:),
	rg_source_file(:,?),
	rg_use_module(:),
	rg_use_module(:,+).

:- dynamic prologFlag/2.
/* :- dynamic goalAndVariables/2.*/
:- dynamic abortMsg/1.

/*----------------------------------------------------------------------*/
/* entry point */
/* Note: the predicate user:rg_init/0 must be provided by the user of the
 *       package. It should initialize the program and print the signon
 *       message. If the program uses the prolog toplevel for input,
 *       init should just succeed, otherwise rg_init may handle the entire
 *       program and exit by calling halt/0.
 */

runtime_entry(start) :-
	init,
	user:rg_init,
	go.
runtime_entry(abort) :-
	(retract(abortMsg(Msg)) ->
	    write(Msg)
	;
	    write('[ Execution aborted ]'),
	    nl,
	    nl
	),
	go.

/* init
 * initializes some data structures, most important of which are
 * the prolog flags and the removed user defined operators.
 */

init :-
	retractall(abortMsg(_)),
%	retractall(goalAndVariables(_,_)),
	rg:prologFlag(Flag,Value),
	prolog_flag(Flag,_,Value),
	fail
	;
	rg:addOp(Prec,Type,Op),
	op(Prec,Type,Op),
	fail
	;
	true.


/* abort(+Msg)
 * is used to abort a computation, but instead of printing the usual
 * [ Execution aborted ] message it prints Msg. Used for 'yes'.
 * Does only work with the runtime generator.
 */

abort(Msg) :-
	assert(abortMsg(Msg)),
	abort.

/*----------------------------------------------------------------------*/
/* toplevel loop */
/* Note: The answer to a query is given without the variable names.
 *       This is done because Quintus Prolog renames all Variables to
 *       the internal form _N when reading a term, so the names typed
 *       by the user are not available to the program.
 */

go :-
	prompt(_,'| ?- '),
	repeatNo,
	portable_read(ReadGoal,Vars),
	(
	    ReadGoal == end_of_file,
	    nl
	;
	    stripGoal(ReadGoal,NakedGoal,AnsMode),
	    retractall(answerMode(_,_)),
	    assert(answerMode(AnsMode,ReadGoal)),
	    rgRename(NakedGoal,NewGoal),
	    attachModule(NewGoal,Goal),
	    call(Goal),
	    printAnswer(Vars),
	    askUser
	).


/* stripGoal(+ClothedGoal,-NakedGoal,-AnswerMode)
 * removes (:-)/1 or (?-)/1 if present
 */

stripGoal(:-(Goal),Goal,silent) :- !.
stripGoal(?-(Goal),Goal,solution) :- !.
stripGoal(Goal,Goal,solution).


/* rgRename(+Old,-New)
 * renames those predicates not allowed in RG, so they can be handled
 * by my runtime environment
 */

rgRename(X,X) :-
	var(X),
	!.
rgRename((Head:-Body),(Head:-Body1)) :-
	rgRename(Body,Body1),
	!.
rgRename(Old,New) :-
	functor(Old,F,N),
	doRecurse(F,N),
	!,
	Old =.. [_|OArgs],
	rgRenameList(OArgs,NArgs),
	New =.. [F|NArgs].
rgRename(Old,rg_env:New) :-
	functor(Old,OF,N),
	doRename(OF,N),
	!,
	Old =.. [_|Args],
	user:mkAtom('rg_%',[OF],NF),
	(doAttachModule(OF,N) ->
	    [Arg1|Rest] = Args,
	    attachModule(Arg1,NewArg1),
	    [NewArg1|Rest] = NewArgs
	;
	    NewArgs = Args
	),
	New =.. [NF|NewArgs].
rgRename(Lit,Lit).

rgRenameList([],[]).
rgRenameList([O|OR],[N|NR]) :-
	rgRename(O,N),
	rgRenameList(OR,NR).


/* attachModule(+NakedGoal,-Goal)
 * if Goal does not already contain a module the module user is added.
 */

attachModule(Goal,Goal) :-
	nonvar(Goal),
	Goal = _M:_G,
	!.
attachModule(Goal,user:Goal).

doRecurse(',',2).
doRecurse(';',2).
doRecurse('->',2).
doRecurse((\+),1).

doRename(debug,0).
doRename(nodebug,0).
doRename(trace,0).
doRename(notrace,0).
doRename(spy,1).
doRename(nospy,1).
doRename(nospyall,0).
doRename(debugging,0).
doRename(leash,1).
doRename(ancestors,1).
doRename(subgoal_of,0).
doRename(depth,1).
doRename(maxdepth,1).
doRename(help,0).
doRename(help,1).
doRename(manual,0).
doRename(manual,1).
doRename(break,0).
doRename(source_file,1).
doRename(source_file,2).
doRename(compile,1).
doRename(consult,1).
doRename(ensure_loaded,1).
doRename(reconsult,1).
doRename(use_module,1).
doRename(use_module,2).
doRename('.',2).
doRename(style_check,1).
doRename(no_style_check,1).
doRename(load_foreign_files,2).

doAttachModule(compile,1).
doAttachModule(consult,1).
doAttachModule(reconsult,1).
doAttachModule(ensure_loaded,1).
doAttachModule(load_foreign_files,2).
doAttachModule(spy,1).
doAttachModule(nospy,1).
doAttachModule(source_file,2).
doAttachModule(use_module,1).
doAttachModule(use_module,2).


/* prepareAnswer(Goal)
 * stores the original term and its list of variables, because the
 * variables are not available in the goal after its execution

prepareAnswer(Goal) :-
	retractall(goalAndVariables(_,_)),
	prologVarsOf(Goal,VarList),
	assert(goalAndVariables(Goal,VarList)).
 */


/* printAnswer(Goal)
 * prints the answer substitution for a goal or 'yes' if there are no
 * variables.
 */

printAnswer(Variables) :-
	(answerMode(solution,_G) ->
	    ( Variables = [] ->
	        nl,
	        abort(yes)
	    ;
	        nl,
	        printBindings(Variables)
	    )
	;
	    abort('')
	).


/* printBindings(L)
 * L contains the substituted terms for the variables in the goal.
 * It is printed similar to qprolog, but variable names are not printed
 * (sorry) because they are not available to the program (variables
 * are renamed into internal their form when reading the goal).
 */

printBindings([]).
printBindings([Name=Value|R]) :-
	identicalBindings(Value,R,Names,Rest),
	writeNames([Name|Names]),
	print(Value),
	( Rest = [] ->
	    write(' ')
	;
	    write(','),
	    nl
	),
	printBindings(Rest).


/* identicalBindings(+Value,+Bindings,-Names,-Rest)
 * from Bindings collect all Names bound to Value and put all
 * other bindings in Rest.
 */

identicalBindings(_,[],[],[]).
identicalBindings(V,[N1=V1|Bs],[N1|Ns],Rest) :-
	V==V1,
	!,
	identicalBindings(V,Bs,Ns,Rest).
identicalBindings(V,[B|Bs],Ns,[B|Rest]) :-
	identicalBindings(V,Bs,Ns,Rest).


/* writeNames(Names)
 * write Names as Name1 = Name2 = .. =
 */

writeNames([]).
writeNames([N|Ns]) :-
	write(N),
	write(' = '),
	writeNames(Ns).


/* prologVarsOf(X,L)
 * builds the list L of all prolog variables in X. Variables in L are in
 * order of appearance in X, and each variable is listed only once.
 * Beware: In order not to instantiate variables we use a special 
 * union/3 predicate.

prologVarsOf(X,[X]) :-
	var(X),
	!.
prologVarsOf(T,Vars) :-
	T =.. [_Func|Args],
	prologVarsOfArgs(Args,Vars).

prologVarsOfArgs([],[]).
prologVarsOfArgs([A|AL],Vars) :-
	prologVarsOf(A,AVars),
	prologVarsOfArgs(AL,ALVars),
	varsUnion(AVars,ALVars,Vars).
 */


/* askUser
 * asks the user what to do after printing a solution to a goal
 */

askUser :-
	getLine(L),
	( L == ";" ->
	    fail
	; L == "" ->
	    abort('')
	;
	    write('Action (";" for more choices, otherwise <return>): '),
	    askUser
	).


/* getLine(L)
 * reads characters up to the next linefeed into L. The LF is read, but
 * not included in L.
 */

getLine(L) :-
	get0(Ch),
	( Ch = 10 ->
	    L=[]
	;
	    getLine(L1),
	    L=[Ch|L1]
	).


/* repeatNo
 * similar to the builtin predicate repeat/0, but on each redo 'no'
 * is printed (only redo, not on first call).
 */

repeatNo :-
	nl.
repeatNo :-
	(answerMode(silent,Goal) ->
	    format(user_error,'[WARNING, goal failed:  ~q]',[Goal]),
	    nl
	;
	    nl,
	    write(no)
	),
	repeatNo.

/*----------------------------------------------------------------------*/
/* varsUnion */
/* This union is different from the one provided with the library !
 * It does not instantiate Variables in the given sets.

varsUnion([],S,S).
varsUnion([E|R],L,U) :-
	varsUnion(R,L,RL),
	( varsMember(E,L) ->
	    U = RL
	;
	    U = [E|RL]
	).

varsMember(E1,[E2|_]) :- E1==E2.	Do not unify different Variables
varsMember(E,[_|L]) :-
	varsMember(E,L).
*/

/*----------------------------------------------------------------------*/
/* runtime support for some functions */

/*----------------------------------------------------------------------*/
/* the following functions are not supported */

rg_debug :-
	notAvailError(debug/0,abort).
rg_nodebug :-
	notAvailError(nodebug/0,continue).
rg_trace :-
	notAvailError(trace/0,abort).
rg_notrace :-
	notAvailError(notrace/0,continue).
rg_spy(_) :-
	notAvailError((spy)/1,abort).
rg_nospy(_) :-
	notAvailError((nospy)/1,continue).
rg_nospyall :-
	notAvailError(nospyall/0,continue).
rg_debugging :-
	notAvailError(debugging/0,continue).
rg_leash(_) :-
	notAvailError(leash/1,continue).

rg_ancestors(_) :-
	notAvailError(ancestors/1,abort).
rg_subgoal_of(_,_) :-
	notAvailError(subgoal_of/2,abort).
rg_depth(_) :-
	notAvailError(depth/1,abort).
rg_maxdepth(_) :-
	notAvailError(maxdepth/1,abort).
rg_help :-
	notAvailError(help/0,abort).
rg_help(_) :-
	notAvailError(help/1,abort).
rg_manual :-
	notAvailError(manual/0,abort).
rg_manual(_) :-
	notAvailError(manual/1,abort).
rg_break :-
	notAvailError(break/0,abort).
rg_source_file(_) :-
	notAvailError(source_file/1,abort).
rg_source_file(_,_) :-
	notAvailError(source_file/2,abort).

/*----------------------------------------------------------------------*/
/* the following functions are emulated */

rg_compile(F) :-
	rg_reconsult(F).

rg_consult(F) :-
	rg_reconsult(F).


rg_style_check(_).	% ignore, no style check possible
rg_no_style_check(_).


:- dynamic consultingPredicate/2.

rg_reconsult(M:F) :-
	absolute_file_name(F,AF),
	retractall(consultingPredicate(_,_)),
	see(AF),
	reconsultInput(M),
	seen,
	retractall(consultingPredicate(_,_)).

reconsultInput(M) :-
	repeat,
	    read(Term),
	    (Term == end_of_file ->
		true
	    ;
		expand_term(Term,XTerm),
		reconsultTerm(M,XTerm),
		fail
	    ),
	!.

reconsultTerm(M,?-(Goal)) :-
	!,
	callNoDecl(M,Goal).
reconsultTerm(M,:-(Goal)) :-
	!,
	callNoDecl(M,Goal).
reconsultTerm(M,Clause) :-
	clauseHead(Clause,Head),
	recordDoing(M,Head),
	assertz(M:Clause).


recordDoing(M,Hd) :-
	consultingPredicate(M,Hd),
	!.
recordDoing(M,Hd) :-
	functor(Hd,F,N),
	functor(Pred,F,N),
	assert(consultingPredicate(M,Pred)),
	abolish(M:F,N).


clauseHead((Head:-_),Head) :- !.
clauseHead(Head,Head).


callNoDecl(_M,Goal) :-
	functor(Goal,F,N),
	compilerDeclaration(F/N),
	!.
callNoDecl(M,Goal) :-
	call(M:Goal).

compilerDeclaration((dynamic)/1).
compilerDeclaration((meta_predicate)/1).
compilerDeclaration((mode)/1).
compilerDeclaration((multifile)/1).
compilerDeclaration((public)/1).

/*----------------------------------------------------------------------*/
/* the following predicates are not needed for CEC and hence not
 * emulated yet
 */

rg_ensure_loaded(_) :-
	notImplError(ensure_loaded/1,abort).
rg_use_module(_) :-
	notImplError(use_module/1,abort).
rg_use_module(_,_) :-
	notImplError(use_module/2,abort).
rg_load_foreign_files(_,_) :-
	notImplError(load_foreign_files/2,continue).

/*----------------------------------------------------------------------*/
/* creating error messages */

notAvailError(Pred,Action) :-
	raiseError('predicate ~q not available',Pred,Action).
notImplError(Pred,Action) :-
	raiseError('predicate ~q not implemented',Pred,Action).

raiseError(Msg,Culprits,Action) :-
	format(user_error,'~N[ERROR: ',[]),
	format(user_error,Msg,Culprits),
	format(user_error,']~n',[]),
	doAction(Action).

doAction(fail) :-
	!,fail.
doAction(continue) :-
	!.
doAction(abort) :-
	abort.
