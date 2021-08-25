/*
 *	file:		rg_dump.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		Juergen Stuber
 *
 *	description:
 *	This file contains predicates to dump modules to files,
 *	to be compiled by the Quintus Runtime Generator.
 *	Predicates in those modules must not be compiled !
 *
 *	history:
 *	891010	js	Added this comment
 *	900322	js	changed test for compiled predicates
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

:- module(rg_dump,[dumpProgram/2]).

/* dumpProgram(+UserFile,+OpFile)
 * dumps a complete prolog program onto files suitable for rg.
 * The file for the user module is named UserFile.rg, and the file
 * containing the operator declarations is called OpFile.rg
 */

dumpProgram(UF,OF) :-
	nl,
	assert(userFileName(UF)),
	recordPrologFlags,
	removeHighUserOps,
	dumpOperators(OF),
	dumpAllModules,
	abolish(userFileName/1),
	abolish(rg:prologFlag,2),
	format('[dump complete]',[]).


/* recordPrologFlags
 * creates a clause rg:prologFlag(Flag,Value) for each flag in
 * module user. These clauses are dumped and used to initialize the
 * prolog flags to their proper values.
 */

recordPrologFlags :-
	prolog_flag(Flag,Value),
	assert(rg:prologFlag(Flag,Value)),
	fail
	;
	true.


/* removeHighUserOps
 * removes all user defined operators with precedence greater than
 * 1000. This is necessary due to a bug in portray_clause where for
 * the '->' and ';' operators parentheses around arguments are not
 * generated. In rg:addOp/3 the removed operators are stored,
 * this is used to declare them during the initialization phase.
 */

removeHighUserOps :-
	current_op(Prec,Type,Op),
%	Prec >= 1000,
	\+(builtInOp(Prec,Type,Op)),
	format('[operator declaration ~w removed]~N',[op(Prec,Type,Op)]),
	op(0,Type,Op),
	assert(rg:addOp(Prec,Type,Op)),
	fail
	;
	true.


/* builtInOp(Prec,Type,Op)
 * gives all built in operators.
 */

builtInOp(1200,xfx,(:-)).
builtInOp(1200,xfx,-->).
builtInOp(1200,fx,(:-)).
builtInOp(1200,fx,(?-)).
builtInOp(1150,fx,(mode)).
builtInOp(1150,fx,(public)).
builtInOp(1150,fx,(dynamic)).
builtInOp(1150,fx,(multifile)).
builtInOp(1150,fx,(meta_predicate)).
builtInOp(1100,xfy,;).
builtInOp(1050,xfy,->).
builtInOp(1000,xfy,',').
builtInOp(900,fy,\+).
builtInOp(900,fy,spy).
builtInOp(900,fy,nospy).
builtInOp(700,xfx,=).
builtInOp(700,xfx,is).
builtInOp(700,xfx,=..).
builtInOp(700,xfx,==).
builtInOp(700,xfx,\==).
builtInOp(700,xfx,@<).
builtInOp(700,xfx,@>).
builtInOp(700,xfx,@=<).
builtInOp(700,xfx,@>=).
builtInOp(700,xfx,=:=).
builtInOp(700,xfx,=\=).
builtInOp(700,xfx,<).
builtInOp(700,xfx,>).
builtInOp(700,xfx,=<).
builtInOp(700,xfx,>=).
builtInOp(600,xfy,:).
builtInOp(500,yfx,+).
builtInOp(500,yfx,-).
builtInOp(500,yfx,\/).
builtInOp(500,yfx,/\).
builtInOp(500,fx,+).
builtInOp(500,fx,-).
builtInOp(400,yfx,/).
builtInOp(400,yfx,//).
builtInOp(400,yfx,*).
builtInOp(400,yfx,<<).
builtInOp(400,yfx,>>).
builtInOp(300,xfx,mod).
builtInOp(200,xfy,^).


/* dumpOperators(+OpFile)
 * writes all operator declarations to file OpFile.
 * The resulting file is intended to be used as an initialization file
 * for rg.
 */

dumpOperators(OF) :-
	addSuffix(OF,F),
	format('[dumping operator declarations to file ~q]~N',[F]),
	tell(F),
	current_op(Prec,Type,Name),
	dumpDirective(op(Prec,Type,Name)),
	fail
	;
	told,
	format('[done]~N',[]).


/* dumpAllModules
 * writes the contents of all modules to files, if necessary.
 * There are three types of modules which are handled different.
 * 1. interpreted user modules:
 *    These are modules which neither contain compiled clauses nor
 *    are library modules (see below for that).
 *    Interpreted user modules are dumped on files and should then
 *    be compiled by rg.
 *    Some modules must be treated this way:
 *    (a) If you use term_expansion when compiling the module, because
 *        term_expansion is not available in rg during compile time.
 *    (b) Any module containing directive rg cannot handle.
 *    (b) Your main module, because it loads the other modules.
 *    (c) Any module without an associated file, e.g. user.
 * 2. compiled user modules:
 *    These are modules which contain some compiled predicates.
 *    The are not dumped on a file, but if a module imports them, they
 *    are loaded via use_module(File,[..]).
 *    There must be a file associated to the module, otherwise
 *    use_module directive cannot be generated and you get an error
 *    message.
 * 3. library modules:
 *    A file is a library file, if its associated file is in a library
 *    directory. This is checked by asking library_directory(..).
 *    These files are not dumped either, but the directive has the 
 *    format use_module(library(Lib),[..]).
 * 4. The module dump (this module) is ignored.
 *
 * The module user is written to the file UserFile.rg.
 * All other modules M are written to M.rg if necessary.
 * To compile the program it should suffice to compile F.rg,
 * if user is the main module. Otherwise the module user is probably not
 * needed, and you should compile your main module. Because we insert
 * use_module directives in all our files, either should cause all
 * needed modules to be compiled.
 */

dumpAllModules :-
	moduleType(M,bad),
	format('[ERROR: no file for compiled module ~q]~N',[M]),
	fail.
dumpAllModules :-
	moduleType(M,interpreted),
	nonEmptyModule(M),
	moduleFile(M,interpreted,F),
	dumpModule(M,F),
	fail
	;
	true.


/* dumpModule(+M,+F)
 * dumps a single module M on file F
 */

dumpModule(M,F) :-
	format('[dumping module ~q to file ~q]~N',[M,F]),
	tmpFileName(TF),
	tell(TF),
	dumpModuleExports(M),
	nl,
	dumpModuleImports(M),
	nl,
	dumpDirective(no_style_check(single_var)),
	dumpModulePredicates(M),
	told,
	format('[renaming predicates of module ~q]~N',[M]),
	renamePredsInFile(TF,F),
	deleteFile(TF),
	format('[done]~N',[]).


/* renamePredsInFile(+Module,+InFile,+OutFile)
 * reads a prolog file and renames all predicates p not supported by
 * the Runtime Generator into rg_p, so they can be handled in the
 * my own environment.
 */

renamePredsInFile(InFile,OutFile) :-
	see(InFile),
	tell(OutFile),
	repeat,
	    read(Old),
	    (Old == end_of_file ->
		true
	    ;
		rgRename(Old,New),
		portray_clause(user:New),
		fail
	    ),
	!,
	told,
	seen.


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
	mkAtom('rg_%',[OF],NF),
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


/* attachModule(+Term,-ModuleTerm)
 * add module user if Term is not already of the form M:T
 */

attachModule(T,T) :-
	nonvar(T),
	T = _M:_T,
	!.
attachModule(T,user:T).


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

/*----------------------------------------------------------------------*/
/* dump exports */

/* dumpModuleExports(+M)
 * writes out a module directive for M, listing all the predicates
 * it exports, if any.
 */

dumpModuleExports(user) :-
	!.
dumpModuleExports(M) :-
	(
	    bagof(Pred, moduleExports(M,Pred), Exports)
	;
	    Exports = []
	),
	dumpDirective(module(M,Exports)),
	!.
dumpModuleExports(_).


/* moduleExports(?Module,?PredSpec)
 * succeeds if module M exports predicate PredSpec.
 * Can be used to backtrack through all exported predicates.
 */

moduleExports(Module, Name/Arity) :-
	predicate_property(Module:Goal, exported),
	functor(Goal, Name, Arity).

/*----------------------------------------------------------------------*/
/* dump imports */

/* dumpModuleImports(+Module)
 * writes out a use_module directive for each module used by Module.
 * Note how bagof enumerates Exporter modules, giving us the Imports
 * list for each Exporter in turn.
 */

dumpModuleImports(Module) :-
	current_module(Exporter),
	(bagof(Pred, moduleImports(Module,Pred,Exporter), Imports) ->
	    true
	;
	    Module = user,
	    Exporter \== user,
	    nonEmptyModule(Exporter),
	    Imports = []
	),
	moduleFile(Exporter,ExpFile),
	nl,
	dumpDirective(use_module(ExpFile,Imports)),
	fail 
	;
	true.


moduleImports(Module, Name/Arity, Exporter) :-
	predicate_property(Module:Goal, imported_from(Exporter)),
	functor(Goal, Name, Arity).

/*----------------------------------------------------------------------*/
/* dump predicates */

/* dumpModulePredicates(+M)
 * dumps all clauses defined in module M.
 * Clauses imported from other modules must not be dumped, so we cannot
 * use listing without specifying the predicate.
 */

dumpModulePredicates(M) :-
	current_predicate(_,M:Goal),	/* only predicates defined in M */
	functor(Goal,F,N),
	dumpPredicate(M,F,N),
	fail
	;
	true.


/* dumpPredicate(+M,+P,+N)
 * dumps all necessary information about predicate P/N in module M.
 * At the moment this is an optional dynamic declaration and the
 * clauses of the predicate.
 */

dumpPredicate(M,P,N) :-
	nl,
	dumpPredicateProperties(M,P,N),
	dumpPredicateClauses(M,P,N).


/* dumpPredicateProperties(+M,+F,+N)
 * dumps properties of a predicate necessary for rg
 */

dumpPredicateProperties(M,F,N) :-
	functor(Goal,F,N),
	predicate_property(M:Goal, (dynamic)),
	nl,
	dumpDirective(dynamic(F/N)),
	fail.
dumpPredicateProperties(_,_,_).


/* dumpPredicateClauses(+M,+F,+N)
 * dumps all clauses of a predicate
 */

dumpPredicateClauses(M,F,N) :-
	listing(M:F/N).

/*----------------------------------------------------------------------*/
/* miscellanous */

/* dumpDirective(+Directive)
 * is used to write directives. It takes the Goal and adds ':-' and '.\n'.
 * It is important that we use ~q and writeq/1 here, so that
 * the predicate names can be read back later (and module names too).
 */

dumpDirective(Goal) :-
	format(':- ~k.~n',Goal).


/* nonEmptyModule(+M)
 * succeeds if module M contains something
 */

nonEmptyModule(M) :-
	predicate_property(M:_G,Prop),
	Prop \== built_in,
	!.

/*----------------------------------------------------------------------
 * Definition of module types
 */

/* moduleType(?M,?T)
 * gives the module type T of M, one of interpreted,compiled,library or
 * bad.
 */

moduleType(M,T) :-
	current_module(M),		/* to backtrack through modules */
	(   M==rg_dump ->
		T=rg_dump
	;
	    current_module(M,F),	/* module has associated file F */
	    library_directory(D),	/* file F is in library */
	    name(F,FN),
	    name(D,DN),
	    append(DN,_,FN) ->
		T=library
	;   \+ containsCompiledPred(M) ->
		T=interpreted
	;   current_module(M,_) ->
		T=compiled
	;   otherwise ->
		T=bad
	).

containsCompiledPred(M) :-
	predicate_property(M:G,compiled),
	current_predicate(_,M:G),		% not imported
	\+ predicate_property(M:G,foreign).

/*----------------------------------------------------------------------
 * generation of filenames
 */

/* moduleFile(+M,-F)
 * gives the file name for module M. The three different types of modules
 * are handled differently.
 */

moduleFile(M,F) :-
	moduleType(M,T),
	moduleFile(M,T,F).


/* moduleFile(+M,+T,-F)
 * gives the filename for module M of module type T.
 */

moduleFile(M,interpreted,F) :-
	(M==user ->
	    userFileName(F1)
	;
	    F1=M
	),
	addSuffix(F1,F).
moduleFile(M,compiled,F) :-
	current_module(M,F).
moduleFile(M,library,F) :-
	libraryName(M,L),
	F=library(L).


/* addSuffix(+Name,-File)
 * adds the suffix to get a filename from a module name 
 * (used also for 'ops'-file).
 * tested: 13.6.1989
 */

addSuffix(N,F) :-
	mkAtom('%.rg.pl',[N],F).


/* libraryName(+M,-L)
 * gives the library (file) name L for a library module M.
 * tested: 13.6.1989
 */

libraryName(M,L) :-
	current_module(M,F),	/* module has associated file F */
	library_directory(D),	/* file F is in library */
	name(F,FN),
	name(D,DN),
	[Slash]="/",
	append(DN,[Slash|NN],FN),
	(append(LN,".pl",NN) ->
	    true
	;
	    NN=LN
	),
	name(L,LN).

/*----------------------------------------------------------------------*/
/* these are some imports we need */

append(X,Y,Z) :-
	user:append(X,Y,Z).

library_directory(X) :-
	user:library_directory(X).

mkAtom(X,Y,Z) :-
	user:mkAtom(X,Y,Z).

tmpFileName(F) :-
	user:tmpFileName(F).

deleteFile(F) :-
        mkAtom('rm %',[F],Cmd),
	unix(system(Cmd)).
