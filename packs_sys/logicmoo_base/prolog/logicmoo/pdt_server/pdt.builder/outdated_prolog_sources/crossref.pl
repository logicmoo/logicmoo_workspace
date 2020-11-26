/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module( xref,
           [buildXrefDb/0,
            reportXref/0, listXref/0
     ]).
     
:- use_module('analyzer/metafile_referencer.pl').
   
/*****************************************************************************
 * Crossreference of Prolog files:
 *
 * Behaviour Protocoll:
 *
 *  <update of Prolog db> ,
 *  buildXrefDb,
 *  [ listXref
 *    | db_xrefFromTo(CallerName, CallerArity, CalledName, CalledArity,CallerFile,CalledFile)
 *    | db_xrefFromToFiles(CallerFile,CalledFiles,CalledPreds)
 *    | db_xrefFromToFilesOnly(?CallerFile,?CalledFile)
 *    | ... other analyses from analyses.pl ...
 *  ]*
 */


/** 
 * db_xrefFromTo(?callerName,?callerArity,?calledName,calledArity,?callerFile:File,?calledFile:File)
 *   Arg1 is a predicate defined using the predicate arg3/4.
 *   Arg2 is its arity.
 *   Arg3 is a predicate called by predicate arg1/2.
 *   Arg4 is its arity. 
 *   Arg5 is the file in which arg1/2 is defined.
 *   Arg6 is the file in which arg3/4 is defined.
 */
:- dynamic db_xrefFromTo/6.

/** 
 * db_xrefFromToFiles(?CallerFile,?CalledFile,?CalledPreds)
 *   The predicates in the list arg3, defined in file arg2,
 *   are called from predicates in file arg1.
 *   The predicates in arg3 are represented as functor/arity terms.
 */
:- dynamic db_xrefFromToFiles/3.

/*
 * db_xrefFromToFilesOnly(?CallerFile,?CalledFile)
 *   Predicates defined in file arg2 are called from predicates in file arg1.
 */
:- dynamic db_xrefFromToFilesOnly/2.
    
/* **************** Creation of XrefDB ***************************************/
     
deleteXrefDb :-
    retractall(db_xrefFromTo(_,_,_,_,_,_)),
    retractall(db_xrefFromToFiles(_,_,_)),
    retractall(db_xrefFromToFilesOnly(_,_)).
   
buildXrefDb :-                    % delete old XrefDb
    deleteXrefDb,
    fail.
buildXrefDb :-                     % create predicate-level XrefInfos
%    userdefined(Module:H),
%      clause(Module:H,T),
%      storeXrefInfoForPreds(T,H),  % <-- main predicate
    find_xref___(_Module,_Head,_Tail),
    fail.
buildXrefDb :-                     % create file and predicate-level XrefInfos
      time(storeXrefInfoForFiles),
    fail.
buildXrefDb :-                     % create file-level-only XrefInfos
      time(storeXrefInfoForFilesOnly),
    fail.
buildXrefDb.
%buildXrefDb :-                    % report result
%    reportXref.                   % done in xrefTest.pl

find_xref___(Module,Head,Tail) :-
     userdefined(Module:Head),
      clause(Module:Head,Tail),
      storeXrefInfoForPreds(Tail,Module:Head).  % <-- main predicate
 
 
reportXref :- 
    report_to_file_in_ctchome(listXref).

listXref :-
    listing(db_xrefFromTo),
    listing(db_xrefFromToFiles),
    listing(db_xrefFromToFilesOnly).


/*
 * is_metaterm(?Literal, ?MetaArguments )
 *  Arg1 is a literal representing a metacall and 
 *  Arg2 is the list of its meta-arguments.
 */
is_metaterm(Literal, MetaArguments) :-
   ( 	var(Literal) 
   -> 	(	current_predicate(_,Literal) 
   		; 	user:current_predicate(_,Literal)  
   		) 
   	; 	true
   	),
   predicate_property(Literal,meta_predicate(MetaTerm)),
   Literal =.. [Functor|Args],
   MetaTerm =.. [Functor|MetaArgs], 
   collect_meta_args(Args,MetaArgs, MetaArguments ).

collect_meta_args(Args,MetaArgs, MetaArguments ) :- 
    findall( 
         Meta,
         extract_meta_argument(Args,MetaArgs, Meta),
         MetaArguments
    ).
    
extract_meta_argument(Args,MetaArgs, (N,NewArg) ) :- 
    nth1(N,MetaArgs,MArg),
    ( MArg == ':' ; integer(MArg)),
    nth1(N,Args,Arg),
    additonal_parameters(MArg,Arg,NewArg).
   
additonal_parameters(':',Arg,Arg) :- !.
additonal_parameters(0,Arg,Arg) :- !.
additonal_parameters(N,Arg,NewArg) :-
	Arg =.. [Functor | Params],				%Eva: Hier wird erwartet, dass Arg gebunden ist!!!!
   	length(N_Elems,N),
   	append(Params,N_Elems,NewParams),
   	NewArg =.. [Functor | NewParams].
    

    
    
 
/****************************************************************************
 * storeXrefInfoForPreds(Head, Body)
 *    Arg1 is the head of a clause, arg2 its body. 
 *    The storeXrefInfoForPreds/2 predicate stores 
 *    db_xrefFromTo(CallerName, CallerArity, CalledName, CalledArity,CallerFile,CalledFile)
 *    facts, which are the basis for all xref analyses.
 *    Head (arg1) becomes the CallerPred and every literal in 
 *    Body (arg2) that is not for a built-in predicate becomes 
 *    a CalledPred.
 */


 
storeXrefInfoForPreds( Call, _Caller) :-
    var(Call),
    !.
storeXrefInfoForPreds( ','(H,T), Caller) :-
    !,
    storeXrefInfoForPreds(H, Caller),
    storeXrefInfoForPreds(T, Caller).
storeXrefInfoForPreds( ';'(H,T), Caller) :-
    !,
    storeXrefInfoForPreds(H, Caller),
    storeXrefInfoForPreds(T, Caller).
storeXrefInfoForPreds( '|'(H,T), Caller) :-
    !,
    storeXrefInfoForPreds(H, Caller),
    storeXrefInfoForPreds(T, Caller).
storeXrefInfoForPreds( '->'(H,T), Caller) :-
    !,
    % GK: Strangely enough, the following does't work
    % but produces an "out of global stack" error:
%     storeXrefInfoForPreds(H, Caller),
    % The following works but I'm not sure whether it does not miss some
    % cases. Is it guaranteed, that H is always a non-meta literal???
    storeXrefInfoForPred_(H, Caller),
    storeXrefInfoForPreds(T, Caller).
storeXrefInfoForPreds( '*->'(H,T), Caller) :-
    !,
    storeXrefInfoForPreds(H, Caller),
    storeXrefInfoForPreds(T, Caller).
storeXrefInfoForPreds( '\+'(T), Caller) :-
    !,
   storeXrefInfoForPreds(T, Caller).
storeXrefInfoForPreds( 'call'(T), Caller) :-
    !,
    % GK: Strangely enough, the following does't work
    % but produces an "out of global stack" error:
%   storeXrefInfoForPreds(T, Caller),
    % The following works but I'm not sure whether it does not miss some
    % cases. Is it guaranteed, that T is always a non-meta literal???
    
    point4,
    storeXrefInfoForPred_(T, Caller).           % DM: Why store only T and not call(T)??

% How to intercept a variable length call? Variable length call to be done:
% storeXrefInfoForPreds( 'call'???????(T), Caller) :-
%    !,
%    storeXrefInfoForPreds(T, Caller).
storeXrefInfoForPreds( 'apply'(T,L), Caller) :-
    !,
    functor(T,F,Arity),
    length(L,Len),
    TotalArity is Arity + Len,
    functor(Tfull,F,TotalArity),
    storeXrefInfoForPred_(Tfull, Caller).
storeXrefInfoForPreds( 'not'(T), Caller) :-
    !,
    storeXrefInfoForPreds(T, Caller).
storeXrefInfoForPreds( 'once'(T), Caller) :-
    !,
    storeXrefInfoForPreds(T, Caller).
storeXrefInfoForPreds( 'ignore'(T), Caller) :-
    !,
    storeXrefInfoForPreds(T, Caller).
storeXrefInfoForPreds( 'call_with_depth_limit'(T,_,_), Caller) :-
    !,
    storeXrefInfoForPreds(T, Caller).
storeXrefInfoForPreds( 'call_cleanup'(Goal,Catch,Clean), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller),
    storeXrefInfoForPreds(Catch, Caller),
    storeXrefInfoForPreds(Clean, Caller).
storeXrefInfoForPreds( 'call_cleanup'(Goal,Clean), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller),
    storeXrefInfoForPreds(Clean, Caller).
storeXrefInfoForPreds('block'(_,Goal,_), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds('findall'(_,Goal,_), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
   
storeXrefInfoForPreds('bagof'(_,Goal,_), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds('setof'(_,Goal,_), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds(_Var^Goal, Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds('maplist'(Goal,_), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds('maplist'(Goal,_,_), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds('maplist'(Goal,_,_,_), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds('sublist'(Goal,_,_), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds('forall'(Cond,Actn), Caller) :-
    !,
    storeXrefInfoForPreds(Cond, Caller),
    storeXrefInfoForPreds(Actn, Caller).
storeXrefInfoForPreds('freeze'(_,Goal), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds('when'(_,Goal), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds('thread_create'(Goal,_,_), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds('thread_at_exit'(Goal), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds('pce_call'(Goal), Caller) :-
    !,
    storeXrefInfoForPreds(Goal, Caller).
storeXrefInfoForPreds(SingleNonMetaLiteral, Caller) :-
    % Phuu... Finally a literal that's not a metapredicate:
    storeXrefInfoForPred_(Caller, SingleNonMetaLiteral),
    !.

%:- endif . % End of conditional compilation 
    
/*
 * Store Xref Info for one pair of caller (arg1) and called 8arg2) pred.
 * The called pred (arg2) is not a metapredicate.
 */

storeXrefInfoForPred_(_Caller,true) :- !.	 % don't store anything for facts 

%storeXrefInfoForPred_(_Caller,Called) :-     % Don't store anything for built-ins
%     predicate_property(Called,built_in),	 % DM: Why not store anything for built-ins??
%     !.     

     
storeXrefInfoForPred_(Caller,Called) :-     % Store pred and file info
    predicate_property(Caller,file(CallerFile)),
    predicate_property_switch(Called,file(CalledFile)),
    strip_module(Caller,_,CallerPlain), functor(CallerPlain,CallerName,CallerArity),
    strip_module(Called,_,CalledPlain), functor(CalledPlain,CalledName,CalledArity),
    storeXrefInfoForPred___(CallerName/CallerArity,
                            CalledName/CalledArity,
                             CallerFile,
                             CalledFile),
    !.					% <- DM: Why is here a cut?
        
predicate_property_switch(Module:Literal, File):-
    predicate_property(Module:Literal,File).

predicate_property_switch(Literal, File):-
    not( Literal = _M:_H),
    ( predicate_property(_Module:Literal,File) 
      -> true  
       ; File = file(built_in)
    ).
	    

  /* Store only if all parameters are instantiated with ground terms,
     and the same fact is not yet in the xref database.
  */
storeXrefInfoForPred___(CallerName/CallerArity,
                         CalledName/CalledArity,
                         CallerFile,
                         CalledFile):-
    ground(test(CallerName/CallerArity,
                CalledName/CalledArity,
                CallerFile,
                CalledFile) ),
    % GK: The following negation would prevent correct analyses for
    % unused predicate definitions, since it would ignore the
    % uses within the same file. It MUST NOT be reactivated!
    % not( CallerFile = CalledFile),
    extractBasePath_(CallerFile,CallerShort),
    extractBasePath_(CalledFile,CalledShort),
    assert_unique_fact( db_xrefFromTo(
                          CallerName,CallerArity,
                          CalledName,CalledArity,
                          CallerShort,
                          CalledShort) ).

/*
 * If File has a known prefix, remove the prefix for better readability.
 */
extractBasePath_(File,Short) :-
    xrefBasePath(Prefix),
    atom_concat(Prefix,Short,File),
    !.
extractBasePath_(File,File) .

/*
 * xrefBasePath(absolutePath)
 *
 * The prefix arg1 is removed from all file names stored in the xrefDb
 * for better readability. It should be the common root directory
 * of a group of analyzed files (e.g. an eclipse project directory).
 *
 * xrefBasePath/1 is assumed to be defined by xref clients, e.g. in the
 * crossrefTest.pl file.
 */

:- multifile(xrefBasePath/1).
:- dynamic(xrefBasePath/1).

/*
  * storeXrefInfoForFiles/0
  *   Stores the results of xrefInfoForFiles(CallerFile,CalledFile,CalledPreds)
  *   as db_xrefFromToFiles(CallerFile,CalledFile,CalledPreds) facts to be
  *   queried later. ALWAYS SUCEEDS!
  *
  *   As an alternative it is possible to use
  *   xrefInfoForFiles(CallerFile,CalledFile,CalledPreds) directly.
  *   That might give better performance if the Prolog Database
  *   is changed very often.
  */
storeXrefInfoForFiles :-
     xrefInfoForFiles_(CallerFile,CalledFile,CalledPreds),
        assert(db_xrefFromToFiles(CallerFile,CalledFile,CalledPreds)),
     fail.
storeXrefInfoForFiles.

%storeXrefInfoForFiles :-
%    xrefInfoForFiles(List),
%    assert_all(List).

 /*****************************************************************************
  * xrefInfoForFiles(List)
  *    Arg1 is a sorted list of terms of the form
  *       db_xrefFromToFiles(CallerFile,CalledFile,CalledPreds)
  *    where the predicates in the list CalledPreds are defined in CalledFile,
  *    The List is sorted in ascending order by normal term comparison criteria.
  *
  *    This predicate is computed dynamically. As an alternative it is
  *    possible to call storeXrefInfoForFiles/0 and then
  *    query db_xrefFromToFiles(CallerFile,CalledFiles,CalledPreds).
  *    That might give better performance if the Prolog Database is
  *    *not* changed very often.
  */
xrefInfoForFiles(List) :-
    setof( db_xrefFromToFiles(CallerFile,CalledFile,CalledPreds),
           xrefInfoForFiles_(CallerFile,CalledFile,CalledPreds),
           List).
    % print_linewise(List).

 /*
  * xrefInfoForFiles_(?CallerFile,?CalledFile,?CalledPreds)
  *    The predicates in the list arg3, defined in file arg2,
  *    are called from predicates in file arg1.
  *    The predicates in arg3 are represented as functor/arity terms.
  */

xrefInfoForFiles_(CallerFile,CalledFile,CalledPreds) :-
    setof( CalledName/CalledArity,
           _callingN^_callingA^db_xrefFromTo(_callingN, _callingA, 
                                           CalledName,CalledArity, 
                                           CallerFile, CalledFile),
           CalledPreds).

/*
  * storeXrefInfoForFilesOnly
  *    Stores the results of xrefInfoForFilesOnly_(CallerFile,CalledFiles)
  *    as db_xrefFromToFilesOnly(CallerFile,CalledFiles) facts to be
  *    queried later. ALWAYS SUCEEDS!
  *
  *    As an alternative it is possible to use
  *    xrefInfoForFilesOnly(CallerFile,CalledFiles) directly. That might
  *    give better performance if the Prolog Database is changed often.
  */
storeXrefInfoForFilesOnly :-
    xrefInfoForFilesOnly(CallerFile,CalledFiles),
       assert(db_xrefFromToFilesOnly(CallerFile,CalledFiles)),
    fail.
storeXrefInfoForFilesOnly. 


 /****************************************************************************
  * xrefInfoForFilesOnly(CallerFile,CalledFiles)
  *    Arg2 is the list of files containing predicates called from
  *    predicates in file arg1. 
  *
  *    This list is computed dynamically. As an alternative it is 
  *    possible to call first storeXrefInfoForFilesIOnly_/0 and then 
  *    query db_xrefFromToFilesOnly(CallerFile,CalledFiles). That might
  *    give better performance if the Prolog Database is *not* changed
  *    very often. 
  */      
xrefInfoForFilesOnly(CallerFile,CalledFiles) :-
    setof( CalledFile, 
           _preds^db_xrefFromToFiles(CallerFile, CalledFile,_preds),
           CalledFiles).    
point4.


