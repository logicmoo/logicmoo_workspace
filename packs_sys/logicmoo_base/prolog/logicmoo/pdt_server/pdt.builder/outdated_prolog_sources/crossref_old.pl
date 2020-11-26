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

before_version(Major,Minor,Patch) :-                           
   swi_pl_version(Major,Minor,Patch,MaxVersion),
   current_prolog_flag(version, CurrentVersion),
   CurrentVersion < MaxVersion . 

swi_pl_version(Major,Minor,Patch,Version) :-
     Version is 10000*Major + 100*Minor + Patch .

:- if( before_version(5,7,0) ).          

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
    storeXrefInfoForPred_(T, Caller).			% DM: Why store only T and not call(T)??

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


:- else .  % Version of storeXrefInfoForPreds/2 since SWI 5.7.x 


