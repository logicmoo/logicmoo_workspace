:- encoding(iso_latin_1).
/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Günter Kniesel (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

% Date: 25.06.2006

/*
 * This file contains predicates for working with SWI-Prolog modules.
 */
:- module( utils4modules, [

           assert_in_module/2,             % Module, Head
           assert_in_module/3,             % Module, Head, Body
           clause_in_module/2,             % Module, Head
           clause_in_module/3,             % Module, Head, Body
           retract_in_module/2,            % Module, Head
           retract_in_module/3,            % Module, Head, Body
           retractall_in_module/2,         % Module, Head
           call_in_module/2,               % Module, Goal
           call_and_report_contex_module/1,% Goal
           report_contex_module/1,         % Module        
           listing_in_module/2,            % Module, FunctorOrHeadOrFkt/Arity
           
           copy_module_predicate/3,        % SrcMod, TargetMod, Head
           move_module_predicate/3         % SrcMod, TargetMod, Head
           ]
 ).
 
:- use_module(library(lists)).
:- use_module(pdt_support, [pdt_support/1]).
:- use_module(logging).
:- use_module(database).
:- use_module(utils4modules_visibility).

:- module_transparent(call_and_report_contex_module/1).
call_and_report_contex_module(Goal) :- 
    context_module(M),
    log_on_stdout('Calling ~w.~n',[M:Goal]),
    call(M:Goal).

:- module_transparent(report_contex_module/1).
report_contex_module(M) :- context_module(M),
    log_on_stdout('Context module = ~w.~n',[M]).
    
   
    
/*
 * call_in_module(+Module, +Head) is nondet
 * 
 * Call Head in Module regardless whether Head is locally defined or
 * imported from another module. 
 */
call_in_module(Module,Goal) :- 
   nonvar(Module)
   -> call( Module:Goal )
    ; ctc_error('Goal called in variable module: ~w:~w.', 
                 [Module,Goal]
   ).

   
/*
 * assert_in_module(?Mod,?Head      ) is det
 *
 * Assert clauses in an explicitly specified module. 
 * 
 * CAUTION: Due to the semantics of modules in SWI-Prolog, the  
 * clause ends up in the module from which the explicitly specified 
 * module imports the declaration of the predicate to be asserted.
 * 
 * assert_in_module/2,3 differs from a normal assert called in a 
 * module, which would assert the fact into the module containing
 * the invocation of assert (unless the predicate containing the 
 * invocation and all its parents on the stack were "module_transparent"
 * and the invoking module was loaded via use_module ...). 
 */
assert_in_module(Mod,Head      ) :- assert( :(Mod,Head) ).
assert_in_module(Mod,Head,Body ) :- not(is_list(Body)), !, assert( :(Mod,':-'(Head,Body)) ).

assert_in_module(Mod,Head, []) :- assert_in_module(Mod,Head ).
    
assert_in_module(Mod,Head, [Opt]) :- 
   (  Opt == unique
   -> assert_unique( :(Mod,Head))
   ;  assert(        :(Mod,Head))
   ).
   
assert_in_module(Mod,Head,Body, [Opt]) :- 
   (  Opt == unique
   -> assert_unique( :(Mod,':-'(Head,Body)))
   ;  assert(        :(Mod,':-'(Head,Body)))
   ).


/*
 * Get or retract clauses from an explicitly specified module.
 * The Module argument must not be a variable! 
 * These predicates never access clauses imported from other modules.
 * They only get or delete clauses that are actually asserted in the
 * specified module. This appears to be standard SWI Prolog behaviour
 * meanwhile but it sometimes changed, so for safety we prefer to 
 * enforce it ourselves. 
 */
clause_in_module(Mod,Head	)   :- defined_in_module(Mod, Head), clause( :(Mod,Head),_ ) .
clause_in_module(Mod,Head,Body) :- defined_in_module(Mod, Head), clause( :(Mod,Head),Body ) .

retract_in_module(Mod,Head   )  :- defined_in_module(Mod, Head), retract( :(Mod,Head) ) .
retract_in_module(Mod,Head,Body):- defined_in_module(Mod, Head), retract( :(Mod,':-'(Head,Body)) ) .

retractall_in_module(Mod,Head)  :- defined_in_module(Mod, Head) -> retractall( :(Mod,Head) ) ; true.

listing_in_module(Module,Goal)  :- listing( Module:Goal ).
   
/*
 * Copy all clauses whose head unifies Arg3 from module Arg1 to 
 * module Arg2 without deleting the original clauses.
 */   
copy_module_predicate(InpMod, OutMod, Head) :-
   copy_predicate_clauses(InpMod:Head, OutMod:Head).  % SWI-PL
%
%   NON-SWIPL implementation:
% 
%   all( copy_module_clause(InpMod, OutMod, Head) ).
%  
%
%copy_module_clause(InpMod, OutMod, Head) :-
%   clause_in_module(InpMod,Head,Body),
%   assert_in_module(OutMod,Head,Body).  
   
/*
 * Move all clauses whose head unifies Arg3 from module Arg1 to 
 * module Arg2, deleting the original clauses.
 */   
move_module_predicate(FromModule, ToModule,Head) :-
   copy_module_predicate(FromModule, ToModule, Head),
   retractall_in_module(FromModule,Head).

/*
 * Replace all clauses whose old head unifies Arg2 from module Arg1  
 * and whose head unifies Arg2 .
 */       
replace_module_predicate(Module, Old, New) :-
	retract_in_module(Module, Old),
	assert_in_module(Module, New).    
	
/*
 * Tests:
 
assert_in_module(Mod,Head     , Goal) :- assert( :(Mod,Head)      ), call(Goal).
assert_in_module(Mod,Head,Body, Goal) :- assert( :(Mod,Head,Body) ), call(Goal).

% :- Dynamically created contents of user module globally visible (without module prefix): 
%       Mod = user, Head=uuu(1), Goal=uuu(X), assert_in_module(Mod,Head, Goal).

% :- Contents of other modules not visible without module prefix: 
%       Mod = mmmm, Head=uuu(2), Goal=uuu(X), assert_in_module(Mod,Head, Goal).

% :- Contents of other modules visible with explicit module prefix: 
%       Mod = mmmm, Head=uuu(3), Goal=mmmm:uuu(X), assert_in_module(Mod,Head, Goal).

% :- Dynamic creation of explicit module prefix: 
%       Mod = mmmm, Head=uuu(4), Goal=':'(mmmm,uuu(X)), assert_in_module(Mod,Head, Goal).

*/

