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

:- module(listing, [
	listing_if_defined/1,
	listing_if_defined/1,
	list_defined_pred_if_logging_enabled/2
]).

:- if(current_prolog_flag(dialect, swi)).
:- use_module(library(listing)).
:- endif.

:- use_module(logging).
:- use_module(general).
:- use_module(utils4modules).

/*
 * listing*(?Module,+FunctorSubstring,?Arity)
 * 
 * Call listing(Module:Functor/Arity) for each currently defined
 * predicate whose functor contains the substring FunctorSubstring.
 * Throws an exception "secondArgumentNotAtomic(Call)" if 
 * +FunctorSubstring is not atomic. 
 */
listing*(Module,FunctorSubstring,Arity) :-
    atomic(FunctorSubstring)
    -> all( ( 
         current_predicate(Module:Functor/Arity),
         sub_atom(Functor, _Start, _Length, _After, FunctorSubstring),
         listing(Module:Functor/Arity)
       ) )
    ;  throw( secondArgumentNotAtomic( listing*(Module,FunctorSubstring,Arity) ) )
    .
    
/*
 * listing_if_defined(+Functor)
 * listing_if_defined(+Functor/+Arity)
 * listing_if_defined(+Head)
 *
 * If a predicate corresponding to Arg1 is defined, its current
 * definition is listed with the built-in predicate listing/1.
 * Otherwise, this call simply succeeds instead of throwing an
 * exception (unlike listing/1).
 */ 
listing_if_defined(Pred) :-
    catch(listing(Pred),_AnyException,true).

/*
 * As above but only list clauses in specified module.
 */
listing_if_defined_in_module(Module,Pred) :-
%    term_to_atom(Pred,P),
%    format('~n   Clauses of predicate ~a in module ~a: ',[P, Module]),
    catch(listing_in_module(Module,Pred),_AnyException,true). 

/*
 * As above but only list clauses if logging enabled.
 */
list_defined_pred_if_logging_enabled(Module,Pred) :-
    do_if_logging_enabled(listing_if_defined_in_module(Module,Pred)).
    


