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

 /*
   * Assert the fact in arg1 only if it isn't there already. Succeed otherwise.
   */
:- module(database, [
	assert_unique_ground_fact/1, 
	assert_unique_fact/1,
	assert_unique/1
]).

:- use_module(logging).

:- module_transparent assert_unique_ground_fact/1, 
                      assert_unique_fact/1,
                      assert_unique/1.
                      
assert_unique_ground_fact(Head) :-
    ( ground(Head)
    -> assert_unique_fact(Head)
     ; ctc_error('Fact assumed to be ground is not: ~w',[Head])
    ). 

assert_unique(Head) :- 
   assert_unique_fact(Head) .
      
assert_unique_fact(Head) :-
% much slower than not(Head):    ( not(clause(Head,true)) 
    ( not(Head) 
      -> assert(Head)
      ;  true
    ).
% 

