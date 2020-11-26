/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: G�nter Kniesel (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

% Date:   12.06.2004

/* ***********************************************************
 * Different semantic and implementation variants of counting:
 *  - Semantic: Counting derivations (success) versus results.
 *  - Implementation: Based on nb_setargs/3, flag/3 or findall/3.
 * *********************************************************** */
:- module(count, [
	count_facts/2,
	count_success/2,
	count/2,
	count_and_print/2,
	count_unique/3,
	count_all_and_unique/3
]).

:- use_module(library(lists)).
 
count_facts(Goal, Nr) :-
  predicate_property(Goal, number_of_clauses(Nr)).

:- meta_predicate count(0, -).   
count(Goal, Times) :- count_successes(Goal, Times).

:- meta_predicate count_success(0, -).   
count_success(Goal, Times) :- count_successes(Goal, Times).
   
:- meta_predicate count_successes(0, -).                                    
count_successes(Goal, Times) :-    
	nb_setval(successcounter, 0),
    (	catch(Goal,_Any,fail),     % turn exceptions into failures
    		nb_getval(successcounter, N),
    		N2 is N + 1,
    		nb_setval(successcounter, N2),
    	fail
	;	nb_getval(successcounter, Times)
	).
  

:- module_transparent count_and_print/2.

count_and_print(Goal, N) :-
	nb_setval(successcounter,0),
	(	catch(Goal,_Any,fail),     % turn exceptions into failures
  			nb_getval(successcounter,N),
    		N2 is N + 1,
    		nb_setval(successcounter,N2),
    		format('~w.~n', [Goal]),
		fail
    ; 	nb_getval(successcounter,N)
    ).
  
/* ***************************************************************
   Findall-based counting. 
   ***************************************************************
   Inappropriate for large factbases but useful for counting
   but useful for counting without duplicates -- which is 
   currently not supported by count/2, count_facts/2 above.
*/  
  
:- module_transparent count_unique/2.

count_unique(Goal,Nall,Nunique) :-
  findall(Goal, catch(Goal,_Any,fail), All),    % turn exceptions into failures
  sort(All,Unique),
  length(All,Nall),
  length(Unique,Nunique).

/*
?- count(create_generic_edges:parent(Id,EdgeVal,NodeType,TargetType),N).
N = 70894 ;

?- count( (ast_node_type_dummy(Id,Type), not( create_generic_edges:parent(Id, EdgeVal, Type, TargetType) )),N).
N = 29742 ;

?- X is 29742 + 70894.

X = 100636 

?- count( ast_node_type_dummy(_,_) , N).
N = 100636 ;
*/



/* *
 * count_all_and_unique(+Goal,Nall,Nunique)
 *   Find all results and all unique results as lists and count
 *   them using count_list_elements.
 */
% Count exceptions as failures:
:- module_transparent count_unique/2.

count_all_and_unique(Goal,Nall,Nunique) :-
  findall(Goal, catch(Goal,_Any,fail), All),  
  count_list_elements(All,_SortedUnique,Nall,Nunique).


/*
 * count_list_elements(+All,?SortedUnique,?LengthAll,?LengthSortedUnique)
 *   Return sorted, duplicate-free list and lenght of both lists.
 */   
count_list_elements(All,SortedUnique,LengthAll,LengthSortedUnique) :-
  sort(All,SortedUnique),
  length(All,LengthAll),
  length(SortedUnique,LengthSortedUnique).
  
/* ********************************************************** 
   Experimental stuff, for grouping before counting. Useful
   for instance, for determining singular groups,  
*/
   

/* *
 * group_by(+Other, +Goal, ?OtherVals)
 *   Other is a term containing variables of Goal whose 
 *   values we want to determine. OtherVals will contain
 *   one instance of Other for each group of same values
 *   for the remaining variables og Goal.
 */ 
group_by(Other, Goal, OtherVals) :-
    bagof(Other, call(Goal), OtherVals ).

