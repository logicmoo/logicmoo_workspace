/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Fabian Noth
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module( pdt_support, [ pdt_support/1 ]).

:- if(current_prolog_flag(dialect,swi)).
	pdt_support(doc_collect).
	pdt_support(flag).
	pdt_support(count_inferences).
	pdt_support(tests).
	pdt_support(clause_property).
	pdt_support(last_call_optimisation).
	pdt_support(tty_control).

:- elif(current_prolog_flag(dialect,yap)).
	pdt_support(reverse_list).
	pdt_support(table).
	pdt_support(remove_duplicates).
	pdt_support(tty_control).

:- else.
	:- writeln('WARNING: unsupported Prolog dialect!\nSupported dialects are: SWI, YAP').

:- endif.


% pdt_support(last_call_optimisation).
%     current_prolog_flag(last_call_optimisation, X) is supported

% pdt_support(table).
%     tabling (and the table/1 directive) is supported
%     prints warning if not supported

% pdt_support(doc_collect).
%     doc_collect/1 is supported.
%     prints warning if not supported

% pdt_support(flag).
%     flag/3 is supported
%     alternative implementation is used if not supported

% pdt_support(tty_control).
%     current_prolog_flag(tty_control, X) is supported

% pdt_support(remove_duplicates).
%     remove_duplicates/2 is supported
%     alternative implementation is used if not supported

% pdt_support(count_inferences)
%	  statistics(inferences, I) is supported


% pdt_support(tests).

% pdt_support(clause_property).

% pdt_support(reverse_list).





:- if( \+ pdt_support(doc_collect) ).
user:doc_collect(true) :-
	writeln('Warning: doc_collect/1 not supported by current prolog system').
:- endif.

:- if( \+ pdt_support(table)).
user:table(X) :-
	write('WARNING: tabling not supported in current prolog version (predicate: '),
	write(X),
	write(')\n').
:- endif.

:- if(\+ pdt_support(flag)).
user:flag(Name, _, _) :-
	var(Name),
	throw(instantiation_error(Name)), !.

user:flag(Name, OldValue, NewValue) :-
	nb_current(Name, OldValue),
	nonvar(NewValue),
	nb_setval(Name, NewValue), !.
	
user:flag(Name, OldValue, NewValue) :-
	OldValue = 0,
	nonvar(NewValue),
	nb_setval(Name, NewValue), !.
:- endif.
