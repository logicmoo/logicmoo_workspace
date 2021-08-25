/*
 *	file:		interrupt.pl
 *	version:	1.0
 *	date:		March 9, 1990
 *	creation:	March 9, 1990
 *	author:		Uwe Waldmann (uwe)
 *
 *	description:
 *	(see below)
 *
 *	history:
 *	900322	js	Added parameter to establish_handler
 *			Cleaned up inconsistencies in naming
 *			Created module interface
 *
 *	Copyright (C) 1990
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% The predicates in this file are used to control the interrupt menu of
% the cec system.  During completion this menu is enhanced by a further
% option "q" ("quit completion").  If the user chooses this option, the
% completion should be terminated as soon as possible.
% 
% 
% Most of the predicates use a global C variable "quit_completion"
% having three posible states:
% 
%    DISABLED      On typing ^C, the user gets the standard interrupt menu.
%
%    ENABLED       On typing ^C, the user has the additional possibility
%                  to type "q" in order to indicate that the completion
%                  should be terminated.
%
%    USED          The "q" option has already been used
% 
% 
% Description of the predicates:
% 
%    establish_handler(+X)
%	installs the new interrupt handler.
%	Set X = 0 for Quintus Prolog Development System
%		  (trace and debug options enabled)
%	or  X = 1 for Quintus Prolog Runtime Generator
%		  (trace and debug options disabled)
%
%    completion_interrupted
%	succeeds, if quit_completion = USED.
%  
%    completion_interrupted(-X)        
%	unifies X with 0, if quit_completion = DISABLED,
%	unifies X with 1, if quit_completion = ENABLED,
%	unifies X with 2, if quit_completion = USED.
%  
%    enable_q_option     quit_completion := ENABLED; succeeds.
%                        (This predicate should be called immediately
%                        after the completion has been started.)
%  
%    disable_q_option    quit_completion := DISABLED; succeeds.
%                        (This predicate should be called immediately
%                        before the completion terminates (no matter whether
%                        it succeeds, fails, or calls "abort"!)% 
%
% 
% The following options from the interrupt menu may modify quit_completion:
% 
%    q   (quit completion)   if quit_completion = ENABLED
%                            then quit_completion := USED.
%  
%    a   (abort)             quit_completion := DISABLED.
% 
% All other options do not change quit_completion.
% 

:- module(interrupt,[establish_handler/1,
                     completion_interrupted/0,
                     disable_q_option/0,
                     enable_q_option/0]).


foreign_file('interrupt.o', [establish_handler,
			     completion_interrupted,
			     disable_q_option,
			     enable_q_option]).

foreign(establish_handler, c, establish_handler(+integer)).
foreign(completion_interrupted, c, completion_interrupted([-integer])).
foreign(disable_q_option, c, disable_q_option).
foreign(enable_q_option, c, enable_q_option).

:- load_foreign_files(['interrupt.o'], []).


completion_interrupted :- completion_interrupted(2).
