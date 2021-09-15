:- encoding(iso_latin_1).
/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Günter Kniesel, Frank Mühlschlegel (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

% Date: 13.02.2006, 04.02.2009

% TODO: Move all logging primitives here.

:- module(logging, [
	ctc_warning/2,
	ctc_error/2,
	log_on_stdout/1,
	log_on_stdout/2,
	do_if_logging_enabled/1
]).
   
:- use_module(library(lists)).
:- use_module(files).

consult_silent_if_logging_disabled(FileOrFiles) :-
    (  not(loggingEnabled) 
    -> consult_silent(FileOrFiles)          % load without printing a  message.
    ;  consult(FileOrFiles)                 % report filename, size, etc.
    ).
    
consult_silent(FileOrFiles) :-              % used for loading test data.
    load_files(FileOrFiles, [silent(true)]).
  
/*
 * Execute Goal and redirect all output to a file. For enforcing 
 * logging during execution of Goal wrap calls to this predicate   
 * as "do_with_logging_enabled(log_once_to_file(File,Goal))".
 */
log_once_to_file(File,Goal) :- 
   with_output_to_file( File, do_with_logging_enabled(Goal) ).

/* ======================================================================== 
 * Control of logging
 * ======================================================================== */
:- dynamic loggingEnabled/0.

:- if(pdt_support:pdt_support(last_call_optimisation)).
enable_logging :- 
   retractall(loggingEnabled),                    % prevent accidental backtracking(!)
   set_prolog_flag(last_call_optimisation,false), % important for retrieval of parentmodule
   assertz(loggingEnabled).

disable_logging :- 
   set_prolog_flag(last_call_optimisation,true),
   retractall(loggingEnabled).
:- else.
enable_logging :- 
   retractall(loggingEnabled),      % prevent accidental backtracking(!)
   assert(loggingEnabled).

disable_logging :- 
   retractall(loggingEnabled).
   
:- endif.   
:- enable_logging.                  % default: logging

/*
 * Only call a goal (that produces console output) if logging is enabled.
 * Indended for use with 'listing' and friends.
 */
 
:- meta_predicate( do_if_logging_enabled(0) ).
 
do_if_logging_enabled(Goal) :-
   (
   	(clause(loggingEnabled,_),check_logging_module)
   		-> call(Goal) ; true 
   ). 
    
/*
 * Do with logging ENABLED and reset logging status afterwards.
 */
verbose(Goal) :- 
   do_with_logging_enabled(Goal).
   
do_with_logging_enabled(Goal) :-
   ( loggingEnabled 
      -> (               enable_ignore_logging_in_module, call(Goal),                  disable_ignore_logging_in_module) 
       ; (enable_logging,enable_ignore_logging_in_module, call(Goal), disable_logging, disable_ignore_logging_in_module)
   ).

/*
 * Do with logging DISABLED and reset logging status afterwards.
 */
silent(Goal) :- 
   do_with_logging_disabled(Goal).
   
do_with_logging_disabled(Goal) :-
   ( loggingEnabled 
      -> (disable_logging, call(Goal), enable_logging)
       ; call(Goal) 
   ).


/* ======================================================================== 
 * Control of module
 * ======================================================================== */
/*
 *  Disables / Enables logging for an specific module
 */
:- dynamic mod_is_disabled/1.
:- dynamic mod_is_enabled/1. 
 
disable_logging_in_module(Module):- 
	current_module(Module),
	retractall(mod_is_enabled(Module)),
	assertz(mod_is_disabled(Module)).

is_logging_in_module_disabled(Module):-
    current_module(Module),
    not(mod_is_enabled(Module)),
    mod_is_disabled(Module).

enable_logging_in_module(Module):-
	current_module(Module),
	assertz(mod_is_enabled(Module)),
	retractall(mod_is_disabled(Module)).

:- dynamic(mod_ignore_is_enabled/0).

enable_ignore_logging_in_module:-
    retractall(mod_ignore_is_enabled),
    assertz(mod_ignore_is_enabled).
    
disable_ignore_logging_in_module:-
    retractall(mod_ignore_is_enabled).

is_ignore_logging_in_module:-
	clause(mod_ignore_is_enabled,_).    
	
/*
 *  Enables logging for all  modules
 */
reset_logging_all_modules:-
	retractall(mod_is_enabled(Module)),
	retractall(mod_is_disabled(Module)).

:- dynamic(showContextModules_enabled/0).
/*
 *  Disables / Enables the out of the hierachie of the contextmodule 
 */
disable_showLoggingContextModules:-retractall(showContextModules_enabled).
enable_showLoggingContextModules:-
	assertz(showContextModules_enabled),
	format('WARNING: === ShowContextModules enabled =========================~n'),
    format('WARNING: This function will double your output and will slow down your console~n'),
    format('WARNING: ==============================================~n').
	

/*
 *  check if the call-context-module is disabled 
 */
check_logging_module:-
    getContextModules(ContextModules),
    member_logging_enabled(ContextModules).
%    forall(
%    	member(Module,ContextModules),
%    	not(clause(mod_is_disabled(Module),true))
%    ).

member_logging_enabled([]) :-
    loggingEnabled.
member_logging_enabled([Module|T]) :-
   ( mod_is_disabled(Module) 
     -> fail 
     ; ( mod_is_enabled(Module) -> true ; member_logging_enabled(T) )
   ).
   
/*
 *  Rekusive predicate to get the list of context-modules 
 */
getContextModules(ParentModules):-   
    prolog_current_frame(Self),
    getParentContextModule(Self, ParentModules).
     	
getParentContextModule(Frame, ParentModules):-   
    prolog_frame_attribute(Frame,parent,Parent),
    prolog_frame_attribute(Parent,context_module,Module),    
    (  prolog_frame_attribute(Parent,top,false)
     -> ( getParentContextModule(Parent, ParentParentModules),    
          ( not(member(Module,ParentParentModules))
      		-> (append([Module],ParentParentModules,ParentModules))
      		 ;
      		ParentModules=ParentParentModules
      	  )
        )     
     ;
       ParentModules = [Module]
    ).   
showContextModules:-
    clause(showContextModules_enabled,true)->
    (
    	getContextModules(Modules),
    	format('INFO: context-modules are ~w~n',[Modules]))
    	;
    true. 
     
/* ======================================================================== 
 * Controlled logging
 * ======================================================================== */

/*
 * Printing of errors and warnings and logging of assert operations.
 */   
ctc_error(Formatterm,Atomlist) :-
    atom_concat('~n *** ERROR: ',Formatterm,Formatterm2),
    atom_concat(Formatterm2,'~n',NewFormatterm),
    write_on_stdout(NewFormatterm,Atomlist).
 
ctc_warning(Formatterm,Atomlist) :-
    atom_concat('~n *** WARNING: ',Formatterm,Formatterm2),
    atom_concat(Formatterm2,'~n',NewFormatterm),
    log_on_stdout(NewFormatterm,Atomlist).

 
ctc_info(Formatterm,Atomlist) :-
    atom_concat('~n *** INFO: ',Formatterm,Formatterm2),
    atom_concat(Formatterm2,'~n',NewFormatterm),
    log_on_stdout(NewFormatterm,Atomlist).

assert_logging(Fact) :-
    log_on_stdout(' --- Asserting: ~k.~n', Fact ),
    assertz(Fact).


/*
 * Basic output predicates including conditional logging to stdout 
 * and to any stream. 
 */               

write_on_stdout(Formatterm) :-
    write_on_stdout(Formatterm,[]).
    
write_on_stdout(Formatterm,Atomlist) :-
    current_output(Stream),
    format(Stream,Formatterm,Atomlist). 

log_on_stdout(Formatterm) :-
    log_on_stdout(Formatterm,[]).
         
log_on_stdout(Formatterm,Atomlist) :-
    current_output(Stream),
    log(Stream,Formatterm,Atomlist).    

log(Stream,Formatterm,Atomlist) :-
    (loggingEnabled,(check_logging_module;is_ignore_logging_in_module)) 
      -> (showContextModules,format(Stream,Formatterm,Atomlist))
       ; true.  
    
     

% Unneeded generality for condor.pl:
%
%switchOffLoggingIfDesired(_uniqueNr,_what) :-  % Don't ask if < 100 results.
%      _uniqueNr <100,
%      !.
%switchOffLoggingIfDesired(_uniqueNr,_what) :-  % Ask whether to disable logging.
%      loggingEnabled,
%      !,
%      log(_stream,'~n --- Printing ~a different ~a might take a while... ', 
%          [_uniqueNr, _what]),
%      log('Skip logging for the rest of this analysis? [yes. / no.]:~n', 
%          []),
%      read(Answer),
%      ( ( Answer='yes') -> 
%         (log(_stream, 'Logging disabled...~n', []), disable_logging)
%        ; true ).
%switchOffLoggingIfDesired(_uniqueNr,_what) .  % Go on logging if in batch mode.
%


