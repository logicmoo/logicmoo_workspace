/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Tobias Rho, Andreas Becker, Fabian Noth
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/


:- module( pdt_reload,
         [ pdt_reload/1                           % Called from ConsultActionDelegate.run()
         , pdt_reload/2                           % Called from ConsultActionDelegate.run()
%         , activate_warning_and_error_tracing/0   % Called from PLMarkerUtils.addMarkers()
%         , deactivate_warning_and_error_tracing/0 % Called from PLMarkerUtils.addMarkers()
         , errors_and_warnings/5                  % Called from PLMarkerUtils.run()
         , reloaded_file/1
         , wait_for_reload_finished/0
         ]).


               /*************************************
                * PDT RELAOAD                       *
                *************************************/

:- use_module(split_file_path).
:- use_module(library(lists)).
:- use_module(library(memfile)).
:- use_module(library(debug)).
:- if(current_prolog_flag(dialect, swi)).
% SWI-Prolog
:- use_module(library(make), []).
:- endif.
:- op(600, xfy, ::).   % Logtalk message sending operator

%% pdt_reload(+File) is det.
%
% Wrapper for consult used to ignore PLEditor-triggered consults in the
% console history and to start the update of the PDT-internal factbase.

% SWI-Prolog list    

:- multifile(reload_message/2).

pdt_reload(FileOrFiles, MessageTerm) :-
	(	nonvar(MessageTerm)
	->	(	atomic(MessageTerm)
		->	Message = MessageTerm
		;	(	reload_message(MessageTerm, Message)
			->	true
			;	Message = MessageTerm
			)
		),
		write(user_error, Message),
		nl(user_error)
	;	true
	),
	pdt_reload(FileOrFiles).

pdt_reload(FileOrFiles) :-
	with_mutex('reloadMutex',(
		setup_call_cleanup(
			activate_warning_and_error_tracing,
			pdt_reload__(FileOrFiles),
			deactivate_warning_and_error_tracing
		)
	)),
	notify_reload_listeners(FileOrFiles).

pdt_reload__(Files):-
    is_list(Files),
    !,
    forall(member(F,Files), pdt_reload__(F)).
	
% Logtalk
pdt_reload__(File):-
    split_file_path(File, _Directory,_FileName,_,lgt),
    !,
    logtalk_reload_adapter::pdt_reload(File),
    assertz(reloaded_file__(File)).

:- if(current_prolog_flag(dialect, swi)).
% SWI-Prolog
pdt_reload__(File):-
	debug(pdt_reload, 'pdt_reload(~w)', [File]),
	
	% we have to continiue, even if reload_file fails
	% normally failing means: the file has errors
	(make:reload_file(File) -> true ; true).
:- else.
pdt_reload__(File):-
	debug(pdt_reload, 'pdt_reload(~w)', [File]),
	
	% we have to continiue, even if reload_file fails
	% normally failing means: the file has errors
	(user:consult(File) -> true ; true).
:- endif.

:- multifile(pdt_reload_listener/1).

notify_reload_listeners(Files) :-
	(	is_list(Files)
	->	pdt_reload_listener(Files)
	;	pdt_reload_listener([Files])
	),
	fail.
notify_reload_listeners(_).

%pdt_reload_listener(Files) :-
%    atomic_list_concat(Files, '<>', FileList),
%    catch(process_observe:process_notify(file_loaded,FileList),_,true).

               /*************************************
                * INTERCEPT PROLOG ERROR MESSAGES   *
                *************************************/

% Store SWI-Prolog error and warning messages as
% traced_messages(Level, Line, Lines, File) facts.

:- dynamic(traced_messages/5).
:- dynamic(warning_and_error_tracing/0).
:- dynamic(reloaded_file__/1).

activate_warning_and_error_tracing :- 
    trace_reload(begin),
    assertz(in_reload),
	retractall(traced_messages(_,_,_,_,_)),
	retractall(reloaded_file__(_)),
	assertz(warning_and_error_tracing).

deactivate_warning_and_error_tracing :-
	retractall(in_reload),
	retractall(warning_and_error_tracing).
 
:- dynamic in_reload/0.

%% message_hook(+Term, +Level,+ Lines) is det. 
%
% intercept prolog messages to collect term positions and 
% error/warning messages in traced_messages/5
% 
% @author trho
%  

:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).

user:message_hook(Term, Level,Lines) :-
    with_mutex('reloadMutex', (
		warning_and_error_tracing,
		(	Term = error(_, file(File, Line, _, _))
		->	true
		;	prolog_load_context(term_position, TermPosition),
			(	TermPosition = '$stream_position'(_,Line,_,_,_)
			->	true
			;	TermPosition = '$stream_position'(_,Line,_,_)
			),
			prolog_load_context(source, File)
		),
		assertz(traced_messages(swi, Level, Line,Lines, File)),
		trace_reload(traced_messages(swi, Level, Line,Lines, File)),
	%	assertz(user:am(_Term, Level,Lines)),
		fail
	)).


user:message_hook(load_file(start(_, file(_, FullPath))), _, _) :-
	with_mutex('reloadMutex', (
		warning_and_error_tracing,
		assertz(reloaded_file__(FullPath)),
		fail
	)).
               /*************************************
                * USE INTERCEPTED PROLOG ERROR MESSAGES   *
                *************************************/

%% errors_and_warnings(?Level,?Line,?Length,?Message,?File) is nondet.
%
errors_and_warnings(Level,Line,0,Message, File) :-
		wait_for_reload_finished,
	    traced_messages(swi, Level, Line, Lines, File),
	    trace_reload(e_w(Lines)),
	%	traced_messages(error(syntax_error(_Message), file(_File, StartLine, Length, _)), Level,Lines),
	    new_memory_file(Handle),
	   	open_memory_file(Handle, write, Stream),
		print_message_lines(Stream,'',Lines),
	    close(Stream),
		memory_file_to_atom(Handle,Message),
	    free_memory_file(Handle).

errors_and_warnings(Level,Line,0,Message, File) :-
	wait_for_reload_finished,
	traced_messages(logtalk, Level, Line, Tokens, File),
	with_output_to(atom(Message), (current_output(S), logtalk::print_message_tokens(S, '', Tokens))).

reloaded_file(LoadedFile) :-
	wait_for_reload_finished,
	reloaded_file__(LoadedFile).
   
wait_for_reload_finished :-
   reset_timout_counter,
   repeat,
   ( with_mutex('reloadMutex', (
      trace_reload(check_in_reload),
       \+in_reload
     ))
    ; ( 
        %writeln(wait_for_reload_to_end),
        trace_reload(wait),
        sleep(0.1),
        ( timeout_reached(Timeout) ->
          throw(reload_timeout_reached(Timeout))
        ; fail 
        )
	  )
	),
    !.


:- dynamic timeout_counter/1.
timeout_threshold(150).

reset_timout_counter :-
   retractall(timeout_counter(_)),
   assert(timeout_counter(0)).
   
   
timeout_reached(New) :-
   timeout_counter(C),
   New is C+1,
   timeout_threshold(Th),
  ( Th == New ->
     true
    ; ( retractall(timeout_counter(_)),
        assert(timeout_counter(New)),
        fail
    )
   ).

% If you want to trace reloading comment out the "fail"
% in the first line of "trace_reload" and then look at
% the reload_trace(What,Time) facts generated. 
% It makes no sense to add a special preference to enable
% reload tracing since this only interests PDT developers, 
% not end users: 
:- dynamic reload_trace/2.

trace_reload(Name):-
    fail,
    get_time(T),
    assert(reload_trace(Name,T)),
    !.
trace_reload(_Name).    


