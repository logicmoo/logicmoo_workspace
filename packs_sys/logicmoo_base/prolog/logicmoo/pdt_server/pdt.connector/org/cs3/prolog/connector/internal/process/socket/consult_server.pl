/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener, Fabian Noth (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

% Date: 23.10.2004

:- module(consult_server,[
	consult_server/1,
	consult_server/2,
	get_var_names/2
	]).


%:-debug(consult_server(startup)).
%:-debug(consult_server(shutdown)).
%:-debug(consult_server(accept_loop)).
%:-debug(consult_server(handler)).
%:-debug(handle_command).


:- use_module(library(socket)).
:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(charsio)).
:- use_module(library(debug)).

option_default(interprete_lists,true).
option_default(canonical,false).



:- dynamic option_value/2.
:- thread_local option_value/2.
option(Name,Value):-
    option_value(Name,AValue),
    !,
    Value=AValue.
option(Name,Value):-
	option_default(Name,Value).
	
set_option(Name,Value):-
	retractall(option_value(Name,_)),
	assert(option_value(Name,Value)).

unset_option(Name):-	
	retractall(option_value(Name,_)).
	
clear_options:-
	retractall(option_value(_,_)).	
		    


create_lock_file(Filename):-
	(	exists_file(Filename)
	->	debug(consult_server(init), 'Found existing lock file ~w.~n Shutting down...~n', [Filename]),		
		thread_signal(main,halt)
	;	open(Filename, write, Stream),
		call_cleanup(
			(	write(Stream,Filename),
				nl(Stream)
			),
			close(Stream)
		)
	).
	
delete_lock_file(Filename):-
	(	exists_file(Filename)
	->	delete_file(Filename)
	;	true
	).



	


:- multifile process_shutdown_hook/0.
:- dynamic process_shutdown_hook/0.

process_shutdown_hook.

call_shutdown_hook:-
    forall(process_shutdown_hook,true).
    
do_shutdown:-
   	debug(consult_server(shutdown), 'begin shutdown ~n', []),
   	call_shutdown_hook,
    %join any thread that is not main.
    (	current_thread(Id,_),
	    do_shutdown_X(Id),
       	fail
	;	debug(consult_server(shutdown), 'shutdown complete~n',[]),
		threads,
		halt
	).
do_shutdown_X(Id):-
    Id\==main,
    debug(consult_server(shutdown), 'joining ~w~n',[Id]),
    thread_join(Id,Status),
    debug(consult_server(shutdown), 'successfully joined ~w, status: ~w ~n', [Id,Status]).
    
	
consult_server(Port):- 
	tcp_socket(ServerSocket),
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	atomic_list_concat([consult_server,'@',Port],Alias),
	recordz(process_flag,port(Port),_),
	thread_create(accept_loop(ServerSocket), _,[alias(Alias)]).

consult_server(Port,Lockfile):-
	consult_server(Port),
	create_lock_file(Lockfile).


	
accept_loop(ServerSocket):-    
	catch(
		accept_loop_impl(ServerSocket),
		Error,
		(
			debug(consult_server(accept_loop), 'accept loop encountered an error:~w~n. Shutting down...~n',[Error]),
			thread_signal(main,halt)
		)
	),
	debug(consult_server(shutdown), 'signaling main to shutdown... ~n',[]),
	thread_signal(main,do_shutdown),
	debug(consult_server(shutdown), 'shutdown signal send, exit current thread. ~n',[]).


accept_loop_impl(ServerSocket) :-
	debug(consult_server(accept_loop), 'enter accept_loop_impl~n', []),
	tcp_accept(ServerSocket, Slave, Peer),
	debug(consult_server(accept_loop),'accepted inbound connection~n',[]),
	debug(consult_server(accept_loop),'connect from host ~w~n',[Peer]),
	accept_loop_impl_X(ServerSocket,Slave).

accept_loop_impl_X(ServerSocket,Slave):-
    debug(consult_server(shutdown),'Checking for shutdown flag. ~n',[]),
    recorded(process_flag,shutdown,_),    
    !,
    debug(consult_server(shutdown),'Shutdown flag is set. We are closing down. ~n',[]),
    tcp_close_socket(Slave),
    tcp_close_socket(ServerSocket).

accept_loop_impl_X(ServerSocket,Slave):-	
	debug(consult_server(accept_loop),'enter accept_loop_impl_X~n',[]),
	tcp_open_socket(Slave, InStream, OutStream),	
	debug(consult_server(accept_loop),'socket opened~n',[]),
	unused_thread_name(handle_client,'',Alias),
	debug(consult_server(accept_loop),'handler thread alias: ~w~n',[Alias]),		
	garbage_collect,
	debug(consult_server(accept_loop),'garbage collected~n',[]),
	debug(consult_server(accept_loop),'creating thread ~w.~n',[Alias]),
	thread_create(handle_client(InStream, OutStream), _ , [alias(Alias),detached(true)]),
	debug(consult_server(accept_loop),'successfully created thread ~w.~n',[Alias]),	
	accept_loop_impl(ServerSocket).

handler_at_exit(InStream,OutStream):-
	debug(consult_server(handler),'Thread is exiting. Trying to close the connection...~n',[]),
	catch(
		(	byebye(InStream,OutStream),
			debug(consult_server(handler),'Connection closed successfully. ~n',[])
		),
		E,
		debug(consult_server(handler),'Error encountered while closing the connection: ~w.~n',[E])
	).

    
	
handle_client(InStream, OutStream):-    
	thread_at_exit(handler_at_exit(InStream,OutStream)),			
    set_stream(InStream,encoding(utf8)),
    set_stream(OutStream,encoding(utf8)),
	repeat,
		debug(consult_server(handler),'start hanlde_client~n',[]),
		catch(
			handle_client_impl(InStream,OutStream),
			Error,
			(	handle_exception(InStream,OutStream,Error,Action),
				!,
				Action==stop
			)				
		),
	!,		
	debug(consult_server(handler),'Thread exiting...~n',[]).    
	
handle_client_impl(InStream, OutStream):-
    repeat,
		request_line(InStream,OutStream,'GIVE_COMMAND',Command),
		( handle_command(InStream,OutStream,Command,Next)
		->report_ok(OutStream)
		;	%report_error(OutStream, 'failed, sorry.'),
			Next=continue
		),
	Next==stop,
	!.
		

		
handle_command(_,_,'BYE',stop) :-	
	!.
handle_command(_,_,'SHUTDOWN',stop):-	
	!,
	% stop accept loop:
	% we set the shutdown flag (which is read by the accept loop)
	% then we have to kick the accept loop out of the tcp_accept/3 call.
	% we do this by simply opening a connection to the listen port.

	recordz(process_flag,shutdown,_),
	recorded(process_flag,port(Port),_),
	tcp_socket(Socket),
	tcp_connect(Socket,localhost:Port),
	tcp_close_socket(Socket).
handle_command(_,_,'',continue):-
	!,
	clear_options.
handle_command(_,OutStream,'PING',continue):-
	!,
	current_prolog_flag(pid,Pid),
    thread_self(Alias),
	my_format(OutStream,'PONG ~w:~w~n',[Pid,Alias]).
handle_command(InStream,OutStream,'ENTER_BATCH',continue):-
	!,
	my_format(OutStream,'GO_AHEAD~n',[]),
	repeat,
		handle_batch_messages(OutStream),
		my_read_command(InStream,Term),
		handle_batch_command(Term,InStream,OutStream),
		Term=end_of_batch,!.
handle_command(InStream,OutStream,'QUERY',continue):-
	!,
	debug('handle_command', 'before my_format', []),
	my_format(OutStream,'GIVE_TERM~n',[]),	
	debug('handle_command', 'after my_format', []),
	call_save(OutStream,my_read_term(InStream,Term,[variable_names(Vars)/*,double_quotes(string)*/])),
	( 
	(debug('handle_command', 'before iterate_solutions ~w', [Term]),
	iterate_solutions(InStream,OutStream,Term,Vars),
	debug('handle_command', 'after iterate_solutions ~w', [Term]))
	; true
	).
handle_command(InStream,OutStream,'QUERY_ALL',continue):-
	!,
	my_format(OutStream,'GIVE_TERM~n',[]),
	call_save(OutStream,my_read_term(InStream,Term,[variable_names(Vars)/*,double_quotes(string)*/])),		
	(
		all_solutions(OutStream,Term,Vars)
	;
		true
	).
handle_command(InStream,OutStream,'SET_OPTION',continue):-
	!,
	request_line(InStream,OutStream,'GIVE_SYMBOL',Symbol),
	request_line(InStream,OutStream,'GIVE_TERM',Term),
	call_save(OutStream,set_option(Symbol,Term)).
handle_command(InStream,OutStream,'GET_OPTION',continue):-
	!,
	request_line(InStream,OutStream,'GIVE_SYMBOL',Symbol),
	call_save(OutStream,
		(	option(Symbol,Term),
			my_format(OutStream,'~w~n',[Term])
		)
	).


my_read_command(InStream,Term):-
    my_read_term(InStream,Term,[]).

my_read_goal(InStream,Term,Vars):-
    my_read_term(InStream,Term,[variable_names(Vars)]).

		
handle_batch_messages(OutStream):-
    repeat,    
    (	thread_peek_message(batch_message(Message)),
    	debug(consult_server(handler),'recieved message: ~w~n',[Message]),
    	handle_batch_message(Message, OutStream)
    ->	thread_get_message(batch_message(Message)), 
		debug(consult_server(handler),'dequeued message: ~w~n',[Message]),
    	fail
    ;	true
    ),
    !.
    
%note on aborts: an abort request is complete if BOTH the async abort message aswell as the
%sync abort marker have been recieved. I originally assumed that the async message would always 
%preceed the marker, but it seems to be more tricky. So i will now handle this symetrically.

record_abort_request(Type,Id):-
    thread_self(Thread),
    (	recorded(process_batch_abort,request(Thread,Type,Id),_)
    ->	true
    ;	recordz(process_batch_abort,request(Thread,Type,Id),_)
    ).
    

erase_abort_request(Type,Id):-
    thread_self(Thread),
    (	recorded(process_batch_abort,request(Thread,Type,Id),Ref)
    ->	erase(Ref)
    ;	true
    ).
    
abort_requested(Type,Id):-
	thread_self(Thread),
	recorded(process_batch_abort,request(Thread,Type,Id),_).    

aborting:-
    abort_requested(async,_).

send_abort_complete(Id,OutStream):-    
   	my_format(OutStream, 'ABORT_COMPLETE: ~w~n',[Id]).
    
handle_batch_message(abort(Id),OutStream):-    
	debug(consult_server,'recieved abort (async, id=~w)~n',[Id]),
	(	abort_requested(sync,Id)
	->	erase_abort_request(sync,Id),
		send_abort_complete(Id,OutStream)
	;	record_abort_request(async,Id),
		debug(consult_server(handler),'recorded abort (async, id=~w)~n',[Id])
	).
handle_batch_command(set_option(Option,Value),_,OutStream):-
	call_save(OutStream,set_option(Option,Value)).
handle_batch_command(abort(Id),_,OutStream):-
    debug(consult_server(handler),'recieved abort (sync, id=~w)~n',[Id]),
	(	abort_requested(async,Id)
	->	erase_abort_request(async,Id),
		send_abort_complete(Id,OutStream)
	;	record_abort_request(sync,Id),
	    debug(consult_server(handler),'recorded abort (sync, id=~w)~n',[Id])
	).
handle_batch_command(join(Id),_,OutStream):-
	my_format(OutStream, 'JOIN_COMPLETE: ~w~n',[Id]).
handle_batch_command(end_of_batch,InStream,OutStream):-
	forall(abort_requested(async,Id),handle_batch_command(abort(Id),InStream,OutStream)),
	forall(abort_requested(sync,Id),handle_batch_message(abort(Id),OutStream)),
	my_format(OutStream, 'END_OF_BATCH_COMPLETE~n',[]).
handle_batch_command(query_once(Id),InStream,OutStream):-
	aborting,
    !,
    my_read_goal(InStream,_,_),    
    my_format(OutStream, 'SKIPPING_QUERY: ~w~n',[Id]).
handle_batch_command(query_all(Id),InStream,OutStream):-
    aborting,
    !,
    my_read_goal(InStream,_,_),
    my_format(OutStream, 'SKIPPING_QUERY: ~w~n',[Id]).
    
handle_batch_command(query_once(Id),InStream,OutStream):-
    my_format(OutStream, 'RESULTS_FOR_QUERY: ~w~n',[Id]),
    call_save(OutStream,(
    		my_read_goal(InStream,Goal,Vars),
    		one_solution(OutStream,Goal,Vars)
    		)
    	).
handle_batch_command(query_all(Id),InStream,OutStream):-
    my_format(OutStream, 'RESULTS_FOR_QUERY: ~w~n',[Id]),
    call_save(OutStream,(
    		my_read_goal(InStream,Goal,Vars),
    		solutions_weak_until_cut(OutStream,Goal,Vars)
    		)
    	).

call_save(OutStream, Goal):-
    catch(Goal,
	    Error,
    		report_error(OutStream,Error)
    	).

solutions_weak_until_cut(OutStream,Term,Vars):-
	(	solutions_until_cut(OutStream,Term,Vars)
	->	my_format(OutStream, 'CUT~n',[])
	;	solutions_yes_or_no(OutStream)
	).


solutions_until_cut(OutStream,Term,Vars):-
	user:Term,
	nb_setval(hasSolutions,1),
	print_solution(OutStream,Vars),
	goal_was_cut(OutStream),!.	

goal_was_cut(OutStream):-
	handle_batch_messages(OutStream),
	aborting.
	
solutions_yes_or_no(OutStream):-
	(	nb_current(hasSolutions,1)
	->	my_format(OutStream,'YES~n',[]),
		nb_delete(hasSolutions)
	; 	my_format(OutStream,'NO~n',[])
	).

	
	


one_solution(OutStream,Term,Vars):-
	( 	user:Term
	->	consult_server:print_solution(OutStream,Vars),
		my_format(OutStream,'YES~n',[])
	; 	my_format(OutStream,'NO~n',[])
	).
	
	
all_solutions(OutStream,Term,Vars):-
	user:forall(
		catch(Term,E,throw(wrapped(E))),
		(
			consult_server:print_solution(OutStream,Vars),
			nb_setval(hasSolutions,1)
		)		
	),
	(	nb_current(hasSolutions,1)
	->	my_format(OutStream,'YES~n',[]),
		nb_delete(hasSolutions)
	;	my_format(OutStream,'NO~n',[])
	).
	
	
iterate_solutions(InStream,OutStream,Term,Vars):-
	( user:forall(
			catch(Term,E,throw(wrapped(E))),
			(
				consult_server:print_solution(OutStream,Vars),	
				consult_server:request_line(InStream,OutStream,'MORE?','YES')										
			)
		)
	->my_format(OutStream,'NO~n',[])
	; my_format(OutStream,'YES~n',[])
	).
	
	
	
	
print_solution(OutStream,Vars):-
	forall(
		(member(Key=Val,Vars), filter_variable(Val)),
		print_binding(OutStream,Key,Val,Vars)
	),
	my_format(OutStream,'END_OF_SOLUTION~n',[]).
	
filter_variable(_) :-
	option(unbound_variables,true), !.
	
filter_variable(Val) :-
	nonvar(Val).
	
print_binding(Out,Key,Val,Vars):-
		my_write(Out,'<'),
		write(Out,Key),
		my_write(Out, '>'),		
		print_value(Out,Val,Vars),		
		nl(Out).

print_values([],_,_). 
print_values([Head|Tail],Out,Vars):-
	print_value(Out,Head,Vars),		
	print_values(Tail,Out,Vars).
	

print_value(Out,Val,Vars):-    	
	option(canonical,true),
	!,
	my_write(Out,'<'),
	(write_escaped(Out,Val,Vars);true),
	my_write(Out, '>').
print_value(Out,Val,Vars):-    	
	( 	is_list(Val), option(interprete_lists,true)
 	->	my_write(Out,'{'),
		print_values(Val,Out,Vars),
		my_write(Out, '}')		
	;	my_write(Out,'<'),
		write_escaped(Out,Val,Vars),
		my_write(Out, '>')
	).



handle_exception(InStream,OutStream,Error,Action):-
    debug(consult_server(handler), 'handle_excpetion (pre): Up:~w, Down:~w, Error:~w~n',[InStream,OutStream,Error]),
    handle_exception_X(InStream,OutStream,Error,Action),
    debug(consult_server(handler), 'handle_excpetion (post): Action:~w~n',[Action]).	
    
handle_exception_X(InStream,OutStream,Error,Action):-
	var(Error),
	!,
	handle_exception(InStream,OutStream,unbound_error_term,Action).	
	

	
handle_exception_X(_InStream,OutStream,peer_reset,continue):-
	catch(
		(
			my_format(OutStream,'RESET~n',[]),
			report_ok(OutStream)
		),							
		_,(
		%	shut_down(InStream,OutStream),
			fail
			)
	),
	!.
	
handle_exception_X(_InStream,OutStream,wrapped(Error),continue):-
	catch(		
		report_error(OutStream,Error),					
		_,(
%			shut_down(InStream,OutStream),
			fail
			)
	),
	!.
	
handle_exception_X(_InStream,OutStream,fatal_read_term_error(Error),stop):-
	catch(		
		report_error(OutStream,Error),					
		_,(
%			shut_down(InStream,OutStream),
			fail
			)
	),
	!.
	
handle_exception_X(_InStream,_OutStream,Error,stop):-
	debug(consult_server(handler),'Unhandled Exception :~w~n Trying to shut down...~n',[Error]).
	
	
	
report_ok(OutStream):-
	my_format(OutStream,'OK~n',[]).	
	
report_error(OutStream, Error):-
	(	var(Error)
	->	my_format(OutStream,'ERROR: unbound error term~n',[])
	;	my_format(OutStream,'ERROR: ~w~n',[Error])
	).			
	
		
byebye(InStream,OutStream):-
	debug(consult_server,'byebye called~n',[]),
	(	is_stream(OutStream)
	->	debug(consult_server(handler),'Downstream is a stream: ~w~n',[OutStream]),
		debug(consult_server(handler),'sending BYE downstream: ~w~n',[OutStream]),
		my_format(OutStream,'BYE~n',[]),
		debug(consult_server(handler),'closing downstream: ~w~n',[OutStream]),		
		catch(close(OutStream),E,
			debug(consult_server(handler),'Problem closing downstream: ~w~n',[E])
		)
	;	debug(consult_server(handler),'Downstream is no stream: ~w~n',[OutStream])
	),
	(	is_stream(InStream)	
	->	debug(consult_server(handler),'Upstream is a stream: ~w~n',[InStream]),
		debug(consult_server(handler),'closing upstream: ~w~n',[InStream]),		
		catch(close(InStream),E,
			debug(consult_server(handler),'Problem closing upstream: ~w~n',[E])
		)
	;	debug(consult_server(handler),'Upstream is no stream: ~w~n',[InStream])
	).
	
	

	
	
	
codes_or_eof_to_atom(end_of_file,_):-
	throw(end_of_file).
	
codes_or_eof_to_atom(Codes,Atom):-
	atom_codes(Atom,Codes).
	
	
count_thread(Prefix,Count):-
	findall(A,
		(	my_current_thread(A),
			atom_concat(Prefix,_,A),
			debug(consult_server(handler),'There is e.g. a thread named ~w~n',[A])
		),
		Bag
	),	 
	length(Bag,Count).
	
unused_thread_name(Prefix,Suffix,Name):-
	unused_thread_name(Prefix,Suffix,0,Name).	
	
unused_thread_name(Prefix,Suffix,Try,Name):-
	atomic_list_concat([Prefix,Try,Suffix],A),
	(	my_current_thread(A)
	->	plus(Try,1,Next),
		unused_thread_name(Prefix,Suffix,Next,Name)
	;	Name=A			
	).
	
my_current_thread(A) :-
  catch(thread_property(A,status(_)), error(existence_error(thread, _), _), fail).
	
	
	
request_line(InStream, OutStream, Prompt, Line):-
	my_format(OutStream,'~w~n',[Prompt]),
	with_interrupts(5,read_line_to_codes(InStream,LineCodes)),
	codes_or_eof_to_atom(LineCodes,Line),
	debug(consult_server(traffic),'(Up:~w, read_line_to_codes)<<< ~w~n',[InStream,Line]).
	
	
	
my_read_term(InStream,Term,Options):-
	with_interrupts(5,read_term(InStream,Term,Options)),
	debug(consult_server(traffic),'(Up:~w read_term) <<<~w~n',[InStream,Term]).

my_write_term(OutStream,Elm,Options):-
	debug(consult_server(traffic),'(Down:~w write_term) >>>~w~n',[OutStream,Elm]),  
	write_term(OutStream,Elm,Options),
	nl(OutStream).
my_write(OutStream,Term):-
	debug(consult_server(traffic),'(Down:~w, write)>>>~w~n',[OutStream,Term]),
	write(OutStream,Term).	
	
my_format(OutStream,Format,Args):-
	atom_concat('(Down:~w, format) >>>',Format,Format2),
	atom_concat(Format2,'~n',Format3),
	debug(consult_server(traffic),Format3,[OutStream|Args]),
	format(OutStream,Format,Args),
	flush_output(OutStream).

my_format(Format,Args):-
    debug(consult_server,Format,Args).
	

write_escaped(Out,Term,Vars):-
	with_output_to(
		atom(Atom),
		(	current_output(O),
			write_term(O, Term, [ignore_ops(true),quoted(true),variable_names(Vars)])
		)
	),
    escape_chars_in_atom(Atom, EscapedAtom),
    my_write(Out,EscapedAtom).
    
escape_chars_in_atom(Atom, EscapedAtom) :-
	atom_chars(Atom, List),
	escape_chars_impl(List, EscapedList),
	atom_chars(EscapedAtom, EscapedList).
	
escape_chars_impl([], []) :- !.

% '<' --> '&lt;'
escape_chars_impl(['<'|Tail], ['&', 'l', 't', ';'|NewTail]) :-
	escape_chars_impl(Tail, NewTail).
	
% '>' --> '&gt;'
escape_chars_impl(['>'|Tail], ['&', 'g', 't', ';'|NewTail]) :-
	escape_chars_impl(Tail, NewTail).
	
% '{' --> '&cbo;'
escape_chars_impl(['{'|Tail], ['&', 'c', 'b', 'o', ';'|NewTail]) :-
	escape_chars_impl(Tail, NewTail).
	
% '}' --> '&cbc;'
escape_chars_impl(['}'|Tail], ['&', 'c', 'b', 'c', ';'|NewTail]) :-
	escape_chars_impl(Tail, NewTail).
	
% '&' --> '&amp;'
escape_chars_impl(['&'|Tail], ['&', 'a', 'm', 'p', ';'|NewTail]) :-
	escape_chars_impl(Tail, NewTail).
	
% '"' --> '&quot;'
escape_chars_impl(['"'|Tail], ['&', 'q', 'u', 'o', 't', ';'|NewTail]) :-
	escape_chars_impl(Tail, NewTail).
	
% '\'' --> '&apos;'
escape_chars_impl(['\''|Tail], ['&', 'a', 'p', 'o', 's', ';'|NewTail]) :-
	escape_chars_impl(Tail, NewTail).

% other chars don't need to be translated
escape_chars_impl([Char|Tail], [Char|NewTail]) :-
	escape_chars_impl(Tail, NewTail).
	
	
write_term_to_memfile(Term,Memfile,Vars):-
	new_memory_file(Memfile),
	open_memory_file(Memfile,write,Stream),
	call_cleanup(
		/*(	option(canonical,true)
		->	write_canonical(Stream,Term)
		;	write(Stream,Term)
		),*/
		write_term(Stream,Term,[ignore_ops(true),quoted(true),variable_names(Vars)]),
		close(Stream)
	).

escape_stream(In,Out):-
    repeat,	    
    (	at_end_of_stream(In)
    ->	!
    ;   get_char(In,Char),    	
	    write_escaped_char(Char,Out),
	    fail
	).
	

write_escaped_char('<',Out):-
	!,
	write(Out,'&lt;').
write_escaped_char('>',Out):-
	!,
	write(Out,'&gt;').
write_escaped_char('{',Out):-
	!,
	write(Out,'&cbo;').
write_escaped_char('}',Out):-
	!,
	write(Out,'&cbc;').
write_escaped_char('&',Out):-
	!,
	write(Out,'&amp;').
write_escaped_char('"',Out):-
	!,
	write(Out,'&quot;').
write_escaped_char('\'',Out):-
	!,
	write(Out,'&apos;').
write_escaped_char(C,Out):-
	put_char(Out,C).	
/*		
with_interrupts(S,Goal):-
	repeat,
	    catch(
	    	(	call_with_time_limit(S,Goal)
	    	->	true
	    	;	!, fail
	    	),
	    	time_limit_exceeded,
	    	fail
	    ),
   !.
*/
with_interrupts(_,Goal):-Goal.
/*
user:prolog_exception_hook(In,_Out,_Frame,CFrame):-
	(	CFrame == none
	->	format("uncaught exception: ~w~n",[In]),
		backtrace(50),
		fail
	;		
	)
	*/	

:- use_module(library(apply)).
    
get_var_names(Goal, _) :-
    not(atomic(Goal)),
    !,
    throw('first argument has to be atomic').
    
get_var_names(Goal, VarNames) :-
    format(atom(Query), '~w.', [Goal]),
    open_chars_stream(Query,Stream),
    read_term(Stream,_,[variable_names(VarNameList)]),
    maplist(extract_var_name, VarNameList, ExtractedList),
    list_2_comma_separated_list(ExtractedList,VarNames).
    
extract_var_name(=(VarName, _), VarName) :- !.
extract_var_name(VarName, VarName) :- !.

list_2_comma_separated_list([],'') :- !.
list_2_comma_separated_list([Element],Element) :- !.
list_2_comma_separated_list([Element|[H|T]],ElementComma) :-
	list_2_comma_separated_list([H|T],RestAtom),
	format(atom(ElementComma),'~w,~w',[Element,RestAtom]).
