%   Module : ccallqp
%   Author : David S. Warren
%   Updated: 4/14/88
%   Purpose: Allow C clients to call Prolog servants.

%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.


:- module(ccallqp, [
	save_ipc_servant/1,
	msg_trace/2,
% -------------------- CEC changes ------------------
	pan_ServeOff/0,
	pan_ServeContinue/0,
	pan_delayGoal/1,
	pan_isRemote/1,
	connect_and_serve_xdr/0
% ---------------- End of CEC changes ---------------
   ]).
:- use_module(library(basics)).

% -------------------- CEC changes ------------------
:- dynamic pan_DoNotServe/0.
:- dynamic pan_goalIsDelayed/1.
:- dynamic pan_Remote/1.
% ---------------- End of CEC changes ---------------


sccs_id('"@(#)88/04/14 ccallqp.pl	26.1"').


/*  These Prolog routines allow a C program to call a Prolog servant.
    These routines are used by the servant to communicate with the C
    program.  C programs can call certain Prolog predicates defined in
    the servant's database.  These predicates must be declared as
    callable by C by means of external/3 facts.  The predicates and
    external facts must be in the user: module.

    Declarations of Prolog routines that can be called by C are as follows:

	external(Routine_Name_For_C, xdr,
		 Prolog_Predicate_Name(parspec1,parspec2,..)).

    The parameter specifications of 'external' predicates are
    similar to those of 'foreign' predicates:
	+integer, -integer,
	+string,  -string,
	+float,   -float,
	+atom,    -atom.
    (The indication that a parameter is a function return is not
    appropriate for Prolog's side of the interface.)  For example,
    +integer means that an integer will be supplied by the C program
    when this predicate is called; -string means that the Prolog
    predicate will generate a string value and return it to the calling
    C program.

    For example, suppose you want compile/1 to be available to the
    C master program under the name 'plc'.  Then you would write
	external(plc, xdr, compile(+string)).
    Or to enable the master to inspect its servant's library
    directory table under the name 'lib', you would write
	external(lib, xdr, library_directory(-string)).
    There is normally no reason for the Routine_Name_For_C and
    Prolog_Predicate_Name to be different.
*/

:- dynamic
	'$external'/5.


/*  The xdrsock routines allow Prolog to set up and communicate
    through sockets using the xdr protocol. So load those routines.
    Note that we don't need a library() wrapper around xdrsock,
    because it is in the same directory as this file.  In fact,
    it would be wrong to put it there.
    The sockutil routines are for tracing and error reporting.
*/
% -------------------- CEC changes ------------------
:- ensure_loaded(library(xdrsock)),
   ensure_loaded(library(sockutil)).
% ---------------- End of CEC changes ---------------



/************************************************************************/
/*									*/
/*			Goal servant for XDR interface			*/
/*									*/
/************************************************************************/


/*  save_ipc_servant(+SavedState)
    creates a saved state in the file SavedState, which can later be used
    by the C routine QPc_create_servant to create a servant for a C program.
*/ 
save_ipc_servant(Savedstate) :-
	externalsok,
	save_ipc_servant1(Savedstate).

save_ipc_servant1(Savedstate) :-
	save(Savedstate, 1),
	!,
	connect_and_serve_xdr.
% -------------------- CEC changes ------------------
%	halt.
% ---------------- End of CEC changes ---------------
save_ipc_servant1(_).


/*  connect_and_serve_xdr
    connects to a socket identified on the command line and becomes a
    servant for it, using an xdr tuple-at-a-time protocol.  All goals it
    will serve must be declared external.

    There are two cases:
	REMOTE EXECUTION THROUGH SOCKETS
	    This command was started up as
		rsh ${HostName} -n ${SavedState} \
		    -socket ${CallHost} ${CallPort} '&' &
	    or as
		rsh ${HostName} -n ${SavedState} \
		    -socket ${CallHost} ${CallPort} \
		    >${OutputFile} 2>&1 '&' &
	    In either case, the command line as Prolog sees it looks like
		${SavedState} -socket ${CallHost} ${CallPort}
	    and we have to call back to ${CallPort} AT ${CallHost}.

	LOCAL EXECUTION THROUGH PIPES
	    This command was started up directly through fork() & exec(),
	    but could be described roughly as
		${SavedState} </dev/null ${I}<${PipeIn} ${O}>${PipeOut} \
		    -pipe ${I} ${O} &
	    or as
		${SavedState} </dev/null ${I}<${PipeIn} ${O}>${PipeOut} \
		    -pipe ${I} ${O} >${OutputFile} 2>&1 &
	    In either case, the command line as Prolog sees it looks like
		${SavedState} -pipe ${I} ${O}
*/
connect_and_serve_xdr :-
% -------------------- CEC changes ------------------
%	sigsign,		% ignore signals as a servant
%
% (The servant is used interactively in another window,
% so it must not ignore signals)
% ---------------- End of CEC changes ---------------
	unix(argv(CommandLine)),
	check_command_line(CommandLine, HostName, InPort, OutPort),
% -------------------- CEC changes ------------------
% In order to be able to suspend the servant temporarily, this
% part had to be rewritten.

	!, connect_and_serve_xdr_1(HostName, InPort, OutPort).

connect_and_serve_xdr_1(HostName, InPort, OutPort) :-
	establish_xdr_client(HostName, InPort, OutPort, 0),
	!,
	( unix(argv(['-socket', _, _, '-remote'|_])) ->
	    assertz(pan_Remote(HostName))
	  ; true
	),
	sendexts,
	serveloop,
        ( pan_DoNotServe ->
            nl,
            write('type "serve." to continue serving.'),
            nl
          ; shutdown_xdr_connection
        ).
connect_and_serve_xdr_1(_,_,_) :- 
% ---------------- End of CEC changes ---------------
	write_sock_error(['starting servant: incorrect parameters']),
	fail.


check_command_line(['-pipe',I,O|_], '', I, O) :- !,
	integer(I), integer(O).
check_command_line(['-socket',H,I|_], H, I, 0) :- !,
	atom(H), integer(I).
check_command_line([_|CommandLine], H, I, O) :-
	check_command_line(CommandLine, H, I, O).



/*  externalsok
    checks that the external declarations are all legal.
    It prints an error message and fails if there is a problem.
*/
externalsok :-
	(   bagof(ext(Name,Spec), user:external(Name,xdr,Spec), Externals) ->
	    chkexts(Externals)
	;   /* bagof fails if there are no externals, so */
	    write_sock_error(['no external declarations']),
	    fail
	).

chkexts([]).
chkexts([ext(Name,Spec)|Exts]) :-
	(   member(ext(Name,_), Exts) ->
	    write_sock_error(['Duplicate external command:',Name]),
	    fail
	;   \+ spec_type(Spec, _, _, _) ->
	    write_sock_error(['Illegal specification:',Spec]),
	    fail
	;   /* looks ok so far */
	    chkexts(Exts)
	).


/*  sendexts
    sends the external declarations to the client.
*/
sendexts :-
	bagof(ext(Name,Spec), user:external(Name,xdr,Spec), Externals),
	length(Externals, Numext),
	xdr_putinteger(Numext, 1),	% number of definitions
	xdr_flush,
	sendeachext(Externals, 0),
	xdr_flush.


/*  sendeachext(Extlist,N)
    sends each external declaration to the client and saves them
    in $external/5.  N is the number of the first external in the list.
*/
sendeachext([], _).
sendeachext([ext(Name,Spec)|Exts], N) :-
	spec_type(Spec, Pred, Arity, Types),
	xdr_putinteger(Arity, 1),	% arity
	xdr_putstring(Name, 1),		% name
	name(Stypes, Types),
	xdr_putstring(Stypes, 1),	% types
	xdr_flush,
	assertz('$external'(N,Pred,Arity,Types,Name)),
	N1 is N+1,
	sendeachext(Exts, N1).


/*  spec_type(+Spec, -Name, -Arity, -Types)
    unpacks the last element of an external/3 fact into a Prolog
    predicate's Name and Arity, and a list of type codes for the
    arguments.
*/
spec_type(Spec, Name, Arity, Types) :-
	nonvar(Spec),
	functor(Spec, Name, Arity),
	atom(Name),
	spec_types(Arity, Spec, [], Types).

spec_types(0, _, Types, Types) :- !.
spec_types(N, Spec, Types0, Types) :-
	arg(N, Spec, Arg),
	typeton(Arg, Type),
	M is N-1,
	spec_types(M, Spec, [Type|Types0], Types).


/*  typeton(TypeSpec,TypeNum)
    associates each type spec with a number.
*/
typeton(+Type, N) :- fwd_type_num(Type, N).
typeton(-Type, N) :- bak_type_num(Type, N).

fwd_type_num(integer, 1).
fwd_type_num(float,   2).
fwd_type_num(atom,    3).
fwd_type_num(string,  4).

bak_type_num(integer, 9).
bak_type_num(float,  10).
bak_type_num(atom,   11).
bak_type_num(string, 12).


/*  serveloop:
    reads and services commands from the socket using the XDR protocol
*/

/* EXTENSIONS FOR CEC/PANNDA-S CONNECTION */

pan_isRemote(HostName) :-
  pan_Remote(HostName).

serveloop :-
	repeat,
% -------------------- CEC changes ------------------
% In order to be able to suspend the servant and to delay goals
% (i.e. to send a goal to the prolog process without waiting for
% a solution), this part had to be rewritten.

        ( pan_DoNotServe -> 
            true
          ; ( pan_goalIsDelayed(Goal) ->
                pan_executeDelayedGoal(Goal)
              ; getcommand(Cmd),
                processtopcmd(Cmd)
            )
        ), !.


pan_ServeOn  :-
  retract(pan_DoNotServe), fail ; true, !.
pan_ServeOff :-
  assertz(pan_DoNotServe).
pan_ServeContinue :-
  pan_ServeOn,
  serveloop,
  !,
  ( pan_DoNotServe ->
      nl,
      write('type "serve." to continue serving.'),
      nl
    ; shutdown_xdr_connection
  ).
pan_delayGoal(Goal) :- 
  ( retract(pan_goalIsDelayed(_)), fail ; true ),
  assertz(pan_goalIsDelayed(Goal)).
pan_executeDelayedGoal(Goal) :- 
  ( retract(pan_goalIsDelayed(_)), fail ; true ),
  !, user:pan_callAndFlush((user:Goal)),
  !, fail.
% ---------------- End of CEC changes ---------------



/*  getcommand(-Cmd):
    reads a command (an integer) from the socket.  This should identifyf either
    an external predicate, or a special command: 
	-1 = next
	-2 = close
	-3 = shutdown
    or a standard command: 
	-4 = QP_string_from_atom
	-5 = QP_atom_from_string
*/
getcommand(Cmd) :-
	xdr_getinteger(Cmd, 1),
	!,
	'$msg_trace'(N),
	(   N == on ->
	    commnd(Cmd, Cname),
	    msg_trace1(['received:',Cname])
	;   true
	).
getcommand(-3 /* shutdown */) :-
	/* to avoid loop if get fails */
% -------------------- CEC changes ------------------
% The servant should not get down, but only stop serving.
% The user can kill the prolog process with "<EOF>".
%
%	write(user_error, 'FATAL ERROR getting command, shutting down.'),
%	nl(user_error).
	pan_ServeOff,
	fail.
% ---------------- End of CEC changes ---------------


commnd(N, Cname) :-
	N >= 0,
	!,
	'$external'(N, _, _, _, Cname).
commnd(-1, next).
commnd(-2, close).
commnd(-3, shutdown).
commnd(-4, 'QP_xstring_from_atom').
commnd(-5, 'QP_xatom_from_string').

/*  processtopcmd(+Cmd):
    processes a top-level command.
    This is when no other goal is still active.
*/
processtopcmd(Cmd) :-
	(   Cmd @< 0 ->		% -ve number or variable ???
	    topcmd(Cmd)
	;   doextcall(Cmd)
	).

topcmd(-3 /*shutdown*/) :-	% ack and succeed out to top
	sendok.
topcmd(-2 /*close*/) :-		% error, no query to close
	senderror,
	fail.
topcmd(-1 /*next*/) :-		% error, no active query
	write_sock_error(['No active external call']),
	senderror.
topcmd(-4) :-			% QP_string_from_atom
	xdr_string_from_atom,
	fail.
topcmd(-5) :-			% QP_atom_from_string
	xdr_atom_from_string,
	fail.


/*  doextcall(+Cmd):
    reads the input parameters from the socket,
    constructs the goal, and calls it.
*/
doextcall(Cmd) :-
    (	getgoal(Cmd, Types, Goal) ->
	msg_trace1(['calling:',Goal]),
	processextcall(Types, Goal)
    ;   write_sock_error(['Illegal external call to:',Cmd]),
	senderror,
	fail
    ).


/*  processextcall(+Types,+Goal).
    Given goal Goal of arity Arity and with list of parameter types (+Types),
    it reads commands from the socket, computing and sending answers to Goal
    as they are requested.
*/
processextcall(_, _) :-
	nextcommand,
	!,
	fail.
processextcall(Types, Goal) :-
	user:Goal,		% externals must be in user
	send_answer(Types, Goal),
	nextcommand,
	!,
	fail.
processextcall(_, _) :-
	sendnomore,
	fail.


/*  nextcommand
    reads and processes a command.
*/
nextcommand :-
	getcommand(Cmd),
	processcommand(Cmd).


/*  processcommand(+Cmd)
    processes a nested command, i.e. a command that may ask for another
    tuple from the current query, or may close the query, as well as
    start up a new nested query.
*/
processcommand(Cmd) :-
	(   Cmd @< 0 ->		% variable or -ve number ???
	    processpredef(Cmd)
	;   % to allow nested calls, do: doextcall(Cmd).
	    write_sock_error(['Nested calls are illegal.'])
	).

processpredef(-1 /*next*/) :-	% simply fail back to get next
	fail.
processpredef(-2 /*close*/) :-	% ack and succeed to cut above
	sendok.
processpredef(-3 /*shutdown*/) :-	% error and succeed 
	senderror.
processpredef(-4) :-
	xdr_string_from_atom,
	nextcommand.
processpredef(-5) :-
	xdr_atom_from_string,
	nextcommand.


/*  getgoal(+Cmd,-Types,-Goal)
    given a command Cmd, finds the external declaration and the Types
    (returned in Types), reads the input parameters from the socket and
    constructs the goal (returned in Goal).
*/
getgoal(Cmd, Types, Goal) :-
	(   '$external'(Cmd, Pred, Arity, Types, _Name) ->
	    functor(Goal, Pred, Arity),
	    get_args(Types, 1, Goal)
	;   write_sock_error([Cmd,' is not a legal external.']),
	    fail
	).


/*  get_args(+Types,+N,+Goal)
    reads remaining (N and beyond) input parameters from the socket to
    instantiate the remaining arguments of Goal (which is passed in with
    all arguments as variables).
*/
get_args([], _, _) :- !.
get_args([Partype|Partypes], N, Goal) :-
	getarg(Partype, X),
	arg(N, Goal, X),
	N1 is N + 1,
	get_args(Partypes, N1, Goal).


/*  These are various routines that send certain messages to
    the calling C routine.
*/


sendnomore :-
	sendflag(1).

sendingmore :-
	xdr_putinteger(0, 1).	/* no flush, answer will flush it */

sendok :-
	sendflag(0).

senderror :-
	sendflag(1).

sendflag(X) :-
	xdr_putinteger(X, 1),
	xdr_flush.


/*  send_answer(+Types,+Goal)
    sends a tuple to the calling C program.
*/
send_answer(Types, Goal) :-
	msg_trace1(['send:',Goal]),
	sendingmore,
	put_args(Types, 1, Goal),
	xdr_flush.


/*  It would be nice to combine the get_ and put_args, but since the C
    interface doesn't support bidirectional parameter passing, the
    distinction would have to be made at least there.  So here seems as
    good as anywhere.
*/


/*  put_args(+Types,+N,+Goal)
    writes the remaining (N and beyond) arguments of Goal to the socket.
*/
put_args([], _, _) :- !.
put_args([Partype|Partypes], N, Goal) :-
	arg(N, Goal, X),
	putarg(Partype, X),
	N1 is N+1,
	put_args(Partypes, N1, Goal).


/*  getarg(+Pspec,?Val)
    if Pspec indicates an input argument (1-4), it reads it from the
    socket, using the appropriately typed routine, and returns it in
    Val.  If Pspec indicates an output argument (9-12), it does nothing.
*/
getarg(Type, X) :-
	(   Type == 1 -> xdr_getinteger(X, 1)
	;   Type == 2 -> xdr_getfloat(X, 1)
	;   Type == 3 -> xdr_getatom(X, 1)
	;   Type == 4 -> xdr_getstring(X, 1)
	;   true
	).


/*  putarg(+Pspec,+Val)
    if Pspec indicates an output argument (9-12), it writes Val to the
    socket, using the appropriately typed routine.  If Pspec indicates
    an input argument (1-4), it does nothing.
*/
putarg(Type, X) :-
	(   Type ==  9 -> Y is integer(X), xdr_putinteger(Y, 1)
	;   Type == 10 -> Y is float(X),   xdr_putfloat(Y, 1)
	;   Type == 11 ->		   xdr_putatom(X, 1)
	;   Type == 12 ->		   xdr_putstring(X, 1)
	;   true
	).

