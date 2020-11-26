/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(rserve,
	  [ r_open/2,			% -RServe, +Options
	    r_close/1,			% +RServe
	    r_login/3,			% +RServe, +User, +Password

	    r_assign/3,			% +RServe, +Var, +Data
	    r_eval/2,			% +RServe, +Command
	    r_eval/3,			% +RServe, +Command, -Result
	    r_eval_ex/3,		% +RServe, +Command, -Result

	    r_read_file/3,		% +RServe, +FileName, -String
	    r_remove_file/2,		% +RServe, +FileName

	    r_detach/2,			% +Rserve, -Session
	    r_resume/2,			% -Rserve, +Session

	    r_server_eval/2,		% +Rserve, +Command
	    r_server_source/2,		% +Rserve, +FileName
	    r_server_shutdown/1		% +Rserve
	  ]).
:- use_module(r_grammar).
:- use_module(r_term).
:- use_module(library(error)).

:- use_foreign_library(foreign(rserve)).

:- multifile
	r_open_hook/2.			% +Name, -Reference

/** <module> SWI-Prolog Rserve client

This module provides a low-level binding to the Rserve R server process.
*/

%%	r_open(-RServe, +Options) is det.
%
%	Open a connection to an R server.  Options:
%
%	  - alias(+Alias)
%	  Give a name to the connection.
%	  - host(+Host)
%	  Connect to Host (default: =|127.0.0.1|=).
%	  - port(+Port)
%	  Connect to port (default: 6311).  If Port is `-1`, `Host` is
%	  interpreted as a path name and a Unix domain socket (named
%	  pipe) is used.
%	  - open(+How)
%	  If `once`, turn opening a connection for the second time
%	  in a no-op.

%%	r_close(+Rserve) is det.
%
%	Close an open connection to an R server.

%%	r_login(+Rserve, +User, +Password) is det.
%
%	Login with the R server.


%%	r_assign(+Rserve, +VarName, +Value) is det.
%
%	Assign a value to variable VarName   in  Rserve. Value follows a
%	generic transformation of Prolog values into R values:
%
%	  $ list :
%	  A list is translated into an R array of elements of the same
%	  type.  The initial type is determined by the first element.
%	  If subsequent elements to not fit the type, the type is
%	  promoted.  Currently defined promotions are:
%	    - Integers are promoted to doubles.
%	  $ boolean :
%	  The Prolog atoms `true` and `false` are mapped to R booleans.
%	  $ integer :
%	  Prolog integers in the range -2147483648..2147483647 are
%	  mapped to R integers.
%	  $ float :
%	  Prolog floats are mapped to R doubles.
%	  $ atom :
%	  Atoms other than `true` and `false` are mapped to strings.
%	  $ string :
%	  A Prolog string is always mapped to an R string. The interface
%	  assumes UTF-8 encoding for R. See the `encoding` setting in
%	  the Rserve config file.
%	  $ c(Elem1, Elem2, ...) :
%	  A compound term with functor `c` is handled in the same way
%	  as a _list_.

r_assign(Rserve, VarName, Value) :-
	r_identifier(VarName), !,
	r_assign_(Rserve, VarName, Value).
r_assign(_, VarName, _Value) :-
	must_be(atom, VarName),
	domain_error(r_variable_name, VarName).

%%	r_eval(+Rserve, +Command, -Value) is det.
%
%	Send Command to Rserve and translate  the resulting R expression
%	into a Prolog representation. The transformation from R
%	expressions to Prolog data is defined as follows:
%
%	  $ TRUE or FALSE :
%	  The R booleans are mapped to the Prolog atoms `true` and
%	  `false`.
%	  $ integer :
%	  R integers are mapped to Prolog integers.
%	  $ double :
%	  R doubles are mapped to Prolog floats.
%	  $ string :
%	  R strings are mapped to Prolog strings. The interface
%	  assumes UTF-8 encoding for R. See the `encoding` setting in
%	  the Rserve config file.
%
%	@see r_eval_ex/3 to map R exceptions  to Prolog. By default, the
%	non-interactive R server terminates on an  exception and thus if
%	r_eval/3 causes an R exception R  terminates and Prolog receives
%	an I/O error.

%%	r_eval_ex(+Rserve, +Command, -Result) is det.
%
%	As r_eval/3, but captures R exceptions and translates these into
%	Prolog exceptions.

r_eval_ex(Connection, Command, Result) :-
	to_string(Command, CommandS),
	r_assign($, 'Rserve2.cmd', CommandS),
	r_eval(Connection,
	       "try(eval(parse(text=Rserve2.cmd)),silent=TRUE)",
	       Result0),
	r_check_error(Result0),
	Result = Result0.

to_string(Command, CommandS) :-
	string(Command), !,
	CommandS = Command.
to_string(Command, CommandS) :-
	string_codes(CommandS, Command).

r_check_error([ErrorString]) :-
	string(ErrorString),
	sub_string(ErrorString, 0, _, _, "Error in "),
	split_string(ErrorString, "\n", "", [Error|Context]), !,
	throw(error(r_error(Error, Context), _)).
r_check_error(_).

%%	r_eval(+Rserve, +Command) is det.
%
%	Evaluate R Command without waiting for a reply.  This is called
%	_void_ evaluation in Rserve.

%%	r_read_file(+RServe, +FileName, -Content:string) is det.
%
%	Read the content of a remote file into the string Content.

%%	r_remove_file(+RServe, +FileName) is det.
%
%	Remove FileName from the server.

%%	r_open_hook(+Alias, -Rserve) is semidet.
%
%	Hook that is used to translate Alias  into an R connection. This
%	is called for R references if  the   argument  is  not an Rserve
%	handle, nor an existing alias. The hook  may create R on demand.
%	One of the use  cases  is   SWISH,  where  we  want thread-local
%	references to R and we want to   create  the R connection on the
%	first reference and destroy it as the query dies.


		 /*******************************
		 *     SESSION MANAGEMENT	*
		 *******************************/

%%	r_detach(+Rserve, -Session) is det.
%
%	Detach a session to  be  resumed   later.  Session  is an opaque
%	handle  to  the  session.  The  session  may  be  resumed  using
%	r_resume/2. The session key may be exchanged with another Prolog
%	process. Note that calling r_detach/2 closes the existing Rserve
%	handle.

r_detach(Rserve, Session) :-
	r_detach_(Rserve, Session),
	r_close(Rserve).

%%	r_resume(-Rserve, +Session) is det.
%%	r_resume(-Rserve, +Session, +Alias) is det.
%
%	Resume an R session from a key obtained using r_detach/2.

r_resume(Rserve, Session) :-
	r_resume(Rserve, Session, _).


		 /*******************************
		 *	  SERVER CONTROL	*
		 *******************************/

%%	r_server_eval(+Rserve, +Command)
%
%	Evaluate Command in the main  server.   The  main server must be
%	configured to allow for _control_ commands.

%%	r_server_source(+Rserve, +FileName)
%
%	Process FileName on the main  server.   The  main server must be
%	configured to allow for _control_ commands.

%%	r_server_shutdown(+Rserve)
%
%	Cause the main server to shutdown. Note that the current session
%	(Rserve) remains valid.


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

prolog:error_message(r_error(Code)) -->
	{ r_error_code(Code, _Id, Message) },
	[ 'R: ~w'-[Message] ].
prolog:error_message(r_error(Main, Context)) -->
	[ 'R: ~w'-[Main] ],
	error_lines(Context).

error_lines([]) --> [].
error_lines([""]) --> !.
error_lines([H|T]) -->
	[ nl, 'R: ~w'-[H] ],
	error_lines(T).


% Sync with CERR_* as defined in Rconnection.h
r_error_code( -1,  connect_failed,    "Connect failed").
r_error_code( -2,  handshake_failed,  "Handshake failed").
r_error_code( -3,  invalid_id,	      "Invalid id").
r_error_code( -4,  protocol_not_supp, "Protocol not supported").
r_error_code( -5,  not_connected,     "Not connected").
r_error_code( -7,  peer_closed,	      "Peer closed connection").
r_error_code( -8,  malformed_packet,  "Malformed packed").
r_error_code( -9,  send_error,	      "Send error").
r_error_code(-10,  out_of_mem,	      "Out of memory").
r_error_code(-11,  not_supported,     "Not supported").
r_error_code(-12,  io_error,	      "I/O error").
r_error_code(-20,  auth_unsupported,  "Authentication not supported").

r_error_code(0x41, auth_failed,	      "Authentication failed").
r_error_code(0x42, conn_broken,	      "Connection broken").
r_error_code(0x43, inv_cmd,	      "Invalid command").
r_error_code(0x44, inv_par,	      "Invalid parameters").
r_error_code(0x45, 'Rerror',	      "R-error occured").
r_error_code(0x46, 'IOerror',	      "I/O error").
r_error_code(0x47, notOpen,	      "Read/write on closed file").
r_error_code(0x48, accessDenied,      "Access denied").
r_error_code(0x49, unsupportedCmd,    "Unsupported command").
r_error_code(0x4a, unknownCmd,	      "Unknown command").
r_error_code(0x4b, data_overflow,     "Incoming packet is too big").
r_error_code(0x4c, object_too_big,    "Requested object is too big").
r_error_code(0x4d, out_of_mem,	      "Out of memory").
r_error_code(0x4e, ctrl_closed,	      "Control pipe to master is closed").

r_error_code(0x50, session_busy,      "Session is still busy").
r_error_code(0x51, detach_failed,     "Unable to detach seesion").

r_error_code(0x61, disabled,	      "Feature is disabled").
r_error_code(0x62, unavailable,	      "Feature is not present").
r_error_code(0x63, cryptError,	      "Crypto-system error").
r_error_code(0x64, securityClose,     "Server-initiated close due to security").
