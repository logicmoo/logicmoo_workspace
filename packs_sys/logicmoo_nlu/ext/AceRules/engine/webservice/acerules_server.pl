% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(acerules_server, [
		acerules_server/1  % +Port
	]).

:- use_module(library(streampool)).
:- use_module(soap_utils).
:- use_module(get_results).

/** <module> AceRules server

This module contains the AceRules server. It is supposed to run continuously and it can
communicate through a socket connection with other programs. A small script is started
every time a web request is performed. This makes the web response very fast.

---+++ Technical remarks:

This module is basically a copy of the APE server module.

This AceRules server runs an own instance of the APE parser. Thus, it does not use the
APE web service.

Generally, it would be better if AceRules uses the APE web service (or communicates
directly with the APE server).

@author Kaarel Kaljurand
@author Tobias Kuhn
@version 2007-08-14
*/


%% acerules_server(+Port)
%
% Starts the AceRules server.

acerules_server(Port) :-
	tcp_socket(Socket),
	tcp_bind(Socket, Port),
	tcp_listen(Socket, 5),
	tcp_open_socket(Socket, In, _Out),
	add_stream_to_pool(In, accept(Socket)),
	stream_pool_main_loop.


accept(Socket) :-
	tcp_accept(Socket, Slave, _Peer),
	tcp_open_socket(Slave, In, Out),
	add_stream_to_pool(In, client(In, Out)).


client(In, Out) :-
	read_lines(In, RequestC),
	close(In),
	atom_codes(Request, RequestC),
	( Request == '' ->
		true
	;
		process_client_request(Out, Request)
	),
  	close(Out),
	delete_stream_from_pool(In).


process_client_request(Out, I) :-
	get_results(I, O),
	write(Out, O),
	write(Out, '\n.\n'),
	flush_output(Out),
	!.

process_client_request(Out, _ClientRequest) :-
	create_fault_element('ar:Unknown', 'Failed to process the request.', FaultElement),
	create_soap_message(FaultElement, SOAPOutput),
	write(Out, SOAPOutput),
	write(Out, '\n.\n'),
	flush_output(Out).


read_lines(In, Codes) :-
    read_line_to_codes(In, Line),
    (Line == end_of_file ->
      Codes = []
    ;
      (Line == [] ->
        Codes = []
      ;
        read_lines(In, CodesRest),
        append(Line, [10|CodesRest], Codes)
      )
    ).
