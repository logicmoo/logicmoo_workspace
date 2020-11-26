% Generic Telia home device

:- module( device_home, [ set_node_value/2, get_node_value/2 ] ).
:- use_module(library(sockets)).
:- use_module(library(lists)).
:- dynamic simulation_node_value/2.

environment_mode(simulation).
%environment_mode(real).

:- ( ( user:flag( visualize_devices, yes ) ) ->
       use_module(library(visualization))
   ; true ).

http_addr('131.115.158.239').
http_port(8080).
%http_addr('www.ling.gu.se').
%http_port(80).

set_node_value(ID,Value) :-
	( environment_mode(real) ->
	    really_set_node_value(ID,Value) ;
	    sim_set_node_value(ID,Value) ),
	( user:flag(visualize_devices,yes) ->
	    gfx_set_node_value(home,ID,Value) ;
	    true ).

really_set_node_value(ID,Value) :-
	socket('AF_INET',Socket),
	http_addr(Addr),
	http_port(Port),
	socket_connect(Socket,'AF_INET'(Addr,Port),Stream),
	socket_buffering(Stream,read,_,unbuf),
	format(Stream,'GET /VoiceLnsServlet/?command=setNodeValue&node=~a&value0=~a\r\n\r\n',[ID,Value]),
	flush_output(Stream),
	repeat,
	get0(Stream,-1),
	!,
	close(Stream).

sim_set_node_value(ID,Value) :-
	try(retract(simulation_node_value(ID,_))),
	assert(simulation_node_value(ID,Value)),
	format(' *** ~a <- ~a\n',[ID,Value]).

get_node_value(ID,Value) :-
	environment_mode(real),
	socket('AF_INET',Socket),
	http_addr(Addr),
	http_port(Port),
	socket_connect(Socket,'AF_INET'(Addr,Port),Stream),
	socket_buffering(Stream,read,_,unbuf),
        format(Stream,'GET /VoiceLnsServlet/?command=getNodeValue&node=~a\r\n\r\n',[ID]),
	flush_output(Stream),
	read_line(Stream,Line1),
	( append(Line,[13],Line1) ->
            true ;
            Line = Line1 ),
        number_chars(Value100,Line),
	write(Value100),
        ValueN is Value100 / 100,
	write(ValueN),
        number_chars(ValueN,ValueS),
        atom_chars(Value,ValueS),
	repeat,
	get0(Stream,-1),
	!,
	close(Stream).

get_node_value(ID,Value) :-
	environment_mode(simulation),
	( simulation_node_value(ID,CurrentValue) ->
	    Value = CurrentValue
	;
	    Value = '0.0'
	),
	format(' *** ~a -> ~a\n',[ID,Value]).

read_line(Stream,[]) :-
	at_end_of_line(Stream),
	!.

read_line(Stream,[C|Cs]) :-
	get0(Stream,C),
	read_line(Stream,Cs).

try(G) :-
	( G -> true ; true ).
