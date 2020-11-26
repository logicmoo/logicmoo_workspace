
:- ['$REGULUS/RegulusLanguageServer/Prolog/server'].

:- use_module(library(lists)).
:- use_module('$REGULUS/PrologLib/utilities').

start_regulus_language_server_from_command_line :-
	prolog_flag(argv, Args),
	start_regulus_language_server_from_command_line1(Args).

start_regulus_language_server_from_command_line1(Args) :-
	get_port_arg_or_complain(Args, Port),
	regulus_language_server(Port).

get_port_arg_or_complain(Args, Port) :-
	Args = [Arg],
	atom_to_int(Arg, Port),
	!.
get_port_arg_or_complain(_Args, _Port) :-
	format('~N~nUsage: sicstus -l \'$REGULUS/RegulusLanguageServer/Prolog/start_server.pl\' -a <Port>~n', []),
	fail.

:- start_regulus_language_server_from_command_line.

:- halt.



