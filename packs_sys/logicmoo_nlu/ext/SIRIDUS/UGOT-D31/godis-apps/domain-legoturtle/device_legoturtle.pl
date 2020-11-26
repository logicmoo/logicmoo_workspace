%%% device_legoturtle
%%%
%%% GoDiS device interface for Lego Mindstorms RCX using pbForth
%%% this version for robot on p. 90 of Ferrari et al.
%%% Staffan Larsson July 2003
%%%
%%% Examples:
%%% device_legoturtle:pbforth_exec( [7,3,0,'MOTOR_SET'],_).
%%% device_legoturtle:pbforth_sendscript( 'test.txt' ).
%%% device_legoturtle:pbforth_exec([1,2,'+','.'],R)

:- module( device_legoturtle, [ dev_do/2, valid_parameter/1 ] ).

:- use_module(library(tcltk)).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

action( fd, [ steps ] ).
action( bk, [ steps ] ).
action( lt, [ degrees ] ).
action( rt, [ degrees ] ).
% commands below not operative
action( pu, [] ).
action( pd, [] ).
action( setbg, [color] ).
action( setpc, [color] ).
action( cs, [] ).

% action( 'MOTOR_SET', [power, mode, index] ).

% TODO: sätt sökstig för tcl, till domain-....

init :-
	tk_new([],Tcl),
	assert(tcl(Tcl)),
	tcl_eval(Tcl, update, _),
	tcl_eval(Tcl,[source,'C:/MyCVS/godis/dist/prolog/godis/domain-legoturtle/rcxTcl/rcxtk_sl.tcl'],_),
%	tcl_eval(Tcl,[source,'C:/Program/rcxTcl/rcxtk_sl.tcl'],_),
	pbforth_sendscript( 'C:/MyCVS/godis/dist/prolog/godis/domain-legoturtle/logo_pbforth_1.txt' ).



stop:-
	tcl(Tcl),
	tcl_delete(Tcl),
	retract(tcl(Tcl)).

% Turtle commands

%exec( fd, [Steps] ):-
%	pbforth_exec(


% tcl commands
tcl_exec( Command, Result ):-
	tcl(Tcl),
	tcl_eval(Tcl, Command, Result),
	tcl_eval(Tcl, update, _).

% pbforth commands (as lists)
pbforth_exec( Command, Result ):-
	append( ['\"'|Command], ['\"'], Command1 ),
	tcl_exec( ['::rcxtk::sendLine', Command1], Result1 ),
	( Result1 = [] ->
	    Result = Result1;
	    unscramble_result( Result1, Command, Result ) ).

pbforth_sendscript( File ):-
	tcl_exec( ['::rcxtk::sendScript', File], _ ).
	
pbforth_define( Name, Program ):-
	pbforth_exec([':',Name,Program,';'],_).


dev_do( Command, Commitments ) :-
	action( Command, Vars ),
	get_command_args( Vars, Commitments, Values ),
	append( Values, [ Command ], Command1 ), 
	pbforth_exec( Command1,_ ).

get_command_args([],_,[]).
get_command_args( [ Var | Vars ], Commitments, [ Val | Vals ] ) :-
	Com =.. [ Var, Val ],
	member( Com, Commitments ),
	get_command_args( Vars, Commitments, Vals ).






% HELP COMMANDS - a bit ugly, should be done in Tcl.
%
% used to turn pbFort results into prolog lists
% Unfortunately, it seems the RCX returns not only the result but also the latest input; this must be pruned off.

unscramble_result( Result1, Command, Result ):-
	append( Result2, [32,32,79,75,32,13,10,17], Result1 ),
	makeString( Command, CommandS ),
	append( CommandS, [32,13,19], CommandS2 ),
	append( [32|CommandS2], Result3, Result2 ),
	makeList( Result3, [], Result ).

% take a tcl command in list format and convert into a string
makeString( [Word], String ):-
	name( Word, String ).
makeString( [Word|Rest], String ):-
	name( Word, Char ),
	append( Char, [32|RestString], String ),
	makeString( Rest, RestString ).

% take a string containing results separated by spaces, and return a list of terms
makeList( [], WordStr, [Word] ):-
	name( Word, WordStr ).
makeList( [32|Chars], WordStr, [Word|Words] ):-
	!,
	name( Word, WordStr ),
	makeList( Chars, [], Words ).
makeList( [Char|Chars], WordStr, Words ):-
	makeList( Chars, [Char|WordStr], Words ).


valid_parameter(_).

:- init.
