/*************************************************************************

         name: myResource.pl 
      version: 
  description: a resource
       author: 
 
*************************************************************************/

:- module( domain_homecentre, [ splan/2, tplan/3, dplan/3, relevant_answer/2 ] ).

:- use_module(library(lists)).

% dplan/1
%
% dialogue plan - derived from scenario plan

dplan( instruct_exec(reinstall(print_head)),
       reinstalled(print_head),
       [ instruct_check(moved_forward(carriage_lock)),
	 instruct_exec(secure(print_head)),
	 instruct_exec(close(top_cover)),
	 instruct_exec(reattach(scanner)),
	 instruct_exec(press_and_release(yellow_LED_button)),
	 raise( move_from_center_position(carriage_head)),
	 if_then( not(move_from_center_position(carriage_head)),
		  instruct_exec(remove_and_reinstall(print_head))),
	 inform(reinstalled(print_head)),
	 inform(next(prepare_cartridge_for_printing))
       ] ).

dplan( instruct_exec(secure(print_head)),
       secured(print_head),
       [ instruct_exec(line_up(hole_in_print_head, post)),
	 instruct_exec(lower(print_head)),
	 instruct_exec(push(cartridge_lock_lever)),
	 inform(secured(print_head))] ).

%dplan( instruct_exec(remove_and_reinstall(print_head)),
%       [inform(restart), % "OK, we'll try again"
%	clear, % clear shared^com
%	instruct_exec(remove(print_head)),
%	instruct_exec(reinstall(print_head)) ] ).

% tplan/2
%
% text plan - derived from scenario plan

/*
tplan( instruct_exec(reinstall(print_head)),
       reinstalled(print_head),
       [ instruct_check(moved_forward(carriage_lock)),
	 instruct_exec(secure(print_head)),
	 instruct_exec(close(top_cover)),
	 instruct_exec(reattach(scanner)),
	 instruct_exec(press_and_release(yellow_LED_button)),
	 inform( if( not_move_from_center_position(carriage_head),
		     [ remove(print_head),
		       reinstall(print_head) ] ) ),
	 inform(reinstalled(print_head)),
	 inform(next(prepare_cartridge_for_printing))
       ] ).

tplan( instruct_exec(secure(print_head)),
       secured(print_head),
       [ instruct_exec(line_up(hole_in_print_head, post)),
	 instruct_exec(lower(print_head)),
	 instruct_exec(push(cartridge_lock_lever)),
	 inform(secured(print_head))] ).

*/
tplan( instruct_exec( STask ), Goal, TP ):-
	splan( STask, Goal, pre:Pre, dec:Dec, eff:Eff, next:Next ),
	pre2tplan( Pre, TP1 ),
	dec2tplan( Dec, TP2 ),
	eff2tplan( Eff, TP3 ),
	next2tplan( Next, TP4 ),
	append( TP1, TP2, TP12 ),
	append( TP12, TP3, TP123 ),
	append( TP123, TP4, TP ).


pre2tplan( [], [] ).
pre2tplan( [ Pre | Pres ], [ instruct_check(Pre) | TP ] ):-
	pre2tplan( Pres, TP ).

dec2tplan( [], [] ).
dec2tplan( [ if_then( C, A ) | As ], [ inform( if_then( C, A ) ) | TP ] ) :-
	!, 
	dec2tplan( As, TP ).
dec2tplan( [ A | As ], [ instruct_exec(A) | TP ] ):-
	dec2tplan( As, TP ).

eff2tplan( [], [] ).
eff2tplan( [ Eff | Effs ], [ inform(Eff) | TP ] ):-
	eff2tplan( Effs, TP ).

next2tplan( [], [] ).
next2tplan( [ Next | Nexts ], [ inform(next(Next)) | TP ] ):-
	next2tplan( Nexts, TP ).

% splan/2
%
% scenario plan 

splan( reinstall(print_head),
       reinstalled(print_head),
       pre:[moved_forward(carriage_lock)],
       dec:[secure(print_head),
	    close(top_cover),
	    reattach(scanner),
	    press_and_release(yellow_LED_button),
	    if_then(not_move_from_center_position(carriage_head),
		    remove_and_reinstall(print_head)) ],
       eff:[reinstalled(print_head)],
       next:[prepare_cartridge_for_printing]
     ).


splan(secure(print_head),
      secured(print_head),
      pre:[],
      dec:[line_up(hole_in_print_head, post),
	   lower(print_head),
	   push(cartridge_lock_lever)],
      eff:[secured(print_head)],
      next:[]).

splan( remove_and_reinstall(print_head),
       pre:[],
       dec:[ remove(print_head),
	     reinstall(print_head) ],
       eff:[],
       next:[] ).
     
%relevant_answer( move_from_center_position(carriage_head), yes ).
%relevant_answer( move_from_center_position(carriage_head), no ).

relevant_answer( X^(task(X)), (task(instruct_exec(reinstall(print_head)))) ).


