:- module( device_rel, [ rel_switch_on/1,
			 rel_switch_off/1,
			 rel_is_switched_on/1,
			 rel_is_switched_off/1
			] ).
:- use_module( library(device_home) ).

rel_switch_on(D) :-
	set_node_value(D,'1.0').

rel_switch_off(D) :-
	set_node_value(D,'0.0').

rel_is_switched_on(D) :-
	get_node_value(D,'1.0').

rel_is_switched_off(D) :-
	get_node_value(D,'0.0').
