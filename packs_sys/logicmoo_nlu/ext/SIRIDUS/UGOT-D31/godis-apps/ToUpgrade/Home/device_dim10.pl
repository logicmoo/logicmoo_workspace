:- module( device_dim10, [ dev_get/2, dev_set/2 ] ).

:- use_module( library(device_dimmer) ).

%set_dimmer(V) :-
dev_set(dimmer, V):-
	dimmer_set_value('DIM10',V).

%read_dimmer(V) :-
dev_get(dimmer, V):-
	dimmer_get_value('DIM10',V).

dev_set( onoff, on ):-
	dimmer_set_value('DIM10',1.0).

dev_set( onoff, off ):-
	dimmer_set_value('DIM10',0.0).

dev_get( onoff, on ):-
	dimmer_get_value('DIM10',V),
	V > 0.0.

dev_get( onoff, off ):-
	dimmer_get_value('DIM10',0.0).
