:- module( device_dimmer, [ dimmer_set_value/2,
			    dimmer_get_value/2 ] ).

:- use_module( library(device_home) ).

dimmer_set_value(D,N) :-
	number_chars(N,Cs),
	atom_chars(V,Cs),
	set_node_value(D,V).

dimmer_get_value(D,N) :-
	get_node_value(D,V),
	atom_chars(V,Cs),
	number_chars(N,Cs).
