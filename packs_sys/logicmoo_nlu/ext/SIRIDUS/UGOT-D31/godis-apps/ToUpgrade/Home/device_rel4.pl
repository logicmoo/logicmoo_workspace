:- module( device_rel4, [ dev_set/2,
		  dev_get/2,
		  dev_do/2,
		  dev_query/2] ).

:- use_module( library(device_rel) ).


dev_set( onoff, on ):-
	rel_switch_on('REL4').
	

dev_set( onoff, off ):-
	rel_switch_off('REL4').


dev_get( onoff, on ):-
	rel_is_switched_on('REL4').


dev_get( onoff, off ):-
	rel_is_switched_off('REL4').
