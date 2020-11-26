/*************************************************************************

         name: semsort_lamps.pl 
	 date: 2004-11-23
       author: Andreas Wallentin
 
*************************************************************************/

:- ensure_loaded( [lamps] ).

/*----------------------------------------------------------------------
    sort_restr( +Prop )

    Prop fulfils the sortal restrictions on propositions
----------------------------------------------------------------------*/

sem_sort( lamps, domain ).
sem_sort( lamps_restart,   action ).
sem_sort( lamps_turn_on,   action ).
sem_sort( lamps_turn_off,  action ).
sem_sort( add_lamp,        action ).
sem_sort( remove_lamp,     action ).
sem_sort( all_lamps_on,    action ).
sem_sort( all_lamps_off,   action ).

%%% should use used_lamp(L), 
%%% not online yet
sem_sort( Lamp, lamp ):-
	is_lamp(Lamp).
	%format("letar efterlamp i semsort ~n",[]),
	%exists(Lamp).%%Lamp == [lampa]

%%% olika lampor => lamp?
isa( lamp_to_turn_on,  lamp ).
isa( lamp_to_turn_off, lamp ).
isa( lamp_to_add,      lamp ).
isa( lamp_to_remove,   lamp ).

isa( T0, T2 ):-
	T0 \= T2,
	isa( T0, T1 ),
	isa( T1, T2 ).
