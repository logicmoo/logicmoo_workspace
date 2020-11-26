/*----------------------------------------------------------------------
    sort_restr( +Prop )

    Prop fulfils the sortal restrictions on propositions
----------------------------------------------------------------------*/

sem_sort( english, language ).
sem_sort( svenska, language ).

sem_sort( telephone, domain ).
sem_sort( vcr, domain ).
sem_sort( tram, domain ).

% Values for information amounts
sem_sort( no, info_val ).
sem_sort( min, info_val ).
sem_sort( interm, info_val ).
sem_sort( max, info_val ).
sem_sort( ground, info_val ).
sem_sort( compl, info_val ).
sem_sort( indet, info_val ).

% Tram stops
sem_sort( tynnered, stop ).
sem_sort( marklandsgatan, stop ).
sem_sort( botaniska-tradgarden, stop ).
sem_sort( linneplatsen, stop ).
sem_sort( olivedalsgatan, stop ).
sem_sort( seminariegatan, stop ).
sem_sort( brunnsgatan, stop ).
sem_sort( handelshogskolan, stop ).
sem_sort( vasa-viktoriagatan, stop ).
sem_sort( vasaplatsen, stop ).
sem_sort( gronsakstorget, stop ).
sem_sort( domkyrkan, stop ).
sem_sort( brunnsparken, stop ).
sem_sort( centralstationen, stop ).
sem_sort( ostra-sjukhuset, stop ).






% action
sem_sort( vcr_top, action ).
sem_sort( vcr_change_play_status, action ).
sem_sort( vcr_play, action ).
sem_sort( vcr_stop, action ).
sem_sort( vcr_ff, action ).
sem_sort( vcr_rew, action ).
sem_sort( vcr_pause_still, action ).
sem_sort( vcr_rec, action ).
sem_sort( vcr_new_channel, action ).
sem_sort( vcr_rec, action ).
sem_sort( vcr_timer_recording, action ).
sem_sort( vcr_add_program, action ).
sem_sort( vcr_delete_program, action ).
sem_sort( vcr_settings, action ).
sem_sort( vcr_set_clock, action ).
sem_sort( vcr_increase_channel, action ).
sem_sort( vcr_decrease_channel, action ).

sem_sort( change_language, action ).
sem_sort( change_domain, action ).

sem_sort( vcr_turnofftv, action ).
sem_sort( vcr_turnontv, action ).


% channel
sem_sort( N, channel) :-
	%to_number(P,N),
	integer(N),
	N >= 1,
	N =< 99.
sem_sort( P, channel) :-
	to_number(P,N),
	integer(N),
	N >= 1,
	N =< 99.

% program
sem_sort( N, program ) :-
%	to_number(P,N),
	integer(N),
	N >= 1,
	N =< 8.

% number

sem_sort( N, number ) :-
	integer(N).

% play_status
sem_sort( playing, play_status ).
sem_sort( stopped, play_status ).
sem_sort( paused, play_status ).
sem_sort( ff, play_status ).
sem_sort( rewinding, play_status ).
sem_sort( recording, play_status ).

% date
sem_sort( today, date ).
sem_sort( D, date ) :-
	number_atom(D),
	atom_chars(D,[D1,D2,D3,D4]),
	number_chars(Day,[D1,D2]),
	Day >= 1,
	Day =< 31,
	number_chars(Month,[D3,D4]),
	Month >= 1,
	Month =< 12.

% time
sem_sort( T, time ) :-
	%number_atom(T),
	%atom_chars(T,[T1,T2,T3,T4]),
	integer( T ),
	name( T, [ T1, T2, T3, T4 ] ),
	name( Hour, [ T1, T2 ] ),
	Hour >= 0,
	Hour =< 23,
	name( Min, [ T3, T4 ] ),
	Min >= 0,
	Min =< 59.
sem_sort( T, time ) :-
	%number_atom(T),
	%atom_chars(T,[T1,T2,T3,T4]),
	integer( T ),
	name( T, [ T2, T3, T4 ] ),
	T1 = 48,
	name( Hour, [ T1, T2 ] ),
	Hour >= 0,
	Hour =< 23,
	name( Min, [ T3, T4 ] ),
	Min >= 0,
	Min =< 59.


to_number( Atom, Number ) :-
%	name( Atom, String ),
	number_atom( Atom ),
	atom_chars( Atom,Cs ),
	number_chars( Number, Cs ).

number_atom(A) :-
	atomic(A),
	\+ number(A).


/*--------------------
conceptual hierarichy
--------------------*/

%Tram
isa( dep_stop, stop ).
isa( arr_stop, stop ).
isa( speech_val, info_val ).
isa( graph_val, info_val ).

isa( channel, number ).
isa( date, number ).
isa( time, number ).
isa( program, number ).

isa( channel_to_store, channel ).
isa( new_channel, channel ).
isa( date_to_store, date ).
isa( start_time_to_store, time ).
isa( stop_time_to_store, time ).
isa( new_clock, time ).
isa( program_to_delete, program ).

isa( T0, T2 ):-
	T0 \= T2,
	isa( T0, T1 ),
	isa( T1, T2 ).
