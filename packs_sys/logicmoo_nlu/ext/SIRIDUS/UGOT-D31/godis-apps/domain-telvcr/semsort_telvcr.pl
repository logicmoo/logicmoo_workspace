:- module(semsort_telvcr, [sem_sort/2, isa/2] ).

%D changes 26/9 channel not dynamic anymore
%:- use_module(lexicon_vcr_english,[channel/1]). %jabben changes



sem_sort( english, language ).
sem_sort( svenska, language ).

% action
sem_sort( top, action ).
sem_sort( add_rec_job, action ).
sem_sort( delete_rec_job, action ).
sem_sort( list_rec_jobs, action).


% D 26/9 not dynamic anymore, (see lexicon_vcr_english.pl)
% channel
%sem_sort(C,channel):-
%	channel(C). %imported dynamic pred

sem_sort( svt1, channel).
sem_sort( svt2, channel).
sem_sort( tv3, channel).
sem_sort( tv4, channel).
sem_sort( tv5, channel).
sem_sort( tve, channel).
sem_sort( raiuno, channel).
sem_sort( sat1, channel).
sem_sort( cnn, channel).
sem_sort( bbcworld, channel).
sem_sort( tvpolonia, channel).
sem_sort( dr1, channel).
sem_sort( dr2, channel).
sem_sort( nrk1, channel).
sem_sort( nrk2, channel).


%rec_job
sem_sort( N, rec_job ) :-
	integer(N),
	N >= 1,
	N =< 100.

sem_sort(rec_job([ID,Usr,Channel,Start,Stop]),rec_job):-
	sem_sort(Channel,channel).

% number
sem_sort( N, number ) :-
	integer(N).

% date
sem_sort( today, date ).
sem_sort(tomorrow, date ).
sem_sort(monday,date).
sem_sort(tuesday,date).
sem_sort(wednesday,date).
sem_sort(thursday,date).
sem_sort(friday,date).
sem_sort(saturday,date).
sem_sort(sunday,date).



% time
sem_sort( T, time ) :-
	atom(T),
	atom_chars(T,[H1,H2,M1,M2]),
	number_chars(H,[H1,H2]),
	number_chars(M,[M1,M2]),
	H >= 0,
	H =< 23,
	M >= 0,
	M =< 59.



/*--------------------
conceptual hierarichy
--------------------*/


%isa( channel, C ):- channel(C).
%isa( time, number ).
%isa( rec_job, number).

isa( channel_to_store, channel ).
isa( date_to_store, date ).
isa( start_time_to_store, time ).
isa( stop_time_to_store, time ).
isa( rec_job_to_delete, rec_job ).

isa( T0, T2 ):-
	T0 \= T2,
	isa( T0, T1 ),
	isa( T1, T2 ).



