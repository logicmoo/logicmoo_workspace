
:- module( domain_telvcr, [ plan/2,
			 issue/2,
			 sort_restr/1,
			 isa/2,
			 postcond/2,
			 depends/2
			  ] ).

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

:- ensure_loaded( library( semsort_telvcr ) ).
%:- ensure_loaded( library( plan_operators ) ).


/*----------------------------------------------------------------------
     Dialogue plans

     plan( ?ActionOrIssue, ?Plan )
----------------------------------------------------------------------*/
default_question(dummy).
depends(dummy,dummy).

%Actions

plan(top, [%forget_all,
	   forget_except(user_name(_)),
	   raise( X^action(X) ),
	   findout( set( [ action(add_rec_job),
			   action(delete_rec_job),
			   action(list_rec_jobs) ] ) ) ] ).
postcond( top, none ).

plan(add_rec_job,[ findout(X1^channel_to_store(X1)),
		   findout(X2^date_to_store(X2)),
		   findout(X3^start_time_to_store(X3)),
		   findout(X4^stop_time_to_store(X4)),
		   dev_query(vcr, valid_recjob(Res) ),
 		   if_then( valid_recjob(Result),
			    [ report('AddRecording',Result) ]),
		   if_then_else( valid_recjob(ok),
				 [dev_do(vcr, 'AddRecording')],
				 [ forget_except(user_name(_A))]) ] ).

postcond( add_rec_job, done( 'AddRecording' ) ).


plan(delete_rec_job,[ %lista inspelningar
		      dev_query(vcr,rec_jobs(Jobs)),
		      if_then( rec_jobs(Jobs),
			       report(vcr,rec_jobs(Jobs))),

		      %om det inte fanns några gör inget
		      if_then_else( rec_jobs([]),
				    [],
				%annars:
		      [ findout(X^rec_job_to_delete(X)),
			if_then( rec_job_to_delete(X),
				 [ dev_query(vcr, rec_job_exists(X))]),

			%om recjob fanns 
			if_then_else( rec_job_exists(X),
				      [ dev_do(vcr, 'DeleteRecording') ],
				      [ report('DeleteRecording',
					       failed(no_such_program) ) ]
				    )
		      ] ) ] ).

postcond( delete_rec_job, done( 'DeleteRecording' ) ).
postcond( delete_rec_job, status( 'DeleteRecording', failed(_) ) ).


%Questions

plan(X^rec_jobs(X),
     [ dev_query(vcr,rec_jobs(X)) ] ).

plan(X^channels(X),
     [ dev_query(vcr,channels(X)) ] ).

%user help (question)
plan(usage,[ dev_query(vcr,usage) ]).


%other...

plan( up, [] ).



/*--------------------------------------------------------------
     Conceptual knowledge
----------------------------------------------------------------------*/

% sortal restrictions; determine relevance and resolvement

sort_restr(usage).

sort_restr( channels([]) ).
sort_restr( channels([C|Cs]) ):-
	sem_sort(C,channel),
	sort_restr( channels(Cs)).

%pretty ugly here, but:

%output - list of recjobs
sort_restr( rec_jobs([]) ).
sort_restr( rec_jobs([C|Cs]) ):-
	sort_restr( C ),
	sort_restr( rec_jobs(Cs)).

%input - a number
sort_restr( rec_job( X ) ) :- sem_sort( X, rec_job ).

%output - a rec job
sort_restr( recJob( _ ,_,_,_,_) ).

sort_restr( channel_to_store( X ) ) :- sem_sort( X, channel ).
sort_restr( start_time_to_store( X ) ) :- sem_sort( X, time ).
sort_restr( stop_time_to_store( X ) ) :- sem_sort( X, time ).
sort_restr( date_to_store( X ) ) :- sem_sort( X, date ).

%HACK
sort_restr( user_name(_)).

sort_restr( rec_job_to_delete( X ) ) :-
	sem_sort( X, rec_job ).

sort_restr( rec_job(X)):-
	sem_sort(X, rec_job).

sort_restr( number( X ) ) :- sem_sort( X, number ).
%sort_restr( channel( X ) ) :- sem_sort( X, channel ).
sort_restr( date( X ) ) :- sem_sort( X, date ).
sort_restr( time( X ) ) :- sem_sort( X, time ).





% negation

sort_restr( not P ) :- sort_restr( P ).

% action

sort_restr( action( X ) ) :- sem_sort( X, action ).
sort_restr( action( respond(Q) ) ) :- sort_restr( issue(Q) ).

sort_restr( issue(Q) ):- plan( Q, _ ),
        \+ sort_restr( action( Q ) ).

sort_restr( issue(Q) ):-
        plan( _, Plan ),
        member( findout(Q), Plan ),
        \+ sort_restr( action( Q ) ).

% metaissue

sort_restr( und(_DP*P) ):- sort_restr(P).

% sloppy, but allows "no" as answer to clarification alt-q
% could be replaced by general rule saying that not(set([p1, ..., ])) is sortally correct,
% menaing the same as (not P1 and not p2 and...)

sort_restr( not und(_DP*set(_))).


%David 26/9: this is what should be dynamically loaded, see device_vcr
%for valid_parameter concerning channels

% parameter validity; determines acceptance

%valid_parameter( new_channel(N) ):- sem_sort( N, channel ).
%moved to device_vcr (as in original IBiS
%valid_parameter( channel_to_store(N) ):- sem_sort( N, channel ).
%valid_parameter( current_channel(N) ):- sem_sort( N, channel ).


valid_parameter( start_time_to_store(N) ):- sem_sort( N, time ).
valid_parameter( stop_time_to_store(N) ):- sem_sort( N, time ).

valid_parameter( date_to_store( X ) ) :- sem_sort( X, date ).

valid_parameter( rec_job_to_delete(number(X)) ):- sem_sort( X, number).

%HACK
valid_parameter( user_name( _ ) ). 

%valid_parameter( new_clock( X ) ) :- sem_sort( X, time ).

%valid_parameter( play_status( X ) ) :- sem_sort( X, play_status ).

