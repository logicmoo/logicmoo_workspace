/***********************************************************************

 
         name: domain_agenda.pl
  description: Domain file with plans and sortal restrictions for AgendaTalk
               Plans: add_event, delete_event, more_info, change_info,
	       change_date, change_time, usage,X^start_time_to_store(X),
	       X^event_to_store(X), X^bookings(X)      
       author: Rebecca Jonson

***************************************************************************/

:- module( domain_agenda, [ plan/2,
			 issue/2,
			 sort_restr/1,
			 isa/2,
			 postcond/2,
			 depends/2,
			 resource_of_type/1
			  ] ).


resource_of_type(domain).


:- discontiguous output_form/3, input_form/2, plan/2, postcond/2.
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:-use_module( calendar, [day2date/2, inconsistent/2, consistent/2] ).

:- ensure_loaded( library( semsort_agendatalk ) ).



/*----------------------------------------------------------------------
     Dialogue plans

     plan( ?ActionOrIssue, ?Plan )
----------------------------------------------------------------------*/
default_question(dummy).
depends(dummy,dummy).

%Actions

plan(top, [forget_all,
	   raise(X^action(X)),
	   findout(set([action(add_event),
	   		action(get_info)
			]))
			
			]).
postcond(top, none).


%%%%%%%%%%%%%%%ADD EVENT%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plan(add_event, [
		findout(X0^event_to_store(X0)),
		findout(X1^date_to_store(X1)),
		findout(X2^start_time_to_store(X2)),
		findout(X3^am_or_pm(X3)),
		findout(X6^take_down_event(X6)),
		if_then_else(take_down_event(yes), [dev_do(agenda,'AddEvent'), report('AddEvent', done),forget_all],
			[findout(set([action(more_info),action(change_info),action(delete_event)]))])
		]).
postcond( add_event, done('AddEvent') ).
postcond(add_event, status('AddEvent',failed(_))).

plan(get_info,[findout(X^issue(X))]).
postcond(get_info,none).
%%%%%%%%%%%%%%%



plan(more_info, [
		findout(X1^location_to_store(X1)), 
		if_then(take_down_event(_), [forget(take_down_event(_))]),
		%if_then(location_to_store(_), [dev_do(agenda, 'MoreInfo')]),
		do(add_event)
		]).
postcond(more_info, none).

%%%delete current event!!!
plan(delete_current_event, [report('AddEvent', failed(_)), forget_all]).
postcond(delete_current_event, status('AddEvent', failed(_))).

plan(change_info, [findout(X1^which_info(X1)),
		   if_then(which_info(date), [forget(date_to_store(_)), findout(X2^date_to_store(X2))]),
		   if_then(which_info(time), [forget(start_time_to_store(_)), forget(am_or_pm(_)), findout(X3^start_time_to_store(X3)), findout(X5^am_or_pm(X5))]),
		   if_then(which_info(location),[if_then_else(location_to_store(_),[forget(location_to_store(_)), findout(X4^location_to_store(X4))], [report('InfoChanged', failed(_))])]),
		   if_then(take_down_event(_),[forget(take_down_event(_))]),
		   %dev_do(agenda, 'InfoChanged'),
		   forget(which_info(X1)),
		   %up
		   do(add_event)
			   ]).
postcond(change_info, done('InfoChanged')).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plan(delete_event,[findout(X1^event_to_store(X1)),
		   findout(X2^olddate(X2)),
		   dev_do(agenda,'DeleteEvent')] ).
postcond(delete_event, done('DeleteEvent')).
postcond(delete_event, status('DeleteEvent',failed)).


plan(change_time,[findout(X1^event_to_store(X1)),
		   findout(X2^olddate(X2)),
		   findout(X3^newtime(X3)),
		   findout(X3^am_or_pm(X3)),
		   dev_do(agenda,'ChangeTime')] ).
postcond(change_time, done('ChangeTime')).
postcond(change_time, status('ChangeTime',failed(_))).

plan(change_date,[findout(X1^event_to_store(X1)),
		   findout(X2^olddate(X2)),
		   findout(X3^newdate(X3)),
		  %%same time? else findout(X4^newtime(X4))
		  %%%and dev_do(agenda,'Change_DateTime')
		   dev_do(agenda,'ChangeDate') ] ).
postcond(change_date, done('ChangeDate')).
postcond(change_date, status('ChangeDate',failed(_))).
%%%%User Questions%%%%

plan(X^start_time_to_store(X),[ findout(X1^event_to_store(X1)),
				findout(X2^olddate(X2)),
				dev_query(agenda,time(X)) ] ).


plan(X^event_to_store(X),[findout(X1^date_to_store(X1)),
			  findout(X2^start_time_to_store(X2)),
		          findout(X3^am_or_pm(X3)),
			  dev_query(agenda, event(X))	
		 ]).

plan(X^bookings(X),[findout(X1^date_to_store(X1)),
		    dev_query(agenda, bookings(X))	
		 ]).

plan(X^todaysdate(X),[
		    dev_query(agenda, todaysdate(X))	
		 ]).

%user help (question)
plan(usage,[ dev_query(agenda,usage) ]).

%other...
plan( up, [] ).

%changin language
/*plan( change_language,
      [ raise(X^language(X)),
	findout(set([language(english), language(svenska)])),
	change_language ] ).
postcond( change_language, done(change_language) ).*/
/*--------------------------------------------------------------
     Conceptual knowledge
----------------------------------------------------------------------*/

% sortal restrictions; determine relevance and resolvement

sort_restr(usage).



%%%Agendatalk

sort_restr( event_to_store(X)) :- sem_sort(X, event).
sort_restr( location_to_store(X)) :- sem_sort(X, location).
sort_restr( location(X)) :- sem_sort(X, location).
sort_restr( start_time_to_store( X ) ) :- sem_sort( X, time ).
sort_restr( stop_time_to_store( X ) ) :- sem_sort( X, time ).
sort_restr( newtime( X ) ) :- sem_sort( X, time ).
sort_restr( date_to_store( X ) ) :- sem_sort( X, date ).
sort_restr( newdate( X ) ) :- sem_sort( X, date ).
sort_restr( olddate( X ) ) :- sem_sort( X, date ).
sort_restr( number( X ) ) :- sem_sort( X, number ).
sort_restr( date( X ) ) :- sem_sort( X, date ).
sort_restr( time( X ) ) :- sem_sort( X, time ).
sort_restr( time( X ) ) :- sem_sort( X, number ).
sort_restr( event( X ) ) :- sem_sort( X, event ).
sort_restr( attendee(X)):- sem_sort(X, person).
sort_restr( add_more_info(X)):-sem_sort(X,yn_answer).
sort_restr( take_down_event(X)):-sem_sort(X,yn_answer).
sort_restr( am_or_pm(X)):-sem_sort(X,dayhalf).
sort_restr( which_info(X)):-sem_sort(X, info).
sort_restr( bookings([E,T])):-check_events(E),check_times(T).
sort_restr( todaysdate(X)):- sem_sort(X, date).
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

check_events([]).
check_events([E|Events]):-					     
	sem_sort(E, event),
	check_events(Events).
check_times([]).
check_times([T|Times]):-
	sem_sort(T,time),
	check_times(Times).
%AgendaTalk
valid_parameter( start_time_to_store(N) ):- sem_sort( N, time ).
valid_parameter( start_time_to_store(N) ):- sem_sort( N, number ).
valid_parameter( stop_time_to_store(N) ):- sem_sort( N, time ).
valid_parameter( newtime(N) ):- sem_sort( N, time ).
valid_parameter( date_to_store( X ) ) :- sem_sort( X, date ), calendar:day2date(X, DATE),calendar:consistent(DATE,yes).
valid_parameter( newdate( X ) ) :- sem_sort( X, date ), calendar:day2date(X, DATE),calendar:consistent(DATE,yes).
valid_parameter( olddate( X ) ) :- valid_parameter( date_to_store(X)).
valid_parameter( event_to_store(X)):-sem_sort(X,event).
valid_parameter( location_to_store(X)):- sem_sort(X,location).
valid_parameter( attendee(X)):-sem_sort(X, person).
valid_parameter( age_to_store(X)):-sem_sort(X, number).
valid_parameter( am_or_pm(X)):-sem_sort(X, dayhalf).
valid_parameter( which_info(X)):-sem_sort(X, info).


incompatible(P,P):-singleton(P).
singleton(start_time_to_store).
singleton(event_to_store).
singleton(am_or_pm).
singleton(date).
singleton(time).

