
/*************************************************************************
         name: domain_cellphone.pl
      version: Feb, 2005
  description: Mini Mobile Phone domain file
      authors: Anna Olsson and Jessica Villing
*************************************************************************/
:- module( domain_cellphone, [ plan/2,
			       postcond/2,
			       sort_restr/1,
			       valid_parameter/1
			     ] ).

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2, incompatible/2.

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

:- ensure_loaded( library( semsort_cellphone ) ).


%%% Definition of yes/no questions

ynq( YNQ ):-
	YNQ \= _^_,		% whq
	YNQ \= [_|_].		% altq


/*----------------------------------------------------------------------
     plan( ?Name, ?Plan )
     -- Dialogue plan
----------------------------------------------------------------------*/

default( something_else ).

% ------------------------------
%%%% TOP
% ------------------------------

plan( top,
      [ forget_all,
	raise( X^action(X) ),
	findout( set( [ action(call),
			action(phonebook),
			action(settings) ] ) ) ] ).
postcond( top, none ).

% ------------------------------
%%%% CALL
% ------------------------------

plan( call,
      [ findout( set( [ action(call_name),
			action(call_number) ] ) ) ] ).
postcond( call, done(call_name) ).
postcond( call, done(call_number) ).

plan( call_name,
      [ findout( X^name(X) ),
	dev_do(cellphone, 'Call_name')
      ] ).
postcond( call_name, done('Call_name') ).

plan( call_number,
      [ findout( X^number(X) ),
	dev_do(cellphone, 'Call_number')
      ] ).
postcond( call_number, done('Call_number') ).


% ------------------------------
%%%% PHONEBOOK
% ------------------------------

plan( phonebook,
      [ findout( set( [ action(search_phonebook),
			action(add_new_entry),
			action(delete_entry) ] ) ) ] ).
postcond( phonebook, done(search_phonebook) ).
postcond( phonebook, done(add_new_entry) ).
postcond( phonebook, done(delete_entry) ).

plan( search_phonebook,
      [ findout( X1^name(X1) ),
	dev_do(cellphone, 'Search')
      ] ).  
postcond( search_phonebook, done('Search') ).


plan( add_new_entry,
      [ findout( X1^name(X1) ),
	findout( X2^number(X2) ),
	dev_do(cellphone, 'Add')
      ] ).
postcond( add_new_entry, done('Add') ).


plan( delete_entry,
      [ findout( X1^name(X1) ),
	%kolla mot databasen om entryt finns,
	%gör i så fall borttag.
	%finns det inte så gör respond att det inte fanns
	dev_do(cellphone, 'Delete')
      ] ).
postcond( delete_entry, done('Delete') ).




% ------------------------------
%%%% SETTINGS
% ------------------------------

plan( settings,
      [ findout( set( [ action(telephone_settings),
			action(security_settings),
			action(reset) ] ) ) ] ).

postcond( settings, done(telephone_settings) ).
postcond( settings, done(security_settings) ).
postcond( phonebook, done(reset) ).


plan( telephone_settings,
      [ findout( action(change_language) )
      ] ).
postcond( telephone_settings, done(telephone_settings) ).


plan( change_language,
      [ findout( X^language(X) ),
	dev_do( cellphone, 'Set_language' )
      ] ).
postcond( change_language, done('Set_language') ).


plan( security_settings,
      [ findout( action(change_security_code) )
      ] ).
postcond( security_settings, done(change_security_code) ).

%Här krävs det att nummer fungerar
plan( change_security_code,
      [ findout( X1^number(X1) ),
	dev_do( cellphone, 'Set_security_code' )
      ] ).
postcond( change_security_code, done('Set_security_code') ).

% needed: some way of confirming wrong user input code..
%Även här krävs det att nummer fungerar
plan( reset,
      [ findout( X^number(X) ),
	dev_do( cellphone, 'Reset')
      ] ).

postcond( reset, done('Reset') ).


% Lagt till sort_restr/1:
% ------------------------------
%%%% Conceptual knowledge
% ------------------------------

% action
sort_restr( action( X ) ) :-
	sem_sort( X, action ).
sort_restr( action( respond(Q) ) ) :-
	sort_restr( issue(Q) ).

% issue
sort_restr( issue(Q) ):-
	plan( Q, _ ),
	\+ sort_restr( action( Q ) ).

sort_restr( issue(Q) ):-
	plan( _, Plan ),
	member( findout(Q), Plan ),
	\+ sort_restr( action( Q ) ).

sort_restr( name( X ) ) :-
	sem_sort( X, name).

sort_restr( language( X ) ) :-
	sem_sort( X, language).

sort_restr( number( X ) ) :-
	sem_sort( X, number).

% Negation
sort_restr( \+ P ) :-
	sort_restr( P ).

% metaissue
sort_restr( und(_DP*P) ):-
	sort_restr(P).


% Lagt till valid_parameter/1:
% general case
valid_parameter( PX ):- % SL040831
	  PX =.. [P,X],
	  sem_sort(X, P).

% parameter validity; determines acceptance

valid_parameter( name( N ) ):- sem_sort( N, name ).

valid_parameter( number( N ) ):- sem_sort( N, number ).

valid_parameter( language( N ) ):- sem_sort( N, language ).

valid_parameter( security_code( N ) ):-
	sem_sort( N, number ).

% Lagt till depends/2:
depends(bla,bla).


% Lagt till incompatible/2 och singleton/1:
/*--------------------------------------------------------------
     Incompatible propositions cannot be in COM at the same time
----------------------------------------------------------------------*/


incompatible(Prop1,Prop2):-
	Prop1 =.. [P,_],
	Prop2 =.. [P,_],
	singleton(P).

singleton(add_entry).
singleton(delete_entry).


% Lagt till default_question/1:
/*--------------------------------------------------------------
     Default questions
     
     If A relevant to both Q1 and Q2, and none on QUD or ISSUES, and
     Q1 is default_question, then assume A is answer to Q1.

----------------------------------------------------------------------*/

default_question(dummy).