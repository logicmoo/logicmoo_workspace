
 
/*************************************************************************

         name: database_travel.pl 
      version: 
  description: An example travel-domain database
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

:- module( database_travel, [consultDB/3, consultDBx/3, validDBquery/2, validDBparameter/1] ).

% loaded by module database.pl

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(terms), [ term_variables/2 ] ).

/*----------------------------------------------------------------------
     consultDB( +Question, +PropList, -AnswerList )
     -- Returns (if it succeeds) an Answer to Question given
        PropList
----------------------------------------------------------------------*/

consultDB( Query, set(PropList), Answer ):-
        post( Post ),
	% unify question with answer in post, remove answer from post
	%( ( Query = _^Answer; Query=Answer ) ; Answer = (not Query) ),
	db_resolves( Query, Answer ),
        select( Answer, Post, Post1 ), 
        % check that known facts are consistent with db post, i.e.
        % anything in the post is also in the proplist
        \+ (
             member( Prop, Post1 ),
             \+ member( Prop, PropList )
           ).

% extended format: returns set of answers; each answer has the form
% db_entry( set(PropList), set(NewProps), Answer )

consultDBx( Q, set(Ps), set(As) ):-
	term_variables( Q, QVars ),
	setof( A, QVars^consultDB0( Q, set(Ps), A ), As ).

consultDB0( Q, set(PropList), db_entry( set(PropList1), set(NewProps), Answer) ):-
        post( Post ),
	% unify question with answer in post, remove answer from post
        db_resolves( Q, Answer ),
        select( Answer, Post, Post1 ), 
        % check that known facts are consistent with db post, i.e.
	% anything property specified in proplist is has same value in post
        \+ (
             member( Prop, Post1 ),
             incompatibles( Prop, PropList )
           ),
	delete_all_negative( PropList, PropList1 ),
	delete_all( PropList1, Post1, NewProps ).
% whq
db_resolves( _X^PX, PX ).
% y/n
db_resolves( P, P ).
db_resolves( P, not(P) ).

% select_all( Elements, List1, List2 )
% select all Elements from List1 to get List2
% if Element is not in List1, that's okay
delete_all( [], L, L ).
delete_all( [E|Es], L1, L3 ):-
	\+ member( E, L1 ),
	delete_all( Es, L1, L3 ).
delete_all( [E|Es], L1, L3 ):-
	select( E, L1, L2 ),
	delete_all( Es, L2, L3 ).

delete_all_negative( L1, L2 ):-
	delete_all( [not(_)], L1, L2 ).

incompatible( PX, PY ):-
	PX =.. [P,X],
	PY =.. [P,Y],
	X \= Y.
incompatible( P, not(P) ).

% P is in compatible with list of proposition Ps
incompatibles( P, Ps ):-
	member( P1, Ps ),
	incompatible( P, P1 ).
	

% relaxed version: set of aswers, each answer related to info from post which was not in proplist

post( [  dept_city(london), dest_city(paris), how(plane), month(april) , class(economy), price(123) ] ).
post( [  dept_city(london), dest_city(paris), how(plane), month(april), dept_day(_), class(business), price(1234) ] ).
post( [  dept_city(gothenburg), dest_city(london), how(plane), month(april), dept_day(_), class(economy), price(4567) ] ).
post( [  dept_city(gothenburg), dest_city(paris), how(plane), month(april), dept_day(_), class(economy), price(7654) ] ).

post( [ dest_city( paris ), citizenship(sweden), need_visa ] ).
post( [ dest_city( london ), citizenship(sweden), (not need_visa) ] ).
post( [ dest_city( london ), citizenship(usa), need_visa ] ).

day(D):-member(D,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]).

/*
post( [ flight(f12), from(london), to(paris), how(plane), month(april), class(economy), (not return), price(123) ] ).
post( [ flight(f144), from(london), to(paris), how(plane), month(april), class(business), (not return), price(1234) ] ).
*/


	


validDBparameter( Prop ):-
	post( Post ),
	member( Prop, Post ).
