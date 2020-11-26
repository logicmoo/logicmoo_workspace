
 
/*************************************************************************

         name: lexicon_autoroute-english.pl 
      version: 
  description: An example lexicon file
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

:- module( lexicon_autoroute_english, [output_form/2, input_form/2,
				       input_form2/3, yn_answer/1] ).

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).



/*----------------------------------------------------------------------
     output_form( +Domain, +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/

%output_form( travel-english, Phrase, Move ):-
%	output_form( Phrase, Move ).


output_form( greet, "Welcome to the Route Planning Service." ).
output_form( thank, "Thank you for using the Route Planning Service." ).

output_form( ask(X^(task(X))), "How can I help you." ).
output_form( quit, "Thank you for your visit!" ).
output_form( reqRep(understanding), "I didnt understand that. Please rephrase." ).
output_form( reqRep(relevance), "What do you mean by that?" ).

output_form( ask(X^(from(X))), "Where would you like to start your journey." ).
output_form( ask(X^(to(X))), "Where would you like to go." ).
output_form( ask(X^(time(X))), "What time would you like to make your journey." ).
output_form( ask(X^(quickshort(X))), "Would you like the quickest or the shortest route." ).
output_form( ask(X^(sendinstruction(X))), "Would you like me to send the instructions to you." ).

output_form( answer(X^dist(X),dist(Dist)), Str ) :-
	number( Dist ), number_chars( Dist, DistStr ),
	append( "The quickest route is ", DistStr, Str1 ),
	append( Str1, " miles.", Str ).

output_form( answer(X^dur(X),dur(H:M)), Str ) :-
	number( H ), number_chars( H, HStr ),
	number( M ), number_chars( M, MStr ),
	append( "It will take you ", HStr, Str1 ),
	append( Str1, " hours ", Str2 ),
	append( Str2, MStr, Str3 ),
	append( Str3, " minutes.", Str ).


output_form( repeat(Move), Str ):-
	output_form( Move, Str ).

output_form( thank, "Thank you very much." ).

/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

%input_form( autoroute-svenska, Phrase, Move ):-
%	input_form( Phrase, Move ).


input_form( [hello], greet ).
input_form( [bye], quit ).
input_form( [quit], quit ).
input_form( [what,did,you,say], reqRep ).
input_form( [what], reqRep ).
input_form( [yes], answer(yes) ).
input_form( [no], answer(no) ).
input_form( [okay], ack ).
input_form( [ok], ack ).

input_form( [to|S], answer(to(C)) ) :- lexsem( S, C ), location( C ).
input_form( [from|S], answer(from(C)) ) :- lexsem( S, C ), location( C ).
input_form( [at|S], answer(time(C)) ) :- lexsem( S, C), time( C ).
input_form( [journey], answer(task(get_trip_info))).
input_form( [route], answer(task(get_trip_info))).

input_form( [C], answer(C) ) :- concept( C ).
input_form( S, answer(C) ) :- lexsem( S, C ), concept( C ).

yn_answer(A):-
	A = 'yes';
	A = 'no'.

% don't use complex interpretation

input_form2( _, _, _ ) :- fail.

/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).


synset( [[L]], L ):-location( L ).

synset( [[one]], 1 ).
synset( [[two]], 2 ).
synset( [[three]], 3 ).
synset( [[four]], 4 ).
synset( [[five]], 5 ).
synset( [[six]], 6 ). 


/*----------------------------------------------------------------------
     - Conceptual knowledge
----------------------------------------------------------------------*/

concept( C ):-
	location( C ) ;
	time( C );
	quickshort( C );
	task( C ).

location( malvern ).
location( edwinstowe ).

hour(H):-
	datatypes:of_type(H, integer),
	H<12,
	H>0.

minute(M):-
	datatypes:of_type(M, integer),
	M<60,
	M>=0.

time(H):-
	hour(H).

time(H-am):-
	hour(H).

time(H-pm):-
	hour(H).

dist(D):-
	datatypes:of_type(D, integer).

dur(H:M):-
	hour(H),
	minute(M).

task( get_trip_info ).

quickshort(quickest).
quickshort(shortest).
