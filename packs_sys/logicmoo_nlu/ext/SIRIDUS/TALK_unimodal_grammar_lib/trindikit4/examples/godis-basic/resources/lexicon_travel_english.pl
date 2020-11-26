
 
/*************************************************************************

         name: lexicon_travel-english.pl 
      version: 
  description: 
       author: Staffan Larsson
 
*************************************************************************/

:- module( lexicon_travel_english, [output_form/2, input_form/2, yn_answer/1, resource_of_type/1]).

:- discontiguous output_form/2.
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- ensure_loaded( library( semsort_travel ) ).


resource_of_type(lexicon).

/*----------------------------------------------------------------------
     output_form( +Move, -WordList )
     -- Canned output
----------------------------------------------------------------------*/


% greet, quit, reqRep, thank

output_form( greet, ['Welcome to the travel agency!'] ).
output_form( quit, ['Thank you for your visit!'] ).
%output_form( reqRep, ['I didnt understand what you said. Please rephrase.'] ).
output_form( ask(X^(how(X))), ['How do you want to travel?'] ).
output_form( ask(X^(dept_city(X))), ['What city do you want to go from?'] ).
output_form( ask(X^(dest_city(X))), ['What city do you want to go to?'] ).
output_form( ask(X^(month(X))), ['What month do you want to leave?'] ).
output_form( ask(X^(dept_day(X))), ['What day do you want to leave?'] ).
output_form( ask(X^(class(X))), ['What class did you have in mind?'] ).
output_form( ask(X^(citizenship(X))), ['What country are you from?'] ).

output_form( ask(X^issue(X) ), ['How can I help you?']).

% object-level clarification and groundnig questions
output_form( ask(C), Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', is that correct?'], Output ).


output_form( ask(set([Alt0|Alts])), Output):-
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
	append(['Do you mean'|Alt0out], AltsOr, Output0 ),
	append(Output0, ['?'], Output).
altlist2alts_or( [Alt], ['or'|OutputAlt] ):-
	output_form(Alt, OutputAlt ).
altlist2alts_or( [Alt|Alts], [','|Output] ):-
	output_form(Alt, OutputAlt ),
	altlist2alts_or(Alts, AltsOr),
	append( OutputAlt, AltsOr, Output).

output_form( Alt, OutputAlt ):-
	input_form( OutputAlt, answer( Alt ) ).


% answer, get_price_info task
output_form( answer(price(Price)), ['The price is ', Price, ' crowns.']).

% db entries

output_form( answer( db_entry( _PropList, set(NewPropList), P ) ), [''|Output ] ):-
	output_forms( NewPropList, NewPropListOut ),
	output_form( answer( P ), AnswerOut ),
	append( AnswerOut, NewPropListOut, Output ).

%output_form( answer( db_entry( set( List )  ) ), Output ):-
%	output_forms( List, Output ).

output_forms( [], [] ).
output_forms( [ Move | Moves ], Output1 ):-
	output_form( Move, Output ),
	output_forms( Moves, Outputs ),
	append( Output, Outputs, Output1 ).
	


				%
				%output_form( answer( db_entry( PropList, P ) ), Output ):-
%	output_forms( PropList, PropOutput ),
%	output_form( answer( P ), AnsOutput ),
%	append( PropOutput, ['.'.

% visa info
output_form( answer(need_visa), ['Yes, you need a Visa.']).
output_form( answer(not(need_visa)), ['No, you dont need a Visa.']).

output_form( answer(notexist(X,Q)), ['Sorry, there is nothing matching your request about '|InputQDot]):-
	input_form( InputQ, ask(X^Q) ),
	append( InputQ, ['.'], InputQDot ).
output_form( answer(unknown(Q)), ['Sorry, there is nothing matching your request about '|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% for asking metaissue clarification question
output_form( issue(Q), ['to ask about'|Out] ):-
	input_form( Out, ask( Q ) ).



% ICM

% contact
output_form( icm:con*neg, ['Hello?'] ).


% perception
output_form( icm:per*int, ['Pardon?'] ).
output_form( icm:per*int, ['What did you say?'] ).
output_form( icm:per*neg, ['Sorry, I didnt hear what you said.'] ).

output_form( icm:per*pos:String, ['I heard you say',Name,'.'] ):-
	name( Name, String ).

output_form( icm:sem*int, ['What do you mean'] ).
output_form( icm:sem*neg, ['Sorry, I dont understand.'] ).
output_form( icm:sem*pos:Move, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).



% understanding(pragmatic)
output_form( icm:und*neg, ['I dont quite understand.']  ).

output_form( icm:und*pos:usr*issue(Q), ['You want to know about'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).
output_form( icm:und*pos:usr*(not issue(Q)), ['You did not ask about'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).

output_form( icm:und*pos:usr*P, AnsPDot  ):-
	( output_form(P, AnsP);
	    input_form( AnsP, answer(P) ) ),
	append(AnsP,['.'],AnsPDot).


output_form( icm:und*int:usr*C, IcmInt  ):-
	output_form( ask(C), IcmInt ).
	%output_form( icm:und*pos:C, IcmPos ),
	%append( IcmPos0,['.'],IcmPos),
	%append( IcmPos0, [', is that correct?'], IcmInt ).


% clarification question
output_form( icm:und*int:usr*AltQ, Output):-
	output_form( ask(AltQ), Output).



% "acceptance"/integration

% icm-Type(-Polarity(-Args))
output_form( icm:acc*pos, ['Okay.'] ).

% reject(issue(Q))
output_form( icm:acc*neg:issue(Q), ['Sorry, I cannot answer questions about'|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% reject proposition P
output_form( icm:acc*neg:P, ['Sorry, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' is not in the database.'], Rest ).

% indicate loading a plan (pushed by findPlan)
%output_form( icm:loadplan, ['I need some information.'] ).
output_form( icm:loadplan, ['Lets see.'] ).


% reraise issue explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:Q, ['Returning to the issue of '|InputQDot]):-
	( input_form( InputQ, ask(Q) ); output_form( ask(Q), InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise issue (system-initiated, where question follows immediately after)
output_form( icm:reraise, ['So,']).

% accommodation
output_form( icm:accommodate:_Q, ['Alright.']  ).
%:-
%	input_form( AnsP, ask( Q ) ),
%	append(AnsP,['.'],AnsPDot).

output_form( icm:reaccommodate:Q, ['Returning to the issue of'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).


% output form for contents

output_form( citizenship(C), ['You are a citizen of '|S] ):- lexsem(S, C).

output_form( not C, ['Not'|S] ):- output_form( C, S ).


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

% user questions

input_form( [visa], ask(need_visa) ).
input_form( [price], ask(X^price(X)) ).
input_form( [connecting,flights], ask(X^con-flight(X)) ).

				% full answers

% phrase "X to Y" -> "from X to Y" 020326
input_form( S1toS2, [ answer(dept_city(C1)), answer(dest_city(C2)) ] ):-
	\+ var( S1toS2 ),
	append(S1,[to|S2],S1toS2),
	lexsem( S1, C1 ), sem_sort( C1, city ),
	lexsem( S2, C2 ), sem_sort( C2, city ).

% phrase "P X no (P) Y" -> "P Y"  --- OCM detection!!!
input_form( PXnoPY, answer(CPY) ):-
	\+ var( PXnoPY ),
	( append([P|X], [no|Y], PXnoPY );
	    append([P|X], [no|[P|Y]], PXnoPY ) ),
	input_form([P|Y], answer(CPY) ).

% "P X P Y" -> "P Y"?
input_form( PXPY, answer(PY) ):-
	\+ var( PXPY ),
	append([P|_X], [P|Y], PXPY ),
	input_form([P|Y], answer(PY) ).



% phrase "not P X, Y" -> "not P X, P Y"
input_form( NotPXY, [ answer(not(CPX)), answer(CPY) ] ):-
	\+ var( NotPXY ),
	append([not|[P|X]], Y, NotPXY),
	X \= [], Y \= [], 
	input_form([P|X], answer(CPX) ),
	input_form([P|Y], answer(CPY) ).




	

input_form( [to|S], answer(dest_city(C)) ) :- lexsem( S, C ), sem_sort( C, city ).

input_form( [from|S], answer(dept_city(C)) ) :- lexsem( S, C ), sem_sort( C, city ).

input_form( [by|S], answer(how(C)) ) :- lexsem( S, C), sem_sort(C, means_of_transport ).
input_form( [in|S], answer(month(C)) ) :- lexsem( S, C), sem_sort( C, month ).

input_form( [the|S], answer(dept_day(C)) ) :- lexsem( S, C), sem_sort(C, day ).

input_form( S, answer(class(C)) ):- lexsem( S, C ), sem_sort( C, class ).

input_form( [not, return], answer(not(return)) ). % lexsem?
input_form( [one, way], answer(not(return)) ). % lexsem?
input_form( [return], answer(return) ).

	

%input_form( S, answer(C) ):- lexsem( S, C ), sem_sort( C, country ).


% general negation 010419
input_form( [not|S], answer(not(C))):- input_form(S,answer(C)).
input_form( [not|S], answer(not(issue(C)))):- input_form(S,ask(C)).


% elliptical answers, e.g. "london"
% here we need conceptual knowledge

input_form( S, answer(C) ) :- lexsem( S, C ).%, sem_sort( C, top ).

input_form( [yes], answer(yes) ).
input_form( [no], answer(no) ).


% simple stuff

input_form( [hello], greet ).
input_form( [bye], quit ).
input_form( [quit], quit ).

% ICM

input_form( [pardon], icm:per*neg ).
input_form( [okay], icm:acc*pos ).
input_form( [dont, know], icm:acc*neg:issue ).



/*----------------------------------------------------------------------
     yn_answer( ?YN )
----------------------------------------------------------------------*/

yn_answer(A):-
	A = 'yes';
	A = 'no'.

/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     Word means Concept
     -- Lexical semantics
----------------------------------------------------------------------*/

lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).

/*----------------------------------------------------------------------
     synset( ?WordList, ?Concept )
     WordList is the list of words with meaning Concept
----------------------------------------------------------------------*/


% city
synset( [[malmoe],[malmö]], malmoe ).
synset( [[paris]], paris ).
synset( [[london]], london ).
synset( [[gothenburg]], gothenburg ).
synset( [[washington]], washington ).
synset( [[seattle]], seattle ).
synset( [[hongkong]], hongkong ).

% country
synset( [[sweden]], sweden ).
synset( [[britain]], britain ).
synset( [[usa]], usa ).
synset( [[china]], china ).
synset( [[france]], france ).

% means_of_transport
synset( [[flight], [flights], [plane], [fly], [airplane]], plane ).
synset( [[boat]], boat ).
synset( [[train], [rail], [railway]], train ).

% month
synset( [[january]], january).
synset( [[february]], february ).
synset( [[march]], march ).
synset( [[april]], april ).
synset( [[may]], may ).
synset( [[june]], june ).
synset( [[july]], july ).
synset( [[august]], august ).
synset( [[september]], september ).
synset( [[october]], october ).
synset( [[november]], november ).
synset( [[december]], december ).

% day
synset( [[D]], D ) :-dates(Ds), member(D,Ds). % dates/1 defined in semsort.pl

% class
synset( [[cheap],[second,class],[second]], economy ).
synset( [[first,class],[first],[expensive],[business,class]], business ).






