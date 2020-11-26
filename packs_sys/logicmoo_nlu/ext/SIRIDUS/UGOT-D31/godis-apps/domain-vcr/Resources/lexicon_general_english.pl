
 
/*************************************************************************

         name: lexicon_general-english.pl 
      version: 
  description: 
       author: Staffan Larsson
 
*************************************************************************/

%:- module( lexicon_travel_english, [output_form/2, input_form/2, yn_answer/1]).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

%:- ensure_loaded( library( semsort_travel ) ).

:- multifile output_form/2.
:- multifile input_form/2.


/*----------------------------------------------------------------------
     output_form( +Move, -WordList )
     -- Canned output
----------------------------------------------------------------------*/



% object-level clarification and groundnig questions
output_form( ask(C), Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', is that correct?'], Output ).


output_form( ask(set([Alt0|Alts])), Output):-
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
%	append(['Do you mean'|Alt0out], AltsOr, Output0 ),
	append(['Do you want to '|Alt0out], AltsOr, Output0 ),
	append(Output0, ['?'], Output).
altlist2alts_or( [Alt], ['or'|OutputAlt] ):-
	output_form(Alt, OutputAlt ).
altlist2alts_or( [Alt|Alts], [','|Output] ):-
	output_form(Alt, OutputAlt ),
	altlist2alts_or(Alts, AltsOr),
	append( OutputAlt, AltsOr, Output).

output_form( Alt, OutputAlt ):-
	input_form( OutputAlt, answer( Alt ) ).



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


output_form( answer(notexist(X,Q)), ['Sorry, there is nothing matching your request about '|InputQDot]):-
	input_form( InputQ, ask(X^Q) ),
	append( InputQ, ['.'], InputQDot ).
output_form( answer(unknown(Q)), ['Sorry, there is nothing matching your request about '|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% for asking metaissue clarification question
output_form( issue(Q), ['to ask about'|Out] ):-
	input_form( Out, ask( Q ) ).


% for asking metaissue clarification question
%output_form( action(Action), ['to '|Out] ):-
%	input_form( Out, request( Action ) ).

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

% reraise action explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:A, ['Returning to '|InputQDot]):-
	( input_form( InputQ, request(A) ); output_form( action(A), InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise issue (system-initiated, where question follows immediately after)
output_form( icm:reraise, ['So,']).

% accommodation
output_form( icm:accommodate:Q, ['Alright.']  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).

output_form( icm:accommodate:A, ['Alright.']  ):-
	input_form( AnsP, request( A ) ),
	append(AnsP,['.'],AnsPDot).

output_form( icm:reaccommodate:Q, ['Returning to the issue of'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).



output_form( not C, ['Not'|S] ):- output_form( C, S ).


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/


	

	

%input_form( S, answer(C) ):- lexsem( S, C ), sem_sort( C, country ).


% general negation 010419
input_form( [not|S], answer(not(C))):- input_form(S,answer(C)).



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








