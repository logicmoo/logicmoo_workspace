 
/*************************************************************************
 
         name: lexicon_agenda_english.pl 
      version: 
  description: A lexicon file which output is adapted to the generate
               module generate_agendatalk
	       input_form(String,Move)
	       output_form(Move,SharedCom,String)
       author: Rebecca Jonson 
 
*************************************************************************/

:- module(lexicon_agenda_english, [output_form/3, input_form/2,
				 yn_answer/1, synset/2, lexsem/2, weekDays/1,
			 resource_of_type/1]).

resource_of_type(lexicon).

%:- discontiguous output_form/3, input_form/2, plan/2, postcond/2.


:- use_module(library(random)).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:- multifile output_form/3, input_form/2.
:- ensure_loaded(generate_english).
:- ensure_loaded(interpret_english).

/*----------------------------------------------------------------------
     output_form( +Move, Com, -String )
     -- Canned output --Where Com corresponds to Shared Commitments
----------------------------------------------------------------------*/
output_form( ask(X^language(X)), _,['What language do you prefer?'] ).
output_form( ask([language(_)|_]),_, ['Do you want to use English or Swedish?'] ).

% object-level clarification and grounding questions
output_form( ask(C), Com, Output  ):-
	output_form( icm:und*pos:_*C, Com, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, ['?'], Output ).



output_form( ask(set([Alt0|Alts])), Com, Output):-
	output_form(Alt0, Com, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
%	append(['Do you mean'|Alt0out], AltsOr, Output0 ),
	append(['Do you want to '|Alt0out], AltsOr, Output0 ),
	append(Output0, ['?'], Output).
altlist2alts_or( [Alt], ['or'|OutputAlt] ):-
	output_form(Alt, _, OutputAlt ).
altlist2alts_or( [Alt|Alts], [','|Output] ):-
	output_form(Alt, _, OutputAlt ),
	altlist2alts_or(Alts, AltsOr),
	append( OutputAlt, AltsOr, Output).

output_form( Alt, _, OutputAlt ):-
	input_form( OutputAlt, answer( Alt ) ).



% db entries

output_form( answer( db_entry( _PropList, set(NewPropList), P ) ), Com, [''|Output ] ):-
	output_forms( NewPropList, NewPropListOut ),
	output_form( answer( P ), Com, AnswerOut ),
	append( AnswerOut, NewPropListOut, Output ).

%output_form( answer( db_entry( set( List )  ) ), Output ):-
%	output_forms( List, Output ).

output_forms( [], [] ).
output_forms( [ Move | Moves ], Output1 ):-
	output_form( Move, _, Output ),
	output_forms( Moves, Outputs ),
	append( Output, Outputs, Output1 ).
	
output_form( answer(notexist(X,Q)), _, ['Sorry, there is nothing matching your request about '|InputQDot]):-
	input_form( InputQ, ask(X^Q) ),
	append( InputQ, ['.'], InputQDot ).
output_form( answer(unknown(Q)), _, ['Sorry, there is nothing matching your request about '|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% for asking metaissue clarification question
output_form( issue(Q), _, ['to ask about'|Out] ):-
	input_form( Out, ask( Q ) ).


% ICM

% contact
output_form( icm:con*neg, _, ['Hello?'] ).


% perception
output_form( icm:per*int, _, ['Pardon?'] ).
output_form( icm:per*int, _, ['What did you say?'] ).
output_form( icm:per*neg, _, ['Sorry, I didnt hear what you said.'] ).

output_form( icm:per*pos:String, _, ['I heard you say',Name,'.'] ):-
	name( Name, String ).

output_form( icm:sem*int, _, ['What do you mean'] ).
output_form( icm:sem*neg, _, ['Sorry, I dont understand.'] ).
output_form( icm:sem*pos:Move, _, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).



% understanding(pragmatic)
output_form( icm:und*neg, _, ['I dont quite understand.']  ).

output_form( icm:und*pos:usr*issue(Q), _, ['You want to know about'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).
output_form( icm:und*pos:usr*(not issue(Q)), _, ['You did not ask about'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).


output_form( icm:und*pos:usr*(not P), Com, AnsNotPDot  ):-
	output_form( icm:und*pos:usr*P, Com, AnsPDot  ),
	append( ['not'],AnsPDot,AnsNotPDot ).

output_form( icm:und*pos:usr*P, Com, AnsPDot  ):-
	( output_form(P, Com, AnsP);
	    input_form( AnsP, answer(P) ) ),
	append(AnsP,['.'],AnsPDot).


% 020702 SL
output_form( icm:und*pos:usr*PX, Com, IcmPos ):-
	PX =.. [P,X],
	isa( P, P1 ),
	P1X =.. [P1,X],
	output_form( icm:und*pos:usr*P1X, Com, IcmPos ).

output_form( icm:und*int:usr*C, Com, IcmInt  ):-
	output_form( ask(C), Com, IcmInt ).

output_form( icm:und*int:usr*C, Com, Output  ):-
	output_form( icm:und*pos:_*C, Com, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', is that correct?'], Output ).
%%%Hack for AgendaTalk
output_form( icm:und*int:usr*take_down_event(X), Com, ['Sorry, did you say yes?']  ).


% clarification question
output_form( icm:und*int:usr*AltQ, Com, Output):-
	output_form( ask(AltQ), Com, Output).



% "acceptance"/integration

% icm-Type(-Polarity(-Args))
output_form( icm:acc*pos, _, ['Okay.'] ).


% reject(issue(Q))
output_form( icm:acc*neg:issue(Q), _, ['Sorry, I cannot answer questions about'|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% reject proposition P
/*output_form( icm:acc*neg:P, _, ['Sorry, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' is not a valid parameter.'], Rest ).
*/
% new reject prop
output_form( icm:acc*neg:date_to_store(P), _, ['Sorry, '|Rest]):-
	input_form( InputP, answer(date_to_store(P)) ),
	append( InputP, [' is not a valid date'], Rest ).

output_form( icm:acc*neg:P, _, ['Sorry, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' is not a valid parameter.'], Rest ).
% indicate loading a plan (pushed by findPlan)
%output_form( icm:loadplan, ['I need some information.'] ).

output_form( icm:loadplan, _, ['Lets see.'] ).


% reraise issue explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:Q, Com, ['Returning to the issue of '|InputQDot]):-
	( input_form( InputQ, ask(Q) ); output_form( ask(Q), Com, InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise action explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:A, Com, ['Returning to'|InputQDot]):-
	( input_form( InputQ, request(A) ); output_form( action(A), Com, InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise issue (system-initiated, where question follows immediately after)
output_form( icm:reraise, _, ['So,']).

% accommodation
output_form( icm:accommodate:_, _, ['Alright.']  ).

output_form( icm:reaccommodate:Q, _, ['Returning to the issue of'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).

output_form( not C, Com, ['Not'|S] ):- output_form( C, Com, S ).

altlist2altstr_and( [D], Str ):-
	alt2altstr( D, Str1 ),
	append( " and ", Str1, Str ).
altlist2altstr_and( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_and( Ds, Str2 ),
	append( Str1, ", ", Str3 ),
	append(Str3, Str2, Str ).

altlist2altstr_or( [D], Str ):-
	alt2altstr( D, Str1 ),
	append( " or ", Str1, Str ).
altlist2altstr_or( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_or( Ds, Str2 ),
	append( Str1, ", ", Str3 ),
	append(Str3, Str2, Str ).

alt2altstr( D, Str ):-
	output_form( D, Str ).

alt2altstr( D, Str ):-
	name( D, Str ).


%%%%%%%%%Extra predicates%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

atomList2Str([],[]).
atomList2Str([A|As],[S|Ss]):-
	name(A,S),
	atomList2Str(As,Ss).



capitalize(A,[CC|Cs]) :-
	atom_chars(A,[C|Cs]),
	cap_char(C,CC).

cap_char(A,B) :-
	A >= 0'a,
	A =< 0'z,
	!,
	B is A - 32.

cap_char(A,A).

/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/
%generic

input_form( [go,up],           request(up) ).
input_form( [go,back],      request(up)).
input_form( [up],           request(up) ).
input_form( [language],        request(change_language) ).
input_form( [english], answer(language(english)) ).
input_form( [swedish], answer(language(svenska)) ).

% general negation 010419
input_form( [not|S], answer(not(C))):- input_form(S,answer(C)).


input_form( [yes], answer(yes) ).
input_form( [yeah],answer(yes)).
input_form( [yes,please],answer(yes)).
input_form( [correct], answer(yes) ).
input_form( [no], answer(no) ).


% simple stuff

input_form( [hello], greet ).
input_form( [hi], greet ).
input_form( [bye], quit ).
input_form( [goodbye], quit).
input_form( [quit], quit ).

% ICM

input_form( [pardon], icm:per*neg ).
input_form( [okay], icm:acc*pos ).
input_form( [ok], icm:acc*pos ).
input_form( [dont, know], icm:acc*neg:issue ).



/*----------------------------------------------------------------------
     yn_answer( ?YN )
----------------------------------------------------------------------*/

yn_answer(A):-
	A = 'yes';
	A = 'no'.


%split(Atom, CharList)
split('',[]).
split(Atom, [C|Cs]):-
	atom_concat(C,Rest,Atom),
	atom_length(C,1),
	split(Rest, Cs).



