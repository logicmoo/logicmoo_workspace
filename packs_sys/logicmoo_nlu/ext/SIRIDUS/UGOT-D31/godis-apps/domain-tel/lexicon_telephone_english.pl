:- module( lexicon_telephone_english, [output_form/2, input_form/2, yn_answer/1]).

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.



:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

:- ensure_loaded( library( semsort_telephone ) ).


%:- multifile( output_form/2 ).
%:- multifile( input_form/2 ).


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

% generic

input_form( [up],           request(up) ).
input_form( [language],        request(change_language) ).
input_form( [english], answer(language(english)) ).
input_form( [swedish], answer(language(svenska)) ).
input_form( [svenska], answer(language(svenska)) ).

% SL021125
input_form( [domain], request(change_domain) ).
input_form( [telephone], answer(domain(telephone)) ).
input_form( [vcr], answer(domain(vcr)) ).


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


% telephone

%answers to task question
input_form( [top], request(top) ).
input_form( [make,a,phone,call], request(tp_phoneCall) ).
input_form( [call], request(tp_phoneCall) ).
input_form( [phone,call], request(tp_phoneCall) ).
input_form( [transfer,calls], request(tp_divertCall) ).
input_form( [transfer], request(tp_divertCall) ).
input_form( [transfer,call], request(tp_divertCall) ).
input_form( [transfer,my,calls], request(tp_divertCall) ).
input_form( [cancel,transferred,calls], request(tp_cancelDivert) ).
input_form( [cancel,transfers], request(tp_cancelDivert) ).
input_form( [conference,call], request(tp_conferenceCall) ).


% SL021125
input_form( [domain], request(change_domain) ).
input_form( [telephone], answer(domain(telephone)) ).
input_form( [vcr], answer(domain(vcr)) ).




/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/
%output_form(icm:und*int:usr*_,['?']).
% ask-moves

%hack(?) gör om till uppmaning f a matcha den önskade dialogen 
output_form( ask(X^(action(X))), ['Please specify a function.'] ).

output_form( ask(action(T)), Str ):-
	output_form(action(T), StrT ),
	append( ['Do you want to '], StrT, Str0 ),
	append( Str0, ['?'], Str).

output_form( ask(X^destination(X)), ['Please specify the destination of the call.']).
output_form( ask(X^divert_to(X)), ['Please specify where you would like to transfer your calls.']).
%output_form( ask( X^person_in_office(X) ), [])
output_form( ask(X^first_person(X)), ['Please specify the first person of the conference call']).
output_form( ask(X^second_person(X)), ['Please specify the second person of the conference call']).
% action

output_form( action(change_domain), ['change domain'] ).
output_form( action(change_language), ['change language'] ).

output_form( action( tp_phoneCall ), ['make a phone call'] ).
output_form( action( tp_cancelDivert ), ['transfer your calls'] ).
output_form( action( tp_divertCall ), ['cancel transferred calls']).
output_form( action( tp_conferenceCall ), ['make a conference call'] ).

output_form( action(top), ['go to the automatic telephone operator']).

output_form( confirm( tp_phoneCall), ['Calling']).

output_form( confirm( tp_phoneCall(Dest)), Cs ):-
	output_form(Dest,DestForm),
	append( 'Calling', DestForm, Cs ).

output_form( confirm( tp_divertCall ), ['Transferring calls']).

output_form( confirm( tp_divertCall(Dest)), Cs ) :-
	output_form(Dest,DestForm),
	append( 'Transferring calls to', DestForm, Cs ).

output_form( confirm(tp_cancelDivert), ['Transfering calls cancelled'] ).

output_form( confirm(tp_conferenceCall),['Making a conference call']).

output_form( confirm(tp_conferenceCall(Dest1,Dest2)), Cs ):-
	output_form(Dest1,DestForm1),
	output_form(Dest2,DestForm2),
	append(DestForm1,['and'|DestForm2],Cs0),
	append('Making a conference call to',Cs0,Cs).


%så länge
output_form( answer(officeNumber(N)),['answer(officeNumber(N))']).

output_form( destination(N), [call|Str]  ):-
	input_form(N,Str0),
	append(Str0,'.',Str).

output_form( divert_to(N), [transfer,to|Str]):-
	input_form(N,Str0),
	append(Str0,'.',Str).


output_form( first_person(N), Str ):-
	input_form(N,Str0),
	append(Str0,'.',Str).

output_form( second_person(N), Str ):-
	input_form(N,Str0),
	append(Str0,'.',Str).


output_form( greet, ['This is your automatic telephone operator.'] ).
output_form( quit, ['Good bye!'] ).

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

				%questions

output_form( ask(X^domain(X)), ['What domain do you want?'] ).
output_form( ask([domain(_X)|_]), ['Do you want to use the VCR or the telephone?'] ). % HACK

%vet ej vad jag ska göra med detta
input_form( [what,office,number], ask(X^tp_officeNumber(X))).

input_form( Str, answer(name(N)) ):-
	name_form(Str,N).

%answers to in-plan questions
name_form( [luis],luis ).
name_form( [juan],juan ).

number_form( NumberStr,destination(No)):-
	lexsem(NumberStr,No),
	sem_sort(No,phone_number).
% numbers

input_form( S, answer( number( C ) ) ) :-
	lexsem( S, C ),
	sem_sort( C, number ).



/*----------------------------------------------------------------------
     output_form( +Move, -WordList )
     -- Canned output
----------------------------------------------------------------------*/



% object-level clarification and groundnig questions
output_form( ask(C), Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
%	append( IcmPos0, [', is that correct?'], Output ).
	append( IcmPos0, ['?'], Output ).



%SL021125
output_form( ask(set([Alt0|Alts])), Output):-
	Alt0=action(_),!,
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
%	append(['Do you mean'|Alt0out], AltsOr, Output0 ),
	append(['Do you want to '|Alt0out], AltsOr, Output0 ),
	append(Output0, ['?'], Output).
output_form( ask(set([Alt0|Alts])), Output):-
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
	append(['Do you want '|Alt0out], AltsOr, Output0 ),
%	append([''|Alt0out], AltsOr, Output0 ),
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


output_forms( [], [] ).
output_forms( [ Move | Moves ], Output1 ):-
	output_form( Move, Output ),
	output_forms( Moves, Outputs ),
	append( Output, Outputs, Output1 ).


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


output_form( icm:und*pos:usr*(not P), AnsNotPDot  ):-
	output_form( icm:und*pos:usr*P, AnsPDot  ),
	append( ['not'],AnsPDot,AnsNotPDot ).

output_form( icm:und*pos:usr*P, AnsPDot  ):-
	( output_form(P, AnsP);
	    input_form( AnsP, answer(P) ) ),
	append(AnsP,['.'],AnsPDot).

% special cases; could make use of isa-hierarchy
%output_form( icm:und*pos:usr*channel_to_store(X), IcmPos  ):-
%	output_form( icm:und*pos:usr*channel(X), IcmPos ).
%output_form( icm:und*pos:usr*new_channel(X), IcmPos  ):-
%	output_form( icm:und*pos:usr*channel(X), IcmPos ).

% 020702 SL
output_form( icm:und*pos:usr*PX, IcmPos ):-
	PX =.. [P,X],
	isa( P, P1 ),
	P1X =.. [P1,X],
	output_form( icm:und*pos:usr*P1X, IcmPos ).

output_form( icm:und*int:usr*C, IcmInt  ):-
	output_form( ask(C), IcmInt ).

output_form( icm:und*int:usr*C, Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', is that correct?'], Output ).

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
output_form( icm:acc*neg:action(change_language), ['Sorry, no habla any other language.']).
output_form( icm:acc*neg:P, ['Sorry, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' is not a valid parameter.'], Rest ).


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
output_form( icm:accommodate:_, ['Alright.']  ).

output_form( icm:reaccommodate:Q, ['Returning to the issue of'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).



output_form( not C, ['Not'|S] ):- output_form( C, S ).



	%
%output_form( _, ['DOH! There was a generation error.']).
%output_form( _, ['I dont know quite how to say this, but my lexicon is incomplete.']).
	



/*----------------------------------------------------------------------
     yn_answer( ?YN )
----------------------------------------------------------------------*/

yn_answer(A):-
	A = 'yes';
	A = 'no'.

/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

% use semantics as surface forms (only possible for english)
lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).


synset(dummy,dummy).






