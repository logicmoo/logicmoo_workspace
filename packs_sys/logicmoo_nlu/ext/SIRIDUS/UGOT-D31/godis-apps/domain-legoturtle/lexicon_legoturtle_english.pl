:- module( lexicon_legoturtle_english, [output_form/2, input_form/2,
				 yn_answer/1]).

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

:- ensure_loaded( library(lexicon_numbers) ).
:- ensure_loaded( library(digits_english) ).

:- ensure_loaded( library( semsort_legoturtle ) ).


%:- multifile( output_form/2 ).
%:- multifile( input_form/2 ).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/

% ask-moves

output_form( ask(X^(action(X) ) ), ['What shall I do?'] ).
output_form( ask(action(T) ), Str) :-
	output_form( action(T), StrT ), 
	append( ['Do you want me to'], StrT, Str0 ),
	append( Str0, ['?'], Str).
output_form( ask(X^steps(X) ), ['How far?'] ).
output_form( ask(X^degrees(X) ), ['Hur many degrees?'] ).
output_form( ask(X^color(X) ), ['Do you want black or black?'] ).

% action

output_form( action(top), ['Ready'] ).
output_form( action(pen_up), ['take the pen up'] ).
output_form( action(pen_down), ['put the pen down'] ).
output_form( action(background), ['byta bakgrundsfärg'] ).
output_form( action(pencolor), ['byta färg på pennan'] ).
output_form( action(move), ['move around'] ).
output_form( action(forward), ['go forward'] ).
output_form( action(backward), ['go backward'] ).
output_form( action(turn), ['turn'] ).
output_form( action(right), ['turn right'] ).
output_form( action(left), ['turn left'] ).
output_form( action(clear), ['erase everything'] ).
output_form( action(circle), ['draw a circle'] ).
output_form( action(tree), ['rita ett träd'] ).
	   
%

output_form( icm:neg:action(forward), 
 	     ['I cant go that far.'] ).
output_form( icm:neg:action(backward), 
 	     ['I cant go that far.'] ).

output_form( icm:neg:action(right), ['Sorry, I cant turn more than 360 degrees.'] ).
output_form( icm:neg:action(left), ['Ledsen, jag kan inte vända mig mer än  360 grader.'] ).

output_form( confirm(_), ['Done!']).
output_form( confirm(pen_up), ['Nu kan jag gå utan att rita.'] ).
output_form( confirm(pen_down), ['Nu kan jag börja rita.'] ).
output_form( confirm(move), ['Nu kan jag flytta på mig.'] ).
output_form( confirm(forward), ['I have moved forward.'] ).
output_form( confirm(backward), ['I have moved backward.'] ).
output_form( confirm(turn), ['Nu kan jag vända mig.'] ).
output_form( confirm(left), ['I have turned left.'] ).
output_form( confirm(right), ['I have turned right.'] ).
output_form( confirm(pencolor), ['Nu har jag bytt färg på pennan.'] ).
output_form( confirm(background), ['Nu har jag bytt bakgrundsfärg.'] ).
output_form( confirm(clear), ['Nu har jag suddat ut allt.'] ).
output_form( confirm(circle), ['Nu har jag ritat en cirkel.'] ).
output_form( confirm(tree), ['Nu har jag ritat ett träd.'] ).

% Hälsning
output_form( greet, ['Hi there!'] ).
output_form( quit, ['Bye!'] ).

%Hjälpmeny
output_form( help, ['Vill du att jag skall rita, vända mig åt höger eller vänster, rita en cirkel, ett träd eller bara gå utan att rita?'] ).

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
% request sequences: if requests (followed by 0-2 answers) are separated by
%"and", reverse list so that first request is put last on stack; this is done by%putting last in the list
% SL 030508

input_form( ReqForms, RevList ) :-
        RevList = [_|_],
        append( ReqForm1, [och | ReqForm2 ], ReqForms ),
        ( AnsList1 = [] ; AnsList1 = [ answer(_) ]; AnsList1 = [answer(_),answer(_)] ),
        input_form( ReqForm1, [ request(A1) | AnsList1 ] ),
        ( AnsList2 = [] ; AnsList2 = [ answer(_) ]; AnsList2 = [answer(_),answer(_)] ),
        input_form( ReqForm2, [ request(A2) | AnsList2 ] ),
        append( [ request(A2) | AnsList2 ],[ request(A1) | AnsList1 ], RevList).

% input form for request followed by sequence of answers
input_form( ReqAnsForm, [ request(A) | AnsList ] ):-
        append( ReqForm, AnsForm, ReqAnsForm),
        input_form( ReqForm, request( A ) ),
        input_form_answers( AnsForm, AnsList ).

% input form for sequence of answers
input_form_answers( [], [] ).
input_form_answers( AnsForms, [answer(Ans)|AnsList] ):-
        append( AnsForm, AnsForms1, AnsForms  ),
        input_form( AnsForm, answer( Ans ) ),
        input_form_answers( AnsForms1, AnsList ).

% generic

input_form( [go,up],           request(up) ).
input_form( [up],           request(up) ).

% speak
input_form( [top], request(top) ).
input_form( [start], request(top) ).
%input_form( [start], request(top) ).
%input_form( [start], request(top) ).
%input_form( [starta], request(top) ).
%input_form( [börja], request(top) ).
%input_form( [början], request(top) ).

input_form( [gå, utan, att, rita], request(pen_up)).
input_form( [utan], request(pen_up) ).
input_form( [ta, upp, pennan], request(pen_up) ).
input_form( [släpp, pennan], request(pen_up) ).
input_form( [sluta, rita], request(pen_up) ).
input_form( [inte, rita], request(pen_up) ).
input_form( [rita, inte], request(pen_up) ).
input_form( [sluta, måla], request(pen_up) ).
input_form( [inte, måla], request(pen_up) ).
input_form( [måla, inte], request(pen_up) ).

input_form( [rita], request(pen_down) ).
input_form( [sätt, ner, pennan], request(pen_down) ).
input_form( [måla], request(pen_down) ).

input_form( [cirkel], request(circle) ).
input_form( [rita, en, cirkel], request(circle) ).
input_form( [ring], request(circle) ).
input_form( [rita, en, ring], request(circle) ).
input_form( [boll], request(circle) ).
input_form( [rita, en, boll], request(circle) ).


input_form( [träd], request(tree) ).
input_form( [ett, träd], request(tree) ).
input_form( [rita, ett, träd], request(tree) ).

input_form( [bakgrundsfärg], request(background) ).
input_form( [bakgrundsfärgen], request(background) ).
input_form( [bakgrund], request(background) ).
input_form( [bakgrunden], request(background) ).
input_form( [skärmen], request(background) ).
input_form( [skärm], request(background) ).

input_form( [pennan], request(pencolor) ).
input_form( [penna], request(pencolor) ).
input_form( [pennfärg], request(pencolor) ).
input_form( [krita], request(pencolor) ).
input_form( [kritan], request(pencolor) ).
input_form( [kritfärg], request(pencolor) ).

input_form( [move], request(move) ).
input_form( [go], request(move) ).
%input_form( [går], request(move) ).

input_form( [line], request(move) ).
input_form( [ett, rakt, streck], request(move) ).
input_form( [rita, ett, streck], request(move) ).
input_form( [linje], request(move) ).
input_form( [rita, en, linje], request(move) ).

input_form( [forward], request(forward) ).
input_form( [ahead], request(forward) ).

input_form( [backward], request(backward) ).
input_form( [back], request(backward) ).

input_form( [turn], request(turn) ).
input_form( [turning], request(turn) ).
%input_form( [svänga], request(turn) ).
%input_form( [sväng], request(turn) ).

input_form( [left], request(left) ).
input_form( [go, left], request(left) ).
input_form( [right], request(right) ).
input_form( [go,right], request(right) ).

%input_form( [vanster], request(left) ).
%input_form( [hoger], request(right) ).

input_form( [sudda], request(clear) ).
input_form( [rensa], request(clear) ).
input_form( [ta, bort], request(clear) ).
input_form( [blanka], request(clear) ).




input_form( SSteg, answer( steps( C ) ) ) :-
	append( S, [steps], SSteg ),
	lexsem( S, C ),
	sem_sort( C, steps ),
	sem_sort( C, number ).

input_form( SGrader, answer( degrees( C ) ) ) :-
	append( S, [degrees], SGrader),
	lexsem( S, C ),
	sem_sort( C, degrees ),
	sem_sort( C, number ).


% numbers

input_form( S, answer( number( C ) ) ) :-
	lexsem( S, C ),
	sem_sort( C, number ).

input_form( S, answer( colors( C ) ) ) :-
	lexsem( S, C ),
	sem_sort( C, colors ).

/*----------------------------------------------------------------------
     output_form( +Move, -WordList )
     -- Canned output
----------------------------------------------------------------------*/



% object-level clarification and groundnig questions
output_form( ask(C), Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', correct?'], Output ).



output_form( ask(set([Alt0|Alts])), Output):-
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
	append(['Do you want me to'|Alt0out], AltsOr, Output0 ),
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
	
output_form( answer(notexist(X,Q)), [' Ledsen, jag vet inte vad jag ska svara på frågan: '|InputQDot]):-
	input_form( InputQ, ask(X^Q) ),
	append( InputQ, ['.'], InputQDot ).
output_form( answer(unknown(Q)), ['Ledsen, jag vet inte vad jag ska svara på frågan: '|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% for asking metaissue clarification question
output_form( issue(Q), ['fråga om'|Out] ):-
	input_form( Out, ask( Q ) ).



% ICM

% contact
output_form( icm:con*neg, ['Hello?'] ).


% perception
%output_form( icm:per*int, ['Pardon?'] ).
%output_form( icm:per*int, ['What did you say?'] ).
output_form( icm:per*neg, ['Pardon?'] ).


output_form( icm:per*pos:String, ['You said ',Name,'.'] ):-
	name( Name, String ).

output_form( icm:sem*int, ['What do you mean'] ).
output_form( icm:sem*neg, ['Sorry, I dont understand.'] ).
output_form( icm:sem*pos:Move, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).


% understanding(pragmatic)
output_form( icm:und*neg, ['I dont quite understand.']  ).


output_form( icm:und*pos:usr*(not issue(Q)), ['You did not ask: '|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).


output_form( icm:und*pos:usr*(not P), AnsNotPDot  ):-
	output_form( icm:und*pos:usr*P, AnsPDot  ),
	append( ['not'],AnsPDot,AnsNotPDot ).

output_form( icm:und*pos:usr*P, AnsPDot  ):-
	( output_form(P, AnsP);
	    input_form( AnsP, answer(P) ) ),
	append(AnsP,['.'],AnsPDot).


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
output_form( icm:acc*neg:issue(Q), ['Ledsen, jag kan inte svara på frågor om: '|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% reject proposition P
output_form( icm:acc*neg:P, ['Ledsen, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' kan man inte skriva.'], Rest ).

% indicate loading a plan (pushed by findPlan)
output_form( icm:loadplan, [] ).


% reraise issue explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:Q, ['Returning to the issue of '|InputQDot]):-
	( input_form( InputQ, ask(Q) ); output_form( ask(Q), InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise action explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:A, [''|InputQDot]):-
	( input_form( InputQ, request(A) ); output_form( action(A), InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise issue (system-initiated, where question follows immediately after)
output_form( icm:reraise, ['So,']).

% accommodation
output_form( icm:accommodate:_, ['Sure.']  ).

output_form( icm:reaccommodate:Q, ['Returning to the issue of'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).



output_form( not C, ['not'|S] ):- output_form( C, S ).


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/


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


/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

% use semantics as surface forms (only possible for english)
lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).


synset( [NumberPhrase], Number ):- number_phrase( NumberPhrase, Number ).

synset([[röd], [rött]], red).
synset([[mörkröd], [mörkrött]], darkred).
synset([[vit], [vitt]], white).
synset([[svart]], black).
synset([[gul], [gult]], yellow).
synset([[ljusgul], [ljusgult]], lightyellow).
synset([[orange], [oranget]], orange).
synset([[mörkorange], [mörkoranget]], darkorange).
synset([[blå], [blått]], blue).
synset([[mörkblå], [mörkblått]], darkblue).
synset([[ljusblå], [ljusblått]], lightblue).
synset([[lila]], purple).
synset([[mörklila]], darkpurple).
synset([[grön], [grönt]], green).
synset([[mörkgrön], [mörkgrönt]], darkgreen).
synset([[ljusgrön], [ljusgrönt]], lightgreen).
synset([[brun], [brunt]], brown).
synset([[rosa]], pink).
synset([[skär], [skärt]], pink).
