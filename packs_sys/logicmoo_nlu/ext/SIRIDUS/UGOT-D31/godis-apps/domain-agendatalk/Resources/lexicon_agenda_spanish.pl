 
/*************************************************************************
 
         name: lexicon_agendatalk_spanish.pl 
      version: 
  description: A lexicon file which output is adapted to the generate
               module generate_agendatalk
       author: Rebecca Jonson 
 
*************************************************************************/

:- module( lexicon_agenda_spanish, [output_form/3, input_form/2,
				 yn_answer/1, synset/2, lexsem/2, weekDays/1,
			 resource_of_type/1]). 

resource_of_type(lexicon).

%:- discontiguous output_form/3, input_form/2, plan/2, postcond/2.


:- use_module(library(random)).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:-multifile output_form/3, input_form/2.
:- ensure_loaded(generate_spanish).
:- ensure_loaded(interpret_spanish).

/*----------------------------------------------------------------------
     output_form( +Move, Com, -String )
     -- Canned output --Where Com corresponds to Shared Commitments
----------------------------------------------------------------------*/
output_form( ask(X^language(X)), _,['Qué idioma quiere usar?'] ).
output_form( ask([language(_)|_]),_, ['Quiere usar inglés, sueco o español?'] ).

% object-level clarification and grounding questions
output_form( ask(C), Com, Output  ):-
	output_form( icm:und*pos:_*C, Com, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, ['?'], Output ).



output_form( ask(set([Alt0|Alts])), Com, Output):-
	output_form(Alt0, Com, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
%	append(['Do you mean'|Alt0out], AltsOr, Output0 ),
	append(['Quieres'|Alt0out], AltsOr, Output0 ),
	append(Output0, ['?'], Output).
altlist2alts_or( [Alt], ['o'|OutputAlt] ):-
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
	
output_form( answer(notexist(X,Q)), _, ['Lo siento, nada coincide con su petición sobre '|InputQDot]):-
	input_form( InputQ, ask(X^Q) ),
	append( InputQ, ['.'], InputQDot ).
output_form( answer(unknown(Q)), _, ['Lo siento, nada coincide con su petición sobre '|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% for asking metaissue clarification question
output_form( issue(Q), _, ['para preguntar sobre'|Out] ):-
	input_form( Out, ask( Q ) ).


% ICM

% contact
output_form( icm:con*neg, _, ['Hola?'] ).

% perception
output_form( icm:per*int, _, ['Cómo dice?'] ).
output_form( icm:per*int, _, ['Qué ha dicho?'] ).
output_form( icm:per*neg, _, ['Perdone, no le he oído bien.'] ).

output_form( icm:per*pos:String, _, ['Ha dicho',Name,'.'] ):-
	name( Name, String ).

output_form( icm:sem*int, _, ['Qué quiere decir'] ).
output_form( icm:sem*neg, _, ['Lo siento, no le entiendo.'] ).
output_form( icm:sem*pos:Move, _, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).



% understanding(pragmatic)
output_form( icm:und*neg, _, ['No le entiendo de todo.']  ).

output_form( icm:und*pos:usr*issue(Q), _, ['Quiere saber algo sobre'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).
output_form( icm:und*pos:usr*(not issue(Q)), _, ['No ha preguntado sobre'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).


output_form( icm:und*pos:usr*(not P), Com, AnsNotPDot  ):-
	output_form( icm:und*pos:usr*P, Com, AnsPDot  ),
	append( ['no'],AnsPDot,AnsNotPDot ).

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
	append( IcmPos0, [', es correcto?'], Output ).
%%%Hack for AgendaTalk
output_form( icm:und*int:usr*take_down_event(X), Com, ['Perdona, ha dicho que sí?']  ).


% clarification question
output_form( icm:und*int:usr*AltQ, Com, Output):-
	output_form( ask(AltQ), Com, Output).



% "acceptance"/integration

% icm-Type(-Polarity(-Args))
output_form( icm:acc*pos, _, ['Vale.'] ).

% reject(issue(Q))
output_form( icm:acc*neg:issue(Q), _, ['Lo siento, no sé contestar a preguntas sobre'|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% reject proposition P
/*output_form( icm:acc*neg:P, _, ['Sorry, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' is not a valid parameter.'], Rest ).
*/
% new reject prop
output_form( icm:acc*neg:date_to_store(P), _, ['Lo siento, '|Rest]):-
	input_form( InputP, answer(date_to_store(P)) ),
	append( InputP, [' no es un fecha válida.'], Rest ).

output_form( icm:acc*neg:P, _, ['Sorry, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' no es un parámetro válido.'], Rest ).
% indicate loading a plan (pushed by findPlan)
%output_form( icm:loadplan, ['I need some information.'] ).

output_form( icm:loadplan, _, ['A ver.'] ).


% reraise issue explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:Q, Com, ['Volviendo al tema de '|InputQDot]):-
	( input_form( InputQ, ask(Q) ); output_form( ask(Q), Com, InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise action explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:A, Com, ['Volviendo a'|InputQDot]):-
	( input_form( InputQ, request(A) ); output_form( action(A), Com, InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise issue (system-initiated, where question follows immediately after)
output_form( icm:reraise, _, ['Así,']).

% accommodation
output_form( icm:accommodate:_, _, ['De acuerdo.']  ).

output_form( icm:reaccommodate:Q, _, ['Volviendo al tema de'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).

output_form( not C, Com, ['No'|S] ):- output_form( C, Com, S ).

altlist2altstr_and( [D], Str ):-
	alt2altstr( D, Str1 ),
	append( " y ", Str1, Str ).
altlist2altstr_and( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_and( Ds, Str2 ),
	append( Str1, ", ", Str3 ),
	append(Str3, Str2, Str ).

altlist2altstr_or( [D], Str ):-
	alt2altstr( D, Str1 ),
	append( " o ", Str1, Str ).
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

input_form( [volver],           request(up) ).
input_form( [subir],      request(up)).
input_form( [idioma],        request(change_language) ).
input_form( [inglés], answer(language(english)) ).
input_form( [sueco], answer(language(svenska)) ).

% general negation 010419
input_form( [no|S], answer(not(C))):- input_form(S,answer(C)).


input_form( [si], answer(yes) ).
input_form( [si,por,favor],answer(yes)).
input_form( [no], answer(no) ).


% simple stuff

input_form( [hola], greet ).
input_form( [adiós], quit ).
input_form( [hasta,luego], quit).
input_form( [salir], quit ).

% ICM

input_form( [cómo], icm:per*neg ).
input_form( [vale], icm:acc*pos ).
input_form( [no,lo,sé], icm:acc*neg:issue ).



/*----------------------------------------------------------------------
     yn_answer( ?YN )
----------------------------------------------------------------------*/

yn_answer(A):-
	A = 'si';
	A = 'no'.


%split(Atom, CharList)
split('',[]).
split(Atom, [C|Cs]):-
	atom_concat(C,Rest,Atom),
	atom_length(C,1),
	split(Rest, Cs).



