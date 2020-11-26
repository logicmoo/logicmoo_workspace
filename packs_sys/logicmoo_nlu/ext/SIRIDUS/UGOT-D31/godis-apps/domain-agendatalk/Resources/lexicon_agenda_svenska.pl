/***********************************************************************

 
         name: lexicon_agenda_svenska.pl
  description: General Lexicon module for Swedish
               generation and interpretation "grammars"
               input_form(String,Move)
	       output_form(Move,SharedCom,String)
       author: written by Staffan Larsson modified by Rebecca Jonson

***************************************************************************/


:-module(lexicon_agenda_svenska, [output_form/3, input_form/2, sametype/2,
			 resource_of_type/1]).

resource_of_type(lexicon).

%:-discontiguous output_form/3, input_form/2.
:- use_module(library(random)).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:- multifile output_form/3, input_form/2.
:- ensure_loaded(generate_svenska).
:- ensure_loaded(semsort_agendatalk).


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/
% generic

input_form( [gå,upp],           request(up) ).
input_form( [gå,tillbaka],           request(up) ).
input_form( [gå,tillbaks],           request(up) ).
input_form( [upp],           request(up) ).
input_form( [språk],        request(change_language) ).
input_form( [engelska], answer(language(english)) ).
input_form( [svenska], answer(language(svenska)) ).
input_form( [inglish], answer(language(english)) ).

% contradiction 050803 RJ
input_form( [nej,inte,NotS,S],[request(not(NotC)),request(C)]):-
	input_form([NotS],request(NotC)),input_form([S],request(C)).
input_form( [nej,inte,NotS,utan,S],[answer(not(NotC)),answer(C)]):-
	input_form([NotS],answer(NotC)),
	input_form([S],answer(C)),
	sametype(NotC,C).
input_form( [nej,inte,NotS,S],[answer(not(NotC)),answer(C)]):-
	input_form([NotS],answer(NotC)),
	input_form([S],answer(C)),
	sametype(NotC,C).


input_form( [inte,NotS,S],[request(not(NotC)),request(C)]):-
	input_form([NotS],request(NotC)),input_form([S],request(C)).
input_form( [inte,NotS,utan,S],[answer(not(NotC)),answer(C)]):-
	input_form([NotS],answer(NotC)),
	input_form([S],answer(C)),
	sametype(NotC,C).
input_form( [inte,NotS,S],[answer(not(NotC)),answer(C)]):-
	input_form([NotS],answer(NotC)),
	input_form([S],answer(C)),
	sametype(NotC,C).

input_form( [S,inte,NotS],[answer(not(NotC)),answer(C)]):-
	input_form([NotS],answer(NotC)),
	input_form([S],answer(C)),
	sametype(NotC,C).

% general negation 010419 does not work for "inte möte" just "inte ett möte"
input_form( [nej,inte|S], answer(not(C))):- input_form(S,answer(C)).
input_form( [inte|S], answer(not(C))):- input_form(S,answer(C)).


input_form( [ja,precis],answer(yes)).
input_form( [ja], answer(yes) ).
input_form( [jaa], answer(yes) ).
input_form( [jajamen],answer(yes)).
input_form( [javisst],answer(yes)).
input_form( [japp],answer(yes)).
input_form( [svar,ja],answer(yes)).
input_form( [exakt],answer(yes)).
input_form( [korrekt],answer(yes)).
input_form( [det,stämmer],answer(yes)).
input_form( [nej,det,stämmer,inte], answer(no) ).
input_form( [nej,det,är,fel], answer(no) ).
input_form( [nej], answer(no) ).
input_form( [svar,nej], answer(no) ).
input_form( [nä], answer(no)).

% simple stuff

input_form( [hej], greet ).
input_form( [hejsan],greet).
input_form( [tjenare],greet).
input_form( [hej,då], quit ).
input_form( [hejdå], quit).
input_form( [avsluta],quit).
input_form( [klart,slut],quit).
input_form( [sluta], quit ).
input_form( [adjö], quit ).

% ICM
input_form( [förlåt], icm:per*neg ).
input_form( [jag,hörde,inte,vad,du,sa], icm:per*neg ).
input_form( [jag,hörde], icm:per*neg ).
input_form( [vad,sa,du], icm:per*neg ).
input_form( [flåt], icm:per*neg ).
input_form( [okej], icm:acc*pos ).
input_form( [ok], icm:acc*pos ).
input_form( [vet, inte], icm:acc*neg:issue ).

/*----------------------------------------------------------------------
     yn_answer( ?YN )
----------------------------------------------------------------------*/

yn_answer(A):-
	A = 'ja';
	A = 'nej'.

%%%%LOAD DOMAIN-SPECIFIC INTERPRETATION LEXICON
:- ensure_loaded(interpret_svenska).

/*----------------------------------------------------------------------
     output_form( +Move, -WordList )
     -- Canned output SL
----------------------------------------------------------------------*/

output_form( ask(X^language(X)), _,['Vilket språk vill du använda?'] ).
output_form( ask([language(_)|_]),_, ['Vill du använda engelska eller svenska?'] ).

% object-level clarification and groundnig questions
output_form( ask(C),Com,Output  ):-
	output_form( icm:und*pos:_*C,Com,IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', är det korrekt?'], Output ).

output_form( ask(set([Alt0|Alts])), Com,Output):-
	output_form(Alt0, Com,Alt0out),
	altlist2alts_or( Alts, AltsOr ),
%	append(['Do you mean'|Alt0out], AltsOr, Output0 ),
	append(['Vill du '|Alt0out], AltsOr, Output0 ),
	append(Output0, ['?'], Output).
altlist2alts_or( [Alt], ['eller'|OutputAlt] ):-
	output_form(Alt, _,OutputAlt ).
altlist2alts_or( [Alt|Alts], [','|Output] ):-
	output_form(Alt, _,OutputAlt ),
	altlist2alts_or(Alts, AltsOr),
	append( OutputAlt, AltsOr, Output).

output_form( Alt, _,OutputAlt ):-
	input_form( OutputAlt, answer( Alt ) ).

% db entries

output_form( answer( db_entry( _PropList, set(NewPropList), P ) ),Com,[''|Output ] ):-
	output_forms( NewPropList, NewPropListOut ),
	output_form( answer( P ), Com,AnswerOut ),
	append( AnswerOut, NewPropListOut, Output ).



output_forms( [],[] ).
output_forms( [ Move | Moves ], Output1 ):-
	output_form( Move, _,Output ),
	output_forms( Moves, Outputs ),
	append( Output, Outputs, Output1 ).
	

output_form( answer(notexist(X,Q)), _,[' Ledsen, det finns inget som matchar din fråga om'|InputQDot]):-
	input_form( InputQ, ask(X^Q) ),
	append( InputQ, ['.'], InputQDot ).
output_form( answer(unknown(Q)), _,['Ledsen, det finns inget som matchar din fråga om'|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% for asking metaissue clarification question
output_form( issue(Q), _,['fråga om'|Out] ):-
	input_form( Out, ask( Q ) ).

% ICM

% contact
output_form( icm:con*neg, _,['Hallå?'] ).


% perception
output_form( icm:per*int, _,['Ursäkta?'] ).
output_form( icm:per*int, _,['Vad sa du?'] ).
%output_form( icm:per*neg, _,['Ursäkta, Jag hörde inte vad du sa.'] ).
output_form(icm:per*neg, _, [Answer]):- random(1,6,N),negperception(List),getNoflist(N,List,Answer).
/* RANDOM VARIATION */
negperception(['Ursäkta, Jag hörde inte vad du sa.','Ursäkta, Jag uppfattade inte vad du sa.', 'Förlåt, jag hörde inte.','Förlåt, kanske talade du samtidigt som mig.', 'Förlåt, vad sa du?']).

output_form( icm:per*pos:String, _,['Jag tyckte du sa',Name,'.'] ):-
	name( Name, String ).
output_form( icm:sem*int, _,['Vad menar du'] ).
output_form( icm:sem*neg, _,['Förlåt, jag förstår inte.'] ).
output_form( icm:sem*pos:Move, _,InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).
% understanding(pragmatic)
output_form( icm:und*neg, _,['Jag förstår inte riktigt.']  ).

output_form( icm:und*pos:usr*issue(Q), _, ['Du vill ha reda på något om'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).
output_form( icm:und*pos:usr*(not issue(Q)), _,['Du frågade inte:'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).

output_form( icm:und*pos:usr*(not P), _,AnsNotPDot  ):-
	output_form( icm:und*pos:usr*P, _,AnsPDot  ),
	append( ['inte'],AnsPDot,AnsNotPDot ).

output_form( icm:und*pos:usr*P, _,AnsPDot  ):-
	( output_form(P, _,AnsP);
	    input_form( AnsP, answer(P) ) ),
	append(AnsP,['.'],AnsPDot).

%
% 020702 SL
output_form( icm:und*pos:usr*PX, _,IcmPos ):-
	PX =.. [P,X],
	isa( P, P1 ),
	P1X =.. [P1,X],
	output_form( icm:und*pos:usr*P1X, _,IcmPos ).

output_form( icm:und*int:usr*C, _,IcmInt  ):-
	output_form( ask(C), _,IcmInt ).

output_form( icm:und*int:usr*C, _,Output  ):-
	output_form( icm:und*pos:_*C, _,IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', är det korrekt?'], Output ).
%%%Hack for AgendaTalk
output_form( icm:und*int:usr*take_down_event(X), Com, ['Var det ett ja?']  ).
% clarification question
output_form( icm:und*int:usr*AltQ, _,Output):-
	output_form( ask(AltQ), _,Output).

% "acceptance"/integration

% icm-Type(-Polarity(-Args))
output_form( icm:acc*pos, _,['Ockej.'] ).

% reject(issue(Q))
output_form( icm:acc*neg:issue(Q), _,['Ledsen, jag kan inte svara på frågor om'|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% reject proposition P
/*output_form( icm:acc*neg:P,_, ['Ledsen, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' är inte en korrekt parameter.'], Rest ).*/
output_form( icm:acc*neg:date_to_store(P), _, ['Ledsen, '|Rest]):-
	input_form( InputP, answer(date_to_store(P)) ),
	append( InputP, [' är inte ett korrekt datum'], Rest ).

output_form( icm:acc*neg:P, _, ['Sorry, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' är inte en korrekt parameter.'], Rest ).
% indicate loading a plan (pushed by findPlan)

output_form( icm:loadplan, _,['Låt se.'] ).

% reraise issue explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:Q, _,['Tillbaka till frågan om '|InputQDot]):-
	( input_form( InputQ, ask(Q) ); output_form( ask(Q), _,InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise action explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:A, _,['Tillbaka till '|InputQDot]):-
	( input_form( InputQ, request(A) ); output_form( action(A), _,InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise issue (system-initiated, where question follows immediately after)
output_form( icm:reraise, _,['Så,']).

% accommodation
output_form( icm:accommodate:_, _,['Visst.']  ).

output_form( icm:reaccommodate:Q, _,['Tillbaka till frågan om'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).

output_form( not C, _,['Inte'|S] ):- output_form( C, _,S ).

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
	output_form( D, _,Str ).

alt2altstr( D, Str ):-
	name( D, Str ).



%split(Atom, CharList)
split('',[]).
split(Atom, [C|Cs]):-
	atom_concat(C,Rest,Atom),
	atom_length(C,1),
	split(Rest, Cs).

sametype(event_to_store(_X),event_to_store(_Y)).
sametype(time(_X),time(_Y)).
sametype(am_or_pm(_X),am_or_pm(_Y)).
sametype(date(_X),date(_Y)).
sametype(location(_X),location(_Y)).
