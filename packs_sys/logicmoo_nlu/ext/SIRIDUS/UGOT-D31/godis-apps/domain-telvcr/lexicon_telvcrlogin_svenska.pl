 
/*************************************************************************
 
         name: lexicon_vcr_svenska.pl 
      version: 
  description: 
       author: Staffan Larsson
 
*************************************************************************/

:- module( lexicon_telvcrlogin_svenska, [output_form/2, input_form/2, yn_answer/1]).

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

:- ensure_loaded( semsort_telvcrlogin ).


%:- multifile( output_form/2 ).
%:- multifile( input_form/2 ).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/

% ask-moves

output_form( ask(X^(action(X))), ['Vad önskas?'] ).

output_form( ask(action(T)), Str ):-
	output_form(action(T), StrT ),
	append( ['Vill du '], StrT, Str0 ),
	append( Str0, ['?'], Str).




output_form( ask(X^user_name(X)),
	     ['Ange användarnamn!'] ).
output_form( ask(X^password(X)),
	     ['Ange lösenord!'] ).



% report

output_form( report(login,validUser(Usr,_,yes)), ['Du är inloggad som '|UsrForm]):-
	output_form(answer(user_name(Usr)),UsrForm).
	  
output_form( report(login,validUser(Usr,Psswd,no)), ['Användaren '|Rest]):-
	var(Psswd),
	append(UsrForm, [' finns inte!'], Rest),
	output_form(answer(user_name(Usr)),UsrForm).

output_form( report(login,validUser(_,Psswd,no)), ['Lösenordet är felaktigt!']):-
	atom(Psswd).




%ouput_form( report(validUser(X,Y,Res), ['']).


% action

output_form( action(login), ['logga in']).


output_form( action(top), ['Login'] ).


output_form( confirm(validUser(U,P,_Res)), Str ) :-
	output_form(user_name(U), Ustr),
	output_form( password(P), Pstr),
	append(['Kontrollerar användarnamn'],Ustr,Str0),
	append(['mot lösenord'],Pstr,Str1),
	append(Str0,Str1,Str).

		
output_form( answer(user_name(U)), Ustr):-
	split(U,Ustr).

output_form( answer(password(P)), PStr):-
	   split(P,PStr).

output_form( answer(alphadigit(A)),S):-
	split(A,S).


		
output_form(user_name(U), Ustr):-
	split(U,Ustr).

output_form(password(P), PStr):-
	   split(P,PStr).

output_form(alphadigit(A),S):-
	split(A,S).

output_form( answer(invalid_password), ['Felaktigt lösenord!!']).



output_form( answer(usage), ['Du måste logga in för att komma till videon.']).


output_form( report( vcr_login, validUser(Usr,_Passw,yes)), ['Välkommen'|UsrForm] ):-
	output_form(answer(user_name(Usr)),UsrForm).
output_form( report( vcr_login, validUser(_Usr,_Passw,no)), ['Inloggningen misslyckades.'] ).


output_form( greet, ['Välkommen till videoentrén !'] ).
output_form( quit, ['Hej då!'] ).


altlist2altstr_and( [D], Str ):-
	alt2altstr( D, Str1 ),
	append( " och ", Str1, Str ).
altlist2altstr_and( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_and( Ds, Str2 ),
	append( Str1, ", ", Str3 ),
	append(Str3, Str2, Str ).

altlist2altstr_or( [D], Str ):-
	alt2altstr( D, Str1 ),
	append( " eller ", Str1, Str ).
altlist2altstr_or( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_or( Ds, Str2 ),
	append( Str1, ", ", Str3 ),
	append(Str3, Str2, Str ).

alt2altstr( D, Str ):-
	output_form( D, Str ).

alt2altstr( D, Str ):-
	name( D, Str ).



atomList2Str([],[]).
atomList2Str([A|As],[S|Ss]):-
	name(A,S),
	atomList2Str(As,Ss).


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

% VCR



input_form( [toppnivå], request(top) ).

input_form( [börja,om], request(top) ).
input_form( [logga,in], request(login)).
input_form( [inloggning], request(login)).

input_form( UserName, answer(alphadigit(U))):-
	alphadigits_form(UserName,Ulist),
	join(Ulist,'',U),
	sem_sort(U,alphadigit).

input_form( Password, answer(alphadigits(P))):-
	alphadigits_form(Password,Plist),
	join(Plist,'',P),
	sem_sort(P,alphadigit).


alphadigits_form([AD],[C]):-
	alphadigit_form(AD,C).
	

alphadigits_form([AD1,AD2|ADs],[C|Cs]):-
	alphadigit_form(AD1,C),
	alphadigits_form([AD2|ADs],Cs).


alphadigit_form(A,B):-
	digit_form([A],B).

alphadigit_form(A,B):-
	alpha_form([A],B).

alphadigit_form(A,B):-
	alpha_spell_form([A],B).



digit_form([noll], '0').
digit_form([ett], '1').
digit_form([två], '2').
digit_form([tre], '3').
digit_form([fyra], '4').
digit_form([fem], '5').
digit_form([sex], '6').
digit_form([sju], '7').
digit_form([åtta], '8').
digit_form([nio], '9').

alpha_form([A],A):-
	atom(A),
	atom_length(A,1),
	%capitalize(A,B),
	member(A,['Q','W','E','R','T','Y','U','I','O','P',
		  'A','S','D','F','G','H','J','K','L','Z',
		  'X','C','V','B','N','M','q','w','e','r',
		  't','y','u','i','o','p','a','s','d','f',
		  'g','h','j','k','l','z','x','c','v','b',
		  'n','m']).

alpha_spell_form([adam],a).
alpha_spell_form([bertil],b).
alpha_spell_form([cesar],c).
alpha_spell_form([david],d).
alpha_spell_form([erik],e).
alpha_spell_form([filip],f).
alpha_spell_form([gustav],g).
alpha_spell_form([helge],h).
alpha_spell_form([ivar],i).
alpha_spell_form([johan],j).
alpha_spell_form([kalle],k).
alpha_spell_form([ludvig],l).
alpha_spell_form([martin],m).
alpha_spell_form([niklas],n).
alpha_spell_form([olof],o).
alpha_spell_form([petter],p).
alpha_spell_form([quintus],q).
alpha_spell_form([rudolf],r).
alpha_spell_form([sigurd],s).
alpha_spell_form([tore],t).
alpha_spell_form([urban],u).
alpha_spell_form([viktor],v).
alpha_spell_form([wilhelm],w).
alpha_spell_form([xerxes],x).
alpha_spell_form([yngve],y).
alpha_spell_form([zäta],z).


capitalize(A,B):-
	
	name(A,[ACode]),
	(   ACode<123,
	    ACode>96,
	    BCode is ACode - 32;
	    ACode<91,
	    ACode>64,
	    ACode=BCode),
	name(B,[BCode]).

	

/*----------------------------------------------------------------------
     output_form( +Move, -WordList )
     -- Canned output
----------------------------------------------------------------------*/



% object-level clarification and groundnig questions
output_form( ask(C), Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, ['?'], Output ).



output_form( ask(set([Alt0|Alts])), Output):-
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
%	append(['Do you mean'|Alt0out], AltsOr, Output0 ),
	append(['Vill du '|Alt0out], AltsOr, Output0 ),
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
	
output_form( answer(notexist(X,Q)), ['Tyvärr finns det inget som rör '|InputQDot]):-
	input_form( InputQ, ask(X^Q) ),
	append( InputQ, ['.'], InputQDot ).
output_form( answer(unknown(Q)), ['Tyvärr finns det inget som rör '|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% for asking metaissue clarification question
output_form( issue(Q), ['fråga om'|Out] ):-
	input_form( Out, ask( Q ) ).


% for asking metaissue clarification question
%output_form( action(Action), ['to '|Out] ):-
%	input_form( Out, request( Action ) ).

% ICM


input_form( [förlåt], icm:per*neg ).
input_form( [okej], icm:acc*pos ).
input_form( [ok], icm:acc*pos ).

input_form( [vet, inte], icm:acc*neg:issue ).


output_form( icm:per*pos:String, ['Du sa ',Name,'.'] ):-
	name( Name, String ).

output_form( icm:sem*int, ['Vad menar du '] ).
output_form( icm:sem*neg, ['Jag förstår tyvärr inte.'] ).
output_form( icm:sem*pos:Move, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).

% ICM

% contact
output_form( icm:con*neg, ['Hallå?'] ).


% perception
output_form( icm:per*int, ['Ursäkta?'] ).
output_form( icm:per*int, ['Vad sade du?'] ).
output_form( icm:per*neg, ['Ursäkta, Jag hörde inte vad du sade.'] ).


output_form( icm:per*pos:String, ['Jag tyckte du sade',Name,'.'] ):-
	name( Name, String ).

output_form( icm:sem*int, ['Vad menar du'] ).
output_form( icm:sem*neg, ['Förlåt, jag förstår inte.'] ).
output_form( icm:sem*pos:Move, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).

% understanding(pragmatic)
output_form( icm:und*neg, ['Jag förstår inte riktigt.']  ).

output_form( icm:und*pos:usr*issue(Q), ['Du vill vet mer om '|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).
output_form( icm:und*pos:usr*(not issue(Q)), ['Du frågade inte om '|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).


output_form( icm:und*pos:usr*(not P), AnsNotPDot  ):-
	output_form( icm:und*pos:usr*P, AnsPDot  ),
	append( ['inte'],AnsPDot,AnsNotPDot ).

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
	%output_form( icm:und*pos:C, IcmPos ),
	%append( IcmPos0,['.'],IcmPos),
	%append( IcmPos0, [', is that correct?'], IcmInt ).

%output_form( icm:und*int:usr*C, IcmInt  ):-
%	input_form( answer(C), IcmInt ).


output_form( icm:und*int:usr*C, Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', stämmer det?'], Output ).




% clarification question
output_form( icm:und*int:usr*AltQ, Output):-
	output_form( ask(AltQ), Output).



% "acceptance"/integration

% icm-Type(-Polarity(-Args))
output_form( icm:acc*pos, ['Okey.'] ).

% reject(issue(Q))
output_form( icm:acc*neg:issue(Q), ['Jag kan tyvärr inte svara på frågan om '|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).



% reject proposition P
output_form( icm:acc*neg:P, ['Ledsen, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' är inte en korrekt parameter.'], Rest ).

% indicate loading a plan (pushed by findPlan)
%output_form( icm:loadplan, ['I need some information.'] ).
output_form( icm:loadplan, ['Låt oss se.'] ).


% reraise issue explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:Q, ['Gå tillbaks till frågan om '|InputQDot]):-
	( input_form( InputQ, ask(Q) ); output_form( ask(Q), InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise action explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:A, ['Gå tillbaks till '|InputQDot]):-
	( input_form( InputQ, request(A) ); output_form( action(A), InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise issue (system-initiated, where question follows immediately after)
output_form( icm:reraise, ['Så,']).

% accommodation
output_form( icm:accommodate:_, ['Visst.']  ).

output_form( icm:reaccommodate:Q, ['Gå tillbaks till frågan om'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).



output_form( not C, ['Inte'|S] ):- output_form( C, S ).


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/


% general negation 010419
input_form( [inte|S], answer(not(C))):- input_form(S,answer(C)).



input_form( [ja], answer(yes) ).
input_form( [nej], answer(no) ).


% simple stuff

input_form( [hej], greet ).
input_form( [hej,då], quit ).
input_form( [sluta], quit ).

% ICM

input_form( [förlåt], icm:per*neg ).
input_form( [okej], icm:acc*pos ).
input_form( [ok], icm:acc*pos ).

input_form( [vet, inte], icm:acc*neg:issue ).


/*----------------------------------------------------------------------
     yn_answer( ?YN )
----------------------------------------------------------------------*/

yn_answer(A):-
	A = 'ja';
	A = 'nej'.






/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

% use semantics as surface forms (only possible for english)
lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).

synset( [[videon],[video]], vcr ).

synset( [NumberPhrase], Number ):- number_phrase( NumberPhrase, Number ).

synset([[Channel]],Channel):- channel(Channel).






join([],_,'').
join([X],_,X).
join([X,Y|Xs],Sep,Joined):-
	atom(X),
	atom(Sep),
	atom_concat(X,Sep,Z),
	join([Y|Xs],Sep,Q),
	atom_concat(Z,Q,Joined).


%split(Atom,CharList).
split('',[]).
split(Atom,[C|Cs]):-
	atom_concat(C,Rest,Atom),
	atom_length(C,1),
	split(Rest,Cs).
