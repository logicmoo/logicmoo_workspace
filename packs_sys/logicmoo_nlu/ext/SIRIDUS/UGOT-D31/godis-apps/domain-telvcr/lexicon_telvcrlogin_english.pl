 
/*************************************************************************
 
         name: lexicon_vcr_english.pl 
      version: 
  description: 
       author: Staffan Larsson
 
*************************************************************************/

:- module( lexicon_telvcrlogin_english, [output_form/2, input_form/2,
				 yn_answer/1]). %jabben changes

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

output_form( ask(X^(action(X))), ['What can I do for you?'] ).

output_form( ask(action(T)), Str ):-
	output_form(action(T), StrT ),
	append( ['Do you want to '], StrT, Str0 ),
	append( Str0, ['?'], Str).




output_form( ask(X^user_name(X)),
	     ['Please say your user name!'] ).
output_form( ask(X^password(X)),
	     ['Please say your password!'] ).



% report

output_form( report(login,validUser(Usr,_,yes)), ['You logged in succesfully as '|UsrForm]):-
	output_form(answer(user_name(Usr)),UsrForm).
	  
output_form( report(login,validUser(Usr,Psswd,no)), ['The user '|Rest]):-
	var(Psswd),
	append(UsrForm, [' does not exist!'], Rest),
	output_form(answer(user_name(Usr)),UsrForm).

output_form( report(login,validUser(_,Psswd,no)), ['The password is not correct!']):-
	atom(Psswd).




%ouput_form( report(validUser(X,Y,Res), ['']).


% action

output_form( action(login), ['log in']).


output_form( action(top), ['Login'] ).


output_form( confirm(validUser(U,P,_Res)), Str ) :-
	output_form(user_name(U), Ustr),
	output_form( password(P), Pstr),
	append(['Checking username'],Ustr,Str0),
	append(['against password'],Pstr,Str1),
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

output_form( answer(invalid_password), ['Invalid password!!']).



output_form( answer(usage), ['You must log in to access the VCR manager.']).


output_form( icm:acc*neg:action( delete_rec_job, no_rec_job_to_delete ), ['There is no recording to delete.'] ).
output_form( confirm(delete_rec_job), ['OK. The recording was deleted.'] ).

output_form( report( vcr_login, validUser(Usr,_Passw,yes)), ['Welcome'|UsrForm] ):-
	output_form(answer(user_name(Usr)),UsrForm).
output_form( report( vcr_login, validUser(_Usr,_Passw,no)), ['No success in logging in.'] ).


output_form( icm:acc*neg:rec_job_to_delete(_), ['No program is stored in that position.'] ).
output_form( icm:acc*neg:channel_to_store(_), ['Sorry, the available channels are blabla.'] ).


output_form( greet, ['Welcome to the VCR gateway! Please log in'] ).
output_form( quit, ['Bye bye!'] ).


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



atomList2Str([],[]).
atomList2Str([A|As],[S|Ss]):-
	name(A,S),
	atomList2Str(As,Ss).


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

% VCR



input_form( [top], request(top) ).
input_form( [log, in], request(login)).
input_form( [login], request(login)).

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

digit_form([zero], '0').
digit_form([one], '1').
digit_form([two], '2').
digit_form([three], '3').
digit_form([four], '4').
digit_form([five], '5').
digit_form([six], '6').
digit_form([seven], '7').
digit_form([eight], '8').
digit_form([nine], '9').


alpha_form([A],A):-
	atom(A),
	atom_length(A,1),
	%capitalize(A,B),
	member(A,['Q','W','E','R','T','Y','U','I','O','P',
		  'A','S','D','F','G','H','J','K','L','Z',
		  'X','C','V','B','N','M',
		  'q','w','e','r','t','y','u','i','o','p',
		  'a','s','d','f','g','h','j','k','l','z',
		  'x','c','v','b','n','m']).



	
capitalize(A,A):- %% HACK!!!! av jacob som vill slippa kapitalisering ett litet tag
	
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
	%output_form( icm:und*pos:C, IcmPos ),
	%append( IcmPos0,['.'],IcmPos),
	%append( IcmPos0, [', is that correct?'], IcmInt ).

%output_form( icm:und*int:usr*C, IcmInt  ):-
%	input_form( answer(C), IcmInt ).


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
