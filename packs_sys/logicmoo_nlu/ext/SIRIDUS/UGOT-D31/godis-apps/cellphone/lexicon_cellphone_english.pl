/*************************************************************************

            name: lexicon_cellphone_english.pl 
         version: Feb, 2005
     description: English lexicon for a cellphone
         authors: Anna Olsson and Jessica Villing
 
*************************************************************************/

:- module( lexicon_cellphone_english, [output_form/2, input_form/2, yn_answer/1] ).


%:- module( lexicon_cellphone_english, [output_form/2, input_form/2, input_form2/3, yn_answer/1, language/2, digit_word/2]).

:- ensure_loaded( library( digits_english ) ).
:- ensure_loaded( library( semsort_cellphone) ).

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
----------------------------------------------------------------------*/

%%% ask-moves
output_form( ask( X^(action(X)) ), ['What do you want to do?'] ).

output_form( ask(action(T)), Str ):-
	output_form(action(T), StrT ),
	append( ['Do you want to '], StrT, Str0 ),
	append( Str0, ['?'], Str).

%%% greet
output_form( greet, ['Welcome to the cellphone!'] ).

%%% quit
output_form( quit, ['Bye!'] ).

%%% help
output_form( help, ['Do you want the phone book, settings or make a call?'] ).


output_form( not C, ['Not'|S] ):- output_form( C, S ).




%%% confirm

output_form( confirm(call_name), ['Calling.'] ).
output_form( confirm(call_number), ['Calling.'] ).
output_form( confirm(search_phonebook), ['I am searching'] ).
output_form( confirm(add_new_entry), ['The name and phonenumber are added to the phonebook'] ).
output_form( confirm(delete_entry), ['The entry is now removed from the phonebook'] ).
output_form( confirm(change_language), ['The language is now changed'] ).
output_form( confirm(change_security_code), ['The security code is now changed'] ).
output_form( confirm(reset), ['Reset have been done'] ).


/*
output_form( confirm( code_correct ),
	     ['The code is correct'] ).
output_form( confirm( code_incorrect ),
	     ['The code is incorrect'] ).
output_form( confirm(entry_added(Name)), String ) :-
	name( Name, NameString ),
	append( NameString, [' has been added to the phone book'], String).
output_form( confirm(entry_deleted(Name)), String ) :-
	name( Name, NameString ),
	append( NameString, [' has been deleted from the phone book'], String).
output_form( confirm( new_language(L) ), String ) :-
	name(L, LString),
	append(['The language is now '], LString, String).
output_form( confirm( phone_reset ),
	     ['You have now reset all the settings'] ).
output_form( confirm( security_code_changed ),
	     ['You have now changed the security code'] ).
*/


%%% ask

% yn-questions
%output_form( ask( call ), ['Do you want to call this number?'] ).
output_form( ask( change_language(L) ), String ) :-
	name(L, LString),
	append( ['Do you want to change the language to '], LString, Str0),
	append(Str0, ['?'], String).


% wh-questions
% top
output_form( ask( [ action(call),
		    action(phonebook),
		    action(settings)
		  ] ),
	     ['Where do you want to go? Phone book or settings?'] ).

output_form( ask( X^current_security_code(X) ), ['What is the current security code?'] ).
output_form( ask( X^entry_to_delete(X) ),
	     ['Which entry do you want to delete? Please give me a name.'] ).
output_form( ask( X^language(X) ), ['What language do you want to change to?'] ).
output_form( ask( X^name_to_add(X) ), ['What name do you want to add?'] ).
output_form( ask( X^new_security_code(X) ), ['What security code do you want instead?'] ).
output_form( ask( X^number_to_add(X) ), ['What number do you want to add?'] ).
output_form( ask( X^search_for_name(X) ), ['What name do you want to search for?'] ).
output_form( ask( X^the_security_code(X) ), ['I need the security code'] ).


%Tillfällig lösning
output_form( ask( X^name(X) ), ['Please give me a name.']).

output_form( ask( X^number(X) ), ['Please give me a number'] ).

output_form( ask( X^language(X) ), ['What language do you want to change to?'] ).

%%% ask about action
output_form( ask( set( [ action(call_name),
			 action(call_number) ] ) ),
	     ['Do you want to call a person in the phone book or call a number?'] ).

output_form( ask( set( [ action(search_phonebook),
			 action(add_new_entry),
			 action(delete_entry) ] ) ),
	     ['Do you want to search the phonebook, add a new entry, or delete an entry?'] ).

% settings
output_form( ask( set( [ action(telephone_settings),
			 action(security_settings),
			 action(reset) ] ) ),
	     ['Do you want to go to telephone settings, security settings, or reset?'] ).

% telephone_settings
output_form( ask( [ action(change_language) ] ),
	     ['Say change the language if you want to change the language'] ).

% security_settings
output_form( ask( action(change_security_code) ),
	     ['Say "change the security code" if you want to change the security code'] ).


%%% action clarification

%SE (to handle a list of one action)
% (list format needed to get the action defined
%  as an action in dominates relations)
output_form( ask(action(T)), Str ):-
	output_form(action(T), StrT ),
	append( ['Do you want to '], StrT, Str0 ),
	append( Str0, ['?'], Str).

output_form( ask(ActionList), Str ) :-
	ActionList = [action(_)|_],
	altlist2altstr_or( ActionList, AltStr ),
	append( ['Do you want to '], AltStr, Str0 ),
	append( Str0, ['?'], Str).

output_form( action(add_new_entry), ['add a new entry']).
output_form( action(call), ['make a phone call to a person in the phone book or a number you know by heart'] ).
output_form( action(call_name), ['make a phone call to a person in the phone book'] ).
output_form( action(call_number), ['make a phone call to a number you know by heart'] ).

output_form( action(change_language), ['change the language'] ).
output_form( action(change_security_code), ['change the security code'] ).
output_form( action(delete_entry), ['delete an entry']).
output_form( action(phonebook), ['go to phonebook'] ).
output_form( action(reset), ['reset the phone'] ).
output_form( action(search_phonebook), ['search the phonebook']).
output_form( action(security_settings), ['go to security settings'] ).
output_form( action(settings), ['go to settings'] ).
output_form( action(telephone_settings), ['go to telephone settings'] ).


%% repeating a move

output_form( repeat(Move), Str ):-
	output_form( Move, Str ).


/*----------------------------------------------------------------------
     ICM
----------------------------------------------------------------------*/

% contact
output_form( icm:con*neg, ['Hello?'] ).


% perception
output_form( icm:per*int, ['Pardon?'] ).
output_form( icm:per*int, ['What did you say?'] ).
output_form( icm:per*neg, ['Sorry, I didnt hear what you said.'] ).

output_form( icm:per*pos:String, ['I heard you say',Name,'. '] ):-
	name( Name, String ).

output_form( icm:sem*int, ['What do you mean'] ).
output_form( icm:sem*neg, ['Sorry, I dont understand.'] ).
output_form( icm:sem*pos:Move, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).

%%% icm, semantic understanding
output_form( icm:sem*neg, ['What do you mean by that?'] ). 

%%% icm, pragmatic understanding
output_form( icm:und*neg,
	     ['I am sorry, I do not understand. Could you say it in a different way?'] ).

% understanding(pragmatic)
output_form( icm:und*neg, ['I dont quite understand.']  ).

% first clause added 021120 SL
output_form( icm:und*pos:usr*issue(Q), ['You want to know '|AnsPDot]  ):-
	output_form( Q, AnsP ),
	append(AnsP,['.'],AnsPDot).

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
output_form( icm:acc*neg:P, ['Sorry, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' is not a valid parameter.'], Rest ).

% indicate loading a plan (pushed by findPlan)
%output_form( icm:loadplan, ['I need some information.'] ).
output_form( icm:loadplan, ['Lets see.'] ).

% reraise issue (system-initiated, where question follows immediately after)
output_form( icm:reraise:top, ['So,']).

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



%%%% other

output_form( answer( _^alts(_,_), alts(_,AltList) ) , Str ):-
	altlist2altstr_and( AltList, AltStr ),
	append( ['The alternatives are: '], AltStr, Str1 ),
	append( Str1, ['.'], Str ).


/*----------------------------------------------------------------------
     altlist2altstr
     Fogar samman flera alternativ i samma fråga
----------------------------------------------------------------------*/

altlist2altstr_and( [D], Str ):-
	alt2altstr( D, Str1 ),
	append( [' and '], Str1, Str ).
altlist2altstr_and( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_and( Ds, Str2 ),
	append( Str1, [', '], Str3 ),
	append(Str3, Str2, Str ).

altlist2altstr_or( [D], Str ):-
	alt2altstr( D, Str1 ),
	append( [' or '], Str1, Str ).
altlist2altstr_or( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_or( Ds, Str2 ),
	append( Str1, [', '], Str3 ),
	append(Str3, Str2, Str ).

alt2altstr( D, Str ):-
	output_form( D, Str ).

alt2altstr( D, Str ):-
	atomic(D),
	name( D, Str ).




	start_trindikit.
/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
----------------------------------------------------------------------*/

%%% greet
input_form( [hello], greet ).

%%% quit
input_form( [bye], quit ).
input_form( [quit], quit ).

%%% yes and no
input_form( [yes], answer(yes) ).
input_form( [no], answer(no) ).

%%% acknowledge
input_form( [okay], ack).
input_form( [ok], ack).

%%% help
input_form( [help], help).

%%% actions

% top
input_form( [phonebook], request(phonebook) ).
input_form( [phone,book], request(phonebook) ).
input_form( [settings], request(settings) ).
input_form( [call], request(call) ).
input_form( [make,a,call], request(call) ).
input_form( [make,a,phonecall], request(call) ).
input_form( [call,someone], request(call_name) ).
input_form( [call,a,person], request(call_name) ).
input_form( [call,a,number], request(call_number) ).
input_form( [a,person], request(call_name) ).
input_form( [a,number], request(call_number) ).
input_form( [person], request(call_name) ).
input_form( [number], request(call_number) ).

% phonebook
input_form( [ search,the,phonebook ], request(search_phonebook) ).
input_form( [ search,the,phone,book ], request(search_phonebook) ).
input_form( [ search ], request(search_phonebook) ).
input_form( [ add,a,new,entry ], request(add_new_entry) ).
input_form( [ add,new,entry ], request(add_new_entry) ).
input_form( [ add,an,entry ], request(add_new_entry) ).
input_form( [ add,entry ], request(add_new_entry) ).
input_form( [ add ], request(add_new_entry) ).
input_form( [ delete,an,entry ], request(delete_entry) ).
input_form( [ delete,entry ], request(delete_entry) ).
input_form( [ delete ], request(delete_entry) ).


% security_settings
input_form( [ change,the,security,code ], request(change_security_code) ).
input_form( [ change,security,code ], request(change_security_code) ).



% settings
input_form( [ telephone,settings ], request(telephone_settings) ).
input_form( [ security,settings ], request(security_settings) ).
input_form( [ reset ], request(reset) ).


% telephone_settings
input_form( [ change,the,phones,language ], request(change_language) ).
input_form( [ change,the,language ], request(change_language) ).
input_form( [ change,language ], request(change_language) ).

/*
input_form( S, answer(number(C)) ) :-
	lexsem( S, C ),
	sem_sort( C, number ).
*/

%%% elliptical answers

% Har ändrat för att få det att funka med sem_sort istället för lex_name och lex_language
% Ska säkert skrivas lite elegantare... /Jessica
input_form( [C], answer(C) ) :- sem_sort( C, _ ).
input_form( S, answer(C) ) :- lexsem( S, C ), sem_sort( C, _ ).


% don't use complex interpretation

input_form2( _, _, _ ) :- fail.


/*----------------------------------------------------------------------
     yn_answer( A )
     --- is A yes or no?
----------------------------------------------------------------------*/

yn_answer(A):-
	A = 'yes';
	A = 'no'.

/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

% SL
lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).


% synset( [ [],[] ], ).

synset( [ [hello],[hi],[good,morning],[good,afternoon],
	  [good,evening] ], hello ).

synset( [ [bye],[goodbye],[bye-bye],[quit],[finish],[stop] ], bye ).

synset( [ [repeat],[sorry],[pardon],[what,did,you,say] ], repeat ).

synset( [NumberPhrase], Number ):- number_phrase( NumberPhrase, Number).