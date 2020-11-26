/*************************************************************************

            name: lexicon_cellphone_english.pl 
         version: 28 May, 2001
     description: English lexicon for Mini Mobile Phone
         authors: Staffan Larsson, Stina Ericsson
 
*************************************************************************/

%:- module( lexicon_cellphone_english, [output_form/2, input_form/2, yn_answer/1] ).


:- module( lexicon_cellphone_english, [output_form/2,
				       input_form/2,
				       input_form2/3,
				       yn_answer/1,
				       language/2,
				       digit_word/2
				      ]).

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
----------------------------------------------------------------------*/




%%% greet
output_form( greet, "Welcome to the cellphone!" ).

%%% quit
output_form( quit, "Bye!" ).

%%% reqRep(relevance)
output_form( reqRep(relevance), "What do you mean by that?"). 

%%% reqRep(understanding)
output_form( reqRep(understanding),
	     "I'm sorry, I don't understand. Could you say it in a different way?").


%%% inform
output_form( inform( calling( NameOrNum ) ), String ) :-
	name( NameOrNum, NameOrNumString ),
	append("I am now calling ", NameOrNumString, String).
output_form( inform( code_correct ),
	     "The code is correct" ).
output_form( inform( code_incorrect ),
	     "The code is incorrect" ).
output_form( inform(entry_added(Name)), String ) :-
	name( Name, NameString ),
	append( NameString, " has been added to the phone book", String).
output_form( inform(entry_deleted(Name)), String ) :-
	name( Name, NameString ),
	append( NameString, " has been deleted from the phone book", String).
output_form( inform( new_language(L) ), String ) :-
	name(L, LString),
	append("The language is now ", LString, String).
output_form( inform( phone_reset ),
	     "You have now reset all the settings" ).
output_form( inform( security_code_changed ),
	     "You have now changed the security code" ).



%%% ask

% yn-questions
output_form( ask( call ), "Do you want to call this number?" ).
output_form( ask( change_language(L) ), String ) :-
	name(L, LString),
	append( "Do you want to change the language to ", LString, Str0),
	append(Str0, "?", String).
%output_form( ask( change_language ), "Do you want to change the phone's language?" ).
%output_form( ask( change_security_code ), "Do you want to change the security code?" ).



% wh-questions
output_form( ask( X^call(X) ), "Who do you want to call? Please give me a name or a number.").
output_form( ask( X^current_security_code(X) ), "What is the current security code?" ).
output_form( ask( X^entry_to_delete(X) ),
	     "Which entry do you want to delete? Please give me a name." ).
output_form( ask( X^language(X) ), "What language do you want to change to?" ).
output_form( ask( X^name_to_add(X) ), "What name do you want to add?" ).
output_form( ask( X^new_security_code(X) ), "What security code do you want instead?" ).
output_form( ask( X^number_to_add(X) ), "What number do you want to add?" ).
output_form( ask( X^search_for_name(X) ), "What name do you want to search for?" ).
output_form( ask( X^the_security_code(X) ), "I need the security code" ).

% wh-questions; ask for task
output_form( ask( X^(task=X) ), "What do you want to do?" ).
output_form( ask( X^(task(X)) ), "What do you want to do?" ).

%%% ask about task

output_form( ask( [ task(search_phonebook),
		    task(add_new_entry),
		    task(delete_entry) ] ),
	     "Do you want to search the phonebook, add a new entry, or delete an entry?").

% security_settings
%output_form( ask( task(change_security_code) ),
%	     "Say 'change the security code' if you want to change the security code" ).
output_form( ask( [ task(change_security_code) ] ),
	     "Say 'change the security code' if you want to change the security code" ).


% settings
output_form( ask( [ task(telephone_settings),
		    task(security_settings),
		    task(reset) ] ),
	     "Do you want to go to telephone settings, security settings, or reset?").

% telephone_settings
output_form( ask( [ task(change_language) ] ),
	     "Say 'change the language' if you want to change the language" ).


% top
output_form( ask( [ task(call), task(phonebook),
		    task(settings)%, task(tone_control)
		  ] ),
	     %"Where do you want to go? Phone book, settings or tone control?" ).
	     "Where do you want to go? Phone book or settings?" ).

output_form( ask(main_menu), "Do you want to choose another domain?" ).





%%% task clarification

%SE (to handle a list of one task)
% (list format needed to get the task defined
%  as a task in dominates relations)
output_form( ask([task(T)]), Str ):-
	output_form(task(T), StrT ),
	append( "Do you want to ", StrT, Str0 ),
	append( Str0, " ?", Str).
	    
output_form( ask(task(T)), Str ):-
	output_form(task(T), StrT ),
	append( "Do you want to ", StrT, Str0 ),
	append( Str0, " ?", Str).


output_form( ask(TaskList), Str ) :-
	TaskList = [task(_)|_],
	altlist2altstr_or( TaskList, AltStr ),
	append( "Do you want to ", AltStr, Str0 ),
	append( Str0, " ?", Str).

output_form( task(add_new_entry), "add a new entry").
output_form( task(call), "make a phone call" ).
output_form( task(change_language), "change the language" ).
output_form( task(change_security_code), "change the security code" ).
output_form( task(delete_entry), "delete an entry").
output_form( task(phonebook), "go to phonebook" ).
output_form( task(reset), "reset the phone" ).
output_form( task(search_phonebook), "search the phonebook").
output_form( task(security_settings), "go to security settings" ).
output_form( task(settings), "go to settings" ).
output_form( task(telephone_settings), "go to telephone settings" ).
output_form( task(tone_control), "go to tone control" ).
output_form( task(main_menu), "choose another domain" ).

%% repeating a move

output_form( repeat(Move), Str ):-
	output_form( Move, Str ).




%%% answer
%output_form( answer( X^(entry_addedDB(X)), entry_addedDB(_) ),
%	     "The entry has been added" ).
%output_form( answer( X^(entry_addedDB(X)), fail ),
%	    "I'm sorry, I cannot add that name and number to the phone book" ). 
%output_form( answer( X^(entry_deletedDB(X)), entry_deletedDB(_) ),
%	     "The entry has been deleted" ).
%output_form( answer( X^(entry_deletedDB(X)), fail ),
%	    "I'm sorry, the name you want to delete is not in the phone book" ). 
output_form( answer( X^(pb_numberDB(X)), pb_numberDB(Number) ), String) :-
%	name(Number, NumberString),
	number( Number ), number_chars_nice( Number, NumberStr ),
	append("The number is ", NumberStr, String).
output_form( answer( X^(pb_numberDB(X)), fail ),
	     "I'm sorry, the name is not in the phone book" ).
	     

%%%% other

output_form( answer( _^alts(_,_), alts(_,AltList) ) , Str ):-
	altlist2altstr_and( AltList, AltStr ),
	append( "The alternatives are: ", AltStr, Str1 ),
	append( Str1, ".", Str ).


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
	append( " or ", Str1, Str ).
altlist2altstr_or( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_or( Ds, Str2 ),
	append( Str1, ", ", Str3 ),
	append(Str3, Str2, Str ).

alt2altstr( D, Str ):-
	output_form( D, Str ).

alt2altstr( D, Str ):-
	atomic(D),
	name( D, Str ).




/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
----------------------------------------------------------------------*/

% numbers

digit_word(zero,0'0).
digit_word(one,0'1).
digit_word(two,0'2).
digit_word(three,0'3).
digit_word(four,0'4).
digit_word(five,0'5).
digit_word(six,0'6).
digit_word(seven,0'7).
digit_word(eight,0'8).
digit_word(nine,0'9).


%%% greet
input_form( Hello, greet ) :- lexsem( Hello, hello ).

%%% quit
input_form( Bye, quit ) :- lexsem( Bye, bye ).

%%% yes and no
input_form( [yes], answer(yes) ).
input_form( [no], answer(no) ).

%%% acknowledge
input_form( [okay], ack).
input_form( [ok], ack).

%%% reqRep
input_form( Repeat, reqRep ) :- lexsem( Repeat, repeat ).



%%% tasks


% phonebook
input_form( [search,the,phonebook], answer(task(search_phonebook)) ).
input_form( [search,the,phone,book], answer(task(search_phonebook)) ).
%input_form( [search], answer(task(search_phonebook)) ).
input_form( [add,a,new,entry], answer(task(add_new_entry)) ).
input_form( [add,new,entry], answer(task(add_new_entry)) ).
input_form( [add,an,entry], answer(task(add_new_entry)) ).
input_form( [add,entry], answer(task(add_new_entry)) ).
input_form( [add], answer(task(add_new_entry)) ).
input_form( [delete,an,entry], answer(task(delete_entry)) ).
input_form( [delete,entry], answer(task(delete_entry)) ).
input_form( [delete], answer(task(delete_entry)) ).
input_form( [domain], answer(task(main_menu))).


% security_settings
input_form( [change,the,security,code], answer(task(change_security_code)) ).
input_form( [change,security,code], answer(task(change_security_code)) ).



% settings
input_form( [telephone,settings], answer(task(telephone_settings)) ).
input_form( [security,settings], answer(task(security_settings)) ).
input_form( [reset], answer(task(reset)) ).


% telephone_settings
input_form( [change,the,phones,language], answer(task(change_language)) ).
input_form( [change,the,language], answer(task(change_language)) ).
input_form( [change,language], answer(task(change_language)) ).


% tone_control



% top
input_form( [call], answer(task(call)) ).
input_form( [make,a,call], answer(task(call)) ).
input_form( [make,a,phonecall], answer(task(call)) ).
input_form( [call,someone], answer(task(call)) ).
input_form( [phonebook], answer(task(phonebook)) ).
input_form( [phone,book], answer(task(phonebook)) ).
input_form( [settings], answer(task(settings)) ).
%input_form( [tone,control], answer(task(tone_control)) ).



%%% non-task answers

input_form( [Language], answer(language(Language)) ) :-
	lex_language(Language).

input_form( [Name], answer(name(Name)) ) :-
	lex_name(Name).

/*
Now handled by TrindiKit (interpret_simple2)

input_form( Number, answer(number(Number)) ) :-
	lex_number(Number).

input_form( Sec_Code, answer(security_code(Sec_Code)) ) :-
	lex_security_code(Sec_Code).
*/


%%% elliptical answers

input_form( [C], answer(C) ) :- concept( C ).
input_form( S, answer(C) ) :- lexsem( S, C ), concept( C ).


% don't use complex interpretation

input_form2( _, _, _ ) :- fail.


/*----------------------------------------------------------------------
     yn_answer( A )
     --- is A yes or no?
----------------------------------------------------------------------*/

yn_answer(A):-
	A = yes;
	A = no.






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







/*----------------------------------------------------------------------
     - Conceptual knowledge
----------------------------------------------------------------------*/


concept(C):-
	lex_language(C);
	lex_name(C).
%        lex_number(C); % Removed by Alex
%	lex_security_code(C).



%%% language lexicon
lex_language(deutsch).
lex_language(english).
lex_language(espanol).
lex_language(francais).
lex_language(french).
lex_language(gaeilge).
lex_language(german).
lex_language(irish).
lex_language(spanish).
lex_language(svenska).
lex_language(swedish).



%%% name lexicon
lex_name(alex).
lex_name(alida).
lex_name(anna).
lex_name(björn).
lex_name(bo).
lex_name(david).
lex_name(edith).
lex_name(elsa).
lex_name(erik).
lex_name(hildur).
lex_name(hugo).
lex_name(jan).
lex_name(john).
lex_name(kalle). 
lex_name(leif).
lex_name(lisa).
lex_name(maria).
lex_name(mia).
lex_name(olle).
lex_name(oscar).
lex_name(paul).
lex_name(pelle). 
lex_name(per).
lex_name(peter).
lex_name(robert).
lex_name(robin). 
lex_name(staffan). 
lex_name(stina).
lex_name(åsa).


%%% (telephone) number lexicon
lex_number(Num) :-
	atomic(Num),
	name(Num,NumString),
	name(N,NumString),
	integer(N),
	10000 =< N.


%%% security code lexicon
lex_security_code(SecCode) :-
	atomic(SecCode),
	name(SecCode, X),
	name(SC, X),
	integer(SC),
	SC =< 9999,
	1000 =< SC.


number_chars_nice( Number, CharsNice ) :-
	number_chars( Number, Chars ),
	nicify( Chars, CharsNice ).

nicify( [], [] ).
nicify( [C], [C] ) :- !.
nicify( [C|Cs], [C,0' |Cs1] ) :-
	nicify( Cs, Cs1 ).
