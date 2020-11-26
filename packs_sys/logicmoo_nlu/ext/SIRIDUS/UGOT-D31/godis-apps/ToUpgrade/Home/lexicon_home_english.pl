:- module( lexicon_home_english, [output_form/2, input_form/2,
				  yn_answer/1]).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/

output_form( ask(X^(task(X))), "What can I do for you?" ).

output_form( ask(task(T)), Str ):-
	output_form(task(T), StrT ),
	append( "Do you want to ", StrT, Str0 ),
	append( Str0, " ?", Str).

output_form( ask(TaskList), Str ) :-
	TaskList = [task(_)|_],
	altlist2altstr_or( TaskList, AltStr ),
	append( "Do you want to ", AltStr, Str0 ),
	append( Str0, " ?", Str).

output_form( ask(quit), "Do you want to quit GoDiS?" ).


output_form( ask(X^(device(X))), "What object are you referring to?" ).
output_form( ask(X^(device_type(X))), "What object are you referring to?" ).
output_form( ask(X^(action(X))), "What do you want to do?" ).
output_form( ask(X^(device_state(X))), "What do you want to know?" ).
output_form( ask(X^(location(X))), "Where?" ).
output_form( ask(main_menu), "Do you want to choose another domain?" ).

output_form( ask(light(X)), Str ) :-
	atom_chars(X,XS),
	append( "Do you want to turn on the light in the ", XS, Str0 ),
	append( Str0, "?", Str ).

output_form( ask(leave_on_light),
	     "Do you want to leave the light on in any of the rooms?" ).

output_form( task(perform_specific_action), "change the light in any of the rooms" ).
output_form( task(make_query), "make a query").
output_form( task(main_menu), "choose another domain" ).
output_form( task(wake_up), "set to morning mode" ).
output_form( task(leaving), "set to leaving mode" ).

output_form( inform(device_already_switched_on(_,L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "The lamp in the ~s is already switched on.", [LS], Cs ).
output_form( inform(device_already_switched_off(_,L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "The lamp in the ~s is already switched off.", [LS], Cs ).
output_form( inform(light_already_dimmed(_,L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "The light in the ~s is already dimmed.", [LS], Cs ).

output_form( inform(no_lamp_in(_)),
	     "There is no lamp there." ).
output_form( inform(no_dimmer_in(_)),
	     "There is no dimmer there." ).

output_form( inform(is_switched_onoff(OnOff,S,L)), Cs ) :-
	concept_string( L, LS ),
	( ( ( OnOff = on,  S = switched_on ) ;
	    ( OnOff = off, S = switched_off ) ) ->
	    CR = "Yes" ; CR = "No" ),
	format_to_chars( "~s, the lamp in the ~s is switched ~a.", [CR,LS,OnOff], Cs ).

output_form( inform(is_dimmed(_,L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "The light in the ~s is dimmed.", [LS], Cs ).
output_form( inform(is_not_dimmed(_,L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "The light in the ~s is not dimmed.", [LS], Cs ).

output_form( inform(bad_action(lamp)),
	     "You can't do that with a lamp." ).
output_form( inform(bad_action(dimmer)),
	     "You can't do that with a dimmer." ).

output_form( inform(switched_on_lamp(L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "The lamp in the ~s is now switched on.", [LS], Cs ).
output_form( inform(switched_off_lamp(L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "The lamp in the ~s is now switched off.", [LS], Cs ).
output_form( inform(dimmed_light(L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "The light in the ~s is now dimmed.", [LS], Cs ).

output_form( inform(woken_up_lit_kitchen),
	     "Switched to morning mode. All lights are now switched on." ).
output_form( inform(woken_up_no_lit_kitchen),
	     "Switched to morning mode. All lights except for the one in the kitchen are now switched on." ).

output_form( inform(closed_down_except(L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "All lights except the one in the ~s have been shut down.", [LS], Cs ).
output_form( inform(closed_down_all),
	     "All lights have been shut down." ).

% silent confirmation of queries and change domain
output_form( inform(task(make_query)), "" ):-!.
output_form( inform(task(main_menu)), "" ):-!.
output_form( inform(task(top)), "" ):-!.
%output_form( inform(task(top)), "OK, top level." ).
%output_form( inform(task(T)), "OK.").
output_form( inform(task(T)), Str ):-
	move_string( task(T), TaskStr ),
	append( "OK, ", TaskStr, Str0 ),
	append( Str0, ".", Str ).
%
output_form( inform(task(T)), "" ).

move_string(Answer,String) :-
	input_form( Words, answer(Answer) ),
	concat_words( Words, String ).
concat_words( [Word], String ) :-
	atom_chars( Word, String ).
concat_words( [Word|Words], String ) :-
	atom_chars( Word, S1 ),
	concat_words( Words, S2 ),
	append( S1, [ 0' | S2 ], String ).



output_form( greet, "Welcome to the home device manager!" ).
output_form( quit, "Good bye!" ).
output_form( reqRep(understanding), "Pardon?" ).
output_form( reqRep(relevance), "What do you mean by that?" ).
output_form( thank, "Thank you very much." ).
output_form( confirm, "OK" ).

% repeating a move

output_form( repeat(Move), Str ):-
	output_form( Move, Str ).


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

/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

input_form( [svenska], answer(changeto(svenska)) ).
output_form( inform(current_language(english)), "OK, switched to English. Jolly good!").

input_form( [turn,on],      answer(action(switch_on)) ).
input_form( [turn,on],      answer(action(switch_on)) ).
input_form( [turn,off],     answer(action(switch_off)) ).
input_form( [turn,it,on],   answer(action(switch_on)) ).
input_form( [turn,it,off],  answer(action(switch_off)) ).
input_form( [switch,on],    answer(action(switch_on)) ).
input_form( [switch,off],   answer(action(switch_off)) ).
input_form( [switch,it,on], answer(action(switch_on)) ).
input_form( [switch,it,off],answer(action(switch_off)) ).
input_form( [dim,the,light],answer(action(dim_light)) ).
input_form( [dim,it],       answer(action(dim_light)) ).
input_form( [dim],       answer(action(dim_light)) ).

input_form( [turned,on],    answer(device_state(switched_on)) ).
input_form( [turned,off],   answer(device_state(switched_off)) ).
input_form( [switched,on],  answer(device_state(switched_on)) ).
input_form( [switched,off], answer(device_state(switched_off)) ).
input_form( [dimmed],       answer(device_state(dimmed)) ).
input_form( [on],    answer(device_state(switched_on)) ).
input_form( [off],   answer(device_state(switched_off)) ).

input_form( [change,domain],answer(task(main_menu)) ).
input_form( [change],       answer(task(perform_specific_action)) ).
input_form( [wake,up],      answer(task(wake_up)) ).
input_form( [good,morning], answer(task(wake_up)) ).
input_form( [morning,mode], answer(task(wake_up)) ).
input_form( [going,out],    answer(task(leaving)) ).
input_form( [leaving],      answer(task(leaving)) ).
input_form( [query],        answer(task(make_query)) ).
input_form( [is,the],       answer(task(make_query)) ).
input_form( [leave,on,the,light], answer(leave_on_light) ).
input_form( [leave,the,light,on], answer(leave_on_light) ).
input_form( [domain],       answer(task(main_menu))).

input_form( S, M ) :-
	lexsem( S, C ),
	input_move( C, M ).

% simple stuff

input_form( [hi], greet ).
input_form( [hello], greet ).
input_form( [bye], quit ).
input_form( [goodbye], quit ).
input_form( [quit], quit ).
input_form( [what,did,you,say], reqRep ).
input_form( ['what?'], reqRep ).
input_form( [sorry], reqRep ).
input_form( [pardon], reqRep ).
input_form( [yes], answer(yes) ).
input_form( [no], answer(no) ).
input_form( [okay], ack ).
input_form( [ok], ack ).
input_form( [aha], ack ).

% concepts

input_move( L, answer(location(L)) ) :-
	location( L ).

input_move( T, answer(device_type(T)) ) :-
	device_type( T ).

/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).

%synset( [[lamp], [light]], lamp ).
synset( [[lamp]], lamp ).
synset( [[dimmer]], dimmer ).
synset( [[kitchen], [diner]], kitchen ).
synset( [[tv,room], [tele,room], [television,room]], tv_room ).
synset( [[living,room], ['living-room']], living_room ).
synset( [[hall]], hall ).
synset( [[hobby]], hobby_room ).
synset( [[study]], study ).

% yes or no answer

yn_answer(A):-
	A = 'yes';
	A = 'no'.

% from concept to string (pick the first synonym)

concept_string( Concept, String ) :-
	synset( [Words|_], Concept ),
	concat_words( Words, String ).

concat_words( [Word], String ) :-
	atom_chars( Word, String ).

concat_words( [Word|Words], String ) :-
	atom_chars( Word, S1 ),
	concat_words( Words, S2 ),
	append( S1, [ 0' | S2 ], String ).

/*----------------------------------------------------------------------
     - Domain concepts
----------------------------------------------------------------------*/

action( X ) :- domain_home:action( X ).
task( T ) :- domain_home:task( T ).
location( L ) :- domain_home:location( L ).
device_type( T ) :- domain_home:device_type( T ).

sublist( SubList, List ) :-
	append( _, Suffix, List ),
	append( SubList, _, Suffix ).
