:- module( lexicon_vcrphone_english, [output_form/2, input_form/2,
				      yn_answer/1]).
:- use_module( library(vcr_languages) ).
:- ensure_loaded( library(lexicon_numbers) ).
:- ensure_loaded( library(digits_english) ).
:- use_module( library(telephone_names) ).
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
	append( Str0, "?", Str).

output_form( ask(TaskList), Str ) :-
	TaskList = [task(_)|_],
	altlist2altstr_or( TaskList, AltStr ),
	append( "Do you want to ", AltStr, Str0 ),
	append( Str0, "?", Str).

output_form( ask(quit), "Do you want to quit GoDiS?" ).


output_form( ask(X^new_program_position(X)),
	     "What channel do you want?" ).
output_form( ask(X^program_position_to_store(X)),
	     "What channel do you want?" ).
output_form( ask(X^date_to_store(X)),
	     "What date?" ).
output_form( ask(X^start_time_to_store(X)),
	     "What time do you want to start recording?" ).
output_form( ask(X^stop_time_to_store(X)),
	     "What time do you want to stop recording?" ).
output_form( ask(X^program_number(X)),
	     "What program number to want to delete?" ).
output_form( ask(X^new_clock(X)),
	     "What time?" ).

output_form( ask(X^base_station_language(X)),
	     "To what language to you want to set the base station display?" ).
output_form( ask(X^handset_language(X)),
	     "To what language to you want to set the handset display?" ).
output_form( ask(X^ring_volume(X)),
	     "Choose a volume level from 0 to 7." ).
output_form( ask(X^earpiece_volume(X)),
	     "Choose a volume level from 0 to 7." ).
output_form( ask(X^signal_type(X)),
	     "Please choose signal type: internal, external, message or search signal." ).
output_form( ask(X^tone_or_melody(X)),
	     "Please choose a tone or melody: low, medium, high, mixed, jumpy, Einie kleine Nachtmusik, Toccata, Elise, Samba or Blues Rythm." ).
output_form( ask( X^phonebook_name_to_add(X) ), "What name do you want to add?" ).
output_form( ask( X^phonebook_number_to_add(X) ), "What number do you want to add?" ).
output_form( ask( X^phonebook_entry_to_delete(X) ),
	     "Which entry do you want to delete? Please give me a name." ).
output_form( ask( X^phonebook_name_to_find(X) ), "What name do you want to search for?" ).

output_form( ask(main_menu), "Do you want to choose another domain?" ).

output_form( ask( call(Number) ), Cs ) :-
	format_to_chars( 'The number is ~a. Do you want to call this number?', [Number], Cs ).

output_form( task(vcr_top), "go to the VCR manager" ).
output_form( task(vcr_change_play_status), "change play status" ).
output_form( task(vcr_timer_recording), "go to timer recording" ).
output_form( task(vcr_settings), "go to settings" ).
output_form( task(vcr_query), "make a query" ).
output_form( task(vcr_new_program_position), "change channel" ).
output_form( task(vcr_play), "play" ).
output_form( task(vcr_stop), "stop" ).
output_form( task(vcr_ff), "fast-forward" ).
output_form( task(vcr_rew), "rewind" ).
output_form( task(vcr_pause_still), "pause" ).
output_form( task(vcr_still_adv), "advance" ).
output_form( task(vcr_rec), "record" ).
output_form( task(vcr_add_program), "add a program" ).
output_form( task(vcr_add_program1), "add a program" ).
output_form( task(vcr_delete_program), "delete a program" ).
output_form( task(vcr_delete_program1), "delete a program" ).
output_form( task(vcr_set_clock), "set the clock" ).
output_form( task(vcr_query_status), "query the current play status" ).
output_form( task(vcr_query_channel), "find out what channel is on" ).

output_form( task(tp_top), "go to the telephone manager" ).
output_form( task(tp_base_station), "go to the base station" ).
output_form( task(tp_handset), "go to the handset" ).
output_form( task(tp_phonebook), "go to the phonebook" ).
output_form( task(tp_answering_machine), "go to the answering machine" ).
%output_form( task(tp_answering_machine_onoff), "switch it on or off" ).
output_form( task(tp_answering_machine_switch_on), "switch it on" ).
output_form( task(tp_answering_machine_switch_off), "switch it off" ).
output_form( task(tp_base_station_settings), "go to base station settings" ).
output_form( task(tp_base_station_settings_volume), "set the volume" ).
output_form( task(tp_base_station_settings_autoanswer), "switch auto answer on or off" ).
output_form( task(tp_base_station_settings_autoanswer_on), "enable auto answer" ).
output_form( task(tp_base_station_settings_autoanswer_off), "disable auto answer" ).
output_form( task(tp_base_station_settings_basic), "go to basic settings" ).
output_form( task(tp_base_station_settings_basic_date), "set the date" ).
output_form( task(tp_base_station_settings_basic_language),
	     "change language for the base station display" ).
output_form( task(tp_handset_warnings), "manage ringer signals and volume" ).
output_form( task(tp_handset_language), "change language for the handset display" ).
output_form( task(tp_handset_earpiece_volume), "set the earpiece volume" ).
output_form( task(tp_handset_warnings_volume), "set the ringer volume" ).
output_form( task(tp_handset_warnings_signals), "manage ringer signals" ).
output_form( task(tp_phonebook_new_entry), "add a new entry" ).
output_form( task(tp_phonebook_search_entry), "search the phonebook" ).
output_form( task(tp_phonebook_delete_entry), "delete an entry" ).

output_form( task(main_menu), "choose another domain" ).

output_form( inform(already_playing), "The VCR is already playing." ).
output_form( inform(now_playing), "The VCR is now playing." ).
output_form( inform(already_stopped), "The VCR is already stopped." ).
output_form( inform(now_stopped), "The VCR is now stopped." ).
output_form( inform(already_ff), "The VCR is already fast-forwarding." ).
output_form( inform(now_ff), "The VCR is now fast-forwarding." ).
output_form( inform(already_rewinding), "The VCR is already rewinding." ).
output_form( inform(now_rewinding), "The VCR is now rewinding." ).
output_form( inform(already_paused), "The VCR is already paused." ).
output_form( inform(now_paused), "The VCR is now paused." ).
output_form( inform(already_recording), "The VCR is already recording." ).
output_form( inform(now_recording), "The VCR is now recording." ).
output_form( inform(advanced), "OK." ).
output_form( inform(not_paused), "The VCR is not paused." ).
output_form( inform(no_available_program_slot), "There is no available program slot." ).
output_form( inform(clock_was_set), "The clock was set." ).
output_form( inform(date_cannot_be_set),
	     "Sorry, the date cannot be set in this version." ).



output_form( inform(ok_new_program_position(P)), Cs ) :-
	format_to_chars( 'Switched to channel ~a.\n', [P], Cs ).

output_form( inform(program_added(Position,Date,Start,Stop)), Cs ) :-
	date_output(Date,DateOutput),
	( atom_chars(Start,[Start1,Start2,Start3,Start4]);
	    atom_chars(Start,[Start2,Start3,Start4]), Start1=48),
	( atom_chars(Stop,[Stop1,Stop2,Stop3,Stop4]);
	    atom_chars(Stop,[Stop2,Stop3,Stop4]), Stop1=48 ),
	format_to_chars( 'I will record channel ~a ~a from ~c~c:~c~c to ~c~c:~c~c.',
			 [Position,DateOutput,
			  Start1,Start2,Start3,Start4,
			  Stop1,Stop2,Stop3,Stop4], Cs ).

output_form( inform(programs(Programs)), Cs ) :-
	program_listing( Programs, Listing ),
	append( "Which program number to you want to delete?", Listing, Cs ).

output_form( inform(no_program_to_delete), "There is no program to delete." ).
output_form( inform(program_deleted), "The program was deleted." ).
output_form( inform(no_such_program), "No program is stored in that position." ).

output_form( inform(confirm_status(S)), Cs ) :-
	status_output(S,SO),
	format_to_chars( 'Yes, the VCR is ~a.', [SO], Cs ).

output_form( inform(status_is(S)), Cs ) :-
	status_output(S,SO),
	format_to_chars( 'No, the VCR is ~a.', [SO], Cs ).

output_form( inform(current_program_position(P)), Cs ) :-
	format_to_chars( 'The current channel is ~a.', [P], Cs ).

output_form( inform(answering_machine_already_on),
	     "The answering machine is already switched on." ).
output_form( inform(answering_machine_now_on),
	     "The answering machine is now switched on." ).
output_form( inform(answering_machine_already_off),
	     "The answering machine is already switched off." ).
output_form( inform(answering_machine_now_off),
	     "The answering machine is now switched off." ).
output_form( inform(autoanswer_already_on),
	     "Auto answer is already enabled." ).
output_form( inform(autoanswer_now_on),
	     "Auto answer is now enabled." ).
output_form( inform(autoanswer_already_off),
	     "Auto answer is already disabled." ).
output_form( inform(autoanswer_now_off),
	     "Auto answer is now disabled." ).
output_form( inform(ring_volume_was_set),
	     "The ring volume was set." ).
output_form( inform(earpiece_volume_was_set),
	     "The earpiece volume was set." ).
output_form( inform(new_earpiece_volume(V)), Cs ) :-
	format_to_chars( 'The earpiece volume was set to ~a.\n', [V], Cs ).
output_form( inform(new_ring_volume(V)), Cs ) :-
	format_to_chars( 'The handset ring volume was set to ~a.\n', [V], Cs ).

output_form( inform(base_station_language_was_set(L)), Cs ) :-
	capitalize(L,LS),
	format_to_chars('The base station language was set to ~s.', [LS], Cs ).
output_form( inform(handset_language_was_set(L)), Cs ) :-
	capitalize(L,LS),
	format_to_chars('The handset language was set to ~s.', [LS], Cs ).
output_form( inform(signal_was_set(SignalType,ToneOrMelody)), Cs ) :-
	%signal_type_string(SignalType,SignalTypeCs),
	%tone_or_melody
	move_string( signal_type(SignalType), SignalTypeCs ),
	move_string( tone_or_melody(ToneOrMelody), ToneOrMelodyCs ),
	format_to_chars('~s calls will use ~s.',[SignalTypeCs,ToneOrMelodyCs],Cs).

output_form( inform(phonebook_entry_added(Name)), String ) :-
	atom_chars( Name, NameString ),
	append( NameString, " has been added to the phone book.", String).
output_form( inform(phonebook_entry_deleted(Name)), String ) :-
	atom_chars( Name, NameString ),
	append( NameString, " has been deleted from the phone book.", String).
output_form( inform(phonebook_entry_exists(Name)), String ) :-
	atom_chars( Name, NameString ),
	append( NameString, " is in the phone book already.", String).
output_form( inform(phonebook_entry_not_found), "No such name was found." ).
output_form( inform(now_calling(Name)), Cs ) :-
	format_to_chars( 'I am now calling ~a.', [Name], Cs ).

% silent confirmation of queries and change domain
output_form( inform(task(top)), "OK, top level." ):-!.
output_form( inform(task(vcr_query)), "" ):-!.
output_form( inform(task(vcr_query_channel)), "" ):-!.
output_form( inform(task(vcr_query_status)), "" ):-!.
output_form( inform(task(main_menu)), "" ):-!.

output_form( inform(task(T)), Str ):-
%	output_form( task(T), TaskStr ),
	move_string( task(T), TaskStr ),
	append( "OK, ", TaskStr, Str0 ),
	append( Str0, ".", Str ).
%input_form( [top,level], task(top) ).
output_form( inform(task(T)), Str ):-
	output_form( task(T), TaskStr ),
	append( "OK, ", TaskStr, Str0 ),
	append( Str0, ".", Str ).
%input_form( [top,level], task(top) ).
output_form( inform(task(T)), "" ).

output_form( greet, "Welcome to the telephone and VCR manager!" ).
output_form( quit, "Good bye!" ).
output_form( reqRep(understanding), "Pardon?" ).
output_form( reqRep(relevance), "What do you mean by that?" ).
output_form( thank, "Thank you very much." ).
output_form( confirm, "OK" ).

% repeating a move

output_form( repeat(Move), Str ):-
	output_form( Move, Str ).

date_output(today,today) :- !.

date_output(Date,DateOutput) :-
	atom_chars(Date,Chars),
	format_to_chars('~c~c/~c~c',Chars,DateOutputCs),
	atom_chars(DateOutput,DateOutputCs).

status_output(ff,'fast-forwarding') :- !.
status_output(S,S).

altlist2altstr_and( [D], Str ):-
	alt2altstr( D, Str1 ),!,
	append( " and ", Str1, Str ).
altlist2altstr_and( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_and( Ds, Str2 ),
	append( Str1, ", ", Str3 ),
	append(Str3, Str2, Str ).

altlist2altstr_or( [D], Str ):-
	alt2altstr( D, Str1 ),!,
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

program_listing( [], "" ).

program_listing( [N:(Position,Date,Start,Stop)|Programs], Listing ) :-
	( atom_chars(Start,[Start1,Start2,Start3,Start4]);
	    atom_chars(Start,[Start2,Start3,Start4]), Start1=48),
	( atom_chars(Stop,[Stop1,Stop2,Stop3,Stop4]);
	    atom_chars(Stop,[Stop2,Stop3,Stop4]), Stop1=48 ),
	atom_chars(Date,[Date1,Date2,Date3,Date4]),
%	atom_chars(Start,[Start1,Start2,Start3,Start4]),
%	atom_chars(Stop,[Stop1,Stop2,Stop3,Stop4]),
	format_to_chars( 'Number ~d: channel ~a, ~c~c/~c~c, from ~c~c:~c~c to ~c~c:~c~c; ',
			 [N,Position,
			  Date1,Date2,Date3,Date4,
			  Start1,Start2,Start3,Start4,
			  Stop1,Stop2,Stop3,Stop4], This ),
	program_listing( Programs, Rest ),
	append( This, Rest, Listing ).

capitalize(A,[CC|Cs]) :-
	atom_chars(A,[C|Cs]),
	cap_char(C,CC).

cap_char(A,B) :-
	A >= 0'a,
	A =< 0'z,
	!,
	B is A - 32.

cap_char(A,A).

move_string(Answer,String) :-
	input_form( Words, answer(Answer) ),
	concat_words( Words, String ).

/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

input_form( [svenska], answer(changeto(svenska)) ).
output_form( inform(current_language(english)), "OK, switched to English. Jolly good!").

% VCR

input_form( [vcr],             answer(task(vcr_top)) ).
input_form( [play,status],     answer(task(vcr_change_play_status)) ).
input_form( [timer,recording], answer(task(vcr_timer_recording)) ).
input_form( [add,a,program],   answer(task(vcr_add_program)) ).
input_form( [add,program],     answer(task(vcr_add_program)) ).
input_form( [delete,program],  answer(task(vcr_delete_program)) ).
input_form( [delete,a,program],answer(task(vcr_delete_program)) ).
input_form( [clock],           answer(task(vcr_set_clock)) ).
input_form( [change,program],  answer(task(vcr_new_program_position)) ).
input_form( [switch,program],  answer(task(vcr_new_program_position)) ).
input_form( [change,channel],  answer(task(vcr_new_program_position)) ).
input_form( [switch,channel],  answer(task(vcr_new_program_position)) ).
input_form( [increase,channel],[answer(task(vcr_new_program_position)),
				answer(incdec(increase))]).
input_form( [decrease,channel],[answer(task(vcr_new_program_position)),
				answer(incdec(decrease))]).
input_form( [next,channel],    [answer(task(vcr_new_program_position)),
				answer(incdec(increase))]).
input_form( [previous,channel],[answer(task(vcr_new_program_position)),
				answer(incdec(decrease))]).
input_form( [specific,channel],answer(task(vcr_new_specific_program_position)) ).
input_form( [play],            answer(task(vcr_play)) ).
input_form( [stop],            answer(task(vcr_stop)) ).
input_form( [forward],         answer(task(vcr_ff)) ).
input_form( [rewind],          answer(task(vcr_rew)) ).
input_form( [pause],           answer(task(vcr_pause_still)) ).
input_form( [advance],         answer(task(vcr_still_adv)) ).
input_form( [record],          answer(task(vcr_rec)) ).
input_form( [today],           answer(date_to_store(today)) ).
input_form( [playing],         answer(status(playing)) ).
input_form( [paused],          answer(status(paused)) ).
input_form( [stopped],         answer(status(stopped)) ).
input_form( [rewinding],       answer(status(rewinding)) ).
input_form( [forwarding],      answer(status(ff)) ).
input_form( [recording],       answer(status(recording)) ).
input_form( [query],           answer(task(vcr_query)) ).
input_form( [what,channel],    answer(task(vcr_query_channel)) ).
input_form( [which,channel],   answer(task(vcr_query_channel)) ).


% ??? should be a command
input_form( [switch,to,PrCh|L], answer(new_program_position(P)) ) :-
	program_word(PrCh),
	input_form( L, answer(number(P)) ).

% ??? same here
input_form( [record,PrCh|L], answer(program_position_to_store(P)) ) :-
	program_word(PrCh),
	input_form( L, answer(number(P)) ).

input_form( [from|L], answer(start_time_to_store(T)) ) :-
	input_form( L, answer(number(T)) ).
input_form( [starting,at|L], answer(start_time_to_store(T)) ) :-
	input_form( L, answer(number(T)) ).

input_form( [to|L], answer(stop_time_to_store(T)) ) :-
	input_form( L, answer(number(T)) ).
input_form( [ending,at|L], answer(stop_time_to_store(T)) ) :-
	input_form( L, answer(number(T)) ).


input_form( [PrCh|L], answer(program_position(P)) ) :-
	program_word(PrCh),
	input_form( L, answer(number(P)) ).

input_form( [clock,to|L], answer(new_clock(C)) ) :-
	input_form( L, answer(number(C)) ).

% Telephone

input_form( [telephone],            answer(task(tp_top)) ).
input_form( [base,station],         answer(task(tp_base_station)) ).
input_form( [answering,machine],    answer(task(tp_answering_machine)) ).
input_form( [base,station,settings],answer(task(tp_base_station_settings)) ).
input_form( [base,station,volume],  answer(task(tp_base_station_settings_volume)) ).
input_form( [volume],               answer(task_type(volume)) ).
input_form( [basic, settings],                answer(task(tp_base_station_settings_basic)) ).
input_form( [date],                 answer(task(tp_base_station_settings_basic_date)) ).
input_form( [base,station,language],answer(task(tp_base_station_settings_basic_language)) ).
input_form( [change,language,for,the,base,station],answer(task(tp_base_station_settings_basic_language)) ).
input_form( [language],             answer(task_type(language)) ).
input_form( [L],                    answer(language(L)) ) :-
	vcr_language(L).
input_form( [auto,answer],          answer(task(tp_base_station_settings_autoanswer)) ).
input_form( [automatic,answer],     answer(task(tp_base_station_settings_autoanswer)) ).
input_form( [handset],              answer(task(tp_handset)) ).
input_form( [handset,language],     answer(task(tp_handset_language)) ).
input_form( [change,language,for,the,handset],     answer(task(tp_handset_language)) ).
input_form( [earpiece,volume],      answer(task(tp_handset_earpiece_volume)) ).
input_form( [phonebook],            answer(task(tp_phonebook)) ).
input_form( [search,the,phonebook], answer(task(tp_phonebook_search_entry)) ).
input_form( [search,the,phone,book],answer(task(tp_phonebook_search_entry)) ).
input_form( [search,for],           answer(task(tp_phonebook_search_entry)) ).
input_form( [what, phonenumber], answer(task(tp_phonebook_search_entry)) ).%011217
input_form( [what, number], answer(task(tp_phonebook_search_entry)) ).%011217


input_form( [add,a,new,entry],      answer(task(tp_phonebook_new_entry)) ).
input_form( [add,new,entry],        answer(task(tp_phonebook_new_entry)) ).
input_form( [add,an,entry],         answer(task(tp_phonebook_new_entry)) ).
input_form( [add,entry],            answer(task(tp_phonebook_new_entry)) ).
input_form( [add,a,new,name],      answer(task(tp_phonebook_new_entry)) ).
input_form( [add,new,name],        answer(task(tp_phonebook_new_entry)) ).
input_form( [add,a,new,number],      answer(task(tp_phonebook_new_entry)) ).
input_form( [add,new,number],        answer(task(tp_phonebook_new_entry)) ).

input_form( [delete,an,entry],      answer(task(tp_phonebook_delete_entry)) ).
input_form( [delete,entry],         answer(task(tp_phonebook_delete_entry)) ).
input_form( [delete,Name,from,the,phonebook], [answer(task(tp_phonebook_delete_entry))|NameAns] ):- input_form([Name],NameAns).

input_form( [ringer],               answer(task(tp_handset_warnings)) ).
input_form( [ringer,signals],       answer(task(tp_handset_warnings_signals)) ).
input_form( [ringer,volume],        answer(task(tp_handset_warnings_volume)) ).
input_form( [low],                  answer(tone_or_melody(low)) ).
input_form( [medium],               answer(tone_or_melody(medium)) ).
input_form( [high],                 answer(tone_or_melody(high)) ).
input_form( [mixed],                answer(tone_or_melody(mixed)) ).
input_form( [jumpy],                answer(tone_or_melody(jumpy)) ).
input_form( [nachtmusik],           answer(tone_or_melody(eine_kleine_nachtmusik)) ).
input_form( [toccata],              answer(tone_or_melody(toccata)) ).
input_form( [elise],                answer(tone_or_melody(elise)) ).
input_form( [samba],                answer(tone_or_melody(samba)) ).
input_form( [blues,rythm],          answer(tone_or_melody(blues_rythm)) ).
input_form( [internal],             answer(signal_type(internal)) ) .
input_form( [external],             answer(signal_type(external)) ) .
input_form( [message],              answer(signal_type(message)) ) .
input_form( [search,signal],               answer(signal_type(search_signal)) ) .

input_form( [add|L], answer(phonebook_name_to_add(N)) ) :-
	input_form( L, answer(name(N)) ).

input_form( [delete|L], answer(phonebook_entry_to_delete(N)) ) :-
	input_form( L, answer(name(N)) ).

input_form( [CallDial|L], answer(phonebook_name_to_find(N)) ) :-
	call_word(CallDial),
	input_form( L, answer(name(N)) ).

%input_form( [call],                 answer(call) ).

input_form( [Name], answer(name(Name)) ) :-
	lex_name(Name).

% Common

input_form( [settings],        answer(task_type(settings)) ).
input_form( [domain],          answer(task(main_menu))).
input_form( [switch,on],       answer(onoff(on)) ).
input_form( [switch,off],      answer(onoff(off)) ).
input_form( [switch,it,on],    answer(onoff(on)) ).
input_form( [switch,it,off],   answer(onoff(off)) ).
input_form( [turn,on],         answer(onoff(on)) ).
input_form( [turn,off],        answer(onoff(off)) ).
input_form( [turn,it,on],      answer(onoff(on)) ).
input_form( [turn,it,off],     answer(onoff(off)) ).
input_form( [enable],          answer(onoff(on)) ).
input_form( [disable],         answer(onoff(off)) ).
input_form( [increase],        answer(incdec(increase)) ).
input_form( [decrease],        answer(incdec(decrease)) ).

%input_form( S, M ) :-
%	lexsem( S, C ),
%	input_move( C, M ).

% numbers
%input_form(['Num'],answer(number(N))).

input_form( S, answer(number(N)) ) :-
	number_form( S, N ).

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

program_word(program).
program_word(channel).

call_word(call).
call_word(dial).

/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).

synset( [[vcr],[video],[v,c,r]], vcr ).
synset( [[phone],[telephone]], telephone ).

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

task(T) :-
	domain_vcrphone:task(T).

%language(L) :-
%	domain_vcrphone:language(L).

sublist( SubList, List ) :-
	append( _, Suffix, List ),
	append( SubList, _, Suffix ).

program_position(P) :-
	to_number(P,N),
	integer(N),
	N >= 1,
	N =< 99.

program_number(P) :-
	to_number(P,N),
	integer(N),
	N >= 1,
	N =< 8.

ring_volume(V) :-
	to_number(V,N),
	integer(N),
	N >= 0,
	N =< 7.

earpiece_volume(V) :-
	to_number(V,N),
	integer(N),
	N >= 0,
	N =< 7.

date(today).

date(D) :-
	number_atom(D),
	atom_chars(D,[D1,D2,D3,D4]),
	number_chars(Day,[D1,D2]),
	Day >= 1,
	Day =< 31,
	number_chars(Month,[D3,D4]),
	Month >= 1,
	Month =< 12.

time(T) :-
	number_atom(T),
	atom_chars(T,[T1,T2,T3,T4]),
	number_chars(Hour,[T1,T2]),
	Hour >= 0,
	Hour =< 23,
	number_chars(Min,[T3,T4]),
	Min >= 0,
	Min =< 59.
time(T) :-
	number_atom(T),
	atom_chars(T,[T2,T3,T4]),
	T1=48,
	number_chars(Hour,[T1,T2]),
	Hour >= 0,
	Hour =< 23,
	number_chars(Min,[T3,T4]),
	Min >= 0,
	Min =< 59.

to_number(Atom,Number) :-
	number_atom(Atom),
	atom_chars(Atom,Cs),
	number_chars(Number,Cs).

number_atom(A) :-
	atomic(A),
	\+ number(A).
