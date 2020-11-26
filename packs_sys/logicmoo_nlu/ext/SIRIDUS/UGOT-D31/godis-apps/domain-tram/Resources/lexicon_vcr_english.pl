 
/*************************************************************************

         name: lexicon_vcr_english.pl 
      version: 
  description: 
       author: Staffan Larsson
 
*************************************************************************/

:- module( lexicon_vcr_english, [resource_of_type/1,
				 output_form/2, input_form/2,
				 yn_answer/1]).

resource_of_type(lexicon).


:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.



:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

:- ensure_loaded( library(lexicon_numbers) ).
:- ensure_loaded( library(digits_english) ).
%:- ensure_loaded( library(lexicon_general_english) ).

:- ensure_loaded( library( semsort_vcr ) ).


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


output_form( ask(X^new_channel(X)),
	     ['What channel do you want?'] ).

output_form( ask(X^channel_to_store(X)),
	     ['What channel do you want?'] ).
output_form( ask(X^date_to_store(X)),
	     ['What date?'] ).
output_form( ask(X^start_time_to_store(X)),
	     ['What time do you want to start recording?'] ).
output_form( ask(X^stop_time_to_store(X)),
	     ['What time do you want to stop recording?'] ).

output_form( ask(X^program_to_delete(X)),
	     ['What program to want to delete?'] ).

output_form( ask(X^new_clock(X)),
	     ['What time?'] ).

% action

output_form( action(change_language), ['change language'] ).
output_form( action(change_domain), ['change domain'] ).

output_form( action(vcr_top), ['go the VCR manager'] ).
output_form( action(vcr_change_play_status), ['change play status'] ).
output_form( action(vcr_timer_recording), ['go to timer recording'] ).
output_form( action(vcr_settings), ['go to settings'] ).
%output_form( action(vcr_query), ['make a query'] ).
output_form( action(vcr_new_channel), ['change channel'] ).
output_form( action(vcr_play), ['play'] ).
output_form( action(vcr_stop), ['stop'] ).
output_form( action(vcr_ff), ['fast-forward'] ).
output_form( action(vcr_rew), ['rewind'] ).
output_form( action(vcr_pause_still), ['pause'] ).
output_form( action(vcr_still_adv), ['advance'] ).
output_form( action(vcr_rec), ['record'] ).
output_form( action(vcr_add_program), ['add a program'] ).
output_form( action(vcr_add_program1), ['add a program'] ).
output_form( action(vcr_delete_program), ['delete a program'] ).
output_form( action(vcr_delete_program1), ['delete a program'] ).
output_form( action(vcr_set_clock), ['set the clock'] ).
%output_form( action(vcr_query_status), ['query the current play status'] ).
%output_form( action(vcr_query_channel), ['find out what channel is on'] ).

% special SL021125
output_form( stop_time_to_store(T), [until|S]):-
	output_form(time(T), S).
output_form( start_time_to_store(T), [from|S]):-
	output_form(time(T), S).


output_form( action(vcr_turnontv), ['turn on the t v'] ).
output_form( action(vcr_turnofftv), ['turn off the t v'] ).

output_form( icm:acc*neg:action(vcr_turnofftv), ['Sorry, I cannot control the TV set.'] ).
output_form( icm:acc*neg:action(vcr_turnontv), ['Sorry, I cannot control the TV set.'] ).

output_form( icm:acc*neg:action(vcr_play,already), ['The VCR is already playing.'] ).
output_form( confirm(vcr_play), ['The VCR is now playing.'] ).
output_form( icm:acc*neg:action(vcr_stop, already), ['The VCR is already stopped.'] ).
output_form( confirm(vcr_stop), ['The VCR is now stopped.'] ).
output_form( icm:acc*neg:action(vcr_ff, already), ['The VCR is already fast-forwarding.'] ).
output_form( confirm(vcr_ff), ['The VCR is now fast-forwarding.'] ).
output_form( icm:acc*neg:action(vcr_rewind, already), ['The VCR is already rewinding.'] ).
output_form( confirm(vcr_rew), ['The VCR is now rewinding.'] ).
output_form( icm:acc*neg:action(vcr_pause_still, already), ['The VCR is already paused.'] ).
output_form( confirm(vcr_pause_still), ['The VCR is now paused.'] ).
output_form( icm:acc*neg:action(vcr_rec, already), ['The VCR is already recording.'] ).
output_form( confirm(vcr_rec), ['The VCR is now recording.'] ).

output_form( confirm(vcr_still_adv), ['The VCR is forwarding slowly.'] ).%??
output_form( answer(not(vcr_paused)), ['The VCR is not paused.'] ).
output_form( icm:acc*neg:action(vcr_add_program,no_available_program_slot), ['There is no available program slot.'] ).
output_form( confirm(vcr_set_clock), ['The clock was set.'] ).

output_form( confirm(vcr_change_channel(P)), Cs ) :-
	format_to_chars( 'OK. Switched to channel ~a.\n', [P], Cs ).
output_form( confirm(vcr_new_channel), ['The channel was changed'] ).

output_form( confirm(vcr_add_program(Position,Date,Start,Stop)), Cs ) :-
	date_output(Date,DateOutput),
	atom_chars(Start,[Start1,Start2,Start3,Start4]),
	atom_chars(Stop,[Stop1,Stop2,Stop3,Stop4]),
	format_to_chars( 'OK, I will record channel ~a ~a from ~c~c:~c~c to ~c~c:~c~c.',
			 [Position,DateOutput,
			  Start1,Start2,Start3,Start4,
			  Stop1,Stop2,Stop3,Stop4], Cs ).

output_form( confirm(vcr_add_program), ['The program has been added.'] ).

output_form( answer(programs(Programs)), Cs ) :-
	program_listing( Programs, Listing ),
	append( 'The current programs are\n', Listing, Cs ).

output_form( ask(X^program_to_delete(X)), ['Which program number to you want to delete?'] ).

output_form( icm:acc*neg:action( vcr_delete_program, no_program_to_delete ), ['There is no program to delete.'] ).
output_form( confirm(vcr_delete_program), ['OK. The program was deleted.'] ).
%output_form( report( 'DeleteProgram', failed(no_such_program)), ['No program is stored in that position.'] ).

output_form( icm:acc*neg:program_to_delete(_N), ['No program is stored in that position.'] ).



output_form( icm:acc*neg:new_channel(_), ['Sorry, the available channels are 1 to 99.'] ).


output_form( answer(play_status(S)), ['The VCR is ',SO] ) :-
	status_output(S,SO).

output_form( answer(not(play_status(S))), ['No, the VCR is not ',SO] ) :-
	status_output(S, SO).

%output_form( answer(not(play_statusstatus(S))), Cs ) :-
%	status_output(S,SO),
%	format_to_chars( 'The VCR is not ~a.', [SO], Cs ).

%output_form( answer(current_channel(P)), Cs ) :-
%	format_to_chars( 'The current channel is ~a.', [P], Cs ).
output_form( answer(current_channel(P)), ['The current channel is', P ] ).



output_form( greet, ['Welcome to the VCR manager!'] ).
output_form( quit, ['Good bye!'] ).


output_form( new_clock(T), S ):-
	output_form( time(T), S );
	input_form( S, answer(time(T)) ).

%output_form( start_time_to_store(T), [from,S]):-
%	output_form(time(T), S).

% o'clock case for time output (others handled by inputform)
% SL 021125
output_form( time(T), [S]):-
	name( T, TS),
	TS = [HS,48,48],
	append( [HS], " o clock",SS),
	name(S, SS).



output_form( ask(X^language(X)), ['What language do you want to use?'] ).
output_form( ask([language(_X)|_]), ['Do you want to use English or Swedish?'] ). % HACK

%SL 021125
output_form( ask(X^domain(X)), ['What domain do you want?'] ).
output_form( ask([domain(_X)|_]), ['Do you want to use the VCR or the telephone?'] ). % HACK


date_output(today,today) :- !.

date_output(Date,DateOutput) :-
	atom_chars(Date,Chars),
	format_to_chars('~c~c/~c~c',Chars,DateOutputCs),
	atom_chars(DateOutput,DateOutputCs).

status_output(ff,'fast-forwarding') :- !.
status_output(S,S).

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

program_listing( [], "" ).

program_listing( [N:(Position,Date,Start,Stop)|Programs], Listing ) :-
	atom_chars(Date,[Date1,Date2,Date3,Date4]),
	atom_chars(Start,[Start1,Start2,Start3,Start4]),
	atom_chars(Stop,[Stop1,Stop2,Stop3,Stop4]),
	format_to_chars( 'Number ~d: position ~a, ~c~c/~c~c, from ~c~c:~c~c to ~c~c:~c~c;\n',
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


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

% generic

input_form( [go,up],           request(up) ).
input_form( [language],        request(change_language) ).
input_form( [english], answer(language(english)) ).
input_form( [swedish], answer(language(svenska)) ).
input_form( [svenska], answer(language(svenska)) ).

% SL021125
input_form( [domain], request(change_domain) ).
input_form( [telephone], answer(domain(telephone)) ).
input_form( [vcr], answer(domain(vcr)) ).

% VCR


%input_form( [top],             request(vcr_top) ).
input_form( [top],             request(top) ).
input_form( [play,status],     request(vcr_change_play_status) ).
input_form( [timer,recording], request(vcr_timer_recording) ).
input_form( [add,a,program],   request(vcr_add_program) ).
input_form( [add,program],     request(vcr_add_program) ).
input_form( [delete,program],  request(vcr_delete_program) ).
input_form( [delete,a,program],request(vcr_delete_program) ).
input_form( [settings],        request(vcr_settings) ).
input_form( [clock],           request(vcr_set_clock) ).
% this will be rejected by system
input_form( [turn,off,the,t,v],           request(vcr_turnofftv) ).
input_form( [turn,on,the,t,v],           request(vcr_turnontv) ).

input_form( [change,channel],  request(vcr_new_channel ) ).
input_form( [switch,channel],  request(vcr_new_channel ) ).
input_form( [switch,to],  request(vcr_new_channel ) ).
input_form( [specific,channel],request(vcr_new_channel ) ).

input_form( [increase,channel], [ request(vcr_increase_channel) ] ).
input_form( [next,channel],  [ request(vcr_increase_channel) ] )  .

input_form( [decrease,channel], [ request(vcr_decrease_channel ) ] ).
input_form( [previous,channel], [ request(vcr_decrease_channel ) ] ).

input_form( [play],            request(vcr_play) ).
input_form( [stop],            request(vcr_stop) ).
input_form( [forward],         request(vcr_ff) ).
input_form( [rewind],          request(vcr_rew) ).
input_form( [pause],           request(vcr_pause_still) ).
input_form( [advance],         request(vcr_still_adv) ).
input_form( [record],          request(vcr_rec) ).

input_form( [today],           answer(date_to_store(today)) ).

input_form( [playing],         ask( play_status( playing ) ) ).
input_form( [paused],          ask( play_status( paused ) ) ).
input_form( [stopped],         ask( play_status( stopped ) ) ).
input_form( [rewinding],       ask( play_status( rewinding ) ) ).
input_form( [forwarding],      ask( play_status( ff ) ) ).
input_form( [recording],       ask( play_status(recording) ) ).
input_form( [vcr,doing],       ask( X^play_status(X) ) ).

input_form( [what,channel],    ask(X^current_channel(X)) ).
input_form( [which,channel],   ask(X^current_channel(X)) ).

input_form( [ from | [ S1, S2 ] ], answer( start_time_to_store( C ) ) ) :-
	input_form( [ S1, S2 ], answer(time(C)) ).

input_form( [ until | [ S1, S2 ] ], answer( stop_time_to_store( C ) ) ) :-
	input_form( [ S1, S2 ], answer(time(C)) ).
input_form( [ to | [ S1, S2 ] ], answer( stop_time_to_store( C ) ) ) :-
	input_form( [ S1, S2 ], answer(time(C)) ).

%input_form( [ switch, to, channel | S ], answer( new_channel( C ) ) ) :-
%	lexsem( S, C ),
%	sem_sort( C, channel ).

% ???
%input_form( [ record, channel | L ], answer( channel_to_store(P)) ) :-
%	lexsem( S, C ),
%sem_sort( C, channel ).

input_form( [ channel | S ], answer( channel( C ) ) ) :-
	lexsem( S, C ),
%	sem_sort( C, channel ).
	sem_sort( C, number ).



input_form( [ set, the, clock, to | [ S1, S2 ] ], [ request( vcr_set_clock ), answer( new_clock( C ) ) ] ) :-
	input_form( [ S1, S2 ], answer(time(C)) ).


% time: two numbers in sequence

input_form( [S1,S2] , answer( time( C ) ) ) :-
	lexsem( [S1], C1 ),
	sem_sort( C1, number ),
	lexsem( [S2], C2 ),
	sem_sort( C2, number ),
	name( C1, C1S ),
	name( C2, C2S ),
	append( C1S, C2S, CS ),
	name( C, CS ),
	sem_sort( C, time ).

% time: X o'clock
input_form( [S1,oclock] , answer( time( C ) ) ) :-
	lexsem( [S1], C1 ),
	sem_sort( C1, number ),
	name( C1, C1S ),
	append( C1S, "00", CS ),
	name( C, CS ),
	sem_sort( C, time ).


% numbers

input_form( S, answer( number( C ) ) ) :-
	lexsem( S, C ),
	sem_sort( C, number ).



/*----------------------------------------------------------------------
     output_form( +Move, -WordList )
     -- Canned output
----------------------------------------------------------------------*/

% addded 021120 SL, used in positive feedback
output_form( X^play_status(X), ['what the vcr is doing']):-!.
output_form( play_status(playing), ['whether the vcr is playing']):-!.
output_form( play_status(stopped), ['whether the vcr is stopped']):-!.

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

%output_form( answer( db_entry( set( List )  ) ), Output ):-
%	output_forms( List, Output ).

output_forms( [], [] ).
output_forms( [ Move | Moves ], Output1 ):-
	output_form( Move, Output ),
	output_forms( Moves, Outputs ),
	append( Output, Outputs, Output1 ).
	


				%
				%output_form( answer( db_entry( PropList, P ) ), Output ):-
%	output_forms( PropList, PropOutput ),
%	output_form( answer( P ), AnsOutput ),
%	append( PropOutput, ['.'.


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

output_form( icm:per*pos:String, ['I heard you say',Name,'. '] ):-
	name( Name, String ).

output_form( icm:sem*int, ['What do you mean'] ).
output_form( icm:sem*neg, ['Sorry, I dont understand.'] ).
output_form( icm:sem*pos:Move, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).



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

synset( [[vcr],[video],[v,c,r]], vcr ).

%synset( [[DigitWord]], Digit ):- digit_word( DigitWord, DigitString ), name( Digit, DigitString ).

synset( [NumberPhrase], Number ):- number_phrase( NumberPhrase, Number ).





