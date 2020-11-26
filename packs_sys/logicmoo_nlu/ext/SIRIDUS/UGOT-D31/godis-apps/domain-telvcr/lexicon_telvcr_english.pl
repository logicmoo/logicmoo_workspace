 
/*************************************************************************
 
         name: lexicon_vcr_english.pl 
      version: 
  description: 
       author: Staffan Larsson
 
*************************************************************************/

:- module( lexicon_telvcr_english, [output_form/2, input_form/2,
				 yn_answer/1, channel/1, synset/2, lexsem/2, rec_job_listing/2,recJobDate2Str/2, weekDays/1]). %jabben changes

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.



:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

:- ensure_loaded( library(lexicon_numbers) ).
:- ensure_loaded( library(digits_english) ).

:- ensure_loaded( library( semsort_telvcr ) ).

:- dynamic channel/1.

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




output_form( ask(X^channel_to_store(X)),
	     ['What channel do you want?'] ).
output_form( ask(X^date_to_store(X)),
	     ['What date?'] ).
output_form( ask(X^start_time_to_store(X)),
	     ['What time do you want to start recording?'] ).
output_form( ask(X^stop_time_to_store(X)),
	     ['What time do you want to stop recording?'] ).
output_form( ask(X^rec_job_to_delete(X)),
	     ['What recording do you want to delete?'] ).
output_form( ask(action(list_rec_jobs)),
	     ['Do you want to list scheduled recordings']).

% action

output_form( action(top), ['restart'] ).
output_form( action(add_rec_job), ['add a recording'] ).
output_form( action(delete_rec_job), ['delete a recording'] ).
output_form( action(list_rec_jobs), ['list scheduled recordings'] ).


%output_form( icm:acc*neg:action(vcr_add_program,no_available_program_slot), ['There is no available program slot.'] ).

%user help
output_form(answer(usage),['You can add recordings, delete recordings,',
			  'and list scheduled recordings.',
			  'To start over, say top.',
			  'To quit, say quit.']).

output_form( confirm(add_rec_job(Position,Date,Start,Stop)), Cs ) :-
	date_output(Date,DateOutput),
	atom_chars(Start,[Start1,Start2,Start3,Start4]),
	atom_chars(Stop,[Stop1,Stop2,Stop3,Stop4]),
	format_to_chars( 'OK, I will record channel ~a ~a from ~c~c:~c~c to ~c~c:~c~c.',
			 [Position,DateOutput,
			  Start1,Start2,Start3,Start4,
			  Stop1,Stop2,Stop3,Stop4], Cs ).

output_form( confirm(add_rec_job), ['The recording has been added.'] ).

output_form( report(vcr, rec_jobs([])) , ['You have no scheduled recordings'] ):-!.
output_form( report(vcr, rec_jobs(Jobs)),['Your scheduled recordings are: '|Listing] ):-
	rec_job_listing(Jobs,Listing).

output_form( answer(recjobs([])), ['There are no scheduled recordings.']).
output_form( answer(rec_jobs(Programs)), Cs ) :-
	rec_job_listing( Programs, Listing ),
	append( ['Your scheduled recordings are: '], Listing, Cs ).

output_form( answer(no_rec_jobs), ['There are no scheduled recordings.']).

output_form( answer(channels([])),  ['There are no available channels.']).
output_form( answer(channels(Cs)), Str):-
	channel_listing(Cs,Listing),
	append( ['The available channels are: '], Listing, Str ).

output_form( ask(X^program_to_delete(X)), ['Which program number to you want to delete?'] ).

output_form( icm:acc*neg:action( delete_rec_job, no_rec_job_to_delete ), ['There is no recording to delete.'] ).
output_form( confirm(delete_rec_job), ['OK. The recording was deleted.'] ).
%output_form( report( 'DeleteProgram', failed(no_such_program)), ['No program is stored in that position.'] ).


output_form( icm:acc*neg:rec_job_to_delete(_), ['No program is stored in that position.'] ).
output_form( icm:acc*neg:channel_to_store(C), Str):-
	append(['The channel'|ChStr],['is not available.'],Str ),
	output_form(C,ChStr).


output_form( greet, ['Welcome to the VCR manager!'] ).
output_form( quit, ['Good bye!'] ).

%lexical semantics (as in input_form).
output_form( Sem,Str):-
	lexsem( Str,Sem).


date_output(today,today) :- !.

date_output(Date,DateOutput) :-
	atom_chars(Date,Chars),
	format_to_chars('~c~c/~c~c',Chars,DateOutputCs),
	atom_chars(DateOutput,DateOutputCs).


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


output_form( answer(list_rec_jobs(Jobs)), Str) :-
	rec_job_listing(Jobs,Str).

output_form( answer(rec_jobs(Jobs)), Str) :-
	rec_job_listing(Jobs,Str).

rec_job_listing([],[]).
rec_job_listing([recJob(JobId,_User,Channel,Start,Stop)|Jobs],Str ):-
	output_form(jobId(JobId),JobIdStr),
	output_form(channel(Channel),ChannelStr),
	output_form(from(dateandtime(Start)),StartStr),
	output_form(until(dateandtime(Stop)),StopStr),	
	append(['Number'],JobIdStr,Str0),
	append(Str0,ChannelStr,Str1),
	append(Str1,StartStr,Str2),
	append(Str2,StopStr,Str).		    


channel_listing([],[]).
channel_listing([C|Cs],Listing):-
	output_form(C,CStr),
	channel_listing(Cs,CsStr),
	append(CStr,CsStr,Listing).

%tills vidare
output_form(jobId(JobId),[JobId]).


%output_form(channel(Channel),ChannelStr) %already covered

output_form(from(T),S):-
	append(['from'],Time,S);
	dateandtimeform(T,Time).

output_form(until(T),S):-
	append(['until'],Time,S);
	dateandtimeform(T,Time).

%tills vidare
dateandtimeform(DT,[DT]).


/*

weekDays([('monday',1),('tuesday',2),('wednesday',3),('thursday',4),('friday',5),('saturday',6),('sunday',7)]).
months([('Jan',1),('Febr',2),('Mar',3),('Apr',4),('May',5),('Jun',6),
	('Jul',7),('Aug',8),('Sep',9),('Oct',10),('Nov',11),('Dec',12)]).
*/




recJobDate2Str(Date,Str):-
	name(Date,[Mo1,Mo2,D1,D2,H1,H2,Mi1,Mi2]),
	name(MonthNumber,[Mo1,Mo2]), %month
	months(Months),
	member((Month,MonthNumber),Months),
	%concatenate
	name(Month,MonthStr),
	Time = [H1,H2,58,Mi1,Mi2],
	(D1=48 ->
	    Day = [D2]
	    ;
	    Day = [D1,D2]
	),
	append(MonthStr,[32|Day], Str1),
	append(Str1, [32|Time], Str).



atomList2Str([],[]).
atomList2Str([A|As],[S|Ss]):-
	name(A,S),
	atomList2Str(As,Ss).



channel_listing(  [], "" ).
channel_listing( [C|Cs],[S|Ss] ):-
	C = S,
	channel_listing(Cs,Ss).


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

% VCR
input_form( [help], ask(usage) ).

input_form( [top],                 request(top) ).
input_form( [add],                 request(add_rec_job) ).
input_form( [add,recording],       request(add_rec_job) ).
input_form( [add,a,recording],     request(add_rec_job) ).
input_form( [delete],              request(delete_rec_job) ).
input_form( [delete,recording],    request(delete_rec_job) ).
input_form( [delete,a,recording],  request(delete_rec_job) ).
input_form( [list,recordings],     request(list_rec_jobs) ).
input_form( [list,my,recordings],  request(list_rec_jobs) ).
input_form( [list],                request(list_rec_jobs) ).

input_form( [today],               answer(date_to_store(today)) ).


%% Jacob 2002100
input_form( [tomorrow],            answer(date_to_store(tomorrow)) ).
input_form( [WeekDay],             answer(date_to_store(WeekDay)) ):-
	weekDays(WDs),
	member(WeekDay, WDs).
%% J

input_form( [channels],                          ask( X^channels(X) )).
input_form( [what,channels],                     ask( X^channels(X) )).
input_form( [show,my,recordings],                ask( X^rec_jobs(X) )).
input_form( [which,are,my,scheduled,recordings], ask( X^rec_jobs(X) )).


% Time
input_form( [ from | [ S1, S2 ] ], answer( start_time_to_store( C ) ) ) :-
	input_form( [ S1, S2 ], answer(time(C)) ),!.

input_form( [ from | [ S1, S2 ,S3] ], answer( start_time_to_store( C ) ) ) :-
	input_form( [ S1, S2 , S3], answer(time(C)) ).

input_form( [ until | [ S1, S2 ] ], answer( stop_time_to_store( C ) ) ) :-
	input_form( [ S1, S2 ], answer(time(C)) ),!.

input_form( [ until | [ S1, S2,S3] ], answer( stop_time_to_store( C ) ) ) :-
	input_form( [ S1, S2, S3 ], answer(time(C)) ).


% Channel
input_form( [ channel | S ], answer( channel( C ) ) ) :-
	lexsem( S, C ),
 	sem_sort( C, channel ).


%% Jacob 20021008
% Date
input_form( [on | WeekDayStr ], answer( date_to_store(WeekDay))):-
	  input_form( WeekDayStr, answer(date_to_store(WeekDay))).
%% J


% time: two numbers in sequence
input_form( [S1,S2] , answer( time( C ) ) ) :-
	!,
	lexsem( [S1], C1 ),
	sem_sort( C1, number ),
	lexsem( [S2], C2 ),
	sem_sort( C2, number ),
	name( C1, C1S ),
	name( C2, C2S ),
	(C2S=[_,_] ->
	    C3S=C2S
	;
	    append([48],C2S,C3S)
	),
	append( C1S, C3S, CS ),
	name( C, CS ),
	sem_sort( C, time ).


% time: three numbers in sequence
input_form( [S1,S2,S3] , answer( time( C ) ) ) :-
	lexsem( [S1], C1 ),
	sem_sort( C1, number ),
	lexsem( [S2,S3], C2 ),
	sem_sort( C2, number ),
	name( C1, C1S ),
	name( C2, C2S ),
	append( C1S, C2S, CS),
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

synset( [[vcr],[video],[v,c,r]], vcr ).

%synset( [[DigitWord]], Digit ):- digit_word( DigitWord, DigitString ), name( Digit, DigitString ).

synset( [NumberPhrase], Number ):- number_phrase( NumberPhrase, Number ).

% dont assert channels dynamically. Must be able to understand non available channels,  
%synset([[Channel]],Channel):- channel(Channel).

%more synonyms will be added in swedish version.
%abbrs are expanded, 'svt' -> 's v t' but not 'es vee tee'
%why not use capitals? nuance terminals must be lowercase.
%why not use numbers? nuance can not handle numbers in swedish grammars (experience by failure)
synset( [[s,v,t,one]],   svt1 ).
synset( [[s,v,t,two]],   svt2 ).
synset( [[t,v,three]],   tv3 ). %non available
synset( [[t,v,four]],    tv4 ).
synset( [[t,v,five]],    tv5 ). %non available
synset( [[t,v,e]],       tve ).
synset( [[rai,uno]],     raiuno ). %abandoning english number pronunciation
synset( [[sat,one]],     sat1).
synset( [[c,n,n]],       cnn ).
synset( [[b,b,c,world]], bbcworld ).
synset( [[t,v,polonia]], tvpolonia ).
synset( [[d,r,one]],     dr1 ).
synset( [[d,r,two]],     dr2 ).
synset( [[n,r,k,one]],   nrk1 ).
synset( [[n,r,k,two]],   nrk2 ).
% and a lot of other channels eventually. 


%
/*
synset( [monday],).
synset( [tuesday],).
synset( [wednesday],).
synset( [thursday],).
synset( [friday],).
synset( [saturday],).
synset( [sunday],).
*/

weekDays([monday,tuesday,wednesday,thursday,friday,saturday,sunday]).



/*-------------------------------------------------
     asserting all channels at compile-time
---------------------------------------------------*/
%D 26/9 dont assert all channels at compile time, use static lexical knowledge


% assertChannels([]).
% assertChannels([Ch|Chs]):-
% 	assert(channel(Ch)),
% 	assertChannels(Chs).

% :- oaag:solve(getChannels(Chs)),assertChannels(Chs).



%output_form(_,['missing outputform']).
