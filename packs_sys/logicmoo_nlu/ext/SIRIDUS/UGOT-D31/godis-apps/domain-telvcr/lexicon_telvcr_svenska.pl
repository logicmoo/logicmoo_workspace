 
/*************************************************************************

         name: lexicon_vcr_svenska.pl
      version: 
  description: 
       author: Staffan Larsson, David Hjelm
 
*************************************************************************/

:- module( lexicon_telvcr_svenska, [output_form/2, input_form/2,
				 yn_answer/1, channel/1, weekDays/1,
				 synset/2, lexsem/2, rec_job_listing/2,recJobDate2Str/2]). %jabben changes

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.



:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

:- ensure_loaded( library(lexicon_numbers) ).
:- ensure_loaded( library(digits_svenska) ).
:- ensure_loaded( library(time_svenska) ).
%:- ensure_loaded( library(lexicon_general_english) ).

:- ensure_loaded( library( semsort_telvcr ) ).
:- dynamic channel/1.

%:- multifile( output_form/2 ).
%:- multifile( input_form/2 ).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/

% ask-moves

output_form( ask(X^(action(X))), ['Vad kan jag hjälpa dig med?'] ).

output_form( ask(action(T)), Str ):-
	output_form(action(T), StrT ),
	append( ['Vill du '], StrT, Str0 ),
	append( Str0, ['?'], Str).



output_form( ask(X^channel_to_store(X)),
	     ['Vilken kanal vill du spela in?'] ).
output_form( ask(X^date_to_store(X)),
	     ['Vilken dag?'] ).
output_form( ask(X^start_time_to_store(X)),
	     ['Vilken tid ska inspelningen starta?'] ).
output_form( ask(X^stop_time_to_store(X)),
	     ['Vilken tid ska inspelningen sluta?'] ).
output_form( ask(X^rec_job_to_delete(X)),
	     ['Vilket programnummer vill du radera?'] ).
output_form( ask(action(list_rec_jobs)),
	     ['Vill du lista kommande inspelningar?']).


% action
output_form( action(change_language), ['byta språk'] ).
output_form( action(top), ['gå till början'] ).
output_form( action(add_rec_job), ['lägga till en inspelning'] ).
output_form( action(delete_rec_job), ['ta bort en inspelning'] ).
output_form( action(list_rec_jobs), ['lista planerade inspelningar'] ).

%output_form( icm:acc*neg:action(vcr_add_program,no_available_program_slot), ['There is no available program slot.'] ).

%user help
output_form(answer(usage),['Du kan lägga till en inspelning,',
			  'ta bort en planerad inspelning,',
			  'och lista planerade inspelningar.',
			  'För att börja om, säg börja om.',
			  'För att sluta, säg sluta.']).


output_form( confirm(add_rec_job(Position,Date,Start,Stop)), Cs ) :-
	date_output(Date,DateOutput),
	atom_chars(Start,[Start1,Start2,Start3,Start4]),
	atom_chars(Stop,[Stop1,Stop2,Stop3,Stop4]),
	format_to_chars( 'Ockey, spelar in kanal ~a ~a från ~c~c:~c~c till ~c~c:~c~c.',
			 [Position,DateOutput,
			  Start1,Start2,Start3,Start4,
			  Stop1,Stop2,Stop3,Stop4], Cs ).

%output_form( confirm(add_rec_job), ['Inspelningen har lagts till.'] ).
output_form( confirm(add_rec_job), [' ']). %handled by report instead

output_form( report('AddRecording',ok),['Inspelningen har lagts till.']).
output_form( report('AddRecording',invalid_time_value),['Felaktigt tidsformat.']).
output_form( report('AddRecording',negative_time_difference),
	     ['Starttid måste komma före sluttid. Inspelningen har inte lagts till.']).
output_form( report('AddRecording',non_existing_user),
	     ['Användaren finns inte']).
output_form( report('AddRecording',time_conflict),
	     ['Tyvärr, videon är upptagen vid den tidpunkten']).
output_form( report('AddRecording',non_existing_channel),
	     ['Kanalen finns inte']).

output_form( report('AddRecording',old_start_time),
	     ['Starttiden har redan passerats. Inspelningen har inte lagts till.']).


output_form( report(vcr, rec_jobs([])) , ['Du har inga planerade inspelningar.'] ):-!.
output_form( report(vcr, rec_jobs(Jobs)),['Planerade inspelningar är: '|Listing] ):-
	rec_job_listing(Jobs,Listing).

output_form( report('DeleteRecording', failed(no_such_program)),
	     ['Det finns ingen inspelning på den positionen']).

output_form( answer(rec_jobs([])),['Du har inga planerade inspelningar.']).

output_form( answer(rec_jobs(Jobs)), Cs ) :-
	rec_job_listing( Jobs, Listing ),
	append( ['Planerade inspelningar är: '], Listing, Cs ).

output_form( answer(channels([])),  ['Det finns inga kanaler.']).
output_form( answer(channels(Cs)), Str):-
	channel_listing(Cs,Listing),
	append( ['Tillgängliga kanaler är:  '], Listing, Str ).

output_form( ask(X^rec_job_to_delete(X)), ['Vilket inspelningsnummer vill du radera?'] ).

output_form( icm:acc*neg:action( delete_rec_job, no_rec_job_to_delete ), ['Du har inga planerade inspelningar.'] ).
output_form( confirm(delete_rec_job), ['Ockey. Inspelningen togs bort.'] ).
%output_form( report( 'DeleteProgram', failed(no_such_program)), ['No program is stored in that position.'] ).

output_form( icm:acc*neg:channel_to_store(_), ['Tyvärr, bara dessa kanaler är tillgängliga: blabla'] ).
output_form( icm:acc*neg:rec_job_to_delete(_), ['Det finns ingen inspelning lagrat på den positionen.'] ).

output_form( issue(X^channels(X)),['veta vilka kanaler som finns']).

output_form( issue(X^rec_jobs(X)),['veta vilka dina planerade inspelningar är']).

output_form( greet, ['Välkommen till videon!'] ).
output_form( quit, ['Hej då!'] ).
output_form( Sem,Str):-
	lexsem( Str,Sem).


output_form( ask(X^language(X)), ['Vilket språk vill du använda?'] ).
output_form( ask([language(_)|_]), ['Vill du använda engelska eller svenska?'] ). % HACK

date_output(today,today) :- !.

date_output(Date,DateOutput) :-
	atom_chars(Date,Chars),
	format_to_chars('~c~c/~c~c',Chars,DateOutputCs),
	atom_chars(DateOutput,DateOutputCs).

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

output_form( answer(list_rec_jobs(Jobs)), Str) :-
	rec_job_listing(Jobs,Str).

rec_job_listing(A,B):-
	%start with number one
	rec_job_listing1(1,A,B).

rec_job_listing1(_, [], [] ).

rec_job_listing1(N,[Job],Str):-
	rec_job_listing2(N,Job,Str).

rec_job_listing1(N,[Job,Job2|Jobs],Listing):-
	rec_job_listing2(N,Job,JobStr0),
	N1 is N+1,
	rec_job_listing1(N1,[Job2|Jobs],L0),
	append(JobStr0,[' '|L0],Listing).

rec_job_listing2(N,recJob(_JobId,_,Channel,Start,Stop),Listing):-
	
	recJobDate2Str(Start,StartStr), %start
	recJobDate2Str(Stop,StopStr),
	output_form(Channel,ChannelStr),
	append(['från'|StartStr],['till'|StopStr],TimeStr),
	append(['Nummer',N,': kanal'| ChannelStr],TimeStr,Listing).


recJobDate2Str(Date,[klockan,HrNumber,MinNumber,den,Day,Month]):-
	name(Date,[Mo1,Mo2,D1,D2,H1,H2,Mi1,Mi2]),
	name(MonthNumber,[Mo1,Mo2]), %month
	name(DayNumber,[D1,D2]),
	name(HrNumber,[H1,H2]),
	name(MinNumber,[Mi1,Mi2]),
	months(Months),
	member((Month,MonthNumber),Months),
	cardinal(Day,DayNumber).
	%concatenate

cardinal(första,01).
cardinal(andra,02).
cardinal(tredje,03).
cardinal(fjärde,04).
cardinal(femte,05).
cardinal(sjätte,06).
cardinal(sjunde,07).
cardinal(åttonde,08).
cardinal(nionde,09).
cardinal(tionde,10).
cardinal(elfte,11).
cardinal(tolfte,12).
cardinal(trettonde,13).
cardinal(fjortonde,14).
cardinal(femtonde,15).
cardinal(sextonde,16).
cardinal(sjuttonde,17).
cardinal(årtonde,18).
cardinal(nittonde,19).
cardinal(tionde,20).
cardinal(tjugoförsta,21).
cardinal(tjugoandra,22).
cardinal(tjugotredje,23).
cardinal(tjugofjärde,24).
cardinal(tjugofemte,25).
cardinal(tjugosjätte,26).
cardinal(tjugosjunde,27).
cardinal(tjugoåttonde,28).
cardinal(tjugonionde,29).
cardinal(trettionde,30).
cardinal(trettioförsta,31).


atomList2Str([],[]).
atomList2Str([A|As],[S|Ss]):-
	name(A,S),
	atomList2Str(As,Ss).



channel_listing([],[]).
channel_listing([C],Listing):-!,
	output_form(C,Listing).
channel_listing([C|Cs],Listing):-
	output_form(C,CStr),
	channel_listing(Cs,CsStr),
	append(CStr,[','|CsStr],Listing).


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
input_form( [språk],                  request(change_language) ).
input_form( [engelska],               answer(language(english)) ).
input_form( [svenska],                answer(language(svenska)) ).
input_form( [inglish],                answer(language(english)) ).

input_form( [hjälp], ask(usage) ).

% VCR
input_form( [toppnivå],                 request(top) ).
input_form( [börja,om],                 request(top) ).
input_form( [lägg,till] ,  request(add_rec_job) ).
input_form( [lägg,till,en,inspelning],  request(add_rec_job) ).
input_form( [lägga,till,inspelning],     request(add_rec_job) ).
input_form( [lägga,till,en,inspelning], request(add_rec_job) ).
input_form( [spela,in], request(add_rec_job) ).


input_form( [ta,bort,inspelning],        request(delete_rec_job) ).
input_form( [ta,bort,en,inspelning],    request(delete_rec_job) ).
input_form( [ta,bort,en,inspelning],    request(delete_rec_job) ).
input_form( [ta,bort,inspelning],        request(delete_rec_job) ).
input_form( [radera,inspelning],         request(delete_rec_job) ).
input_form( [radera,en,inspelning],     request(delete_rec_job) ).
input_form( [ta,bort], request(delete_rec_job) ).
input_form( [radera], request(delete_rec_job) ).

input_form( [kanaler],                   ask( X^channels(X) )).
input_form( [vilka,kanaler],            ask( X^channels(X) )).

input_form( [mina,inspelningar], ask(X^rec_jobs(X)) ).
input_form( [lista], ask(X^rec_jobs(X)) ).
input_form( [vilka,inspelningar], ask(X^rec_jobs(X)) ).
input_form( [planerade,inspelningar], ask(X^rec_jobs(X)) ).

% Channel
input_form(  S , answer( channel( C ) ) ) :-
	lexsem( S, C ),
 	sem_sort( C, channel ).

%date
input_form( S, answer(date_to_store(D)) ):-
	lexsem(S, D),
	sem_sort(D, date).

input_form( [ på|S] , answer(date_to_store(D)) ):-
	lexsem(S, D),
	sem_sort(D, date).


% Time
input_form( [ från | [ S1, S2 ] ], answer( start_time_to_store( C ) ) ) :-
	input_form( [ S1, S2 ], answer(time(C)) ),!.

input_form( [ från | [ S1, S2 ,S3] ], answer( start_time_to_store( C ) ) ) :-
	input_form( [ S1, S2 , S3], answer(time(C)) ).

input_form( [ till | [ S1, S2 ] ], answer( stop_time_to_store( C ) ) ) :-
	input_form( [ S1, S2 ], answer(time(C)) ),!.

input_form( [ till | [ S1, S2,S3] ], answer( stop_time_to_store( C ) ) ) :-
	input_form( [ S1, S2, S3 ], answer(time(C)) ).

input_form(Str, answer( time(C)) ):-
	time_form(Str,C),
	sem_sort(C,time).


	



/*----------------------------------------------------------------------
     output_form( +Move, -WordList )
     -- Canned output
----------------------------------------------------------------------*/
% addded 021120 SL, used in positive feedback
output_form( X^channels(X), ['vilka kanaler som finns']):-!.
output_form( X^rec_jobs(X), ['vilka dina inspelningar är']):-!.

% object-level clarification and groundnig questions
output_form( ask(C), Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, ['?'], Output ).



% SL030514
output_form( ask(set([Alt0|Alts])), Output):-
	Alt0 = action(_),!, % if action, then "Vill du..."
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
	append(['Vill du '|Alt0out], AltsOr, Output0 ),
	append(Output0, ['?'], Output).

output_form( ask(set([Alt0|Alts])), Output):-
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
	append(['Menar du '|Alt0out], AltsOr, Output0 ),
	append(Output0, ['?'], Output).

altlist2alts_or( [Alt], ['eller'|OutputAlt] ):-
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
	

output_form( answer(notexist(X,Q)), [' Ledsen, det finns inget som matchar din fråga om'|InputQDot]):-
	input_form( InputQ, ask(X^Q) ),
	append( InputQ, ['.'], InputQDot ).
output_form( answer(unknown(Q)), ['Ledsen, det finns inget som matchar din fråga om'|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% for asking metaissue clarification question
%output_form( issue(Q), ['fråga om'|Out] ):-
%	input_form( Out, ask( Q ) ).

% ICM

% contact
output_form( icm:con*neg, ['Hallå?'] ).


% perception
output_form( icm:per*int, ['Ursäkta?'] ).
output_form( icm:per*int, ['Vad sa du?'] ).
output_form( icm:per*neg, ['Ursäkta, Jag hörde inte vad du sa.'] ).


output_form( icm:per*pos:String, ['Jag tyckte du sa',Name,'.'] ):-
	name( Name, String ).

output_form( icm:sem*int, ['Vad menar du'] ).
output_form( icm:sem*neg, ['Förlåt, jag förstår inte.'] ).
output_form( icm:sem*pos:Move, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).


% understanding(pragmatic)
output_form( icm:und*neg, ['Jag förstår inte riktigt.']  ).

%bugintroduktion...
output_form( icm:und*pos:usr*issue(_),[' ']).

output_form( icm:und*pos:usr*(not issue(Q)), ['Du frågade inte:'|AnsPDot]  ):-
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
	append( IcmPos0, [', är det korrekt?'], Output ).




% clarification question
output_form( icm:und*int:usr*AltQ, Output):-
	output_form( ask(AltQ), Output).



% "acceptance"/integration

% icm-Type(-Polarity(-Args))
output_form( icm:acc*pos, ['Ockey.'] ).

% reject(issue(Q))
output_form( icm:acc*neg:issue(Q), ['Ledsen, jag kan inte svara på frågor om'|InputQDot]):-
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
output_form( icm:accommodate:_, ['Jag antar att du menar']  ).

output_form( icm:reaccommodate:Q, ['Gå tillbaks till'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).



output_form( not C, ['Inte'|S] ):- output_form( C, S ).


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/


	

	

%input_form( S, answer(C) ):- lexsem( S, C ), sem_sort( C, country ).


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


%synset( [[DigitWord]], Digit ):- digit_word( DigitWord, DigitString ), name( Digit, DigitString ).

synset( [NumberPhrase], Number ):- number_phrase( NumberPhrase, Number ).
% dont assert channels dynamically. Must be able to understand non available channels,  
%synset([[Channel]],Channel):- channel(Channel).
synset( [[s,v,t,ett],[t,v,ett],[kanal,ett],[ettan]],   svt1 ).
synset( [[s,v,t,två],[t,v,två],[kanal,två],[tvåan]],   svt2 ).
synset( [[t,v,tre],[trean]], tv3 ). %non available
synset( [[t,v,fyra],[fyran]], tv4 ).
synset( [[t,v,fem],[femman]], tv5 ). %non available
synset( [[t,v,e]],       tve ).
synset( [[rai,uno]],     raiuno ). %abandoning english number pronunciation
synset( [[sat,ains]],     sat1).
synset( [[c,n,n],[si,n,n]],       cnn ).
synset( [[b,b,c,world]], bbcworld ).
synset( [[t,v,polonia],[polonia]], tvpolonia ).
synset( [[d,r,ett]],     dr1 ).
synset( [[d,r,två]],     dr2 ).
synset( [[n,r,k,ett]],   nrk1 ).
synset( [[n,r,k,två]],   nrk2 ).


synset( [[idag], [i,dag]],today ).
synset( [[imorgon], [i,morgon]],tomorrow ).
      
synset( [[måndag]],monday).
synset( [[tisdag]],tuesday).
synset( [[onsdag]],wednesday).
synset( [[torsdag]],thursday).
synset( [[fredag]],friday).
synset( [[lördag]],saturday).
synset( [[söndag]],sunday).


/*-------------------------------------------------
     asserting all channels at compile-time
---------------------------------------------------*/
%D 26/9 dont assert all channels at compile time, use static lexical knowledge

%%assertChannels([]).
%%assertChannels([Ch|Chs]):-
%%	assert(channel(Ch)),
%%	assertChannels(Chs).

%:- oaag:solve(getChannels(Chs)),assertChannels(Chs).

months([('Januari',1),('Februari',2),('Mars',3),('April',4),('Maj',5),('Juni',6),
	('Juli',7),('Augusti',8),('September',9),('Oktober',10),('November',11),('December',12)]).

weekDays([monday,tuesday,wednesday,thursday,friday,saturday,sunday]).

%output_form(_,['missing outputform']).
