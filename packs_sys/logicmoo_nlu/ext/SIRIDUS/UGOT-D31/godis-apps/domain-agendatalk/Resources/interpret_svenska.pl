 
/*************************************************************************
 
         name: interpret_svenska.pl
      version: 
  description: An interpretation file for Swedish and the domain AgendaTalk
       author: Rebeca Jonson
 
*************************************************************************/

%:- discontiguous output_form/3, input_form/2, plan/2, postcond/2.
:-multifile input_form/2.


:- use_module(library(random)).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:- use_module(calendar, [ampm_disamb/3, day2date/2, day2nr/2, month2nr/2]).
:- ensure_loaded(digits_svenska).
%:- ensure_loaded(time_svenska).
:- ensure_loaded(semsort_agendatalk).

/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/
input_form( [nej,inte,S], answer(not(C))):- input_form([S],answer(C)).
input_form( [inte,S], answer(not(C))):- input_form([S],answer(C)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          AgendaTalk answers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


input_form([göra, en, anteckning], request(add_event)).
input_form([anteckna,i,kalendern], request(add_event)).
input_form([jag,vill,anteckna],request(add_event)).
input_form([anteckna],request(add_event)).
input_form([anteckning],request(add_event)).
input_form([lägga,till],request(add_event)).
input_form([lägg,till],request(add_event)).
input_form([boka],request(add_event)).
input_form([boka,in],request(add_event)).
input_form([föra,in],request(add_event)).
input_form([jag,ska,på|Ev],[answer(event_to_store(C)),request(add_event)]):-
	input_form(Ev,answer(event_to_store(C))).
input_form([jag,ska,till|Ev],[answer(event_to_store(C)),request(add_event)]):-
	input_form(Ev,answer(event_to_store(C))).
input_form([jag,har|Ev],[answer(event_to_store(C)),request(add_event)]):-
	input_form(Ev,answer(event_to_store(C))).

input_form([kolla, med, kalendern],request(get_info)).
input_form([kolla, kalendern], request(get_info)).
input_form([fråga, kalendern], request(get_info)).
input_form([jag,vill,kolla],request(get_info)).

input_form([lägga,till, mer, information], request(more_info)).
input_form([lägga, till, information], request(more_info)).
input_form([lägg, till, information], request(more_info)).

input_form([ta, bort,allt],request(delete_current_event)).
input_form([glöm, allt], request(delete_current_event)).

input_form([ta, bort, en, bokning], request(delete_event)).
input_form([ta, bort],request(delete_event)).
input_form([tabort],request(delete_event)).
input_form([radera],request(delete_event)).
input_form([avboka],request(delete_event)).

input_form([ändra, informationen], request(change_info)).
input_form([göra, en, ändring], request(change_date)).
input_form([jag,vill,ändra, datum], request(change_date)).
input_form([jag,vill,ändra, datumet], request(change_date)).
input_form([jag,vill,byta,datum], request(change_date)).
input_form([jag,vill,byta,datumet], request(change_date)).
input_form([jag,vill,flytta], request(change_date)).
input_form([jag,vill,flytta,morgondagens], [request(change_date), answer(date_to_store(tomorrow))]).
input_form([jag,vill,flytta,dagens], [request(change_date), answer(date_to_store(today))]).
%input_form([flytta], request(change_date)).
input_form([jag,vill, göra, en, ändring], request(change_date)).
input_form([ändra, datum], request(change_date)).
input_form([ändra, datumet], request(change_date)).
input_form([ändra, tiden], request(change_time)).
input_form([jag,vill,ändra, tiden], request(change_time)).
input_form([jag,vill,byta,tid], request(change_time)).
input_form([jag,vill,byta,tiden], request(change_time)).
input_form([ändra, tid], request(change_time)).
input_form([byta, tid], request(change_time)).
input_form([byta, tiden], request(change_time)).

input_form([datum], answer(which_info(date))).
input_form([datumet], answer(which_info(date))).
input_form([dag], answer(which_info(date))).
input_form([tid], answer(which_info(time))).
input_form([tiden], answer(which_info(time))).
input_form([lokal], answer(which_info(location))).
input_form([lokalen], answer(which_info(location))).
input_form([annan, tid], [request(change_info), answer(which_info(time))]).
input_form([annat, datum], [request(change_info), answer(which_info(date))]).
input_form([ändra,lokal], [request(change_info), answer(which_info(location))]).
input_form([ändra,lokalen], [request(change_info), answer(which_info(location))]).
%HELP
input_form( [hjälp], ask(usage) ).
input_form( [vad, kan, man, göra], ask(usage)).

input_form( [toppnivå],                 request(top) ).
input_form( [gå,tillbaka,till,toppnivå],                 request(top) ).
input_form( [gå,tillbaks,till,början],                 request(top) ).
input_form( [börja,om,från,början], request(top)).
input_form( [början], request(top)).
input_form( [börja,om], request(top)).
input_form( [starta,om], request(top)).

%%%%%%%%%%%%Check Calendar%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

input_form([vilken, tid],ask(X^start_time_to_store(X))).
input_form([när, är],ask(X^start_time_to_store(X))).
input_form([när, var],ask(X^start_time_to_store(X))).
input_form([när, börjar],ask(X^start_time_to_store(X))).
input_form([kolla, tiden],ask(X^start_time_to_store(X))).
input_form([har,jag,något,bokat],ask(X^event_to_store(X))).
input_form([är,jag,ledig],ask(X^event_to_store(X))).
input_form([är,jag,bokad],ask(X^event_to_store(X))).
input_form([är,jag,upptagen],ask(X^event_to_store(X))).
input_form([är,jag,uppbokad],ask(X^event_to_store(X))).
input_form([har,jag,något,planerat],ask(X^event_to_store(X))).
input_form([hur,ser,schemat,ut],ask(X^bookings(X))).
input_form([vad,gör,jag],ask(X^bookings(X))).
input_form([vad,ska,jag,göra],ask(X^bookings(X))).
input_form([vad,har,jag,bokat],ask(X^bookings(X))).
input_form([vad,har,jag,på,schemat],ask(X^bookings(X))).
input_form([kolla,min,kalender],ask(X^bookings(X))).
input_form([vad,har,jag],ask(X^bookings(X))).
nput_form([vad,händer],ask(X^bookings(X))).
input_form([vad,står,i,min,kalender],ask(X^bookings(X))).
input_form([hur,ser,dagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(today))]).
input_form([hur,ser,morgondagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(tomorrow))]).
input_form([hur,ser,måndagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(monday))]).
input_form([hur,ser,tisdagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(tuesday))]).
input_form([hur,ser,onsdagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(wednesday))]).
input_form([hur,ser,torsdagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(friday))]).
input_form([hur,ser,fredagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(friday))]).
input_form([hur,ser,dagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(today))]).
input_form([hur,ser,morgondagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(tomorrow))]).
input_form([hur,ser,måndagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(monday))]).
input_form([hur,ser,tisdagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(tuesday))]).
input_form([hur,ser,onsdagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(wednesday))]).
input_form([hur,ser,torsdagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(friday))]).
input_form([hur,ser,fredagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(friday))]).
input_form([hur,ser,mitt,schema,ut],ask(X^bookings(X))).
input_form([hur,ser,mitt,program,ut],ask(X^bookings(X))).
input_form([morgondagens,program],ask(X^bookings(X))).
input_form([morgondagens,schema],ask(X^bookings(X))).
input_form([dagens,schema],ask(X^bookings(X))).
input_form([dagens,program],ask(X^bookings(X))).
input_form([har,jag,Ev],[answer(event_to_store(C)),ask(X^start_time_to_store(X))]):- input_form([Ev],answer(event_to_store(C))).

input_form([dagens,datum],ask(X^todaysdate(X))).
input_form([vilken,dag,är,det,idag],ask(X^todaysdate(X))).
input_form([vilket,datum,är,det,idag],ask(X^todaysdate(X))).
input_form([vad,är,det,för,dag,idag],ask(X^todaysdate(X))).
input_form([vad,är,det,för,datum,idag],ask(X^todaysdate(X))).
input_form([vad,är,dagens,datum],ask(X^todaysdate(X))).
%%%%%%%%%%%%%%%%%EVENTS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

input_form( E,	answer(event_to_store(C))):- lexsem(E, C), sem_sort(C, event).
input_form( [ett|E],	answer(event_to_store(C))):- lexsem(E, C), sem_sort(C, event).
input_form( L,	[request(more_info),answer(location(C))] ):-lexsem(L, C), sem_sort(C, location).
%%%Attendees%input_form([med,S1,och,S2],attendee(C1,C2)):- lexsem(S1,C1),lexsem(S2,C1),sem_sort(C1,person),sem_sort(C2,person.)
%input_form([med|S],attendee(C)):- lexsem(S,C),sem_sort(C,person).

%%%DATES%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               DATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
input_form([till|DATE],answer(newdate(D))):-input_form(DATE,answer(date(D))).

input_form( DAY,               answer(date(today)) ):- lexsem(DAY,today).
input_form( DAY,            answer(date(tomorrow)) ):- lexsem(DAY,tomorrow).
input_form( DAY,            answer(date(aftertomorrow)) ):- lexsem(DAY,aftertomorrow).
input_form([nästa,vecka], answer(date([next,week]))).
%%friday the first of july
input_form([Weekday, den, Day, Month], answer(date([Wkday, DayC, MonthC]))):-
	weekDaysSw(WDs),
	member(Weekday, WDs),
	lexsem([Weekday],Wkday),
	dayStr( [Day], DayNr),
	day2nr([DayC],DayNr),
	monthStr([Month],MonthNr),
	month2nr([MonthC],MonthNr).
%%friday first of july
input_form([Weekday, Day, Month], answer(date([Wkday, DayC, MonthC]))):-
	weekDaysSw(WDs),
	member(Weekday, WDs),
	lexsem([Weekday],Wkday),
	dayStr( [Day], Dnr),
	day2nr([DayC],Dnr),
	monthStr([Month],MonthNr),
	month2nr([MonthC],MonthNr).
%% (the) first of july
input_form([den, Day, Month], answer(date([DayC, MonthC]))):-
	dayStr( [Day], Nr),
	day2nr( [DayC],Nr),
	monthStr([Month],MNr),
	month2nr([MonthC],MNr).
%input_form([thirtyfirst, of, april], answer(incons_date([thirtyfirst, april]))).
input_form([Day, Month], answer(date([DayC, MonthC]))):-
	dayStr( [Day], Nr),
	day2nr( [DayC],Nr),
	monthStr([Month],MNr),
	month2nr([MonthC],MNr).

%%the first
input_form([den, Day], answer(date(DayC))):-
	dayStr( [Day], Nr),
	day2nr( [DayC],Nr).
%%friday the second
input_form([Weekday, den, Day], answer(date([Wkday, DayC]))):-
	weekDaysSw(WDs),
	member(Weekday, WDs),
	lexsem([Weekday],Wkday),
	dayStr( [Day], Nr),
	day2nr( [DayC],Nr).

input_form( [på , WeekDayStr, nästa, vecka], answer( date([next,WkDay]))):-
	weekDaysSw(WDs),
	member(WeekDayStr, WDs),
	lexsem([WeekDayStr],WkDay).
%%next friday
input_form( [nästa, WeekDay],             answer(date([next, WkDay])) ):-
	weekDaysSw(WDs),
	member(WeekDay, WDs),
	lexsem([WeekDay],WkDay).

%%friday
input_form( [på , WeekDayStr ], answer( date(WkDay))):-
	weekDaysSw(WDs),
	member(WeekDayStr, WDs),
	lexsem([WeekDayStr],WkDay).

input_form( [WeekDay], answer(date(WkDay)) ):-
	weekDaysSw(WDs),
	member(WeekDay, WDs),
	lexsem([WeekDay],WkDay).
input_form( [WeekDay], answer(date(WkDay)) ):-
	weekDaysSwDef(WDs),
	member(WeekDay, WDs),
	lexsem([WeekDay],WkDay).

%%%%%%%%%%%%%%%%%AMPM%%%%%%%%%%%%%%%%%%%%%%%%%%%%
input_form( [på,förmiddagen], answer(am_or_pm(am))).
input_form( [förmiddagen], answer(am_or_pm(am))).
input_form( [på, morgonen], answer(am_or_pm(am))).
input_form( [morgon], answer(am_or_pm(am))).
input_form( [före,lunch], answer(am_or_pm(am))).
input_form( [på,natten],answer(am_or_pm(am))).
input_form( [på,eftermiddagen], answer(am_or_pm(pm))).
input_form( [eftermiddagen], answer(am_or_pm(pm))).
input_form( [efter,lunch], answer(am_or_pm(pm))).
input_form( [på,kvällen], answer(am_or_pm(pm))).
input_form( [kväll], answer(am_or_pm(pm))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               TIME EXPRESSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Time with ampm disambiguation RJ
input_form([lunchtid],[TIME, answer(am_or_pm(am))]):-input_form([klockan, tolv], TIME).
input_form([vid, midnatt],[TIME, answer(am_or_pm(pm))]):-input_form([klockan, tolv], TIME).
input_form( [ikväll],[answer(date(today)),answer(am_or_pm(pm))]).
input_form( [i,eftermiddag],[answer(date(today)),answer(am_or_pm(pm))]).

input_form([till|S],answer(time(C))):- input_form(S,answer(time(C))).
input_form([tiden,är|S],answer(time(C))):- input_form(S,answer(time(C))).
input_form([tid|S],answer(time(C))):- input_form(S,answer(time(C))).
%På svenska minska tiden med 1 timme
input_form([fem,över,halv|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[trettiofem],S1S2),
	input_form(S1S2, answer(time(C2))).
input_form([fem,i,halv|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[tjugofem],S1S2),
	input_form(S1S2, answer(time(C2))).

input_form([klockan, halv|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[trettio],S1S2),
	input_form(S1S2, answer(time(C2))).
input_form([halv|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[trettio],S1S2),
	input_form(S1S2, answer(time(C2))).

input_form([kvart,över|S1],answer(time(C))):-	
	append(S1,[femton],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([kvart,i|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[fyrtiofem],S1S2),
	input_form(S1S2, answer(time(C2))).


input_form([tio,över|S1],answer(time(C))):-	
	append(S1,[tio],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([tio,i|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[femtio],S1S2),
	input_form(S1S2, answer(time(C2))).

input_form([fem,över|S1],answer(time(C))):-	
	append(S1,[fem],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([fem,i|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[femtiofem],S1S2),
	input_form(S1S2, answer(time(C2))).
input_form([tjugo,över|S1],answer(time(C))):-	
	append(S1,[tjugo],S1S2),
	input_form(S1S2, answer(time(C))).
input_form([tjugo,i|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[fyrtio],S1S2),
	input_form(S1S2, answer(time(C2))).
input_form([klockan, S1, och, S2], [answer(time(C)), answer(am_or_pm(pm))]):-
	input_form([S1,S2], answer(time(C))), 
	calendar:dayhalf(C,pm).

input_form([S1, S2], [answer(time(C)), answer(am_or_pm(pm))]):-
	input_form([S1,S2], answer(time(C))), 
	calendar:dayhalf(C,pm).

input_form([klockan |[ S1,S2]], [answer(time(C)), answer(am_or_pm(pm))]):-
	input_form([S1,S2], answer(time(C))),
	calendar:dayhalf(C, pm).
input_form([klockan,S1,och,S2], answer(time(C))):-
	input_form([S1,S2], answer(time(C))).
input_form([klockan |[S1,S2]], answer(time(C))):-
	input_form([S1,S2], answer(time(C))).
	%sem_sort(C,_).
	
input_form([klockan| S1], [answer(time(C)),answer(am_or_pm(pm))]):-
	append(S1, [noll], S1S2),
	input_form(S1S2, answer(time(C))),
	calendar:dayhalf(C, pm).
input_form([klockan| S1], [answer(time(C)),answer(am_or_pm(pm))]):-
	append(S1, [noll], S1S2),
	input_form(S1S2, answer(time(C))),
	calendar:timeknowledge(C, pm).
input_form([klockan| S1], [answer(time(C)),answer(am_or_pm(am))]):-
	append(S1, [noll], S1S2),
	input_form(S1S2, answer(time(C))),
	calendar:timeknowledge(C, am).

input_form([klockan| S1], answer(time(C))):-
	append(S1, [noll], S1S2),
	input_form(S1S2 ,answer(time(C))).


% digital time: two numbers in sequence
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
%%To help out with generation
input_form(S,answer(newtime(T))):- input_form(S,answer(time(T))).
input_form(S,answer(olddate(T))):- input_form(S,answer(date(T))).
/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

% use semantics as surface forms (only possible for english???)
lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).

%AgendaTalk
synset([[möte],[mötet],[affärsmöte],[lunchmöte]], meeting).
synset([[middag],[middagen],[kvällsmat],[bjudning]], dinner).
synset([[lunch],[lunchen],[affärslunch]], lunch).
synset([[träff],[dejt],[träffen]], appointment).
synset([[konferens], [konferensen], [seminarie]], conference).
synset([[fika],[fikat]],coffee).
synset([[tandläkarbesök],[tandläkaren],[tandläkartid],[tandläkartiden]],dentist).
synset([[klipptid],[frissan],[frisören],[klippning],[hårfrisörskan]],haircut).
synset([[fest],[festen],[kalas],[party],[partyt]],party).
synset([[bio],[biobesök],[bion]],movie).
synset([[handla],[shoppa],[shopping]],shopping).
synset([[resa],[flygresa],[tågresa],[flyget], [resan], [resa,bort]],trip).
synset([[träning],[yoga],[workout],[simning],[jogging],[vattengympa], [vattengympan], [taichi],[aerobic],[gymma],[styrketräna],[träningen],[styrketräningen],[yogan]],training).
synset([[presentation],[presentationen]], presentation).
synset([[föreläsning],[lektion],[lektionen],[föreläsningen]],lecture).
synset([[laboration],[labb],[laborationen],[labben]],lab).
synset([[tenta],[tentan],[skrivning]], exam).
synset([[inlämning],[inlämningen]], deadline).
sysnset([[konsert],[konserten]], concert).

synset([[på,skolan],[i,skolan]],school).
synset([[på,mitt,rum],[i,mitt,rum],[på,mitt,kontor],[på,rummet]],myroom).
synset([[i,akvariet]],aquarium).
synset([[på,kontoret],[på,jobbet]],office).
synset([[i,dialoglabbet],[på,lindholmen]],dialoglab).
synset([[i,kafeet],[på,kafeet]],cafe).
synset([[på,mässan]],fair).
synset([[hemma],[hemma,hos,mig]],home).
synset([[sal,g,tre,hundra,tolv]],roomG312).
synset([[sal,f,tre,hundra,fjorton]],roomF314).
synset([[maclabbet]],maclab).

%synset([[rum, NR]], Room):- number_phrase(NR,Number), append("rum", Number,Room).
synset( [[idag], [i,dag]],today ).
synset( [[imorgon], [i,morgon], [imorrn]],tomorrow ).
synset( [[iövermorgon], [i,övermorgon]],aftertomorrow ).
     
synset( [[måndag],[måndagens]],monday).
synset( [[tisdag],[tisdagens]],tuesday).
synset( [[onsdag],[onsdagens]],wednesday).
synset( [[torsdag],[torsdagens]],thursday).
synset( [[fredag],[fredagens]],friday).
synset( [[lördag],[lördagens]],saturday).
synset( [[söndag],[söndagens]],sunday).

synset( [NumberPhrase], Number ):- number_phrase( NumberPhrase, Number ).
weekDaysSw([måndag, tisdag, onsdag, torsdag, fredag, lördag, söndag]).
weekDaysSwDef([måndagens, tisdagens, onsdagens, torsdagens, fredagens, lördagens, söndagens]).






