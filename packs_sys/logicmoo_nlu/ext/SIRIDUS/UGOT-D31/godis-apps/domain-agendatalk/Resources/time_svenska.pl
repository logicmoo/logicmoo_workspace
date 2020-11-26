
%time_form(Str,Sem).
% e.g. time_form([kvart,i,ett],'0045').

time_form([klockan|Str],Sem):-
	time(Sem,Str,[]).

time_form(Str,Sem):-
	time(Sem,Str,[]).

time(Hour) -->
	hour(H),{atom_concat(H,'00',Hour)}.

time(Time) -->
	hour(Hour),
	minutes(Mins),{atom_concat(Hour,Mins,Time)}.

time(Time) -->
	hour(Hour),
	[och],
	minutes(Mins),{atom_concat(Hour,Mins,Time)}.

time(Time)-->
	[fem,'över'],
	one2twelve(Hour),{atom_concat(Hour,'5',Time)}.

time(Time)-->
	[tio,'över'],
	one2twelve(Hour),{atom_concat(Hour,'10',Time)}.

time(Time)-->
	[kvart,'över'],
	one2twelve(Hour),{atom_concat(Hour,'15',Time)}.	

time(Time)-->
	[tjugo,'över'],
	one2twelve(Hour),{atom_concat(Hour,'20',Time)}.
time(Time)-->
	[fem,i,halv],
	one2twelve(H),{preceding_hour(H,Hour),atom_concat(Hour,'25',Time)}.
time(Time)-->
	[halv],
	one2twelve(H),{preceding_hour(H,Hour),atom_concat(Hour,'30',Time)}.
time(Time)-->
	[fem,'över',halv],
	one2twelve(H),{preceding_hour(H,Hour),atom_concat(Hour,'35',Time)}.
time(Time)-->
	[tjugo,i],
	one2twelve(H),{preceding_hour(H,Hour),atom_concat(Hour,'40',Time)}.
time(Time)-->
	[kvart,i],
	one2twelve(H),{preceding_hour(H,Hour),atom_concat(Hour,'45',Time)}.
time(Time)-->
	[tio,i],
	one2twelve(H),{preceding_hour(H,Hour),atom_concat(Hour,'50',Time)}.
time(Time)-->
	[fem,i],
	one2twelve(H),{preceding_hour(H,Hour),atom_concat(Hour,'55',Time)}.


hour('00') -->
	[noll].
hour('00') -->
	[noll,noll].
hour(Hour) -->
	[noll],
	one2nine(Hour).
hour(Hour) -->
	one2nine(Hour).

hour(Hour) -->
	tens(Hour).
hour('20') -->
	[tjugo].
hour('21') -->
	[tjugo,ett].
hour('22') -->
	[tjugo,'två'].
hour('23') -->
	[tjugo,tre].

%inte tillåtet med t.ex. "tjugo noll"
minutes('00') -->
	[noll,noll].

minutes(Mins) -->
	[noll],
	one2nine(Mins).
minutes(Mins) -->
	tens(Mins).
minutes(Mins) -->
	twenties(Mins).
minutes(Mins) -->
	thirties(Mins).
minutes(Mins) -->
	fourties(Mins).
minutes(Mins) -->
	fifties(Mins).


%1-9
one2nine('01')-->
	[ett].
one2nine('02')-->
	['två'].
one2nine('03')-->
	[tre].
one2nine('04')-->
	[fyra].
one2nine('05')-->
	[fem].
one2nine('06')-->
	[sex].
one2nine('07')-->
	[sju].
one2nine('08')-->
	['åtta'].
one2nine('09')-->
	[nio].

one2twelve('10')-->
	[tio].
one2twelve('11')-->
	[elva].
one2twelve('12')-->
	[tolv].

one2twelve(N)-->
	one2nine(N).

%10-19
tens('10')-->
	[tio].
tens('11')-->
	[elva].
tens('12')-->
	[tolv].
tens('13')-->
	[tretton].
tens('14')-->
	[fjorton].
tens('15')-->
	[femton].
tens('16')-->
	[sexton].
tens('17')-->
	[sjutton].
tens('18')-->
	[arton].
tens('19')-->
	[nitton].

%20-29
twenties('20')-->
	[tjugo].
twenties('21')-->
	[tjugo,ett].
twenties('22')-->
	[tjugo,'två'].
twenties('23')-->
	[tjugo,tre].
twenties('24')-->
	[tjugo,fyra].
twenties('25')-->
	[tjugo,fem].
twenties('26')-->
	[tjugo,sex].
twenties('27')-->
	[tjugo,sju].
twenties('28')-->
	[tjugo,'åtta'].
twenties('29')-->
	[tjugo,nio].

%30-39
thirties('30')-->
	[trettio].
thirties('31')-->
	[trettio,ett].
thirties('32')-->
	[trettio,'två'].
thirties('33')-->
	[trettio,tre].
thirties('34')-->
	[trettio,fyra].
thirties('35')-->
	[trettio,fem].
thirties('36')-->
	[trettio,sex].
thirties('37')-->
	[trettio,sju].
thirties('38')-->
	[trettio,'åtta'].
thirties('39')-->
	[trettio,nio].
%40-49
fourties('40')-->
	[fyrtio].
fourties('41')-->
	[fyrtio,ett].
fourties('42')-->
	[fyrtio,'två'].
fourties('43')-->
	[fyrtio,tre].
fourties('44')-->
	[fyrtio,fyra].
fourties('45')-->
	[fyrtio,fem].
fourties('46')-->
	[fyrtio,sex].
fourties('47')-->
	[fyrtio,sju].
fourties('48')-->
	[fyrtio,'åtta'].
fourties('49')-->
	[fyrtio,nio].
%50-59
fifties('50')-->
	[femtio].
fifties('51')-->
	[femtio,ett].
fifties('52')-->
	[femtio,'två'].
fifties('53')-->
	[femtio,tre].
fifties('54')-->
	[femtio,fyra].
fifties('55')-->
	[femtio,fem].
fifties('56')-->
	[femtio,sex].
fifties('57')-->
	[femtio,sju].
fifties('58')-->
	[femtio,'åtta'].
fifties('59')-->
	[femtio,nio].


preceding_hour('01','00').
preceding_hour('02','01').
preceding_hour('03','02').
preceding_hour('04','03').
preceding_hour('05','04').
preceding_hour('06','05').
preceding_hour('07','06').
preceding_hour('08','07').
preceding_hour('09','08').
preceding_hour('10','09').
preceding_hour('11','10').
preceding_hour('12','11').
