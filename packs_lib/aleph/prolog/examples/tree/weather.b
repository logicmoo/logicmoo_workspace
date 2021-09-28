:- modeh(1,class(+day,-class)).
:- modeb(1,outlook(+day,#outlook)).
:- modeb(1,temp(+day,-temp)).
:- modeb(1,humidity(+day,-humidity)).
:- modeb(1,windy(+day,#windy)).
:- modeb(*,lteq(+temp,#temp)).
:- modeb(*,lteq(+humidity,#humidity)).

:- determination(class/2,outlook/2).
:- determination(class/2,temp/2).
:- determination(class/2,humidity/2).
:- determination(class/2,windy/2).
:- determination(class/2,lteq/2).

% :- set(tree_type,classification).
:- set(tree_type,class_probability).
:- set(classes,[play,dont_play]).
:- set(minpos,2).	% minimum examples in leaf for splitting
:- set(clauselength,5).
:- set(lookahead,2).	% to allow lookahead to lteq/2
:- set(prune_tree,true).
:- set(confidence,0.25).% pruning conf parameter used by C4.5
:- set(evalfn,entropy).
% :- set(evalfn,gini).
:- set(dependent,2).	% second arg of class/2 is to predicted


% type predicates
outlook(sunny).
outlook(overcast).
outlook(rain).

windy(true).
windy(false).

temp(64).
temp(65).
temp(68).
temp(69).
temp(70).
temp(71).
temp(75).
temp(80).
temp(81).
temp(83).
temp(85).

humidity(65).
humidity(70).
humidity(75).
humidity(80).
humidity(85).
humidity(86).
humidity(90).
humidity(95).
humidity(96).


lteq(X,Y):-
	var(Y), !,
	X = Y.
lteq(X,Y):-
	number(X), number(Y),
	X =< Y.

outlook(Day,Outlook):-
	table(Day,Outlook,_,_,_).
temp(Day,Temp):-
	table(Day,_,Temp,_,_).
humidity(Day,Humidity):-
	table(Day,_,_,Humidity,_).
windy(Day,Windy):-
	table(Day,_,_,_,Windy).

table(d1,sunny,75,70,true).
table(d2,sunny,80,90,true).
table(d3,sunny,85,85,false).
table(d4,sunny,72,95,false).
table(d5,sunny,69,70,false).
table(d6,overcast,72,90,true).
table(d7,overcast,83,86,false).
table(d8,overcast,64,65,true).
table(d9,overcast,81,75,false).
table(d10,rain,71,80,true).
table(d11,rain,65,70,true).
table(d12,rain,75,80,false).
table(d13,rain,68,80,false).
table(d14,rain,70,96,false).
