% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% airlines.pl [Chapter  9] Example rulebase for inference, etc.
%
located('Air Cal','US').
located('American Airlines','US').
located('British Airways','England').
located('British Caledonian','England').
located('Cambrian Airways','Scotland').
located('Carl Icahn','US').
located('Continental','US').
located('Delta','US').
located('Eastern Airlines','US').
located('Empire','US').
located('Frontier','US').
located('Hughes Airwest','US').
located('National Airlines','US').
located('Northwest','US').
located('Ozark','US').
located('Pan Am Pacific','US').
located('Pan Am','US').
located('People Express','US').
located('Piedmont','US').
located('Republic','US').
located('Scotair','Scotland').
located('Scottish Airways','Scotland').
located('Texas Air','US').
located('Trans World Airlines','US').
located('US Air','US').
located('United Airlines','US').
located('Virgin Airways','England').
located('Western','US').
%
bought('American Airlines','Air Cal').
bought('British Airways','British Caledonian').
bought('Carl Icahn','Trans World Airlines').
bought('Delta','Western').
bought('Northwest','Republic').
bought('Pan Am','National Airlines').
bought('People Express','Frontier').
bought('Piedmont','Empire').
bought('Republic','Hughes Airwest').
bought('Texas Air','Continental').
bought('Texas Air','Eastern Airlines').
bought('Texas Air','People Express').
bought('Trans World Airlines','Ozark').
bought('United Airlines','Pan Am Pacific').
bought('US Air','Piedmont').
%
subsidiary('Scotair','British Caledonian').
subsidiary('Scotair','British Airways').
subsidiary('Cambrian Airways','British Airways').
subsidiary('Scottish Airways','British Airways').
%
airline(Airline) :- located(Airline,_).
%
uk_based(Airline) :- located(Airline,'England').
uk_based(Airline) :- located(Airline,'Scotland').

us_based(Airline) :- located(Airline,'US').

subsidiary(Airline) :- subsidiary(Airline,_),!.
subsidiary(Airline1,Airline2) :- bought(Airline2,Airline1),!.
%
test0 :-
	write('Are all airlines US-based?'),nl,
	all(X,
                   airline(X),
                   us_based(X)).
test1 :-
	write('What Scottish airlines are there?'),nl,
	all(X,
                   located(X,'Scotland'),
                   printout(X)).
test2 :-
	write('Which airline was bought by Carl Icahn?'),nl,
	all(X,
                   and(airline(X),
                       bought('Carl Icahn',X)),
                   printout(X)).
test3 :-
	write('Which subsidiaries of British Airways are based in Scotland?'),nl,
	all(X,
                   and(located(X,'Scotland'),
                       subsidiary(X,'British Airways')),
                   printout(X)).
test4 :-
	write('What airlines bought another airline?'),nl,
	all(X,
                   and(airline(X),
                       exists(Y,
                              airline(Y),
                              bought(X,Y))),
                   printout(X)).
