%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pretty portrayal of literals
% for the Michalski trains problem

aleph_portray(eastbound(T)):-
	write('Train '), write(T), write(' is eastbound').
aleph_portray(short(C)):-
	write('car '), write(C), write(' is short').
aleph_portray(closed(C)):-
	write('car '), write(C), write(' is closed').
aleph_portray(long(C)):-
	write('car '), write(C), write(' is long').
aleph_portray(open_car(C)):-
	write('car '), write(C), write(' is open').
aleph_portray(double(C)):-
	write('car '), write(C), write(' is double-walled').
aleph_portray(jagged(C)):-
	write('car '), write(C), write(' has a jagged roof').
aleph_portray(shape(C,S)):-
	write('car '), write(C), write(' is '), write(S), write('-shaped').
aleph_portray(load(C,S,N)):-
	write('car '), write(C), write(' has '), write(N),
	write(' '), write(S),
	(N > 1 -> write('-shaped loads'); write('-shaped load')).
aleph_portray(wheels(C,N)):-
	write('car '), write(C), write(' has '), write(N), write(' wheels').
aleph_portray(has_car(T,C)):-
	write(T), write(' has a car '), write(C).
