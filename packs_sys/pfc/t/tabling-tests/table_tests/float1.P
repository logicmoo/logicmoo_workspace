


%-------------------------------------------------------
% This needs to be compiled with option(spec_off) !!
% otherwise it produces wrong results.
%-------------------------------------------------------

:- table p/2.
:- import get_calls/3, get_returns/2 from tables.

p(1.2,2.3).

r(C) :- get_calls(C,X,Y), get_returns(X,Y).

test1:- p(1.2,X), X = 2.3,
	p(1.2,Y), Y = 2.3.

test :- writeln('Test begins'), fail.
test :- write('a:Testing Incomplete:'),
	test1,
	writeln('ok'),
	write('b:Testing Complete:'),
	test1,
	writeln('ok'),
	write('c:Testing get_calls:'),
	findall(X1,get_calls(p(X1,_),_,_),L1), write(l1(L1)), L1 = [1.2],
	writeln('ok'),
	write('d:Testing get_returns:'),
	findall(X2,r(p(1.2,X2)),L2), write(l2(L2)), L2 = [2.3],
	writeln('ok'),
	fail.
test :- writeln('test ends').

