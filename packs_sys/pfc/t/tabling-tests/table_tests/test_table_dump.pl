
:- import table_dump/0 from dump_table.
:- import table_dump/1 from dump_table.
:- import table_dump/2 from dump_table.
:- import table_dump/3 from dump_table.
:- import shell_to_list/3 from shell.
:- import numbervars/1 from num_vars.

:- table p/2.
:- dynamic p/2.
p(1,a).
p(1,b) :- tnot(p(2,b)).
p(2,b) :- tnot(p(1,b)).
p(3,X) :- q(X).

:- table q/1.
:- dynamic q/1.
q(1).
q(2).

:- dynamic r/1.
:- table r/1.
r(a).

:- dynamic s/2.
:- table s/2.
s(X,Y) :- q(X),r(Y).
s(2,b).
s(1,a1).
s(2,b1).

test :- p(_X,_Y),fail.
test:- test_1,fail.
test:- test_6,fail.
test:- writeln('----------------------'),fail.
test:- test_7,fail.
test.

test_1 :-
	write('test 1 '),
	table_dump(p(_,_),[summary(true)]),
	writeln('----------------------'),
	write('test 2 '),
	table_dump(p(_,_),[details(true)]),
	writeln('----------------------'),
	write('test 2a '),
	table_dump(p(_,_),[details(subgoals)]),
	writeln('----------------------'),
	write('test 3 '),
	table_dump(p(_,_),[details(true),summary(false)]),
	writeln('----------------------'),
	write('test 4 '),
	table_dump(p(_,_),[results(Results)]),numbervars(Results),writeln(results_4(Results)),
	writeln('----------------------'),
	write('test 5 '),
	table_dump(p(_,_),[details(true),summary(false),results(R2)]),numbervars(R2),writeln(results_5(R2)),
	writeln('----------------------').

test_6:- 
	write('test 6 '),
	table_dump(p(_,_),[details(true),summary(true),results(R3)]),numbervars(R3),writeln(results_6(R3)).

test_7:- 
	write('test 7 '),
	catch(table_dump(p(_,_),[badopt]),error(A,context(B,C)),writeln(caughtit_2(A))),
	writeln('----------------------').

:- table inc/1.
inc(_X):- table_dump(inc(_),[details(true)]).

test_2:- inc(_X).

end_of_file.

test_2:-
	writeln('----------------------'),
	shell_to_list('rm testtd testtd2',_,_),
	open(testtd,write,S),table_dump(S,p(_,_),[summary(true),details(true)]),close(S), nl,shell_to_list('cat testtd',Out,_),writeq(testtd:Out),nl,nl,close(S),
	writeln('----------------------'),
	open(testtd2,write,S2),table_dump(S2,p(_,_),[results(R),summary(true),details(true)]),close(S2),writeq(fileandresults(R)), nl,nl,shell_to_list('cat testtd2',R1,_),nl,nl,writeq(testtd2:R1),nl,close(S2).
