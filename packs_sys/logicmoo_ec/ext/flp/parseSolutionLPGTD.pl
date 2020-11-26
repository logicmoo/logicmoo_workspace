% Support for reading file as a list.

:- ensure_loaded('readFileI').
%% :- ensure_loaded('sharedPDDL2.2.pl').

% parseDomain(+File, -Output).
% Parse PDDL domain File and return it rewritten prolog syntax.   
parseSolution(F, O):-
	view([solutionFile,F]),
	atomic_concat(F,'.filtered',F2),
	currentPlanner(Planner),
	(   (	Planner = 'LPG') ->
	    (	atomic_list_concat(['trim.pl',F,'>',F2],' ',Command) ) ;
	    (	(   Planner = 'OPTIC_CLP') -> atomic_list_concat(['trim_optic_clp.pl',F,'>',F2],' ',Command) ; true)),
	view([command,Command]),
	shell(Command,_),
	%% parseSolution(F, O, _).
	parseSolution(F2, O, _).

% The same as above and also return rest of file. Can be useful when domain and problem are in one file.
parseSolution(File, Output, R) :-
	read_file(File, List),
	view([parseSolutionList,List]),
	solution(Output, List, R),!.

lower_case_list(A,B) :-
	findall(Y,(member(X,A),lower_case(X,Y)),B).

capitalize(WordLC, WordUC) :-
	atom_chars(WordLC, [FirstChLow|LWordLC]),
	atom_chars(FirstLow, [FirstChLow]),
	upcase_atom(FirstLow, FirstUpp),
	atom_chars(FirstUpp, [FirstChUpp]),
	atom_chars(WordUC, [FirstChUpp|LWordLC]),!.

% List of DCG rules describing structure of domain file in language PDDL.
% BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
% This parser do not fully NOT support PDDL 3.0
% However you will find comment out lines ready for futher development.
%% solution(solution(N, CLI, P, T, ST, PT, MT, MV))
solution(solution(P))
                        -->
				%% [';','version', 'lpg-td-1.0'],
				%% [';','seed'],number(N),
				%% [';','command line:'], commandLine,
				%% [';','problem'], problem,
				%% [';','time'], mfloat(T),
				%% [';','search time'], mfloat(ST),
				%% [';','parsing time'], mfloat(PT),
				%% [';','mutex time'], mfloat(MT),
				%% [';','metricvalue'] ,mfloat(MV),
				generatedPlan(P).

%% commandLine(CLI)        --> ['lpg-td-1.0 -o hygiene.d.pddl -f hygiene.p.pddl -out worlds/hygiene.p.pddl.LPG.sol -speed'].
%% problem(P)              --> ['hygiene.p.pddl'].

generatedPlan(P)                    --> ['<',no,solution,'>'],{P = none}.
generatedPlan(P)                    --> oneOrMore(plan_step, P).
%% plan_step(P)               --> mfloat(T), [:], ['('], action(A), oneOrMore(action_argument, Args), [')'], ['['], mfloat(D), [']'], {P = [T,A,Args,D]}.
plan_step(P)               --> mfloat(T), [:], ['('], action(A), oneOrMore(action_argument, Args), [')'], mfloat(D), {P = [T,A,Args,D]}.

action(A)                  --> name(A).
action_argument(A)         --> name(A).

%% duration(F)                --> ['[',N1,'.',N2,']'], {atomic_list_concat([N1,'.',N2],'',Tmp),atom_number(Tmp,F)}.

% BNF description include operator <term>+ to mark zero or more replacements.
% This DCG extension to overcome this. 
oneOrMore(W, [R|Rs], A, C) :- F =.. [W, R, A, B], F, (
						      oneOrMore(W, Rs, B, C) ;
						      (Rs = [] , C = B)
						     ).

number(N)			--> [N], {integer(N)}.
number(N)			--> mfloat(N).
mfloat(F)                       --> [N1,'.',N2], {atom(N1), atom(N2), atomic_list_concat([N1,'.',N2],'',Tmp),atom_number(Tmp,F)}.
%% mfloat(F)                       --> [N], {atom(N),atomic_list_concat([N,'.0'],'',Tmp),atom_number(Tmp,F)}.
mfloat(F)                       --> [N], {atom(N),atom_number(N,F)}.

name(N)				--> [N], {integer(N), !, fail}.
name(N)				--> [N], {float(N), !, fail}.
name(N)				--> [N], {N=')', !, fail}.
name(N)				--> [N], {N='(', !, fail}.
name(N)				--> [N], {N='?', !, fail}.
name(N)				--> [N].
