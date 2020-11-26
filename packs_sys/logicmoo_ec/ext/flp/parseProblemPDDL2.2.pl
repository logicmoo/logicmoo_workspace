%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseDomain.pl
%%   Simple parser of PDDL domain file into prolog syntax.
%% Author: Robert Sasak, Charles University in Prague
%%
%% Example: 
%% ?-parseProblemPDDL('problem.pddl', O).
%%   O = problemPDDL('blocks-4-0',							%name
%%              blocks,										%domain name
%%              _G1443,                            %require definition
%%              [block(d, b, a, c)],					%object declaration
%%              [ clear(c), clear(a), clear(b), clear(d), ontable(c), %initial state
%%                ontable(a), ontable(b), ontable(d), handempty,
%%                set('total-cost', 0)	],
%%              [on(d, c), on(c, b), on(b, a)],		%goal
%%              _G1447,										%constraints-not implemented
%%              metric(minimize, 'total-cost'),		%metric
%%              _G1449										%length_specification-not implemented
%%              )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Support for reading file as a list.

:- ensure_loaded('readFileI').
:- ensure_loaded('sharedPDDL2.2').

% parseProblemPDDL(+File, -Output).
% Parse PDDL problem File and return rewritten prolog syntax. 
parseProblemPDDL(F, O):-
	view([problemPDDLFile,F]),
	parseProblemPDDL(F, O, _).

% parseProblemPDDL(+File, -Output, -RestOfFile).
% The same as above and also return rest of file. Can be useful when domain and problem are in one file.
parseProblemPDDL(F, O, R) :-
	read_file(F, L),
	%% view([problemPDDLList,L]),
	problemPDDL(O, L, R).

% List of DCG rules describing structure of problem file in language PDDL.
% BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
% This parser do not fully NOT support PDDL 3.0
% However you will find comment out lines ready for futher development.
% Some of the rules are already implemented in parseDomain.pl

%% :-['parseDomainPDDL2.2']. %make sure that it is loaded.

problemPDDL(problem(Name, Domain, OD, I, G, MS))   
				--> ['(',define,'(',problem,Name,')',
				     '(',':',domain, Domain,')'],
				%% (require_def(R)		; []),
				(p_object_declaration(OD)	; []),
				p_init(I),
				p_goal(G),
				%% (constraints(C)      ; []), %:constraints
				(p_metric_spec(MS)	; []),
				%% (length_spec(LS)	; []),
				[')'].

:- ensure_loaded('sharedPDDL2.2Problem').