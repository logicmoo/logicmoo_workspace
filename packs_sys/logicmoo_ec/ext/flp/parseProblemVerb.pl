%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseProblemVerb.pl
%%   Simple parser of PDDL domain file into prolog syntax.
%% Author: Robert Sasak, Charles University in Prague
%%
%% Example: 
%% ?-parseProblemVerb('problem.pddl', O).
%%   O = problem('blocks-4-0',							%name
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

% parseProblemVerb(+File, -Output).
% Parse PDDL problem File and return rewritten prolog syntax. 
parseProblemVerb(F, O):-
	view([problemFile,F]),
	parseProblemVerb(F, O, _).

% parseProblemVerb(+File, -Output, -RestOfFile).
% The same as above and also return rest of file. Can be useful when domain and problem are in one file.
parseProblemVerb(F, O, R) :-
	read_file(F, L),
	view([problemVerb,L]),
	problemVerb(O, L, R).

% List of DCG rules describing structure of problem file in language PDDL.
% BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
% This parser do not fully NOT support PDDL 3.0
% However you will find comment out lines ready for futher development.
% Some of the rules are already implemented in parseDomain.pl

%% :-['parseDomainPDDL2.2']. %make sure that it is loaded.

%% problemVerb(problemVerb(Name, Domain, Includes, StartDate, Units, OD, I, G, MS))   
problemVerb(problem(Name, Domain, OD, I, G, MS))   
				--> ['(',define,'(',problem,Name,')',
				     '(',':',domain, Domain,')',
				     '(',':',includes],zeroOrMore(token,Includes),[')',
				     '(',':',timing,
				     '(','start-date'],dateTimeZone(StartDate),[')',
				     '(',units],duration(Duration),[')',
				     ')'
				     ],
				
				%% (require_def(R)		; []),
				(p_object_declaration(OD)	; []),
				p_init(I),
				p_goal(G),
				%% (constraints(C)      ; []), %:constraints
				(p_metric_spec(MS)	; []),
				%% (length_spec(LS)	; []),
				[')'],
				{Units = units(Duration)}.

:- ensure_loaded('sharedPDDL2.2Problem').

%% problem(N,D,O,I2,G,M) :-
%% 	problemVerb(N,D,I,S,U,O,I2,G,M).

