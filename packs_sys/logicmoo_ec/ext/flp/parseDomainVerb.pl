%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseDomainVerb.pl
%%   Simple parser of Verb domain file into prolog syntax.
%% Author: Robert Sasak, Charles University in Prague
%%
%% Example: 
%% ?-parseDomainVerb('blocks_world.pddl', O).
%%   O = domainVerb(blocks,
%%        [strips, typing, 'action-costs'],
%%        [block],
%%        _G4108,
%%        [ on(block(?x), block(?y)),
%%	         ontable(block(?x)),
%%	         clear(block(?x)),
%%	         handempty,
%%	         holding(block(?x)) ],
%%        [number(f('total-cost', []))],
%%        _G4108,
%%        [ action('pick-up', [block(?x)],       %parameters
%%		      [clear(?x), ontable(?x), handempty], %preconditions
%%		      [holding(?x)],                       %positiv effects
%%          [ontable(?x), clear(?x), handempty], %negativ effects
%%          [increase('total-cost', 2)]),        %numeric effects
%%         ...],
%%       ...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Support for reading file as a list.

:- ensure_loaded('readFileI').

% parseDomainVerb(+File, -Output).
% Parse Verb domain File and return it rewritten prolog syntax.   
parseDomainVerb(F, O):-
	view([domainVerbFile,F]),
	parseDomainVerb(F, O, _).

% parseDomainVerb(+File, -Output, -RestOfFile)
% The same as above and also return rest of file. Can be useful when domain and problem are in one file.
parseDomainVerb(File, Output, R) :-
		read_file(File, List),
		view([domainVerbList,List]),
		domainVerb(Output, List, R).
% List of DCG rules describing structure of domain file in language Verb.
% BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
% This parser do not fully NOT support Verb 3.0
% However you will find comment out lines ready for futher development.

%% domainVerb(domainVerb(N, R, T, I, P, F, S))
domainVerb(domain(N, R, T, P, F, S))
			--> ['(','define', '(','domain'], name(N), [')'],
                             (require_def(R)	; []),
                             (types_def(T)    	; []), %:typing
                             (timing_def(I) ; []), %:timing
                             %% (constants_def(Cs) ; []),
                             (predicates_def(P)	; []),
                             (functions_def(F)	; []), %:fluents
			     %% (constraints(Co) ; []), %:constraints
                             zeroOrMore(structure_def, S),
			     [')'].

timing_def(Timing)		--> ['(',':',timing,'(',units],duration(Duration),[')',')'],{Timing = units(Duration)}.

:- ensure_loaded('sharedPDDL2.2Domain').

%% domain(N,R,T,P,F,S) :-
%% 	domainVerb(N,R,T,I,P,F,S).
