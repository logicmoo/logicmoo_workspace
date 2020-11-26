%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseDomainPDDL.pl
%%   Simple parser of PDDL domain file into prolog syntax.
%% Author: Robert Sasak, Charles University in Prague
%%
%% Example: 
%% ?-parseDomainPDDL('blocks_world.pddl', O).
%%   O = domain(blocks,
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

% parseDomainPDDL(+File, -Output).
% Parse PDDL domain File and return it rewritten prolog syntax.   
parseDomainPDDL(F, O):-
	view([domainPDDLFile,F]),
	parseDomainPDDL(F, O, _).

% parseDomainPDDL(+File, -Output, -RestOfFile)
% The same as above and also return rest of file. Can be useful when domain and problem are in one file.
parseDomainPDDL(File, Output, R) :-
		read_file(File, List),
		%% view([domainPDDLList,List]),
		domainPDDL(Output, List, R).

% List of DCG rules describing structure of domain file in language PDDL.
% BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
% This parser do not fully NOT support PDDL 3.0
% However you will find comment out lines ready for futher development.
domainPDDL(domain(N, R, T, P, F, S))
			--> ['(','define', '(','domain'], name(N), [')'],
                             (require_def(R)	; []),
                             (types_def(T)    	; []), %:typing
                             %% (constants_def(Cs) ; []),
                             (predicates_def(P)	; []),
                             (functions_def(F)	; []), %:fluents
			     %% (constraints(Co) ; []), %:constraints
                             zeroOrMore(structure_def, S),
			     [')'].

:- ensure_loaded('sharedPDDL2.2Domain').