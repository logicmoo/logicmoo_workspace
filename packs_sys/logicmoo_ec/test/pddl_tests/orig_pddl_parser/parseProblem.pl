%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseDomain.pl
%%   Simple parser of PDDL domain file into prolog syntax.
%% Author: Robert Sasak, Charles University in Prague
%%
%% Example: 
%% ?-parseProblem('problem.pddl', O).
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


% parseProblem(+File, -Output).
% Parse PDDL problem File and return rewritten prolog syntax. 
parseProblem(F, O):-parseProblem(F, O, _).

% parseProblem(+File, -Output, -RestOfFile).
% The same as above and also return rest of file. Can be useful when domain and problem are in one file.
parseProblem(F, O, R) :-
	read_file(F, L),
	problem(O, L, R).

% Support for reading file as a list.
:-[readFile].



% List of DCG rules describing structure of problem file in language PDDL.
% BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
% This parser do not fully NOT support PDDL 3.0
% However you will find comment out lines ready for futher development.
% Some of the rules are already implemented in parseDomain.pl
:-[parseDomain]. %make sure that it is loaded.
problem(problem(Name, Domain, R, OD, I, G, _, MS, LS))   
				--> ['(',define,'(',problem,Name,')',
							'(',':',domain, Domain,')'],
                     (require_def(R)		; []),
							(object_declaration(OD)	; []),
							init(I),
							goal(G),
%                     (constraints(C)		; []), %:constraints
							(metric_spec(MS)	; []),
                     (length_spec(LS)	; []),
				[')'].

object_declaration(L)		--> ['(',':',objects], typed_list_as_list(name, L),[')'].

typed_list_as_list(W, [G|Ns])           --> oneOrMore(W, N), ['-'], type(T), !, typed_list_as_list(W, Ns), {G =.. [T,N]}.
typed_list_as_list(W, N)                --> zeroOrMore(W, N).



init(I)                      	--> ['(',':',init], zeroOrMore(init_el, I), [')'].

init_el(I)			--> literal(name, I).
init_el(set(H,N))		--> ['(','='], f_head(H), number(N), [')'].					%fluents
init_el(at(N, L))		--> ['(',at], number(N), literal(name, L), [')'].				% timed-initial literal
goal(G)				--> ['(',':',goal], pre_GD(G),[')'].
%constraints(C)			--> ['(',':',constraints], pref_con_GD(C), [')'].				% constraints
pref_con_GD(and(P))		--> ['(',and], zeroOrMore(pref_con_GD, P), [')'].
%pref_con_GD(foral(L, P))	--> ['(',forall,'('], typed_list(variable, L), [')'], pref_con_GD(P), [')'].	%universal-preconditions
%pref_con_GD(prefernce(N, P))	--> ['(',preference], (pref_name(N) ; []), con_GD(P), [')'].			%prefernces
pref_con_GD(P)			--> con_GD(P).

con_GD(and(L))			--> ['(',and], zeroOrMore(con_GD, L), [')'].
con_GD(forall(L, P))		--> ['(',forall,'('], typed_list(variable, L),[')'], con_GD(P), [')'].
con_GD(at_end(P))		--> ['(',at,end],	gd(P), [')'].
con_GD(always(P))		--> ['(',always],	gd(P), [')'].
con_GD(sometime(P))		--> ['(',sometime],	gd(P), [')'].
con_GD(within(N, P))		--> ['(',within], number(N), gd(P), [')'].

con_GD(at_most_once(P))		--> ['(','at-most-once'], gd(P),[')'].
con_GD(some_time_after(P1, P2))	--> ['(','sometime-after'], gd(P1), gd(P2), [')'].
con_GD(some_time_before(P1, P2))--> ['(','sometime-before'], gd(P1), gd(P2), [')'].
con_GD(always_within(N, P1, P2))--> ['(','always-within'], number(N), gd(P1), gd(P2), [')'].
con_GD(hold_during(N1, N2, P))	--> ['(','hold-during'], number(N1), number(N2), gd(P), [')'].
con_GD(hold_after(N, P))	--> ['(','hold-after'], number(N), gd(P),[')'].

metric_spec(metric(O, E))	--> ['(',':',metric], optimization(O), metric_f_exp(E), [')'].

optimization(minimize)		--> [minimize].
optimization(maximize)		--> [maximize].

metric_f_exp(E)			--> ['('], binary_op(O), metric_f_exp(E1), metric_f_exp(E2), [')'], {E =..[O, E1, E2]}.
metric_f_exp(multi_op(O,[E1|E]))--> ['('], multi_op(O), metric_f_exp(E1), oneOrMore(metric_f_exp, E), [')']. % I dont see meanful of this rule, in additional is missing in f-exp
metric_f_exp(E)			--> ['(','-'], metric_f_exp(E1), [')'], {E=..[-, E1]}.
metric_f_exp(N)			--> number(N).
metric_f_exp(F)			--> ['('], function_symbol(S), zeroOrMore(name, Ns), [')'], { F=..[S|Ns]}.%concat_atom([S|Ns], '-', F) }.
metric_f_exp(function(S))	--> function_symbol(S).
metric_f_exp(total_time)	--> ['total-time'].
metric_f_exp(is_violated(N))	--> ['(','is-violated'], pref_name(N), [')'].

% Work arround
length_spec([])			--> [not_defined].	% there is no definition???

