% Defining operator ?. It is a syntax sugar for marking variables: ?x
:-op(300, fy, ?).

lower_case_list(A,B) :-
	findall(Y,(member(X,A),lower_case(X,Y)),B).

capitalize(WordLC, WordUC) :-
	atom_chars(WordLC, [FirstChLow|LWordLC]),
	atom_chars(FirstLow, [FirstChLow]),
	upcase_atom(FirstLow, FirstUpp),
	atom_chars(FirstUpp, [FirstChUpp]),
	atom_chars(WordUC, [FirstChUpp|LWordLC]),!.

require_def(R)                                  --> ['(',':','requirements'], oneOrMore(require_key, R), [')'].
require_key(strips)				--> [':strips'].
require_key(typing)				--> [':typing'].
require_key('negative-preconditions')		--> [':negative-preconditions'].
require_key('disjunctive-preconditions')	--> [':disjunctive-preconditions'].
require_key(equality)				--> [':equality'].
require_key('existential-preconditions')	--> [':existential-preconditions'].
require_key('universal-preconditions')		--> [':universal-preconditions'].
require_key('quantified-preconditions')	        --> [':quantified-preconditions'].
require_key('conditional-effects')		--> [':conditional-effects'].
require_key(fluents)				--> [':fluents'].
require_key(adl)				--> [':adl'].
require_key('durative-actions')			--> [':durative-actions'].
require_key('derived-predicates')		--> [':derived-predicates'].
require_key('timed-initial-literals')		--> [':timed-initial-literals'].
require_key(preferences)			--> [':preferences'].
%require_key(constraints)			--> [':constraints'].
% Universal requirements
require_key(R)		--> [':', R].

types_def(L)			--> ['(',':',types],      subsumptionList(L), [')'].
constants_def(L)		--> ['(',':',constants],  typed_list(name, L), [')'].
predicates_def(P)		--> ['(',':',predicates], oneOrMore(atomic_formula_skeleton, P), [')'].

atomic_formula_skeleton(F)
                                --> ['('], predicate(P), typed_list(variable, L), [')'], {F =.. [P|L]}.
                                %% --> ['('], predicate(P), typed_list(variable, L), [')'], {compound_name_arguments(F,P,L)}.

atomic_function_skeleton(f(S, L))
				--> ['('], function_symbol(S), typed_list(variable, L), [')'].

functions_def(F)		--> ['(',':',functions], zeroOrMore(function_def,F), [')'].	%:fluents

function_def(F)                 --> ['('],name(N),zeroOrMore(typed_arguments,A),[')'],{F = f(N,A)}.
typed_arguments(A)              --> oneOrMore(variable,Variables),['-'],name(T),{A = are(Variables,T)}.

%% functions_def(F)		--> ['(',':',functions], function_typed_list(atomic_function_skeleton, F), [')'].	%:fluents
%constraints(C)		        --> ['(',':',constraints], con_GD(C), [')']. %:constraints
structure_def(A)		--> action_def(A).
structure_def(D)		--> durative_action_def(D).	%:durativeactions
structure_def(D)		--> derived_def(D).		%:derivedpredicates

function_typed_list(W, [F|Ls])  --> oneOrMore(W, L), ['-'], !, function_type(T), function_typed_list(W, Ls), {F =.. [are,L,T]}.	%:typing
function_typed_list(W, L)	--> zeroOrMore(W, L).

function_type(number)		--> [number].
emptyOr(_)			--> ['(',')'].
emptyOr(W)			--> W.

% Actions definitons
action_def(action(S, L, Precon, Pos, Neg, Assign))
				--> ['(',':',action], action_symbol(S),
						[':',parameters,'('], typed_list(variable, L), [')'],
						action_def_body(Precon, Pos, Neg, Assign),
					[')'].

durative_action_def(durativeAction(S, L, V, P1, P2))
                                --> ['(',':','durative-action'], action_symbol(S),
					[':',parameters,'('], typed_list(variable, L), [')'],
					[':',duration,'('], ['='], variable(_), value(V), [')'],
					durative_action_def_body(P1, P2), [')'].

value(V)                        --> atomic_function(_,V).
value(V)                        --> f_exp(V).

action_symbol(N)		--> name(N).
action_def_body(P, Pos, Neg, Assign)
				--> (([':',precondition], emptyOr(pre_GD(P)))	; []),
				    (([':',effect],       emptyOr(effect(Pos, Neg, Assign)))	; []).

durative_action_def_body(P1, P2)
				--> (([':',condition], temp_GD(P1)) ; []),
				    (([':',effect], temp_GD(P2)) ; []).

temp_GD(P)			--> ['(',and], oneOrMore(temp_GD,P), [')'].
temp_GD('at start'(P))          --> ['('],[at,start],pre_GD(P),[')'].
temp_GD('at end'(P))            --> ['('],[at,end],pre_GD(P),[')'].
temp_GD('over all'(P))          --> ['('],[over,all],pre_GD(P),[')'].
temp_GD(P)			--> pre_GD(P).

pref_name(N)			--> name(N).

atomic_function(_, F)		--> ['('], function_symbol(S), zeroOrMore(term, T), [')'], {F =.. [S|T]}.

f_exp(N)			--> number(N).
f_exp(op(O, E1, E2))		--> ['('], binary_op(O), f_exp(E1), f_exp(E2), [')'].
f_exp('-'(E))			--> ['(','-'], f_exp(E), [')'].
f_exp(H)			--> f_head(H).

binary_op(O)			--> multi_op(O).
binary_op('-')			--> ['−'].
binary_op('/')			--> ['/'].
multi_op('*')			--> ['*'].
multi_op('+')			--> ['+'].
multi_op('-')			--> ['-'].

cond_effect(E)   		--> ['(',and], zeroOrMore(p_effect, E), [')'].				%:conditional-effects
cond_effect(E)			--> p_effect(E).						%:conditional-effects

%% float(F, Head, Tail) :-
%% 	float(F), !,
%% 	with_output_to(codes(Head, Tail), write(F)).
%% float(F) -->
%% 	number(F),
%% 	{ float(F) }.

effect(P, N, A)			--> ['(',and], c_effect(P, N, A), [')'].
effect(P, N, A)			--> c_effect(P, N, A).
%% c_effect(forall(E))		--> ['(',forall,'('], typed_list(variabl∗), effect(E), ')'.	%:conditional-effects
c_effect(when(P, E))		--> ['(',when], gd(P), cond_effect(E), [')'].			%:conditional-effects
c_effect(P, N, A)		--> p_effect(P, N, A).
p_effect([], [], [])		--> [].
p_effect(Ps, Ns, [F|As])        --> ['('], assign_op(O), [')'], p_effect(Ps, Ns, As), {F =.. [O]}.
%% p_effect(Ps, Ns, [F|As])        --> ['('], assign_op(O), f_head(H), f_exp(E), [')'], p_effect(Ps, Ns, As), {F =.. [O, H, E]}.
p_effect(Ps, [F|Ns], As)	--> ['(',not], atomic_formula(term,F), [')'], p_effect(Ps, Ns, As).
p_effect([F|Ps], Ns, As)	--> atomic_formula(term, F), p_effect(Ps, Ns, As).
p_effect(op(O, H, E))		--> ['('], assign_op(O), f_head(H), f_exp(E), [')'].	%:fluents , What is difference between rule 3 lines above???

derived_def(derived(S,G))       --> ['(', ':', 'derived'], atomic_formula_skeleton(S), gd(G), [')'].

:- ensure_loaded('sharedPDDL2.2').
