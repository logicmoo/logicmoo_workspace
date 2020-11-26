lower_case_list(A,B) :-
	findall(Y,(member(X,A),lower_case(X,Y)),B).

p_object_declaration(L)		--> ['(',':',objects], typed_list(name, L),[')'].
p_init(I)                      	--> ['(',':',init], zeroOrMore(init_el, I), [')'].

init_el(I)			--> literal(name, I).
init_el(at(N,L))		--> ['(',at], oneOrMore(token,List), gd(L), [')'],
	{atomic_list_concat(List,'',Atom),
	 atom_number(Atom,N)}.			% timed-initial literal
init_el(set(H,N))		--> ['(','='], f_head(H), (number(N) ; name(N)), [')'].				% fluents

p_goal(G)				--> ['(',':',goal], pre_GD(G), [')'].

%constraints(C)			--> ['(',':',constraints], pref_con_GD(C), [')'].				% constraints
pref_con_GD(and(P))		--> ['(',and], zeroOrMore(pref_con_GD, P), [')'].
%pref_con_GD(forall(L, P))	--> ['(',forall,'('], typed_list(variable, L), [')'], pref_con_GD(P), [')'].	% universal-preconditions
%pref_con_GD(preference(N, P))	--> ['(',preference], (pref_name(N) ; []), con_GD(P), [')'].			% prefernces
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

p_metric_spec(metric(O, E))	--> ['(',':',metric], optimization(O), metric_f_exp(E), [')'].

optimization(minimize)		--> [minimize].
optimization(maximize)		--> [maximize].

metric_f_exp(E)			--> ['('], binary_op(O), metric_f_exp(E1), metric_f_exp(E2), [')'], {E =..[O, E1, E2]}.
metric_f_exp(multi_op(O,[E1|E]))--> ['('], multi_op(O), metric_f_exp(E1), oneOrMore(metric_f_exp, E), [')']. % I dont see meanful of this rule, in additional is missing in f-exp
metric_f_exp(E)			--> ['(','+'], metric_f_exp(E1), [')'], {E=..[+, E1]}.
metric_f_exp(E)			--> ['(','-'], metric_f_exp(E1), [')'], {E=..[-, E1]}.
metric_f_exp(N)			--> number(N).
metric_f_exp(F)			--> ['('], function_symbol(S), zeroOrMore(name, Ns), [')'], { concat_atom([S|Ns], '-', F) }.
metric_f_exp(function(S))	--> function_symbol(S).
metric_f_exp(total_time)	--> ['total-time'].

metric_f_exp(is_violated(N))	--> ['(','is-violated'], pref_name(N), [')'].

% Work arround
length_spec([])			--> [not_defined].	% there is no definition???

float(F)			--> [F], {float(F)}.

:- ensure_loaded('shared').