%% typed_list(W, G)		--> oneOrMore(W, N), ['-'], type(T), {G = are(N,T)}.
%% typed_list(W, [G,Ns])		--> oneOrMore(W, N), ['-'], type(T), !, typed_list(W, Ns), {T }.
%% typed_list(W, N)		--> zeroOrMore(W, N).

%% typed_list(L)                      --> oneOrMore(typed_list_element(E),L).
%% typed_list_element(E)              --> oneOrMore(nonDashToken,Items),['-'],token(T), {E = are(Items,T)}.
%% nonDashToken(T)                    --> token(T), {T \= '-'}.

typed_list(W, [F|Ls])           --> oneOrMore(W, L), ['-'], !, type(T), typed_list(W, Ls), {F =.. [are,L,T]}.	%:typing
typed_list(W, L)                --> zeroOrMore(W, L).

subsumptionList(T)              --> oneOrMore(subsumes,T).

subsumes(A)                     --> oneOrMore(nonDashToken,SubTypes),['-'],token(SuperType),{A = genls(SubTypes,SuperType)}.
nonDashToken(T)                 --> token(T), {T \= '-'}.

token(T)                        --> name(T).

% BNF description include operator <term>+ to mark zero or more replacements.
% This DCG extension to overcome this. 
oneOrMore(W, [R|Rs], A, C) :- F =.. [W, R, A, B], F, (
					oneOrMore(W, Rs, B, C) ;
					(Rs = [] , C = B) 
				).
% BNF operator <term>*
zeroOrMore(W, R)		--> oneOrMore(W, R).
zeroOrMore(_, [])		--> [].


% Name is everything that is not number, bracket or question mark.
% Those rules are not necessary, but rapidly speed up parsing process.
name(N)				--> [N], {integer(N), !, fail}.
name(N)				--> [N], {float(N), !, fail}.
name(N)				--> [N], {N=')', !, fail}.
name(N)				--> [N], {N='(', !, fail}.
%% name(N)				--> [N], {N=']', !, fail}.
%% name(N)				--> [N], {N='[', !, fail}.
name(N)				--> [N], {N='?', !, fail}.
name(N)				--> [N].

type(either(PT))		--> ['(',either], !, oneOrMore(primitive_type, PT), [')'].
type(PT)			--> primitive_type(PT).

primitive_type(N)		--> name(N).


literal(T, F)			--> atomic_formula(T, F).
literal(T, not(F))		--> ['(',not], atomic_formula(T, F), [')'].

atomic_formula(_, F)		--> ['('], predicate(P), zeroOrMore(term, T), [')'], {F =.. [P|T]}.		% cheating, maybe wrong

predicate(P)			--> name(P).

term(N)				--> name(N).
term(V)				--> variable(V).

variable('$VAR'(V))             --> ['?'], name(N), {capitalize(N,V)}.

number(N)			--> [N], {integer(N)}.
number(N)			--> mfloat(N).

gd(F)				--> atomic_formula(term, F).	%: this option is covered by gd(L)
gd(L)				--> literal(term, L).								%:negative-preconditions
gd(P)				--> ['(',and],  zeroOrMore(gd, P), [')'].
gd(or(P))			--> ['(',or],   zeroOrMore(gd ,P), [')'].					%:disjuctive-preconditions
gd(not(P))			--> ['(',not],  gd(P), [')'].							%:disjuctive-preconditions
gd(imply(P1, P2))		--> ['(',imply], gd(P1), gd(P2), [')'].						%:disjuctive-preconditions
gd(exists(L, P))		--> ['(',exists,'('], typed_list(variable, L), [')'], gd(P), [')'].		%:existential-preconditions
gd(forall(L, P))		--> ['(',forall,'('], typed_list(variable, L), [')'], gd(P), [')'].		%:universal-preconditions
gd(F)				--> f_comp(F).	%:fluents

f_head(F)			--> ['('], function_symbol(S), zeroOrMore(term, T), [')'], { F =.. [S|T] }.
f_head(S)			--> function_symbol(S).

function_symbol(S)		--> name(S).

mfloat(F)                       --> [N1,'.',N2], {number(N1), number(N2), atomic_list_concat([N1,'.',N2],'',Tmp),atom_number(Tmp,F)}.

pre_GD(P)			--> pref_GD(P).
pre_GD(P)                       --> oneOrMore(assignment,P).
pre_GD(P)			--> ['(',and], pre_GD(P), [')'].
pre_GD(forall(L, P))		--> ['(',forall,'('], typed_list(variable, L), [')'], pre_GD(P), [')'].		%:universal-preconditions

pref_GD(preference(N, P))	--> ['(',preference], (pref_name(N); []), gd(P), [')'].				%:preferences
pref_GD(P)			--> gd(P).

f_comp(compare(C, E1, E2))	--> ['('], binary_comp(C), f_exp(E1), f_exp(E2), [')'].

binary_comp('>')		--> ['>'].
binary_comp('<')		--> ['<'].
binary_comp('=')		--> ['='].
binary_comp('>=')		--> ['>','='].
binary_comp('<=')		--> ['<','='].

assignment(A)                   --> ['('], assign_op(P), f_head(H), f_exp(E), [')'], {A =.. [P,H,E]}.

assign_op(assign)		--> [assign].
assign_op(scale_up)		--> [scale_up].
assign_op(scale_down)		--> [scale_down].
assign_op(increase)		--> [increase].
assign_op(decrease)		--> [decrease].

:- ensure_loaded('shared').
