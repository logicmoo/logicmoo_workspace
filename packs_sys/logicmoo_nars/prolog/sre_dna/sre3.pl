
% DCTG grammar for stochastic regular expressions
% Feb/99
%
% Grammar:  a | E:F  |  [a:E1(N1)+b:E2(N2)]  |  E*(Pr)  |  E+(Pr)
%
% epsilon e:  	 epsilon (not allowed in constructs; equiv. to [] generation)
% action:  	 a 
% concatanation: E:F			Pr(E:F) = Pr(E)*Pr(F)
% choice:	 [E1(N1)+b:E2(N2)]	Pr(Ei(Ni)) = Ni / N1+N2
% Kleene star:   E*(P)			Pr(E*) = [epsilon (1-P), (E:E*)(P)] 
% Kleene plus:   E+(P)			Pr(E+) = Pr(E:(E*(P)))
%
% where: Ni = integers >= 0,  P is probability (0 <= P < 1)
%
% Also: (a) no directly nested iteration allowed (star, plus)
%       (b) choice must have at least 2 terms
%
% Semantic rules:
% construct(E): makes a Prolog structure equivalent of expression
% generate(S,SL1,SL2,P): randomly interprets an expression, giving output 
%	string S (as list) with associated probability P. 
%	Note that repetition (star, plus) are executed according to 
%	probability, as well as for a max. generated string length.
%	When length met, then no repetitions possible (max_string_length_P/1)
%	Resulting string may unavoidably exceed this max length.
% raw_generate(S,SL1,SL2): like generate, but no probability computed.
% recognize(S,P): finds way of recognizing a string with the
% 	expression, resulting in probability P. Will work with backtracking,
%	so that all possibile derivations can be found (no probabilistic
%	execution as with generate/2.


expr ::= iter_expr^^A
<:>
(construct(E) ::- A^^construct(E)),
(raw_generate(S, SL1, SL2) ::- A^^raw_generate(S, SL1, SL2)),
(recognize(S, S2, PrSoFar, Pr) ::- 
	check_prob(PrSoFar), 
	A^^recognize(S, S2, PrSoFar, Pr)).

expr ::= noniter_expr^^A
<:>
(construct(E) ::- A^^construct(E)),
(raw_generate(S, SL1, SL2) ::- A^^raw_generate(S, SL1, SL2)),
(recognize(S, S2, PrSoFar, Pr) ::- 
	check_prob(PrSoFar), 
	A^^recognize(S, S2, PrSoFar, Pr)).

% -------------------------------------

noniter_expr ::= [a]   % action a
<:>
construct(a),
(raw_generate([a], SL1, SL2) ::- SL2 is SL1+1),
(recognize([a|T], T, PrSoFar, PrSoFar) ::- check_prob(PrSoFar)).

noniter_expr ::= [b]   % action b
<:>
construct(b),
(raw_generate([b], SL1, SL2) ::- SL2 is SL1+1),
(recognize([b|T], T, PrSoFar, PrSoFar) ::- check_prob(PrSoFar)).

% - - - -

noniter_expr ::= guardedexpr_a^^A1, intval^^B1, guardedexpr_b^^A2, intval^^B2
<:>
(construct([(E1,N1),(E2,N2)]) ::-  
	A1^^construct(E1), 
	B1^^construct(N1),
	A2^^construct(E2), 
	B2^^construct(N2)),
(raw_generate(S, SL1, SL2) ::- 
	B1^^construct(N1), B2^^construct(N2),	
	(raw_select_term([N1,N2], 1) ->
		A1^^raw_generate(S, SL1, SL2)
		;
		A2^^raw_generate(S, SL1, SL2))),
(recognize(S, S2, PrSoFar, Pr) ::- 
	B1^^construct(Val1), B2^^construct(Val2),
	Pr2 is PrSoFar*(Val1/(Val1+Val2)),
	check_prob(Pr2),
	A1^^recognize(S, S2, Pr2, Pr)),
(recognize(S, S2, PrSoFar, Pr) ::- 
	B1^^construct(Val1), B2^^construct(Val2),
	Pr2 is PrSoFar*(Val2/(Val1+Val2)),
	check_prob(Pr2),
	A2^^recognize(S, S2, Pr2, Pr)).

% - - - -

noniter_expr ::= expr^^A, expr^^B  % concat
<:>
(construct((E:F)) ::- A^^construct(E), B^^construct(F)),
(raw_generate(S, SL1, SL2) ::- 
	A^^raw_generate(S1, SL1, SL3), 
	B^^raw_generate(S2, SL3, SL2), 
	append(S1, S2, S)),
(recognize(S, S2, PrSoFar, Pr) ::- 
	check_prob(PrSoFar),
	A^^recognize(S, S3, PrSoFar, Pr1), 
	check_prob(Pr1),
	B^^recognize(S3, S2, Pr1, Pr)).

% -------------------------------------

iter_expr ::= noniter_expr^^A, probval^^B    % star
<:>
(construct((E)*(P)) ::- A^^construct(E), B^^construct(P)),
(raw_generate(S, SL1, SL2) ::- 
	B^^construct(P),
	max_string_length_P(MaxL),
	raw_gen_loop(A, P, MaxL, S, SL1, SL2)),
(recognize(S, S2, PrSoFar, Pr) ::- 
	check_prob(PrSoFar),
	B^^construct(Pr1),
	recognize_loop(A, Pr1, S, S2, PrSoFar, Pr)).

% - - - -

iter_expr ::= noniter_expr^^A, probval^^B    % plus
<:>
(construct((E)+(P)) ::- A^^construct(E), B^^construct(P)),
(raw_generate(S, SL1, SL2) ::- 
	A^^raw_generate(S1, SL1, SL3),
	B^^construct(P),
	max_string_length_P(MaxL),
	raw_gen_loop(A, P, MaxL, S2, SL3, SL2),
	append(S1, S2, S),
	!),
(recognize(S, S2, PrSoFar, Pr) ::- 
	check_prob(PrSoFar),
	A^^recognize(S, S3, PrSoFar, Pr1),
	\+ (S=S3), % new
	check_prob(Pr1),
	B^^construct(Pr2),
	recognize_loop(A, Pr2, S3, S2, Pr1, Pr)).

% -------------------------------------

guardedexpr_a ::= [a]   % action a
<:>
construct(a),
(raw_generate([a], SL1, SL2) ::- SL2 is SL1+1),
(recognize([a|T], T, PrSoFar, PrSoFar) ::- check_prob(PrSoFar)).

guardedexpr_a ::= [a], expr^^A  % concat
<:>
(construct((a:E)) ::- A^^construct(E)),
(raw_generate([a|S], SL1, SL2) ::- 
	A^^raw_generate(S, SL1, SL3), 
	SL2 is SL3+1),
(recognize([a|S], S2, PrSoFar, Pr) ::- 
	check_prob(PrSoFar),
	A^^recognize(S, S2, PrSoFar, Pr)).

guardedexpr_b ::= [b]   % action b
<:>
construct(b),
(raw_generate([b], SL1, SL2) ::- SL2 is SL1+1),
(recognize([b|T], T, PrSoFar, PrSoFar) ::- check_prob(PrSoFar)).

guardedexpr_b ::= [b], expr^^A  % concat
<:>
(construct((b:E)) ::- A^^construct(E)),
(raw_generate([b|S], SL1, SL2) ::- 
	A^^raw_generate(S, SL1, SL3), 
	SL2 is SL3+1),
(recognize([b|S], S2, PrSoFar, Pr) ::- 
	check_prob(PrSoFar),
	A^^recognize(S, S2, PrSoFar, Pr)).


% -------------------------------------

intval ::= [N], { is_an_integer(N) }
<:>
construct(N).

probval ::= [R], { is_a_probability(R)}
<:>
construct(R).

% ------------------------------------
% Prolog utilities...
% ------------------------------------

% is_an_integer(N):
%	N - integer value
% Succeeds if N is an integer. If N is variable, a random integer 
% in desired range is created.

is_an_integer(N) :-
	integer(N),
	!.
is_an_integer(N) :-
	int_range(Low, High),
	random(Low, High, N).

int_range(0, 1000).

% ------------------------------------
% is_a_probability(R):
%	R - real value 0.0 <= R < 1.0
% Succeeds if R is a float,  0.0 <= R < 1.0. 
% If R is variable, a random probability in desired range is created.

is_a_probability(R) :-
	float(R),
	!.
is_a_probability(R) :-
	random(T),
	R is truncate(T*100)/100.

% ------------------------------------
% raw_select_term(L, K):
%	L - list of probability weights
%	K - kth term selected via prob. weighting (between 1 and length(L))
%	Pr - calculated probability of selected term
% Like select_term, but no probability computed.

raw_select_term(L, K) :-
	sumlist(L, SL, 0, Sum),
	random(0, Sum, X),
	select_kth_term(SL, X, 1, K, _),
	!.


% ------------------------------------
% sumlist(A, B, L, S):
%	A - list of weights
%	B - summed list of weights (roulette wheel)
%	L - sum so far
%	S - final sum
% Creates a summed list of prob weights, with final total S.

sumlist([], [], Sum, Sum).
sumlist([N|R], [NewSum|SumList2], LastSum, Sum) :-
	NewSum is LastSum + N,
	sumlist(R, SumList2, NewSum, Sum).

% ------------------------------------
% select_kth_term(W, Val, SoFar, K, Val)
%	W - list of summed weights (roulette wheel)
%	Val - random value in wheel to use
%	SoFar - counter
%	K - selected term according to Val on W
%	Val - value of selected term

select_kth_term([Val], _, K, K, Val) :- !.  
select_kth_term([Val|_], X, K, K, Val) :-
	Val >= X, 
	!.
select_kth_term([_|R], X, K, K2, Val) :-
	K3 is K + 1,
	select_kth_term(R, X, K3, K2, Val).

% ------------------------------------
% raw_gen_loop(Tree, Pr, MaxL, S, SL1, SL2):
%	Tree - grammar tree to process
%	Pr - probability of doing an iteration
%	MaxL - max length of generated string for terminating looping
%	S - final generated string
%	SL1, SL2 - current length and final length of generated string
% Like gen_loop, but no probabilities computed.

raw_gen_loop(Tree, Pr, MaxL, S, SL1, SL2) :-
	SL1 < MaxL,
	maybe(Pr),
	Tree^^raw_generate(S1, SL1, SL3),
	raw_gen_loop(Tree, Pr, MaxL, S2, SL3, SL2),
	append(S1, S2, S),
	!.
raw_gen_loop(Tree, Pr, _, [], SL, SL) :- 
	!.


% ------------------------------------
% recognize_loop(Tree, Pr, S, S2, FinalPr):
%	Tree - grammar tree to process
%	Pr - probability of doing an iteration
%	S, S2 - string to recognize (before, after)
%	FinalPr - final probability of execution
% recognize_loop performs successive iterations of an iterative expression.
% Attempts to recognize S, computing probability each time. 
% No limit to number of iterations, other than the ability to consume S.
%	epsilon
%	E
%	E:E
%	E:E:E  etc.
% As soon as an iteration fails to consume (after backtracking as well), 
% then iteration quits.

recognize_loop(_, Pr, [], [], PrSoFar, FinalPr) :- % new
	!,
	FinalPr is PrSoFar*(1.0 - Pr),
	check_prob(FinalPr).
recognize_loop(T, Pr, S, S, PrSoFar, FinalPr) :-
	FinalPr is PrSoFar*(1.0 - Pr),
	check_prob(FinalPr).
recognize_loop(Tree, Pr, S, S2, PrSoFar, FinalPr) :-
	Pr3 is PrSoFar*Pr,
	check_prob(Pr3),
	Tree^^recognize(S, S3, Pr3, Pr1),
	\+(S=S3),
	check_prob(Pr1),
	recognize_loop(Tree, Pr, S3, S2, Pr1, FinalPr).

check_prob(P) :-
	min_grammar_prob_P(E),
	P > E,
	!.

% ------------------------------------
% for testing...

sre(Type, Expr, String, SL) :-
	repeat,
	(Type = full ; Type = grow),
	generate_tree(expr, Type, 12, _, Tree, _),
	Tree^^construct(Expr),
	Tree^^raw_generate(String, 0, SL),
	nl,sre_pp(Expr),nl,
	write('tree '),write(Tree),nl,
	tree_depth(Tree, Depth),
	write('Depth = '), write(Depth),nl.

sre2(Type, Expr, Input) :-
	repeat,
	(Type = full ; Type = grow),
	generate_tree(expr, Type, 12, _, Tree, _),
	Tree^^construct(Expr),
	nl, write(Type),
	nl,sre_pp(Expr),nl,
	bagof((Leftover,Pr),Tree^^recognize(Input, Leftover, 1.0, Pr), Rlist),
	write('Recog list: '), nl, writelist(Rlist), nl.

sre2c(Type, Expr, Input) :-
	repeat,
	(Type = full ; Type = grow),
	generate_tree(expr, Type, 12, _, Tree, _),
	Tree^^construct(Expr),
	nl, write(Type),
	nl,sre_pp(Expr),nl,
	bagof(Pr,Tree^^recognize(Input, [],  1.0, Pr), Prlist),
	write('Pr list: '), nl, writelist(Prlist), nl.

sre2b(Input) :-
	generate_tree(expr, grow, 8, _, Tree, _),
	Tree^^construct(Expr),
	Tree^^recognize(Input, Leftover, 1.0, Pr),
	nl,sre_pp(Expr),nl,
	write('Prob = '), write(Pr), nl,
	write('Leftover = '), write(Leftover), nl.

% ------------------------------------
% sre pretty printer

sre_pp((E*R)) :-
	write('('),
	sre_pp(E),
	write(')*'),
	write(R),
	!.
sre_pp((E+R)) :-
	write('('),
	sre_pp(E),
	write(')+'),
	write(R),
	!.
sre_pp((E:F)) :-
	sre_pp(E),
	write(':'),
	sre_pp(F),
	!.
sre_pp([A|T]) :-
	sre_pp_l([A|T]),
	!.
sre_pp((A,B)) :-
	write('('),
	sre_pp(A),
	write(','),
	write(B),
	write(')'),
	!.
sre_pp(X) :-
	write(X).

sre_pp_l([A]) :-
	sre_pp(A),
	!.
sre_pp_l([A|T]) :-
	write('['),
	sre_pp(A),
	write('+'),
	sre_pp_l(T),
	write(']'),
	!.

% ------------------------------------
% solution dump: writes soln expression to a file, for input later.
% Grammatical expression is written in multiple lines, since the full
% expression is often larger than Prolog's builtin "write" can handle.

/*
write_soln(Run, E) :-
	set_file_name("soln", Run, File),
	tell(File),
	write('soln('),
	write_term(E),
	write(').'),
	nl,
	told,
	tell(user),
	!.

write_term(node(X,List,Y)) :-
	!,
	write('node('),
	write(X),
	write(',['),
	write_tlist(List),
	write('],'),
	write(Y),
	write(')').
write_term(X) :- write(X).

write_tlist([]) :- !.
write_tlist([X,Y|Z]) :-
	!,
	write_term(X),
	write(','),
	nl,
	write_tlist([Y|Z]).
write_tlist([X]) :-
	write_term(X).
*/

