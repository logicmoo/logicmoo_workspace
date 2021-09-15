:- module(pplatex, [pp_form/1,
		    pp_form/2,
		    pp_matrix/1,
		    pp_matrix/2,
		    set_default_pp_form_options/1,
		    register_pplatex_op/4,
		    register_pplatex_functor/3,
		    write_form/2]).

:- use_module(swilib(onto_atom)).
:- use_module(swilib(options)).

:- dynamic(last_options/1).
:- dynamic(latex/2).

binary_logop('<->', '\\equi ', 1160, xfx).
binary_logop('<-', '\\revimp ', 1050, xfx).
binary_logop('->', '\\imp ', 1050, xfx).
binary_logop(',', '\\land ', 1000, xfy).
binary_logop(';', '\\lor ', 1000, xfy).
% binary_logop(';', '\\lor ', 1025, xfy).
binary_logop('=>', '\\entails ', 1200, xfx).
binary_logop('<=>', '\\equiv ', 1200, xfx).

unary_logop('~', '\\lnot ', 550, fy).

quantifier_logop('all', '\\forall ', 550, fy).
quantifier_logop('ex', '\\exists ', 550, fy).
quantifier_logop('all2', '\\forall ', 550, fy).
quantifier_logop('ex2', '\\exists ', 550, fy).
quantifier_logop('alla', '\\forall ', 550, fy).
quantifier_logop('exa', '\\exists ', 550, fy).
quantifier_logop('allp', '\\forall ', 550, fy).
quantifier_logop('exp', '\\exists ', 550, fy).

%
% quantifier_logop('all', '\\forall ', 1200, fy).
% quantifier_logop('ex', '\\exists ', 1200, fy).
% quantifier_logop('all2', '\\forall ', 1200, fy).
% quantifier_logop('ex2', '\\exists ', 1200, fy).
% quantifier_logop('alla', '\\forall ', 1200, fy).
% quantifier_logop('exa', '\\exists ', 1200, fy).
% quantifier_logop('allp', '\\forall ', 1200, fy).
% quantifier_logop('exp', '\\exists ', 1200, fy).
%

nullary_logop('true', '\\true ', 1).
nullary_logop('false', '\\false ', 1).

:- dynamic current_pretty_op_1/4.
:- dynamic current_pretty_functor/3.

current_pretty_op(Pri, Assoc, Name, LatexName) :-
	current_pretty_op_1(Pri, Assoc, Name, LatexName),
	!.
current_pretty_op(Pri, Assoc, Name, Name) :-
	current_op(Pri, Assoc, Name).

register_pplatex_op(Pri, Assoc, Name, LatexName) :-
	retractall(current_pretty_op_1(_, _, Name, _)),
	( var(LatexName) ->
	  %% to just delete the registration
	  true
	; assert(current_pretty_op_1(Pri, Assoc, Name, LatexName))
	).

register_pplatex_functor(Name, LatexName, PrintLength) :-
	retractall(current_pretty_functor(Name, _, _)),
	( var(LatexName) ->
	  %% to just delete the registration
	  true
	; assert(current_pretty_functor(Name, LatexName, PrintLength))
	).

% logop('\\setof').
% logop('\\nl').
% logop('\\pl').


binary_logop_priority(Op, Options, P, M) :-
	from_options(format=prolog, Options),
	!,
	current_op(P, M, Op),
	(M = xfx ; M = xfy),
	!.
binary_logop_priority(Op, _, P, M) :-
	binary_logop(Op, _, P, M).

unary_logop_priority(Op, Options, P, M) :-
	from_options(format=prolog, Options),
	!,
	current_op(P, M, Op),
	(M = fx ; M = fy),
	!.
unary_logop_priority(Op, _, P, M) :-
	unary_logop(Op, _, P, M).

quantifier_logop_priority(Op, _, P, M) :-
	quantifier_logop(Op, _, P, M).

:- dynamic default_pp_form_options/1.

set_default_pp_form_options(Options) :-
	retractall( default_pp_form_options(_) ),
	basic_default_pp_form_options(BOptions),
	default_options(Options, BOptions, Options1),
	assert(  default_pp_form_options(Options1) ).

basic_default_pp_form_options(Options) :-
	Options =
	[format=prolog,	  %% prolog | latex
	 style=full,	  %% full | brief
	 qstyle=comma,    %% comma | nocomma | quant
	 maxpos=60,	  %% approx. max allowed width
	 aoparen=false,   %% parens around "and" within "or" (deprecated)
	 %%
	 %% The following take effects only in latex format:
	 %%
	 finally='',      %% insert at end of last array row
	 rightcols=0,     %% top binops up to given depth shown in right cols
	 camel=false].    %% transform underscores to camel case 

:- set_default_pp_form_options([]).

write_n_times(N, A) :-
	N > 0,
	!,
	write(A),
	N1 is N-1,
	write_n_times(N1, A).
write_n_times(0, _).

set_last_options(Os) :-
	retractall( last_options(_) ),
	assert( last_options(Os) ).

pp_form(F) :-
	pp_form(F, []).

pp_form(_, Options) :-
	from_options(style=skip, Options, none),
	!.
pp_form(F, Options) :-
	copy_term(F, F1),
	numbervars(F1, 0, _),
	default_pp_form_options(DefaultOptions),
	default_options(Options, DefaultOptions, Options1),
	set_last_options(Options1),
	from_options(camel=Camel, Options1),
	( Camel=true ->
	  underscore_to_camel(F1, F2)
	; F2 = F1
	),
	from_options(format=Format, Options1),
	( Format = latex ->
	  retractall(latex(_,_)),
	  from_options(rightcols=RCB, Options1),	  
	  write('\\begin{array}{l'),
	  write_n_times(RCB, 'l'),
	  writeln('}')
	; true
	),
	bound_new(Bound),
	ppl(F2, 1500, top, 0, true, Bound, Options1),
	( Format = latex ->
	  retractall(latex(_,_)),
	  from_options(finally=Finally, Options1),
	  write(Finally),
	  S2 = '\n\\end{array}',
	  writeln(S2)
	; true
	).

write_form(_, Options) :-
	from_options(style=skip, Options, none),
	!.
write_form(F, Options) :-
	copy_term(F, F1),
	numbervars(F1, 0, _),
	default_pp_form_options(DefaultOptions),
	default_options(Options, DefaultOptions, Options1),
	set_last_options(Options1),
	from_options(camel=Camel, Options1),
	( Camel=true ->
	  underscore_to_camel(F1, F2)
	; F2 = F1
	),
	bound_new(Bound),
%	ppl(F2, 1000, top, 0, true, Bound, [maxpos=10000|Options1]),
	ppl(F2, 1500, top, 0, true, Bound, [maxpos=10000|Options1]),	
	from_options(finally=Finally, Options1),
	write(Finally).
	
% 	from_options(style=Style, Options1),
% 	( Style=brief ->
% 	  write_brief_latex(F2, 1000)
% 	; write_semibrief_latex(F2, 1000)
% 	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pp_matrix(M) :-
	pp_matrix(M, []).

%%%% 
%%%% draft
%%%%
pp_matrix(_, Options) :-
	from_options(style=skip, Options, none),
	!.
pp_matrix(M, Options) :-
	default_pp_form_options(DefaultOptions),
	default_options(Options, DefaultOptions, Options1),
	set_last_options(Options1),
	( M = [C|Cs] ->
	  write('['),
	  \+ \+ ppl_clause(C, Options1),
	  ( member(C1, Cs),
	    write(','),
	    pp_nl(Options1),
	    write(' '),
	    ppl_clause(C1, Options1),
	    fail
	  ; true
	  ),
	  write(']')
	; write('[]')
	),
	nl.

ppl_clause(C, Options) :-
	( from_options(style=brief, Options) ->
	  term_variables(C, Vs),
	  instantiate_vars(Vs, C, 0, _)
	; numbervars(C, 1, _)
	),
	( C = [L|Ls] ->
	  write('['),
	  pp_form(L, Options),
	  ( member(L1, Ls),
	    write(', '),
	    pp_form(L1, Options),
	    fail
	  ; true
	  ),
	  write(']')
	; write('[]')
	).

instantiate_vars([X|Xs], F, N, N1) :-
	nth_var_name(N, X1),
	( sub_term(T, F), \+ var(T), functor(T, X1, _) ->
	  N2 is N+1,
	  instantiate_vars([X|Xs], F, N2, N1)
	; X = X1,
	  N2 is N+1,
	  instantiate_vars(Xs, F, N2, N1)
	).
instantiate_vars([], _, N, N).
	
nth_var_name(N, X) :-
	K is N mod 6,
	( K = 0 ->
	  A = x
	; K = 1 ->
	  A = y
	; K = 2 ->
	  A = z
	; K = 3 ->
	  A = u
	; K = 4 ->
	  A = v
	; K = 5 ->
	  A = w
	),
	C is N div 6,
	( C = 0 ->
	  X = A
	; concat_atom([A,C], X)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pp_variables_latex(Q, X, Options) :-
	from_options(style=Style, Options),
	from_options(qstyle=QStyle, Options),
	(X \= [_|_] -> X1 = [X] ; X1 = X ),
	bound_new(X1, B),
	( Style=brief ->
	  ( QStyle=nocomma ->
	    pp_unary_op(Q, Options),
	    onto_atom(write_commalist_latex_brief(X1, B), Latex)
	  ; QStyle=quant ->
	    pp_unary_op(Q, Options),
	    onto_atom(write_commalist_latex_brief(X1, B), Latex)
	  ; pp_unary_op(Q, Options),
	    onto_atom(write_commalist_latex_brief(X1, B), Latex)
	  )
	; ( QStyle=nocomma ->
	    pp_unary_op(Q, Options),
	    onto_atom(write_nocommalist_latex(X1, B), Latex)
	  ; QStyle=quant ->
	    onto_atom(write_quantlist_latex(X1, Q,  Options, B), Latex)
	  ; pp_unary_op(Q, Options),
	    onto_atom(write_commalist_latex(X1, B), Latex)
	  )
	),
	atom_length(Latex, L),
	pp_latex_string(Latex, L).

pp_variables_prolog(X, Options) :-
	from_options(style=Style, Options),
	( Style = brief ->
	  (X \= [_|_] -> X1 = [X] ; X1 = X ),
	  write_commalist_brief(X1)
	; writeq1(X)
	).

ppl(F, OuterPri, _OuterOp, BinopDepth, OuterBreak, Bound, Options) :-
	F =.. [Op,Vs,F1],
	quantifier_logop_priority(Op, Options, Pri, OpMode),
	!,
	from_options(format=Format, Options),
	( Format = prolog ->
	  NeedsParen = false,
	  binary_logop_priority(',', Options, PriToPass, _)
	; ( Pri < OuterPri -> NeedsParen = false
	  ; Pri = OuterPri, OpMode = fy ->
	    NeedsParen = false
	  ; NeedsParen = true
	  ),
	  PriToPass = Pri
	),
	( NeedsParen == true -> pp_paren_open(Options) ; true ),
	( Format = prolog ->
	  pp_unary_op(Op, Options),
	  write('('),
	  pp_variables_prolog(Vs, Options),
	  write(',')
	; pp_variables_latex(Op, Vs, Options)
	),
	pp_sep(Options),
	pp_varform_sep(Options),
	bound_add_variables(Bound,  Vs, Bound1),
	ppl(F1, PriToPass, Op, BinopDepth, OuterBreak, Bound1, Options),
%	ppl(F1, PriToPass, Op, BinopDepth, true, Bound1, Options),
	( Format = prolog -> write(')') ; true ),
	( NeedsParen == true -> pp_paren_close(Options) ; true ).
ppl(~(X=Y), OuterPri, OuterOp, BinopDepth, OuterBreak, Bound, Options) :-
	from_options(format=Format, Options),
	from_options(style=Style, Options),
	( Format \= prolog ; Style = brief), 
	!,
	ppl('#'(X,Y), OuterPri, OuterOp, BinopDepth, OuterBreak, Bound, Options).
ppl(F, OuterPri, _OuterOp, BinopDepth, OuterBreak, Bound, Options) :-
	F =.. [Op,F1],	
	unary_logop_priority(Op, Options, Pri, OpMode),
	!,
	( Pri < OuterPri -> NeedsParen = false
	; Pri = OuterPri, OpMode = fy ->
	  NeedsParen = false
	; NeedsParen = true
	),
	( NeedsParen == true -> pp_paren_open(Options) ; true ),
	pp_unary_op(Op, Options),
	from_options(format=Format, Options),
	( Format = prolog,
	  F1 =.. [SubOp,_,_], binary_logop(SubOp, _, _, _) ->
	  write(' ')
	; true
	),
	pp_sep(Options),
	ppl(F1, Pri, Op, BinopDepth, OuterBreak, Bound, Options),
	( NeedsParen == true -> pp_paren_close(Options) ; true ).
ppl('$macro'(T), _, _, BinopDepth, _, Bound, Options) :-
	!,
	from_options(format=Format, Options),
	fits_in_line(T, false, Options, Break),
	( (Break = false ; atomic(T)) ->
	  pp_logatom('$macro'(T), Bound, Options)
	; T =.. [F|Args],
	  ( Format  = latex ->
	    onto_atom(write_macro_functor_brief_latex(F, Bound), Functor),
	    atom_length(Functor, Len),
	    pp_latex_string(Functor, Len)
	  ; writeq1(F)
	  ),
	  pp_paren_open(Options),
	  ppsave_position(Options, Pos),
	  hard_save_full_position(Options, Pos1),
	  BinopDepth1 is BinopDepth + 1,
	  Args = [Arg1|Args1],
	  ppl(Arg1, 1500, top, BinopDepth1, true, Bound, Options),
	  ( member(Arg2, Args1),
	    pp_comma(Options),
	    pp_nl(Options),
	    hard_restore_full_position(Options, Pos1),
	    pp_indent(Options, Pos),
	    ppl(Arg2, 1500, top, BinopDepth1, true, Bound, Options),
	    fail
	  ; true
	  ),	  
	  pp_paren_close(Options),
	  prelease_position(Options)
	).
ppl(F, OuterPri, OuterOp, BinopDepth, OuterBreak, Bound, Options) :-
	F =.. [Op,F1,F2],
	binary_logop_priority(Op, Options, Pri, OpMode),
	!,
	( Pri < OuterPri ->
	  from_options(aoparen=AOParen, Options),
	  ( Op = ',', OuterOp = ';', AOParen = true ->
	    NeedsParen = true
	  ; NeedsParen = false
	  ),
	  ( OuterBreak = true ->
	    fits_in_line(F, NeedsParen, Options, Break)
	  ; Break = false
	  ),
	  BinopDepth1 is BinopDepth + 1
	; Pri = OuterPri, OuterOp = Op, OpMode = xfy ->
	  NeedsParen = false,
	  Break = OuterBreak,
	  BinopDepth1 = BinopDepth
	; NeedsParen = true,
	  ( OuterBreak = true ->
	    fits_in_line(F, NeedsParen, Options, Break)
	  ; Break = false
	  ),
	  BinopDepth1 is BinopDepth + 1
	),
	( NeedsParen == true -> pp_paren_open(Options) ; true ),
	ppsave_position(Options, Pos),
	ppl(F1, Pri, Op, BinopDepth1, Break, Bound, Options),
	pp_sep(Options),
	pp_binary_op(Op, BinopDepth1, Break, Options),
	( Break = false ->
	  pp_sep(Options)
	; pp_nl(Options),
	  pp_indent(Options, Pos)
	),
	prelease_position(Options),
	ppl(F2, Pri, Op, BinopDepth1, Break, Bound, Options),
	( NeedsParen == true -> pp_paren_close(Options) ; true ).
ppl(true, _OuterPri, _OuterOp, _BD, _OuterBreak, _, Options) :-
	!,
	pp_nullary(true, Options).
ppl(false, _OuterPri, _OuterOp, _BD, _OuterBreak, _, Options) :-
	!,
	pp_nullary(false, Options).
ppl(F, _OuterPri, _OuterOp, _BD, _OuterBreak, Bound, Options) :-
	pp_logatom(F, Bound, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

latex_op(Op, LatexOp, ApproxLen) :-
	binary_logop(Op, LatexOp, _, _),
	!,
	( memberchk(Op, ['<->', '->', '->']) ->
	  ApproxLen = 2
	; ApproxLen = 1
	).
latex_op(Op, LatexOp, 1) :-
	unary_logop(Op, LatexOp, _, _),
	!.
latex_op(Op, LatexOp, 1) :-
	quantifier_logop(Op, LatexOp, _, _),
	!.
latex_op(X, X, 1).

fits_in_line(F, NeedsParen, Options, Break) :-
	from_options(format=Format, Options),
	from_options(maxpos=MaxPos, Options),
	%% use write length as approximate indicator
	rm_macro_wrapper(F, F1),
	format(atom(A), '~w', [F1]),
	atom_length(A, L),
	( NeedsParen = true ->
	  L1 is L+2
	; L1 = L
	),
	( Format = latex ->
	  findall(L, latex(_, L), Ls),
	  sum(Ls, Pos)
	; line_position(current_output, Pos)
	),
	( Pos + L1 =< MaxPos ->
	  Break = false
	; Break = true
	).

rm_macro_wrapper('$macro'(X), Y) :-
	!,
	rm_macro_wrapper(X, Y).
rm_macro_wrapper(X, Y) :-
	compound(X),
	!,
	X =.. [F|Xs],
	map_rm_macro_wrapper(Xs, Ys),
	Y =.. [F|Ys].
rm_macro_wrapper(X, X).

map_rm_macro_wrapper([X|Xs], [X1|Xs1]) :-
	rm_macro_wrapper(X, X1),
	map_rm_macro_wrapper(Xs, Xs1).
map_rm_macro_wrapper([], []).

prelease_position(Options) :-
	from_options(format=Format, Options),
	( Format = latex ->
	  retract_latex_to_mark
	; true
	).

hard_save_full_position(Options, Pos) :-
	from_options(format=Format, Options),
	( Format = latex ->
	  findall(latex(A,B), latex(A,B), Pos)
	; true
	).

hard_restore_full_position(Options, Pos) :-
	from_options(format=Format, Options),
	( Format = latex ->
	  retractall(latex(_,_)),
	  ( member(P, Pos),
	    assertz(P),
	    fail
	  ; true
	  )
	; true
	).

ppsave_position(Options, Pos) :-
	from_options(format=Format, Options),
	( Format = latex ->
	  findall(S, (latex(S,_), S \= mark(_)), Pos1),
	  asserta(latex(mark(k), 0)),
	  reverse(Pos1, Pos)
	; line_position(current_output, Pos)
	).

pp_indent(Options, Pos) :-
	from_options(format=Format, Options),
	( Format = latex ->
	  ( Pos = [] ->
	    true
	  ; write('\\hphantom{'),
	    ( member(S, Pos),
	      write(S),
	      fail
	    ; true
	    ),
	    write('} ')
	  )
	; write_spaces(Pos)
	).
	
sum([], 0).
sum([N|Ns], S) :-
	sum(Ns, S1),
	S is N+S1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pp_comma(Options) :-
	from_options(format=Format, Options),
	( Format = prolog ->
	  write(',')
	; Format = latex ->
	  asserta(latex(',', 0)),
	  write(',')
	).

pp_sep(Options) :-
	from_options(format=Format, Options),
	( Format = prolog ->
	  true
	; Format = latex ->
	  asserta(latex(' ', 0)),
	  write(' ')
	).

pp_varform_sep(Options) :-
	from_options(format=Format, Options),
	( Format = prolog ->
	  write(' ')
	; Format = latex ->
	  asserta(latex('\\, ', 1)),
	  write('\\, ')
	).

pp_paren_open(Options) :-
	from_options(format=Format, Options),
	( Format = prolog ->
	  write('(')
	; Format = latex ->
	  asserta(latex('(', 1)),
	  write('(')
	).

pp_paren_close(Options) :-
	from_options(format=Format, Options),
	( Format = prolog ->
	  write(')')
	; Format = latex ->
	  asserta(latex(')', 1)),
	  write(')')
	).

pp_unary_op(Op, Options) :-
	from_options(format=Format, Options),
	( Format = prolog ->
	  write(Op)
	; Format = latex ->
	  latex_op(Op, LatexOp, ApproxLen),
	  asserta(latex(LatexOp, ApproxLen)),
	  write(LatexOp)
	).

dry_pp_unary_op(Op, Options) :-
	from_options(format=Format, Options),
	( Format = prolog ->
	  write(Op)
	; Format = latex ->
	  latex_op(Op, LatexOp, _ApproxLen),
	  write(LatexOp)
	).

pp_binary_op(Op, BinopDepth, BeforeBreak, Options) :-
	from_options(format=Format, Options),
	( Format = prolog ->
	  write(Op)
	; Format = latex ->
	  ( BeforeBreak=true,
	    from_options(rightcols=RCB, Options),
	    RCB > 0,
	    BinopDepth =< RCB ->
	    Col is RCB + 1 - BinopDepth,
	    write_n_times(Col, '&')
	  ; true
	  ),
	  latex_op(Op, LatexOp, ApproxLen),
	  asserta(latex(LatexOp, ApproxLen)),
	  ( BeforeBreak = true ->  write('\\; ') ; true ),
	  write(LatexOp)
	).

pp_nl(Options) :-
	from_options(format=Format, Options),
	( Format = prolog ->
	  nl
	; Format = latex ->
	  NL = '\\\\',
	  writeln(NL)
	).

retract_latex_to_mark :-
	retract( latex(X, _) ),
	!,
	( X = mark(_) ->
	  !
	; retract_latex_to_mark
	).
retract_latex_to_mark.


pp_nullary(F, Options) :-
	from_options(format=Format, Options),
	( Format = prolog ->
	  writeq1(F)
	; Format = latex ->
	  nullary_logop(F, Latex, ApproxLen),
	  pp_latex_string(Latex, ApproxLen)
	).

pp_logatom(F, Bound, Options) :-
	from_options(format=Format, Options),
	from_options(style=Style, Options),
	( Format = prolog ->
	  ( Style = brief ->
	    write_brief(F)
	  ; writeq1(F)
	  )
	; Format = latex ->
	  ( Style = brief ->
	    onto_atom( (write_brief_latex(F, 1000, Bound)), Latex )
	  ; onto_atom( (write_semibrief_latex(F, 1000, Bound)), Latex )
	  ),
	  atom_length(Latex, Len),
	  pp_latex_string(Latex, Len)
	).

pp_latex_string(Latex, ApproxLen) :-
	write(Latex),
	asserta(latex(Latex, ApproxLen)).

write_brief_latex_op(-) :-
	write('\\textrm{-}').
write_brief_latex_op(Op) :-
	write(Op).

write_semibrief_latex_binop(Op, X, Y, K, B) :-
	write_semibrief_latex(X, K, B),
	write_brief_latex_op(Op),
	write_semibrief_latex(Y, K, B).

write_brief_latex_binop(Op, X, Y, K, B) :-
	write_brief_latex(X, K, B),
	write_brief_latex_op(Op),
	write_brief_latex(Y, K, B).

write_semibrief_latex_binop_paren(Op, X, Y, K, B) :-
	write('('),
	write_semibrief_latex(X, K, B),
	write_brief_latex_op(Op),
	write_semibrief_latex(Y, K, B),
	write(')').

write_brief_latex_binop_paren(Op, X, Y, K, B) :-
	write('('),
	write_brief_latex(X, K, B),
	write_brief_latex_op(Op),
	write_brief_latex(Y, K, B),
	write(')').

write_semibrief_latex(F/T, K, B) :-
	%% support for tform syntax
	T =.. [t|Args],
	!,
	write_semibrief_latex('$varfunctor'(F, Args), K, B).
write_semibrief_latex('$vf'(F, Args), K, B) :-
	!,
	write_semibrief_latex('$varfunctor'(F, Args), K, B).
write_semibrief_latex('$varfunctor'(F, Args), K, B) :-
	!,
	( Args = [] ->
	  write_semibrief_latex(F, K, B)
	; write_semibrief_latex(F, K, B),
	  write_arguments_semibrief_latex(Args, B)
	).
write_semibrief_latex('$macro'(T), K, B) :-
	functor(T, Op, 2),
	latex_binop(Op, _, _),
	!,
	write_semibrief_latex(T, K, B).
write_semibrief_latex('$macro'(T), _, B) :-
	!,
	( compound(T) ->
	  T =.. [F|Ts],
	  write_macro_functor_brief_latex(F, B),
	  write_form_arguments_semibrief_latex(Ts, B)
	; write_macro_functor_brief_latex(T, B)
	).
write_semibrief_latex('$defmacro'(T), K, B) :-
	functor(T, Op, 2),
	latex_binop(Op, _, _),
	!,
	write_semibrief_latex(T, K, B).
write_semibrief_latex('$defmacro'(T), _, B) :-
	!,
	( compound(T) ->
	  T =.. [F|Ts],
	  write_defmacro_functor_brief_latex(F, B),
	  write_arguments_semibrief_latex(Ts, B)
	; write_defmacro_functor_brief_latex(T, B)
	).
write_semibrief_latex('$VAR'(F), _, _) :-
	!,
	( number(F) ->
	  N1 is F+1,
	  format('\\pplparamnum{~w}', [N1])
	; current_pretty_functor(F, C, _) ->
	  ( is_multiletter(C) ->
	    format('\\pplparam{~w}', [C])
	  ; format('\\pplparamplain{~w}', [C])
	  )
	; atom_primes_postfix(F, C1, P) ->
	  ( atom_subscript_postfix(C1, C2, N) ->
	    ( is_multiletter(C2) ->
	      format('\\pplparamsupidx{~w}{~w}{~w}', [C2,P,N])
	    ; format('\\pplparamplainsupidx{~w}{~w}{~w}', [C2,P,N])
	    )
	  ; ( is_multiletter(C1) ->
	      format('\\pplparamsup{~w}{~w}', [C1, P])
	    ; format('\\pplparamplainsup{~w}{~w}', [C1, P])
	    )
	  )
	; atom_subscript_postfix(F, C, N) ->
	  ( is_multiletter(C) ->
	    format('\\pplparamidx{~w}{~w}', [C,N])
	  ; format('\\pplparamplainidx{~w}{~w}', [C,N])
	  )
	; ( is_multiletter(F) ->
	    format('\\pplparam{~w}', [F])
	  ; format('\\pplparamplain{~w}', [F])
	  )
	).  
write_semibrief_latex('#'(X,Y), _, B) :-
	!,
	write_semibrief_latex_binop('\\neq ', X, Y, 1000, B).
write_semibrief_latex(F, K, B) :-
	F =.. [Op,X,Y],
	latex_binop(Op, Op1, K1),
	!,
	( K1 >= K ->
	  write_semibrief_latex_binop_paren(Op1, X, Y, K1, B)
	; write_semibrief_latex_binop(Op1, X, Y, K1, B)
	).
write_semibrief_latex([], _, _) :-
	!,
	write('{[]}').
write_semibrief_latex([T|Ts], _, B) :-
 	!,
 	write('{[}'),
	write_list_latex_semibrief([T|Ts], B),
  	write('{]}').
write_semibrief_latex(lambda(X,F), _, B) :-
	!,
	write('\\lambda '),
	( atom(X) -> X1 = [X] ; X1 = X ),
	bound_add_variables(X1, B, B1),
	write_arguments_semibrief_latex(X1, B1),
	write('.'),
	last_options(Options),
	ppl(F, 1500, top, 0, true, B1, Options).
write_semibrief_latex(T, _, B) :-
	compound(T),
	!,
	T =.. [F|Ts],
	write_functor_brief_latex(F, B),
	write_arguments_semibrief_latex(Ts, B).
write_semibrief_latex(T, _, B) :-
	write_functor_brief_latex(T, B).


is_multiletter(X) :-
	atom(X),
	atom_length(X, L),
	L > 1,
	\+ sub_atom(X, 0, 1, _, '\\').

write_form_arguments_semibrief_latex(Ts, B) :-
	last_options(Options),
	write('('),
	( Ts = [T] ->
	  write_form_semibrief_latex(T, 2000, B, Options)
	; write_form_semibrief_latex_commaseq(Ts, B, Options)
	),
	write(')').  

write_form_semibrief_latex_commaseq([T], B, Options) :-
	!,
	write_form_semibrief_latex(T, 1000, B, Options).
write_form_semibrief_latex_commaseq([T|Ts], B, Options) :-
	!,
	write_form_semibrief_latex(T, 1000, B, Options),
	write(','),
	write_form_semibrief_latex_commaseq(Ts, B, Options).
write_form_semibrief_latex_commaseq(X, B, Options) :-
	write_form_semibrief_latex(X, 1000, B, Options).

write_form_semibrief_latex(F, N, B, Options) :-
	ppl(F, N, top, 0, true, B, [maxpos=10000|Options]).

write_arguments_semibrief_latex(Ts, B) :-
	write('('),
	( Ts = [T] ->
	  write_semibrief_latex(T, 2000, B)
	; write_semibrief_latex_commaseq(Ts, B)
	),
	write(')').  

write_semibrief_latex_commaseq([T], B) :-
	!,
	write_semibrief_latex(T, 1000, B).
write_semibrief_latex_commaseq([T|Ts], B) :-
	!,
	write_semibrief_latex(T, 1000, B),
	write(','),
	write_semibrief_latex_commaseq(Ts, B).
write_semibrief_latex_commaseq(X, B) :-
	write_semibrief_latex(X, 1000, B).

write_list_latex_semibrief([], _).
write_list_latex_semibrief([T|Ts], B) :-
	write_semibrief_latex(T, 1000, B),
	( Ts = [_|_] ->
	  write(','),
	  write_list_latex_semibrief(Ts, B)
	; Ts = [] ->
	  true
	; write('\\vert'),
	  write_semibrief_latex(Ts, 1000, B)
	).

write_brief_latex(F/T, K, B) :-
	%% support for tform syntax
	T =.. [t|Args],
	!,
	write_brief_latex('$varfunctor'(F, Args), K, B).
write_brief_latex('$vf'(F, Args), K, B) :-
	!,
	write_brief_latex('$varfunctor'(F, Args), K, B).
write_brief_latex('$varfunctor'(F, Args), K, B) :-
	!,
	( Args = [] ->
	  write_brief_latex(F, K, B)
	; write_brief_latex(F, K, B),
	  write_arguments_brief_latex(Args, F, B)
	).
write_brief_latex('$macro'(T), K, B) :-
	functor(T, Op, 2),
	latex_binop(Op, _, _),
	!,
	write_brief_latex(T, K, B).
write_brief_latex('$macro'(T), _, B) :-
	!,	
	( compound(T) ->
	  T =.. [F|Ts],
	  write_macro_functor_brief_latex(F, B),
	  write_form_arguments_brief_latex(Ts, F, B)
	; write_macro_functor_brief_latex(T, B)
	).
write_brief_latex('$defmacro'(T), K, B) :-
	functor(T, Op, 2),
	latex_binop(Op, _, _),
	!,
	write_brief_latex(T, K, B).
write_brief_latex('$defmacro'(T), _, B) :-
	!,
	( compound(T) ->
	  T =.. [F|Ts],
	  write_defmacro_functor_brief_latex(F, B),
	  write_arguments_brief_latex(Ts, F, B)
	; write_defmacro_functor_brief_latex(T, B)
	).
write_brief_latex('$VAR'(F), K, B) :-
	!,
	write_semibrief_latex('$VAR'(F), K, B).
write_brief_latex('#'(X,Y), _, B) :-
	!,
	write_brief_latex_binop('\\neq ', X, Y, 1000, B).
write_brief_latex(F, K, B) :-
	F =.. [Op,X,Y],
	latex_binop(Op, Op1, K1),
	!,
	( K1 >= K ->
	  write_brief_latex_binop_paren(Op1, X, Y, K1, B)
	; write_brief_latex_binop(Op1, X, Y, K1, B)
	).
write_brief_latex([], _, _) :-
	!,
	write('{[]}').
write_brief_latex([T|Ts], _, B) :-
	!,
	write('{[}'),
        write_list_latex_brief([T|Ts], B),      
	write('{]}').
write_brief_latex(lambda(X,F), _, B) :-
	!,
	write('\\lambda '),
	( atom(X) -> X1 = [X] ; X1 = X ),
	bound_add_variables(X1, B, B1),
	write_arguments_brief_latex(X, lambda, B),
	write('.'),
	last_options(Options),
	ppl(F, 1500, top, 0, true, B1, Options).
write_brief_latex(T, _, B) :-
	compound(T),
	!,
	T =.. [F|Ts],
	write_functor_brief_latex(F, B),
	write_arguments_brief_latex(Ts, F, B).
write_brief_latex(T, _, B) :-
	write_functor_brief_latex(T, B).

write_arguments_brief_latex(Ts, Functor, B) :-
	\+ var(Functor),
	( Functor='$VAR'(F1) -> true
	; F1 = Functor
	),
	( current_pretty_functor(F1, _, FunctorLength) ->
	  true
	; atom_primes_postfix(F1, C, _) ->
	  ( atom_subscript_postfix(C, C1, _) ->
	    atom_length(C1, FunctorLength)
	  ; atom_length(C, FunctorLength)
	  )
	; atom_subscript_postfix(F1, C, _) ->
	  atom_length(C, FunctorLength)
	; atom_length(F1, FunctorLength)
	),
	FunctorLength > 1,
	!,
	write_arguments_semibrief_latex(Ts, B).
write_arguments_brief_latex(Ts, _, B) :-
	( member(T1, Ts),
	  write_brief_latex(T1, 1000, B),
	  fail
	; true
	).

latex_binop(Op, Op1, K1) :-
	( current_pretty_op(K1, xfy, Op, Op1)
	; current_pretty_op(K1, xfx, Op, Op1)
	; current_pretty_op(K1, yfx, Op, Op1)
	),
	!.

write_form_arguments_brief_latex(Ts, Functor, B) :-
	\+ var(Functor),
	( Functor='$VAR'(F1) -> true
	; F1 = Functor
	),
	( current_pretty_functor(F1, _, FunctorLength) ->
	  true
	; atom_primes_postfix(F1, C, _) ->
	  ( atom_subscript_postfix(C, C1, _) ->
	    atom_length(C1, FunctorLength)
	  ; atom_length(C, FunctorLength)
	  )
	; atom_subscript_postfix(F1, C, _) ->
	  atom_length(C, FunctorLength)
	; atom_length(F1, FunctorLength)
	),
	FunctorLength > 1,
	!,
	write_form_arguments_semibrief_latex(Ts, B).
write_form_arguments_brief_latex(Ts, _, B) :-
	last_options(Options),
	( member(T1, Ts),
	  write_form_semibrief_latex(T1, 1000, B, Options),
	  fail
	; true
	).


write_list_latex_brief([], _).
write_list_latex_brief([T|Ts], B) :-
	write_brief_latex(T, 1000, B),
	( Ts = [_|_] ->
	  write(','),
	  write_list_latex_brief(Ts, B)
	; Ts = [] ->
	  true
	; write('\\vert'),
	  write_brief_latex(Ts, 1000, B)
	).

write_functor_brief_latex_1(Wrapper, F) :-
	( current_pretty_functor(F, C, _) ->
	  format(atom(S), '\\~w{~w}', [Wrapper, C])
	; atom_primes_postfix(F, C, P) ->
	  ( atom_subscript_postfix(C, C1, N) ->
	    latex_escape(C1, C2),
	    format(atom(S), '\\~w{~w^{~w}_{~w}}', [Wrapper, C2, P, N])
	  ; latex_escape(C, C1),
	    format(atom(S), '\\~w{~w^{~w}}', [Wrapper, C1, P])
	  )
	; atom_subscript_postfix(F, C, N) ->
	  latex_escape(C, C1),
	  format(atom(S), '\\~w{~w_{~w}}', [Wrapper, C1, N])
	; latex_escape(F, F1),	  
	  format(atom(S), '\\~w{~w}', [Wrapper, F1])
        ),
	write(S).

write_functor_brief_latex(F, B) :-
	( bound_is_bound(F, B) ->
	  write_functor_brief_latex_1(mathit, F)
	; write_functor_brief_latex_1(mathsf, F)
	).

write_macro_functor_brief_latex(F, _) :-
	write_functor_brief_latex_1(pplmacro, F).

write_defmacro_functor_brief_latex(F, _) :-
	write_functor_brief_latex_1(ppldefmacro, F).

latex_escape(A, A1) :-
	atom_chars(A, A2),
	map_lesc(A2, A3),
	atom_chars(A1, A3).

escapable_latex_special_char('_').
escapable_latex_special_char('&').
escapable_latex_special_char('$').
escapable_latex_special_char('#').
escapable_latex_special_char('{').
escapable_latex_special_char('}').

map_lesc([C|Cs], ['\\', C|Cs1]) :-
	escapable_latex_special_char(C),
	!,
	map_lesc(Cs, Cs1).
map_lesc([C|Cs], [C|Cs1]) :-
	map_lesc(Cs, Cs1).
map_lesc([], []).

write_brief(T) :-
	compound(T),
	!,
	T =.. [F|Ts],
	write(F),
	( member(T1, Ts),
	  write_brief(T1),
	  fail
	; true
	).
write_brief(T) :-
	write(T).

write_spaces(N) :-
	N >= 1,
	!,
	write(' '),
	N1 is N-1,
	write_spaces(N1).
write_spaces(_).

write_commalist_latex([X], B) :-
	!,
	write_semibrief_latex(X, 1000, B).
write_commalist_latex([X|Xs], B) :-
	!,
	write_semibrief_latex(X, 1000, B),
	write(','),
	write_commalist_latex(Xs, B).
write_commalist_latex([], _) :-
	!,
	true.
write_commalist_latex(X, B) :-
	write_semibrief_latex(X, 1000, B).

write_commalist_latex_brief([X], B) :-
	!,
	write_brief_latex(X, 1000, B).
write_commalist_latex_brief([X|Xs], B) :-
	!,
	write_brief_latex(X, 1000, B),
        write_commalist_latex_brief(Xs, B).
write_commalist_latex_brief([], _) :-
	!,
	true.		   
write_commalist_latex_brief(X, B) :-
	write_brief_latex(X, 1000, B).

write_commalist_brief([X]) :-
	!,
	write(X).
write_commalist_brief([X|Xs]) :-
	!,
	write(X),
        write_commalist_brief(Xs).
write_commalist_brief([]) :-
	true.

writeq1(X) :-
	context_module(Module),
	write_term(X, [ quoted(true), 
	                character_escapes(true),
			numbervars(true),
			spacing(next_argument),
			portray(true),
			module(Module) ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_nocommalist_latex([X], B) :-
	!,
	write_semibrief_latex(X, 1000, B).
write_nocommalist_latex([X|Xs], B) :-
	!,
	write_semibrief_latex(X, 1000, B),
	write(' '),
	write_nocommalist_latex(Xs, B).
write_nocommalist_latex([], _) :-
	!,
	true.
write_nocommalist_latex(X, B) :-
	write_semibrief_latex(X, 1000, B).
 
write_quantlist_latex([X], Q, Options, B) :-
	!,
	dry_pp_unary_op(Q, Options),
	write_semibrief_latex(X, 1000, B).
write_quantlist_latex([X|Xs], Q, Options, B) :-
	!,
	dry_pp_unary_op(Q, Options),
	write_semibrief_latex(X, 1000, B),
	write(' '),
	write_quantlist_latex(Xs, Q, Options, B).
write_quantlist_latex([], _, _, _) :-
	!,
	true.
write_quantlist_latex(X, Q, Options, B) :-
	dry_pp_unary_op(Q, Options),
	write_semibrief_latex(X, 1000, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

underscore_to_camel(T, T1) :-
	atom(T),
	!,
	utc(T, T1).
underscore_to_camel(T, T1) :-
	compound(T),
	!,
	T =.. [F|Ts],
	utc(F, F1),
	map_underscore_to_camel(Ts, Ts1),
	T1 =.. [F1|Ts1].
underscore_to_camel(T, T).

map_underscore_to_camel([X|Xs], [X1|Xs1]) :-
	underscore_to_camel(X, X1),
	map_underscore_to_camel(Xs, Xs1).
map_underscore_to_camel([], []).

utc(A, A1) :-
	atom_chars(A, A2),
	utc1(A2, A3),
	atom_chars(A1, A3).

utc1(['_',C|Cs], [C|Cs1]) :-
	char_type(C, digit),
	!,
	utc1(Cs, Cs1).
utc1(['_',C|Cs], [C1|Cs1]) :-
	char_type(C1,to_upper(C)),
	!,
	utc1(Cs, Cs1).
utc1([C|Cs], [C|Cs1]) :-
	utc1(Cs, Cs1).
utc1([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

atom_subscript_postfix(F, C, N) :-
	atom_chars(F, F1),
	reverse(F1, [N1|F2]),
	( F2 = ['_'|F3] ->
	  N = N1,
	  reverse(F3, C1),
	  atom_chars(C, C1)
	; char_type(N1, digit) ->
	  strip_digits(F2, N2, F3),
	  ( F3 = ['_'|F3a] -> true ; F3a = F3 ),
	  reverse(F3a, F4),
 	  atom_chars(C, F4),
	  reverse([N1|N2], N3),
	  number_chars(N, N3)
	; N1 = '}',
	  strip_curly(F2, 0, N2, F3),
	  ( F3 = ['_'|F3a] -> true ; F3a = F3 ),
	  reverse(F3a, F4),
 	  atom_chars(C, F4),
	  reverse(N2, N3),
	  atom_chars(N, N3)
	),
	!.

atom_primes_postfix(F, C, N) :-
	atom_chars(F, F1),
	reverse(F1, F2),
	appf(F2, F3, F4),
	!,
	reverse(F4, F5),
	atom_chars(C, F5),
	concat_atom(F3, N).

appf([p|Cs], ['\\prime'|Cs1], Cs2) :-
	!,
	appf(Cs, Cs1, Cs2).
appf(['_'|Cs], [], Cs) :-
	!.
	     
strip_digits([C|Cs], [C|Cs1], Cs2) :-
	char_type(C, digit),
	!,
	strip_digits(Cs, Cs1, Cs2).
strip_digits([], _, _) :-
	!,
	fail.
strip_digits(Cs, [], Cs).

strip_curly(['{'|Cs], 0, [], Cs) :-
	!.
strip_curly(['{'|Cs], N, ['{'|Cs1], Cs2) :-
	!,
	N1 is N-1,
	strip_curly(Cs, N1, Cs1, Cs2).	
strip_curly(['}'|Cs], N, ['}'|Cs1], Cs2) :-
	!,
	N1 is N+1,
	strip_curly(Cs, N1, Cs1, Cs2).
strip_curly([C|Cs], N, [C|Cs1], Cs2) :-
	strip_curly(Cs, N, Cs1, Cs2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bound_add_variables(X, Y, Z) :-
	( is_full_list(Y) ->
	  append(Y, X, Z)
	; Z = [Y|X]
	).

is_full_list(X) :- var(X), !, fail.
is_full_list([_|Y]) :-
	is_full_list(Y).
is_full_list([]).

bound_is_bound(X, Y) :-
	member(A, Y), X == A, !.

bound_new([]).

bound_new(X, X).
