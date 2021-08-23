
:- module(brief_syntax,
	  [ set_default_brief_options/1,
	    default_brief_options/2,
	    brief_to_form/2,
	    brief_to_form/3 ]).


:- use_module(swilib(options)).
:- use_module(swilib(info)).
:- use_module(swilib(err)).

:- dynamic default_brief_options_1/2.

set_default_brief_options(Options) :-
	( prolog_load_context(source, Source) ->
	  true
	; Source = user
	),
	retractall( default_brief_options_1(Source, _) ),
	basic_default_brief_options(BOptions),
	default_options(Options, BOptions, Options1),
	asserta(  default_brief_options_1(Source, Options1) ).

basic_default_brief_options(Options) :-
	Options =
	[predicates=[p,q,r,s],
	 constants=[x,y,z,u,v,w,a,b,c,d],
	 functions=[f/1,g/1,h/1],
	 prt=false,
	 into_compounds=false,
	 is_defined_elsewhere_callback=none].
	
:- set_default_brief_options([]).

default_brief_options(Source, Options) :-
	( default_brief_options_1(Source, Options) -> true
	; default_brief_options_1(_, Options)
	).

brief_to_form(F, F1) :-
	default_brief_options(_, Options),
	brief_to_form(F, Options, F1).

brief_to_form(F, Options, F1) :-
	brief_to_form_1(F, Options, F1),
	( from_options(prt=true, Options) ->
	  \+ \+ ( numbervars(F1,1,_), format('% Expanded form: ~q~n', [F1]) )
	; true
	).


brief_to_form_1(F, Options, F1) :-
	compound(F),
	functor(F, Op, N),
	log_op(Op, N, TgtOp),
	!,
	F =.. [Op|Args],
	map_brief_to_form_1(Args, Options, Args1),
	F1 =.. [TgtOp|Args1].
brief_to_form_1(F, Options, F1) :-
	compound(F),
	functor(F, Op, 2),
	log_quantifier(Op, 2),
	!,
	F =.. [Op,Upon,Arg],
	brief_to_form_1(Arg, Options, Arg1),
	parse_upon_spec(Upon, Options, Upon1),
	F1 =.. [Op,Upon1,Arg1].
brief_to_form_1(false, _, false) :-
	!.
brief_to_form_1(true, _, true) :-
	!.
brief_to_form_1(F, Options, (A1 = B1)) :-
	F = (A = B),
	!,
	parse_logic_term(A, Options, A1),
	parse_logic_term(B, Options, B1).
brief_to_form_1(F, Options, ~(A1 = B1)) :-
	( F = '#'(A,B)
	; F = (A \= B)
	),
	!,
	parse_logic_term(A, Options, A1),
	parse_logic_term(B, Options, B1).
brief_to_form_1(F, Options, F1) :-
	atom(F),
	!,
	parse_logic_atom(F, Options, F1).
brief_to_form_1(F, Options, F1) :-
	functor(F, _, N),
	N >= 1,
	from_options(into_compounds=true, Options, false),
	!,
	F =.. [Functor|Args],
	map_brief_to_form_1(Args, Options, Args1),
	F1 =.. [Functor|Args1].
%% accept logic atoms in "normal" syntax too:
brief_to_form_1(F, _, F).

log_op('&', 2, ',').
log_op('v', 2, ';').
log_op('|', 2, ';').
log_op(X, N, X) :-
	log_op(X, N). 

log_op('->', 2).
log_op('<-', 2).
log_op('<->', 2).
log_op(',', 2).
log_op(';', 2).
log_op('~', 1).

log_quantifier(ex, 2).
log_quantifier(all, 2).
log_quantifier(ex2, 2).
log_quantifier(all2, 2).

map_brief_to_form_1([X|Xs], Y1, [X1|Xs1]) :-
	brief_to_form_1(X, Y1, X1),
	map_brief_to_form_1(Xs, Y1, Xs1).
map_brief_to_form_1([], _, []).

parse_logic_atom(S, Options, LogicAtom) :-
	atom_codes(S, S1),
	( phrase(logic_atom(Options, LogicAtom), S1, []) ->
	  true
	; from_options(is_defined_elsewhere_callback=Predicate, Options, none),
	  ( Predicate \= none ->
	    Call =.. [Predicate,S],
	    ( call(Call) ->
	      true
	    ; info(10, 'Warning: undeclared logic atom in brief syntax: ~q', [S])
	    )
	  ; info(10, 'Warning: undeclared logic atom in brief syntax: ~q', [S])
	  ),
	  LogicAtom = S
	).

parse_logic_term(S, Options, LogicTerm) :-
	atom_codes(S, S1),
	( phrase(term(Options, LogicTerm), S1, []) ->
	  true
	; LogicTerm = S
	).

parse_upon_spec(S, Options, Upon) :-
	atom(S),
	!,
	atom_codes(S, S1),
	( phrase(upon_spec(Options, Upon), S1, []) ->
	  true
	; err('Failed to parse brief upon specifier: ~q', [S])
	).
%% accept non-atoms (lists) in "normal" syntax too:
parse_upon_spec(S, _, S).
	
pred_char_1(Options, C) :-
	from_options(predicates=Preds, Options),
	memberchk(C, Preds).

const_char_1(Options, C) :-
	from_options(constants=Constants, Options),
	memberchk(C, Constants).

fun_char_1(Options, C, Arity) :-
	from_options(functions=Functions, Options),
	memberchk(C/Arity, Functions).

logic_atom(O, A) -->
 	pred_functor(O, F),
	args(O, Terms),
	!,
	{ A =.. [F|Terms] }.

term(O, T) -->
	const_functor(O, T),
	!.
term(O, T) -->
	fun_functor(O, F, Arity),
	terms(O, Arity, Terms),
	{ T =.. [F|Terms] }.

terms(O, N, [T|Ts]) -->
	{ N > 0 },
	!,
	term(O, T),
	{ N1 is N - 1 },
	terms(O, N1, Ts).
terms(_, 0, []) -->
	[].

args(O, [T|Ts]) --> term(O, T), !, args(O, Ts).
args(_, []) --> [].
	
pred_functor(O, F) -->
	pred_char(O, C),
	natnum(D), { concat_atom([C, D], F) },
	!.
pred_functor(O, F) -->
	pred_char(O, F).

fun_functor(O, F, Arity) -->
	fun_char(O, C, Arity),
	natnum(D), { concat_atom([C, D], F) },
	!.
fun_functor(O, F, Arity) -->
	fun_char(O, F, Arity).

const_functor(O, F) -->
	const_char(O, C),
	natnum(D), { concat_atom([C, D], F) },
	!.
const_functor(O, F) -->
	const_char(O, F).

upon_spec(Options, [V|Vs]) -->
	const_functor(Options, V),
	!,
	upon_spec(Options, Vs).
upon_spec(Options, [V|Vs]) -->
	pred_functor(Options, V),
	!,
	upon_spec(Options, Vs).
upon_spec(_, []) -->
	[].

fun_char(O, C, Arity) --> [D], { char_code(C, D), fun_char_1(O, C, Arity) }.
pred_char(O, C) --> [D], { char_code(C, D), pred_char_1(O, C) }.
const_char(O, C) --> [D], { char_code(C, D), const_char_1(O, C) }.

/*
sym(Symbols, [C|Cs]) -->
	[D],
	{ char_code(C, D),
	  writeq(strip_first(Symbols, C, Symbols1)), nl,
	  strip_first(Symbols, C, Symbols1),
	  Symbols1 \= []
	},
	!,
	sym(Symbols1, Cs).
sym([''], []) -->
	[].
*/

strip_first([S|Symbols], C, [S1|Symbols1]) :-
	atom_prefix(S, C),
	!,
	sub_atom(S, 1, _, 0, S1),
	strip_first(Symbols, C, Symbols1).
strip_first([_|Symbols], C, Symbols1) :-
	strip_first(Symbols, C, Symbols1).
strip_first([], _, []).
	

natnum(D) -->
	digit(D0),
	digits(T),
	{ number_codes(D, [D0|T])
	}.

digit(D) --> [D], { code_type(D, digit)	}.
digits([D|T]) --> digit(D), !, digits(T).
digits([]) --> [].
