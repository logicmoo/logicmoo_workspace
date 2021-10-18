/*
	DCG Term Expansion
	
	Modified translation of DCGs:
	Prolog Code inside {..} and Terminals are wrapped in 'P'/3 resp. 'T'/3 predicates. 
	
	Usage: 
	Since a term-expansion doesn't work beyond module boundaries, 
	this has to be initialised in a Module which reaches the DCG.
	
	term_expansion(R, R0) :-
		dcg_hashexp:dcg_expansion(R, R0).
*/
:- module(dcg_hashexp,
	[
		dcg_expansion/2,
		'P'/3,
		'T'/3
	]).

/*
	Load modified version SWI module '$dcg'.
	dcg_hashexp relies on this
*/
:- use_module(dcg_hashswi).
:- use_module(dcg_hashlib).

/*
	Modified DCG Expansion
	
	Terminals and {..} fragments are replaced. 
*/
dcg_expansion((H --> B), (H0 :- B1)) :-
	exp(B, B0),
	dcg_translate_rule_((H --> B0), (H0 :- B1)).

/*
	Expand grammar bodies
	
	Recognising Terminals:
	[..] -> 'T'([..])
	
	Recognising {..} Prolog Code:
	{..} -> 'P'(..)
	
	Recognising Strings:
	".." -> 'T'([.,.])
	
	This doesn't change the codes behaviour, 
	but makes it possible to identify these fragmens later on.
	
	Future Work:
	- changes behaviour of cut (!) (since 'P' is opaque for the cut..)
*/
exp({A}, 'P'(A)).
exp(A, 'T'(A0)) :-
	string(A),
	string_chars(A, A0).
exp(A, 'T'(A)) :-
	is_list(A).
exp(A, A) :-
	A \= {_},
	A \= (_,_),
	\+is_list(A). 
exp((A,B), (A0, B0)) :-
	exp(A, A0),
	exp(B, B0).	

/*
	Wrapper for {...} (Plain Prolog code)
	
	Goals are checked using safe_goal/1, allows for white-listed built-in predicates
*/
'P'(Prolog, In, Rest) :-
	safe_goal(Prolog),
	call(Prolog),
	In = Rest.

/*
	Wrapper for Terminals
*/
'T'(Terminal, In, Rest) :- 
	append(Terminal, Rest, In).

