/*************************************************************************

        name: isis1_datatypes.pl 
     version: 
 description: 
      author: Staffan Larsson
 
*************************************************************************/

:- multifile is_type/1, of_type/2, empty_object/2, operation/4, relation/2, function/3, selector/5.
:- discontiguous is_type/1, of_type/2, operation/4, relation/2, function/3, selector/5.

:- use_module(library(terms)).


% THIS FILE IS INCOMPLETE!!!
%
% Current trindikit does not need full type specs.



/*----------------------------------------------------------------------
     dmove -- dialogue move
----------------------------------------------------------------------*/

% Social
of_type( quit, move ).
of_type( greet, move ).


% Q&A
of_type( answer(R), move ) :-
	of_type( R, proposition );
	of_type( R, answer ).

of_type( ask(Q), move ) :-
	of_type( Q, question ) .


/*----------------------------------------------------------------------
     action
----------------------------------------------------------------------*/

is_type( action ).

of_type( quit, action ).
of_type( greet, action ).
of_type( respond(Q), action ) :-
	of_type( Q, question ).
of_type( findout(Q), action ) :-
	of_type( Q, question ).

/*----------------------------------------------------------------------
     question
----------------------------------------------------------------------*/

is_type( question ).

of_type( Q, question ):-
	( of_type( Q, ynq );
	    of_type( Q, whq ) ).

is_type(whq).

of_type( X^P, whq ) :-
	var(X),
	nonvar(P),
	term_variables(P, [X]),
	P =.. [Pred|Args],
	of_type(Pred, pred),
	delete(Args, X, Inds),
	inds(Inds).

is_type( ynq ).

of_type( P, ynq ) :-
	of_type( P, proposition ).


/*----------------------------------------------------------------------
     proposition
----------------------------------------------------------------------*/

is_type( proposition ).

of_type( P, proposition ):-
	nonvar( P ),
	( atom(Pred);
	    ( P =.. [Pred|Args],
		predicate(Pred),
		inds(Args) ) ).

% predicates
	    
predicate(Pred):-atom(Pred).

% individuals

ind(Ind):-atom(Ind).

inds([]).
inds([Ind|Inds]):-
	ind(Ind),
	inds(Inds).

/*----------------------------------------------------------------------
     answer (shortAns)
----------------------------------------------------------------------*/

is_type( answer ).

of_type( A, answer ):-
	( A == yes;
	    (A == no;
		ind(A) ) ).


/*----------------------------------------------------------------------
     language
----------------------------------------------------------------------*/

% Social
of_type( english, language ).
of_type( svenska, language ).

/*----------------------------------------------------------------------
     contect free grammars
----------------------------------------------------------------------*/
is_type( cfg_production_rule).
of_type( rule(LHS,RHS), cfg_production_rule) :-
	 cfg_nonterminal(LHS),
	 cfg_symbols(RHS).

cfg_nonterminal(nonterminal(C)):-
	atom(C).
cfg_terminal(terminal(Str)):-
	is_list(Str).


cfg_symbols([]).
cfg_symbols([S|Ss]):-
	cfg_symbol(S),
	cfg_symbols(Ss).

cfg_symbol(S):-
	cfg_nonterminal(S).
cfg_symbol(S):-
	cfg_terminal(S).