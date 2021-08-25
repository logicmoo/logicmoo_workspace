/******************************************************************************
    This file is being distributed, by written permission of Quintus 
    Corporation, for use with the BACK system only.  This file may not
    be used with Prolog implementations other than Quintus Prolog except
    (a) as part of the BACK system, or (b) with the written permission
    of Quintus Corporation.  To obtain such written permission, please
    contact:

	Quintus Corporation
	2100 Geng Road
	Palo Alto,
	California  94303
	USA
	415-813-3800
	marketing@quintus.com
******************************************************************************/

%   Package: add_portray
%   Author : Richard A. O'Keefe
%   Updated: 11/20/89
%   Purpose: let you define clauses for portray in modules.

%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(add_portray, [
	add_portray/1,			% for managing portray/1
	del_portray/1,
	add_expansion/1,		% for managing term_expansion/2
	del_expansion/1,
	add_linking_clause/3,		% for your own similar predicates
	del_linking_clause/3
   ]).

:- meta_predicate
	add_portray(1),
	del_portray(1),
	add_expansion(2),
	del_expansion(2),
	add_linking_clause(:, +, +),
	del_linking_clause(:, +, +).

:- use_module(library(types)).

sccs_id('"@(#)89/11/21 addportray.pl	200.3"').

/*  In Dec-10 Prolog, or C Prolog, a program could contain clauses
    like
	portray(X) :-
		should_be_handled_here(X),
		print_it_this_way(X).
    scattered through any number of files.  In Quintus Prolog, this
    does not work, because each file will wipe out every other file's
    clauses for portray/1, and of course a clause for portray/1 in a
    module won't do anything at all for you.

    What you can do now is this:
	:- use_module(library(add_portray)).

	local_portray(X) :-
		should_be_handled_here(X),
		print_it_this_way(X).

	:- add_portray(local_portray).

    To cancel such a link, you can call
	:- del_portray(local_portray).

    Note that if you use this package, you should not define portray/1
    any other way, otherwise you'll lose these links.

    You can link to other predicates this way too.  Suppose the other
    predicate to be linked to is user:Pred/Arity.  Then
	:- add_linking_clause(Link, Pred, Arity).
    ensures that there is a clause
	Pred(X1,...,Xarity) :- Link(X1,...,Xarity).
    in module user:, where the call to Link/Arity calls the predicate
    Link/Arity in the source module of the call to add_linking_clause,
    and
	:- del_linking_clause(Link, Pred, Arity).
    ensures that there is no such clause.  For example, suppose you
    want to add a case to term_expansion/2.  You could do
	:- add_linking_clause(local_expander, term_expansion, 2).

    I am not happy about the form of Pred/Arity specification which
    these two commands take.  Properly, they should look like goals.
    There would be no difficulty in accepting a goal (minus its
    user: module prefix) as second argument of add_linking_clause;
    the trouble is del_linking_clause.
*/

%   add_portray(+SymbolToBeCalled)
%   ensures that user:portray/1 will call your predicate (which may be
%   local to a module) by ensuring that there is a clause
%	portray(X) :- <module:>SymbolToBeCalled(X).

add_portray(Link) :-
	add_linking_clause(Link, portray, 1, add_portray(Link)).


%   del_portray(+SymbolToBeCalled)
%   cancels the effect of a previous call to add_portray/1.

del_portray(Link) :-
	del_linking_clause(Link, portray, 1, del_portray(Link)).



%   add_expansion(+SymbolToBeCalled)
%   ensures that user:term_expansion/2 will call your predicate (which may
%   be local to a module) by ensuring that there is a clause
%	term_expansion(X) :- <module:>SymbolToBeCalled(X).

add_expansion(Link) :-
	add_linking_clause(Link, term_expansion, 2, add_expansion(Link)).


%   del_expansion(+SymbolToBeCalled)
%   cancels the effect of a previous call to add_expansion/1.

del_expansion(Link) :-
	del_linking_clause(Link, term_expansion, 2, del_expansion(Link)).



%   add_linking_clause(+Caller, +SymbolToBeCalled, +Arity)
%   ensures that user:Link/Arity will call your predicate (which may be
%   local to a module) by ensuring that there is a clause
%	Link(X1,...) :- <module:>SymbolToBeCalled(X1,...).

add_linking_clause(Link, Pred, Arity) :-
	add_linking_clause(Link, Pred, Arity,
	    add_linking_clause(Link,Pred,Arity)).


%   del_linking_clause(+Caller, +SymbolToBeCalled, +Arity)
%   cancels the effect of a previous call to add_linking_clause/3.

del_linking_clause(Link, Pred, Arity) :-
	del_linking_clause(Link, Pred, Arity,
	    del_linking_clause(Link,Pred,Arity)).



add_linking_clause(Link, Pred, Arity, Goal) :-
	linking_clause(Link, Pred, Arity, Goal, user, Head, Body),
	(   user:clause(Head, Body) -> true
	;   user:assert((Head :- Body))
	).


del_linking_clause(Link, Pred, Arity, Goal) :-
	linking_clause(Link, Pred, Arity, Goal, user, Head, Body),
	(   user:retract((Head :- Body)) -> true
	;   true
	).



linking_clause(Module:Link, Pred, Arity, Goal, _, Head, Body) :-
	atom(Module),
	!,
	linking_clause(Link, Pred, Arity, Goal, Module, Head, Body).
linking_clause(Link, Pred, Arity, Goal, Module, Head, Body) :-
	atom(Link),
	!,
	must_be_symbol(Pred, 2, Goal),
	must_be_nonneg(Arity, 3, Goal),
	functor(Head, Pred, Arity),
	functor(Call, Link, Arity),
	same_arguments(Arity, Head, Call),
	(   Module = user -> Body = Call
	;   Body = Module:Call
	).
linking_clause(Link, _, _, Goal, _, _, _) :-
	must_be_symbol(Link, 1, Goal).


same_arguments(N, Head, Call) :-
    (	N =< 0 -> true
    ;	arg(N, Head, Arg),
	arg(N, Call, Arg),
	M is N-1,
	same_arguments(M, Head, Call)
    ).

