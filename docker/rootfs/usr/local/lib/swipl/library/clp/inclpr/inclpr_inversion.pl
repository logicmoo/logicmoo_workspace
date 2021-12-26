/*  Part of INCLP(R)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2011, K.U. Leuven
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(inclpr_inversion,
	[
	    all_occurrences/1,
	    invert/3
	]).
:- use_module(library(lists),
	[
	    reverse/2
	]).

% Module for creating the inverse of functions with one occurrence per variable
% and for counting the number of variable occurrences.

% invert(Function,Variable,Inverse)
%
% Creates the inverse of function <Function> with respect to variable
% <Variable> and stores it into <Inverse>. This means that a constraint
% <Function> = 0 is transformed into an equivalent constraint
% <Variable> = <Inverse>.

invert(Term,Var,Inverse) :-
	get_attr(Var,inclpr_occurrence_count,one(ReversePath)),
	reverse(ReversePath,Path),
	invert(Term,Path,i(-0.0,0.0),Inverse).

% invert(Function,Path,InverseIn,InverseOut)
%
% Converts the constraint <Function> = <InverseIn> into the equivalent
% constraint <Variable> = <InverseOut> where <Variable> is located by following
% the path given by <Path>.
% A path is a list of 1's and 2's and represents where in the tree formed by
% binary operators the variable is located. For example if <Path> is the list
% [1,2] and <Function> is (A+B)*(C+D) then first the subfunction A+B is chosen
% because the first element of path is 1 and then B is chosen because the
% second element of path is 2. Paths are built while counting the number of
% occurrences for each variable.

invert(Term,Path,InverseIn,InverseOut) :-
	(   var(Term)
	->  InverseOut = InverseIn
	;   functor(Term,Op,Arity),
	    (   Arity =:= 2
	    ->  Path = [H|T],
		arg(1,Term,L),
		arg(2,Term,R),
		(   H = 1
		->  invert_binary_left(Op,InverseIn,R,InverseTemp),
		    invert(L,T,InverseTemp,InverseOut)
		;   invert_binary_right(Op,L,InverseIn,InverseTemp),
		    invert(R,T,InverseTemp,InverseOut)
		)
	    ;	arg(1,Term,X),
		invert(X,Path,-InverseIn,InverseOut)
	    )
	).

% invert_binary_left(Operator,InverseIn,Right,InverseOut)
%
% Converts the constraint <Function> = <InverseIn> where <Function> is composed
% of binary operator <Operator> applied to arguments <Left> and <Right> into an
% equivalent constraint <Left> = <InverseOut>.

invert_binary_left(+,L,R,L-R).
invert_binary_left(-,L,R,L+R).
invert_binary_left(*,L,R,L/R).
invert_binary_left(/,L,R,L*R).
invert_binary_left(**,L,R,root(L,R)).

% invert_binary_right(Operator,Left,InverseIn,InverseOut)
%
% Converts the constraint <Function> = <InverseIn> where <Function> is composed
% of binary operator <Operator> applied to arguments <Left> and <Right> into an
% equivalent constraint <Right> = <InverseOut>.

invert_binary_right(+,L,R,R-L).
invert_binary_right(-,L,R,L-R).
invert_binary_right(*,L,R,R/L).
invert_binary_right(/,L,R,L/R).

% all_occurrences(Function)
%
% Counts all occurrences of all variables in <Function> and stores them in
% the attribute inclpr_occurrence_count of each variable. The attribute either
% is the term `one(ReversePath)' with <ReversePath> the reverse of a path (see
% invert/4) or the term `more' for variables occurring more than once.
% Expects that all functors are either unary or binary

all_occurrences(Term) :- all_occurrences(Term,[]).

% all_occurrences(Function,ReversePath)
%
% The same as all_occurrences/1 but with a given initial reverse path in
% <ReversePath>.
		
all_occurrences(Term,ReversePath) :-
	(   var(Term)
	->  (   get_attr(Term,inclpr_occurrence_count,_)
	    ->  put_attr(Term,inclpr_occurrence_count,more)
	    ;   put_attr(Term,inclpr_occurrence_count,one(ReversePath))
	    )
	;   number(Term)
	->  true
	;   functor(Term,_,Arity),
	    (   Arity =:= 2
	    ->  arg(1,Term,Arg1),
		arg(2,Term,Arg2),
		all_occurrences(Arg1,[1|ReversePath]),
		all_occurrences(Arg2,[2|ReversePath])
	    ;	arg(1,Term,Arg),
		all_occurrences(Arg,ReversePath)
	    )
	).