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

:- module(inclpr_symbolic_processing,
	[
	    partial_derivative/3,
	    to_standard_form/2
	]).

% Module for converting constraints into a standard form and for calculating
% partial derivatives.

% public predicates

% partial_derivative(Function,Variable,Derivative)
%
% Returns in <Derivative> the partial derivative of function <Function> with
% respect to variable <Variable>.

partial_derivative(Expr,Var,D) :-
	pd(Expr,Var,Res),
	rewrite(Res,D).

% to_standard_form(Constraint,StandardForm)
%
% Rewrites the constraint in <Constraint> into the form Res = 0.0 (for
% equalities) or Res =< 0.0 (for inequalities).

to_standard_form(L = R,Res = 0.0) :- rewrite(L-R,Res).
to_standard_form(L =< R, Res =< 0.0) :- rewrite(L-R,Res).
to_standard_form(L >= R, Res =< 0.0) :- rewrite(R-L,Res).

% private predicates

% pd(Function,Var,Derivative)
%
% Returns in <Derivative> the partial derivative of function <Function> with
% respect to variable <Variable>.

pd(V,Var,D) :-
	(   var(V)
	->  (   V == Var
	    ->  D = 1.0
	    ;   D = 0.0
	    )
	;   number(V)
	->  D = 0.0
	;   functor(V,Op,Arity),
	    (   Arity =:= 2
	    ->  arg(1,V,L),
		arg(2,V,R),
		pd_binary(Op,L,R,Var,D)
	    ;   arg(1,V,X),
		pd_unary(Op,X,Var,D)
	    )
	).

% pd_binary(Operator,Left,Right,Variable,Derivative)
%
% Returns in <Derivative> the partial derivative with respect to variable
% <Variable> of the function made of the binary operator <Operator> applied to
% functions <Left> and <Right>.

pd_binary(+,L,R,Var,DL+DR) :-
	pd(L,Var,DL),
	pd(R,Var,DR).
pd_binary(-,L,R,Var,DL-DR) :-
	pd(L,Var,DL),
	pd(R,Var,DR).
pd_binary(*,L,R,Var,L*DR+R*DL) :-
	pd(L,Var,DL),
	pd(R,Var,DR).
pd_binary((^),X,N,Var,PD) :- pd_binary((**),X,N,Var,PD).
pd_binary((**),X,N,Var,PD) :-
	integer(N),
	pd(X,Var,PDX),
	M is N-1,
	(   M =:= 0
	->  PD = PDX
	;   M =:= 1
	->  PD = 2*X*PDX
	;   PD = N*(X**M)*PDX
	).

% pd_unary(Operator,Argument,Variable,Derivative)
%
% Returns in <Derivative> the partial derivative with respect to variable
% <Variable> of the function made of the unary operator <Operator> applied to
% the function <Argument>.

pd_unary(-,L,Var,-DL) :-
	pd(L,Var,DL).

% rewrite(Function,Rewritten)
%
% Rewrites function <Function> into an equivalent function <Rewritten> by
% applying some simplifications.

rewrite(Term,RW) :-
	(   var(Term)
	->  RW = Term
	;   number(Term)
	->  RW = Term
	;   functor(Term,Op,Arity),
	    (   Arity =:= 2
	    ->  arg(1,Term,L),
		arg(2,Term,R),
		rewrite_binary(Op,L,R,RW)
	    ;	arg(1,Term,X),
		rewrite_unary(Op,X,RW)
	    )
	).

% rewrite_binary(Operator,Left,Right,Rewritten)
%
% Rewrites the function made of binary operator <Operator> applied to functions
% <Left> and <Right> into the equivalent but more simple function <Rewritten>.

rewrite_binary(+,X,Y,Z) :-
	rewrite(X,Xr),
	rewrite(Y,Yr),
	(   number(Xr)
	->  (   number(Yr)
	    ->  Z is Xr + Yr
	    ;   Xr =:= 0.0
	    ->  Z = Yr
	    ;   nonvar(Yr),
		Yr = -Ym
	    ->  Z = Xr - Ym
	    ;   Z = Xr + Yr
	    )
	;   number(Yr)
	->  (   Yr =:= 0.0
	    ->  Z = Xr
	    ;   Yr < 0
	    ->  Ym is - Yr,
		Z = Xr - Ym
	    ;   Z = Xr + Yr
	    )
	;   nonvar(Yr),
	    Yr = -Ym
	->  Z = Xr - Ym
	;   Z = Xr + Yr
	).
rewrite_binary(-,X,Y,Z) :-
	rewrite(X,Xr),
	rewrite(Y,Yr),
	(   number(Xr)
	->  (   number(Yr)
	    ->  Z is Xr - Yr
	    ;   Xr =:= 0
	    ->  (   nonvar(Yr),
		    Yr = -Ym
		->  Z = Ym
		;   Z = -Yr
		)
	    ;   nonvar(Yr),
		Yr = -Ym
	    ->  Z = Xr + Ym
	    ;   Z = Xr - Yr
	    )
	;   number(Yr)
	->  (   Yr =:= 0.0
	    ->  Z = Xr
	    ;   Yr < 0
	    ->  Ym is -Yr,
		Z = Xr + Ym
	    ;   Z = Xr - Yr
	    )
	;   nonvar(Yr),
	    Yr = -Ym
	->  Z = Xr + Ym
	;   Z = Xr - Yr
	).
rewrite_binary(*,X,Y,Z) :-
	rewrite(X,Xr),
	rewrite(Y,Yr),
	(   number(Xr)
	->  (   number(Yr)
	    ->  Z is Xr * Yr
	    ;   Xr =:= 0.0
	    ->  Z = 0.0
	    ;   Xr =:= 1.0
	    ->  Z = Yr
	    ;   Xr =:= -1.0
	    ->  (   nonvar(Yr),
		    Yr = -Ym
		->  Z = Ym
		;   Z = -Yr
		)
	    ;   Xr < 0
	    ->  Xm is -Xr,
		(   nonvar(Yr),
		    Yr = -Ym
		->  Z = Xm * Ym
		;   Z = -(Xm * Yr)
		)
	    ;   nonvar(Yr),
		Yr = -Ym
	    ->  Z = -(Xr * Ym)
	    ;   Z = Xr * Yr
	    )
	;   number(Yr)
	->  (   Yr =:= 0.0
	    ->  Z = 0.0
	    ;   Yr =:= 1.0
	    ->  Z = Xr
	    ;   Yr =:= -1.0
	    ->  (   nonvar(Xr),
		    Xr = -Xm
		->  Z = Xm
		;   Z = -Xr
		)
	    ;   Yr < 0
	    ->  Ym is -Yr,
		(   nonvar(Xr),
		    Xr = -Xm
		->  Z = Xm * Ym
		;   Z = -(Xr * Ym)
		)
	    ;   nonvar(Xr),
		Xr = -(Xm)
	    ->  Z = -(Xm * Yr)
	    ;   Z = Xr * Yr
	    )
	;   nonvar(Xr),
	    Xr = -Xm
	->  (   nonvar(Yr),
		Yr = -Ym
	    ->  Z = Xm * Ym
	    ;   Z = -(Xm * Yr)
	    )
	;   nonvar(Yr),
	    Yr = -Ym
	->  Z = -(Xr * Ym)
	;   Z = Xr * Yr
	).
rewrite_binary(/,X,Y,Z) :-
	rewrite(X,Xr),
	rewrite(Y,Yr),
	(   number(Xr)
	->  (   number(Yr)
	    ->  Yr =\= 0.0,
		Z is Xr / Yr
	    ;   Xr =:= 0.0
	    ->  Z = 0.0
	    ;   Xr < 0.0
	    ->  Xm is -Xr,
		(   nonvar(Yr),
		    Yr = -Ym
		->  Z = Xm / Ym
		;   Z = -(Xm / Yr)
		)
	    ;   nonvar(Yr),
		Yr = -Ym
	    ->  Z = -(Xr / Ym)
	    ;   Z = Xr / Yr
	    )
	;   number(Yr)
	->  Yr =\= 0.0,
	    (   Yr =:= 1.0
	    ->  Z = Xr
	    ;   Yr =:= -1.0
	    ->  (   nonvar(Xr),
		    Xr = -Xm
		->  Z = Xm
		;   Z = -Xr
		)
	    ;   Yr < 0.0
	    ->  Ym is -Yr,
		(   nonvar(Xr),
		    Xr = -Xm
		->  Z = Xm / Ym
		;   Z = -(Xr / Ym)
		)
	    ;   nonvar(Xr),
		Xr = -Xm
	    ->  Z = -(Xm / Yr)
	    ;   Z = Xr / Yr
	    )
	;   nonvar(Xr),
	    Xr = -Xm
	->  (   nonvar(Yr),
		Yr = -Ym
	    ->  Z = Xm / Ym
	    ;   Z = -(Xm / Yr)
	    )
	;   nonvar(Yr),
	    Yr = -Ym
	->  Z = -(Xr / Ym)
	;   Z = Xr / Yr
	).
rewrite_binary(^,X,N,Z) :- rewrite_binary(**,X,N,Z).
rewrite_binary(**,X,N,Z) :-
	integer(N),
	rewrite(X,Xr),
	(   number(Xr)
	->  Z is Xr ** N
	;   nonvar(Xr),
	    Xr = -Xm
	->  (   N mod 2 =:= 0
	    ->  Z = Xm ** N
	    ;   Z = -(Xm ** N)
	    )
	;   Z = Xr ** N
	).

% rewrite_unary(Operator,Argument,Rewritten)
%
% Rewrites the function made of unary operator <Operator> applied to function
% <Argument> into the equivalent but more simple function <Rewritten>.

rewrite_unary(-,X,Z) :-
	rewrite(X,Xr),
	(   number(Xr)
	->  Z is -Xr
	;   nonvar(Xr),
	    Xr = -Xm
	->  Z is Xm
	;   Z = -Xr
	).