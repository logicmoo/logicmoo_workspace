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

:- module(inclpr_newton,
	[
	    interval_newton/6
	]).
:- use_module(inclpr_consistency,
	[
	    get_full_evaluation/5
	]).
:- use_module(inclpr_interval_arithmetic,
	[
	    interval_center/2,
	    interval_contains/2,
	    interval_new_class/2,
	    interval_intersection/3,
	    interval_div/3,
	    interval_exclude_zero_bound/2,
	    interval_split_excluding_zero/3,
	    interval_minus/3,
	    interval_union/3
	]).

% Module for calculating an Interval Newton domain reduction step.

% interval_newton(IntervalExtension,FunctionPartialEvaluation,
%	DerivativePartialEvaluation,Variable,Interval,Answer)
%
% Computes an Interval Newton step for the function with partially evaluated *
% interval extension <FunctionPartialEvaluation> and partially evaluated *
% interval extension of the derivative <DerivativeIntervalExtension>, with
% respect to variable <Variable> which has initial domain <Interval>. The
% result is returned in <Answer> and is one of the following:
%
% nr(d):   no domain reduction because the result was larger than the initial
%	   domain.
% nr(f):   no domain reduction because we failed to apply the Interval Newton
%	   operator because of a shared zero between the function range for
%	   <Variable> in the center of <Interval> and the derivative range for
%	   <Variable> in <Interval>.
% r(D):    domain reduction: the reduced domain is returned in <D>.
% sr(L,R): domain reduction: the reduced domain consists of two parts, returned
%	   in <L> and <R>.
% f:	   failed: no solution for <Variable> in <Interval>.
%
% * The partial evaluation consists of replacing all variables but <Variable>
%   by their domains and performing calculations if possible.

interval_newton(IT,FPE,DPE,V,Interval,Answer) :-
	(   newton_intervals(IT,FPE,DPE,V,Interval,Result)
	->  (   union_intersect(Result,Interval,Intersect)
	    ->  (   Intersect == Interval
		->  Answer = nr(d) % no reduction
		;   Intersect = i(_,_)
		->  Answer = r(Intersect) % reduction
		;   Intersect = u(L,R)
		->  Answer = sr(L,R) % split reduction
		)
	    ;   Answer = f % fail: no solution in <Interval>
	    )
	;   Answer = nr(f) % no reduction: failed to apply interval Newton
	).

% newton_interval(IntervalExtension,FunctionPartialEvaluation,
%	DerivativePartialEvaluation,Variable,Interval,Result)
%
% Computes an Interval Newton step for the function with partially evaluated *
% interval extension <FunctionPartialEvaluation> and partially evaluated *
% interval extension of the derivative <DerivativeIntervalExtension>, with
% respect to variable <Variable> which has initial domain <Interval>. The
% result is returned in <Result> and is either an interval or a union of
% two intervals. No intersection of the result with <Interval> is made in this
% step. This predicate fails if we cannot apply the Interval Newton step
% because of a shared zero between the function range for <Variable> in the
% center of <Interval> and the derivative range for <Variable> in <Interval>.
%
% * The partial evaluation consists of replacing all variables but <Variable>
%   by their domains and performing calculations if possible.

newton_intervals(IT,FPE,DPE,V,Interval,Result) :-
	interval_center(Interval,Center),
	get_full_evaluation(IT,FPE,V,Center,FInt),
	get_full_evaluation(IT,DPE,V,Interval,DInt),
	(   interval_contains(DInt,0)
	->  (   interval_split_excluding_zero(DInt,Left,Right)
	    ->  interval_new_class(FInt,Class),
		(   Class == p
		->  interval_div(FInt,Left,LeftDiv),
		    interval_div(FInt,Right,RightDiv)
		;   Class == n
		->  interval_div(FInt,Left,RightDiv),
		    interval_div(FInt,Right,LeftDiv)
		),
		interval_minus(Center,LeftDiv,RightResult),
		interval_minus(Center,RightDiv,LeftResult),
		(   interval_intersection(LeftResult,RightResult,_)
		->  interval_union(LeftResult,RightResult,Result)
		;   Result = u(LeftResult,RightResult)
		)
	    ;	interval_exclude_zero_bound(DInt,Nonzero),
		\+ interval_contains(FInt,0),
		interval_div(FInt,Nonzero,Div),
		interval_minus(Center,Div,Result)
	    )	
	;   interval_div(FInt,DInt,Div),
	    interval_minus(Center,Div,Result)
	).

% union_intersect(Input,Interval,Intersection)
%
% Intersects the input <Input> which is either an interval or a union of two
% intervals, by the interval <Interval> and returns the result in
% <Intersection> which is again either an interval or a union of two intervals.

union_intersect(X,Y,Z) :-
	(   X = i(_,_)
	->  interval_intersection(X,Y,Z)
	;   X = u(L,R)
	->  (   interval_intersection(L,Y,LInt)
	    ->  (   interval_intersection(R,Y,RInt)
		->  Z = u(LInt,RInt)
		;   Z = LInt
		)
	    ;   interval_intersection(R,Y,Z)
	    )
	).