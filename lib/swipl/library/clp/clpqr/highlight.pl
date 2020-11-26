/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Richard O'Keefe
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, CWI, Amsterdam
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

:- module(clpqr_highlight, []).

:- multifile
	prolog_colour:goal_colours/3,
	prolog_colour:syntax_message//1.

prolog_colour:goal_colours({Constraints}, imported(File), Colours) :-
	clpqr_module(Module),
	module_property(Module, file(File)), !,
	Colours = goal(imported(File)) - ConstraintColours,
	constraint_colours(Constraints,	Module, ConstraintColours).

clpqr_module(clpq).
clpqr_module(clpr).

constraint_colours(Var, _, classify) :-
	var(Var), !.
constraint_colours((R1,R2), M, classify-[C1,C2]) :- !,
	constraint_colours(R1, M, C1),
	constraint_colours(R2, M, C2).
constraint_colours((R1;R2), M, classify-[C1,C2]) :- !,
	constraint_colours(R1, M, C1),
	constraint_colours(R2, M, C2).
constraint_colours(Term,   M, constraint(M)-[classify,classify]) :-
	clpqr_constraint(Term), !.
constraint_colours(_, M, type_error(Type)) :-
	constraint_type(M, Type).

constraint_type(clpq, clpq_constraint).
constraint_type(clpr, clpr_constraint).

clpqr_constraint(_ > _).
clpqr_constraint(_ >= _).
clpqr_constraint(_ < _).
clpqr_constraint(_ =< _).
clpqr_constraint(_ =:=_).
clpqr_constraint(_ =\= _).
clpqr_constraint(_ = _).

prolog_colour:syntax_message(constraint(clpq)) -->
	[ 'clp(Q) constraint'-[] ].
prolog_colour:syntax_message(constraint(clpr)) -->
	[ 'clp(R) constraint'-[] ].
prolog_colour:syntax_message(type_error(constraint(clpq))) -->
	[ 'Only clp(Q) constraints may appear inside {}'-[] ].
prolog_colour:syntax_message(type_error(constraint(clpq))) -->
	[ 'Only clp(R) constraints may appear inside {}'-[] ].
