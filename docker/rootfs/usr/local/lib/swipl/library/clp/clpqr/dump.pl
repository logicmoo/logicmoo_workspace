/*  Part of CLP(Q,R) (Constraint Logic Programming over Rationals and Reals)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2006, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is based on CLP(Q,R) by Christian Holzbaur for SICStus
    Prolog and distributed under the license details below with permission from
    all mentioned authors.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(clpqr_dump,
	  [ dump/3,
	    projecting_assert/1
	  ]).
:- use_module(class, [class_allvars/2]).
:- use_module(geler, [collect_nonlin/3]).
:- use_module(library(assoc), [empty_assoc/1, put_assoc/4, assoc_to_list/2]).
:- use_module(itf, [dump_linear/3, dump_nonzero/3]).
:- use_module(project, [project_attributes/2]).
:- use_module(ordering, [ordering/1]).
:- use_module(library(error), [must_be/2]).

%!  dump(+Target,-NewVars,-Constraints) is det.
%
%   Returns in Constraints,  the  constraints   that  currently  hold on
%   Target where all variables in Target are  copied to new variables in
%   NewVars and the constraints are  given   on  these new variables. In
%   short, you can safely  manipulate   NewVars  and Constraints without
%   changing the constraints on Target.

dump([],[],[]) :- !.
dump(Target,NewVars,Constraints) :-
	must_be(list(var), Target),
	copy_term_clpq(Target, NewVars, Constraints).

:- meta_predicate projecting_assert(:).

projecting_assert(Module:Clause) :-
	copy_term_clpq(Clause,Copy,Constraints),
	l2c(Constraints,Conj),			% fails for []
	(   Sm = clpq
	;   Sm = clpr
	),			% proper module for {}/1
	!,
	(   Copy = (H:-B)
	->  % former rule
	    assert(Module:(H:-Sm:{Conj},B))
	;   % former fact
	    assert(Module:(Copy:-Sm:{Conj}))
	).
projecting_assert(Clause) :-	% not our business
	assert(Clause).

copy_term_clpq(Term,Copy,Constraints) :-
	State = state(-),
	(   copy_term_clpq_(Term, NV, Cs),
	    nb_setarg(1, State, NV/Cs),
	    fail
	;   arg(1, State, Copy/Constraints)
	).

copy_term_clpq_(Term, Copy, Constraints) :-
	term_variables(Term,Target),		 % get all variables in Term
	ordering(Target),
	related_linear_vars(Target,All),	 % get all variables of the classes of the variables in Term
	nonlin_crux(All,Nonlin),		 % get a list of all the nonlinear goals of these variables
	project_attributes(Target,All),
	related_linear_vars(Target,Again),	 % project drops/adds vars
	all_attribute_goals(Again,Gs,Nonlin),
	copy_term_nat(Term/Gs,Copy/Constraints). % strip constraints

% l2c(Lst,Conj)
%
% converts a list to a round list: [a,b,c] -> (a,b,c) and [a] becomes a

l2c([X|Xs],Conj) :-
	(   Xs = []
	->  Conj = X
	;   Conj = (X,Xc),
	    l2c(Xs,Xc)
	).

% related_linear_vars(Vs,All)
%
% Generates a list of all variables that are in the classes of the variables in
% Vs.

related_linear_vars(Vs,All) :-
	empty_assoc(S0),
	related_linear_sys(Vs,S0,Sys),
	related_linear_vars(Sys,All,[]).

% related_linear_sys(Vars,Assoc,List)
%
% Generates in List, a list of all to classes to which variables in Vars
% belong.
% Assoc should be an empty association list and is used internally.
% List contains elements of the form C-C where C is a class and both C's are
% equal.

related_linear_sys([],S0,L0) :- assoc_to_list(S0,L0).
related_linear_sys([V|Vs],S0,S2) :-
	(   get_attr(V,clpqr_itf,Att),
	    arg(6,Att,class(C))
	->  put_assoc(C,S0,C,S1)
	;   S1 = S0
	),
	related_linear_sys(Vs,S1,S2).

% related_linear_vars(Classes,[Vars|VarsTail],VarsTail)
%
% Generates a difference list of all variables in the classes in Classes.
% Classes contains elements of the form C-C where C is a class and both C's are
% equal.

related_linear_vars([]) --> [].
related_linear_vars([S-_|Ss]) -->
	{
	    class_allvars(S,Otl)
	},
	cpvars(Otl),
	related_linear_vars(Ss).

% cpvars(Vars,Out,OutTail)
%
% Makes a new difference list of the difference list Vars.
% All nonvars are removed.

cpvars(Xs) --> {var(Xs)}, !.
cpvars([X|Xs]) -->
	(   { var(X) }
	->  [X]
	;   []
	),
	cpvars(Xs).

% nonlin_crux(All,Gss)
%
% Collects all pending non-linear constraints of variables in All.
% This marks all nonlinear goals of the variables as run and cannot
% be reversed manually.

nonlin_crux(All,Gss) :-
	collect_nonlin(All,Gs,[]),	% collect the nonlinear goals of variables All
					% this marks the goals as run and cannot be reversed manually
	nonlin_strip(Gs,Gss).

% nonlin_strip(Gs,Solver,Res)
%
% Removes the goals from Gs that are not from solver Solver.

nonlin_strip([],[]).
nonlin_strip([_:What|Gs],Res) :-
	(   What = {G}
	->  Res = [G|Gss]
	;   Res = [What|Gss]
	),
	nonlin_strip(Gs,Gss).

all_attribute_goals([]) --> [].
all_attribute_goals([V|Vs]) -->
	dump_linear(V),
	dump_nonzero(V),
	all_attribute_goals(Vs).

%%	attribute_goals(@V)// is det.
%
%	Translate  attributes  back  into  goals.    This   is  used  by
%	copy_term/3, which also determines  the   toplevel  printing  of
%	residual constraints.

clpqr_itf:attribute_goals(V) -->
	(   { term_attvars(V, Vs),
	      dump(Vs, NVs, List),
	      List \== [],
	      NVs = Vs,
	      del_itf(Vs),
	      list_to_conj(List, Conj)
	    }
	->  [ {}(Conj) ]
	;   []
	).

clpqr_class:attribute_goals(_) --> [].

clpqr_geler:attribute_goals(V) --> clpqr_itf:attribute_goals(V).

del_itf([]).
del_itf([H|T]) :-
	del_attr(H, clpqr_itf),
	del_itf(T).


list_to_conj([], true) :- !.
list_to_conj([X], X) :- !.
list_to_conj([H|T0], (H,T)) :-
	list_to_conj(T0, T).

		 /*******************************
		 *	       SANDBOX		*
		 *******************************/
:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(clpqr_dump:dump(_,_,_)).
