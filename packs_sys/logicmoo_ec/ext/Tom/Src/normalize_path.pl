%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date:  1/02/95   File: normalize_path.pl            %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%%  1/02/95 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Zoltan Rigo                                                      %%
%%                                                                           %%
%% Usage:   prolog normalize_path.pl                                         %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\chapter
[Die Datei {\tt tom\_normalize\_path}]
{Die Datei {\Huge \tt tom\_normalize\_path}}

\Predicate normalize_path/2(+Formula, -NormalizedFormula).

This predicate normalises the path terms in the formula according to the
new convention introduced in Caen. (We co-operate, so this might explain
the mess.)

This code was written by Gilbert Boyreau.

\PL*/
normalize_path(Var,Var2) :-
	var(Var),
	!,
	Var = Var2.
normalize_path(Atom,Atom2) :-
	atomic(Atom),
	!,
	Atom = Atom2.
normalize_path(+(A1,Arg),
	      NormalArg) :-
	A1 == 0,
	!,
	normalize_path(Arg,NormalArg).
normalize_path(+(Arg,A2),
	      NormalArg) :-
	A2 == 0,
	!,
	normalize_path(Arg,NormalArg).
normalize_path(+(A1,C),
	      Normal) :-
	nonvar(A1),
	A1 = +(A,B),
	!,
	normalize_path(+(A,+(B,C)),
			Normal).
normalize_path(+(A,B),
	      +(NormalA,NormalB)) :-
	!,
	normalize_path(A,NormalA),
	normalize_path(B,NormalB).

normalize_path(Term,NormalTerm) :-
	Term =.. [F|Args],
	normalize_path_list(Args,NormalArgs),
	NormalTerm =.. [F|NormalArgs].

normalize_path_list([],[]).
normalize_path_list([H|T],[NormalH|NormalT]) :-
	!,
	normalize_path(H,NormalH),
	normalize_path_list(T,NormalT).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */

