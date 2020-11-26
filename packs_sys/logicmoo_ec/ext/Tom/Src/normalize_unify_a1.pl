%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date:  7/07/94   File: test.pl                      %%
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%%  7/07/94 Created                                                          %%
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  gilbert boyreau                                                  %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
% :- module_interface( upp_unify ).
% :- export unify_a1/2.
% :- export upp_unify/3.

% :- begin_module( upp_unify ).

\chapter
[Die Datei {\tt tom\_normalize\_unify\_a1}]
{Die Datei {\Huge \tt tom\_normalize\_unify\_a1}}

This file uses a unification algorithm that is written in C. It has to be
named |modal| and has to be in the directory |~/bin|, executable, of course.
This code has been written by Gilbert Boyreau, Caen University, France.

\PL*/


list_to_conjonction( [],true ).
list_to_conjonction( [T|Q],','(T,Conj) ) :-
	list_to_conjonction( Q,Conj ).

/* le predicat d'initialisation */
start :-
	exec( "modal",[modal_in,modal_out],_ ),
	read_string( modal_out,">",_,_ ),
	printf( modal_in,"Eclipse\n%b",[] ),
%	printf( modal_in,"debug\n%b",[] ),
	read_string( modal_out," ",_,_ ).


:- start.

stop:-
	printf(modal_in,"quit\n%b",[]).

normalize_unify_a1(Pred,Pred1) :- 
	normalize_ass(Pred,Pred0),
	normalize_ass(Pred1,Pred10),
	unify_a1(Pred0,Pred10).



/* le predicat de calcul */
upp_unify( T1,T2,L_variables,Unifier ) :-
	term_variables( T1+T2,L_variables ),
	printf( modal_in,"unif\n%QODVw\n%QODV.w\n%QODV.w\n%b",
			 [L_variables,T1,T2] ),
	read( modal_out,L_variables-L_l_unifiers ),
	member( Unifier,L_l_unifiers ).

/* les predicats d'interface */
upp_unify( T1,T2,Unifier_list ) :-
	upp_unify( T1,T2,_,Unifier ),
	conjunction_to_list( Unifier,Unifier_list ).

unify_a1( T1,T2 ) :-
	upp_unify( T1,T2,_,Unifier ),
	call( Unifier ).


%% Author: Gerd Neugebauer
%%=============================================================================
% Master File: unify_a1.tex


:- set_flag(occur_check,on).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate normalize_ass/(+Term, ?Normal).

Bring a $associative\_binary\_functor$ term into normal form by eliminating
superfluous $associative\_neutral\_element$ and flattening
$associative\_binary\_functor$ terms.

\PL*/

normalize_ass(Var,Var2) :-
	var(Var),
	!,
	Var = Var2.
normalize_ass(Atom,Atom2) :-
	atomic(Atom),
	!,
	Atom = Atom2.
normalize_ass(+(A1,Arg),
	      NormalArg) :-
	A1 == 0,
	!,
	normalize_ass(Arg,NormalArg).
normalize_ass(+(Arg,A2),
	      NormalArg) :-
	A2 == 0,
	!,
	normalize_ass(Arg,NormalArg).
normalize_ass(+(A1,C),
	      Normal) :-
	nonvar(A1),
	A1 = +(A,B),
	!,
	normalize_ass(+(A,
					+(B,C)),
					Normal).
normalize_ass(+(A,B),
	      +(NormalA,NormalB)) :-
	!,
	normalize_ass(A,NormalA),
	normalize_ass(B,NormalB).
normalize_ass(Term,NormalTerm) :-
	Term =.. [F|Args],
	normalize_ass_list(Args,NormalArgs),
	NormalTerm =.. [F|NormalArgs].

normalize_ass_list([],[]).
normalize_ass_list([H|T],[NormalH|NormalT]) :-
	!,
	normalize_ass(H,NormalH),
	normalize_ass_list(T,NormalT).

analyse([],[]).
analyse([_+I|Rest],[I|Indices]) :- 
	analyse(Rest,Indices).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */

