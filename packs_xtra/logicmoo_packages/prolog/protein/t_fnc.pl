%********************      t_fnc.pl      ********************

% Praedikate zur Verwalung der Funktorenstruktur.

%************************************************************

:- module_interface( t_fnc).

:- export fnc_init / 0,
	  fnc_next / 1,
	  fnc_set / 1,
          fnc_struct / 3.

:- dynamic fnc / 2.
        
:- begin_module( t_fnc).

:- use_module( t_definitions).
:- use_module( t_literal).


% ---------- Funktorstruktur ----------

%%% fnc_struct( Functor, Arity, Fnc)
%%% Definition der fnc-Struktur

fnc_struct( Functor, Arity, (Functor,Arity)).


%%% fnc_init
%%% Initialisierung der Funktorenverwaltung

fnc_init :- 
	retract_all( fnc(_,_)),
	def_special_lit( F, A, _),
	   functor( Lit, F, A),
	   fnc_set( Lit),
        fail.

fnc_init.


%%% fnc_set( Lit)
%%% Extrahiert die Funktoren und legt sie im fnc-Praedikat ab.

:- mode fnc_set( +).

fnc_set( true).
fnc_set( false).

fnc_set( Lit) :- def_test_prolog_lit( Lit).

fnc_set( Lit) :-
	functor( Lit, F, A),
	lit_neg_fnc( F, NotF),
	l_set( F, A),
	l_set( NotF, A).


%%% l_set( Functor, Arity)
%%% Assertieren des functor-Praedikats

:- mode l_set( ++, ++).

l_set( F, A) :- fnc( F, A), !.
l_set( F, A) :- assert( fnc(F,A)).


%%% fnc_next( Fnc)
%%% Ermittelt nachste fnc

fnc_next( Fnc) :- 
	fnc( F, A),
	fnc_struct( F, A, Fnc). 








