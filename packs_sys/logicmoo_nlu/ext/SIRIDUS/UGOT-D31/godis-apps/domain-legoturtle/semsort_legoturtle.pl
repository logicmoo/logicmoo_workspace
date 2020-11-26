:- use_module(library(lists),[member/2]).

/*----------------------------------------------------------------------
    sort_restr( +Prop )

    Prop fulfils the sortal restrictions on propositions
----------------------------------------------------------------------*/

sem_sort( svenska, language ).
sem_sort( speak, domain ).

% action
sem_sort( pen_up, action ).
sem_sort( pen_down, action ).
sem_sort( move, action ).
sem_sort( turn, action ).
sem_sort( background, action ).
sem_sort( pencolor, action ).
sem_sort( clear, action ).

sem_sort( forward, action ).
sem_sort( backward, action ).
sem_sort( right, action ).
sem_sort( left, action ).

sem_sort( circle, action ).
sem_sort( tree, action ).

%colors
sem_sort( C, colors) :-
	isColor(C).

% number
sem_sort( N, number ) :-
	integer(N).

% degrees
sem_sort( N, degrees ) :-
	%to_number(P,N),
	integer(N),
	N >= 0,
	N =< 360.

sem_sort( P, degrees ):-
	to_number(P, N),
	integer(N),
	N >= 0,
	N =< 360.

%steps
sem_sort( N, steps ):-
	%to_number(_P, N),
	integer(N).

%color
sem_sort( N, color ):-
	isColor(N).


to_number( Atom, Number ) :-
%	name( Atom, String ),
	number_atom( Atom ),
	atom_chars( Atom,Cs ),
	number_chars( Number, Cs ).

number_atom(A) :-
	atomic(A),
	\+ number(A).

isColor(X) :-
	member(X, [red, darkred, white, black, yellow, lightyellow, orange, darkorange, blue, darkblue, lightblue, purple, darkpurple, green, darkgreen, lightgreen, brown, pink]).

/*--------------------
conceptual hierarichy
--------------------*/
isa( color, colors).
isa( steps, number ).
isa( degrees, number ).


isa( T0, T2 ):-
	T0 \= T2,
	isa( T0, T1 ),
	isa( T1, T2 ).
