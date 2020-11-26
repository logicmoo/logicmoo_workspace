/*----------------------------------------------------------------------
    sort_restr( +Prop )

    Prop fulfils the sortal restrictions on propositions
----------------------------------------------------------------------*/


sem_sort( english, language ).
sem_sort( svenska, language ).

% action
sem_sort( top, action ).
sem_sort( login, action ).


% alphadigit
sem_sort(A, alphadigit):-
	atom(A),
	name(A,AStr),
	alphaDigit(AStr).
	

alphaDigit([]).

alphaDigit([A|As]):-
	(
	  A >= 97,
	  A =< 122
	  ; 
	  A >= 65,
	  A =< 90
	;
	  A >= 48,
	  A =< 57
	;
	  A >= 97,
	  A =< 122
	),
	alphaDigit(As).


/*--------------------
conceptual hierarichy
--------------------*/

isa( password, alphadigit).
isa( user_name, alphadigit).
isa( alphadigit, top).

isa( T0, T2 ):-
	T0 \= T2,
	isa( T0, T1 ),
	isa( T1, T2 ).



