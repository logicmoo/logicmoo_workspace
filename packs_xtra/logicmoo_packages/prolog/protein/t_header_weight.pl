%******************** t_header_weight.pl ********************

% Praedikate fuer das iterative term deepening

%************************************************************

%%% p_weight( LitL, Weight)
%%% Ermittlung der Gewichtung einer Klausel: Maximum der Gewichte der Literale

:- mode p_weight( +, -).

p_weight( [], 0).

p_weight( [Lit|LitR], Weight) :-
	h_weight_term( Lit, TermWeight),
	p_weight( LitR, ZwiWeight),
	max( TermWeight, ZwiWeight, Weight), !.


%%% h_weight_term( Lit, TermDepth)
%%% Ermittlung der Gewichtung eines Literals.

:- mode h_weight_term( ?, -).

h_weight_term( Lit, 0) :- atomic( Lit), !.
h_weight_term( Lit, 0) :- var( Lit), !.

h_weight_term( Lit, Depth) :- 
	functor( Lit, _, Arity),
	h_weight_funct( Lit, 1, Arity, Arity, Depth).


%%% h_weight_funct( Lit, Index, AnzahlArgumente, InDepth, OutDepth)
%%% Ermittlung der Gewichtung einer Funktion: Summe der Gewichte der Argumente

:- mode h_weight_funct( +, ++, ++, ++, -).

h_weight_funct( _, I, Max, Depth, Depth) :- I > Max, !.

h_weight_funct( Lit, I, Max, InDepth, OutDepth) :-
	arg( I, Lit, Arg),
	h_weight_term( Arg, ArgDepth),
	ZwiDepth is InDepth + ArgDepth,
	I1 is I + 1,
	h_weight_funct( Lit, I1, Max, ZwiDepth, OutDepth).

% END t_header_weight.pl END

