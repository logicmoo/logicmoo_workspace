
/* ------------------------------------------------------------------------
 > FILENAME:	utils.pl
 > PURPOSE:	utils required across all prolog versions
 > AUTHORS:	Rob Gaizauskas, A.Hartley, Kevin Humphreys
 > NOTES:	
 ------------------------------------------------------------------------ */

:- assert('$package_name'('shef.nlp.supple.prolog.cafe')).

/* name_conc/2
From two atoms build a third which is their concatenation
*/
name_conc(ListofAs,NewA) :-
    name_conc1(ListofAs,NewN),
    name(NewA,NewN),!.

name_conc1([],[]).
name_conc1([A|As],CN) :-
    name(A,AN),
    name_conc1(As,AsN),
    conc(AN,AsN,CN),!.

/* conc/3
True if L3 is the concatenation of L1 and L2
NO BACKTRACKING
*/
conc([],L2,L2) :- !.
conc([X | L1],L2, [X | L3]) :-
    conc(L1,L2,L3),!.

/* foreach/2
For each X do Y
*/
foreach(X,Y) :-	X, do(Y), fail.
foreach(X,Y) :-	true.
do(Y) :- Y,!.

/* gensym/2
*/
buchart_gensym(Atom,Sym) :-
    retract(gensymmark(Atom,N)),
    M is N + 1,
    name(M,MChars),
    name(Atom,AtomChars),
    append(AtomChars,MChars,SymChars),
    name(Sym,SymChars),
    assert(gensymmark(Atom,M)),!.
buchart_gensym(Atom,Sym) :-
    name(1,MChars),
    name(Atom,AtomChars),
    append(AtomChars,MChars,SymChars),
    name(Sym,SymChars),
    assert(gensymmark(Atom,1)).

/* string_to_number/2
convert string to number
assumes string is single character!
fix for level bug
*/
string_to_number(String, Number) :-
  name(String, [CharCode]),
  name('0', [ZeroCode]),
  Number is CharCode - ZeroCode.


/* write_nl/1
write a list of things plus a newline
*/
write_nl([]) :-
  nl,
  !. 
write_nl([ Thing |MoreThings ]) :-
  write(Thing),
  write_nl(MoreThings).


/* string conversions */

lower_chars(-, _) :- !, fail.
lower_chars([], []).
lower_chars([T|Ts], [U|Us]) :-
        to_lower(T, U),
        lower_chars(Ts, Us).
 
%   to_lower(?UpperCase, ?LowerCase) - converts upper case ascii to lower
to_lower(U, L) :- 
	U > 64, U < 91, !,
	L is U + 32.
to_lower(L, L).
 
 
%   upper(+Text, ?Upper)
upper(Text, Upper) :-
        (   atom(Text) ->
                atom_chars(Text, TextChars),
                upper_chars(TextChars, UpperChars),
                atom_chars(Upper, UpperChars)
        ;   upper_chars(Text, Upper)
        ).
 
upper_chars(-, _) :- !, fail.
upper_chars([], []).
upper_chars([T|Ts], [U|Us]) :-
        to_upper(T, U),
        upper_chars(Ts, Us).
 
%   to_upper(?LowerCase, ?UpperCase) - converts lower case ascii to upper
to_upper(L, U) :- 
	L > 96, L < 123, !,
	U is L - 32.
to_upper(U, U).
 
