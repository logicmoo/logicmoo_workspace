
/* ------------------------------------------------------------------------
 > FILENAME:	swi_utils
 > PURPOSE:	
 > AUTHORS:	Kevin Humphreys
 > NOTES:	
 ------------------------------------------------------------------------ */

cvsid_swi_utils("$Id: swi_utils.pl 7085 2005-12-05 16:32:03Z ian_roberts $").


:- consult('supple_utils.pl').


%reconsult(File) :- consult(File).

no_doubles(X,Y) :- list_to_set(X,Y).


max_list([Head|Tail], Max) :-
        max_list(Tail, Head, Max).
 
max_list([], Max, Max).
max_list([Head|Tail], Element, Max) :-
        Head =< Element, !,
        max_list(Tail, Element, Max).
max_list([Head|Tail], _, Max) :-
        max_list(Tail, Head, Max).
 
 
%   lower(+Text, ?Lower)
lower(Text, Lower) :-
	atom(Text),
	atom_codes(Text, TextChars),
	lower_chars(TextChars, LowerChars),
	atom_codes(Lower, LowerChars).
