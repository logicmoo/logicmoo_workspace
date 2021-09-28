/*
    This file is part of NSM-DALIA, an extensible parser and generator
    for NSM grammars.
       
    Copyright (C) 2009 Francesco Zamblera.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/


% USATE DA TOKI PONA

get_next_word(String,Rest,Word) :-
	get_next_word1(String,Rest1,Word1),
	ifAffix(Word1,Word,Rest1,Rest).

ifAffix(W1,W,['='|R1],R) :-
	last(W1,'='),
	!,
	get_next_word(R1,R,W2),
	append(W1,W2,W).
ifAffix(W1,W,['='|R1],R) :-
	!,
	get_next_word(R1,R,W2),
	append(W1,['='|W2],W).
ifAffix(W1,W,R1,R) :-
	last(W1,'='),
	!,
	get_next_word(R1,R,W2),
	append(W1,W2,W).
ifAffix(W,W,R,R).

get_next_word1([' '|String],Rest,Word) :-
	!,
	get_next_word2(String,Rest,Word).
get_next_word1(String,Rest,Word) :-
	get_next_word2(String,Rest,Word).	

get_next_word2(String,Rest,Word) :-
	dl(String,S1-S2),
	concat_dl(A-B,[' '|C]-C,Rest-[],S1-S2),
	ld(Word,A-B).


dl(String,S1-S2) :-
	append(String,S2,S1).

ld(S,S1-S2) :-
	append(S,S2,S1),!.
concat_dl(A-B,B-C,C-D,A-D).




sublist([],L2,[],L2).
sublist([C|L1],L2,Before,After) :-
	member(C,L2,Before,NewL2),
	sublist_aux(L1,NewL2,After).

member(C,[C|L2],[],L2).
member(C,[C1|L2],[C1|Before],NewL2) :-
	member(C,L2,Before,NewL2).

sublist_aux([],L2,L2).
sublist_aux([C|L1],[C|L2],NewL2) :-
	sublist_aux(L1,L2,NewL2).


put_string([]). % VERSIONE DI PARSE_TP.PL
put_string([C|String]) :-
	put(C),
	put_string(String).

put_msg_string([]). % VERSIONE DI PARSE_TP.PL
put_msg_string([C|String]) :-
	var(C),
	!,
	put("_"),
	put_msg_string(String).
put_msg_string([C|String]) :-
	put(C),
	put_msg_string(String).


match_shape(A,X+Y) :-
	not(var(X)),
	X = _ + _,
	append(Z,Y,A),
	match_shape(Z,X).
match_shape(A,X+Y) :-
	!,
	append(X,Y,A).
match_shape(A,A).

write_word(A,B,C) :-
	write(C),
	write(': '),
	put_string(B),
	write(' ('),
	write(A),
	write(')'),
	nl.

	
remove_verbatim([91|S],S1,V) :-
	!,remove_verb_aux(S,S1,V).
remove_verbatim(S,S,[]).

remove_verb_aux([93|S],S,[]) :- !.
remove_verb_aux([],[],[]).
remove_verb_aux([C|S],S1,[C|V]) :-
	remove_verb_aux(S,S1,V).


/* IN STRINGA */
skip_whites([32|S],S1) :-
	!,
	skip_whites(S,S1).
skip_whites([13|S],S1) :-
	!,
	skip_whites(S,S1).
skip_whites([10|S],S1) :-
	!,
	skip_whites(S,S1).
skip_whites([8|S],S1) :-
	!,
	skip_whites(S,S1).
skip_whites(S,S).


/* IN FILE */
skipwhite(32,C) :-
	!,
	get0(C1),
	skipwhite(C1,C).
skipwhite(13,C) :-
	!,
	get0(C1),
	skipwhite(C1,C).
skipwhite(10,C) :-
	!,
	get0(C1),
	skipwhite(C1,C).
skipwhite(8,C) :-
	!,
	get0(C1),
	skipwhite(C1,C).
skipwhite(C,C).


/* IN FILE */
skip_newline(10,C) :-
	!,
	get0(C1),
	skip_newline(C1,C).
skip_newline(13,C) :-
	!,
	get0(C1),
	skip_newline(C1,C).
skip_newline(C,C).

normalize_spaces([],[]).
normalize_spaces([32,32|S],S1) :- 
	!,
	normalize_spaces_aux(S,S1).
normalize_spaces([32|S],S1) :- 
	!,
	normalize_spaces_aux(S,S1).
normalize_spaces([C|S],[C|S1]) :- 
	!,
	normalize_spaces_aux(S,S1).

normalize_spaces_aux([32,32],[]) :- !.
normalize_spaces_aux([32],[]) :- !.
normalize_spaces_aux([32,32|S],[32|S1]) :- 
	!,
	normalize_spaces_aux(S,S1).
normalize_spaces_aux([C|S],[C|S1]) :- 
	!,
	normalize_spaces_aux(S,S1).
normalize_spaces_aux([32],[]) :- !.

normalize_spaces_aux([],[]).


