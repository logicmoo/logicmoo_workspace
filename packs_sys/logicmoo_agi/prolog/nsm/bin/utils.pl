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


:- module(utils,[
	sublist/4,
	member/4,
	put_string/1,
	put_msg_string/1,
	match_shape/2,
	skipwhite/2,
	skip_whites/2,
	skip_newline/2,
	write_word/3,
	remove_verbatim/3,
	normalize_spaces/2,
	append_list/2,
	put_message/1,
	put_message_var/1,
	put_term/1,
	put_dep_gen_rule/3,
	put_dep_rule/3,
	get_n_spaces/2,
	put_format/2,
	put_string_format/2,
	put_wiki_string_format/2,		 
	load_file/2,
	split_list/4,
	do_actions/2,
	read_ident/3
	]).	
	

:- include('operators.pl').

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

%%	put_string(+String) is det
%
%	Wrhites a string (implemented as a list of codes)
%	to the terminal or to a file (if global_par:current_output_file/1
%       is set).
%	
put_string([]). % VERSIONE DI PARSE_TP.PL
put_string([C|String]) :-
	put_code(C),
	put_string(String).

put_msg_string([]). % VERSIONE DI PARSE_TP.PL
put_msg_string([C|String]) :-
	var(C),
	!,
	put_code("_"),
	put_msg_string(String).
put_msg_string([C|String]) :-
	put_code(C),
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
%%	skipwhite(+C1,-C2)
%
%	Reads (from file or user) terminal until a non-white
%	character is input.
%	
skipwhite(32,C) :-
	!,
	get_code(C1),
	skipwhite(C1,C).
skipwhite(13,C) :-
	!,
	get_code(C1),
	skipwhite(C1,C).
skipwhite(10,C) :-
	!,
	get_code(C1),
	skipwhite(C1,C).
skipwhite(8,C) :-
	!,
	get_code(C1),
	skipwhite(C1,C).
skipwhite(C,C).


/* IN FILE */
skip_newline(10,C) :-
	!,
	get_code(C1),
	skip_newline(C1,C).
skip_newline(13,C) :-
	!,
	get_code(C1),
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



append_list(S,List) :-
	append_list(S,[],List).

append_list(S,S,[]).
append_list(S,S2,[S1|List]) :-
	append(S2,S1,S3),
	append_list(S,S3,List).



/* CLINE */

put_message(S) :-
	put_string(" ** "),
	put_string(S),nl.

put_message_var(S) :-
	put_string(" ** "),
	put_msg_string(S),nl.

put_term(T) :-
	write(T),write('.'),nl.

put_dep_rule(Deg,Dep,C) :-
	write('Rule: ('),write(Deg),write(') '),
	write(Dep),write('cond: '),write(C),nl,nl.

put_dep_gen_rule(Deg,A+B==>D,_C) :-
	write('Rule: ('),write(Deg),write(') '),
	write(D),write(' ==> '),nl,tab(8),
	write(A),
	nl,
	tab(5),
	write(' + '),
	write(B),nl,nl.
put_dep_gen_rule(Deg,A+B+E==>D,_C) :-
	write('Rule: ('),write(Deg),write(') '),
	write(D),write(' ==> '),nl,tab(8),
	write(A),
	nl,
	tab(5),
	write(' + '),
	write(B),
	nl,
	tab(5),
	write(' + '),
	write(E),nl,nl.


get_n_spaces(0,"") :- !.
get_n_spaces(N,"") :- N < 0,!.
get_n_spaces(N,[32|Spaces]) :-
	M is N - 1,
	get_n_spaces(M,Spaces).


%%	put_format(+Format,+CharCode) is det
%
%	Writes (to the current actuve output file or to the terminal
%       if no output file is currently selected) a character, escaping
%       it according to the definitions provided in 
%       mark_up:escaped_char/3. For example, if we are writing rtf or
%       latex output, "\" needs to be escaped as "\\"; if we are writing
%       html, "<" must be changed to "&lt;".
%       
put_format(Format,C) :-
	mark_up:escaped_char(Format,C,C1),
	!,
	put_string(C1).
put_format(_,C) :-
	put_code(C).

%%	put_string_format(+Format,+String:list) is det
%
%	Writes a string  to the current active output file (or to the terminal
%       if no output file is currently selected), escaping characters according
%       to the definitions provided in mark_up:escaped_char/3 (see
%       put_format/2).
%       
put_string_format(_,[]).
put_string_format(Format,[C|String]) :-
	put_format(Format,C),
	put_string_format(Format,String).

%%	put_wiki_string_format(+Format,+String:list) is det
%
%	Writes a string  to the current active output file (or to the terminal
%       if no output file is currently selected), escaping characters according
%       to the definitions provided in mark_up:escaped_char/3 (see
%       put_format/2). 
%       
%       Moreover, =|_string of characters_|= is rendered as _|string
%       of characters|_ and =|*string of characters*|= as *|string of
%       characters|*.
put_wiki_string_format(_,[]).
put_wiki_string_format(Format,[64,C|String]) :-
	!,
	put_format(Format,C),
	put_wiki_string_format(Format,String).
put_wiki_string_format(Format,[95|String]) :-
	!,
	mark_up:write_auto_text(Format,emph(1)),
	put_wiki_string_format(Format,String,emph).
put_wiki_string_format(Format,[42|String]) :-
	!,
	mark_up:write_auto_text(Format,bold(1)),
	put_wiki_string_format(Format,String,bold).
put_wiki_string_format(Format,[10,10|String]) :-
	!,
	mark_up:write_auto_text(Format,p(0)),
	mark_up:write_auto_text(Format,p(1)),
	put_wiki_string_format(Format,String).
put_wiki_string_format(Format,[10,13,10,13|String]) :-
	!,
	mark_up:write_auto_text(Format,p(0)),
	mark_up:write_auto_text(Format,p(1)),
	put_wiki_string_format(Format,String).
put_wiki_string_format(Format,[13,13|String]) :-
	!,
	mark_up:write_auto_text(Format,p(0)),
	mark_up:write_auto_text(Format,p(1)),	
	put_wiki_string_format(Format,String).
put_wiki_string_format(Format,[C|String]) :-
	put_format(Format,C),
	put_wiki_string_format(Format,String).


put_wiki_string_format(_,[],_).
put_wiki_string_format(Format,[42|String],bold) :-
	!,
	mark_up:write_auto_text(Format,bold(0)),
	put_wiki_string_format(Format,String).
put_wiki_string_format(Format,[95|String],emph) :-
	!,
	mark_up:write_auto_text(Format,emph(0)),
	put_wiki_string_format(Format,String).
put_wiki_string_format(Format,[C|String],BOLD_EMPH) :-
	put_format(Format,C),
	put_wiki_string_format(Format,String,BOLD_EMPH).



load_file(_,end_of_file) :- !.
load_file(Module,S) :-
	assertz(S),
	read_term(S1,[module(Module)]),
	load_file(Module,S1).


split_list(List,Sublist,[],Rest) :-
	append(Sublist,Rest,List),
	!.
split_list([],_,_,_) :- !,fail.
split_list([Item|List],Sublist,[Item|Prefix],Suffix) :-
	split_list(List,Sublist,Prefix,Suffix).

do_actions(_,[]).
do_actions(Module,[Action|Actions]) :-
	call(Module:Action),
	!,
	do_actions(Module,Actions).

%%	read_ident(+C,-Ident,-C1) is det
%
%	Reads (from file or user terminal) an identifier, stopping
%	at the first space, newline or tab characters.
%	
read_ident([],10,10).% :- skipwhite(10,C),!.
read_ident([],13,10).% :- skipwhite(13,C),!.
read_ident([],32,32). % :- skipwhite(32,C),!.
read_ident([],8,32). % :- skipwhite(8,C),!.
read_ident([C|Ident],C,NewC) :-
	get_code(C1),
	read_ident(Ident,C1,NewC).
