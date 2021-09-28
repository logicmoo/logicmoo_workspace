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


:- include('operators.pl').
:- include('dynamic.pl').
:- include('morphophon.pl').
:- include('word_scanner.pl').

/* MORPHOLOGICAL PARSER */

/* SCANNER */

tm_morph_all_readings(Lang,Format,String,Readings,Rest) :-
	get_word(String,W,Rest),
	findall(A1,
		generic_p_morph(Lang,W,A1,_Lex),
		Readings1),
	format_readings(Format,Readings1,Readings).

p_morph_lex(Lang,W,A) :-
	generic_p_morph(Lang,W,A,Lex),
        nl,nl,
	write('Morphemic String: '),
	name(X,Lex),
	write(X),
	nl.


format_readings(_,[],[]).
format_readings(Format,[A1|Readings1],[A|Readings]) :-
	o_format(Format,A1,A),
	format_readings(Format,Readings1,Readings).


	
tm_morph_parse_word(Lang,Format,String,A,Rest) :-
	get_word(String,W,Rest),
	p_morph(Lang,W,A1,_Lex),
	o_format(Format,A1,A).


generic_p_morph(Lang,W,A,Lex) :-
	morph_grammar_type(Lang,tagmemic),
	p_morph(Lang,W,A,Lex).
generic_p_morph(Lang,W,A,Lex) :-
	morph_grammar_type(Lang,dependency),
	dg_parse_word_standalone(Lang,W,A,Lex).


/* TAGMEMIC PARSER */


p_morph(Lang,W,m(Name,A),Lex) :-
	append(W,"#",W2),
	find_syntagmeme(Lang,W2,W1,synt(A1,P,C,Name),parsed("#",[],[]),Parsed),
	scan_rest(Lang,W1,P,A1,A,Name,C,Parsed,parsed(_Surf,[45|Lex],_NewSuccLex)). %NewSuccLex dovrebbe essere [].



find_syntagmeme(Lang,W,W1,synt(A,P,C,Name),P1,P2) :-
	find_morph(Lang,Class,Exponent,W,W1,P1,P2),
	get_syntagmeme(Lang,Name,Class,Exponent,P,A,C).


get_syntagmeme(Lang,Name,Class,Exponent,P,A,C) :-
	morph_synt(Lang,Name,[_Slot:Class/Exponent|P],A,C).
get_syntagmeme(Lang,Name,Class,Exponent,P,A,C) :-
	morph_synt(Lang,Name,[?_Slot:Class/Exponent|P],A,C).
get_syntagmeme(Lang,Name,Class,Exponent,P,A,C) :-
	morph_synt(Lang,Name,[_Slot:[F|Filler]/Exponent|P],A,C),
	member(Class,[F|Filler]).
get_syntagmeme(Lang,Name,Class,Exponent,P,A,C) :-
	morph_synt(Lang,Name,[? _Slot:[F|Filler]/Exponent|P],A,C),
	member(Class,[F|Filler]).
get_syntagmeme(Lang,Name,Class,Exponent,P,A,C) :-
	morph_synt(Lang,Name,[?_Slot1:_Filler1/e|P1],A,C),
	check_syntagmeme(morpheme(_Slot,Class,Exponent),P1,P,A).

/*
check_syntagmeme(morpheme(Slot,Class,Exponent),[Slot:Class/Exponent|P],P,_A) :- 
	!.
check_syntagmeme(morpheme(Slot,Class,Exponent),[?Slot:Class/Exponent|P],P,_A) :-
	!.
check_syntagmeme(morpheme(Slot,Class,Exponent),[Slot:[F|Iller]/Exponent|P],P,_A) :- 
        member(Class,[F|Iller]),!.
check_syntagmeme(morpheme(Slot,Class,Exponent),[?Slot:[F|Iller]/Exponent|P],P,_A) :-
	member(Class,[F|Iller]),!.
check_syntagmeme(M,[?_Slot1:_Class1/e|P],P1,A) :-
	check_syntagmeme(M,P,P1,A).
*/


scan_rest(Lang,[],P,A,A,_,C,Parsed,Parsed) :- 
	check_cond_list(Lang,C),
	close_facultative(P,A).
scan_rest(Lang,String,[],A1,A,Name,C1,Parsed1,Parsed2) :-
	not_empty(String),
	check_cond_list(Lang,C1),
	find_continuation(Lang,Name,A1,synt(NewA1,P,C,NewName)),
	!,
	scan_rest(Lang,String,P,NewA1,A,NewName,C,Parsed1,Parsed2).
scan_rest(Lang,String,P,A1,A,Name,C,Parsed1,Parsed2) :-
	find_morph(Lang,Class,Exponent,String,Rest,Parsed1,Parsed3),
	check_syntagmeme(morpheme(_Slot,Class,Exponent),P,P1,A1),
	scan_rest(Lang,Rest,P1,A1,A,Name,C,Parsed3,Parsed2).

close_facultative([],_).
close_facultative([?Role:_/e|P],A) :-
	member(Role:e,A),!,
	close_facultative(P,A).
close_facultative([?_:_/e|P],A) :-
	close_facultative(P,A).

not_empty([]) :- !,fail.
not_empty(_).


find_continuation(Lang,Name,A1,synt(NewA1,P,C,NewName)) :-
	get_syntagmeme(Lang,NewName,Name,A1,P,NewA1,C).
 

