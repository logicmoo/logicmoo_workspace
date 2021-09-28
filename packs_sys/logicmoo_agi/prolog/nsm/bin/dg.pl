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


/* SYNTACTICAL DEPENDENCY PARSER  */
dg_parse(Lang,Sent,Parse) :-
	generic_get_word(Lang,Sent,"##",Word,[],Rest,Stack),
	parse_dg(Lang,Word,Rest,Stack,Parse).

generic_get_word(Lang,Sent,PrevWord,Word,Stack,Rest,NewStack) :-
	morph_grammar_type(Lang,dependency),
	!,
	dg_morph_parse_word(Lang,Sent,PrevWord,Word,Stack,Rest,NewStack).
generic_get_word(Lang,Sent,PrevWord,Word,Stack,Rest,[Analysis|Stack]) :-
	morph_grammar_type(Lang,tagmemic),
	!,
	tm_morph_parse_word(Lang,dg,Sent,PrevWord,Word,Analysis,Rest).
      

parse_dg(Lang,_Word,[],Stack,Parse) :-
	!,
	try_reduce(Lang,1,Stack,NewStack,yes),
	reduce_all(Lang,1,NewStack,Parse).
parse_dg(Lang,PrevWord,Sent,Stack,Parse) :-
	generic_get_word(Lang,Sent,PrevWord,Word,Stack,Rest,Stack1),
        notify_shift(Stack1),
	try_reduce(Lang,1,Stack1,Stack2,no),
	parse_dg(Lang,Word,Rest,Stack2,Parse).


try_reduce(Lang,Level,[W1,W2,W3,W4|Stack],NewStack,B) :-
	dbl_dependency(Lang,Level,W2,W3,W4,W5,Degree),
	no_dependency(Lang,Level,W1,W2,Degree),
	notify_reduce(Degree,W2+W3+W4==>W5,[W1,W5|Stack]),
	try_reduce(Lang,Level,[W1,W5|Stack],NewStack,B).
try_reduce(Lang,Level,[W1,W2,W3|Stack],NewStack,B) :-
	dependency(Lang,Level,W2,W3,W4,Degree),
	no_dependency(Lang,Level,W1,W2,Degree),
	no_dbl_dependency(Lang,Level,W1,W2,W3,Degree),
	notify_reduce(Degree,W3+W2==>W4,[W1,W4|Stack]),
	try_reduce(Lang,Level,[W1,W4|Stack],NewStack,B).
try_reduce(Lang,Level,[W1,W2|Stack],NewStack,yes) :-
	dependency(Lang,Level,W1,W2,W3,Degree),
	notify_reduce(Degree,W2+W1==>W3,[W3|Stack]),
	try_reduce(Lang,Level,[W3|Stack],NewStack,yes).
try_reduce(Lang,Level,[W1,W2,W3|Stack],NewStack,yes) :-
	dbl_dependency(Lang,Level,W1,W2,W3,W5,Degree),
	notify_reduce(Degree,W3+W2+W1==>W5,[W5|Stack]),
	try_reduce(Lang,Level,[W5|Stack],NewStack,yes).
try_reduce(_Lang,_Level,Stack,Stack,_).


reduce_all(_Lang,_Level,[Parse],Parse) :- !.
reduce_all(Lang,Level,_Stack,noparse) :- 
	max_dep_threshold(Lang,L),
	Level > L,
	!.
reduce_all(Lang,Level,Stack,Parse) :- 
	NewLevel is Level + 1,
	reverse(Stack,NewStack),
	reparse(Lang,NewLevel,NewStack,[],Parse).

reparse(_Lang,_Level,[],[Parse],Parse) :- !.
reparse(Lang,Level,[],Stack,Parse) :- 
	reduce_all(Lang,Level,Stack,Parse),!.
reparse(Lang,Level,[W],Stack,Parse) :-
	!,
	try_reduce(Lang,Level,[W|Stack],Stack1,no),
	try_reduce(Lang,Level,Stack1,Stack2,yes),
	reduce_all(Lang,Level,Stack2,Parse).
reparse(Lang,Level,[W|Sent],Stack,Parse) :-
	try_reduce(Lang,Level,[W|Stack],Stack1,no),
	reparse(Lang,Level,Sent,Stack1,Parse).
	


dependency(Lang,Level,W2,W1,W3,Deg) :-
	dep(Lang,Deg,W1+W2 ==> W3,C),
	dep_threshold(Lang,Level,X),
	Deg < X,
	check_cond_list(Lang,C).

dbl_dependency(Lang,Level,W3,W2,W1,W4,Deg) :-
	dep(Lang,Deg,W1+W2+W3 ==> W4,C),
	dep_threshold(Lang,Level,X),
	Deg < X,
	check_cond_list(Lang,C).

no_dependency(Lang,_Level,W2,W1,Deg) :-
	dependency(Lang,_Level1,W2,W1,_,Deg1),
	!,
	Deg < Deg1.
no_dependency(_,_,_,_,_).

no_dbl_dependency(Lang,Level,W3,W2,W1,Deg) :-
	dbl_dependency(Lang,Level,W1,W2,W3,_W4,Deg1),
	!,
	Deg < Deg1.
no_dbl_dependency(_Lang,_Level,_W3,_W2,_W1,_Deg).


/* MORPHOLOGICAL DEPENDENCY PARSER */

dg_parse_word_standalone(Lang,Word,Analysis,Lex) :-
	append(Word,"#",Word1),
	dg_scan_rest(Lang,start,Word1,parsed("#",[],[]),parsed(_,[45|Lex],_),[],Stack),
	!,
	reduce_all(Lang,1,Stack,Analysis).

dg_morph_parse_word(Lang,Sent,PrevWord,Word2,Stack,Rest,NewStack) :-
	get_word(Sent,Word,Rest),
	check_external_sandhi(Lang,PrevWord,Word,Rest,Word2),
	!,
	dg_morph_parse_word_aux(Lang,Word2,Stack,NewStack).

dg_morph_parse_word_aux(Lang,Word,Stack,[ct(Cat,LF)|Stack]) :-
	m(Lang,Cat,Word,Exp),
	convert_lexical(Lang,Cat,Exp,LF),!.
dg_morph_parse_word_aux(Lang,Word2,Stack,NewStack) :-
	append(Word2,"#",Word1),
	dg_scan_rest(Lang,start,Word1,parsed("#",[],[]),_Parsed1,Stack,NewStack).

dg_scan_rest(Lang,Class,[],Parsed,Parsed,Stack,Stack) :-
	arc(Lang,Class,stop),!.
dg_scan_rest(Lang,Class,"#",Parsed,Parsed,Stack,Stack) :-
	arc(Lang,Class,stop),!.
dg_scan_rest(Lang,OldClass,RestWord,Parsed,NewParsed,Stack,NewStack) :-
	find_morph(Lang,NewClass,NewExponent,RestWord,NewRestWord,Parsed,Parsed1),
	arc(Lang,OldClass,NewClass),
	convert_lexical(Lang,NewClass,NewExponent,LF),
	append([ct(NewClass,LF)],Stack,Stack1),
	notify_shift_morph(Stack1),
	try_reduce(Lang,1,Stack1,Stack2,no),
	dg_scan_rest(Lang,NewClass,NewRestWord,Parsed1,NewParsed,Stack2,NewStack).

convert_lexical(Lang,Class,Exp,LF) :-
	dg_class_macro(Lang,Class,Exp,LF),!.
convert_lexical(_Lang,_Class,LF,LF).

check_external_sandhi(Lang,Prev,Surf,Next,Lex) :-
	allo(Lang,Surf,Lex,_Residue,PredSurf,SuccSurf,[],[],Conditions),
	check_pred(PredSurf,Prev),
	check_succ(SuccSurf,Next),
	check_cond_list_aux(Lang,Conditions,PredSurf,[]),
	!.
check_external_sandhi(_,_,W,_,W).


/* DEPENDENCY GENERATOR (MORPHOLOGICAL AND SYNTACTICAL) */

dg_gen(Lang,LF,PF) :-
	dg_gen_aux(Lang,LF,Lex),
	lex_to_surf(Lang,Lex,PF).

dg_gen_aux(Lang,ct(Cat,LF),PF) :-
	lex(Lang,Cat,PF,LF),
	notify_gen_found(PF),
	!.
dg_gen_aux(Lang,LF,PF) :-
	dep(Lang,Deg,A+B+D ==> LF,C),
	check_cond_list_reverse(Lang,C),
	notify_gen_pushdown(Deg,A+B+D ==> LF,C),
	dg_gen_aux(Lang,A,PF1),
	dg_gen_aux(Lang,B,PF2),
	dg_gen_aux(Lang,D,PF3),
	find_correct_boundary(Lang,Deg,A,B,Boundary1),
	find_correct_boundary(Lang,Deg,B,D,Boundary2),	
	append(PF1,[Boundary1],NewPF1),
	append(NewPF1,PF2,NewPF2),	
	append(NewPF2,[Boundary2],NewPF3),	
	append(NewPF3,PF3,PF),
	notify_gen_sofar(PF).
dg_gen_aux(Lang,LF,PF) :-
	dep(Lang,Deg,A+B ==> LF,C),
	check_cond_list_reverse(Lang,C),
	notify_gen_pushdown(Deg,A+B ==> LF,C),
	dg_gen_aux(Lang,A,PF1),
	dg_gen_aux(Lang,B,PF2),
	find_correct_boundary(Lang,Deg,A,B,Boundary),
	append(PF1,[Boundary],NewPF1),
	append(NewPF1,PF2,PF),
	notify_gen_sofar(PF).

find_correct_boundary(Lang,_Deg,ct(Cat1,_LF1),ct(Cat2,_LF2),45) :-
	arc(Lang,Cat1,Cat2),!.
find_correct_boundary(Lang,Deg,_A,_B,45) :-
	morph_threshold(Lang,Deg1),
	Deg < Deg1,
	!.
find_correct_boundary(_Lang,_Deg,_A,_B,32).

lex_to_surf(Lang,Lex,PF) :-
	lex_to_surf(Lang,Lex,[],PF),!.
lex_to_surf(_Lang,[],PF,PF).
lex_to_surf(Lang,Lex,PF,ResultPF) :-
	dg_get_next_word(Lex,Word,NewLex),
	scan_phonemes(Lang,Word,NewWord),
	append(PF,[32|NewWord],PF1),
	lex_to_surf(Lang,NewLex,PF1,ResultPF).

dg_get_next_word([32|Lex],[],Lex) :- !.
dg_get_next_word([],[],[]).
dg_get_next_word([C|Lex],[C|Word],Rest) :-
	dg_get_next_word(Lex,Word,Rest).

lex(Lang,Cat,PF,LF) :-
	m(Lang,Cat,PF,LF),!.
lex(Lang,Cat,PF,LF) :-
	dg_class_macro(Lang,Cat,Exp,LF),
	m(Lang,Cat,PF,Exp).



/* SYNTACTICAL DEPENDENCY GENERATOR for MORPHOLOGICAL TAGMEMIC GRAMMAR */

dg_gen_tagm(Lang,LF,PF) :-
	dg_gen_tagm_aux(Lang,LF,Lex), 
	tm_morph_gen_sent(Lang,Lex,PF).

dg_gen_tagm_aux(Lang,ct(Cat,LF),[m(Cat,LF)]) :-
	morph_synt(Lang,Cat,_Tagmeme,LF,_C),
	!.
dg_gen_tagm_aux(Lang,LF,Lex) :-
	dep(Lang,_Deg,A+B ==> LF,C),
	check_cond_list_reverse(Lang,C),
	dg_gen_tagm_aux(Lang,A,Lex1),
	dg_gen_tagm_aux(Lang,B,Lex2),
	append(Lex1,Lex2,Lex).





     	
