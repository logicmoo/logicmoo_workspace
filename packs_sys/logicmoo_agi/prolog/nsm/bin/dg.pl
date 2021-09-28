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

    :- module(dg,[
		  dg_parse/3,
		  dg_gen/3,
		  dg_gen_tagm/3,
		  dg_parse_text_level/3,
		  dg_gen_text_level/3
		 ]).
		  
/** <module> Dependency grammar

This module contains the routines for syntactic parsing and
generation with NSM-DALIA dependency grammars.

@tbd dg_parse_text_level/3 and dg_gen_text_level/3 implementation.
*/
    

:- include('operators.pl').

:- use_module(messages).
:- use_module(checkers).
:- use_module(grammar).
:- use_module(word_scanner).
:- use_module(morph_parse).
:- use_module(morph_gen).


/* SYNTACTICAL DEPENDENCY PARSER  */


%%	dg_parse(+Lang,+Sentence:string,-Parse) is det
%
%	Parses a sentence written in language Lang according
%	to the active transcription table (see module transcr.pl)
%	and returns the parse in the third argument.
%	
%	The first word is parsed, calling 
%	morph_parse:generic_parse_word/7. Then, the internal 
%	procedure parse_dg/5 does the job.
dg_parse(Lang,Sent,ct(CT,Parse)) :-
	generic_parse_word(Lang,Sent,"##",Word,[],Rest,Stack),
	parse_dg(Lang,Word,Rest,Stack,ct(CT,Parse1)),
	explain_idiom(Lang,Parse1,Parse),
	!.
dg_parse(_,Sent,noparse(Sent)).
   

parse_dg(Lang,_PrevWord,[],Stack,Parse) :-
%	!,
	try_reduce(Lang,1,Stack,NewStack,yes),
	reduce_all(Lang,1,NewStack,Parse).
parse_dg(Lang,PrevWord,Sent,Stack,Parse) :-
	generic_parse_word(Lang,Sent,PrevWord,Word,Stack,Rest,Stack1),
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
try_reduce(Lang,Level,[W1,W2|Stack],NewStack,yes) :- %yes
	dependency(Lang,Level,W1,W2,W3,Degree),
	notify_reduce(Degree,W2+W1==>W3,[W3|Stack]),
	try_reduce(Lang,Level,[W3|Stack],NewStack,yes).
try_reduce(Lang,Level,[W1,W2,W3|Stack],NewStack,yes) :- %yes
	dbl_dependency(Lang,Level,W1,W2,W3,W5,Degree),
	notify_reduce(Degree,W3+W2+W1==>W5,[W5|Stack]),
	try_reduce(Lang,Level,[W5|Stack],NewStack,yes).
try_reduce(_Lang,_Level,Stack,Stack,_).


reduce_all(_Lang,_Level,[Parse],Parse) :- !.
reduce_all(Lang,Level,Stack,Parse) :- 
	grammar:max_dep_threshold(Lang,L),	
	Level < L,
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
	grammar:dep(Lang,Deg,W1+W2 ==> W3,C),
	grammar:dep_threshold(Lang,Level,X),
	Deg < X,
	check_cond_list(Lang,C).

dbl_dependency(Lang,Level,W3,W2,W1,W4,Deg) :-
	grammar:dep(Lang,Deg,W1+W2+W3 ==> W4,C),
	grammar:dep_threshold(Lang,Level,X),
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



explain_back_idiom(Lang,ct(Cat,Parse1),ct(Cat,Parse)) :-
	explain_idiom(Lang,Parse,Parse1).

explain_idiom(Lang,Parse,Parse1) :-
	grammar:idiom(Lang,Parse,Parse1),
	!.

explain_idiom(Lang,if(S1,S2),if(S1a,S2a)) :-
	explain_idiom(Lang,S1,S1a),
	explain_idiom(Lang,S2,S2a).
explain_idiom(Lang,when(S1,S2),when(S1a,S2a)) :-
	explain_idiom(Lang,S1,S1a),
	explain_idiom(Lang,S2,S2a).
explain_idiom(Lang,because(S1,S2),because(S1a,S2a)) :-
	explain_idiom(Lang,S1,S1a),
	explain_idiom(Lang,S2,S2a).
explain_idiom(Lang,like(S1,S2),like(S1a,S2a)) :-
	explain_idiom(Lang,S1,S1a),
	explain_idiom(Lang,S2,S2a).
explain_idiom(Lang,s(A,B,C,D,E,p(P,[R:S,prop:PROP|REST]),F,G),
	           s(A,B,C,D,E,p(P,[R:S,prop:PROP1|REST]),F,G)) :-
	explain_idiom(Lang,PROP,PROP1).
explain_idiom(_Lang,Parse,Parse).


%%	dg_parse_text_level(+Lang,-Parse,+NewParse) is det
%
%	Scans a parse result for _|text-level dependencies|_.
%	To be done.
%	
dg_parse_text_level(_,T,T).
/*
dg_parse_text_level(Lang,T,T) :-
	max_dep_threshold(Lang,Threshold),
	actual_max_dep(Lang,MaxDep),
	MaxDep < Threshold,
	!.

dg_parse_text_level(Lang,T1,T2) :-
	max_dep_threshold(Lang,Threshold),
	dg_parse_text_level_aux(Lang,Threshold,T1,T2).

dg_parse_text_level_aux(Lang,Level,[S1,S2|T1],[S3,S4|T2]) :-
	text_dependency(Lang,Level,S1,S2,S3+S4),
	!,
	dg_parse_text_level_aux(Lang,Level,T1,T2).
dg_parse_text_level_aux(Lang,Level,[S1,S2|T1],[S3|T2]) :-
	text_dependency(Lang,Level,S1,S2,S3),
	!,
	dg_parse_text_level_aux(Lang,Level,T1,T2).
dg_parse_text_level_aux(Lang,Level,[S1|T1],[S1|T2]) :-
	dg_parse_text_level_aux(Lang,Level,T1,T2).
dg_parse_text_level_aux(_,_,[],[]).

*/

/* MORPHOLOGICAL DEPENDENCY PARSER */

/* DEPENDENCY GENERATOR (MORPHOLOGICAL AND SYNTACTICAL) */

%%	dg_gen(+Lang,+LF,-PF) is det
%
%	Generates a sentence into language Lang, from the 
%	semantic representation contained in LF, and unifies the
%	third argument with the generated sentence. 
%	
%	Procedure dg_gen_aux/3 generates the sentence as a sequence
%	of morphemes, which is then turned into a string by
%	procedure lex_to_surf/3.
dg_gen(Lang,LF,PF) :-
	explain_back_idiom(Lang,LF,LF1),
	dg_gen_aux(Lang,LF1,Lex),
	lex_to_surf(Lang,Lex,PF).

dg_gen_aux(Lang,ct(Cat,LF),PF) :-
	lex(Lang,Cat,PF,LF),
	notify_gen_found(PF),
	!.
dg_gen_aux(Lang,LF,PF) :-
	grammar:dep(Lang,Deg,A+B+D ==> LF,C),
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
	grammar:dep(Lang,Deg,A+B ==> LF,C),
	check_cond_list_reverse(Lang,C),
	notify_gen_pushdown(Deg,A+B ==> LF,C),
	dg_gen_aux(Lang,A,PF1),
	dg_gen_aux(Lang,B,PF2),
	find_correct_boundary(Lang,Deg,A,B,Boundary),
	append(PF1,[Boundary],NewPF1),
	append(NewPF1,PF2,PF),
	notify_gen_sofar(PF).

/*
find_correct_boundary(Lang,_Deg,ct(Cat1,_LF1),ct(Cat2,_LF2),45) :-
	grammar:arc(Lang,Cat1,Cat2),!.
*/

find_correct_boundary(Lang,Deg,_A,_B,45) :-
	grammar:morph_threshold(Lang,Deg1),
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
	grammar:m(Lang,Cat,PF,LF),!.
lex(Lang,Cat,PF,LF) :-
	grammar:dg_class_macro(Lang,Cat,Exp,LF),
	grammar:m(Lang,Cat,PF,Exp).
lex(_Lang,group,[],_).



% TEXT LEVEL DEPENDENCIES (funzionano solo con traduzione, parse text,
% gen text)

%%	dg_gen_text_level(+Lang,+NSMFormulas,-NewNSMFormulas) is det
%
%	Scans a formula for _|text-level dependencies|_ before generation.
%	To be done.
%	
dg_gen_text_level(_,T,T).
/*
% prima clausola: so non ho dipendenze di livello testo, 
% inutile fare lo scanning
dg_gen_text_level(Lang,T,T) :-
	max_dep_threshold(Lang,Threshold),
	actual_max_dep(Lang,MaxDep),
	MaxDep < Threshold,
	!.
dg_gen_text_level(Lang,T1,T2) :-
	max_dep_threshold(Lang,Threshold),
	dg_gen_text_level_aux(Lang,Threshold,T1,T2).

dg_gen_text_level_aux(Lang,Level,[S1,S2|T1],[S3,S4|T2]) :-
	text_dependency(Lang,Level,S3,S4,S1+S2),
	!,
	dg_gen_text_level_aux(Lang,Level,T1,T2).
dg_gen_text_level_aux(Lang,Level,[S3|T1],[S1,S2|T2]) :-
	text_dependency(Lang,Level,S1,S2,S3),
	!,
	dg_gen_text_level_aux(Lang,Level,T1,T2).
dg_gen_text_level_aux(Lang,Level,[S1|T1],[S1|T2]) :-
	dg_gen_text_level_aux(Lang,Level,T1,T2).
dg_gen_text_level_aux(_,_,[],[]).
*/

text_dependency(Lang,Level,S1,S2,S3) :-
	grammar:dep(Lang,Deg,S1+S2==>S3,C),
	check_cond_list(Lang,C),
	Deg >= Level.


/* SYNTACTICAL DEPENDENCY GENERATOR for MORPHOLOGICAL TAGMEMIC GRAMMAR */

%%	dg_gen_tagm(+Lang,+LF,-PF) is det
%
%	Syntactic generator for tagmemic grammars.
%	To be revised.
%	
dg_gen_tagm(Lang,LF,PF) :-
	dg_gen_tagm_aux(Lang,LF,Lex), 
	tm_morph_gen_sent(Lang,Lex,PF).

dg_gen_tagm_aux(Lang,ct(Cat,LF),[m(Cat,LF)]) :-
	grammar:morph_synt(Lang,Cat,_Tagmeme,LF,_C),
	!.
dg_gen_tagm_aux(Lang,LF,Lex) :-
	grammar:dep(Lang,_Deg,A+B ==> LF,C),
	check_cond_list_reverse(Lang,C),
	dg_gen_tagm_aux(Lang,A,Lex1),
	dg_gen_tagm_aux(Lang,B,Lex2),
	append(Lex1,Lex2,Lex).

















