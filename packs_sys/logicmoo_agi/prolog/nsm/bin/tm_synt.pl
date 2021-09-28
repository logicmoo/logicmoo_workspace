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


% VERSIONE STANDALONE DEL CHART-PARSER E GENERATORE TOP-DOWN XSTAGMEMICO

:- op(700,xfx,<>).
:- op(700,xfx,=>).
:- op(500,xfx,//).
:- op(850,fx,?).
:- op(800,xfx,===).
:- op(900,xfx,::).
:- op(700,fx,'P').
:- op(740,xfx,:).
:- op(700,xfx,/).
:- op(700,xfx,<<).


:- dynamic(syntagmeme/5).


parse(Lang,Sent,Parse) :-
	morph_analysis(Sent,Lex),
	parse_tagm(Lang,Lex,Parse1,Stack,b(0,Breaks)),
	reduce(Lang,Parse1,Parse2,Stack,Breaks),
	check_parse(Lang,Parse2,Parse).

morph_analysis(Sent,Sent).

check_parse(_Lang,[m(Slot,Filler)],Slot:Filler).
check_parse(Lang,[Item|PartialParse],Parse) :-
	find_tagmeme(Lang,Item,Tagmeme),
	traverse(Lang,[Item|PartialParse],Tagmeme,Parse1,Stack,0,Breaks),
	reduce(Lang,Parse1,Parse2,Stack,Breaks),
	check_parse(Lang,Parse2,Parse).

parse_tagm(Lang,[Item|Sent],Parse,Stack,b(Break,NewBreak)) :-
	find_tagmeme(Lang,Item,Tagmeme),
%	!,
	traverse(Lang,[Item|Sent],Tagmeme,Parse,Stack,Break,NewBreak).
parse_tagm(Lang,[Item|Sent],[Item|Parse],Stack,b(Break,NewBreak)) :-
	parse_tagm(Lang,Sent,Parse,Stack,b(Break,NewBreak)).
parse_tagm(_,[],[],[],b(B,B)).

find_tagmeme(Lang,m(Class,Exponent),t(Name,[Slot:Class/Exponent|P],A,C)) :-
	syntagmeme(Lang,Name,[Slot:Class/Exponent|P],A,C).
find_tagmeme(Lang,m(Class,Exponent),t(Name,[?Slot:Class/Exponent|P],A,C)) :-
	syntagmeme(Lang,Name,[?Slot:Class/Exponent|P],A,C).
find_tagmeme(Lang,m(Class,Exponent),t(Name,[Slot:[F|Iller]/Exponent|P],A,C)) :-
	syntagmeme(Lang,Name,[Slot:[F|Iller]/Exponent|P],A,C),
	member(Class,[F|Iller]).
find_tagmeme(Lang,m(Class,Exponent),t(Name,[?Slot:[F|Iller]/Exponent|P],A,C)) :-
	syntagmeme(Lang,Name,[Slot:[F|Iller]/Exponent|P],A,C),
	member(Class,[F|Iller]).
find_tagmeme(Lang,m(Class,Exponent),t(Name,[Slot:Class/Exponent|P],A,C)) :-
	syntagmeme(Lang,Name,[?_Slot1:_Class1/e|P1],A,C),
	check_syntagmeme(morpheme(Slot,Class,Exponent),P1,P,A).


/*
find_tagmeme(Lang,Class:Exponent,t(Name,[Slot:Class/Exponent|P],A,C)) :-
	syntagmeme(Lang,Name,[Slot:Class/Exponent|P],A,C).
find_tagmeme(Lang,m(Class,Exponent),t(Name,[?Slot:Class/Exponent|P],A,C)) :-
	syntagmeme(Lang,Name,[?Slot:Class/Exponent|P],A,C).
find_tagmeme(Lang,m(Class,Exponent),t(Name,[Slot:[F|Iller]/Exponent|P],A,C)) :-
	syntagmeme(Lang,Name,[Slot:[F|Iller]/Exponent|P],A,C),
	member(Class,[F|Iller]).
find_tagmeme(Lang,m(Class,Exponent),t(Name,[?Slot:[F|Iller]/Exponent|P],A,C)) :-
	syntagmeme(Lang,Name,[Slot:[F|Iller]/Exponent|P],A,C),
	member(Class,[F|Iller]).
find_tagmeme(Lang,m(Class,Exponent),t(Name,[?Slot:Class/Exponent|P],A,C)) :-
	syntagmeme(Lang,Name,[?_Slot1:_Class1/e|P1],A,C),
	check_syntagmeme(morpheme(Slot,Class,Exponent),P1,P,A).
*/



% Presa da morph.pl
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



traverse(Lang,[],t(Name,[],A,C),[m(Name,A)],[],Breaks,Breaks) :-
	check_cond_list(Lang,C),!.
traverse(Lang,Sent,t(Name,[],A,C),[m(Name,A)|Parse],Stack,Breaks1,Breaks) :-
	check_cond_list(Lang,C),
	!,
	parse_tagm(Lang,Sent,Parse,Stack,b(Breaks1,Breaks)).
traverse(Lang,[m(Class,Exp)|Sent],t(Name,P,A,C),Parse,Stack,Breaks1,Breaks) :-
	check_syntagmeme(morpheme(_,Class,Exp),P,NewP,A),
	!,
	traverse(Lang,Sent,t(Name,NewP,A,C),Parse,Stack,Breaks1,Breaks).
traverse(Lang,Sent,t(Name,P,A,C),[break(M)|Parse],[t(Name,P,A,C)|Stack],N,Breaks) :-
	M is N + 1,
	parse_tagm(Lang,Sent,Parse,Stack,b(M,Breaks)).
	
reduce(_Lang,Parse,Parse,[],0).
reduce(Lang,Parse,NewParse,[Tagmeme|Stack],N) :-
	N > 0,
	split(Parse,break(N),A,B),
	traverse(Lang,B,Tagmeme,Parse1,NewStack,0,0),
	M is N - 1,
	append(A,Parse1,NewParse1),
	append(NewStack,Stack,Stack1),
	reduce(Lang,NewParse1,NewParse,Stack1,M).
	

split(List,Item,A,B) :-
	append(A,[Item],X),
	append(X,B,List).


/* PRESO DA MORPH.PL */

check_cond_list_aux(L,C,_A,_B) :-
	check_cond_list(L,C).


check_cond_list(_,[]).
check_cond_list(Lang,[Cond|List]) :-
	check_cond(Lang,Cond),!,
	check_cond_list(Lang,List).

check_cond(_Lang,'P' Cond) :-
	!,call(Cond).
check_cond(Lang,(Segm << Class)) :- 
	!,
        phonetic_class(Lang,Class,Members),
	member([Segm],Members).
check_cond(_Lang,pref(Pref,X)) :-
	!,
        append(Pref,_,X).
check_cond(_Lang,suff(Suff,X)) :-
	!,
        append(_,Suff,X).
check_cond(Lang,Cond) :-
	call(paradigm(Lang,Cond)).










/* adattato da MORPH.PL */

gen(Lang,_Slot:F,S) :-
	syntagmeme(Lang,_Name,Synt,F,C),
	check_cond_list_aux(Lang,C,Synt,[]),
	scan_syntagmeme(Lang,Synt,[],S).



scan_syntagmeme(_Lang,[],Lex,Lex).
scan_syntagmeme(Lang,[?_Slot:_Filler/e|Synt],Lex1,Lex) :-
	!,scan_syntagmeme(Lang,Synt,Lex1,Lex).
scan_syntagmeme(Lang,[_Slot:_Filler/e|Synt],Lex1,Lex) :-
	!,scan_syntagmeme(Lang,Synt,Lex1,Lex).
scan_syntagmeme(Lang,[_Slot:Filler/A|Synt],Lex1,Lex) :-
	syntagmeme(Lang,Filler,P,A,C),
	check_cond_list_aux(Lang,C,Synt,[]),
	!,
	scan_syntagmeme(Lang,P,Lex1,Lex2),
	scan_syntagmeme(Lang,Synt,Lex2,Lex).
scan_syntagmeme(Lang,[?_Slot:Filler/A|Synt],Lex1,Lex) :-
	syntagmeme(Lang,Filler,P,A,C),
	check_cond_list_aux(Lang,C,Synt,[]),
	!,
	scan_syntagmeme(Lang,P,Lex1,Lex2),
	scan_syntagmeme(Lang,Synt,Lex2,Lex).
scan_syntagmeme(Lang,[_Slot:Filler/Exp|Synt],Lex1,Lex) :-
	append(Lex1,[m(Filler,Exp)],Lex2),
	scan_syntagmeme(Lang,Synt,Lex2,Lex).
scan_syntagmeme(Lang,[?_Slot:Filler/Exp|Synt],Lex1,Lex) :-
	append(Lex1,[m(Filler,Exp)],Lex2),
	scan_syntagmeme(Lang,Synt,Lex2,Lex).

% tagmeme(Lang,Name,P,A,C).

syntagmeme(chn,s,[?time:tense/T, subj:np/S, h:v/V],[tense:T, subj:S, v:V],[]).
syntagmeme(chn,np,[?det:det/D, ?mod:a/A, h:n/N],[det:D, adj:A, n:N],[]).

m(tense,anGadi).
m(n,tluchman).
m(a,dEnEs).
m(det,uk).
m(v,tladwa).


go :-
	parse(chn,[m(det,uk),m(a,dEnEs),m(n,tluchman),m(v,tladwa)],X), write(X).






