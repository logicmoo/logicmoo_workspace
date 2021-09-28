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

p_chart(Lang,MorphParser,Sent,Name:Parse,Rest) :-
	init_chart(Lang,MorphParser,length(1,Max),Sent,Rest),
	edge(1,Max,Name,_Found,[],Parse,C),
	check_cond_list_aux(Lang,C,Parse,_),
	!,
	retractall(edge(_,_,_,_,_,_,_)).
p_chart(_Lang,_,Sent,noparse:Sent,[]) :-
	retractall(edge(_,_,_,_,_,_,_)).
%RIVEDERE SECONDA CLAUSOLA: RESTITUIRE FRASE SCORRETTA PARI PARI SOLO FINO A PUNTO E POI IL RESTO IN REST


init_chart(_Lang,_MorphParser,length(Max,Max),[],[]) :- !.
init_chart(_Lang,_MorphParser,length(Max,Max),[fullstop|Rest],Rest) :- !.
init_chart(Lang,MorphParser,length(N,M),Sent,Rest) :-
	get_next_word(Lang,MorphParser,Sent,ListOfReadings,Rest1),
	X is N + 1,
	add_empty_edges(Lang,ListOfReadings,N,X,Lang),
	init_chart(Lang,MorphParser,length(X,M),Rest1,Rest).


get_next_word(_,_,[],[],[]).
get_next_word(_Lang,e,[W|S],[W],S).
%USO CON TM_MORPH
get_next_word(Lang,tm_morph,String,W,Rest) :-
	tm_morph_all_readings(Lang,tagm,String,W,Rest).



add_empty_edges(_,[],_,_,_).
add_empty_edges(Lang,[m(Cat,Exp)|Readings],From,To,Lang) :-
	add_edge(Lang,From,To,Cat,[],[],Exp,[]),
	add_empty_edges(Lang,Readings,From,To,Lang).

add_edge(_Lang,From,To,Cat,Found,ToFind,A,C) :-
	edge(From,To,Cat,Found,ToFind,A,C),!.
add_edge(Lang,From,To,Cat,Found,[],Exp,C) :-
	asserta(edge(From,To,Cat,Found,[],Exp,C)),
	find_left_corners(Lang,Cat,Exp,SyntList),
	add_active_edges(Lang,From,SyntList),
	advance_dotted_edges(Lang,From,To,Cat,Exp).
add_edge(Lang,From,To,Cat,Found,[Name|ToFind],A,C) :-
	asserta(edge(From,To,Cat,Found,[Name|ToFind],A,C)),
	findall(edge(To,NewTo,Name,Found1,[],A1,C1),
		edge(To,NewTo,Name,Found1,[],A1,C1),
		List),
	advance_dotted_cat(List,Lang,Cat,From,A1,ToFind,A,C).

advance_dotted_cat([],_Lang,_Cat,_From,_A1,_ToFind,_A,_C).
advance_dotted_cat([edge(_To,NewTo,Name,Found1,[],A1,_C1)|List],
		  Lang,Cat,From,A1,ToFind,A,C) :-
	add_edge(Lang,From,NewTo,Cat,[Name:A1|Found1],ToFind,A,C),
	advance_dotted_cat(List,Lang,Cat,From,A1,ToFind,A,C).


advance_dotted_edges(Lang,From,To,Cat,Exp) :-
	findall(edge(X,From,Name,Found2,[Slot:Cat/Exp|P2],A2,C2),
		edge(X,From,Name,Found2,[Slot:Cat/Exp|P2],A2,C2),
		List1),
	findall(edge(X,From,Name,Found2,[Slot:Cat/Exp|P2],A2,C2),
		edge(X,From,Name,Found2,[?Slot:Cat/Exp|P2],A2,C2),
		List2),
	findall(edge(X1,From,Name1,Found1,[?Slot1:Cat1/Exp1|P1],A1,C1),
		edge(X1,From,Name1,Found1,[?Slot1:Cat1/Exp1|P1],A1,C1),
		List3),
	check_edges(List3,Cat,Exp,List4),
	append(List1,List2,List5),
	append(List5,List4,List),
	advance_dotted_list(Lang,To,List).

advance_dotted_list(_,_,[]).
advance_dotted_list(Lang,To,[edge(X,_From,Name,Found2,[Slot:Cat/Exp|P2],A2,C2)|List]) :-
	check_cond_if_inactive(Lang,P2,C2,A2),
	add_edge(Lang,X,To,Name,[Slot:Cat/Exp|Found2],P2,A2,C2),
	advance_dotted_list(Lang,To,List).

check_cond_if_inactive(Lang,[],C,A) :-
	check_cond_list_aux(Lang,C,A,_).
check_cond_if_inactive(_Lang,[_|_],_C,_A).

find_left_corners(Lang,Cat,Exp,SyntList) :-
	findall(synt(Lang,Name,[Slot:Cat/Exp|P1],A,C),
		syntagmeme(Lang,Name,[Slot:Cat/Exp|P1],A,C),
		List1),
	add_facultative(Lang,Cat,Exp,List1,SyntList).

add_facultative(Lang,Cat,Exp,List1,SyntList) :-
	findall(synt(Lang,Name,[?Slot1:Cat1/Exp1|P1],A,C),
		syntagmeme(Lang,Name,[?Slot1:Cat1/Exp1|P1],A,C),
		List),
	check_syntagmemes(List,Cat,Exp,NewList),
	append(List1,NewList,SyntList).


add_active_edges(_Lang,_From,[]).
add_active_edges(Lang,From,[synt(Lang,Name,P,A,C)|SyntList]):-
	add_edge(Lang,From,From,Name,[],P,A,C),
	add_active_edges(Lang,From,SyntList).
	


check_edges([],_Cat,_Exp,[]).
check_edges([edge(X1,From,Name1,Found1,[?_Slot1:_Cat1/e|P1],A,C)|L1],
		  Cat,Exp,
		  [edge(X1,From,Name1,Found1,[Slot:Cat/Exp|P],A,C)|L2]) :-
	check_syntagmeme(morpheme(Slot,Cat,Exp),P1,P,A),
	!,
	check_edges(L1,Cat,Exp,L2).
check_edges([_Edge|L1],
		  Cat,Exp,L2) :-
	check_edges(L1,Cat,Exp,L2).


check_syntagmemes([],_Cat,_Exp,[]).
check_syntagmemes([synt(Lang,Name,P,A,C)|L1],
		  Cat,Exp,
		  [synt(Lang,Name,[Slot:Cat/Exp|NewP],A,C)|L2]) :-
	check_syntagmeme(morpheme(Slot,Cat,Exp),P,NewP,A),
	!,
	check_syntagmemes(L1,Cat,Exp,L2).
check_syntagmemes([_Synt|L1],
		  Cat,Exp,L2) :-
	check_syntagmemes(L1,Cat,Exp,L2).
	

/* da MORPH.PL 
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

*/

syntagmeme(chn,s,[?time:tense/T, subj:np/S, h:v/V],[tense:T, subj:S, v:V],[]).
syntagmeme(chn,np,[?det:det/D, ?mod:a/A, h:n/N],[det:D, adj:A, n:N],[]).


go :-
	retractall(edge(_,_,_,_,_,_,_)),
	p_chart(chn,e,[m(det,uk),m(n,tluchman),m(v,tladwa)],Parse,_Rest),
	write(Parse).
