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

check_cond_list_reverse(Lang,CondList) :-
	reverse(CondList,C),
	check_cond_list(Lang,C).

check_cond_list_aux(L,C,_A,_B) :-
	check_cond_list(L,C).

check_cond_list(_,[]).
check_cond_list(Lang,[Cond|List]) :-
	notify_check(Cond),
	check_cond(Lang,Cond),!,
	notify_check_ok(Cond),
	check_cond_list(Lang,List).

check_cond(_Lang,'P' Cond) :-
	!,call(Cond).
check_cond(Lang,not(Cond)) :-
	check_cond(Lang,Cond),
	!,
	fail.
check_cond(_Lang,not(_Cond)) :- !.
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

check_pred(A,B) :-
	append(_,A,B),!.
check_succ(A,B) :-
	append(A,_,B),!.


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
