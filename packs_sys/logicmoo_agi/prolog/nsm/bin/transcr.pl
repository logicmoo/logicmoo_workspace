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


:- include('dynamic.pl').


transcr(Lang,S,S1) :-
	transcr_table(Lang,T),
	!,
	transcr_aux(T,S,[],S1).
transcr(_,S,S).

transcr_aux(T,S,[],S1) :-
	var(S1),
	transcr_sent(T,S,[],S1).

transcr_aux(T,S,[],S1) :-
	var(S),
	transcr_back_sent(T,S1,[],S).
	


transcr_back(Lang,S,S1) :-
	transcr_table(Lang,T),
	!,
	transcr_back_sent(T,S,[],S1).
transcr_back(_,S,S).


transcr_back_sent(T,S,NewS,Final) :-
	in_table_back(S,T,P,S1),
	!,
	append(NewS,P,NewS1),
	transcr_back_sent(T,S1,NewS1,Final).

transcr_back_sent(T,[A|S],NewS,Final) :-
	append(NewS,[A],NewS1),
	transcr_back_sent(T,S,NewS1,Final).

transcr_back_sent(_T,[],S,S).


/* PROVARE QUESTO INVECE DI QUELLI ; tieni l'ultima (catchall) */
transcr_sent(T,S,NewS,Final) :-
	in_table(S,T,P,S1),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S1,NewS1,Final).

transcr_sent(T,[A|S],NewS,Final) :-
	append(NewS,[A],NewS1),
	transcr_sent(T,S,NewS1,Final).

transcr_sent(_T,[],S,S).


in_table(Sent,[S:P|_Table],P,NewSent) :-
	append(S,NewSent,Sent),!.
in_table(Sent,[_S1:_P1|Table],P,NewSent) :-
	in_table(Sent,Table,P,NewSent).

in_table_back(Sent,[P:S|_Table],P,NewSent) :-
	append(S,NewSent,Sent),!.
in_table_back(Sent,[_S1:_P1|Table],P,NewSent) :-
	in_table_back(Sent,Table,P,NewSent).

/*

transcr_sent(T,[A,B,C,D,E,F,G|S],NewS,Final) :-
	member([A,B,C,D,E,F,G]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A,B,C,D,E,F|S],NewS,Final) :-
	member([A,B,C,D,E,F]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A,B,C,D,E|S],NewS,Final) :-
	member([A,B,C,D,E]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A,B,C,D|S],NewS,Final) :-
	member([A,B,C,D]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A,B,C|S],NewS,Final) :-
	member([A,B,C]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A,B|S],NewS,Final) :-
	member([A,B]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A|S],NewS,Final) :-
	member([A]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A|S],NewS,Final) :-
	append(NewS,[A],NewS1),
	transcr_sent(T,S,NewS1,Final).
	

transcr_sent_aux(_T,[],S,S).
transcr_sent_aux(T,[A,B,C|S],NewS,Final) :-
	member(P:[A,B,C],T),
	!,
	append(NewS,P,NewS1),
	transcr_sent_aux(T,S,NewS1,Final).
transcr_sent_aux(T,[A,B|S],NewS,Final) :-
	member(P:[A,B],T),
	!,
	append(NewS,P,NewS1),
	transcr_sent_aux(T,S,NewS1,Final).
transcr_sent_aux(T,[A|S],NewS,Final) :-
	member(P:[A],T),
	!,
	append(NewS,P,NewS1),
	transcr_sent_aux(T,S,NewS1,Final).
transcr_sent_aux(T,[A|S],NewS,Final) :-
	append(NewS,[A],NewS1),
	transcr_sent_aux(T,S,NewS1,Final).
	
*/


	
