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


/* GENERATOR */

:- include('operators.pl').
:- include('dynamic.pl').

tm_morph_gen_sent(Lang,F,S) :-
	tm_morph_gen_sent(Lang,F,[],S).

tm_morph_gen_sent(_Lang,[],Sent,Sent).
tm_morph_gen_sent(Lang,[m(Class,Exp)|F],S,NewS) :-
	tm_morph_gen_word(Lang,Class,Exp,W),
	append(W,[32],W1),
	append(S,W1,S1),
	tm_morph_gen_sent(Lang,F,S1,NewS).
	

tm_morph_gen_word(Lang,Class,F,S) :-
	morph_synt(Lang,Class,Synt,F,C),
	check_cond_list_aux(Lang,C,Synt,[]),
	scan_syntagmeme(Lang,Synt,[],[45|Lex]),
	scan_phonemes(Lang,Lex,S).






scan_syntagmeme(_Lang,[],Lex,Lex).
scan_syntagmeme(Lang,[?_Slot:_Filler/e|Synt],Lex1,Lex) :-
	!,scan_syntagmeme(Lang,Synt,Lex1,Lex).
scan_syntagmeme(Lang,[_Slot:_Filler/e|Synt],Lex1,Lex) :-
	!,scan_syntagmeme(Lang,Synt,Lex1,Lex).
scan_syntagmeme(Lang,[_Slot:Filler/Exp|Synt],Lex1,Lex) :-
	m(Lang,Filler,M,Exp),
	append(Lex1,[45],Lex2),
	append(Lex2,M,Lex3),
	!,scan_syntagmeme(Lang,Synt,Lex3,Lex).
scan_syntagmeme(Lang,[?_Slot:Filler/Exp|Synt],Lex1,Lex) :-
	m(Lang,Filler,M,Exp),
	append(Lex1,[45],Lex2),
	append(Lex2,M,Lex3),
	!,scan_syntagmeme(Lang,Synt,Lex3,Lex).
scan_syntagmeme(Lang,[_Slot:[F|Iller]/Exp|Synt],Lex1,Lex) :-
	find_morph_from_list(Lang,Exp,[F|Iller],M),
	append(Lex1,[45],Lex2),
	append(Lex2,M,Lex3),
	!,scan_syntagmeme(Lang,Synt,Lex3,Lex).
scan_syntagmeme(Lang,[?_Slot:[F|Iller]/Exp|Synt],Lex1,Lex) :-
	find_morph_from_list(Lang,Exp,[F|Iller],M),
	append(Lex1,[45],Lex2),
	append(Lex2,M,Lex3),
	!,scan_syntagmeme(Lang,Synt,Lex3,Lex).
scan_syntagmeme(Lang,[_Slot:Filler/Exp|Synt],Lex1,Lex) :-
	is_list(Exp),
	morph_synt(Lang,Filler,NewSynt,Exp,C),
	check_cond_list_aux(Lang,C,Synt,[]),
	scan_syntagmeme(Lang,NewSynt,[],S1),
	append(Lex1,S1,Lex3),
	!,
	scan_syntagmeme(Lang,Synt,Lex3,Lex).
scan_syntagmeme(Lang,[?_Slot:Name/Exp|Synt],Lex1,Lex) :-
	is_list(Exp),
	morph_synt(Lang,Name,NewSynt,Exp,C),
	check_cond_list_aux(Lang,C,Synt,[]),
	scan_syntagmeme(Lang,NewSynt,[],S1),
	append(Lex1,S1,Lex3),
	!,
	scan_syntagmeme(Lang,Synt,Lex3,Lex).
	




find_morph_from_list(Lang,Exp,[F|_Iller],M) :-
	m(Lang,F,M,Exp),!.
find_morph_from_list(Lang,Exp,[_F|Iller],M) :-
	find_morph_from_list(Lang,Exp,Iller,M).



scan_phonemes(Lang,Lex,Surf) :-
	append("#",Lex,Lex1),
	append(Lex1,"#",Lex2),
	scan_phonemes(Lang,[],[],Lex2,[],Surf).

scan_phonemes(_Lang,[m(_M1,Mo1),m("#"," ")],[],[],Surf,Surf1) :-
	!,
	append(Surf,Mo1,Surf1).

scan_phonemes(Lang,[m(M1,Mo1),m(M2,Mo2),m("#"," ")],[],[],Surf,Surf1) :-
	!,
	append(Surf,Mo1,Surf2),
	check_allo(Lang,[m(M1,Mo1),m(M2,Mo2),m("#"," ")],_,_),
	append(Surf2,Mo2,Surf1).

scan_phonemes(Lang,Stack,[],[35|Lex],Surf,Surf1) :-
	!,
	append(Stack,[m("#"," ")],NewStack),
%	append(Surf," ",NewSurf),
	scan_phonemes(Lang,NewStack,[],Lex,Surf,Surf1).


scan_phonemes(Lang,[m(M1,Mo1),m([],_),m(M3,Mo3)],M3,[35|Lex],Surf,Surf1) :-
	!,
	scan_phonemes(Lang,[m(M1,Mo1),m(M3,Mo3)],[],Lex,Surf,Surf1).
scan_phonemes(Lang,Stack,Morpheme,[35|Lex],Surf,Surf1) :-
	!,
	append(Stack,[m(Morpheme,_Mo3)],NewStack),
	check_allo(Lang,NewStack,NewStack1,Mo1),
	append(Surf,Mo1,NewSurf),
	append(NewStack1,[m("#"," ")],NewStack2),
	check_allo(Lang,NewStack2,NewStack3,Mo2),
	append(NewSurf,Mo2,NewSurf1),
	scan_phonemes(Lang,NewStack3,[],Lex,NewSurf1,Surf1).

scan_phonemes(Lang,Stack,M3,[45|Lex],Surf,Surf1) :-
	!,
	append(Stack,[m(M3,_Morph3)],NewStack),
	check_allo(Lang,NewStack,NewStack1,Mo1),
	append(Surf,Mo1,NewSurf),
	scan_phonemes(Lang,NewStack1,[],Lex,NewSurf,Surf1).

scan_phonemes(Lang,Stack,Morph,[C|Lex],Surf,Surf1) :-
	append(Morph,[C],NewMorph),
	scan_phonemes(Lang,Stack,NewMorph,Lex,Surf,Surf1).


check_allo(_Lang,[A],[A],[]).
check_allo(_Lang,[A,B],[A,B],[]).
check_allo(Lang,[m(M1,Morph1),m(M2,Morph2),m(M3,Morph3)],[m(M2,Morph2),m(NewM3,Morph3)],Morph1) :-
	allo(Lang,Morph2,M2,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions),
	make_succ(SuccSurf,SuccLex,Succ),
	append(_,PredLex,M1),
	append(Succ,_,M3),
	append(_,PredSurf,Morph1),
	append(Residue,NewM3,M3),
	check_cond_list_reverse(Lang,Conditions),
	!.

check_allo(_Lang,[m(_M1,Mo1),m(M2,M2),M3],[m(M2,M2),M3],Mo1).

make_succ(SuccSurf,[],SuccSurf1) :- 
	eat_up_boundary(SuccSurf,SuccSurf1),!.
make_succ(_,SuccLex,SuccLex).

eat_up_boundary([],[]).
eat_up_boundary("#",[]) :- !.
eat_up_boundary([A|B],[A|B1]) :-
	eat_up_boundary(B,B1).
