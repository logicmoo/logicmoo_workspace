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
	loader:m(Lang,Filler,M,Exp),
	append(Lex1,[45],Lex2),
	append(Lex2,M,Lex3),
	!,scan_syntagmeme(Lang,Synt,Lex3,Lex).
scan_syntagmeme(Lang,[?_Slot:Filler/Exp|Synt],Lex1,Lex) :-
	loader:m(Lang,Filler,M,Exp),
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
	loader:m(Lang,F,M,Exp),!.
find_morph_from_list(Lang,Exp,[_F|Iller],M) :-
	find_morph_from_list(Lang,Exp,Iller,M).




/*
eat_up_boundary([],[]).
eat_up_boundary("#","#") :- !.
eat_up_boundary(A,B) :-
	eat_up_boundary_aux(A,B).

eat_up_boundary_aux([],[]).
eat_up_boundary_aux("#",[]) :- !.
eat_up_boundary_aux([A|B],[A|B1]) :-
	eat_up_boundary_aux(B,B1).

*/
