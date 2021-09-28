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

find_fullname(L,_Lang,Name) :-
	available_language(L,Name),
	!.
find_fullname(L:D,_Lang,Name) :-
	name(L,NL),
	name(D,ND),
	append_list(Name,[NL,":",ND]).
	
notify_load(L,Lang) :-
	find_fullname(L,Lang,FullName),
	append(FullName," language module loaded.",String1),
	put_message(String1).

	
notify_save(L:D,BinFile) :-
	name(L,Lang),
	dialect_name(D,Dial),
	name(BinFile,FileName),
	append_list(S,[Lang,"\" language ",Dial, " module saved in file ",FileName]),
	put_message(S).

dialect_name(e,[]) :- !.
dialect_name(D,Name) :-
	name(D,DD),
	append_list(Name,["(",DD,")"]).


notify_set_lang(L,Lang,Type) :-
	find_fullname(L,Lang,FullName),
	append_list(S3,["Current ",Type," set to ",FullName]),
	put_message(S3).

notify_tracing(Module,1) :-
	!,append(Module," tracing is on.",S),
	put_message(S).
notify_tracing(Module,0) :-
	!,append(Module," tracing is off.",S),
	put_message(S).

notify_parse(A) :-
	put_term(A).

notify_generated(S) :-
	put_string(S).

notify_reduce(_Deg,_Dep,_Stack) :-
	trace_synt(0),!.
notify_reduce(Deg,Dep,Stack) :-
	write('REDUCE: '),write(Stack),nl,
	put_dep_rule(Deg,Dep,_).

notify_shift(_Stack) :-
	trace_synt(0),!.
notify_shift(Stack) :-
	write('SHIFT: '),write(Stack),nl,nl.

notify_shift_morph(_Stack) :-
	trace_morph(0),!.
notify_shift_morph(Stack) :-
	write('SHIFT: '),write(Stack),nl,nl.


notify_try_allo(_,_) :-
        trace_morph(0),!.
notify_try_allo(
    allo(Morpheme-Residue => Morph,PredSurf-SuccSurf,l:PredLex-NextSuccLex, 
	                 cond :_Conditions),
    cont(prev:_PreviousSuccLex,ParsedSurf-Rest,l:ParsedLex-[])) :- 
	append_list(S1,["TRYING ALLOMORPH: ",Morpheme,"-",Residue,
		    "  => ",Morph," ",PredSurf,"-",SuccSurf,"  l:",PredLex,"-",NextSuccLex]),
	append_list(S2,[" ** CONTEXT: ",ParsedSurf,"-",Rest,"  l: ",ParsedLex]),
     put_message_var(S1),
     put_message_var(S2).


notify_try_morph(_PF) :-
	trace_morph(0),!.
notify_try_morph(PF) :-
	append_list(S,["Trying morpheme: \"",PF,"\""]),
	put_message_var(S).

notify_discard_morph(_,_) :- trace_morph(0),!.
notify_discard_morph(
    allo(Morpheme-Residue => Morph,PredSurf-SuccSurf,l:PredLex-NextSuccLex, 
	                 cond :Conditions),
    cont(prev:_PreviousSuccLex,ParsedSurf-Rest,l:ParsedLex-[])) :- 
	append_list(S1,["DISCARDED BECAUSE OF ALLOMORPH: ",Morpheme,"-",Residue,
		    " => ",Morph," ",PredSurf,"-",SuccSurf," l:",PredLex,"-",NextSuccLex]),
	append_list(S2,[" ** CONTEXT: ",ParsedSurf,"-",Rest," l: ",ParsedLex," cond: "]),
     put_message_var(S1),put_term(Conditions),
     put_message_var(S2).

notify_gen_pushdown(_Deg,_Dep,_C) :-
	trace_gen(0),!.
notify_gen_pushdown(Deg,Dep,C) :- nl,
	put_message("PUSH: "),
	put_dep_gen_rule(Deg,Dep,C).

notify_gen_sofar(_PF) :-
	trace_gen(0),!.
notify_gen_sofar(PF) :- nl,
	append_list(S,["GENERATED: \"",PF,"\""]),
	put_message(S).

notify_gen_found(_PF) :-
	trace_gen(0),!.
notify_gen_found(PF) :- nl,
	append("FOUND: ",PF,S),
	put_message(S).

notify_check(_) :-
	tracing_mode(0),
	!.
notify_check(Cond) :- nl,
	put_message("TRYING PARADIGM MATCH:"),
	put_term(Cond).

notify_check_ok(_) :-
	tracing_mode(0),
	!.
notify_check_ok(NewParad) :-
	put_term(' -- PARADIGM MATCHES'),
	put_term(NewParad),nl.


append_list(S,List) :-
	append_list(S,[],List).

append_list(S,S,[]).
append_list(S,S2,[S1|List]) :-
	append(S2,S1,S3),
	append_list(S,S3,List).
