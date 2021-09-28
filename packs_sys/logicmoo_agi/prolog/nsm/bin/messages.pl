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

:- module(messages,[notify_load/2,
		    notify_save/2,
		    notify_set_lang/3,
		    notify_tracing/2,
		    notify_parse/1,
		    notify_reduce/3,
		    notify_shift/1,
		    notify_shift_morph/1,
		    notify_generated/1,
		    notify_try_allo/2,
		    notify_try_morph/1,
		    notify_discard_morph/2,
		    notify_gen_pushdown/3,
		    notify_gen_sofar/1,
		    notify_check/1,
		    notify_gen_found/1,
		    notify_check_ok/1,
		    notify_id_format/0,
		    notify_stt/0,
		    notify_load_text_db/1,
		    notify_markup/1,
		    notify_unknown_markup/1,
		    notify_double_format/1,
		    notify_missing/2
		   ]).
:- include('operators.pl').
:- include('dynamic.pl').

:- use_module(utils).


/** <module> Answers to user input.

@tbd A better implementation: export only a generat three-place
procedure message(+Language,+MessageId:term,+Data:list)
*/
find_fullname(L,_Lang,Name) :-
	global_par:available_language(L,Name),
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

notify_parse(noparse(A)) :-
	!,
	put_string("noparse(\""),
	put_string(A),
	put_string("\")."),
	nl.
notify_parse(A) :-
	put_term(A).

notify_generated(S) :-
	put_string(S).

notify_reduce(_Deg,_Dep,_Stack) :-
	global_par:trace_synt(0),!.
notify_reduce(Deg,Dep,Stack) :-
	write('REDUCE: '),write(Stack),nl,
	put_dep_rule(Deg,Dep,_).

notify_shift(_Stack) :-
	global_par:trace_synt(0),!.
notify_shift(Stack) :-
	write('SHIFT: '),write(Stack),nl,nl.

notify_shift_morph(_Stack) :-
	global_par:trace_morph(0),!.
notify_shift_morph(Stack) :-
	write('SHIFT: '),write(Stack),nl,nl.


notify_try_allo(_,_) :-
        global_par:trace_morph(0),!.
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
	global_par:trace_morph(0),!.
notify_try_morph(PF) :-
	append_list(S,["Trying morpheme: \"",PF,"\""]),
	put_message_var(S).

notify_discard_morph(_,_) :- cline_interface:trace_morph(0),!.
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
	global_par:trace_gen(0),!.
notify_gen_pushdown(Deg,Dep,C) :- nl,
	put_message("PUSH: "),
	put_dep_gen_rule(Deg,Dep,C).

notify_gen_sofar(_PF) :-
	global_par:trace_gen(0),!.
notify_gen_sofar(PF) :- nl,
	append_list(S,["GENERATED: \"",PF,"\""]),
	put_message(S).

notify_gen_found(_PF) :-
	global_par:trace_gen(0),!.
notify_gen_found(PF) :- nl,
	append("FOUND: ",PF,S),
	put_message(S).

notify_check(_) :-
	global_par:tracing_mode(0),
	!.
notify_check(Cond) :- nl,
	put_message("TRYING PARADIGM MATCH:"),
	put_term(Cond).

notify_check_ok(_) :-
	global_par:tracing_mode(0),
	!.
notify_check_ok(NewParad) :-
	put_term(' -- PARADIGM MATCHES'),
	put_term(NewParad),nl.

notify_markup(Markup) :-
	name(Markup,Name),
	append("Markup set to ",Name,S),
	put_message(S).

notify_unknown_markup(Markup) :-
	name(Markup,Name),
	append("Unknown markup identifier ",Name,S),
	put_message(S).

notify_double_format(F) :-
	name(F,Name),
	append("Double-text format set to ",Name,S),
	put_message(S).

notify_load_text_db(FileName) :-
	append_list(S,["Text database \"",FileName,"\" compiled."]),
	put_message(S).

notify_stt :-
	put_message("Transcription table set").

notify_id_format :-
	put_message("Text identifier format set").

notify_missing(Missing,In) :-
	append_list(S,[Missing," missing in ",In]),
	put_message(S).
