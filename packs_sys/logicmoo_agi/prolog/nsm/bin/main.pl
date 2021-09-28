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


:- include('gpl.pl').
:- include('operators.pl').
:- include('dynamic.pl').
:- include('utils.pl').
:- include('loader.pl').
:- include('checkers.pl').
:- include('tm_chart.pl').
:- include('tm_synt_gen').
:- include('morph_parse').
:- include('tm_morph_gen').
:- include('hist_pf.pl').
:- include('formats.pl').
:- include('transcr.pl').
:- include('dg.pl').
:- include('file_reader.pl').
:- include('config.pl').
:- include('messages.pl'). 

load(Lang) :-
	load_lang(Lang,Code),
	!,
	build_full_name(Code,L),
	notify_load(L,Code),	
	set_current_lang(L).
load(Lang) :- fail_load(Lang).

build(Lang) :-
	build_lang(Lang,Code),
	!,
	build_full_name(Code,L),
	notify_load(L,Code),	
	set_current_lang(L).
build(Lang) :- fail_load(Lang).


fail_load(L:D) :-
	name(L,LL),
	name(D,DD),
	append_list(S,["No \"",LL,":",DD,"\" language module found"]),
	put_message(S).
	     
fail_load(Lang) :-
	name(Lang,X),
	append_list(S,["No \"",X,"\" language module found"]),	
	put_message(S).


load_l2(Lang) :-
	load_lang(Lang,Code),
	!,
	build_full_name(Code,L),
	notify_load(L,Code),
	set_current_l2(L).
load_l2(Lang) :- fail_load(Lang).

parse_and_write_word(Word) :-
	current_lang(Lang),
	transcr(Lang,Word,Word1),
	p_morph_lex(Lang,Word1,A),
	notify_parse(A),
	!.
parse_and_write_word(_Word) :-
	notify_parse(noparse).

gen_and_write_word(Word) :-
	current_lang(Lang),
	tm_morph_gen_word(Lang,_Class,Word,S),
	transcr(Lang,S1,S),
	notify_generated(S1),
	!.
gen_and_write_word(Word) :-
	notify_parse(Word).



parse_and_write_sentence(S) :-
	get_current_lang(Lang),
	parse_and_write_sentence(Lang,S).
	
parse_and_write_sentence(Lang,S) :-
	get_synt_grammar_type(Lang,Grammar),
	decide_parse_and_write_sent(Lang,Grammar,S),
	!.
parse_and_write_sentence(_Lang,_S) :-
	notify_parse(noparse).

get_synt_grammar_type(Lang,Grammar) :-
	synt_grammar_type(Lang,Grammar),!.
get_synt_grammar_type(_Lang,dependency).

decide_parse_and_write_sent(xxx,_,_) :-
	put_message("No current language is set").
decide_parse_and_write_sent(Lang,tagmemic,S) :-
	skip_whites(S,S3),
	remove_verbatim(S3,S1,V),
	skip_whites(S1,S2),
	transcr(Lang,S2,S5),
	p_chart(Lang,tm_morph,S5,Name:Parse,Rest),
	write('Parse: '),put_string(V),
	write_parse(Name:Parse),nl,
	write('Rest: '),write(Rest),nl.

decide_parse_and_write_sent(Lang,dependency,S) :-
	transcr(Lang,S,S1),
	dg_parse(Lang,S1,Parse),
	notify_parse(Parse).

generic_gen_sentence(_Lang,_Grammar,noparse(PF),PF) :-	!.
generic_gen_sentence(Lang,dependency,LF,PF) :-	
	dg_gen(Lang,LF,PF1),
	normalize_spaces(PF1,PF2),
	transcr_back(Lang,PF2,PF). % in UTILS.PL
generic_gen_sentence(Lang,tagmemic,LF,PF) :-
	tm_synt_gen_sent(Lang,s:LF,Lex),
	tm_morph_gen_sent(Lang,Lex,[],PF1),
	normalize_spaces(PF1,PF2),
	transcr_back(Lang,PF2,PF).

gen_and_write_sentence(F) :-
	get_current_lang(Lang),
	gen_and_write_sentence(Lang,F).

gen_and_write_sentence(Lang,F) :-
	get_synt_grammar_type(Lang,Grammar),
	decide_gen_and_write_sent(Lang,Grammar,F),
	!.
gen_and_write_sentence(_,F) :- % FAILURE
	notify_parse(F).

decide_gen_and_write_sent(xxx,_,_) :-
	put_message("No current language is set").
decide_gen_and_write_sent(Lang,tagmemic,F) :-
	tm_gen_sentence(Lang,F).
decide_gen_and_write_sent(Lang,dependency,F) :-
	dg_gen(Lang,F,PF),
	transcr_back(Lang,PF,PF1),
	normalize_spaces(PF1,PF2),
	append(PF2,".\n",PF3),
	notify_generated(PF3).

parse_sentence(Lang,S5,Parse,Rest) :-
	synt_grammar_type(Lang,tagmemic),
	p_chart(Lang,tm_morph,S5,Parse,Rest),!.
parse_sentence(Lang,S,Parse,[]) :-
	synt_grammar_type(Lang,dependency),
	dg_parse(Lang,S,Parse),!.
parse_sentence(_Lang,S,noparse(S),[]). % FAIL

gen_sentence(Lang,LF,PF) :-
	synt_grammar_type(Lang,dependency),
	dg_gen(Lang,LF,PF).
gen_sentence(Lang,LF,PF) :-
	synt_grammar_type(Lang,tagmemic),
	tm_morph_gen_sent(Lang,s:LF,PF).

parse_text(_,[],[]).
parse_text(Lang,[S|T],[LF|P]) :-
	parse_sentence(Lang,S,LF,_),
	parse_text(Lang,T,P).

gen_text(_,[],[]).
gen_text(Lang,[LF|P],[PF|T]) :-
	gen_sentence(Lang,LF,PF),
	gen_text(Lang,P,T).

	
gen_and_write_text(_,[]).
gen_and_write_text(Lang,[LF|P]) :-
	gen_and_write_sentence(Lang,LF),
	gen_and_write_text(Lang,P).

gen_and_write_nsm_text(_,[]).
gen_and_write_nsm_text(Lang,[Indent+Label:LF|P]) :-
	!,
	put_label(Label),
	nsm_indent(Label,Indent), % in tm_cline
	gen_and_write_sentence(Lang,LF),
	gen_and_write_text(Lang,P).
gen_and_write_nsm_text(Lang,[LF|P]) :-
	gen_and_write_sentence(Lang,LF),
	gen_and_write_text(Lang,P).

translate_and_write_sentence(S1) :-
	current_lang(L1),
	current_l2(L2),
	translate_sentence(L1,L2,S1,S2),
	notify_generated(S2).
	
translate_sentence(L1,L1,S,S) :- !.
translate_sentence(L1,L2,S1,S2) :-
	parse_sentence(L1,S1,P,_Rest),
	gen_sentence(L2,P,S2),
	!.
translate_sentence(_L1,_L2,S1,S1). % FAILURE
	

translate_text(L1,L2,[S1|T1],[S2|T2]) :-
	translate_sentence(L1,L2,S1,S2),
	translate_text(L1,L2,T1,T2).
translate_text(_,_,[],[]).

translate_and_write_text(L1,L2,T) :-
	parse_text(L1,T,P),
	gen_and_write_text(L2,P).


tp_parse_text(T) :-
	tp_parse_text(T,P),
	write('Analysis: '),write(P),nl.

tp_gen_text(T) :-
	current_lang(Lang),
	tp_parse_text(T,P),
	tp_g(Lang,P,PF),
	write(PF).
	
cg_parse_sentence(Lang,Sent,Parse) :-
	cg_parse(Lang,Sent,Parse).

write_parse(noparse:S) :-
	!,put_string(S).
write_parse(P:S) :-
	write(P:S).



tm_gen_sentence(Lang,F) :-
%	current_lang(Lang),
	tm_synt_gen_sent(Lang,s:F,F1),
	tm_morph_gen_sent(Lang,F1,S),
	transcr(Lang,S1,S),
	write('Sentence: '), write(S1),nl.
	
tm_transl_sentence(L1,L2,S1,S2,R1) :-
	transcr(L1,S1,S1a),
	p_chart(L1,tagm,S1a,_Slot:Parse,R1),
	analyze_semantics(L1,Parse,Parse1),
	synt_semantics(L2,Parse1,Parse2),
	tm_morph_gen_sent(L2,s:Parse2,S2a),
	transcr(L2,S2,S2a).

word_list(L) :-
	build_full_name(L,L1),
	make_word_list(L1).

make_word_list(Lang) :-
	m(Lang,A,B,C),
	write_word(A,B,C), % in UTILS.PL
	fail.
make_word_list(_).

trace_morphology(SWITCH) :-
	retractall(trace_morph(_)),
	get_switch(SWITCH,S),
	asserta(trace_morph(S)),
	notify_tracing("Morphology",S),
	check_tracing_mode.
trace_syntax(SWITCH) :-
	retractall(trace_synt(_)),
	get_switch(SWITCH,S),
	asserta(trace_synt(S)),
	notify_tracing("Syntax",S),
	check_tracing_mode.
trace_generation(SWITCH) :-
	retractall(trace_gen(_)),
	get_switch(SWITCH,S),
	asserta(trace_gen(S)),
	notify_tracing("Syntax",S),
	check_tracing_mode.

check_tracing_mode :-
	trace_gen(0),
	trace_synt(0),
%	trace_morph(0),
	!,
	retractall(tracing_mode(1)),
	asserta(tracing_mode(0)).

check_tracing_mode :-
	retractall(tracing_mode(0)),
	asserta(tracing_mode(1)).	
	
get_switch(1,1) :- !.
get_switch(on,1) :- !.
get_switch(_,0).

parse_semantics(_,T,T).
gen_semantics(_,T,T).


trace_synt(0).
trace_morph(0).


list :-
	put_message("Installed modules:"),
	write_available_language.

write_available_language :-
        retract(available_language(L:D,N)),
	!,
	put_string(N),
	write(' ('),write(L),
	write_dial(D),write(')'),nl,
	write_available_language.
write_available_language.
		
write_dial(e) :- !.
write_dial(D) :- write(':'),write(D).
