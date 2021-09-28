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

:- module(nsmdalia,[
		    load/1,
		    build/1,
		    load_l2/1,
		    load_text_database/1,
		    
		    parse_and_write_word/1,
		    parse_and_write_sentence/1,
		    parse_and_write_sentence/3,
    		    parse_and_write_sentence_tabular/2,
    		    parse_and_write_sentence_tabular/3,		    
		    parse_sentence/4,
		    parse_text/3,
		    parse_text_level/3,
		    
		    
		    gen_sentence/3,
		    generic_gen_sentence/4,
		    gen_and_write_sentence/1,
		    gen_and_write_sentence/2,
		    gen_text/3,
		    gen_and_write_text/2,		    
		    gen_and_write_nsm_text/2,
		    gen_text_level/3,
		    
		    translate_sentence/4,
		    translate_and_write_sentence/1,		    
		    translate_text/4,
		    translate_and_write_text/3,
		    
		    word_list/1,
		    
		    trace_morphology/1,
		    trace_syntax/1,
		    trace_generation/1,
		    
		    list/0
		   ]).

:- include(operators).

:- use_module(utils).
:- use_module(messages). 
:- use_module(checkers).
:- use_module(grammar).
:- use_module(global_par).
:- use_module(dg).
:- use_module(morph_parse).
:- use_module(transcr).
:- use_module(gdoc).
:- use_module(nsm_minimal).

/** <module> Program main module

This module contains the main interface routines for grammar loading and
compiling, and sentence parsing, generation and translation.

The command-line interfaces contains aliases to this module and
to module file_reader.pl
*/

%%	load(+Lang) is det
%
%	Full form of cline_interface:l(+Lang).
%	Argument Lang is a language code or a language full name.
%	The grammar loader is first invoked, then the user is 
%	notified that the module has been loaded, and the global
%	parameter current_lang(+Lang) is set to the language
%	just loaded.
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

load_text_database(FileName) :-
	name(File,FileName),
	exists_file(File),
	!,
	see(File),
	read(S),
	load_file(nsmdalia,S),
	seen,
	notify_load_text_db(FileName).
load_text_database(FileName) :-	
	append("File not found: ",FileName,S),
	put_message(S).


parse_and_write_word(Word) :-
	global_par:current_lang(Lang),
	transcr(Lang,Word,Word1),
	p_morph_lex(Lang,Word1,A),
	notify_parse(A),
	!.
parse_and_write_word(_Word) :-
	notify_parse(noparse).


/*
gen_and_write_word(Word) :-
	global_par:current_lang(Lang),
	tm_morph_gen_word(Lang,_Class,Word,S),
	transcr(Lang,S1,S),
	notify_generated(S1),
	!.
gen_and_write_word(Word) :-
	notify_parse(Word).
*/


parse_and_write_sentence(S) :-
	get_current_lang(Lang),
	nsm_minimal:lf_version(Lang,Version),
	parse_and_write_sentence(Lang,S,Version).
	
parse_and_write_sentence(Lang,S,nsmpl) :-
	get_synt_grammar_type(Lang,Grammar),
	decide_parse_and_write_sent(Lang,Grammar,S,nsmpl,formula),
	!.
parse_and_write_sentence(Lang,S,univ) :-
	get_synt_grammar_type(Lang,Grammar),
	decide_parse_and_write_sent(Lang,Grammar,S,univ,table(minimal)),
	!.
parse_and_write_sentence(_Lang,S,_) :-
	notify_parse(noparse(S)).


parse_and_write_sentence_tabular(S,Full) :-
	get_current_lang(Lang),
	parse_and_write_sentence_tabular(Lang,S,Full).

parse_and_write_sentence_tabular(Lang,S,Full) :-
	get_synt_grammar_type(Lang,Grammar),
	decide_parse_and_write_sent(Lang,Grammar,S,nsmpl,table(Full)),
	!.
parse_and_write_sentence_tabular(_Lang,S,_) :-
	notify_parse(noparse(S)).


decide_parse_and_write_sent(xxx,_,_,_,_) :-
	put_message("No current language is set").
decide_parse_and_write_sent(Lang,dependency,S,nsmpl,formula) :-
	transcr(Lang,S,S1),
	dg_parse(Lang,S1,Parse),
	notify_parse(Parse).
decide_parse_and_write_sent(Lang,dependency,S,nsmpl,table(Full)) :-
	transcr(Lang,S,S1),
	dg_parse(Lang,S1,Parse1),
	nsm2univ(Parse1,Parse),
        pp_univ(Parse,Full).
decide_parse_and_write_sent(Lang,dependency,S,univ,table(Full)) :-
	transcr(Lang,S,S1),
	dg_parse(Lang,S1,Parse1),
	Parse1 = ct(Cat,Parse),
        pp_univ([Cat::Parse],Full).
decide_parse_and_write_sent(Lang,tagmemic,S,_,formula) :-
	skip_whites(S,S3),
	remove_verbatim(S3,S1,V),
	skip_whites(S1,S2),
	transcr(Lang,S2,S5),
	p_chart(Lang,tm_morph,S5,Name:Parse,Rest),
	write('Parse: '),put_string(V),
	write_parse(Name:Parse),nl,
	write('Rest: '),write(Rest),nl.


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
	normalize_spaces(PF,PF1),
	transcr_back(Lang,PF1,PF2),
	append(PF2,".\n",PF3),
	notify_generated(PF3).



parse_sentence(Lang,S5,Parse,Rest) :-
	grammar:synt_grammar_type(Lang,tagmemic),
	p_chart(Lang,tm_morph,S5,Parse,Rest),!.
parse_sentence(Lang,S,Parse,[]) :-
	grammar:synt_grammar_type(Lang,dependency),
	transcr(Lang,S,S1),
	dg_parse(Lang,S1,Parse),!.
parse_sentence(_Lang,S,noparse(S),[]). % FAIL



gen_sentence(Lang,LF,PF) :-
	grammar:synt_grammar_type(Lang,dependency),
	dg_gen(Lang,LF,PF).
gen_sentence(Lang,LF,PF) :-
	grammar:synt_grammar_type(Lang,tagmemic),
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
	global_par:current_lang(L1),
	global_par:current_l2(L2),
	translate_sentence(L1,L2,S1,S2),
	notify_generated(S2).
	
translate_sentence(L1,L1,S,S) :- !.
translate_sentence(L1,L2,S1,S2) :-
	parse_sentence(L1,S1,P,_Rest),
	adjust_lf_version(L1,L2,nsm,P,P1),
	gen_sentence(L2,P1,PF),
	normalize_spaces(PF,PF1),
	transcr_back(L2,PF1,PF2),
	append(PF2,".\n",S2),
	!.
translate_sentence(_L1,_L2,S1,S1). % FAILURE
	

translate_text(L1,L2,[S1|T1],[S2|T2]) :-
	translate_sentence(L1,L2,S1,S2),
	translate_text(L1,L2,T1,T2).
translate_text(_,_,[],[]).

translate_and_write_text(L1,L2,T) :-
	parse_text(L1,T,P),
	gen_and_write_text(L2,P).

/*
tp_parse_text(T) :-
	tp_parse_text(T,P),
	write('Analysis: '),write(P),nl.

tp_gen_text(T) :-
	global_par:current_lang(Lang),
	tp_parse_text(T,P),
	tp_g(Lang,P,PF),
	write(PF).
*/


write_parse(noparse:S) :-
	!,put_string(S).
write_parse(P:S) :-
	write(P:S).

word_list(L) :-
	build_full_name(L,L1),
	make_word_list(L1).

make_word_list(Lang) :-
	global_par:m(Lang,A,B,C),
	write_word(A,B,C), % in UTILS.PL
	fail.
make_word_list(_).

trace_morphology(SWITCH) :-
	retractall(global_par:trace_morph(_)),
	get_switch(SWITCH,S),
	asserta(global_par:trace_morph(S)),
	notify_tracing("Morphology",S),
	check_tracing_mode.
trace_syntax(SWITCH) :-
	retractall(global_par:trace_synt(_)),
	get_switch(SWITCH,S),
	asserta(global_par:trace_synt(S)),
	notify_tracing("Syntax",S),
	check_tracing_mode.
trace_generation(SWITCH) :-
	retractall(global_par:trace_gen(_)),
	get_switch(SWITCH,S),
	asserta(global_par:trace_gen(S)),
	notify_tracing("Syntax",S),
	check_tracing_mode.

check_tracing_mode :-
	global_par:trace_gen(0),
	global_par:trace_synt(0),
%	trace_morph(0),
	!,
	retractall(global_par:tracing_mode(1)),
	asserta(global_par:tracing_mode(0)).

check_tracing_mode :-
	retractall(global_par:tracing_mode(0)),
	asserta(global_par:tracing_mode(1)).	
	
get_switch(1,1) :- !.
get_switch(on,1) :- !.
get_switch(_,0).


parse_text_level(Lang,T,T1) :-
	dg_parse_text_level(Lang,T,T1).
gen_text_level(Lang,T,T1) :-
	dg_gen_text_level(Lang,T,T1).

list :-
	put_message("Installed modules:"),
	write_available_language.

write_available_language :-
        retract(global_par:available_language(L:D,N)),
	!,
	put_string(N),
	write(' ('),write(L),
	write_dial(D),write(')'),nl,
	write_available_language.
write_available_language.
		
write_dial(e) :- !.
write_dial(D) :- write(':'),write(D).



put_label(Label) :-
	put_string(Label).
nsm_indent(Label,Indent) :-
	length(Label,Indent1),
	NewIndent is Indent + Indent1,
	tab(NewIndent).




