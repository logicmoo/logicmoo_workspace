
/*  This file is part of NSM-DALIA, an extensible parser and generator
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
:- module(grammar,[
		  save_lang/1,
		  save_lang/2,
		  load_lang/2,
		  build_lang/2
		 ]).
	  
/** <module> Grammar loader

*/
:- include('operators.pl'). %IMPORTANTE PER LEGGERE LE GRAMMATICHE

:- include('buildallo.pl').

:- use_module(hist_pf).
:- use_module(messages).
:- use_module(utils).
:- use_module(checkers).
:- use_module(global_par).



/* DYNAMIC PREDICATES */

% FILE LOADER
:- dynamic(parsing_morphology/0).
:- dynamic(morph_grammar_type/2).
:- dynamic(synt_grammar_type/2).


% DEPENDENCY
:- dynamic(dep/4).
:- dynamic(arc/3).
:- dynamic(morph_seq/2).
:- dynamic(morph_threshold/2).
:- dynamic(dg_class_macro/4).
:- dynamic(dep_threshold/3).
:- dynamic(max_dep_threshold/2).
:- dynamic(actual_max_dep/2).
:- dynamic(non_directional/2).

:- dynamic(macro/1).

:- dynamic(pattern/4).
:- dynamic(allo/9).
:- dynamic(edge/7).
:- dynamic(syntagmeme/5).
:- dynamic(allosyntagma/5).
:- dynamic(morph_synt/5).


% LEXICON
:- dynamic(m/4).
:- dynamic(hm/5).
:- dynamic(paradigm/2).

% PARADIGMS (CONDITIONS)
:- dynamic(subdialect/3).


:- dynamic(phonetic_class/3).


:- dynamic(default_analysis/5).
:- dynamic(ph/11).
:- dynamic(historic_pf/5).
:- dynamic(tp_pf/4).
:- dynamic(cg/3).


:- dynamic(transcr_table/3).
:- dynamic(current_transcr_table/2).
:- dynamic(current_wiki_transcr_table/2).


:- dynamic(idiom/3).


% Formatted grammar
:- dynamic(g_title/3).
:- dynamic(g_version/2).
:- dynamic(g_author/2).
:- dynamic(g_date/2).
:- dynamic(g_ackn/2).

:- dynamic(g_abstract/3).
:- dynamic(g_prologue/3).
:- dynamic(g_transcr/3).
:- dynamic(g_levels/3).
:- dynamic(g_morph/3).
:- dynamic(g_synt/3).
:- dynamic(g_dep_intro/3).
:- dynamic(g_epilogue/3).



% :- dynamic(g_dep/5).

:- dynamic(dep_doc/3).
:- dynamic(dep_analyze/3).
:- dynamic(dep_transl/3).

:- dynamic(ph_doc/3).
:- dynamic(ph_analyze/3).

:- dynamic(dep_print_cond_list/3).
:- dynamic(ph_print_cond_list/3).

:- dynamic(auto_text/3).
% ?
:- dynamic(formatted_formula/4).
:- dynamic(lexical/4).


%%	save_lang(+L) is det
%
%	This is the implementation of the alias cline_interface:s(+Lang).
%	Prompts for a language name, then calls the actual 
%	saving routine, save_lang/2.
save_lang(_) :-
	put_message("Temporarily disabled due to bugs in generated grammars."),
	!.	
save_lang(L) :-
	put_message("This language will be added to configuration file"),
	put_message("Input language name (between double quotas, end with fullstop"),
	read(NAME),
	save_lang(L,NAME).

%%	save_lang(+LanguageCode,+LanguageFullName) is det
%
%	This routine saves all the compiled version of the
%	grammar clauses (the listing_pred/1 calls), then
%	updates the configuration file config.pl with the new
%	language in the available-language list.
save_lang(_,_) :-
	put_message("Temporarily disabled due to bugs in generated grammars."),
	!.
save_lang(L,NAME) :-
	build_full_name(L,Lang),
	build_bin_file_name(Lang,BinFile),
	tell(BinFile),
	listing_pred(morph_grammar_type(Lang,_A)),
	listing_pred(synt_grammar_type(Lang,_B)),
	listing_pred(max_dep_threshold(Lang,_C)),
	listing_pred(morph_threshold(Lang,_D)),
	listing_pred(dep_threshold(Lang,_E,_F)),
	listing_pred(actual_max_dep(Lang,_Degb)),
	listing_pred(current_transcr_table(Lang,_Num)),
	listing_pred(transcr_table(Lang,_Num1,_G)),	
	listing_pred(phonetic_class(Lang,_H,_I)),
	listing_pred(lexical(Lang,_J,_K,_L)),
	listing_pred(arc(Lang,_M,_N)),
	listing_pred(dep(Lang,_Deg,_Cat1+_Cat2+_Cat4 ==> _Cat3,_Cond)),
	listing_pred(dep(Lang,_Dega,_Cat1a+_Cat2a ==> _Cat3a,_Conda)),
	listing_pred(dg_class_macro(Lang,_R,_S,_T)),
	listing_pred(paradigm(Lang,_Paradigm)),
	listing_pred(morph_synt(Lang,_U,_V,_W,_X)),
	listing_pred(syntagmeme(Lang,_Y,_Z,_AA,_BB)),
	listing_pred(pattern(Lang,_CC,_DD,_EE)),
	listing_pred(tparadigm(Lang,_FF)),
	listing_pred(default_analysis(Lang,_GG,_HH,_II,_JJ)),
	listing_pred(cg(Lang,_KK,_LL)),
	listing_pred(allo(Lang,_MM,_NN,_OO,_PP,_QQ,_RR,_SS,_TT)),
	listing_pred(m(Lang,_UU,_VV,_WW)),
	told,
	notify_save(Lang,BinFile),	
	append('./bin/config.pl'),
	put_string("available_language("),
	write(Lang),write(', "'),
	put_string(NAME),write('").'),nl,
	told,
	assert_new_available_lang(Lang,NAME),
	put_message("New language added to configuration file.").

listing_pred(Pred) :-
	Pred,
	write(Pred),
	put_string(".\n"),
	fail.
listing_pred(_).


find_lang_code(L,Code) :-
	is_list(L),
	global_par:available_language(Code,L),
	!.
find_lang_code(L,xxx) :-
	is_list(L),
	!,
	append("Unknown language: ",L,S),
	put_message(S).
find_lang_code(C,C).


%%	load_lang(+Lang,-Lang1) is det
%
%	This procedure checks whether Lang is a string (full language 
%	name) or an atom (language code), and instantiates
%	Lang1 with the code. Then it calls load_or_compile_lang/2;
%	which loads a precompiled language file, if present, for
%	the selected language; otherwise compiles one.
%	
load_lang(Lang,Lang1) :- 
	find_lang_code(Lang,Lang1),
	build_full_name(Lang1,L),
	build_bin_file_name(L,BinFile),
	load_or_compile_lang(L,BinFile).


%%	build_lang(+Lang,-Lang1) is det
%
%	This procedure checks whether Lang is a string (full language 
%	name) or an atom (language code), and instantiates
%	Lang1 with the code. Then it compiles a gramamr file.
%	Used when a binary file is present, but the user wants to
%	load the source grammar instead.
%		
build_lang(Lang,Lang1) :-
	find_lang_code(Lang,Lang1),	
	build_full_name(Lang1,L),	
	read_grammar_file(L),
	build_allo(L).



build_bin_file_name(L:D,BinFile) :-
	name(L,F),
	name(D,F1),
	global_par:lang_bin_dir(DIR),
	append_list(BinFileName,[DIR,F,"-",F1,"_bin.pl"]),
	name(BinFile,BinFileName).

load_or_compile_lang(_L,BinFile) :-
	exists_file(BinFile),
	!,
	load_lang_module(BinFile).
load_or_compile_lang(L,_L1) :-
	read_grammar_file(L),
	build_allo(L).

load_lang_module(BinFile) :-
	see(BinFile),
	read_term(S,[module(grammar)]),
	load_file(grammar,S),
	seen.

read_grammar_file(Lang) :-
	get_dial(Lang,Lang1,Dial),
	name(Lang1,XX),
	global_par:lang_src_dir(DIR),
	append(DIR,XX,X),
	append(X,"_hist.pl",F1),
	name(File1,F1),
	read_if_exists(File1,Lang1,Dial),
	append(X,"_nsmg.pl",Y),
	name(File,Y),
	exists_file(File),
	!,
	see(File),
	read_grammar(Lang1,Dial),
	seen,
	append(X,"_gr.pl",YY),
	name(ExtGr,YY),
	read_if_exists(ExtGr,Lang1,Dial),
	append(X,"_pf.pl",Z),
	name(PhonFile,Z),
	read_if_exists(PhonFile,Lang1,Dial),
	append(X,"_lx.pl",Z1),
	name(LexFile,Z1),
	read_if_exists(LexFile,Lang1,Dial),
	append(X,"_rf.pl",Z2),
	name(FormatRuleFile,Z2),
	read_if_exists(FormatRuleFile,Lang1,Dial),
%	append(X,"_tp.pl",K),
%	name(TokiPona,K),
%	read_if_exists(TokiPona,Lang1,Dial),
	pass_through_hist(Lang),
	retractall(parsing_morphology),
	order_dep(Lang),
	compile_morph_seq(Lang),
	!.

read_if_exists(File,Lang,Dial) :-
	exists_file(File),
	!,
	see(File),
	read_grammar(Lang,Dial),
	seen.
read_if_exists(_,_,_).


read_grammar(Lang,Dial) :-
	read_term(Rule,[module(grammar)]),
	load_rule(Rule,Lang,Dial).
	
get_dial(Lang:Dial,Lang,Dial) :- !.
get_dial(Lang,Lang,e).



load_rule(end_of_file,_,_) :- true.

load_rule(Dial :: m :: Name === A <> B // C,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(morph_synt(Lang:Dial1,Name,A,B,C)),
	read_grammar(Lang,Dial1).
load_rule(m :: Name === A <> B // C,Lang,Dial) :-
	!,
	assertz(morph_synt(Lang:Dial,Name,A,B,C)),
	read_grammar(Lang,Dial).
load_rule(Dial :: m :: Name === A <> B ,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(morph_synt(Lang:Dial1,Name,A,B,[])),
	read_grammar(Lang,Dial1).
load_rule(m :: Name === A <> B ,Lang,Dial) :-
	!,
	assertz(morph_synt(Lang:Dial,Name,A,B,[])),
	read_grammar(Lang,Dial).

load_rule(Dial :: m :: Name === P1 ,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	build_syntagmeme(Lang,P1,synt(Name,P,A,C)),
	assertz(morph_synt(Lang:Dial1,Name,P,A,C)),
	read_grammar(Lang,Dial1).
load_rule(m :: Name === P1 ,Lang,Dial) :-
	!,
	build_syntagmeme(Lang,P1,synt(Name,P,A,C)),
	assertz(morph_synt(Lang:Dial,Name,P,A,C)),
	read_grammar(Lang,Dial).




load_rule(Dial :: Name === A <> B // C,Lang,Dial1) :-
	parsing_morphology,
	is_a(Lang,Dial1,Dial),
	!,
	assertz(morph_synt(Lang:Dial1,Name,A,B,C)),
	read_grammar(Lang,Dial1).
load_rule(Name === A <> B // C,Lang,Dial) :-
	parsing_morphology,
	!,
	assertz(morph_synt(Lang:Dial,Name,A,B,C)),
	read_grammar(Lang,Dial).
load_rule(Dial :: Name === A <> B ,Lang,Dial1) :-
	parsing_morphology,
	is_a(Lang,Dial1,Dial),
	!,
	assertz(morph_synt(Lang:Dial1,Name,A,B,[])),
	read_grammar(Lang,Dial1).
load_rule(Name === A <> B ,Lang,Dial) :-
	parsing_morphology,
	!,
	assertz(morph_synt(Lang:Dial,Name,A,B,[])),
	read_grammar(Lang,Dial).

load_rule(Dial :: Name === P1 ,Lang,Dial1) :-
	parsing_morphology,
	is_a(Lang,Dial1,Dial),
	!,
	build_syntagmeme(Lang,P1,synt(Name,P,A,C)),
	assertz(morph_synt(Lang:Dial1,Name,P,A,C)),
	read_grammar(Lang,Dial1).
load_rule(Name === P1 ,Lang,Dial) :-
	parsing_morphology,
	!,
	build_syntagmeme(Lang,P1,synt(Name,P,A,C)),
	assertz(morph_synt(Lang:Dial,Name,P,A,C)),
	read_grammar(Lang,Dial).




load_rule(Dial :: Name === A <> B // C,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(syntagmeme(Lang:Dial1,Name,A,B,C)),
	read_grammar(Lang,Dial1).
load_rule(Name === A <> B // C,Lang,Dial) :-
	!,
	assertz(syntagmeme(Lang:Dial,Name,A,B,C)),
	read_grammar(Lang,Dial).
load_rule(Dial :: Name === A <> B ,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(syntagmeme(Lang:Dial1,Name,A,B,[])),
	read_grammar(Lang,Dial1).
load_rule(Name === A <> B ,Lang,Dial) :-
	!,
	assertz(syntagmeme(Lang:Dial,Name,A,B,[])),
	read_grammar(Lang,Dial).

load_rule(Dial :: Name === P1 ,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	build_syntagmeme(Lang,P1,synt(Name,P,A,C)),
	assertz(syntagmeme(Lang:Dial1,Name,P,A,C)),
	read_grammar(Lang,Dial1).
load_rule(Name === P1 ,Lang,Dial) :-
	!,
	build_syntagmeme(Lang,P1,synt(Name,P,A,C)),
	assertz(syntagmeme(Lang:Dial,Name,P,A,C)),
	read_grammar(Lang,Dial).





load_rule(Dial :: pattern(A,B,C),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(pattern(Lang:Dial1,A,B,C)),
	read_grammar(Lang,Dial1).
load_rule(pattern(A,B,C),Lang,Dial) :-
	!,
	assertz(pattern(Lang:Dial,A,B,C)),
	read_grammar(Lang,Dial).

load_rule(Dial :: p ::: Paradigm, Lang, Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(paradigm(Lang:Dial1,Paradigm)),
	read_grammar(Lang,Dial1).
load_rule(p ::: Paradigm, Lang, Dial) :-
	!,
	assertz(paradigm(Lang:Dial,Paradigm)),
	read_grammar(Lang,Dial).

% TOKI PONA PARADIGMS

load_rule(Dial :: tp ::: Paradigm, Lang, Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(tparadigm(Lang:Dial1,Paradigm)),
	read_grammar(Lang,Dial1).
load_rule(tp ::: Paradigm, Lang, Dial) :-
	!,
	assertz(tparadigm(Lang:Dial,Paradigm)),
	read_grammar(Lang,Dial).



load_rule(Dial :: Num / m(A,B,C),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(hm(Lang:Dial1,Num,A,B,C)),
	read_grammar(Lang,Dial1).
load_rule(Num/m(A,B,C),Lang,Dial) :-
	!,
	assertz(hm(Lang:Dial,Num,A,B,C)),
	read_grammar(Lang,Dial).
load_rule(Dial :: m(A,B,C),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(m(Lang:Dial1,A,B,C)),
	read_grammar(Lang,Dial1).
load_rule(m(A,B,C),Lang,Dial) :-
	!,
	assertz(m(Lang:Dial,A,B,C)),
	read_grammar(Lang,Dial).

load_rule(Dial :: allo(A,B,C,D,E,F,G,H),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(allo(Lang:Dial1,A,B,C,D,E,F,G,H)),
	read_grammar(Lang,Dial1).
load_rule(allo(A,B,C,D,E,F,G,H),Lang,Dial) :-
	!,
	assertz(allo(Lang:Dial,A,B,C,D,E,F,G,H)),
	read_grammar(Lang,Dial).

load_rule(Dial :: X => Y : B // C,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(default_analysis(Lang:Dial1,X,Y,C,B)),
	read_grammar(Lang,Dial1).
load_rule(X => Y :B // C,Lang,Dial) :-
	!,
	assertz(default_analysis(Lang:Dial,X,Y,C,B)),
	read_grammar(Lang,Dial).

load_rule(Dial :: X => Y // C,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(default_analysis(Lang:Dial1,X,Y,C,[])),
	read_grammar(Lang,Dial1).
load_rule(X => Y // C,Lang,Dial) :-
	!,
	assertz(default_analysis(Lang:Dial,X,Y,C,[])),
	read_grammar(Lang,Dial).


load_rule(Dial :: A => B,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(default_analysis(Lang:Dial1,A,B,[],[])),
	read_grammar(Lang,Dial1).
load_rule(A => B,Lang,Dial) :-
	!,
	assertz(default_analysis(Lang:Dial,A,B,[],[])),
	read_grammar(Lang,Dial).

	
load_rule(subdialect(A,B),Lang,Dial) :-
	!,
	assertz(subdialect(Lang,A,B)),
	read_grammar(Lang,Dial).


load_rule(Dial :: ph(Name,A,B,C,D,E,F,G,H,I),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(ph(Lang:Dial1,Name,A,B,C,D,E,F,G,H,I)),
	read_grammar(Lang,Dial1).
load_rule(ph(Name,A,B,C,D,E,F,G,H,I),Lang,Dial) :-
	!,
	assertz(ph(Lang:Dial,Name,A,B,C,D,E,F,G,H,I)),
	read_grammar(Lang,Dial).

load_rule(Dial :: ph ::: Rule,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	ph_expand(Rule,ph(A,B,C,D,E,F,G,H,I)),
	assertz(ph(Lang:Dial1,[],A,B,C,D,E,F,G,H,I)),
	read_grammar(Lang,Dial1).
load_rule(ph ::: Rule,Lang,Dial) :-
	!,
	ph_expand(Rule,ph(A,B,C,D,E,F,G,H,I)),
	assertz(ph(Lang:Dial,[],A,B,C,D,E,F,G,H,I)),
	read_grammar(Lang,Dial).

load_rule(Dial :: ph(Name) ::: Rule,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	ph_expand(Rule,ph(A,B,C,D,E,F,G,H,I)),
	assertz(ph(Lang:Dial1,Name,A,B,C,D,E,F,G,H,I)),
	read_grammar(Lang,Dial1).
load_rule(ph(Name) ::: Rule,Lang,Dial) :-
	!,
	ph_expand(Rule,ph(A,B,C,D,E,F,G,H,I)),
	assertz(ph(Lang:Dial,Name,A,B,C,D,E,F,G,H,I)),
	read_grammar(Lang,Dial).




load_rule(Dial :: Num ::: wfr m(Cat,A,LF) ---> m(Cat1,B,LF1) // C,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(historic_pf(Lang:Dial1,info(wf,Num),m(Cat,A,LF),m(Cat1,B,LF1),C)),
	read_grammar(Lang,Dial1).
load_rule(Dial :: Num ::: wfr m(Cat,A,LF) ---> m(Cat1,B,LF1),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(historic_pf(Lang:Dial1,info(wf,Num),m(Cat,A,LF),m(Cat1,B,LF1),[])),
	read_grammar(Lang,Dial1).
load_rule(Num ::: wfr m(Cat,A,LF) ---> m(Cat1,B,LF1) // C,Lang,Dial) :-
	!,
	assertz(historic_pf(Lang:Dial,info(wf,Num),m(Cat,A,LF),m(Cat1,B,LF1),C)),
	read_grammar(Lang,Dial).
load_rule(Num ::: wfr m(Cat,A,LF) ---> m(Cat1,B,LF1),Lang,Dial) :-
	!,
	assertz(historic_pf(Lang:Dial,info(wf,Num),m(Cat,A,LF),m(Cat1,B,LF1),[])),
	read_grammar(Lang,Dial).



load_rule(Dial :: Num ::: wfr A ---> B // C,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(historic_pf(Lang:Dial1,info(wf,Num),m(Cat,A,LF),m(Cat,B,LF),C)),
	read_grammar(Lang,Dial1).
load_rule(Dial :: Num ::: wfr A ---> B,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(historic_pf(Lang:Dial1,info(wf,Num),m(Cat,A,LF),m(Cat,B,LF),[])),
	read_grammar(Lang,Dial1).
load_rule(Num ::: wfr A ---> B // C,Lang,Dial) :-
	!,
	assertz(historic_pf(Lang:Dial,info(wf,Num),m(Cat,A,LF),m(Cat,B,LF),C)),
	read_grammar(Lang,Dial).
load_rule(Num ::: wfr  A ---> B,Lang,Dial) :-
	!,
	assertz(historic_pf(Lang:Dial,info(wf,Num),m(Cat,A,LF),m(Cat,B,LF),[])),
	read_grammar(Lang,Dial).

load_rule(Dial :: wfr pf(Num,A,B,C),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(historic_pf(Lang:Dial1,info(wf,Num),A,B,C)),
	read_grammar(Lang,Dial1).
load_rule(wfr pf(Num,A,B,C),Lang,Dial) :-
	!,
	assertz(historic_pf(Lang:Dial,info(wf,Num),A,B,C)),
	read_grammar(Lang,Dial).





load_rule(Dial :: Num ::: m(Cat,A,LF) ---> m(Cat1,B,LF1) // C,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(historic_pf(Lang:Dial1,info(nowf,Num),m(Cat,A,LF),m(Cat1,B,LF1),C)),
	read_grammar(Lang,Dial1).
load_rule(Dial :: Num ::: m(Cat,A,LF) ---> m(Cat1,B,LF1),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(historic_pf(Lang:Dial1,info(nowf,Num),m(Cat,A,LF),m(Cat1,B,LF1),[])),
	read_grammar(Lang,Dial1).
load_rule(Num ::: m(Cat,A,LF) ---> m(Cat1,B,LF1) // C,Lang,Dial) :-
	!,
	assertz(historic_pf(Lang:Dial,info(nowf,Num),m(Cat,A,LF),m(Cat1,B,LF1),C)),
	read_grammar(Lang,Dial).
load_rule(Num ::: m(Cat,A,LF) ---> m(Cat1,B,LF1),Lang,Dial) :-
	!,
	assertz(historic_pf(Lang:Dial,info(nowf,Num),m(Cat,A,LF),m(Cat1,B,LF1),[])),
	read_grammar(Lang,Dial).



load_rule(Dial :: Num ::: A ---> B // C,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(historic_pf(Lang:Dial1,info(nowf,Num),m(Cat,A,LF),m(Cat,B,LF),C)),
	read_grammar(Lang,Dial1).
load_rule(Dial :: Num ::: A ---> B,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(historic_pf(Lang:Dial1,info(nowf,Num),m(Cat,A,LF),m(Cat,B,LF),[])),
	read_grammar(Lang,Dial1).
load_rule(Num ::: A ---> B // C,Lang,Dial) :-
	!,
	assertz(historic_pf(Lang:Dial,info(nowf,Num),m(Cat,A,LF),m(Cat,B,LF),C)),
	read_grammar(Lang,Dial).
load_rule(Num ::: A ---> B,Lang,Dial) :-
	!,
	assertz(historic_pf(Lang:Dial,info(nowf,Num),m(Cat,A,LF),m(Cat,B,LF),[])),
	read_grammar(Lang,Dial).


load_rule(Dial :: pf(Num,A,B,C),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(historic_pf(Lang:Dial1,info(nowf,Num),A,B,C)),
	read_grammar(Lang,Dial1).
load_rule(pf(Num,A,B,C),Lang,Dial) :-
	!,
	assertz(historic_pf(Lang:Dial,info(nowf,Num),A,B,C)),
	read_grammar(Lang,Dial).





load_rule(Dial :: Class << Members,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(phonetic_class(Lang:Dial1,Class,Members)),
	read_grammar(Lang,Dial1).
load_rule(Class << Members,Lang,Dial) :-
	!,
	assertz(phonetic_class(Lang:Dial,Class,Members)),
	read_grammar(Lang,Dial).




load_rule(Dial :: Cat cat CG,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(cg(Lang:Dial1,Cat,CG)),
	read_grammar(Lang,Dial1).
load_rule(Cat cat CG,Lang,Dial) :-
	!,
	assertz(cg(Lang:Dial,Cat,CG)),
	read_grammar(Lang,Dial).

load_rule(Dial :: Cat isa Cat1,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	cg(Lang:Dial,Cat1,CG),
	!,
	assertz(cg(Lang:Dial1,Cat,CG)),
	read_grammar(Lang,Dial1).
load_rule(Cat isa Cat1,Lang,Dial) :-
	cg(Lang:Dial,Cat1,CG),
	!,
	assertz(cg(Lang:Dial,Cat,CG)),
	read_grammar(Lang,Dial).



/* DEPENDENCY GRAMMAR */



load_rule(Dial2 :: Num dar Rule,Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	nsm_minimal:build_dr(Lang:Dial,Num,Rule,DR),	
	assert_dep(DR),	
	read_grammar(Lang,Dial).
load_rule(Num dar Rule,Lang,Dial) :-
	!,
	nsm_minimal:build_dr(Lang:Dial,Num,Rule,DR),
	assert_dep(DR),		
	read_grammar(Lang,Dial).



load_rule(Dial2 :: morph_grammar_type(Type),Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	asserta(morph_grammar_type(Lang:Dial,Type)),
	read_grammar(Lang,Dial).
load_rule(morph_grammar_type(Type),Lang,Dial) :-
	!,
	asserta(morph_grammar_type(Lang:Dial,Type)),
	read_grammar(Lang,Dial).

load_rule(Dial2 :: synt_grammar_type(Type),Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	asserta(synt_grammar_type(Lang:Dial,Type)),
	read_grammar(Lang,Dial).
load_rule(synt_grammar_type(Type),Lang,Dial) :-
	!,
	asserta(synt_grammar_type(Lang:Dial,Type)),
	read_grammar(Lang,Dial).



load_rule(Dial2 :: morph_threshold(Deg),Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	asserta(morph_threshold(Lang:Dial,Deg)),
	read_grammar(Lang,Dial).
load_rule(morph_threshold(Deg),Lang,Dial) :-
	!,
	asserta(morph_threshold(Lang:Dial,Deg)),
	read_grammar(Lang,Dial).


load_rule(Dial2 :: dep_threshold(Level,Deg),Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	asserta(dep_threshold(Lang:Dial,Level,Deg)),
	read_grammar(Lang,Dial).
load_rule(dep_threshold(Level,Deg),Lang,Dial) :-
	!,
	asserta(dep_threshold(Lang:Dial,Level,Deg)),
	read_grammar(Lang,Dial).


load_rule(Dial2 :: max_dep_threshold(Level),Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	asserta(max_dep_threshold(Lang:Dial,Level)),
	read_grammar(Lang,Dial).
load_rule(max_dep_threshold(Level),Lang,Dial) :-
	!,
	asserta(max_dep_threshold(Lang:Dial,Level)),
	read_grammar(Lang,Dial).



load_rule(Dial2 :: lexical(Cat,Meaning,LF),Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	asserta(lexical(Lang:Dial,Cat,Meaning,LF)),
	read_grammar(Lang,Dial).
load_rule(lexical(Cat,Meaning,LF),Lang,Dial) :-
	!,
	asserta(lexical(Lang:Dial,Cat,Meaning,LF)),
	read_grammar(Lang,Dial).

load_rule(Dial2 :: arc(From,To),Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	asserta(arc(Lang:Dial,From,To)),
	read_grammar(Lang,Dial).
load_rule(arc(From,To),Lang,Dial) :-
	!,
	asserta(arc(Lang:Dial,From,To)),
	read_grammar(Lang,Dial).

load_rule(Dial2 :: ms ::: Formula,Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	asserta(morph_seq(Lang:Dial,Formula)),
	read_grammar(Lang,Dial).
load_rule(ms ::: Formula,Lang,Dial) :-
	!,
	asserta(morph_seq(Lang:Dial,Formula)),
	read_grammar(Lang,Dial).

% ms e wf sono la stessa cosa
load_rule(Dial2 :: wf ::: Formula,Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	asserta(morph_seq(Lang:Dial,Formula)),
	read_grammar(Lang,Dial).
load_rule(wf ::: Formula,Lang,Dial) :-
	!,
	asserta(morph_seq(Lang:Dial,Formula)),
	read_grammar(Lang,Dial).




load_rule(Dial :: Deg dr Cat1 + Cat2 + Cat4 ==> Cat3 // Cond,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(dep(Lang:Dial1,Deg,Cat1+Cat2+Cat4 ==> Cat3,Cond)),
	read_grammar(Lang,Dial1).
load_rule(Dial2 :: Deg dr Cat1 + Cat2 + Cat4 ==> Cat3,Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2+Cat4 ==> Cat3,[])),
	read_grammar(Lang,Dial).

load_rule(Deg dr Cat1 + Cat2 + Cat4 ==> Cat3 // Cond,Lang,Dial) :-
	!,
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2+Cat4 ==> Cat3,Cond)),
	read_grammar(Lang,Dial).

load_rule(Deg dr Cat1 + Cat2 + Cat4 ==> Cat3,Lang,Dial) :-
	!,
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2+Cat4 ==> Cat3,[])),
	read_grammar(Lang,Dial).

load_rule(Dial :: Deg dr Cat1 + Cat2 ==> Cat3 // Cond,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(dep(Lang:Dial1,Deg,Cat1+Cat2 ==> Cat3,Cond)),
	read_grammar(Lang,Dial1).
load_rule(Dial2 :: Deg dr Cat1 - Cat2 ==> Cat3 // Cond,Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	assertz(dep(Lang:Dial,Deg,Cat2+Cat1 ==> Cat3,Cond)),	
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,Cond)),
	assertz(non_directional(Lang:Dial,Deg)),
	read_grammar(Lang,Dial).
load_rule(Dial2 :: Deg dr Cat1 + Cat2 ==> Cat3,Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,[])),
	read_grammar(Lang,Dial).
load_rule(Dial2 :: Deg dr Cat1 - Cat2 ==> Cat3,Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	assertz(dep(Lang:Dial,Deg,Cat2+Cat1 ==> Cat3,[])),	
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,[])),
	assertz(non_directional(Lang:Dial,Deg)),
	read_grammar(Lang,Dial).


load_rule(Deg dr Cat1 + Cat2 ==> Cat3 // Cond,Lang,Dial) :-
	!,
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,Cond)),
	read_grammar(Lang,Dial).
load_rule(Deg dr Cat1 - Cat2 ==> Cat3 // Cond,Lang,Dial) :-
	!,
	assertz(dep(Lang:Dial,Deg,Cat2+Cat1 ==> Cat3,Cond)),
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,Cond)),
	assertz(non_directional(Lang:Dial,Deg)),
	read_grammar(Lang,Dial).

load_rule(Deg dr Cat1 + Cat2 ==> Cat3,Lang,Dial) :-
	!,
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,[])),
	read_grammar(Lang,Dial).
load_rule(Deg dr Cat1 - Cat2 ==> Cat3,Lang,Dial) :-
	!,
	assertz(dep(Lang:Dial,Deg,Cat2+Cat1 ==> Cat3,[])),
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,[])),	
	assertz(non_directional(Lang:Dial,Deg)),
	read_grammar(Lang,Dial).



/*
load_rule(Dial :: Deg dr Cat1 ==> Cat2 + Cat3 // Cond,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(dep(Lang:Dial1,Deg,Cat1 ==> Cat2 + Cat3,Cond)),
	read_grammar(Lang,Dial1).
load_rule(Dial2 :: Deg dr Cat1 ==> Cat2 - Cat3 // Cond,Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	assertz(dep(Lang:Dial,Deg,Cat1 ==> Cat2+Cat3,Cond)),	
	assertz(dep(Lang:Dial,Deg,Cat1 ==> Cat3+Cat2,Cond)),
	read_grammar(Lang,Dial).
load_rule(Dial2 :: Deg dr Cat1 + Cat2 ==> Cat3,Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,[])),
	read_grammar(Lang,Dial).
load_rule(Dial2 :: Deg dr Cat1 - Cat2 ==> Cat3,Lang,Dial) :-
	is_a(Lang,Dial,Dial2),
	!,
	assertz(dep(Lang:Dial,Deg,Cat2+Cat1 ==> Cat3,[])),	
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,[])),
	read_grammar(Lang,Dial).


load_rule(Deg dr Cat1 + Cat2 ==> Cat3 // Cond,Lang,Dial) :-
	!,
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,Cond)),
	read_grammar(Lang,Dial).
load_rule(Deg dr Cat1 - Cat2 ==> Cat3 // Cond,Lang,Dial) :-
	!,
	assertz(dep(Lang:Dial,Deg,Cat2+Cat1 ==> Cat3,Cond)),
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,Cond)),
	read_grammar(Lang,Dial).

load_rule(Deg dr Cat1 + Cat2 ==> Cat3,Lang,Dial) :-
	!,
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,[])),
	read_grammar(Lang,Dial).
load_rule(Deg dr Cat1 - Cat2 ==> Cat3,Lang,Dial) :-
	!,
	assertz(dep(Lang:Dial,Deg,Cat2+Cat1 ==> Cat3,[])),
	assertz(dep(Lang:Dial,Deg,Cat1+Cat2 ==> Cat3,[])),	
	read_grammar(Lang,Dial).

*/


load_rule(Dial :: ct(Class,Exp) ::: univ(UCLASS)-LF,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	nsm_minimal:univ(UCLASS,nsm,F),
	nsm_minimal:inst_dependants_aux(LF,F,LF1,no),
	assertz(dg_class_macro(Lang:Dial1,Class,Exp,LF1)),
	read_grammar(Lang,Dial1).
load_rule(ct(Class,Exp) ::: univ(UCLASS)-LF,Lang,Dial1) :-
	!,
	nsm_minimal:univ(UCLASS,nsm,F),
	nsm_minimal:inst_dependants_aux(LF,F,LF1,no),
	assertz(dg_class_macro(Lang:Dial1,Class,Exp,LF1)),
	read_grammar(Lang,Dial1).



load_rule(Dial :: ct(Class,Exp) ::: LF,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(dg_class_macro(Lang:Dial1,Class,Exp,LF)),
	read_grammar(Lang,Dial1).
load_rule(ct(Class,Exp) ::: LF,Lang,Dial1) :-
	!,
	assertz(dg_class_macro(Lang:Dial1,Class,Exp,LF)),
	read_grammar(Lang,Dial1).


load_rule(Dial :: ff(A,B,C),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	asserta(rule_formatter:formatted_formula(Lang:Dial1,A,B,C)),
	read_grammar(Lang,Dial1).
load_rule(ff(A,B,C),Lang,Dial1) :-
	!,
	asserta(rule_formatter:formatted_formula(Lang:Dial1,A,B,C)),
	read_grammar(Lang,Dial1).


load_rule(Dial :: idiom(A,B),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	asserta(idiom(Lang:Dial1,A,B)),
	read_grammar(Lang,Dial1).
load_rule(idiom(A,B),Lang,Dial1) :-
	!,
	asserta(idiom(Lang:Dial1,A,B)),
	read_grammar(Lang,Dial1).



/* TOKI PONA */

load_rule(Dial :: A >> B // C,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(tpconstr(Lang:Dial1,A,B,C)),
	read_grammar(Lang,Dial1).
load_rule(Dial :: A >> B,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(tpconstr(Lang:Dial1,A,B,[])),
	read_grammar(Lang,Dial1).

load_rule(A >> B // C,Lang,Dial) :-
	!,
	assertz(tpconstr(Lang:Dial,A,B,C)),
	read_grammar(Lang,Dial).
load_rule(A >> B,Lang,Dial) :-
	!,
	assertz(tpconstr(Lang:Dial,A,B,[])),
	read_grammar(Lang,Dial).

load_rule(Dial :: tp : Paradigm, Lang, Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(tparadigm(Lang:Dial1,Paradigm)),
	read_grammar(Lang,Dial1).
load_rule(tp : Paradigm, Lang, Dial) :-
	!,
	assertz(tparadigm(Lang:Dial,Paradigm)),
	read_grammar(Lang,Dial).


load_rule(Dial :: A =>> B // C,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(tp_pf(Lang:Dial1,A,B,C)),
	read_grammar(Lang,Dial1).
load_rule(A =>> B // C,Lang,Dial) :-
	!,
	assertz(tp_pf(Lang:Dial,A,B,C)),
	read_grammar(Lang,Dial).
load_rule(Dial :: A =>> B,Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(tp_pf(Lang:Dial1,A,B,[])),
	read_grammar(Lang,Dial1).
load_rule(A =>> B,Lang,Dial) :-
	!,
	assertz(tp_pf(Lang:Dial,A,B,[])),
	read_grammar(Lang,Dial).

/*
load_rule(Dial :: tpattern(A,B,C,D,E),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(Lang:Dial1 'TPF' pattern(A,B,C,D,E)),
	read_grammar(Lang,Dial1).
load_rule(tpattern(A,B,C,D,E),Lang,Dial) :-
	!,
	assertz(Lang:Dial 'TPF' pattern(A,B,C,D,E)),
	read_grammar(Lang,Dial).
*/


load_rule(Dial :: transcr_table(Num,T),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(transcr_table(Lang:Dial1,Num,T)),
	read_grammar(Lang,Dial1).

load_rule(transcr_table(Num,T),Lang,Dial) :-
	!,
	assertz(transcr_table(Lang:Dial,Num,T)),
	read_grammar(Lang,Dial).

load_rule(Dial :: transcr_table(T),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(transcr_table(Lang:Dial1,1,T)),
	asserta(current_transcr_table(Lang:Dial1,1)),
	read_grammar(Lang,Dial1).

load_rule(transcr_table(T),Lang,Dial) :-
	!,
	assertz(transcr_table(Lang:Dial,1,T)),
	asserta(current_transcr_table(Lang:Dial,1)),	
	read_grammar(Lang,Dial).


load_rule(Dial :: current_transcr_table(T),Lang,Dial1) :-
	is_a(Lang,Dial1,Dial),
	!,
	assertz(current_transcr_table(Lang:Dial1,T)),
	read_grammar(Lang,Dial1).

load_rule(current_transcr_table(T),Lang,Dial) :-
	!,
	assertz(current_transcr_table(Lang:Dial,T)),
	read_grammar(Lang,Dial).


load_rule(morphology,Lang,Dial) :-
	!,
	assertz(parsing_morphology),
	read_grammar(Lang,Dial).
load_rule(syntax,Lang,Dial) :-
	!,
	retract(parsing_morphology),
	read_grammar(Lang,Dial).



/* FORMATTED GRAMMAR */
load_rule(L2 gtitle(Title),Lang,Dial) :-
	!,
	assertz(g_title(Lang:Dial,L2,Title)),
	read_grammar(Lang,Dial).
load_rule(gversion Version,Lang,Dial) :-
	!,
	assertz(g_version(Lang:Dial,Version)),
	read_grammar(Lang,Dial).
load_rule(gauthor Author,Lang,Dial) :-
	!,
	assertz(g_author(Lang:Dial,Author)),
	read_grammar(Lang,Dial).
load_rule(gdate Date,Lang,Dial) :-
	!,
	assertz(g_date(Lang:Dial,Date)),
	read_grammar(Lang,Dial).
load_rule(gackn Date,Lang,Dial) :-
	!,
	assertz(g_ackn(Lang:Dial,Date)),
	read_grammar(Lang,Dial).


load_rule(L2 gabstract Abstract ,Lang,Dial) :-
	!,
	assertz(g_abstract(Lang:Dial,L2 ,Abstract)),
	read_grammar(Lang,Dial).

load_rule(L2 gprologue Prologue ,Lang,Dial) :-
	!,
	assertz(g_prologue(Lang:Dial,L2,Prologue)),
	read_grammar(Lang,Dial).

load_rule(L2 glevels Lev ,Lang,Dial) :-
	!,
	assertz(g_levels(Lang:Dial,L2,Lev)),
	read_grammar(Lang,Dial).

load_rule(L2 gtranscr Prologue ,Lang,Dial) :-
	!,
	assertz(g_transcr(Lang:Dial,L2,Prologue)),
	read_grammar(Lang,Dial).

load_rule(L2 gmorph Prologue ,Lang,Dial) :-
	!,
	assertz(g_morph(Lang:Dial,L2,Prologue)),
	read_grammar(Lang,Dial).

load_rule(L2 gsynt Prologue ,Lang,Dial) :-
	!,
	assertz(g_synt(Lang:Dial,L2,Prologue)),
	read_grammar(Lang,Dial).

load_rule(L2 gepilogue Prologue ,Lang,Dial) :-
	!,
	assertz(g_epilogue(Lang:Dial,L2,Prologue)),
	read_grammar(Lang,Dial).


load_rule(L2 gdep_intro Prologue ,Lang,Dial) :-
	!,
	assertz(g_dep_intro(Lang:Dial,L2,Prologue)),
	read_grammar(Lang,Dial).

/*
load_rule(glevel(Level,Name,Descr),Lang,Dial) :-
	!,
	assertz(g_level(Lang:Dial,Level,Name,Descr)),
	read_grammar(Lang,Dial).


load_rule(g_dep(RuleNumber,Name,Descr,Examples),Lang,Dial) :-
	!,
	assertz(g_dep(Lang:Dial,RuleNumber,Name,Descr,Examples)),
	read_grammar(Lang,Dial).
*/


load_rule(Rule dr r_doc Doc,Lang,Dial) :-
	!,
	assertz(dep_doc(Lang:Dial,Rule,Doc)),
	read_grammar(Lang,Dial).
load_rule(Rule dr r_a Doc,Lang,Dial) :-
	!,
	assertz(dep_analyze(Lang:Dial,Rule,Doc)),
	read_grammar(Lang,Dial).
load_rule(Rule dr r_t Doc,Lang,Dial) :-
	!,
	assertz(dep_transl(Lang:Dial,Rule,Doc)),
	read_grammar(Lang,Dial).
load_rule(Rule dr r_c CondList,Lang,Dial) :-
	!,
	assertz(dep_print_cond_list(Lang:Dial,Rule,CondList)),
	read_grammar(Lang,Dial).



load_rule(ph(Rule) : r_doc Doc,Lang,Dial) :-
	!,
	assertz(ph_doc(Lang:Dial,Rule,Doc)),
	read_grammar(Lang,Dial).
load_rule(ph(Rule) : r_a Doc,Lang,Dial) :-
	!,
	assertz(ph_analyze(Lang:Dial,Rule,Doc)),
	read_grammar(Lang,Dial).
load_rule(ph(Rule) : r_c CondList,Lang,Dial) :-
	!,
	assertz(ph_print_cond_list(Lang:Dial,Rule,CondList)),
	read_grammar(Lang,Dial).

load_rule(auto_text(A,B),Lang,Dial) :-
	!,
	assertz(auto_text(Lang:Dial,A,B)),
	read_grammar(Lang,Dial).

/* FONTS */
load_rule(text_font(Format,Font),Lang,dial) :-
	!,
	asserta(mark_up:markup_text(Format,text_font(Lang:Dial),Font)),
	read_grammar(Lang,Dial).
load_rule(nsm_font(Format,Font),Lang,dial) :-
	!,
	asserta(mark_up:markup_text(Format,nsm_font(Lang:Dial),Font)),
	read_grammar(Lang,Dial).




load_rule(_DiscardedRule,Lang,Dial) :-
	!,
	read_grammar(Lang,Dial).

load_rule(end_of_file,_,_) :- true.


assert_dep(dep(Lang:Dial,Num,A+B==>D,Cond)) :-
	assertz(dep(Lang:Dial,Num,A+B==>D,Cond)).
assert_dep(dep(Lang:Dial,Num,A-B==>D,Cond)) :-
	assertz(dep(Lang:Dial,Num,A+B==>D,Cond)),
	assertz(dep(Lang:Dial,Num,B+A==>D,Cond)).




is_a(_,D,D).
is_a(Lang,D,D1) :-
	subdialect(Lang,D,D1).
is_a(Lang,D,D2) :-
	subdialect(Lang,D,D1),
	is_a(Lang,D1,D2).

% v => [class:v, h:V, time:T1, subj:P] 
%          : [mood/Vow, time/T] // ConditionList

build_syntagmeme(Lang,P1,synt(Name,P,A,C)) :-
	default_analysis(Lang,Name,A1,C,B),
	inst_synt(P1,P,B),
	inst_a(P1,A,A1,B).

inst_synt([],[],_B).
inst_synt([Slot:Filler|P],[Slot:Filler/Exp|P1],B) :-
	check_bindings(Slot:Filler/Exp,B),
	inst_synt(P,P1,B).

check_bindings(Slot:_Filler/Exp,Bindings) :-
	member(Slot/Exp,Bindings),
	!.
check_bindings(_Slot:_Filler/_Exp,_B).


inst_a(_,[],[],_).
inst_a(P,[Slot:Value|A],[Slot:_Value1|A1],Bindings) :-
	!,
	member(Slot/Value,Bindings),
	inst_a(P,A,A1,Bindings).
inst_a(P,[Slot|A],[Slot:_|A1],Bindings) :-
	!,
	member(Slot/_,Bindings),
	inst_a(P,A,A1,Bindings).
inst_a(P,[Slot:Value|A],[Slot:Value|A1],B) :-
	!,
	member(Slot:_Filler/Value,P),
	inst_a(P,A,A1,B).
inst_a(P,[Slot|A],[Slot:Value|A1],B) :-
	member(Slot:_Filler/Value,P),
	inst_a(P,A,A1,B).



pass_through_hist(Lang) :-
	findall(0:m(A,B,C),
		m(Lang,A,B,C),
		Dict1),
	findall(Num:m(A,B,C),
		hm(Lang,Num,A,B,C),
		Dict2),
	append(Dict1,Dict2,Dict),	
	retractall(m(Lang,_,_,_)),
	retractall(hm(Lang,_,_,_,_)),
	derive_lex(Lang,Dict),
	retractall(historic_pf(Lang,_,_,_,_)).
% quando farò il formattatore di gramm storiche:
% la lista Dict1 e Dict2: assert_list cambiando m in qualche forma di hm ( es. dict1 con con numero 0)
% non retract più historic_pf



derive_lex(_Lang,[]).
derive_lex(Lang,[Num:m(A,B,C)|Dict]) :-
	findall(pf(WF,N,AA,BB,CC),
		historic_pf(Lang,info(WF,N),AA,BB,CC),
		Rules),
	append([35|B],[35],B1a),
	do_pf(Lang,Rules,Num,shape(m(A,B1a,C),m(A1,B1b,C1)),AddToDict),
	append([35|B1],[35],B1b),
	assertz(m(Lang,A1,B1,C1)),
	append(Dict,AddToDict,NewDict),
	derive_lex(Lang,NewDict).

ph_expand([],ph(_A,_B,C,D,E,F,G,I,H)) :-
	close_empty([C,D,E,F,G,I,H]). 

ph_expand([(Morph-Residue) => Morpheme|Rule],
	  ph(Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions,PreConditions)) :-
	!,
	ph_expand(Rule,ph(Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions,PreConditions)).

ph_expand([Morph => Morpheme|Rule],
	  ph(Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions,PreConditions)) :-
	ph_expand(Rule,ph(Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions,PreConditions)).

ph_expand([cond : Conditions|Rule],
	  ph(Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions,PreConditions)) :-
	ph_expand(Rule,ph(Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions,PreConditions)).

ph_expand([precond : PreConditions|Rule],
	  ph(Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions,PreConditions)) :-
	ph_expand(Rule,ph(Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions,PreConditions)).

ph_expand([l:PredLex - SuccLex|Rule],
	  ph(Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions,PreConditions)) :-
	!,
	ph_expand(Rule,ph(Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions,PreConditions)).
ph_expand([PredSurf - SuccSurf|Rule],
	  ph(Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions,PreCond)) :-
	ph_expand(Rule,ph(Morph,Morpheme,Residue,PredSurf,SuccSurf,PredLex,SuccLex,Conditions,PreCond)).


close_empty([]).
close_empty([[]|L] ) :-
	!,
	close_empty(L).
close_empty([_|L]) :-
	close_empty(L).




/* COMPILE WORD FORMULAS INTO arc/3 FACTS */
compile_morph_seq(Lang) :-
	findall(WF,morph_seq(Lang,WF),WF_L),
	compile_wf_lists(Lang,WF_L).
%	retractall(morph_seq(Lang,_)). lasciarla per formattatore

compile_wf_lists(_Lang,[]).
compile_wf_lists(Lang,[WF|List]) :-	
	append([start|WF],[stop],L1),
	compile_wf_list(Lang,L1),
	compile_wf_lists(Lang,List).
	
compile_wf_list(_,[]).
compile_wf_list(_Lang,[stop]).
compile_wf_list(Lang,[M1,M2|List]) :-
	assert_arcs(Lang,M1,M2,List),
	compile_wf_list(Lang,[M2|List]).
	
assert_arcs(Lang,*M1,M2,List) :-
	(   \+var(M1)),
	!,
	assert_arc(arc(Lang,M1,M1),List),
	assert_arcs(Lang,M1,M2,List).
assert_arcs(Lang,?M1,M2,List) :-
	(   \+var(M1)),	
	!,
	assert_arcs(Lang,M1,M2,List).
assert_arcs(Lang,M1+M2,M3,List) :-
	(   \+var(M1)),	
	(   \+var(M2)),	
	!,
	assert_arcs(Lang,M1,M2,[M3|List]),
	assert_arcs(Lang,M2,M3,List).
assert_arcs(Lang,M1,?M2,[M3|List]) :-
	(   \+var(M2)),	
	!,
	assert_arc(arc(Lang,M1,M2),List),
	assert_arcs(Lang,M1,M3,List).
assert_arcs(Lang,M1,*M2,[M3|List]) :-
	(   \+var(M2)),	
	!,
	assert_arc(arc(Lang,M1,M2),List),
	assert_arcs(Lang,M1,M3,List).
assert_arcs(Lang,M1,M2,List) :-
	assert_arc(arc(Lang,M1,M2),List).

assert_arc(arc(Lang,M1,M2),List) :-
	is_list(M1),
	!,
	assert_arc_list(Lang,M1,M2,List).
assert_arc(arc(Lang,M1,M2),List) :-
	is_list(M2),
	!,
	assert_arc_list_2(Lang,M1,M2,List).
assert_arc(arc(Lang,M1,M2),_List) :-
	check_assertz(arc(Lang,M1,M2)).


assert_arc_list(_Lang,[],_,_).
assert_arc_list(Lang,[M1|M],M2,List) :-
	assert_arcs(Lang,M1,M2,List),
	assert_arc_list(Lang,M,M2,List).
assert_arc_list(Lang,[M1+M3|M],M2,List) :-
	!,
	assert_arc_list(Lang,[M1|M],M3,[M2|List]).
%	assert_arc_list(Lang,M,M2,List).

assert_arc_list_2(_Lang,_,[],_).
assert_arc_list_2(Lang,M1,[M2+M3|M],List) :-
	!,
	assert_arc_list_2(Lang,M1,[M2|M],[M3|List]).
%	assert_arc_list_2(Lang,M1,M,List).
assert_arc_list_2(Lang,M1,[M2|M],List) :-
	assert_arcs(Lang,M1,M2,List),
	assert_arc_list_2(Lang,M1,M,List).


check_assertz(arc(_,start,start)) :- !.
check_assertz(arc(_,start,stop)) :- !.
check_assertz(arc(_,stop,stop)) :- !.

check_assertz(arc(L,VAR,C)) :-
	var(VAR),
	!,
	assertz(arc(L,VAR,C)).
check_assertz(arc(L,B,VAR)) :-
	var(VAR),
	!,
	assertz(arc(L,B,VAR)).
check_assertz(X) :- X,!.
check_assertz(X) :- assertz(X).



% SVILUPPO

assert_syntagmeme(LD,Name,P,A,C) :-
       build_allotagmas(LD,Name,p(P,[]),A,C),
       erase_marks(P,P1),
       assertz(syntagmeme(LD,Name,P1,A,C)).

build_allotagmas(LD,Name,p([],P),A,C) :-
	erase_marks(P,P1),
	assertx(allosyntagma(LD,Name,P1,A,C)).
build_allotagmas(LD,Name,p([~Slot:Filler/Exp|P],P1),A,C) :-
	!,
	append(P1,P,AllP),
	insert_movable(LD,Name,Slot:Filler/Exp,AllP,[],A,C).
build_allotagmas(LD,Name,p([~ ? Slot:Filler/Exp|P],P1),A,C) :-
	!,
	append(P1,P,AllP),
	insert_movable(LD,Name,Slot:Filler/Exp,AllP,[],A,C),
	build_allotagmas(LD,Name,p(AllP,[]),A,C).
build_allotagmas(LD,Name,p([?Slot:Filler/Exp|P],P1),A,C) :-
	!,
	append(P1,[Slot:Filler/Exp],P2),
	build_allotagmas(LD,Name,p(P,P2),A,C),
	build_allotagmas(LD,Name,p(P,P1),A,C).
build_allotagmas(LD,Name,p(['<'|P],P1),A,C) :-
	!,
	append(P1,['<'],P2),
	build_allotagmas(LD,Name,p(P,P2),A,C).
build_allotagmas(LD,Name,p(['>'|P],P1),A,C) :-
	!,
	append(P1,['>'],P2),	
	build_allotagmas(LD,Name,p(P,P2),A,C).
build_allotagmas(LD,Name,p([Slot:Filler/Exp|P],P1),A,C) :-
	!,
	append(P1,[Slot:Filler/Exp],P2),
	build_allotagmas(LD,Name,p(P,P2),A,C).


insert_movable(LD,Name,Tagmeme,[],P,A,C) :-
	append(P,[Tagmeme],P1),
	build_allotagmas(LD,Name,p(P1,[]),A,C).
insert_movable(LD,Name,Tagmeme,['<'|Pa],Pb,A,C) :-
	!,
	skip_kernel(Pa,Pb,NewPa,NewPb),
	insert_movable(LD,Name,Tagmeme,NewPa,NewPb,A,C).
insert_movable(LD,Name,Tagmeme,[T|Pa],Pb,A,C) :-
	append(Pb,[Tagmeme,T|Pa],P),
	build_allotagmas(LD,Name,p(P,[]),A,C),
	append(Pb,[T],NewPb),
	insert_movable(LD,Name,Tagmeme,Pa,NewPb,A,C).


skip_kernel(['>'|P1],P2,P1,P2) :- !.
skip_kernel([],P2,[],P2) :- !.
skip_kernel([T|P1],P2,NewP1,NewP2) :-
	append(P2,[T],P),
	skip_kernel(P1,P,NewP1,NewP2).
	

erase_marks(['<'|P],P1) :-
	!,erase_marks(P,P1).
erase_marks(['>'|P],P1) :-
	!,erase_marks(P,P1).
erase_marks([T|P],[T|P1]) :-
	erase_marks(P,P1).
erase_marks([~T|P],[T|P1]) :-
	!,
	erase_marks(P,P1).
erase_marks([],[]).


assertx(A) :- A,!.
assertx(A) :- assertz(A).



transform(Lang) :-
	findall(synt(Name,P,A,C),
		syntagmeme(Lang,Name,P,A,C),
		S),
	do_transforms(Lang,S).

do_transforms(_,[]).
do_transforms(Lang,[synt(N,P,A,C)|S]) :-
	findall(t(SD,SC),
		transformations(Lang,SD,SC),
		TList),
	try_transforms(Lang,synt(N,P,A,C),TList),
	do_transforms(Lang,S).

try_transforms(Lang,synt(N,P,A,C),[t(sd(p:P1,a:A1),SC)|List]) :-
	contains(P,P1),
	contains(A,A1),
	!,
	do_transform(Lang,synt(N,P,A,C),SC),
	try_transforms(Lang,synt(N,P,A,C),List).

		     
do_transform(_Lang,_Synt,[]).
do_transform(Lang,Synt,[T|SC]) :-
	do_one(Lang,Synt,T),
	do_transform(Lang,Synt,SC).
	
do_one(Lang,synt(N,P,A,C),sc(p:P1,a:A1)) :-
	do_elem_ts(P,P1,NewP),
	replace(A,A1,NewA),
	assert_syntagmeme(Lang,N,NewP,NewA,C).
	
	
	
contains(_List,[]).
contains(List,[A:B/C|L]) :-
	!,
	member(A:B/C,List),
	contains(List,L).
contains(List,[A:B|L]) :-
	!,
	member(A:B/_,List),
	contains(List,L).
contains(List,[A|L]) :-
	!,
	member(A:_/_,List),
	contains(List,L).

replace([],_,[]).
replace([Item|L],L1,[NewItem|NewL]) :-
	member(Item => NewItem,L1),
	!,
	replace(L,L1,NewL).
replace([Item|L],L1,[Item|NewL]) :-
	replace(L,L1,NewL).
	
do_elem_ts(P,[],P).
do_elem_ts(P,[T|TList],NewP) :-
	do_elem(P,T,P1),
	do_elem_ts(P1,TList,NewP).

do_elem(P,front(X),P1) :-
	do_front(X,P,P1).
do_elem(P,del(X),P1) :-
	do_del(X,P,P1).
do_elem(P,addf(X),P1) :-
	do_addf(X,P,P1).
do_elem(P,addq(X),P1) :-
	do_addq(X,P,P1).

do_front(A:B/C,P,[A:B/C|P1]) :-
	del_from_list(A:B/C,P,P1),!.
do_front(A:B,P,[A:B/C|P1]) :-
	del_from_list(A:B/C,P,P1),!.
do_front(A,P,[A:B/C|P1]) :-
	del_from_list(A:B/C,P,P1),!.
	
do_del(A:B/C,P,P1) :-
	!,del_from_list(A:B/C,P,P1).
do_del(A:B,P,P1) :-
	!,del_from_list(A:B/_,P,P1).
do_del(A,P,P1) :-
	del_from_list(A:_/_,P,P1).

do_addf(A:B/C,P,[A:B/C|P]) :- !.
do_addf(A:B,P,[A:B/_|P]) :- !.

do_addq(A:B/C,P,P1) :-
	!,
	append(P,[A:B/C],P1).
do_addq(A:B,P,P1) :-
	!,
	append(P,[A:B/_],P1).

	
del_from_list(Item,[Item|List],List) :- !.
del_from_list(Item,[I|List],[I|List1]) :-
	del_from_list(Item,List,List1).
del_from_list(_,[],[]).

order_dep(Lang) :-
	order_dep(Lang,[],List),
	assert_max_dep(List),
	assertz_list(List).

assert_max_dep([dep(Lang,Deg,_,_)|_]) :-
	asserta(actual_max_dep(Lang,Deg)),!.
assert_max_dep(_).

order_dep(Lang,List,FinalList) :-
	retract(dep(Lang,Deg,Dep,Cond)),
	!,
	ins_ord(List,dep(Lang,Deg,Dep,Cond),NewList),
	order_dep(Lang,NewList,FinalList).
order_dep(_Lang,List,List).

ins_ord([],DEP,[DEP]) :- !.
ins_ord([dep(Lang,Deg,Dep,Cond)|L],dep(Lang,Deg1,Dep1,Cond1),
	[dep(Lang,Deg,Dep,Cond)|L1]) :-
	Deg > Deg1,
	!,
	ins_ord(L,dep(Lang,Deg1,Dep1,Cond1),L1).
ins_ord([dep(Lang,Deg,Dep,Cond)|L],dep(Lang,Deg1,Dep1,Cond1),
	[dep(Lang,Deg1,Dep1,Cond1),dep(Lang,Deg,Dep,Cond)|L]) :-
	Deg =< Deg1,
	!.

assertz_list([]).
assertz_list([Item|List]) :-
	assertz(Item),
	assertz_list(List).

% transformation(Lang,SD,SC).

transformation(eng, sd(
	        p:[time:tense],
	        a:[wh:aff]
		      ),
	        % SC
	       [ % INTERROGATIVE
 		  sc( p:[front(time)],
		    a:[wh:aff => wh:int]
		   ),
				
		 % IMPERATIVE
		  sc(
 	              p:[del(subj)],
		      a:[wh:aff => wh:imp]
		   )
	       ]).



