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

:- module(file_reader,[
		       parse_file/1
		      ]).


:- use_module(global_par).
:- use_module(mark_up).
:- use_module(messages).
:- use_module(utils).
:- use_module(nsmdalia).
:- use_module(transcr).
:- use_module(nsm_dict).
:- use_module(nsm_minimal).

/** <module> NSM-file parser

This module implements a parser for an NSM-input file 
(see the documentation file nsmfiles.txt)
*/
%%	parse_file(+File) is det
%
%	Reads a file name, checks if the file exists, then either calls
%	the file-parsing routine (see parse_file_aux(F)), or fails
%	with a _|File not found|_ message
parse_file(F) :-
	get_input_file(F,F1),
	exists_file(F1),
	!,
	parse_file_aux(F1).
parse_file(F) :-
	append("File not found: ",F,S),
	put_message(S).

parse_file_aux(F1) :-
	see(F1),
	switch_output_file,
	!,
	global_par:get_current_lang(L1),
	global_par:get_current_l2(L2),
	global_par:current_markup(Format),
	write_auto_text(Format,prologue(L1,L2)),
	get_code(C),
	parse_f(C,Format,[],[],Toc,no_in_toc,nl),
	write_toc(Format,Toc),
	write_auto_text(Format,epilogue),
	seen,
	close_output_file,
	!.
parse_file_aux(_) :- put_message("FILE PARSING ERROR").


get_input_file(FileName,File) :-
	name(File,FileName).



parse_f(64,Format,Autotext,Toc,NewToc,InToc,nl) :-
	!,
	get_code(Look),
	do_command(Look,Format,Autotext,NewAutotext,Toc,NewToc1,InToc,NewInToc,NewLook),
	parse_f(NewLook,Format,NewAutotext,NewToc1,NewToc,NewInToc,nl).

parse_f(end_of_file,_,_,Toc,Toc,_InToc,_Mode) :- !.
parse_f(26,_,_,Toc,Toc,_InToc,_Mode) :- !.
parse_f(-1,_,_,Toc,Toc,_InToc,_Mode) :- !.
parse_f(10,Format,Autotext,Toc,NewToc,InToc,r) :-
	!,
	put_format(Format,10),
	parse_f(10,Format,Autotext,Toc,NewToc,InToc,nl).
parse_f(13,Format,Autotext,Toc,NewToc,InToc,r) :-
	!,
	put_format(Format,10),	
	parse_f(10,Format,Autotext,Toc,NewToc,InToc,nl).
parse_f(10,Format,Autotext,Toc,NewToc,InToc,nl) :-	
	get_nl_list(L,13,Look),
	put_string(L),
	parse_f(Look,Format,Autotext,Toc,NewToc,InToc,nl).
parse_f(Look,Format,Autotext,Toc,NewToc,no_in_toc,_Mode) :-
	put_format(Format,Look),
	get_code(NewLook),
	parse_f(NewLook,Format,Autotext,Toc,NewToc,no_in_toc,r).
parse_f(Look,Format,Autotext,[entry(Lev,Num,Text)|Toc],NewToc,in_toc,_Mode) :-
	put_format(Format,Look),
	get_code(NewLook),
	append(Text,[Look],Text1),
	parse_f(NewLook,Format,Autotext,[entry(Lev,Num,Text1)|Toc],NewToc,in_toc,r).

get_nl_list([10|L],10,Look) :-
	!,
	get_code(C),
	get_nl_list(L,C,Look).
get_nl_list(L,13,Look) :-
	!,
	get_code(C),
	get_nl_list(L,C,Look).
get_nl_list([],C,C).

/*
put_nl_list(_L,_Format,64) :-
	!,nl.
put_nl_list(L,Format,_) :-
	put_format_list(Format,L).


put_format_list(_,[]).
put_format_list(rtf,_) :-
	!,put_code(10),put_code(13),put_code(32).
put_format_list(F,[A|B]) :-
	put_format(F,A),
	put_format_list(F,B).
*/

/*
check_mode_and_put(C,mode(echo)) :-
	current_markup(Format),
	put_format(Format,C).
check_mode_and_put(_C,mode(skip)).
*/




/* @p: PARSE AND WRITE RUNNING SENTENCES WITHOUT NSM INTERFACE */
read_parse_write_sent(Next,Format) :-
	get_code(C),skipwhite(C,C1),
	read_and_parse_aux(C1,Next,Format).

read_and_parse_aux(end_of_file,end_of_file,_) :- !.
read_and_parse_aux(64,64,_) :- 
	!,
	put_string("end.\n").
%	parse_f_init(64,_Mode).
read_and_parse_aux(C,Next,Format) :-
	input_sentence(C,PF),
	write_auto_text(Format,pre_sent),
	parse_and_write_sentence(PF),
	write_auto_text(Format,post_sent),
	get_code(C2),
	skipwhite(C2,C3),
	read_and_parse_aux(C3,Next,Format).


/* @a: PARSE AND WRITE RUNNING SENTENCES WITHOUT NSM INTERFACE AND
OUTPUT ANALYSIS */
read_parse_write_sent_2(Lang,Next,Format) :-
	get_code(C),skipwhite(C,C1),
	read_and_parse_aux_2(Lang,C1,Next,Format).

read_and_parse_aux_2(_Lang,end_of_file,end_of_file,_) :- !.
read_and_parse_aux_2(_Lang,64,64,_) :- 
	!,
	put_string("end.\n").
%	parse_f_init(64,_Mode).
read_and_parse_aux_2(Lang,C,Next,Format) :-
	input_sentence(C,PF),
	write_auto_text(Format,pre_sent_with_analysis),
	parse_sentence(Lang,PF,LF,_Rest),
	put_string(PF),
	write_auto_text(Format,infra_sent_with_analysis),
	write(LF),
	write_auto_text(Format,post_sent_with_analysis),	
	get_code(C2),
	skipwhite(C2,C3),
	read_and_parse_aux_2(Lang,C3,Next,Format).

input_text(64,[]).
input_text(C,[PF|T]) :-
	input_sentence(C,PF),
	get_code(C1),
	skipwhite(C1,C2),
	input_text(C2,T).


/* @g: GENERATE FROM RUNNING FORMULAS, ONE SENT. AT A TIME */

read_gen_write_sent(Format) :-
	read(Formula),
	read_gen_write_sent(Format,Formula).
read_gen_write_sent(_Format,end) :- !.
read_gen_write_sent(Format,F) :-
	write_auto_text(Format,pre_sent),
	gen_and_write_sentence(F),
	write_auto_text(Format,post_sent),
	read(F1),
	read_gen_write_sent(Format,F1).



read_and_translate_text(L,L,C) :-
	!,
	get_code(C).
%	parse_f(C1,_Mode).
read_and_translate_text(L1,L2,C2) :-
	get_code(C),
	skipwhite(C,C1),
	read_and_parse_sentences(L1,L2,C1,T,C2),
%	parse_text_level(L1,T,T1),
%	gen_text_level(L2,T1,T2),
	gen_and_write_text(L2,T).


read_and_parse_sentences(_,_,end_of_file,[],end_of_file) :- !.
read_and_parse_sentences(_,_,64,[],64) :- !.
%	get_code(C),
%	do_command(C,_).
read_and_parse_sentences(Lang,L2,C3,[P1|T],Next) :- 
	input_sentence(C3,S),
	parse_sentence(Lang,S,P,_Rest),
	adjust_lf_version(Lang,L2,nsm,P1,P),
	get_code(C4),
	skipwhite(C4,C9),
	read_and_parse_sentences(Lang,L2,C9,T,Next).


input_sentence(46,[]) :- !.
input_sentence(C,[C|S]) :-
	get_code(C1),
	input_sentence(C1,S).





/* PARSING AND TRANSLATING WITH NSM FORMAT */
read_and_translate_nsm_text(L,L,_Format,C1) :-
	!,
	get_code(C1).
%	parse_f(C1,_Mode).
read_and_translate_nsm_text(L1,L2,Format,Look) :-
	get_code(C),skipwhite(C,C7),
	read_and_parse_nsm_text(L1,L2,C7,[],T,[0],Format,Look),
	parse_text_level(L1,T,T1),
	gen_text_level(L2,T1,T2),
	get_synt_grammar_type(L2,Grammar),
	gen_and_write_nsm_text(L2,Grammar,Format,T2).

read_and_translate_2_nsm_text(L,L,_Format,C1) :-
	!,
	get_code(C1).
%	parse_f(C1,_Mode).
read_and_translate_2_nsm_text(L1,L2,Format,Look) :-
	get_code(C),
	read_and_parse_2_nsm_text(L1,L2,C,[],T,[],S,[0],Look),
	parse_text_level(L1,T,T1),
	gen_text_level(L2,T1,T2),
	get_synt_grammar_type(L2,Grammar),
	gen_and_write_2_nsm_text(L2,Grammar,T2,Format,S).

/*
read_and_parse_nsm_text(_,end_of_file,T,T,[_Indent]) :- !.
read_and_parse_nsm_text(_,64,T,T,[_Indent]) :- 
	!,
	get_code(C),
	do_command(C,_).
*/

read_and_parse_nsm_text(_,_,end_of_file,T,T1,Stack,_Format,end_of_file) :- 
	check_nested_groups(-1,Stack,_,T,T1), !.
read_and_parse_nsm_text(_,_,64,T,T1,Stack,_Format,64) :- 
	check_nested_groups(-1,Stack,_,T,T1),
	!.
%	get_code(C),
%	do_command(C,_).
read_and_parse_nsm_text(Lang,L2,C,T,T1,Stack,Format,Look) :-
	skipwhite(C,C4),
	input_nsm_sentence(NewIndent,C4,S,Label,C1),
	transcr(Lang,S,S1),	
	add_grouping(NewIndent,T,T2,Stack,NewStack),
	parse_sentence(Lang,S1,P1,_Rest),
%	adjust_lf_version(Lang,L2,P1,P),
	append(T2,[NewIndent+Label:P1],T3),
	skip_newline(C1,C2),
	read_and_parse_nsm_text(Lang,L2,C2,T3,T1,NewStack,Format,Look).


read_and_parse_2_nsm_text(_,_,end_of_file,T,T1,S,S,Stack,end_of_file) :- 
	check_nested_groups(-1,Stack,_,T,T1),!.
read_and_parse_2_nsm_text(_,_,64,T,T1,S,S,Stack,64) :- 
	check_nested_groups(-1,Stack,_,T,T1),
	!.
%	get_code(C),
%	do_command(C,_).
read_and_parse_2_nsm_text(Lang,L2,C,T,T1,SQ,SQ1,Stack,Look) :-
	input_nsm_sentence(NewIndent,C,S,Label,C1),
	transcr(Lang,S,S1),	
	add_grouping(NewIndent,T,T2,Stack,NewStack),
	parse_sentence(Lang,S1,P1,_Rest),
	adjust_lf_version(Lang,L2,P1,P),	
	append(T2,[NewIndent+Label:P],T3),
	skip_newline(C1,C2),
	append(SQ,NewIndent+Label:S,SQ3),
	read_and_parse_2_nsm_text(Lang,_,C2,T3,T1,SQ3,SQ1,NewStack,Look).


read_parse_write_nsm_text(L,Format,Look) :-
	get_code(C),
	skipwhite(C,C1), 
	read_parse_write_nsm_text(L,C1,[],[0],Format,Look).

read_parse_write_nsm_text(_,end_of_file,T,Stack,Format,end_of_file) :- 
	!,
	check_nested_groups(-1,Stack,_,T,T1),
	append(T1,[end],T2),
	write_nsm_parse(Format,T2).
read_parse_write_nsm_text(_,64,T,Stack,Format,64) :- 
	!,
	check_nested_groups(-1,Stack,_,T,T2),
	append(T2,[end],T1),
	write_nsm_parse(Format,T1).
%	do_command(C,_).
read_parse_write_nsm_text(Lang,C,T,Stack,Format,Look) :-
	input_nsm_sentence(NewIndent,C,S,Label,C1),
	transcr(Lang,S,S1),
	add_grouping(NewIndent,T,T2,Stack,NewStack),
	parse_sentence(Lang,S1,P,_Rest),
	append(T2,[NewIndent+Label:P],T3),
	skip_newline(C1,C2),
	read_parse_write_nsm_text(Lang,C2,T3,NewStack,Format,Look).



add_grouping(Indent,T,T,[Indent|Stack],[Indent|Stack]) :- !.
add_grouping(NewIndent,T,T1,[Indent|Stack],[NewIndent,Indent|Stack]) :-
	NewIndent > Indent,
	!,
	append(T,[Indent+"":ct(group,begin)],T1).
add_grouping(NewIndent,T,T1,Stack,NewStack) :-
%	NewIndent < OldIndent
	check_nested_groups(NewIndent,Stack,NewStack,T,T1).

check_nested_groups(-1,[I],[I],T,T) :- !.
check_nested_groups(Indent,[Indent|Stack],[Indent|Stack],T,T) :- !.
check_nested_groups(I,[OldI|Stack],NewStack,T,T2) :- 
	I < OldI,
	!,
	append(T,[OldI+"":ct(group,end)],T1),
	check_nested_groups(I,Stack,NewStack,T1,T2).
check_nested_groups(NewIndent,[Indent|Stack],[Indent|Stack],T,T) :- 
	Indent < NewIndent,!.
check_nested_groups(_,[I],[I],T,T) :- !.



/* NSM FRONTEND */

input_nsm_sentence(Indent,40,S,Label,C1) :-	
	!,			     
	input_label(40,Label,41,0,Indent1),
	get_indent(C,Indent1,Indent),
	input_nsm_rest_sent(C,S,C1).
input_nsm_sentence(Indent,91,S,Label,C1) :-	
	!,			     
	input_label(91,Label,93,0,Indent1),
	get_indent(C,Indent1,Indent),
	input_nsm_rest_sent(C,S,C1).
input_nsm_sentence(Indent,32,S,[],C1) :-	
	!,
	get_indent(C,1,Indent),
	input_nsm_rest_sent(C,S,C1).
input_nsm_sentence(Indent,8,S,[],C1) :-	
	!,
	get_indent(C,4,Indent),
	input_nsm_rest_sent(C,S,C1).
input_nsm_sentence(0,C,S,[],C1) :-	
	input_nsm_rest_sent(C,S,C1).

input_nsm_rest_sent(47,[32|S],C1) :-
	!,
	get_code(C),
	skipwhite(C,C2),
	input_nsm_rest_sent(C2,S,C1).
input_nsm_rest_sent(92,[32|S],C1) :-
	!,
	get_code(C),
	skipwhite(C,C2),
	input_nsm_rest_sent(C2,S,C1).
input_nsm_rest_sent(10,[],10) :- !.
input_nsm_rest_sent(13,[],13) :- !.
input_nsm_rest_sent(32,[32|S],C2) :-
	!,
	get_code(C1),
	skipwhite(C1,C3),
	input_nsm_rest_sent(C3,S,C2).
input_nsm_rest_sent(C,[C|S],C2) :-
	get_code(C1),
	input_nsm_rest_sent(C1,S,C2).

input_label(end_of_file,[C,32],C,I,I) :- !.
input_label(C,[C,32],C,I,J) :- J is I + 1, !.
input_label(C,[C|Label],C1,OldIndent,Indent) :-
	get_code(C2),
	NewIndent is OldIndent + 1,
	input_label(C2,Label,C1,NewIndent,Indent).

get_indent(C,OldIndent,Indent) :-
	get_code(C2),
	get_indent_aux(C,C2,OldIndent,Indent).

get_indent_aux(C,32,OldIndent,Indent) :-
	get_code(C1),
	NewIndent is OldIndent + 1,
	!,
	get_indent_aux(C,C1,NewIndent,Indent).
get_indent_aux(C,8,OldIndent,Indent) :-
	get_code(C1),
	NewIndent is OldIndent + 4,
	!,
	get_indent_aux(C,C1,NewIndent,Indent).
get_indent_aux(C,C,Indent,Indent).




/* GENERATORE IN FORMATO NSM */

% GEN AND WRITE DOUBLE, traduzione testo a fronte (genera L2 e copia L1)
gen_and_write_2_nsm_text(L2,G,T,Format,S) :-
	global_par:current_double_format(whole_text),
	!,
	write_auto_text(Format,whole_text_prologue),
	put_nsm_text(S),
	write_auto_text(Format,whole_text_infra),
	gen_and_write_nsm_text(L2,G,Format,T),
	write_auto_text(Format,whole_text_epilogue).
gen_and_write_2_nsm_text(L2,G,T,Format,S) :-
	global_par:current_double_format(line_by_line),
	!,
	write_auto_text(Format,single_line_prologue),	
	gen_and_write_2_nsm_text_aux(L2,G,T,Format,S),
	write_auto_text(Format,single_line_epilogue).

gen_and_write_2_nsm_text_aux(_L2,_G,[],_Format,[]).
gen_and_write_2_nsm_text_aux(L2,G,[],Format,[S|SentList]) :- 
	write_auto_text(Format,single_line_before),
	put_nsm_text(S),
	write_auto_text(Format,single_line_infra),	
	write_auto_text(Format,single_line_after),
	gen_and_write_2_nsm_text_aux(L2,G,[],Format,SentList).
gen_and_write_2_nsm_text_aux(L2,Grammar,[Formula|T],Format,[]) :- 
	write_auto_text(Format,single_line_before),
	write_auto_text(Format,single_line_infra),
	is_end_of_list(T,Last),
	gen_and_write_nsm_sent(L2,Grammar,Format,Formula,Last),
	write_auto_text(Format,single_line_after),
	gen_and_write_2_nsm_text_aux(L2,Grammar,T,Format,[]).
gen_and_write_2_nsm_text_aux(L2,Grammar,[Formula|T],Format,[S|SentList]) :- 
	write_auto_text(Format,single_line_before),
	put_nsm_text(S),
	write_auto_text(Format,single_line_infra),
	is_end_of_list(T,Last),
	gen_and_write_nsm_sent(L2,Grammar,Format,Formula,Last),
	write_auto_text(Format,single_line_after),
	gen_and_write_2_nsm_text_aux(L2,Grammar,T,Format,SentList).

gen_and_write_nsm_text(_L2,_,_Format,[]).
gen_and_write_nsm_text(L2,Grammar,Format,[S|T]) :-
	is_end_of_list(T,Last),
	gen_and_write_nsm_sent(L2,Grammar,Format,S,Last),
	gen_and_write_nsm_text(L2,Grammar,Format,T).

is_end_of_list([],last).
is_end_of_list([_|_],no_last).


% GEN AND WRITE DOUBLE, legge formule da file e genera in 2 lingue
read_gen_write_2_nsm_sent :-
	read(Formula),
	global_par:get_current_lang(L1),
	global_par:get_current_l2(L2),
	global_par:current_double_format(DF),
	global_par:current_markup(Markup),
	get_synt_grammar_type(L1,G1),
	get_synt_grammar_type(L2,G2),	
	get_nsm_text(Formula,T),
	!,
	gen_text_level(L1,T,T1),
	gen_text_level(L2,T,T2),	
	gen_and_write_double(DF,Markup,L1,L2,G1,G2,T1,T2).

get_nsm_text(end,[]) :- !.
get_nsm_text(end_of_file,[]) :- !.
get_nsm_text(Formula,[Formula|Text]) :-
	read(NewFormula),
	get_nsm_text(NewFormula,Text).

gen_and_write_double(line_by_line,Format,L1,L2,G1,G2,T1,T2) :-
	write_auto_text(Format,single_line_prologue),
	gen_and_write_2_by_line(Format,L1,L2,G1,G2,T1,T2),
	write_auto_text(Format,single_line_epilogue).

gen_and_write_double(whole_text,Format,L1,L2,G1,G2,T1,T2) :-
	write_auto_text(Format,whole_text_prologue),	
	gen_and_write_2_whole(Format,L1,L2,G1,G2,T1,T2),
	write_auto_text(Format,whole_text_epilogue).	

gen_and_write_2_by_line(_,_,_,_,_,[],[]).
gen_and_write_2_by_line(Format,L1,L2,G1,G2,[I1+La1:F1|T1],[]) :-
	generic_gen_sentence(L1,G1,F1,S1),
	write_auto_text(Format,single_line_before),
	put_nsm_text([I1+La1:S1]),
	write_auto_text(Format,single_line_infra),	
	write_auto_text(Format,single_line_after),
	gen_and_write_2_by_line(Format,L1,L2,G1,G2,T1,[]).
gen_and_write_2_by_line(Format,L1,L2,G1,G2,[],[I2+La2:F2|T2]) :-
	generic_gen_sentence(L2,G2,F2,S2),	
	write_auto_text(Format,single_line_before),
	write_auto_text(Format,single_line_infra),	
	put_nsm_text([I2+La2:S2]),	
	write_auto_text(Format,single_line_after),
	gen_and_write_2_by_line(Format,L1,L2,G1,G2,[],T2).
gen_and_write_2_by_line(Format,L1,L2,G1,G2,[I1+La1:F1|T1],[I2+La2:F2|T2]) :-
	generic_gen_sentence(L1,G1,F1,S1),
	generic_gen_sentence(L2,G2,F2,S2),	
	write_auto_text(Format,single_line_before),
	put_nsm_text([I1+La1:S1]),
	write_auto_text(Format,single_line_infra),	
	put_nsm_text([I2+La2:S2]),	
	write_auto_text(Format,single_line_after),
	gen_and_write_2_by_line(Format,L1,L2,G1,G2,T1,T2).

	
gen_and_write_2_whole(Format,L1,L2,G1,G2,T1,T2) :-
	write_auto_text(Format,whole_before),
	gen_and_write_nsm_text(L1,G1,Format,T1),
	write_auto_text(Format,whole_infra),
	gen_and_write_nsm_text(L2,G2,Format,T2),
	write_auto_text(Format,whole_after).

% UTILS

spaces(N,[]) :-
	N =< 0,!.
spaces(N,[32|S]) :-
	M is N - 1,
	spaces(M,S).

add_label([],Indent,PF,String) :-
	!,
	spaces(Indent,Spaces),
	append(Spaces,PF,String).
add_label(Label,Indent,PF,String) :-
	length(Label,Length),
	Tab is Indent - Length,
	spaces(Tab,Spaces),
	append(Spaces,PF,String1),
	append(Label,String1,String).

% GEN AND WRITE, SINGLE LANG, formula passata da altra procedura 
gen_and_write_nsm_sent(Lang,Grammar,Format,Indent+Label:S,Last) :-
	generic_gen_sentence(Lang,Grammar,S,PF),
	!,
	add_label(Label,Indent,PF,String),
	write_auto_text(Format,pre_nsm_sent),
	put_string(String),
	write_auto_text(Format,post_nsm_sent(Last)),
	nl.
gen_and_write_nsm_sent(_Lang,_Grammar,Format,Indent+Label:S,Last) :-
	add_label(Label,Indent,S,String),
	write_auto_text(Format,pre_nsm_sent),
	put_string(String),
	write_auto_text(Format,post_nsm_sent(Last)),
	nl.
gen_and_write_nsm_sent(_Lang,_Grammar,Format,I+L:noparse(PF),Last) :-
	add_label(L,I,PF,String),	
	write_auto_text(Format,pre_nsm_sent),
	put_string(String),
	write_auto_text(Format,post_nsm_sent(Last)),
	nl.
gen_and_write_nsm_sent(_Lang,_Grammar,Format,noparse(PF),Last) :-
	write_auto_text(Format,pre_nsm_sent),
	put_string(PF),
	write_auto_text(Format,post_nsm_sent(Last)),
	nl.
gen_and_write_nsm_sent(Lang,Grammar,Format,ct(group,GROUP),Last) :-
	generic_gen_sentence(Lang,Grammar,ct(group,GROUP),PF),
	write_auto_text(Format,pre_nsm_sent),
	put_string(PF),
	write_auto_text(Format,post_nsm_sent(Last)),
	nl.


% LEGGE FORMULE E GENERA FRASI
read_gen_write_nsm_sent(Format) :-
	read(Formula),
	global_par:current_lang(L),
	get_synt_grammar_type(L,G),
	read_gen_write_nsm_sent(L,G,Format,Formula).

read_gen_write_nsm_sent(_,_,_Format,end) :- !.
read_gen_write_nsm_sent(L,G,Format,I+Label:F) :- 
	!,
	read(F1),
	is_last(F1,Last),
	gen_and_write_nsm_sent(L,G,Format,I+Label:F,Last),
	read_gen_write_nsm_sent(L,G,Format,F1)	.
read_gen_write_nsm_sent(L,G,Format,F) :-	
	read(F1),
	is_last(F1,Last),
	gen_and_write_nsm_sent(L,G,Format,0+"":F,Last),
	read_gen_write_nsm_sent(L,G,Format,F1).

is_last(end_of_file,last) :-!.
is_last(end,last) :- !.
is_last(_,no_last).


% BACKEND PER NSM (scrive in formato NSM)
write_nsm_parse(_Format,[]).
write_nsm_parse(Format,[I+L:noparse(S)|T]) :-
	!,
	write_auto_text(Format,pre_sent),
	write(I),write('+'),	
	put_nsm_label(L),write(':'),
	append_list(S1,["noparse(\"",S,"\")."]),
	put_string(S1),
	write_auto_text(Format,post_sent),
	nl,
	write_nsm_parse(Format,T).
write_nsm_parse(Format,[I+L:P|T]) :-
	!,
	write_auto_text(Format,pre_sent),
	write(I),write('+'),
	put_nsm_label(L),write(':'),
	write(P), write('.'),
	write_auto_text(Format,post_sent),
	nl,
	write_nsm_parse(Format,T).
write_nsm_parse(Format,[C|T]) :-
	write_auto_text(Format,pre_sent),
	write(C),write('.'),
	write_auto_text(Format,post_sent),
	nl,
	write_nsm_parse(Format,T).


put_nsm_label([]) :- put_string("[]"),!.
put_nsm_label(L) :- append_list(S,["\"",L,"\""]), put_string(S).


put_nsm_text([]).
put_nsm_text([Indent+Label:S|T]) :-
	add_label(Label,Indent,S,String),
	put_string(String),
	nl,
	put_nsm_text(T).





/* COMMANDS */

% @@
do_command(64,Format,Autotext,Autotext,Toc,Toc,InToc,InToc,NewLook) :-
	!,
	put_format(Format,64),
	get_code(NewLook).
%	parse_f(NewLook,Mode).

% @p (parse)
do_command(112,Format,Autotext,Autotext,Toc,Toc,InToc,InToc,Look) :-
	!,
	read_parse_write_sent(Look,Format).
%	get_code(Look).
%	parse_f(Look,Mode).

% @P
do_command(80,Format,Autotext,Autotext,Toc,Toc,InToc,InToc,Look) :-
	!,
	get_current_lang(L1),
	read_parse_write_nsm_text(L1,Format,Look).
%	get_code(Look).


% @g
do_command(103,Format,Autotext,Autotext,Toc,Toc,InToc,InToc,10) :-
	!,
	read_gen_write_sent(Format).
%	get_code(C).
%	parse_f(C,Mode).

% @G
do_command(71,Format,Autotext,Autotext,Toc,Toc,InToc,InToc,10) :-
	!,
	read_gen_write_nsm_sent(Format).
%	get_code(Look).


% @t
do_command(116,_Format,Autotext,Autotext,Toc,Toc,InToc,InToc,Look) :-
	!,
	get_current_lang(L1),
	get_current_l2(L2),
	read_and_translate_text(L1,L2,Look).
%	get_code(Look).
% @T
do_command(84,Format,Autotext,Autotext,Toc,Toc,InToc,InToc,Look) :-
	!,
	get_current_lang(L1),
	get_current_l2(L2),
	read_and_translate_nsm_text(L1,L2,Format,Look).
%	get_code(Look).


% @e (end)
do_command(101,_Format,Autotext,Autotext,Toc,Toc,InToc,InToc,C):-
	!,
	get_code(C).
%	parse_f(C,Mode).
	
/*
% @l
do_command(108,_Mode,L5) :-
	!,
	get_current_lang(Lang),
	get_code(Look),
	skipwhite(Look,L1),
	get_code(L2),get_code(L3),get_code(L4),
	name(Lang1,[L1,L2,L3]),
%	set_lang_mode(Lang,Lang1,Mode,NewMode),
	skipwhite(L4,L5).
%	parse_f(L5,NewMode).
*/

% @c
do_command(99,_Format,Autotext,Autotext,Toc,Toc,InToc,InToc,Look) :-
	!,
	readcommands(Look).

% @i
do_command(105,Format,Autotext,Autotext,Toc,NewToc,InToc,InToc,Look) :-
	!,
	get_current_lang(L1),
	get_current_l2(L2),
	get_l_name(L1,L1a),
	get_l_name(L2,L2a),
	get_code(C),
	skipwhite(C,C1),
	read_ident(Ident,C1,Look),
%	get_code(Look),
	print_text(Ident,Format,L1a,L2a,Text),
	add_text_to_toc(InToc,Text,Toc,NewToc).
%	parse_f(C2,Mode).

% @D
do_command(68,Format,Autotext,Autotext,Toc,Toc,InToc,InToc,Look) :-
	!,
	get_current_lang(L1),
	get_current_l2(L2),
	read_and_translate_2_nsm_text(L1,L2,Format,Look).

% @S
do_command(83,_Format,Autotext,Autotext,Toc,Toc,InToc,InToc,10) :-
	!,
	read_gen_write_2_nsm_sent.

% @a
do_command(97,Format,Autotext,Autotext,Toc,Toc,InToc,InToc,Next) :-
	!,
	global_par:current_lang(Lang),
	read_parse_write_sent_2(Lang,Next,Format).

% @f 
do_command(102,Format,Autotext,NewAutotext,Toc,NewToc,InToc,NewInToc,C2) :-
	!,
	get_code(C),
	skipwhite(C,C1),
	read_ident(Ident,C1,C2),
	switch_to_toc(Ident,InToc,NewInToc,Toc,NewToc),
	write_auto_text(Format,Ident),
	write_automatic_numbers(Ident,Autotext,NewAutotext,String),
	add_to_toc(NewInToc,String,NewToc).
%	get_code(C2).
%	parse_f(C2,Mode).

% @E
do_command(69,Format,Autotext,Autotext,Toc,Toc,InToc,InToc,Next) :-
	!,
	global_par:current_language(L1),
	switch_makedict(1),
	parse_dict_entry_header(Record,C1),
	read_and_parse_nsm_text(L1,e,C1,[],T,[0],Format,Next),
	assert_dict_entry(L1,Record,T),
	switch_makedict(0).
	
% @<ENTER>
do_command(10,_Format,Autotext,Toc,Toc,InToc,InToc,Autotext,C2) :-
	skipwhite(10,C2).

% @UNKNOWN TAG
do_command(Code,_Format,Autotext,Autotext,Toc,Toc,InToc,InToc,C1) :-
	append(" ** @",[Code],X),
	put_message(X),
	get_code(C),skipwhite(C,C1).

switch_to_toc([115,Level],_OldInToc,in_toc,Toc,[entry(Level,_,[])|Toc]) :- !.
switch_to_toc([116,_Num],in_toc,in_toc,Toc,Toc) :- !.
switch_to_toc(_Tag,_OldInToc,no_in_toc,Toc,Toc).

add_to_toc(no_in_toc,_,_).
add_to_toc(in_toc,_String,[]).
add_to_toc(in_toc,String,[entry(_,String,_)|_]).

add_text_to_toc(no_in_toc,_,Toc,Toc).
add_text_to_toc(in_toc,_String,[],[]).
add_text_to_toc(in_toc,Text,[entry(Lev,Num,OldText)|Toc],[entry(Lev,Num,NewText)|Toc]) :-
	append(OldText,Text,NewText).



get_l_name(L1,L2) :-
	L1 =.. [L2,[]],!.
get_l_name(L,L).


	
% prima prova L2, poi prova L1 (perché sto traducendo da L1 in L2)
print_text(Id,Format,_L1,L2,Text1) :-
	global_par:mtext(Id,L2,Text),
	!,
	transcr_back(L2,Text,Text1),
	print_id(Format,Id),
	write_auto_text(Format,text_line_before),
	put_string(Text1),
	write_auto_text(Format,text_line_after).
print_text(Id,Format,L1,_L2,Text1) :-
	global_par:mtext(Id,L1,Text),
	!,
	transcr_back(L1,Text,Text1),	
	print_id(Format,Id),	
	write_auto_text(Format,text_line_before),
	put_string(Text1),
	write_auto_text(Format,text_line_after).
print_text(Id,_Format,_,_,Id) :-
	put_string(Id).

print_id(Format,Id) :-
	global_par:text_id_format_number(N),
	!,
	global_par:text_id_format(N,Id,NewId),
	put_id(Format,NewId).
print_id(_Format,_Id).

put_id(_,[]) :- !.
put_id(Format,Id) :-
	write_auto_text(Format,id_before),
	put_string(Id),
	write_auto_text(Format,id_after).

readcommands(C) :-
	read(Command),
	call_command(Command,C).

call_command(end_of_file,end_of_file) :- !.
call_command(end,C) :- !,get_code(C).
call_command(Command,C) :-
	!,
	call(Command),
	readcommands(C).

/*
set_lang_mode(L1,L1,Mode,Mode) :- !.
set_lang_mode(L1,L2,Mode,Mode) :- 
	L1 =.. [L2,[]],!.
set_lang_mode(_,xxx,Mode,Mode) :- !.
set_lang_mode(_L1,_L2,_Mode,mode(skip)).
*/

	
% write_automatic_numbers("s0",AT,AT) :- !. Fare S invece di s0
write_automatic_numbers([115,Num],AT,NAT,String) :-
	!,
	get_and_increment_number(Num,AT,NAT,[],String),
	put_string(String).
write_automatic_numbers(_,AT,AT,_).

get_and_increment_number(Level,[Level:N|_],[Level:M],S1,S) :-
	!,
	M is N + 1,
	name(M,X),
	append_list(S,[S1,X,". "]).
get_and_increment_number(Level,[Level1:N|AT],[Level1:N|NAT],S1,S) :-
	name(N,X),
	append_list(S2,[S1,X,"."]),
	get_and_increment_number(Level,AT,NAT,S2,S).
get_and_increment_number(Level,[],[Level:1],S1,S) :-
	append(S1,"1. ",S).


