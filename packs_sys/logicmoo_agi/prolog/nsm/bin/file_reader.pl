
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

parse_file(F) :-
	get_input_file(F,F1),
	see(F1),
	switch_output_file,
	get0(C),
	parse_f_init(C,mode(echo)),
	seen,
	close_output_file,
	!.

get_input_file(FileName,File) :-
	name(File,FileName).

set_output_file(FileName) :-
	retractall(current_output_file(_)),
	!,
	asserta(current_output_file(FileName)),
	append("Output file set to ",FileName,S),
	put_message(S).

set_output_file(FileName) :-
	asserta(current_output_file(FileName)),
	append("Output file set to ",FileName,S),
	put_message(S).


switch_output_file :-
	current_output_file(FileName),
	name(File,FileName),
	!,
	append(File).
switch_output_file.

close_output_file :-
	current_output_file(F),
	!,
	told,
	retractall(current_output_file(_Filename)),
	append_list(S,["Output written to file \"",F,"\"; output file closed"]),
	put_message(S),
	nl.
close_output_file.


% @ All'inizio riga
parse_f_init(64,Mode) :-
	!,
	get0(Look),
	do_command(Look,Mode),
	get0(NewLook),
	parse_f(NewLook,Mode).
parse_f_init(end_of_file,_Mode) :- !.
parse_f_init(26,_Mode) :- !.
parse_f_init(-1,_Mode) :- !.
parse_f_init(10,Mode) :-
	!,
	check_mode_and_put(10,Mode),
	get0(NewLook),
	parse_f_init(NewLook,Mode).
parse_f_init(13,Mode) :-
	!,
	check_mode_and_put(13,Mode),
	get0(NewLook),
	parse_f_init(NewLook,Mode).
parse_f_init(Look,Mode) :-
	check_mode_and_put(Look,Mode),
	get0(NewLook),
	parse_f(NewLook,Mode).


parse_f(end_of_file,_Mode) :- !.
parse_f(26,_Mode) :- !.
parse_f(-1,_Mode) :- !.
parse_f(10,Mode) :-
	!,
	check_mode_and_put(10,Mode),
	get0(NewLook),
	parse_f_init(NewLook,Mode).
parse_f(13,Mode) :-
	!,
	check_mode_and_put(13,Mode),
	get0(NewLook),
	parse_f_init(NewLook,Mode).
parse_f(Look,Mode) :-
	check_mode_and_put(Look,Mode),
	get0(NewLook),
	parse_f(NewLook,Mode).

check_mode_and_put(C,mode(echo)) :-
	put(C).
check_mode_and_put(_C,mode(skip)).


/* @p: PARSE AND WRITE RUNNING SENTENCES WITHOUT NSM INTERFACE */
read_parse_write_sent :-
	get0(C),skipwhite(C,C1),
	read_and_parse_aux(C1).

read_and_parse_aux(64) :- 
	!,
	put_string("end.\n"),
	parse_f_init(64,_Mode).

read_and_parse_aux(C) :-
	input_sentence(C,PF),
	parse_and_write_sentence(PF),
	get0(C2),
	skipwhite(C2,C3),
	read_and_parse_aux(C3).

input_sentence(46,[]) :- !.
input_sentence(C,[C|S]) :-
	get0(C1),
	input_sentence(C1,S).

input_text(64,[]).
input_text(C,[PF|T]) :-
	input_sentence(C,PF),
	get0(C1),
	skipwhite(C1,C2),
	input_text(C2,T).


/* @g: GENERATE FROM RUNNING FORMULAS, ONE SENT. AT A TIME */

read_gen_write_sent :-
	read(Formula),
	read_gen_write_sent(Formula).
read_gen_write_sent(end) :- !.
read_gen_write_sent(F) :-	
	gen_and_write_sentence(F),
	read(F1),
	read_gen_write_sent(F1).


read_and_translate_text(L,L) :-
	!,
	get0(C1),
	parse_f(C1,_Mode).
read_and_translate_text(L1,L2) :-
	get0(C),
	read_and_parse_sentences(L1,C,T),
	parse_semantics(L1,T,T1),
	gen_semantics(L2,T1,T2),
	gen_and_write_text(L2,T2).

read_and_parse_sentences(_,end_of_file,[]) :- !.
read_and_parse_sentences(_,"@",[]) :- !,
	get0(C),
	do_command(C,_).
read_and_parse_sentences(Lang,C,[P|T]) :- 
	input_sentence,(C,S),
	parse_sentence(Lang,S,P,_Rest),
	get0(C),
	skipwhite(C,C1),
	read_and_parse_sentences(Lang,C1,T).



gen_and_write_nsm_text(_L2,_,[]).
gen_and_write_nsm_text(L2,Grammar,[I+L:noparse(PF)|T]) :-
	append(L,PF,String),
	tab(I),
	put_string(String),
	gen_and_write_nsm_text(L2,Grammar,T).

gen_and_write_nsm_text(_L2,_Grammar,[]).
gen_and_write_nsm_text(L2,Grammar,[I+L:CT|T]) :-
	gen_and_write_nsm_sent(L2,Grammar,I,L,CT),
	gen_and_write_nsm_text(L2,Grammar,T).
gen_and_write_nsm_text(L2,Grammar,[noparse(PF)|T]) :-
	put_string(PF),nl,
	gen_and_write_nsm_text(L2,Grammar,T).
gen_and_write_nsm_text(L2,Grammar,[ct(group,GROUP)|T]) :-
	generic_gen_sentence(L2,Grammar,ct(group,GROUP),PF),
	put_string(PF),nl,
	gen_and_write_nsm_text(L2,Grammar,T).

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

gen_and_write_nsm_sent(Lang,Grammar,Indent,Label,S) :-
	generic_gen_sentence(Lang,Grammar,S,PF),
	!,
	add_label(Label,Indent,PF,String),
	put_string(String),nl.
gen_and_write_nsm_sent(_Lang,_Grammar,Indent,Label,S) :-
	add_label(Label,Indent,S,String),	
	put_string(String),nl.

read_gen_write_nsm_sent :-
	read(Formula),
	current_lang(L),
	get_synt_grammar_type(L,G),
	read_gen_write_nsm_sent(L,G,Formula).
read_gen_write_nsm_sent(_,_,end) :- !.
read_gen_write_nsm_sent(L,G,I+Label:F) :- 
	!,
	gen_and_write_nsm_sent(L,G,I,Label,F),
	read(F1),
	read_gen_write_nsm_sent(L,G,F1)	.
read_gen_write_nsm_sent(L,G,F) :-	
	gen_and_write_nsm_sent(L,G,0,"",F),
	read(F1),
	read_gen_write_nsm_sent(L,G,F1).


/* NSM FRONTEND */
read_and_translate_nsm_text(L,L) :-
	!,
	get0(C1),
	parse_f(C1,_Mode).
read_and_translate_nsm_text(L1,L2) :-
	get0(C),
	read_and_parse_nsm_sentences(L1,0,C,[],T),
	parse_semantics(L1,T,T1),
	gen_semantics(L2,T1,T2),
	get_synt_grammar_type(L2,Grammar),
	gen_and_write_nsm_text(L2,Grammar,T2).

read_parse_write_nsm_text(L) :-
	get0(C),
	skipwhite(C,C1), 
	read_and_parse_nsm_text(L,0,C1,[]).

write_nsm_parse([]).
write_nsm_parse([I+L:noparse(S)|T]) :-
	!,
	write(I),write('+'),	
	put_nsm_label(L),write(':'),
	append_list(S1,["noparse(\"",S,"\")."]),
	put_string(S1),nl,
	write_nsm_parse(T).
write_nsm_parse([I+L:P|T]) :-
	!,
	write(I),write('+'),
	put_nsm_label(L),write(':'),
	write(P), write('.'),
	nl,
	write_nsm_parse(T).
write_nsm_parse([C|T]) :-
	write(C),write('.'),nl,
	write_nsm_parse(T).


put_nsm_label([]) :- put_string("[]"),!.
put_nsm_label(L) :- append_list(S,["\"",L,"\""]), put_string(S).


read_and_parse_nsm_sentences(_,_,end_of_file,T,T) :- !.
read_and_parse_nsm_sentences(_,_,64,T,T) :- 
	!,
	get0(C),
	do_command(C,_).
read_and_parse_nsm_sentences(Lang,Indent,C,T,T1) :-
	input_nsm_sentence(NewIndent,C,S,Label,C1),
	transcr(Lang,S,S1),	
	add_grouping(Indent,NewIndent,T,T2),
	parse_sentence(Lang,S1,P,_Rest),
	append(T2,[NewIndent+Label:P],T3),
	skip_newline(C1,C2),
	read_and_parse_nsm_sentences(Lang,NewIndent,C2,T3,T1).


read_and_parse_nsm_text(_,_,end_of_file,T) :- 
	!,
	append(T,[end],T1),
	write_nsm_parse(T1).
read_and_parse_nsm_text(_,_,64,T) :- 
	!,
	append(T,[end],T1),
	write_nsm_parse(T1),
	get0(C),
	do_command(C,_).
read_and_parse_nsm_text(Lang,Indent,C,T) :-
	input_nsm_sentence(NewIndent,C,S,Label,C1),
	transcr(Lang,S,S1),
	add_grouping(Indent,NewIndent,T,T2),
	parse_sentence(Lang,S1,P,_Rest),
	append(T2,[NewIndent+Label:P],T3),
	skip_newline(C1,C2),
	read_and_parse_nsm_text(Lang,NewIndent,C2,T3).

add_grouping(Indent,NewIndent,T,T1) :-
	NewIndent > Indent,
	append(T,[ct(group,begin)],T1).
add_grouping(Indent,NewIndent,T,T1) :-
	NewIndent < Indent,
	append(T,[ct(group,end)],T1).
add_grouping(Indent,Indent,T,T).
	

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
	get0(C),
	skipwhite(C,C2),
	input_nsm_rest_sent(C2,S,C1).
input_nsm_rest_sent(92,[32|S],C1) :-
	!,
	get0(C),
	skipwhite(C,C2),
	input_nsm_rest_sent(C2,S,C1).
input_nsm_rest_sent(10,[],10) :- !.
input_nsm_rest_sent(13,[],13) :- !.
input_nsm_rest_sent(32,[32|S],C2) :-
	!,
	get0(C1),
	skipwhite(C1,C3),
	input_nsm_rest_sent(C3,S,C2).
input_nsm_rest_sent(C,[C|S],C2) :-
	get0(C1),
	input_nsm_rest_sent(C1,S,C2).

input_label(end_of_file,[C,32],C,I,I) :- !.
input_label(C,[C,32],C,I,J) :- J is I + 1, !.
input_label(C,[C|Label],C1,OldIndent,Indent) :-
	get0(C2),
	NewIndent is OldIndent + 1,
	input_label(C2,Label,C1,NewIndent,Indent).

get_indent(C,OldIndent,Indent) :-
	get0(C2),
	get_indent_aux(C,C2,OldIndent,Indent).

get_indent_aux(C,32,OldIndent,Indent) :-
	get0(C1),
	NewIndent is OldIndent + 1,
	!,
	get_indent_aux(C,C1,NewIndent,Indent).
get_indent_aux(C,8,OldIndent,Indent) :-
	get0(C1),
	NewIndent is OldIndent + 4,
	!,
	get_indent_aux(C,C1,NewIndent,Indent).
get_indent_aux(C,C,Indent,Indent).




/* COMMANDS */

% @@
do_command(64,Mode) :-
	!,
	check_mode_and_put(64,Mode),
	get0(NewLook),
	parse_f(NewLook,Mode).

% @p (parse)
do_command(112,Mode) :-
	!,
	read_parse_write_sent,
	get0(Look),
	parse_f(Look,Mode).

% @P
do_command(80,_Mode) :-
	!,
	get_current_lang(L1),
	read_parse_write_nsm_text(L1).


% @g
do_command(103,Mode) :-
	!,
	read_gen_write_sent,
	get0(C),
	parse_f(C,Mode).

% @G
do_command(71,_Mode) :-
	!,
	read_gen_write_nsm_sent.


% @t
do_command(116,_Mode) :-
	!,
	get_current_lang(L1),
	get_current_l2(L2),
	read_and_translate_text(L1,L2).
% @T
do_command(84,_Mode) :-
	!,
	get_current_lang(L1),
	get_current_l2(L2),
	read_and_translate_nsm_text(L1,L2).


% @e (end)
do_command(101,Mode):-
	!,
	get0(C),
	parse_f(C,Mode).
	
% @l
do_command(108,Mode) :-
	!,
	get_current_lang(Lang),
	get0(Look),
	skipwhite(Look,L1),
	get0(L2),get0(L3),get0(L4),
	name(Lang1,[L1,L2,L3]),
	set_lang_mode(Lang,Lang1,Mode,NewMode),
	skipwhite(L4,L5),
	parse_f(L5,NewMode).

% @c
do_command(99,Mode) :-
	!,
	readcommands(Mode).

% @i
do_command(105,Mode) :-
	!,
	get_current_lang(L1),
	get_current_l2(L2),
	get_l_name(L1,L1a),
	get_l_name(L2,L2a),
	get0(C),
	skipwhite(C,C1),
	read_ident(Ident,C1),
	get0(C2),
	get_title(Ident,Title,Id),
	print_text(Title,L1a,L2a,Id),
	parse_f(C2,Mode).

% @UNKNOWN TAG
do_command(Code,_Mode) :-
	append("Note: Unknown tag @",[Code],X),
	put_message(X).

get_l_name(L1,L2) :-
	L1 =.. [L2,[]],!.
get_l_name(L,L).

read_ident([],10) :- !.
read_ident([],13) :- !.
read_ident([],32) :- !.
read_ident([],8) :- !.
read_ident([C|Ident],C) :-
	get0(C1),
	read_ident(Ident,C1).


print_text(Title,L1,_L2,Id) :-
	mtext(Title,Id,L1,Text),
	!,
	put_string(Text).
print_text(Title,_L1,L2,Id) :-
	mtext(Title,Id,L2,Text),
	!,
	put_string(Text).
print_text(_,_,_,_).
	


readcommands(Mode) :-
	read(Command),
	call_command(Command,Mode).

call_command(end_of_file,_) :- !.
call_command(end,Mode) :- !,get0(C),parse_f(C,Mode).
call_command(Command,Mode) :-
	!,
	call(Command),
	readcommands(Mode).

get_current_lang(Lang) :-
	current_lang(Lang),!.
get_current_lang(xxx).


get_current_l2(Lang) :-
	current_l2(Lang),!.
get_current_l2(xxx).

set_lang_mode(L1,L1,Mode,Mode) :- !.
set_lang_mode(L1,L2,Mode,Mode) :- 
	L1 =.. [L2,[]],!.
set_lang_mode(_,xxx,Mode,Mode) :- !.
set_lang_mode(_L1,_L2,_Mode,mode(skip)).



