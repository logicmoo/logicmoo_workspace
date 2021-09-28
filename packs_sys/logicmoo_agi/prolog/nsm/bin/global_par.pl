:- module(global_par,[
		      set_current_lang/1,
		      set_current_l2/1,
		      set_markup/1,
		      set_double_text_format/1,
		      set_output_file/1,
		      rewrite_output_file/1,
		      switch_output_file/0,
		      close_output_file/0,
		      set_text_id_format/1,
		      set_id_format/1,
		      init_tracing/0,
		      assert_new_available_lang/2,
		      current_markup/1,
		      current_double_format/1,
		      build_full_name/2,
		      switch_languages/0,
		      get_current_lang/1,
		      get_current_l2/1,
		      get_synt_grammar_type/2
		  ]).

:- use_module(config).

:- use_module(utils).
:- use_module(messages).

:- dynamic(current_lang/1).
:- dynamic(current_l2/1).
:- dynamic(current_output_file/1).
:- dynamic(mtext/3).
:- dynamic(markup_text/3).

/** <module> Global parameters

This module contains some dynamic predicates which define various
options for the running system.
*/

%%	current_markup(+Markup)
%
%	The currently active markup scheme for NSM-input file parsing
%	and for grammar formatted files.
:- dynamic(current_markup/1).
:- dynamic(current_double_format/1).
:- dynamic(text_id_format_number/1).
:- dynamic(trace_morph/1).
:- dynamic(trace_synt/1).
:- dynamic(trace_gen/1).
:- dynamic(tracing_mode/1).
:- dynamic(available_language/2).
:- dynamic(makedict/1).


%%	set_current_lang(+Lang) is det
%
%	
set_current_lang(L) :-
	build_full_name(L,L1),
	retract(current_lang(_)),
	!,
	asserta(current_lang(L1)),
	notify_set_lang(L,[],"L1").
set_current_lang(L) :-
	build_full_name(L,L1),
	asserta(current_lang(L1)),
	notify_set_lang(L,[],"L1").


%%	set_current_l2(+Lang) is det
%
%	Full form of cline_interface:sl2/1. Sets a language
%	(whose grammar must already have been loaded) as the
%	current L2. From now on, every _translate_ command will
%	be interpreted as _|translate into this language|_.
%	
set_current_l2(Lang) :-
	retract(current_l2(_)),
	!,
	build_full_name(Lang,L),
	asserta(current_l2(L)),
	notify_set_lang(L,Lang,"L2").
set_current_l2(Lang) :-
	build_full_name(Lang,L),	
	asserta(current_l2(L)),
	notify_set_lang(L,Lang,"L2").	

%%	switch_languages
%
%	Switches the current L1 with the current L2.
%	
switch_languages :-
	current_lang(L1),
	current_l2(L2),
	!,
	set_current_lang(L2),
	set_current_l2(L1).
switch_languages :-
	put_message("L1 or L2 were not set.").


%%	set_markup(+Markup) is det
%
%	Sets the currently active markup scheme to Markup.
set_markup(Markup) :-
	supported_markup_list(L),
	member(Markup,L),
	!,
	retractall(current_markup(_)),
	asserta(current_markup(Markup)),
	notify_markup(Markup).
set_markup(Markup) :-
	notify_unknown_markup(Markup).

%%	set_double_text_format(+Format) is det
%
%	Full form of smf2/1.
%	
%	Retracts the previous _|double_text_format|_, if any,
%	then asserts the new current one.
%	
set_double_text_format(Format) :-
	member(Format,[1,2]),
	!,
	retractall(current_double_format(_)),
	member(Format:Type,[0:whole_text,1:line_by_line]),
	asserta(current_double_format(Type)),
	notify_double_format(Type).
set_double_text_format(_) :-
	put_message("Possible values:\n 0 (for whole_text format) \n\
	1 (for line_by_line format)").

%%	set_output_file(+FileName:string) is det
%
%	Full form of: cline_interface:so/1.
%	
set_output_file(FileName) :-
	retractall(global_par:current_output_file(_)),
	!,
	asserta(current_output_file(FileName)),
	append("Output file set to ",FileName,S),
	put_message(S).

% ?
set_output_file(FileName) :-
	asserta(current_output_file(FileName)),
	append("Output file set to ",FileName,S),
	put_message(S).


rewrite_output_file(FileName) :-
	retractall(current_output_file(_)),
	!,
	asserta(current_output_file(rewrite(FileName))),
	append_list(S,["Output file set to ",FileName,". WARNING: File will be overwritten"]),
	put_message(S).

rewrite_output_file(FileName) :-
	asserta(current_output_file(rewrite(FileName))),
	append_list(S,["Output file set to ",FileName,". WARNING: File will be overwritten"]),
	put_message(S).


switch_output_file :-
	current_output_file(rewrite(FileName)),
	name(File,FileName),
	!,
	tell(File).
switch_output_file :-
	current_output_file(FileName),
	name(File,FileName),
	!,
	append(File).
switch_output_file.


close_output_file :-
	current_output_file(rewrite(F)),
	!,
	told,
	retractall(current_output_file(_Filename)),
	append_list(S,["Output written to file \"",F,"\"; output file closed"]),
	put_message(S),
	nl.
close_output_file :-
	current_output_file(F),
	!,
	told,
	retractall(global_par:current_output_file(_Filename)),
	append_list(S,["Output written to file \"",F,"\"; output file closed"]),
	put_message(S),
	nl.
close_output_file.


set_text_id_format(N) :-
	retractall(text_id_format_number(_)),
	asserta(text_id_format_number(N)).


set_id_format(Num) :-
	retractall(global_par:text_id_format_number(_)),
	asserta(global_par:text_id_format_number(Num)),
	notify_id_format.

init_tracing :-
	retractall(trace_morph(_)),
	retractall(trace_synt(_)),
	retractall(trace_gen(_)),
	retractall(tracing_mode(_)),
	asserta(current_double_format(line_by_line)),
	asserta(current_markup(txt)),
	asserta(grammar:text_font(_,rtf,"Aboriginal Serif")),
	asserta(grammar:nsm_font(_,rtf,"Aboriginal Sans")),
	asserta(trace_morph(0)),
	asserta(trace_synt(0)),
	asserta(trace_gen(0)),
	asserta(tracing_mode(0)).

assert_new_available_lang(Lang,NAME) :-
	assertz(available_language(Lang,NAME)).

current_markup(txt).
current_double_format(line_by_line).


get_current_lang(Lang) :-
	global_par:current_lang(Lang),!.
get_current_lang(xxx).

get_current_l2(Lang) :-
	global_par:current_l2(Lang),!.
get_current_l2(Lang) :-
	global_par:current_lang(Lang),!.
get_current_l2(xxx).


get_synt_grammar_type(Lang,Grammar) :-
	grammar:synt_grammar_type(Lang,Grammar),!.
get_synt_grammar_type(_Lang,dependency).


build_full_name(L:D,L:D) :- !.
build_full_name(L,L:e).

