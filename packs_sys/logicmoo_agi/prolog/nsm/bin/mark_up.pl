:- module(mark_up,[load_markup/0,
		   write_toc/2,
		   write_auto_text/2
		  ]).

:- use_module(utils).

:- dynamic(markup_text/3).
:- dynamic(escaped_char/3).

/** <module> Adds markup to output files

*/

/*
MARKUP IDENTIFIER: 
- STRINGA se lo voglio poter chiamare dal file con @f MARKUP-ID
- ATOMO se lo volgio chiamare solo internamente (markup automatico del
formato, tipo header rtf o latex)
- PREDICATO se lo volgio chiamare solo internamente (markup automatico del
formato, tipo header rtf o latex) e se devo mettere cose lang-specifiche
*/

%%	 load_markup is det
%
%	Loads the various markup commands for the
%       supported formats into the program database.
%       
%       config:supported_markup_list/1 contains a list
%       of the supported formats.
load_markup :-
	global_par:supported_markup_list(List),
	load_markup_list(List).

load_markup_list([]).
load_markup_list([Item|List]) :-
	name(Item,F),
	append_list(FileName,["./mark_up/",F,".pl"]),
	name(File,FileName),
	see(File),
	read(I),
	load_markup(Item,I),
	seen,
	load_markup_list(List).

load_markup(_,end_of_file) :- !.
load_markup(Format,F) :-
	assert_markup(Format,F),
	read(F1),
	load_markup(Format,F1).

/*
	markup_text(prologue(L1,L2),PROLOGUE) :-
	active_lang(L1,L2,Lang),
	get_lang_text_font(Lang,rtf,F0),
	get_lang_nsm_font(L1,rtf,F1),
	get_l2_nsm_font(L2,rtf,F2),
	append_list(PROLOGUE,[ 
"{\\rtf1\\ansi\\deff0\n\
{\\fonttbl\
{\\f0 ",
			      F0,
";}{\\f1 ",
			      F1,
";}{\\f2 ",
			      F2,
			      ";}}\n",
			      "{\\colortbl;\
\\red0\\green0\\blue0;\
\\red255\\green0\\blue0;\
\\red0\\green0\\blue255;}\
\n\
{\\stylesheet\n\
{\\s1\\f1\\cf3\\fs24 NSM1;}\
{\\s2\\f2\\cf1\\fs28 NSM2;}}\n"]).

*/

/*
assert_markup(Format,markup_text(A,B)) :-
	expand_markup_vars(Format,B,B1,Actions),
	!,
	assertz(mark_up:markup_text(Format,A,B1,Actions)).
*/

assert_markup(Format,markup_text(A,B)) :-
	!,
	assertz(mark_up:markup_text(Format,A,B)).
assert_markup(Format,escaped_char(A,B)) :-
	!,
	assertz(mark_up:escaped_char(Format,A,B)).
assert_markup(_,_).



%%	write_auto_text(+Markup,+Id) is det
%
%	Writes to the current output file a string stored in
%	markup_text(Markup,Id,String).
%	
%	This procedure is used by the NSM-file parser, to
%	produce output in some markup language. For example, HTML output needs
%	HTML tags to be inserted appropriately.
%	
write_auto_text(Markup,Id) :-
	markup_text(Markup,Id,Text),
	!,
	put_auto_text(Markup,Text).
write_auto_text(_,_).

put_auto_text(Markup,l:Texts) :-
	put_auto_texts(Markup,Texts).
put_auto_text(_Markup,Text) :-
	is_list(Text),
	!,
	put_string(Text).
put_auto_text(Markup,Text) :- % Text è un Id ricorsivo, come per i font rtf
	Text =.. [_|_],
	!,
	write_auto_text(Markup,Text).
	
put_auto_texts(_,[]).
put_auto_texts(Markup,[Text|TextList]) :-
	put_auto_text(Markup,Text),
	put_auto_texts(Markup,TextList).

indent_n(_,N,Spaces) :- utils:get_n_spaces(N,Spaces).

lang_text_font(_Lang,rtf,"Times").
lang_nsm_font(_Lang,rtf,"Aboriginal Serif").
l2_nsm_font(_Lang,rtf,"Aboriginal Sans").


get_lang_text_font(Lang,rtf,F0) :-
	lang_text_font(Lang,rtf,F0),!.
get_lang_text_font(_,rtf,"Times").

get_lang_nsm_font(Lang,rtf,F0) :-
	lang_nsm_font(Lang,rtf,F0),!.
get_lang_nsm_font(_,rtf,"Times").

get_l2_nsm_font(Lang,rtf,F0) :-
	l2_nsm_font(Lang,rtf,F0),!.
get_l2_nsm_font(_,rtf,"Courier").


%%	write_toc(+Format,+ListOfHeaders) is det
%
%	Writes a _|table of contents|_ in the specified format.
%	
write_toc(Format,B) :-
	reverse(B,B1),
	write_auto_text(Format,toc_prologue),
	write_toc_aux(Format,B1),
	write_auto_text(Format,toc_epilogue).

write_toc_aux(_,[]).

write_toc_aux(Format,[entry(Level,_Sec,_Text)|B]) :-
	[Level] > "3",
	!,
	write_toc_aux(Format,B).
	
write_toc_aux(Format,[entry(Level,Section,Text)|B]) :-
	write_auto_text(Format,pre_toc_entry([Level])),
	put_string(Section),
	write_auto_text(Format,infra_toc_entry),
	put_string(Text),
	write_auto_text(Format,post_toc_entry),
	write_toc_aux(Format,B).



