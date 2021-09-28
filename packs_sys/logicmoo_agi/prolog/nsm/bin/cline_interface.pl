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


:- module(cline_interface,[
			   run/0,
			   l/1,
			   l2/1,
			   b/1,
			   ldb/1,
			   sl1/1,
			   sl2/1,
			   sl/0,
			   sm/1,
			   smf/1,
			   stt/1,
			   pw/1,
		%	   gw/1,
			   ps/1,
			   pst/1,
			   pstf/1,
			   gs/1,
			   so/1,
			   ro/1,
			   co/0,
			   pf/1,
			   tm/1,
			   ts/1,
			   tg/1,
			   t/1,
			   s/2,
			   wl/1,
			   w/0,
			   c/0,
			   pg/0,
			   swtt/1
			  ]).

:- use_module(nsmdalia).
:- use_module(file_reader).
:- use_module(utils).
:- use_module(mark_up).
:- use_module(global_par).
:- use_module(transcr).
:- use_module(grammar).
:- use_module(gnu).

/** <module> NSM-DALIA, command line version

This module provides aliases to call the main NSM-DALIA predicates,
	(contained in modules nsmdalia.pl and file_reader.pl), and
	the running interface.

@tbd wl/0 implementation
@tbd re-implementation of pw
*/

%%	is_command(+Command) is nondet
%
%	This is a list of supported commands. User-introduced commands
%	are checked against this list. 
is_command(l(_)).
is_command(l2(_)).
is_command(ldb(_)).
is_command(b(_)).
is_command(sl1(_)).
is_command(sl2(_)).
is_command(sl).
is_command(sm(_)).
is_command(smf(_)).
is_command(stt(_)).
is_command(pw(_)).
% is_command(gw(_)).
is_command(ps(_)).
is_command(pst(_)).
is_command(pstf(_)).
is_command(gs(_)).
is_command(t(_)).
is_command(so(_)).
is_command(ro(_)).
is_command(co).
is_command(pf(_)).
is_command(tm(_)).
is_command(ts(_)).
is_command(tg(_)).
is_command(s(_)).
is_command(s(_,_)).
is_command(wl(_)).
is_command(w).
is_command(c).
is_command(pg).
is_command(swtt(_)).
is_command(end_of_file).
is_command(stop).
is_command(prolog).
is_command(halt).



/** l(+Lang) is det

Loads a language module into the program, and sets the
current_language flag to the loaded language. From now on, parse
commands will be taken to refer to that language as the parsing-from
language.

@see nsmdalia:load(Lang)

The "Lang" argument is either a language code (without quotation
marks), or a full language name (a PROLOG string, between double
quotation marks).

The "load" command works like this: if a compiled module is found in
the lang_bin directory, it will be loaded. If not, the directory
lang_src will be searched for the source files, compiled and
loaded. (The latter procedure is slower).

The user who is not also writing a grammar need only know which
languages are available, and what their code is (the "list" command
displays all the installed languages with their code). As the only
language available in NSM-DALIA v. 0.8 is English (code: eng), you can
load the English grammar with:

==
DALIA> l(eng).
==
or:

==
DALIA> l("English").
==

A note for gramamr writers: please remember that, once a grammar is
saved in the lang_bin directory ("s" command), l(Lang) will read from
that directory, NOT from the source file. If, after you modified your
source file, and you are wondering why on earth that sentence still do
not parse, though you have fixed the bug in your grammar -- that's
because you have loaded the file with "l", not with "b", so the
program does not read the new grammar, but the old compiled one! When
you are developing a grammar, better not to compile it ("c" command")
until you are finished. (However, see "b" command below).
*/
l(Lang) :- load(Lang).

/** l2(+Lang) is det

This command loads a grammar module, just like l(Lang), with
the difference that the language loaded is set as *l2* (_|second
language|_). Translation commands like t(Sentence) will translate into 
l2.
*/
l2(Lang) :- load_l2(Lang).

%% ldb(+FileName:string) is det
%
%	Loads a text database. Text is stored in a series of mtext/3 
%	facts (see nsmfiles.txt for details).
%	
ldb(FileName) :- load_text_database(FileName).

/** b(+Lang) is det

Full form: build(+Lang). 
Loads a language module into the program, and sets the
current_language flag to the loaded language. Files are searched for
exclusively in the lang_src directory. Useful for grammar writers. The
"Lang" argument is a language code or a string (full language name).
*/
b(Lang) :- build(Lang).

%%	sl1(+Lang) is det
%
%	Sets Lang as the current language.
%	
sl1(Lang) :- set_current_lang(Lang).


/** sl2(+Lang) is det

Full form: set_current_l2(Lang). 

Used in automatic translation. Once current_lang and current_l2 are
set, the "translation" commands translate NSM sentences from
current_lang into current_l2. Lang = language code.
*/
sl2(Lang) :- set_current_l2(Lang).

/** sl

Full form: switch_languages. 

Useful to switch between various languages when you have loaded more
than one grammar. The "Lang" argument is a language code.
*/
sl :- switch_languages.


%%	sm(+MarkUp) is det
%
%	Full form: set_markup(+MarkUp).
%
%	MarkUp is a term referring to one of the supported markup 
%	schemes (see NSM-files.txt and grammars.txt).
sm(M) :- set_markup(M).


%%	smf(+Format) is det
%
%	Format is one of the terms =|line_by_line|= or =|whole_text|=.
%	The NSM-file parser has an option for translating an NSM text
%	and displaying the translation together with the original.
%	
%	Note that, for example, the rtf markup scheme requires
%	a =|line_by_line|= option which, however, result in the
%	texts being displayed in a two_column table, one
%	column for the original, the other for the translation, and
%	with corresponding lines aligned.
%	
smf(F) :- set_double_text_format(F).


%%	stt(+TableId:integer) is det
%
%	For languages having more than one transcription possibility, 
%	sets the transcription table number TableId as active.
stt(TableId) :- set_transcr_table(TableId).


swtt(TableId) :- set_wiki_transcr_table(TableId).

/** pw(+W) is det

Full form: parse_and_write_word(W). 

You won't need this very much, because "ps" can parse single words, as
well as whole sentences. When you are developing a grammar, however,
this command can be useful, because it also outputs a morpheme
split-up of the word.  The argument W is a double-quoted string, which
is analyzed as a word of the last current language set. Ex.

==
DALIA> pw("things").
==

NSM-DALIA answers:

==
Morphemic String: thing-s
ct(n(n), sp(e, e, plur(e), [], something(thing))).
==
*/
pw(W) :- parse_and_write_word(W).


% gw(W) :- gen_and_write_word(W).

/** ps(+S) is det

Full form: parse_and_write_sentence(S). 
Like "pw", but it parses a whole sentence. Ex.

==
DALIA> ps("something good is happening now").
==

NSM-DALIA answers with the corresponding NSM-PROLOG formula:

==
ct(s, s(e, time(e, now, e), e, e, e, 
	p(i(happen), 
	  [o:sp(e, e, sing(e),[good], something(thing)), 
	   d:e]), 
	e, e)).
==
*/
ps(S) :- parse_and_write_sentence(S).



/** pst(+S) is det

Full form: parse_and_write_tabular_sentence(S,min). 
Like "ps", but delivers the analysis in tabular fashion.

Prints only the "minimal" table.

==
DALIA> ps("I do something good")
==

NSM-DALIA answers with the corresponding NSM-PROLOG formula:

==
s: _____ 
. pred: _____ 
. . v:do
. . a: _____ 
. . . pers:loc(me)
. . . n:me
. . o: _____ 
. . . a: _____ 
. . . . eval: _____ 
. . . . . a:good
. . . n:something(thing)
==
*/
pst(S) :- parse_and_write_sentence_tabular(S,minimal).



/** pstf(+S) is det

Full form: parse_and_write_tabular_sentence(S,full). 
Like "ps", but delivers the analysis in tabular fashion.

Prints only the full table.

==
DALIA> ps("I do something good")
==

NSM-DALIA answers with the corresponding NSM-PROLOG formula:

==
s: _____ 
. c: _____ 
. . compl:e
. . top:e
. . int:e
. . top2:e
. . foc:e
. . pol:e
. mod: _____ 
. . speech_act:e
. . eval:e
. . evid:e
. . epist:e
. f: _____ 
. . top3:e
. . finite:e
. t: _____ 
. . past:e
. . fut:e
. m: _____ 
. . irrealis:e
. . necess:e
. . possib:e
. . vol:e
. . oblig:e
. . allow:e
. asp: _____ 
. . hab:e
. . rep:e
. . freq:e
. . celer:e
. . ant:e
. . term:e
. . cont:e
. . perf:e
. . retro:e
. . pross:e
. . dur:e
. . prog:e
. . prosp:e
. . compl_sg:e
. . compl_pl:e
. vo: _____ 
. . v_1:e
. . v_2:e
. . v_3:e
. ak: _____ 
. . celer:e
. . comp:e
. . rep:e
. . freq:e
. pred: _____ 
. . v:do
. . a: _____ 
. . . det:e
. . . alt:e
. . . q:e
. . . pers:loc(me)
. . . a: _____ 
. . . . eval:e
. . . . size:e
. . . . length:e
. . . . height:e
. . . . speed:e
. . . . width:e
. . . . weight:e
. . . . temp:
. . . . age:e
. . . . shape:e
. . . . colour:e
. . . . origin:e
. . . . material:e
. . . dem:e
. . . poss:e
. . . class:e
. . . n:me
. . o: _____ 
. . . det:e
. . . alt:e
. . . q:e
. . . pers:e
. . . a: _____ 
. . . . eval: _____ 
. . . . . int:e
. . . . . a:good
. . . . size:e
. . . . length:e
. . . . height:e
. . . . speed:e
. . . . width:e
. . . . weight:e
. . . . temp:e
. . . . age:e
. . . . shape:e
. . . . colour:e
. . . . origin:e
. . . . material:e
. . . dem:e
. . . poss:e
. . . class:e
. . . n:something(thing)
. . d:e
. . e:e
. . c:e
. . i:e
. . b:e
. . l:e
. . m:e
==
*/
pstf(S) :- parse_and_write_sentence_tabular(S,full).






/** gs(+LF) is det

Full form: gen_and_write_sentence(LF). 
The "LF" argument is an NSM-PROLOG formula; the output is the
corresponding sentence in the current language (or the formula itself,
if NSM-DALIA fails generation). Ex.

==
DALIA> gs(ct(s, s(e, before(now), e, e, e, 
		  p(do, [a:sp(e, e, sing(e), [], me), 
			 o:sp(e, e, sing(e), [good], 
			      something(thing)), 
			 d:e, c:e, i:e]), 
		  e, e))).
==

Answer:

==
I did something good
==
*/
gs(LF) :- gen_and_write_sentence(LF).


/** t(+Sentence) is det

Full form: translate_and_write_sentence(Sentence). 
Translates a sentence from current_language to current_l2. Sentence
argument is a string, between double quotation marks.
*/
t(S) :- translate_and_write_sentence(S).


/** so(+Filename) is det

Full form: set_output_file(Filename). 
Redirects the output to the file named Filename (in some operating
systems, you will need to give a full name with extension -- NSM-DALIA
adds none). "Filename" is double-quoted string. Ex.

==
DALIA> so("pippo.txt").
 ** Output file set to pippo.txt
==
 
You will perhaps use this mostly before a "pf" command.
*/
so(F) :- set_output_file(F).

%%	ro(+F) is det
%
%	Used to select an output file if the user wants it to be 
%	overwritten by the new data. so/1 _appends_ the output
%	file if it exists, and does not rewrite it, as ro/1 does.
%	
ro(F) :- rewrite_output_file(F).

%%	co
%
%	Full form: close_output_file
%	
%	Users will rarely use this command, because the output
%	file is automatically closed (and the output redirected again
%	to the console) when the parsing of an NSM-input file 
%	is finished.
%	
co :- close_output_file.


/** pf(+F) is det

Full form: parse_file(F). 
Parses and outputs a text file with particular tags (see the
documentation file "NSM-files.txt"). You can write a whole file
of NSM texts in, say, English NSM, and then have it parsed or
translated automatically. This will probably be one of the most used
commands, when other language modules are available.
*/
pf(F) :- parse_file(F).

tpt(TokiPonaText) :- tp_parse_text(TokiPonaText).


/** tm(+SWITCH) is det

Full form: trace_morphology(SWITCH).
To switch on verbose mode for morphology parsing, say:

==
tm(1).
==

To switch it off, say 

==
tm(0)
==

Verbose modes are useful in grammar
development. Turning tracing morphology on, the "parse" and "generate"
commands will display information about the grammar rules used during
the parsing process, in the morphophonemic component of the
grammar. This will help you to find out why the morphophonemic
component of your grammar is not doing what you intended it to do.
*/
tm(SWITCH) :- trace_morphology(SWITCH).


/** ts(+SWITCH) is det

Full form: trace_syntax(SWITCH). 
"SWITCH" is either "1" or "0". Turns on or off syntax verbose mode for
parsing.
*/
ts(SWITCH) :- trace_syntax(SWITCH).


/** tg(+SWITCH) is det

Full form: trace_generation(SWITCH).
"SWITCH" is either 0 or 1. Turns on/of verbose mode for generation.
*/
tg(SWITCH) :- trace_generation(SWITCH).


/** s(+L) is det

Full form: save_lang(L). 
Used by grammar writers. NSM-DALIA grammars can manage various related
language at once, in just one grammar. Because of this, grammar rules
and lexical items are elaborated and compiled, and, especially when a
grammar and its lexicon are big, there is no meaning in repeating this
amount of elaboration each time a language is loaded. Therefore, from
a single grammar you can load various languages or dialects. When the
grammar is finished, these langauges can be compiled separately for
fast loading.

s/1 prompts for the language full name then calls s/2.
*/
s(L) :- save_lang(L).

/** s(+L,+FullName) is det

See s/1 
*/
s(L,Name) :- save_lang(L,Name).

%%	wl(+Lang) is det
%
%	Full form: word_list(+Lang).
%	
%	Saves a formatted version of the lexical database (stored
%	with m/4 predicates), in the current active markup format.
%	
wl(L) :-
	build_full_name(L,L1),
	!,
	word_list(L1).

/** w is det

GNU-ish command, to display the "WARRANTY" part of the GNU GPL.
*/
w :- gnu_no_warranty(W), put_string(W).

/** c is det

GNU-ish command, a pointer to the  "CONDITION" part of the GNU GPL.
*/
c :- gnu_conditions(C), put_string(C).

/** pg is det

Prints a formatted version of the grammar.
*/
pg :- gdoc:print_grammar.


end_of_file :- halt.

write_gnu :-
	gpl("DALIA","1.0","2009","Francesco Zamblera",S),
	put_string(S),
	nl.

write_prompt :-
	nl,write('DALIA> ').


perform(stop) :-
	!,
	write('Done. Returning to SWI-PROLOG'),nl,nl.
perform(Command) :-
	is_command(Command),
	!,
	call(Command),
	loop.
perform(Command) :-
	call(macro(Command)),
	loop.
perform(_Command) :-
	put_message("Unknown command"),
	loop.



init :-
	init_tracing,
%	read_macro,
	mark_up:load_markup,
	asserta(global_par:makedict(no)),
	write_gnu.

%%	loop is nondet
%       A typical command line interface, prompting the user for
%       a command, reading and executing the command.
loop :- 
	write_prompt,
	read(Command),
	perform(Command).

%%	run is det.
%
%       Procedure run calls init/0, then enters the main loop.
%       Fails on bugs
%       
run :- init,loop.

