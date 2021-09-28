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


:- include('main.pl').


l(Lang) :- load(Lang).

l2(Lang) :- load_l2(Lang).

b(Lang) :- build(Lang).

sl1(Lang) :- set_current_lang(Lang).

sl2(Lang) :- set_current_l2(Lang).

sl :- switch_languages.

pw(W) :- parse_and_write_word(W).

gw(W) :- gen_and_write_word(W).

ps(S) :- parse_and_write_sentence(S).

gs(LF) :- gen_and_write_sentence(LF).

t(S) :- translate_and_write_sentence(S).

so(F) :- set_output_file(F).

co :- close_output_file.

pf(F) :- parse_file(F).

tpt(TokiPonaText) :- tp_parse_text(TokiPonaText).

tm(SWITCH) :- trace_morphology(SWITCH).

ts(SWITCH) :- trace_syntax(SWITCH).

tg(SWITCH) :- trace_generation(SWITCH).

s(L) :- save_lang(L).

s(L,Name) :- save_lang(L,Name).

wl(L) :-
	build_full_name(L,L1),
	!,
	word_list(L1).


w :- gnu_no_warranty(W), put_string(W).

c :- gnu_conditions(C), put_string(C).

write_gnu :-
	gpl("DALIA","0.8","2009","Francesco Zamblera",S),
	put_string(S),
	nl.

write_prompt :-
	nl,write('DALIA> ').

put_message(S) :-
	put_string(" ** "),
	put_string(S),nl.

put_message_var(S) :-
	put_string(" ** "),
	put_msg_string(S),nl.

put_term(T) :-
	write(T),write('.'),nl.

put_dep_rule(Deg,Dep,C) :-
	write('Rule: ('),write(Deg),write(') '),
	write(Dep),write('cond: '),write(C),nl,nl.

put_dep_gen_rule(Deg,A+B==>D,_C) :-
	write('Rule: ('),write(Deg),write(') '),
	write(D),write(' ==> '),nl,tab(8),
	write(A),
	nl,
	tab(5),
	write(' + '),
	write(B),nl,nl.
put_dep_gen_rule(Deg,A+B+E==>D,_C) :-
	write('Rule: ('),write(Deg),write(') '),
	write(D),write(' ==> '),nl,tab(8),
	write(A),
	nl,
	tab(5),
	write(' + '),
	write(B),
	nl,
	tab(5),
	write(' + '),
	write(E),nl,nl.

	

perform(stop) :-
	!,
	write('Done. Returning to SWI-PROLOG'),nl,nl.
perform(Command) :-
	call(Command),!,loop.
perform(Command) :-
	name(Command,C),
	append(C,": ** ERROR ** ",S),
	put_message(S),
	loop.


put_label(Label) :-
	put_string(Label).
nsm_indent(Label,Indent) :-
	length(Label,Indent1),
	NewIndent is Indent + Indent1,
	tab(NewIndent).

init :-
	asserta(trace_morph(0)),
	asserta(trace_synt(0)),
	asserta(trace_gen(0)),
	asserta(tracing_mode(0)),
	write_gnu.

loop :- 
	write_prompt,
	read(Command),
	perform(Command).

run :- init,loop.

