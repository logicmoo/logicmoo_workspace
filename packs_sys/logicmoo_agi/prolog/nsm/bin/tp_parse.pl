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


/*

:- op(500,xfx,:).
:- op(500,fx,'U').

:- op(700,xfx,::).
:- op(550,xfx,===).
:- op(550,xfy,>>).
:- op(550,xfx,<<).
:- op(570,xfx,//).
:- op(500,xfx,=>).
:- op(400,xf,'?').
:- op(700,xfx,'PAT').
:- op(700,xfx,'LX').
:- op(700,xfx,'RG').
:- op(700,xfx,'PF').
:- op(700,xfx,'TR').



:- dynamic(paradigm/2).
:- dynamic(construction/4).
:- dynamic('PF'/2).
:- dynamic('RG'/2).
:- dynamic(subdialect/3).
:- dynamic(prefix_language/2).
:- dynamic(phonetic_class/3).
:- dynamic(current_output_file/1).
:- dynamic(current_input_file/1).
*/

% COMMANDS
% 
l(Lang) :- load_lang(Lang).


pt(S) :-
	scan(S,_,Sent),
	parse_text(Sent,Struct),
	display_analysis(Struct).

pt(S,Struct) :-
	scan(S,_,Sent),	
	parse_text(Sent,Struct).

t(Lang,S) :-
	scan(S,_,TP_PF),
	parse_text(TP_PF,TP_LF),
	gen_text(Lang,TP_LF,Lex),
	transcribe_sent(Lex,PF1),
	lexToSurfString(Lang,PF1,PF),
	put_string(PF).

t(Lang,S,PF) :-
	scan(S,_,TP_PF),
	parse_text(TP_PF,TP_LF),
	gen_text(Lang,TP_LF,Lex),
	transcribe_sent(Lex,PF1),
	lexToSurfString(Lang,PF1,PF).

so(F) :- set_output_file(F).

si(F) :- set_input_file(F).

p(F) :- parse_file(F).

p :- current_input_file(F), parse_file(F).





set_output_file(FileName) :-
	name(File,FileName),
	set_o_f(File).
set_input_file(FileName) :-
	name(File,FileName),
	set_i_f(File).







% NSM-P.PL


lexToSurf(_Lang,WP,WP) :-
	\+member('=',WP).
lexToSurf(Lang,WP,String) :-
	member('=',WP),
	!,
	lexToSurfwp(Lang,_Layer,WP,String).

lexToSurfwp(Lang,Layer,WP,String) :-
	member('=',WP),
	get_direction_parameter(Lang,Dir),
	splitPrefOrSuff(Dir,WP,'=',Pref,Suff),
	lexToSurf(Lang,Pref,Pref1),
	lexToSurf(Lang,Suff,Suff1),
	Lang 'PF' pattern(Type,NewLayer,Scheme,p(Pref1,Suff1),Cond),
	checkLayer(Layer,NewLayer,gen),
	check(Lang,Cond),
	transform_pattern(Type,Scheme,NewString),
	lexToSurfwp(Lang,Layer,NewString,String).
lexToSurfwp(_Lang,_Layer,String,String).

transform_pattern('<',A+B,C) :-
	append(A,B,C).
transform_pattern('>',A+B,C) :-
	append(A,B,C).
transform_pattern('<>',A+B+C,D) :-
	append(A,B,C1),
	append(C1,C,D).
transform_pattern('=',A,A).


get_direction_parameter(Lang,'<') :-
	prefix_language(Lang,0),!.
get_direction_parameter(_Lang,'>').
	

splitPrefOrSuff('>',WP,'=',Pref,Suff) :-
	splitList(WP,'=',Pref,Suff).
splitPrefOrSuff('<',WP,'=',Pref,Suff) :-
	splitFromBack(WP,'=',Pref,Suff).


checkLayer(Layer,Layer,_) :- !.
checkLayer(Layer,NewLayer,Direction) :-
	Layer =.. [System1,Lev1],
	NewLayer =.. [System2,Lev2],
	checkLayer2(System1,Lev1,System2,Lev2,Direction).

checkLayer2(S1,_L1,S2,_L2,_Dir) :-
	S1 \= S2,!.
checkLayer2(S,L1,S,L2,parse) :-
	L1 > L2.
checkLayer2(S,L1,S,L2,gen) :-
	L1 < L2.


check(_,[]).
check(Lang,[suff(Suff,X)|Condition]) :-
	!,
        append(_,Suff,X),
        check(Lang,Condition).
check(Lang,[pref(Pref,X)|Condition]) :-
	!,
        append(Pref,_,X),
        check(Lang,Condition).
check(Lang,[(Segm << Class)|Condition]) :- 
	!,
        phonetic_class(Lang,Class,Members),
	member(Segm,Members),
	check(Lang,Condition).
check(Lang,[Cond|Condition]) :-
        Cond,
        !,
        check(Lang,Condition).


















% STRUTILS.PL
% 



splitFromBack(L1,L,Before,After) :-
	reverse(L1,L1b),
	member(L,L1b,B1,A1),
	reverse(A1,After),
	reverse(B1,Before).

last(List,Item) :-
        append(_,[Item],List).

sublist([],L2,[],L2).
sublist([C|L1],L2,Before,After) :-
	member(C,L2,Before,NewL2),
	sublist_aux(L1,NewL2,After).

member(C,[C|L2],[],L2).
member(C,[C1|L2],[C1|Before],NewL2) :-
	member(C,L2,Before,NewL2).

sublist_aux([],L2,L2).
sublist_aux([C|L1],[C|L2],NewL2) :-
	sublist_aux(L1,L2,NewL2).











% output


transcribe(Lang,Sent,NewSent) :-
	Lang :: transcr(Table),
	!,
	transcr_sent(Table,Sent,NewSent).
transcribe(_Lang,C,C).

transcr_sent(Table,S,S1) :-
	transcr_aux(Table,S,S2),
	name(S1,S2).

transcr_aux(_Table,[],[]).
transcr_aux(Table,[C|S],[C1,S1]) :-
	member(C::C1,Table),
	transcr_aux(Table,S,S1).

put_string([]).
put_string([C|String]) :-
	put(C),
	put_string(String).


display_analysis([S|Rest]) :-
	display_analysis(S),
	display_analysis(Rest).
display_analysis([]).
display_analysis(S) :-
	not(is_list(S)),
	write('Sentence: '),nl,
	display_struct(S).

display_struct(s(A,B,C,D,Pred)) :-
	tab(4),write('Interrogative-Negative: '),write_e(A),nl,
	tab(4),write('Time: '),write_e(B),nl,
	tab(4),write('Modality: '),write_e(C),nl,
	tab(4),write('Duration: '),write_e(D),nl,
	tab(4),write('Predication:'),nl,
	tab(7),write(Pred),nl,nl.
display_struct(taso(S)) :-
	tab(2),write('Adversative: taso'),
	display_struct(S).
display_struct(when(S1,S2)) :-
	tab(2),write('When-clause: '),
	display_struct(S1),
	tab(2),write('Main clause: '),
	display_struct(S2).
display_struct(if(S1,S2)) :-
	tab(2),write('If-clause: '),
	display_struct(S1),
	tab(2),write('Main clause: '),
	display_struct(S2).
	

write_e(e) :- !, write('-').
write_e(A) :- write(A).
	
	
	

/*
emitList(L) :-
	emitList(L,1).

emitList([' ',',',' '|L],N) :-
	N > 59,
	!,
	write(','),
	nl,
	tab(8),
	emitList(L,9).
emitList([' '|L],N) :-
	N > 59,
	!,
	nl,
	tab(8),
	emitList(L,9).
emitList(['\n'|L],_) :-
	!,nl,
	emitList(L,1).
emitList([C|L],N) :-
	write(C),
	M is N + 1,
	emitList(L,M).
emitList([],_).
*/

punctuation('.').
punctuation(',').
punctuation(':').
punctuation(';').
punctuation('?').
punctuation('!').
punctuation('§').






% FILE UTILS

set_o_f(File) :-
	retract(current_output_file(_)),!,
	asserta(current_output_file(File)).
set_o_f(File) :-
	asserta(current_output_file(File)).

set_i_f(File) :-
	retract(current_input_file(_)),!,
	asserta(current_input_file(File)).
set_i_f(File) :-
	asserta(current_input_file(File)).

switch_output_file :-
	current_output_file(File),
	!,
	tell(File).
switch_output_file.

switch_input_file :-
	current_input_file(File),
	!,
	see(File).
switch_input_file.


close_output_file :-
	current_output_file(_),
	!,
	told.
close_output_file.

close_input_file :-
	current_output_file(_),
	!,
	seen.
close_input_file.




skipwhite(32,C) :- !,get0(C1), skipwhite(C1,C).
skipwhite(13,C) :- !,get0(C1), skipwhite(C1,C).
skipwhite(10,C) :- !,get0(C1), skipwhite(C1,C).
skipwhite(8,C) :- !,get0(C1), skipwhite(C1,C).
skipwhite(C,C).







% PROCESS FILE

parse_file(F) :-
	set_input_file(F),
	switch_input_file,
	switch_output_file,
	get0(C),
	parse_f_init(C,mode(echo)),
	close_input_file,
	close_output_file,
	!.

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

% @@
do_command(64,Mode) :-
	check_mode_and_put(64,Mode),
	get0(NewLook),
	parse_f(NewLook,Mode).

% @t
do_command(102,Mode) :-
	get_current_lang(Lang),
	get0(C),
	transl_file(Lang,C),
	get0(C1),
	parse_f(C1,Mode).

% @l
do_command(108,Mode) :-
	get_current_lang(Lang),
	get0(Look),
	skip_white(Look,L1),
	get0(L2),get0(L3),get0(L4),
	name(Lang1,[L1,L2,L3]),
	set_lang_mode(Lang,Lang1,Mode,NewMode),
	skip_white(L4,L5),
	parse_f(L5,NewMode).

% @c
do_command(99,Mode) :-
	readcommands(Mode).

% @i
do_command(105,Mode) :-
	get_current_lang(L1),
	get_current_aux_lang(L2),
	get_l_name(L1,L1a),
	get_l_name(L2,L2a),
	get0(C),
	skip_white(C,C1),
	read_ident(Id,C1),
	get0(C2),
	print_text(L1a,L2a,Id),
	parse_f(C2,Mode).

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


print_text(L1,_L2,Id) :-
	mtext(Id,L1,Text),
	!,
	put_string(Text).
print_text(_L1,L2,Id) :-
	mtext(Id,L2,Text),
	!,
	put_string(Text).
print_text(_,_,_).
	


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


get_current_aux_lang(Lang) :-
	current_aux_lang(Lang),!.
get_current_aux_lang(xxx).

get_current_third_lang(Lang) :-
	current_third_lang(Lang),!.
get_current_third_lang(xxx).

set_lang_mode(L1,L1,Mode,Mode) :- !.
set_lang_mode(L1,L2,Mode,Mode) :- 
	L1 =.. [L2,[]],!.
set_lang_mode(_,xxx,Mode,Mode) :- !.
set_lang_mode(_L1,_L2,_Mode,mode(skip)).




transl_file(C,Lang) :-
	skipwhite(C,C1),
	transl_file(C1,Lang,Text),
	text_cohesion(Text,Text1),
	transl_text(Lang,Text1).

transl_file(@,_,[]) :- !.
transl_file(end_of_file,_,[]) :- !.
transl_file(C,Lang,[S|Text]) :-
	input_sentence(C,S1),
	scan(S1,_,Tokens),
	parse_s(Tokens,S,_),
	get0(C1),
	skipwhite(C1,C2),
	transl_file(C2,Lang,Text).


input_sentence(46,[]) :- !.
input_sentence(C,[C|S]) :-
	get0(C1),
	input_sentence(C1,S).

transl_text(Lang,Text) :-
	gen_text(Lang,Text,Lex),
	transcribe_sent(Lex,PF1),
	lexToSurfString(Lang,PF1,PF),
	put_string(PF).







/*
inst_vars(_,[]).
inst_vars(Lang,['U' Var|Vars]) :-
	!,
	call(Var),
	inst_vars(Lang,Vars).
inst_vars(Lang,[Var|Vars]) :-
	call(paradigm(Lang,Var)),!,
	inst_vars(Lang,Vars).


*/



transcribe_sent(Lex,PF) :-
	transcr(Lex,[],PF1),
	attach_morphemes(PF1,PF2),
	append(PF2,[' '],PF).

transcr([W|Lex],SoFar,PF) :-
	name(W,X),
	append(SoFar,[' '],X1),
	append(X1,X,X2),
	transcr(Lex,X2,PF).
transcr([],PF,PF).



attach_morphemes([],[]).
attach_morphemes([' ','='|S],['=',S1]) :-
	!,attach_morphemes(S,S1).
attach_morphemes(['=',' '|S],['=',S1]) :-
	!,attach_morphemes(S,S1).
attach_morphemes([M|S],[M|S1]) :-
	attach_morphemes(S,S1).







lexToSurfString(Lang,Lex,Surf) :-
   lexToSurfString(Lang,Lex,[],Surf1),
   cleanSpaces(Surf1,Surf).




lexToSurfString(_Lang,[],Surf,Surf) :- !.
lexToSurfString(Lang,Lex,Surf,NewSurf) :-
	lexToSurfWord(Lang,Lex,Rest,Word1),
	append(Word1,[' '],Word),
	append(Surf,Word,Surf1),
	lexToSurfString(Lang,Rest,Surf1,NewSurf).

lexToSurfWord(_,[],[],[]) :-!.
lexToSurfWord(_,[' '],[],[]) :- !.
lexToSurfWord(Lang,String,Rest,PF) :-
	      get_next_word(String,Rest,Word),
%	      lexToSurf(Lang,Word,PF1),
	      do_pf(Lang,Word,PF).


do_pf(Lang,S1,S2) :-
	findall(pf(A,B,C),
		Lang 'RG' pf(A,B,C),
		Rules),
	do_pf_rules(Lang,Rules,S1,S2).

% do_pf_rules/3 è in hist.pl


cleanSpaces([' '|S],S1) :-
	!,
	cleanSpaces2(S,S1).
cleanSpaces(S,S1) :- 
	cleanSpaces2(S,S1).


cleanSpaces2([' ',' '|S],[' '|S1]) :-
	!,
	cleanSpaces2(S,S1).
cleanSpaces2([' '],[]) :- !.
cleanSpaces2([],[]).
cleanSpaces2([X|S],[X|S1]) :-
	cleanSpaces2(S,S1).
