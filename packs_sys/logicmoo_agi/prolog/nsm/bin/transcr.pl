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

:- module(transcr,[
		       set_transcr_table/1,
		       set_wiki_transcr_table/1,
		       transcr/3,
		       transcr_back/3,
		       put_lang_string_format/3,
		       set_transcr_table/2
		      ]).


:- use_module(hex).
:- use_module(messages).
:- use_module(global_par).
:- use_module(utils).

/** <module> Transcription

This module's routines allow the user to input
natural language data with different systems of transcription
(transliteration) for the same language, and to switch among these
different systems.
*/


%%	put_lang_string_format(+Lang,+Format,+Text:string) is det.
%
%	Writes Text to the currently selected output stream, using
%	markup scheme defined by Format. Text must be written in
%	language Lang.
%	
put_lang_string_format(Lang,Format,Text) :-
	transcr_wiki_text(Lang,Text,T),
	!,
	put_wiki_string_format(Format,T).
put_lang_string_format(_Lang,Format,Text) :-
	put_wiki_string_format(Format,Text).

%%	set_transcr_table(+Num:int) is det
%
%	Selects the transcription table with Num as identifier as
%	the currently active transcription table.
set_transcr_table(Num) :-
	global_par:current_lang(Lang),
	set_transcr_table(Lang,Num),
	notify_stt.

set_transcr_table(Lang,0) :-
	retractall(grammar:current_transcr_table(Lang,_)),
	asserta(grammar:current_transcr_table(Lang,0)),
	!.
set_transcr_table(Lang,Num) :-
	grammar:transcr_table(Lang,Num,_),
	!,
	retractall(grammar:current_transcr_table(Lang,_)),
	asserta(grammar:current_transcr_table(Lang,Num)).
set_transcr_table(_Lang,_Num) :-
	put_message("No such transcription table").




set_wiki_transcr_table(Num) :-
	global_par:current_l2(Lang),
	set_wiki_transcr_table(Lang,Num),
	notify_stt.

set_wiki_transcr_table(Lang,0) :-
	retractall(grammar:current_wiki_transcr_table(Lang,_)),
	asserta(grammar:current_wiki_transcr_table(Lang,0)),
	!.
set_wiki_transcr_table(Lang,Num) :-
	grammar:transcr_table(Lang,Num,_),
	!,
	retractall(grammar:current_wiki_transcr_table(Lang,_)),
	asserta(grammar:current_wiki_transcr_table(Lang,Num)).
set_wiki_transcr_table(_Lang,_Num) :-
	put_message("No such transcription table").



%%	transcr(+Lang,+S:string,-S1:string) is det.
%%	transcr(+Lang,-S:string,+S1:string) is det.
%
%	Transcribes a string written in language Lang using 
%       the currently active transcription table.
%       
%       Text can be transcribed both ways.
transcr(_,S,S1) :-
	var(S),
	var(S1),
	!,
	S=S1.
transcr(Lang,S,S) :-
	grammar:current_transcr_table(Lang,0),
	!.
transcr(Lang,S,S1) :-
	grammar:current_transcr_table(Lang,Num),
	grammar:transcr_table(Lang,Num,T),
	!,
	transcr_aux(T,S,[],S1).
transcr(_,S,S).


transcr_wiki_text(Lang,T,T) :-
	grammar:current_wiki_transcr_table(Lang,0),!.
transcr_wiki_text(Lang,T,T1) :-
	grammar:current_wiki_transcr_table(Lang,N),
	grammar:transcr_table(Lang,N,Table),
	!,
	transcr_aux(Table,T,[],T1).
transcr_wiki_text(_Lang,T,T).


transcr_aux(T,S,[],S1) :-
	var(S1),
	transcr_sent(T,S,[],S1).

transcr_aux(T,S,[],S1) :-
	var(S),
	transcr_back_sent(T,S1,[],S).
	

%%	transcr_back(+Lang,+S:string,-S1:string) is det.
%
%	Alias to transcr(+Lang,-S,+S1).
%	
transcr_back(Lang,S,S1) :-
	grammar:current_transcr_table(Lang,Num),	
	grammar:transcr_table(Lang,uni(Num),T),
	!,
	global_par:current_markup(Format),
	transcr_back_sent_uni(T,Format,S,[],S1).
transcr_back(Lang,S,S1) :-
	grammar:current_transcr_table(Lang,Num),	
	grammar:transcr_table(Lang,Num,T),
	!,
	transcr_back_sent(T,S,[],S1).
transcr_back(_,S,S).


transcr_back_sent(T,S,NewS,Final) :-
	in_table_back(S,T,P,S1),
	!,
	append(NewS,P,NewS1),
	transcr_back_sent(T,S1,NewS1,Final).
transcr_back_sent(T,[A|S],NewS,Final) :-
	append(NewS,[A],NewS1),
	transcr_back_sent(T,S,NewS1,Final).
transcr_back_sent(_T,[],S,S).

transcr_back_sent_uni(T,Format,S,NewS,Final) :-
	uni_in_table_back(S,Format,T,P,S1),
	!,
	append(NewS,P,NewS1),
	transcr_back_sent_uni(T,Format,S1,NewS1,Final).
transcr_back_sent_uni(T,Format,[A|S],NewS,Final) :-
	append(NewS,[A],NewS1),
	transcr_back_sent_uni(T,Format,S,NewS1,Final).
transcr_back_sent_uni(_T,_Format,[],S,S).


/* PROVARE QUESTO INVECE DI QUELLI ; tieni l'ultima (catchall) */
transcr_sent(T,S,NewS,Final) :-
	in_table(S,T,P,S1),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S1,NewS1,Final).

transcr_sent(T,[A|S],NewS,Final) :-
	append(NewS,[A],NewS1),
	transcr_sent(T,S,NewS1,Final).

transcr_sent(_T,[],S,S).


in_table(Sent,[S:P|_Table],P,NewSent) :-
	append(S,NewSent,Sent),!.
in_table(Sent,[_S1:_P1|Table],P,NewSent) :-
	in_table(Sent,Table,P,NewSent).

in_table_back(Sent,[P:S|_Table],P,NewSent) :-
	append(S,NewSent,Sent),!.
in_table_back(Sent,[_S1:_P1|Table],P,NewSent) :-
	in_table_back(Sent,Table,P,NewSent).

uni_in_table_back(Sent,Format,[P:S|_Table],P,NewSent) :-
	hex2decs(S,S2),
	get_unicode_escape(Format,S2,S1),
	append(S1,NewSent,Sent),!.
uni_in_table_back(Sent,Format,[_S1:_P1|Table],P,NewSent) :-
	uni_in_table_back(Sent,Format,Table,P,NewSent).

get_unicode_escape(rtf,S,S1) :-
	!,
	append("\\u",S,S2),
	append(S2,"?",S1).
get_unicode_escape(_,S,S).

/*

transcr_sent(T,[A,B,C,D,E,F,G|S],NewS,Final) :-
	member([A,B,C,D,E,F,G]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A,B,C,D,E,F|S],NewS,Final) :-
	member([A,B,C,D,E,F]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A,B,C,D,E|S],NewS,Final) :-
	member([A,B,C,D,E]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A,B,C,D|S],NewS,Final) :-
	member([A,B,C,D]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A,B,C|S],NewS,Final) :-
	member([A,B,C]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A,B|S],NewS,Final) :-
	member([A,B]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A|S],NewS,Final) :-
	member([A]:P,T),
	!,
	append(NewS,P,NewS1),
	transcr_sent(T,S,NewS1,Final).
transcr_sent(T,[A|S],NewS,Final) :-
	append(NewS,[A],NewS1),
	transcr_sent(T,S,NewS1,Final).
	

transcr_sent_aux(_T,[],S,S).
transcr_sent_aux(T,[A,B,C|S],NewS,Final) :-
	member(P:[A,B,C],T),
	!,
	append(NewS,P,NewS1),
	transcr_sent_aux(T,S,NewS1,Final).
transcr_sent_aux(T,[A,B|S],NewS,Final) :-
	member(P:[A,B],T),
	!,
	append(NewS,P,NewS1),
	transcr_sent_aux(T,S,NewS1,Final).
transcr_sent_aux(T,[A|S],NewS,Final) :-
	member(P:[A],T),
	!,
	append(NewS,P,NewS1),
	transcr_sent_aux(T,S,NewS1,Final).
transcr_sent_aux(T,[A|S],NewS,Final) :-
	append(NewS,[A],NewS1),
	transcr_sent_aux(T,S,NewS1,Final).
	
*/


	
