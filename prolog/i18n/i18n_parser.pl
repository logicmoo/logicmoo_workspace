/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(i18n_parser, [parse_po_entry/3, parse_po_entries/3]).

:- use_module(library(lists)).

tr_text(A,B,T) :- append(A,T,B).

comment(Text) --> "#", tr_text(Text), "\n".

translator_comments([TC|TCs]) -->
        "# ", tr_text(TC), "\n",
        !,
        translator_comments(TCs).
translator_comments([[]|TCs]) -->
        "#\n",
        !,
        translator_comments(TCs).
translator_comments([]) --> [].

extracted_comments([EC|ECs]) -->
        "#. ", tr_text(EC), "\n",
        !,
        extracted_comments(ECs).
extracted_comments([]) --> [].

reference([R|Rs]) -->
        "#: ", tr_text(R), "\n",
        !,
        reference(Rs).
reference([]) --> [].

flag([F|Fs]) -->
        "#, ", tr_text(F), "\n",
        !,
        flag(Fs).
flag([]) --> [].

%% parse_po_entries(+Term,-Codes,?Tail).
%% parse_po_entries(-Term,+Codes,?Tail).

%% Note that parse_po_entries/3 is reversible, it is used to compile and
%% to decompile the .po file. Example:

/*
%% i18n_support:parse_po_entries([i18n_support:i18n("a","b","c"),
%% i18n_support:i18n("a","b\nc","c")],D,[]), atom_codes(A,D),write(A).

  read_file_to_codes('/usr/share/cups/locale/es/cups_es.po',C,[]),
  i18n_support:parse_po_entries(Term,C,[]),
  i18n_support:parse_po_entries(Term,S,[]),format('~s',[S]).
*/

parse_po_entry(entry(TranslatorComments, ExtractedComments,
                     Reference, Flag, MsgId, MsgStr)) -->
        translator_comments(TranslatorComments),
        extracted_comments(ExtractedComments),
        reference(Reference),
        flag(Flag),
        msg("msgid", MsgId),
        msg("msgstr", MsgStr),
        ( "\n" ->[] ; [] ),
        !.
parse_po_entry(comment(Text)) -->
        comment(Text).
parse_po_entry(nl) -->
        "\n".
% parse_po_entry(error(E)) --> flush(E).

% flush([E|T]) --> [E], !, flush(T).
% flush([]) --> [].

parse_po_entries([Entry|Tail]) -->
        parse_po_entry(Entry),
        !,
        parse_po_entries(Tail).
parse_po_entries([]) --> [].

msg(Key, [Line|Lines]) -->
        phrase(Key), " ",
        text_line(Line),
        text_lines(Lines), !.

text_lines([Line|Lines]) --> text_line(Line), text_lines(Lines).
text_lines([]) --> "".

text_line(Line) --> "\"", escape_text(Line), "\"\n".

escape_char(0'\n) --> "\\n", !.
escape_char(0'\") --> "\\\"", !.
escape_char(0'\\) --> "\\\\", !.
escape_char(C) --> [C].

escape_text([]) --> [].
escape_text([C|Text]) --> escape_char(C), escape_text(Text).
