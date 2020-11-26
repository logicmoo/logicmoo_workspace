/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2020, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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

:- module(rdf_portray,
          [ rdf_portray_as/1,           % +Style
            rdf_portray_lang/1          % +Lang
          ]).
:- use_module(library(semweb/rdf_db),[rdf_global_id/2,rdf_has/3]).
:- use_module(library(semweb/rdfs),[rdfs_ns_label/2]).

:- autoload(library(error),[must_be/2]).
:- autoload(library(lists),[member/2]).

/** <module> Portray RDF resources

This module defines  rules  for  user:portray/1   to  help  tracing  and
debugging  RDF  resources  by  printing   them    in   a   more  concise
representation and optionally adding comment  from   the  label field to
help the user interpreting the URL. The main predicates are:

        * rdf_portray_as/1 defines the overall style
        * rdf_portray_lang/1 selects languages for extracting label comments

@tbd    Define alternate predicate to use for providing a comment
@tbd    Use rdf:type if there is no meaningful label?
@tbd    Smarter guess whether or not the local identifier might be
        meaningful to the user without a comment.  I.e. does it look
        `word-like'?
*/

:- dynamic
    style/1,
    lang/1.

%!  rdf_portray_as(+Style) is det.
%
%   Set the style used to portray resources.  Style is one of:
%
%           $ =|prefix:id|= :
%           Write as NS:ID, compatible with what can be handed to
%           the rdf predicates.  This is the default.
%
%           $ =writeq= :
%           Use quoted write of the full resource.
%
%           $ =|prefix:label|= :
%           Write namespace followed by the label.  This format
%           cannot be handed to rdf/3 and friends, but can be
%           useful if resource-names are meaningless identifiers.
%
%           $ =|prefix:id=label|= :
%           This combines prefix:id with prefix:label, providing both human
%           readable output and output that can be pasted into the
%           commandline.

rdf_portray_as(Style) :-
    must_be(oneof([writeq, prefix:id, prefix:label, prefix:id=label]), Style),
    retractall(style(_)),
    assert(style(Style)).

%!  rdf_portray_lang(+Lang) is det.
%
%   If Lang is a list, set the list or preferred languages. If it is
%   a  single  atom,  push  this  language  as  the  most  preferred
%   language.

rdf_portray_lang(Lang) :-
    (   is_list(Lang)
    ->  must_be(list(atom), Lang),
        retractall(lang(_)),
        forall(member(L,Lang), assert(lang(L)))
    ;   must_be(atom, Lang),
        asserta(lang(Lang))
    ).

try_lang(L) :- lang(L).
try_lang(_).


:- multifile
    user:portray/1.

user:portray(URL) :-
    atom(URL),
    http_url(URL),
    !,
    (   style(Style)
    ->  true
    ;   Style = prefix:id
    ),
    portray_url(Style, URL).
user:portray(URL) :-
    atom(URL),
    atom_concat('_:file://', URL2, URL),
    sub_atom(URL2, S, _, A, #),
    sub_atom(URL2, _, A, 0, Local),
    sub_atom(URL2, 0, S, _, Path),
    file_base_name(Path, Base),
    format('_:~w#~w', [Base, Local]).

http_url(URL) :- sub_atom(URL, 0, _, _, 'http://'), !.
http_url(URL) :- sub_atom(URL, 0, _, _, 'https://'), !.

portray_url(writeq, URL) :-
    writeq(URL).
portray_url(prefix:id, URL) :-
    (   rdf_global_id(NS:Id, URL)
    ->  writeq(NS:Id)
    ;   writeq(URL)
    ).
portray_url(prefix:id=label, URL) :-
    (   rdf_global_id(NS:Id, URL)
    ->  Value = NS:Id
    ;   Value = URL
    ),
    (   Id \== '',
        (   (   try_lang(Lang),
                rdf_has(URL, rdfs:label, literal(lang(Lang, Label)))
            ->  nonvar(Lang),
                \+ label_is_id(Label, Id)
            )
        ->  format('~q/*"~w"@~w*/', [Value, Label, Lang])
        ;   rdf_has(URL, rdfs:label, literal(type(Type, Label))),
            nonvar(Type),
            \+ label_is_id(Label, Id)
        ->  (   rdf_global_id(TNS:TId, Type)
            ->      TVal = TNS:TId
            ;       TVal = Type
            ),
            format('~q/*"~w"^^~w*/', [Value, Label, TVal])
        ;   rdf_has(URL, rdfs:label, literal(Label)),
            atom(Label),
            Label \== Id
        ->  format('~q/*"~w"*/', [Value, Label])
        )
    ->  true
    ;   writeq(Value)
    ).
portray_url(prefix:label, URL) :-
    rdfs_ns_label(URL, Label),
    write(Label).

label_is_id(_, Var) :-
    var(Var), !, fail.
label_is_id(Label, Label) :- !.
label_is_id(L0, L1) :-
    downcase_atom(L0, Lwr),
    downcase_atom(L1, Lwr).

