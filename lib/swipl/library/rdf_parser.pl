/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2013, University of Amsterdam
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

:- module(rdf_parser,
          [ xml_to_plrdf/3,             % +XMLTerm, -RDFTerm, +State
            element_to_plrdf/3,         % +ContentList, -RDFTerm, +State
            make_rdf_state/3,           % +Options, -State, -RestOptions
            rdf_modify_state/3,         % +XMLAttrs, +State0, -State
            rdf_name_space/1
          ]).
:- use_module(library(record),[(record)/1, op(_,_,record)]).
:- use_module(library(rewrite_term),
              [ rew_term_expansion/2,rew_goal_expansion/2,
                op(_,_,::=)
              ]).
:- autoload(library(lists),[select/3,append/3,member/2]).
:- autoload(library(occurs),[sub_term/2]).
:- autoload(library(sgml),[xml_name/1]).
:- autoload(library(uri),[iri_normalized/3]).


% xml_name/1
:- op(500, fx, \?).                     % Optional (attrs)

term_expansion(F, T) :- rew_term_expansion(F, T).
goal_expansion(F, T) :- rew_goal_expansion(F, T).

goal_expansion(attrs(Attrs, List), Goal) :-
    translate_attrs(List, Attrs, Goal).

translate_attrs(Var, Attrs, rewrite_term(Var, Attrs)) :-
    var(Var),
    !.
translate_attrs([], _, true) :- !.
translate_attrs([H], Attrs, Goal) :-
    !,
    (   var(H)
    ->  Goal = rewrite_term(H, Attrs)
    ;   H = \?Optional
    ->  Goal = (   member(A, Attrs),
                   OptRewrite
               ->  true
               ;   true
               ),
        expand_goal(rewrite_term(\Optional, A), OptRewrite)
    ;   Goal = (   member(A, Attrs),
                   Rewrite
               ->  true
               ),
        expand_goal(rewrite_term(H, A), Rewrite)
    ).
translate_attrs([H|T], Attrs0, (G0, G1)) :-
    !,
    (   var(H)
    ->  G0 = rewrite_term(H, Attrs0),
        Attrs1 = Attrs0
    ;   H = \?Optional
    ->  G0 = (   select(A, Attrs0, Attrs1),
                 OptRewrite
             ->  true
             ;   Attrs1 = Attrs0
             ),
        expand_goal(rewrite_term(\Optional, A), OptRewrite)
    ;   G0 = (   select(A, Attrs0, Attrs1),
                 Rewrite
             ),
        expand_goal(rewrite_term(H, A), Rewrite)
    ),
    translate_attrs(T, Attrs1, G1).
translate_attrs(Rule, Attrs, Goal) :-
    expand_goal(rewrite_term(Rule, Attrs), Goal).


:- multifile rdf_name_space/1.
:- dynamic   rdf_name_space/1.

%!  rdf_name_space(?URL) is nondet.
%
%   True if URL must be handled  as rdf: Determines special handling
%   of rdf:about, rdf:resource, etc.


rdf_name_space('http://www.w3.org/1999/02/22-rdf-syntax-ns#').
rdf_name_space('http://www.w3.org/TR/REC-rdf-syntax').


:- record
    rdf_state(base_uri='',
              lang='',
              ignore_lang=false,
              convert_typed_literal).


%!  xml_to_plrdf(+RDFElementOrObject, -RDFTerm, +State)
%
%   Translate an XML (using namespaces)  term   into  an Prolog term
%   representing the RDF data.  This  term   can  then  be  fed into
%   rdf_triples/[2,3] to create a list of   RDF triples. State is an
%   instance of an rdf_state record.

xml_to_plrdf(Element, RDF, State) :-
    (   is_list(Element)
    ->  rewrite_term(\xml_content_objects(RDF, State), Element)
    ;   rewrite_term(\xml_objects(RDF, State), Element)
    ).

%!  element_to_plrdf(+DOM, -RDFTerm, +State)
%
%   Rewrite a single XML element.

element_to_plrdf(Element, RDF, State) :-
    rewrite_term(\nodeElementList(RDF, State), [Element]).

xml_objects(Objects, Options0) ::=
        E0,
        { modify_state(E0, Options0, E, Options), !,
          rewrite_term(\xml_objects(Objects, Options), E)
        }.
xml_objects(Objects, Options) ::=
        element((\rdf('RDF'), !),
                _,
                \nodeElementList(Objects, Options)),
        !.
xml_objects(Objects, Options) ::=
        element(_, _, \xml_content_objects(Objects, Options)).

xml_content_objects([], _) ::=
        [].
xml_content_objects([H|T], Options) ::=
        [ \xml_objects(H, Options)
        | \xml_content_objects(T, Options)
        ].


nodeElementList([], _Options) ::=
        [], !.
nodeElementList(L, Options) ::=
        [ (\ws, !)
        | \nodeElementList(L, Options)
        ].
nodeElementList([H|T], Options) ::=
        [ \nodeElementOrError(H, Options)
        | \nodeElementList(T, Options)
        ].

nodeElementOrError(H, Options) ::=
        \nodeElement(H, Options), !.
nodeElementOrError(unparsed(Data), _Options) ::=
        Data.

nodeElement(description(Type, About, Properties), Options) ::=
        \description(Type, About, Properties, Options).


                 /*******************************
                 *          DESCRIPTION         *
                 *******************************/

description(Type, About, Properties, Options0) ::=
        E0,
        { modify_state(E0, Options0, E, Options), !,
          rewrite_term(\description(Type, About, Properties, Options), E)
        }.
description(description, About, Properties, Options) ::=
        element(\rdf('Description'),
                \attrs([ \?idAboutAttr(About, Options)
                       | \propAttrs(PropAttrs, Options)
                       ]),
                \propertyElts(PropElts, Options)),
        { !, append(PropAttrs, PropElts, Properties)
        }.
description(Type, About, Properties, Options) ::=
        element(\name_uri(Type, Options),
                \attrs([ \?idAboutAttr(About, Options)
                       | \propAttrs(PropAttrs, Options)
                       ]),
                \propertyElts(PropElts, Options)),
        { append(PropAttrs, PropElts, Properties)
        }.

propAttrs([], _) ::=
        [], !.
propAttrs([H|T], Options) ::=
        [ \propAttr(H, Options)
        | \propAttrs(T, Options)
        ].

propAttr(rdf:type = URI, Options) ::=
        \rdf_or_unqualified(type) = \value_uri(URI, Options), !.
propAttr(Name = Literal, Options) ::=
        Name = Value,
        { mkliteral(Value, Literal, Options)
        }.

propertyElts([], _) ::=
        [], !.
propertyElts(Elts, Options) ::=
        [ (\ws, !)
        | \propertyElts(Elts, Options)
        ].
propertyElts([H|T], Options) ::=
        [ \propertyElt(H, Options)
        | \propertyElts(T, Options)
        ].

propertyElt(E, Options) ::=
        \propertyElt(Id, Name, Value, Options),
        { mkprop(Name, Value, Prop),
          (   var(Id)
          ->  E = Prop
          ;   E = id(Id, Prop)
          )
        }.

mkprop(NS:Local, Value, rdf:Local = Value) :-
    rdf_name_space(NS),
    !.
mkprop(Name, Value, Name = Value).


propertyElt(Id, Name, Value, Options0) ::=
        E0,
        { modify_state(E0, Options0, E, Options), !,
          rewrite_term(\propertyElt(Id, Name, Value, Options), E)
        }.
propertyElt(Id, Name, Value, Options) ::=
        \literalPropertyElt(Id, Name, Value, Options), !.
propertyElt(_, Name, Literal, Options) ::=
        element(Name,
                \attrs([ \parseLiteral
                       ]),
                Content),
        { !,
          literal_value(Content, Literal, Options)
        }.
propertyElt(Id, Name, collection(Elements), Options) ::=
        element(Name,
                \attrs([ \parseCollection,
                         \?idAttr(Id, Options)
                       ]),
                \nodeElementList(Elements, Options)).
                                        % 5.14 emptyPropertyElt
propertyElt(Id, Name, Value, Options) ::=
        element(Name, A, \all_ws),
        { !,
          rewrite_term(\emptyPropertyElt(Id, Value, Options), A)
        }.

propertyElt(_, Name, description(description, Id, Properties), Options) ::=
        element(Name,
                \attrs([ \parseResource,
                         \?idAboutAttr(Id, Options)
                       ]),
                \propertyElts(Properties, Options)),
        !.
propertyElt(Id, Name, Literal, Options) ::=
        element(Name,
                \attrs([ \?idAttr(Id, Options)
                       ]),
                [ Value ]),
        { atom(Value), !,
          mkliteral(Value, Literal, Options)
        }.
propertyElt(Id, Name, Value, Options) ::=
        element(Name,
                \attrs([ \?idAttr(Id, Options)
                       ]),
                \an_rdf_object(Value, Options)), !.
propertyElt(Id, Name, unparsed(Value), Options) ::=
        element(Name,
                \attrs([ \?idAttr(Id, Options)
                       ]),
                Value).

literalPropertyElt(Id, Name, Literal, Options) ::=
        element(Name,
                \attrs([ \typeAttr(Type, Options),
                         \?idAttr(Id, Options)
                       ]),
                Content),
        { typed_literal(Type, Content, Literal, Options)
        }.

emptyPropertyElt(Id, Literal, Options) ::=
        \attrs([ \?idAttr(Id, Options),
                 \?parseLiteral
               | \noMoreAttrs
               ]),
        { !,
          mkliteral('', Literal, Options)
        }.
emptyPropertyElt(Id,
                 description(description, About, Properties),
                 Options) ::=
        \attrs([ \?idAttr(Id, Options),
                 \?aboutResourceEmptyElt(About, Options),
                 \?parseResource
               | \propAttrs(Properties, Options)
               ]), !.

aboutResourceEmptyElt(about(URI), Options) ::=
        \resourceAttr(URI, Options), !.
aboutResourceEmptyElt(node(URI), _Options) ::=
        \nodeIDAttr(URI).

%!  literal_value(+In, -Value, +Options)
%
%   Create the literal value for rdf:parseType="Literal" attributes.
%   The content is the Prolog XML DOM tree for the literal.
%
%   @tbd    Note that the specs demand a canonical textual representation
%           of the XML data as a Unicode string.  For now the user can
%           achieve this using the convert_typed_literal hook.

literal_value(Value, literal(type(rdf:'XMLLiteral', Value)), _).

%!  mkliteral(+Atom, -Object, +Options)
%
%   Translate attribute value Atom into an RDF object using the
%   lang(Lang) option from Options.

mkliteral(Text, literal(Val), Options) :-
    atom(Text),
    (   rdf_state_lang(Options, Lang),
        Lang \== ''
    ->  Val = lang(Lang, Text)
    ;   Val = Text
    ).

%!  typed_literal(+Type, +Content, -Literal, +Options)
%
%   Handle a literal attribute with rdf:datatype=Type qualifier. NB:
%   possibly  it  is  faster  to  use  a  global  variable  for  the
%   conversion hook.

typed_literal(Type, Content, literal(Object), Options) :-
    rdf_state_convert_typed_literal(Options, Convert),
    nonvar(Convert),
    !,
    (   catch(call(Convert, Type, Content, Object), E, true)
    ->  (   var(E)
        ->  true
        ;   Object = E
        )
    ;   Object = error(cannot_convert(Type, Content), _)
    ).
typed_literal(Type, [], literal(type(Type, '')), _Options) :- !.
typed_literal(Type, [Text], literal(type(Type, Text)), _Options) :- !.
typed_literal(Type, Content, literal(type(Type, Content)), _Options).


idAboutAttr(id(Id), Options) ::=
        \idAttr(Id, Options), !.
idAboutAttr(about(About), Options) ::=
        \aboutAttr(About, Options), !.
idAboutAttr(node(About), _Options) ::=
        \nodeIDAttr(About), !.

%!  an_rdf_object(-Object, +OptionsURI)
%
%   Deals with an object, but there may be spaces around.  I'm still
%   not sure where to deal with these.  Best is to ask the XML parser
%   to get rid of them, So most likely this code will change if this
%   happens.

an_rdf_object(Object, Options) ::=
        [ \nodeElement(Object, Options)
        ], !.
an_rdf_object(Object, Options) ::=
        [ (\ws, !)
        | \an_rdf_object(Object, Options)
        ].
an_rdf_object(Object, Options) ::=
        [ \nodeElement(Object, Options),
          \ws
        ], !.

ws ::=
        A,
        { atom(A),
          atom_chars(A, Chars),
          all_blank(Chars), !
        }.
ws ::=
        pi(_).

all_ws ::=
        [], !.
all_ws ::=
        [\ws | \all_ws].

all_blank([]).
all_blank([H|T]) :-
    char_type(H, space),            % SWI-Prolog specific
    all_blank(T).


                 /*******************************
                 *         RDF ATTRIBUTES       *
                 *******************************/

idAttr(Id, Options) ::=
        \rdf_or_unqualified('ID') = \uniqueid(Id, Options).

aboutAttr(About, Options) ::=
        \rdf_or_unqualified(about) = \value_uri(About, Options).

nodeIDAttr(About) ::=
        \rdf_or_unqualified(nodeID) = About.

resourceAttr(URI, Options) ::=
        \rdf_or_unqualified(resource) = \value_uri(URI, Options).

typeAttr(Type, Options) ::=
        \rdf_or_unqualified(datatype) = \value_uri(Type, Options).

name_uri(URI, Options) ::=
        NS:Local,
        {   !, atom_concat(NS, Local, A),
            rewrite_term(\value_uri(URI, Options), A)
        }.
name_uri(URI, Options) ::=
        \value_uri(URI, Options).

value_uri(URI, Options) ::=
        A,
        {   rdf_state_base_uri(Options, Base),
            (   Base \== []
            ->  iri_normalized(A, Base, URI)
            ;   URI = A
            )
        }.

uniqueid(Id, Options) ::=
        A,
        {   unique_xml_name(A, HashID),
            make_globalid(HashID, Options, Id)
        }.

unique_xml_name(Name, HashID) :-
    atom_concat(#, Name, HashID),
    (   xml_name(Name)
    ->  true
    ;   print_message(warning, rdf(not_a_name(Name)))
    ).

make_globalid(In, Options, Id) :-
    rdf_state_base_uri(Options, Base),
    iri_normalized(In, Base, Id).

parseLiteral    ::= \rdf_or_unqualified(parseType) = 'Literal'.
parseResource   ::= \rdf_or_unqualified(parseType) = 'Resource'.
parseCollection ::= \rdf_or_unqualified(parseType) = 'Collection'.


                 /*******************************
                 *           PRIMITIVES         *
                 *******************************/

rdf(Tag) ::=
        NS:Tag,
        { rdf_name_space(NS), !
        }.

rdf_or_unqualified(Tag) ::=
        Tag.
rdf_or_unqualified(Tag) ::=
        NS:Tag,
        { rdf_name_space(NS), !
        }.


                 /*******************************
                 *             BASICS           *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This code is translated by the  goal_expansion/2   rule  at the start of
this file. We leave the original code for reference.

attrs(Bag) ::=
        L0,
        { do_attrs(Bag, L0)
        }.

do_attrs([], _) :- !.
do_attrs([\?H|T], L0) :- !,             % optional
        (   select(X, L0, L),
            rewrite_term(\H, X)
        ->  true
        ;   L = L0
        ),
        do_attrs(T, L).
do_attrs([H|T], L0) :-
        select(X, L0, L),
        rewrite_term(H, X), !,
        do_attrs(T, L).
do_attrs(C, L) :-
        rewrite_term(C, L).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%       \noMoreAttrs
%
%       Check attribute-list is empty.  Reserved xml: attributes are
%       excluded from this test.

noMoreAttrs ::=
        [], !.
noMoreAttrs ::=
        [ xml:_=_
        | \noMoreAttrs
        ].

%!  modify_state(+Element0, +Options0, -Element, -Options) is semidet.
%
%   If Element0 contains xml:base = Base, strip it from the
%   attributes list and update base_uri(_) in the Options
%
%   It Element0 contains xml:lang = Lang, strip it from the
%   attributes list and update lang(_) in the Options
%
%   Remove all xmlns=_, xmlns:_=_ and xml:_=_.  Only succeed
%   if something changed.

modify_state(element(Name, Attrs0, Content), Options0,
             element(Name, Attrs,  Content), Options) :-
    modify_a_state(Attrs0, Options0, Attrs, Options),
    Attrs0 \== Attrs.

rdf_modify_state(Attributes, State0, State) :-
    modify_a_state(Attributes, State0, _, State).


modify_a_state([], Options, [], Options).
modify_a_state([Name=Value|T0], Options0, T, Options) :-
    modify_a(Name, Value, Options0, Options1),
    !,
    modify_a_state(T0, Options1, T, Options).
modify_a_state([H|T0], Options0, [H|T], Options) :-
    modify_a_state(T0, Options0, T, Options).


modify_a(xml:base, Base1, Options0, Options) :-
    !,
    rdf_state_base_uri(Options0, Base0),
    remove_fragment(Base1, Base2),
    iri_normalized(Base2, Base0, Base),
    set_base_uri_of_rdf_state(Base, Options0, Options).
modify_a(xml:lang, Lang, Options0, Options) :-
    !,
    rdf_state_ignore_lang(Options0, false),
    !,
    set_lang_of_rdf_state(Lang, Options0, Options).
modify_a(xmlns, _, Options, Options).
modify_a(xmlns:_, _, Options, Options).
modify_a(xml:_, _, Options, Options).


%!  remove_fragment(+URI, -WithoutFragment)
%
%   When handling xml:base, we must delete the possible fragment.

remove_fragment(URI, Plain) :-
    sub_atom(URI, B, _, _, #),
    !,
    sub_atom(URI, 0, B, _, Plain).
remove_fragment(URI, URI).


                 /*******************************
                 *     HELP PCE-EMACS A BIT     *
                 *******************************/

:- multifile
    emacs_prolog_colours:term_colours/2,
    emacs_prolog_colours:goal_classification/2.

expand(c(X), _, X) :- !.
expand(In,   Pattern, Colours) :-
    compound(In),
    !,
    In =.. [F|Args],
    expand_list(Args, PatternArgs, ColourArgs),
    Pattern =.. [F|PatternArgs],
    Colours = functor(F) - ColourArgs.
expand(X, X, classify).

expand_list([], [], []).
expand_list([H|T], [PH|PT], [CH|CT]) :-
    expand(H, PH, CH),
    expand_list(T, PT, CT).

:- discontiguous
    term_expansion/2.

term_expansion(term_colours(C),
               emacs_prolog_colours:term_colours(Pattern, Colours)) :-
    expand(C, Pattern, Colours).

term_colours((c(head(+(1))) ::= c(match), {c(body)})).
term_colours((c(head(+(1))) ::= c(match))).

emacs_prolog_colours:goal_classification(\_, expanded).

:- dynamic
    prolog:meta_goal/2.
:- multifile
    prolog:meta_goal/2,
    prolog:called_by/2.

prolog:meta_goal(rewrite_term(A, _), [A]).
prolog:meta_goal(\A,            [A+1]).

prolog:called_by(attrs(Attrs, _Term), Called) :-
    findall(G+1, sub_term(\?G, Attrs), Called, Tail),
    findall(G+1, sub_term(\G, Attrs), Tail).


