/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2015, VU University Amsterdam
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

:- module(rdfa,
          [ read_rdfa/3,                % +Input, -RDF, +Options
            xml_rdfa/3                  % +XMLDom, -RDF, +Options
          ]).
:- use_module(library(semweb/rdf_db),
	    [ rdf_register_prefix/2,
	      rdf_meta/1,
	      rdf_global_id/2,
	      rdf_equal/2,
	      rdf_is_bnode/1,
	      rdf_global_term/2,
	      rdf_transaction/2,
	      rdf_assert/4,
	      rdf_set_graph/2,
              op(_,_,_)
	    ]).
:- use_module(library(xpath),[xpath/3, op(_,_,_)]).

:- autoload(library(apply),[maplist/3,maplist/2,exclude/3,include/3]).
:- autoload(library(debug),[debugging/1,debug/3]).
:- autoload(library(error),[instantiation_error/1,type_error/2]).
:- autoload(library(gui_tracer),[gtrace/0]).
:- autoload(library(lists),[append/2,reverse/2,member/2,append/3]).
:- autoload(library(option),[merge_options/3,option/2,option/3]).
:- autoload(library(prolog_stack),[backtrace/1]).
:- autoload(library(sgml),
	    [ load_xml/3, load_html/3, xml_basechar/1, xml_ideographic/1,
	      xml_digit/1, xml_combining_char/1, xml_extender/1
	    ]).
:- autoload(library(sgml_write),[xml_write/2]).
:- autoload(library(uri),
	    [ uri_file_name/2, uri_components/2, uri_data/3, uri_data/4,
	      iri_normalized/3, iri_normalized/2, uri_normalized/3
	    ]).
:- autoload(library(dcg/basics),[blanks/2,blank/2,alpha_to_lower/3]).
:- autoload(library(http/http_open),[http_open/3]).

/** <module> Extract RDF from an HTML or XML DOM

This module implements extraction of  RDFa   triples  from parsed XML or
HTML documents. It has two interfaces:  read_rdfa/3 to read triples from
some input (stream, file, URL) and xml_rdfa/3 to extract triples from an
HTML or XML  document  that  is   already  parsed  with  load_html/3  or
load_xml/3.

@see http://www.w3.org/TR/2013/REC-rdfa-core-20130822/
@see http://www.w3.org/TR/html-rdfa/
*/

:- rdf_register_prefix(rdfa, 'http://www.w3.org/ns/rdfa#').

:- rdf_meta
    add_triple(+, r, r, o),
    add_incomplete_triple(+, t).

:- discontiguous
    term_expansion/2.

:- predicate_options(xml_rdfa/3, 3,
                     [ base(atom),
                       anon_prefix(any),
                       lang(atom),
                       vocab(atom),
                       markup(atom)
                     ]).
:- predicate_options(read_dom/3, 3,
                     [ pass_to(load_html/3, 3),
                       pass_to(load_xml/3, 3)
                     ]).
:- predicate_options(read_rdfa/3, 3,
                     [ pass_to(read_dom/3, 3),
                       pass_to(xml_rdfa/3, 3),
                       pass_to(system:open/4, 4),
                       pass_to(http_open:http_open/3, 3)
                     ]).


                 /*******************************
                 *          STREAM READING      *
                 *******************************/

%!  read_rdfa(+Input, -Triples, +Options) is det.
%
%   True when Triples is a list of rdf(S,P,O) triples extracted from
%   Input. Input is either a stream, a  file name, a URL referencing
%   a file name or a URL that  is valid for http_open/3. Options are
%   passed to open/4, http_open/3 and  xml_rdfa/3.   If  no  base is
%   provided in Options, a base is deduced from Input.

read_rdfa(Input, Triples, Options) :-
    setup_call_cleanup(
        open_input(Input, In, NewOptions, Close, Options),
        read_dom(In, DOM, Options),
        close_input(Close)),
    merge_options(Options, NewOptions, RDFaOptions),
    xml_rdfa(DOM, Triples, RDFaOptions).

open_input(Input, In, NewOptions, Close, Options) :-
    open_input2(Input, In, NewOptions, Close0, Options),
    detect_bom(In, Close0, Close).

open_input2(stream(In), In, Options, true, _) :-
    !,
    (   stream_property(In, file_name(Name)),
        to_uri(Name, URI)
    ->  Options = [base(URI)]
    ;   Options = []
    ).
open_input2(In, In, Options, true, _) :-
    is_stream(In),
    !,
    (   stream_property(In, file_name(Name)),
        to_uri(Name, URI)
    ->  Options = [base(URI)]
    ;   Options = []
    ).
open_input2(URL, In, [base(URL)], close(In), Options) :-
    atom(URL),
    uri_file_name(URL, File),
    !,
    open(File, read, In, Options).
open_input2(URL, In, [base(Base)], close(In), Options) :-
    atom(URL),
    to_uri2(URL, Base),
    !,
    http_open(URL, In, Options).
open_input2(File, In, [base(URI)], close(In), Options) :-
    absolute_file_name(File, Path, [access(read)]),
    uri_file_name(URI, Path),
    open(Path, read, In, Options).

%!  detect_bom(+In, +Close0, -Close) is det.
%
%   We may be loading a binary stream. In   that  case we want to do
%   BOM detection.

detect_bom(In, Close0, Close) :-
    stream_property(In, type(binary)),
    stream_property(In, encoding(Enc)),
    catch(set_stream(In, encoding(bom)),_,fail),
    !,
    merge_close(Close0, set_stream(In, encoding(Enc)), Close).
detect_bom(_, Close, Close).

merge_close(true, Close, Close) :- !.
merge_close(Close, _, Close).

to_uri(URI0, URI) :-
    to_uri2(URI0, URI),
    !.
to_uri(URI0, URI) :-
    absolute_file_name(URI0, Path),
    uri_file_name(URI, Path).

to_uri2(URI0, Base) :-
    uri_components(URI0, Components),
    uri_data(scheme, Components, Scheme),
    ground(Scheme),
    http_scheme(Scheme),
    !,
    uri_data(fragment, Components, _, Components2),
    uri_components(Base, Components2).

http_scheme(http).
http_scheme(https).

close_input(true).
close_input(close(X)) :- close(X).
close_input(set_stream(In, encoding(Enc))) :- set_stream(In, encoding(Enc)).

read_dom(In, DOM, Options) :-
    option(dialect(Dialect), Options),
    !,
    (   xml_dialect(Dialect)
    ->  load_xml(stream(In), DOM, Options)
    ;   load_html(stream(In), DOM, Options)
    ).
read_dom(In, DOM, Options) :-
    peek_string(In, 1000, Start),
    guess_dialect(Start, Dialect),
    read_dom(In, DOM, [dialect(Dialect)|Options]).

xml_dialect(xml).
xml_dialect(xmlns).
xml_dialect(svg).
xml_dialect(xhtml).
xml_dialect(xhtml5).

guess_dialect(Start, Dialect) :-
    sub_string(Start, _, _, _, "<?xml"),
    !,
    Dialect = xml.
guess_dialect(Start, Dialect) :-
    sub_string(Start, _, _, _, "<html"),
    !,
    (   sub_string(Start, _, _, _, "xmlns:")
    ->  Dialect = xhtml
    ;   string_codes(Start, Codes),
        phrase(html_doctype(DialectFound), Codes, _)
    ->  Dialect = DialectFound
    ;   Dialect = html
    ).
guess_dialect(Start, Dialect) :-
    sub_string(Start, _, _, _, "<svg"),
    !,
    Dialect = svg.
guess_dialect(_, xml).

html_doctype(html5) -->
    blanks,
    "<!DOCTYPE", blank, blanks, "html", blanks, ">",
    !.
html_doctype(html4) -->
    blanks,
    "<!", icase_string(`doctype`), blank, blanks, icase_string(`html`),
    blank, blanks,
    icase_string(`public`),
    blank,
    !.

icase_string([]) --> [].
icase_string([H|T]) --> alpha_to_lower(H), icase_string(T).


                 /*******************************
                 *        DOM PROCESSING        *
                 *******************************/

%!  xml_rdfa(+DOM, -RDF, +Options)
%
%   True when RDF is a list of   rdf(S,P,O) terms extracted from DOM
%   according to the RDFa specification. Options processed:
%
%     * base(+BaseURI)
%     URI to use for ''. Normally set to the document URI.
%     * anon_prefix(+AnnonPrefix)
%     Prefix for blank nodes.
%     * lang(+Lang)
%     Default for =lang=
%     * vocab(+Vocab)
%     Default for =vocab=
%     * markup(+Markup)
%     Markup language processed (xhtml, xml, ...)

xml_rdfa(DOM, _, _) :-
    var(DOM),
    !,
    instantiation_error(DOM).
xml_rdfa(DOM, RDF, Options) :-
    is_list(DOM),
    !,
    maplist(xml_rdfa_aux(Options), DOM, RDFList),
    append(RDFList, RDF).
xml_rdfa(DOM, RDF, Options) :-
    DOM = element(_,_,_),
    !,
    rdfa_evaluation_context(DOM, EvalContext, Options),
    process_node(DOM, EvalContext),
    arg(1, EvalContext.triples, List),
    reverse(List, RDF0),
    apply_patterns(RDF0, RDF).
% XML Processing Instruction (PI).
xml_rdfa(DOM, [], _) :-
    DOM = pi(_),
    !.
xml_rdfa(DOM, _, _) :-
    type_error(xml_dom, DOM).

xml_rdfa_aux(Options, DOM, RDF) :-
    xml_rdfa(DOM, RDF, Options).

process_node(DOM, EvalContext) :-
    rdfa_local_context(EvalContext, LocalContext),  % 7.5.1
    update_vocab(DOM, LocalContext),                % 7.5.2
    update_prefixes(DOM, LocalContext),             % 7.5.3
    update_lang(DOM, LocalContext),                 % 7.5.4
    update_subject(DOM, LocalContext),              % 7.5.5, 7.5.6
    emit_typeof(DOM, LocalContext),                 % 7.5.7
    update_list_mapping(DOM, LocalContext),         % 7.5.8
    step_7_5_9(DOM, LocalContext),                  % 7.5.9
    step_7_5_10(DOM, LocalContext),                 % 7.5.10
    update_property_value(DOM, LocalContext),       % 7.5.11
    complete_triples(LocalContext),                 % 7.5.12
    descent(DOM, LocalContext),                     % 7.5.13
    complete_lists(LocalContext),
    !.                % 7.5.14
process_node(DOM, EvalContext) :-
    print_message(warning, rdfa(failed(DOM, EvalContext))),
    (   debugging(rdfa(test))
    ->  gtrace,
        process_node(DOM, EvalContext)
    ;   true
    ).


%!  rdfa_evaluation_context(+DOM, -Context, +Options)
%
%   7.5.0: Create the initial evaluation context
%
%   @tbd:   derive markup from DOM

rdfa_evaluation_context(DOM, Context, Options) :-
    Context = rdfa_eval{base:Base,                  % atom
                        parent_subject:Base,        % atom
                        parent_object:null,         % null or atom
                        incomplete_triples:[],      % list
                        list_mapping:ListMapping,   % IRI --> list(List)
                        lang:Lang,                  % null or atom
                        iri_mapping:IRIMappings,    % dict
                        term_mapping:TermMappings,  % dict
                        vocab:Vocab,                % null or atom
                        bnode_id:bnode(1),          % integer
                        markup:Markup,              % Processing profile
                        anon_prefix:AnonPrefix,
                        named_bnodes:r{v:_{}},
                        root:DOM,                   % XML DOM
                        triples:triples([])},       % list
    empty_list_mapping(ListMapping),
    option(markup(Markup), Options, xhtml),
    base(DOM, Options, Base),
    default_vocab(Markup, DefaultVocab),
    option(lang(Lang), Options, ''),
    option(vocab(Vocab), Options, DefaultVocab),
    (   option(anon_prefix(AnonPrefix), Options)
    ->  true
    ;   atom_concat('__', Base, AnonPrefix)
    ),
    default_prefixes(Markup, DefPrefixes),
    mapping(prefixes(IRIMappings0), Options),
    put_dict(DefPrefixes, IRIMappings0, IRIMappings),
    mapping(terms(TermMappings), Options).

base(DOM, _Options, Base) :-
    xpath(DOM, //base(@href=Base), _),
    !.
base(_DOM, Options, Base) :-
    option(base(Base0), Options),
    rdf_global_id(Base0, Base),
    !.
base(_, _, 'http://www.example.org/').

mapping(Term, Options) :-
    Term =.. [Name, Value],
    (   TermG =.. [Name, Var],
        option(TermG, Options)
    ->  dict_create(Value, Name, Var)
    ;   dict_create(Value, Name, [])
    ).

%!  default_prefixes(+Markup, -Dict)
%
%   Create a default prefix map. Which   prefixes are supposed to be
%   in this map?

default_prefixes(Markup, _{'':DefPrefix}) :-
    default_prefix_mapping(Markup, DefPrefix).

%!  rdfa_core_prefix(?Prefix, ?URI) is nondet.
%
%   RDFa initial context prefix declarations.
%
%   @see http://www.w3.org/2011/rdfa-context/rdfa-1.1

rdfa_core_prefix(dcat,    'http://www.w3.org/ns/dcat#').
rdfa_core_prefix(qb,      'http://purl.org/linked-data/cube#').
rdfa_core_prefix(grddl,   'http://www.w3.org/2003/g/data-view#').
rdfa_core_prefix(ma,      'http://www.w3.org/ns/ma-ont#').
rdfa_core_prefix(org,     'http://www.w3.org/ns/org#').
rdfa_core_prefix(owl,     'http://www.w3.org/2002/07/owl#').
rdfa_core_prefix(prov,    'http://www.w3.org/ns/prov#').
rdfa_core_prefix(rdf,     'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
rdfa_core_prefix(rdfa,    'http://www.w3.org/ns/rdfa#').
rdfa_core_prefix(rdfs,    'http://www.w3.org/2000/01/rdf-schema#').
rdfa_core_prefix(rif,     'http://www.w3.org/2007/rif#').
rdfa_core_prefix(rr,      'http://www.w3.org/ns/r2rml#').
rdfa_core_prefix(sd,      'http://www.w3.org/ns/sparql-service-description#').
rdfa_core_prefix(skos,    'http://www.w3.org/2004/02/skos/core#').
rdfa_core_prefix(skosxl,  'http://www.w3.org/2008/05/skos-xl#').
rdfa_core_prefix(wdr,     'http://www.w3.org/2007/05/powder#').
rdfa_core_prefix(void,    'http://rdfs.org/ns/void#').
rdfa_core_prefix(wdrs,    'http://www.w3.org/2007/05/powder-s#').
rdfa_core_prefix(xhv,     'http://www.w3.org/1999/xhtml/vocab#').
rdfa_core_prefix(xml,     'http://www.w3.org/XML/1998/namespace').
rdfa_core_prefix(xsd,     'http://www.w3.org/2001/XMLSchema#').
rdfa_core_prefix(cc,      'http://creativecommons.org/ns#').
rdfa_core_prefix(ctag,    'http://commontag.org/ns#').
rdfa_core_prefix(dc,      'http://purl.org/dc/terms/').
rdfa_core_prefix(dcterms, 'http://purl.org/dc/terms/').
rdfa_core_prefix(dc11,    'http://purl.org/dc/elements/1.1/').
rdfa_core_prefix(foaf,    'http://xmlns.com/foaf/0.1/').
rdfa_core_prefix(gr,      'http://purl.org/goodrelations/v1#').
rdfa_core_prefix(ical,    'http://www.w3.org/2002/12/cal/icaltzd#').
rdfa_core_prefix(og,      'http://ogp.me/ns#').
rdfa_core_prefix(rev,     'http://purl.org/stuff/rev#').
rdfa_core_prefix(sioc,    'http://rdfs.org/sioc/ns#').
rdfa_core_prefix(v,       'http://rdf.data-vocabulary.org/#').
rdfa_core_prefix(vcard,   'http://www.w3.org/2006/vcard/ns#').
rdfa_core_prefix(schema,  'http://schema.org/').

default_prefix_mapping(xhtml, 'http://www.w3.org/1999/xhtml/vocab#') :- !.
default_prefix_mapping(_,     'http://www.example.org/').

default_vocab(_, '').

%!  rdfa_local_context(EvalContext, LocalContext)
%
%   7.5.1: Create the local context

rdfa_local_context(EvalContext, LocalContext) :-
    LocalContext = rdfa_local{skip_element:false,
                              new_subject:null,
                              current_object_resource:null,
                              typed_resource:null,
                              iri_mapping:IRIMappings,
                              incomplete_triples:[],
                              list_mapping:ListMapping,
                              lang:Lang,
                              term_mapping:TermMapping,
                              vocab:Vocab,
                              eval_context:EvalContext
                             },
    _{ iri_mapping:IRIMappings,
       list_mapping:ListMapping,
       lang:Lang,
       term_mapping:TermMapping,
       vocab:Vocab
     } :< EvalContext.


%!  update_vocab(+DOM, +Context) is det.
%
%   7.5.2.  Handle @vocab

update_vocab(DOM, Context) :-
    xpath(DOM, /(*(@vocab=Vocab0)), _),
    !,
    (   Vocab0 == ''
    ->  Vocab = ''                  % Host Language defined default?
    ;   iri(Vocab0, Vocab, Context)
    ),
    nb_set_dict(vocab, Context, Vocab),
    add_triple(Context,
               Context.eval_context.base,
               rdfa:usesVocabulary,
               Vocab).
update_vocab(_, _).

%!  update_prefixes(+DOM, +Context) is det.
%
%   7.5.3:  Update  prefix  map  using  @prefix  and  @xmlns.  First
%   processes xmlns:Prefix=IRI.

update_prefixes(DOM, Context) :-
    DOM=element(_,Attrs,_),
    xmlns_dict(Attrs, _{}, Dict0),
    (   xpath(DOM, /(*(@prefix=PrefixDecl)), _)
    ->  prefix_dict(PrefixDecl, Dict0, Dict)
    ;   Dict = Dict0
    ),
    Dict \= _{},
    !,
    put_dict(Dict, Context.iri_mapping, NewMapping),
    b_set_dict(iri_mapping, Context, NewMapping).
update_prefixes(_, _).

xmlns_dict([], Dict, Dict).
xmlns_dict([Attr=IRI|T0], Dict0, Dict) :-
    (   Attr = xmlns:Name
    ;   atom_concat('xmlns:', Name, Attr)
    ),
    !,
    downcase_atom(Name, Prefix),
    put_dict(Prefix, Dict0, IRI, Dict1),
    xmlns_dict(T0, Dict1, Dict).
xmlns_dict([_|T0], Dict0, Dict) :-
    xmlns_dict(T0, Dict0, Dict).

prefix_dict(Text, Dict0, Dict) :-
    atom_codes(Text, Codes),
    phrase(prefixes(Dict0, Dict), Codes).

%!  update_lang(+DOM, +Context) is det.
%
%   7.5.4: Update lang

update_lang(DOM, Context) :-
    DOM=element(_,Attrs,_),
    (   (   memberchk(xml:lang=Lang, Attrs)         % XML with namespaces
        ;   memberchk('xml:lang'=Lang, Attrs)       % XML without namespaces
        ;   memberchk(lang=Lang, Attrs)             % HTML 5
        )
    ->  nb_set_dict(lang, Context, Lang)
    ;   true
    ),
    (   (   memberchk(xml:base=Base, Attrs)         % XML with namespaces
        ;   memberchk('xml:base'=Base, Attrs)       % XML without namespaces
        )
    ->  nb_set_dict(base, Context.eval_context, Base)
    ;   true
    ).


%!  update_subject(+DOM, +Context) is det.
%
%   7.5.5 and 7.5.6: establish a value for new subject

update_subject(DOM, Context) :-
    DOM=element(E,Attrs,_),
    \+ has_attribute(rel, Attrs, Context),
    \+ has_attribute(rev, Attrs, Context),    % Commit to rule-set 7.5.5
    !,
    (   memberchk(property=_, Attrs),
        \+ memberchk(content=_, Attrs),
        \+ memberchk(datatype=_, Attrs)
    ->  (   (   about(DOM, About, Context)  % 7.5.5.1
            ;   About = Context.eval_context.parent_object
            ),
            About \== null
        ->  nb_set_dict(new_subject, Context, About)
        ;   true
        ),
        (   memberchk(typeof=_, Attrs)
        ->  (   (   iri_attr(about, Attrs, TypedIRI, Context),
                    TypedIRI \== null
                ;   DOM == Context.eval_context.root
                ->  iri('', TypedIRI, Context)
                ;   (   iri_attr(resource, Attrs, TypedIRI, Context)
                    ;   iri_attr(href,     Attrs, TypedIRI, Context)
                    ;   iri_attr(src,      Attrs, TypedIRI, Context)
                    ;   new_bnode(TypedIRI, Context)
                    ),
                    TypedIRI \== null
                ->  nb_set_dict(typed_resource, Context, TypedIRI),
                    nb_set_dict(current_object_resource, Context, TypedIRI)
                )
            ->  nb_set_dict(typed_resource, Context, TypedIRI)
            ;   true
            )
        ;   true
        )
    ;   (   new_subject_attr_2(SubjectAttr),        % 7.5.5.2
            memberchk(SubjectAttr=About0, Attrs),
            attr_convert(SubjectAttr, About0, About, Context),
            About \== null
        ->  true
        ;   html_root(E, Context),
            About = Context.eval_context.parent_object,
            About \== null
        ->  true
        ;   DOM == Context.eval_context.root
        ->  iri('', About, Context)
        ;   memberchk(typeof=_, Attrs)
        ->  new_bnode(About, Context)
        ;   About = Context.eval_context.parent_object,
            About \== null
        ->  (   \+ memberchk(typeof=_, Attrs)
            ->  nb_set_dict(skip_element, Context, true)
            ;   true
            )
        ),
        debug(rdfa(new_subject), '~w: set new_subject to ~p', [E, About]),
        nb_set_dict(new_subject, Context, About),
        (   memberchk(typeof=_, Attrs)
        ->  nb_set_dict(typed_resource, Context, About)
        ;   true
        )
    ).
update_subject(DOM, Context) :-
    DOM=element(_,Attrs,_),                 % 7.5.6
    (   iri_attr(about, Attrs, NewSubject, Context)
    ->  nb_set_dict(new_subject, Context, NewSubject),
        (   memberchk(typeof=_, Attrs)
        ->  nb_set_dict(typed_resource, Context, NewSubject)
        ;   true
        )
    ;   true        % was \+ memberchk(resource=_, Attrs):
                    % If no resource is provided ...
    ->  (   DOM == Context.eval_context.root
        ->  iri('', NewSubject, Context),
            nb_set_dict(new_subject, Context, NewSubject),
            (   memberchk(typeof=_, Attrs)
            ->  nb_set_dict(typed_resource, Context, NewSubject)
            ;   true
            )
        ;   NewSubject = Context.eval_context.parent_object,
            NewSubject \== null
        ->  nb_set_dict(new_subject, Context, NewSubject)
        ;   true
        )
    ),
    (   (   iri_attr(resource, Attrs, CurrentObjectResource, Context)
        ;   iri_attr(href,     Attrs, CurrentObjectResource, Context)
        ;   iri_attr(src,      Attrs, CurrentObjectResource, Context)
        ;   memberchk(typeof=_, Attrs),
            \+ memberchk(about=_, Attrs),
            new_bnode(CurrentObjectResource, Context)
        ),
        CurrentObjectResource \== null
    ->  nb_set_dict(current_object_resource, Context, CurrentObjectResource)
    ;   true
    ),
    (   memberchk(typeof=_, Attrs),
        \+ memberchk(about=_, Attrs)
    ->  nb_set_dict(typed_resource, Context,
                    Context.current_object_resource)
    ;   true
    ).

new_subject_attr_2(about).
new_subject_attr_2(resource).
new_subject_attr_2(href).
new_subject_attr_2(src).

html_root(head, Context) :- html_markup(Context.eval_context.markup).
html_root(body, Context) :- html_markup(Context.eval_context.markup).

html_markup(html).
html_markup(xhtml).

%!  emit_typeof(+DOM, +LocalContext) is det.
%
%   7.5.7: emit triples for @typeof value.

emit_typeof(DOM, Context) :-
    DOM = element(_,Attrs,_),
    Subject = Context.typed_resource,
    Subject \== null,
    memberchk(typeof=TypeOf, Attrs),
    !,
    iri_list(TypeOf, IRIs, Context),
    maplist(type_triple(Context), IRIs).
emit_typeof(_, _).

type_triple(Context, IRI) :-
    add_triple(Context, Context.typed_resource, rdf:type, IRI).

%!  update_list_mapping(+DOM, +Context) is det.
%
%   7.5.8: Create a list mapping if appropriate

update_list_mapping(_DOM, Context) :-
    Context.new_subject \== null,
    Context.new_subject \== Context.eval_context.parent_object,
    !,
    empty_list_mapping(ListMapping),
    b_set_dict(list_mapping, Context, ListMapping).
update_list_mapping(_, _).

%!  empty_list_mapping(-Mapping) is det.
%!  empty_list_mapping(+Mapping) is semidet.
%!  get_list_mapping(+IRI, +Mapping, -List) is semidet.
%!  add_list_mapping(+IRI, !Mapping, +List) is det.
%
%   Manage a list mapping. Note this needs   to be wrapped in a term
%   to be able to extend the mapping while keeping its identity.

empty_list_mapping(list_mapping(_{})).

get_list_mapping(IRI, list_mapping(Dict), Dict.get(IRI)).

add_list_mapping(IRI, LM, List) :-
    LM = list_mapping(Dict),
    setarg(1, LM, Dict.put(IRI, List)).

list_mapping_pairs(list_mapping(Dict), Pairs) :-
    dict_pairs(Dict, _, Pairs).


%!  step_7_5_9(+DOM, +Context)

step_7_5_9(_DOM, Context) :-
    Context.current_object_resource == null,
    !.
step_7_5_9(DOM, Context) :-
    DOM = element(_,Attrs,_),
    memberchk(inlist=_, Attrs),
    has_attribute(rel, Attrs, Rel, Context),
    !,
    iri_list(Rel, Preds, Context),
    CurrentObjectResource = Context.current_object_resource,
    maplist(add_property_list(Context, CurrentObjectResource),
            Preds).
step_7_5_9(DOM, Context) :-
    DOM = element(_,Attrs,_),
    (   has_attribute(rel, Attrs, Rel, Context),
        \+ memberchk(inlist=_, Attrs)
    ->  iri_list(Rel, RelIRIs, Context),
        maplist(rel_triple(Context), RelIRIs)
    ;   true
    ),
    (   has_attribute(rev, Attrs, Rev, Context)
    ->  iri_list(Rev, RevIRIs, Context),
        maplist(rev_triple(Context), RevIRIs)
    ;   true
    ).

rel_triple(Context, IRI) :-
    add_triple(Context,
               Context.new_subject, IRI, Context.current_object_resource).

rev_triple(Context, IRI) :-
    add_triple(Context,
               Context.current_object_resource, IRI, Context.new_subject).

%!  step_7_5_10(+DOM, +Context)
%
%   Similar to step_7_5_9, but adding to incomplete triples.

step_7_5_10(_DOM, Context) :-
    Context.current_object_resource \== null,
    !.
step_7_5_10(DOM, Context) :-
    DOM = element(_,Attrs,_),
    memberchk(inlist=_, Attrs),
    has_attribute(rel, Attrs, Rel, Context),
    !,
    set_current_object_resource_to_bnode(Context),
    iri_list(Rel, IRIs, Context),
    maplist(incomplete_ll_triple(Context), IRIs).
step_7_5_10(DOM, Context) :-
    DOM = element(_,Attrs,_),
    (   has_attribute(rel, Attrs, Rel, Context),
        \+ memberchk(inlist=_, Attrs)
    ->  iri_list(Rel, RelIRIs, Context),
        set_current_object_resource_to_bnode(Context),
        maplist(incomplete_rel_triple(Context), RelIRIs)
    ;   true
    ),
    (   has_attribute(rev, Attrs, Rev, Context)
    ->  iri_list(Rev, RevIRIs, Context),
        set_current_object_resource_to_bnode(Context),
        maplist(incomplete_rev_triple(Context), RevIRIs)
    ;   true
    ).

set_current_object_resource_to_bnode(Context) :-
    new_bnode(BNode, Context),
    b_set_dict(current_object_resource, Context, BNode).

incomplete_ll_triple(Context, IRI) :-
    LM = Context.list_mapping,
    (   get_list_mapping(IRI, LM, LL)
    ->  true
    ;   LL = list([]),
        add_list_mapping(IRI, LM, LL)
    ),
    add_incomplete_triple(Context, _{list:LL, direction:none}).

incomplete_rel_triple(Context, IRI) :-
    add_incomplete_triple(Context, _{predicate:IRI, direction:forward}).

incomplete_rev_triple(Context, IRI) :-
    add_incomplete_triple(Context, _{predicate:IRI, direction:reverse}).


%!  update_property_value(+DOM, +Context) is det.
%
%   7.5.11: establish current property value.

update_property_value(DOM, Context) :-
    DOM = element(Element,Attrs,Content),
    memberchk(property=PropSpec, Attrs),
    !,
    iri_list(PropSpec, Preds, Context),
    (   memberchk(datatype=DTSpec, Attrs)
    ->  (   DTSpec \== '',
            term_or_curie_or_absiri(DTSpec, DataType, Context),
            DataType \== null
        ->  (   (   rdf_equal(rdf:'XMLLiteral', DataType)
                ;   rdf_equal(rdf:'HTML', DataType)
                )
            ->  content_xml(Content, Text)
            ;   content_text(DOM, Text, Context)
            ),
            Obj0 = literal(type(DataType, Text))
        ;   content_text(DOM, Text, Context),
            Obj0 = literal(Text)
        )
    ;   memberchk(content=Text, Attrs)
    ->  Obj0 = literal(Text)
    ;   \+ has_attribute(rel, Attrs, Context),
        \+ has_attribute(rev, Attrs, Context),
        %\+ memberchk(content=_, Attrs),    % already guaranteed
        (   iri_attr(resource, Attrs, Obj0, Context)
        ;   iri_attr(href,     Attrs, Obj0, Context)
        ;   iri_attr(src,      Attrs, Obj0, Context)
        ),
        Obj0 \== null
    ->  true
    ;   (   memberchk(datetime=DateTime, Attrs)
        ;   Element == time,
            Content = [DateTime]
        ),
        html_markup(Context.eval_context.markup)
    ->  (   date_time_type(DateTime, DataType)
        ->  Obj0 = literal(type(DataType, DateTime))
        ;   Obj0 = literal(DateTime)
        )
    ;   memberchk(typeof=_, Attrs),
        \+ memberchk(about=_, Attrs)
    ->  Obj0 = Context.typed_resource
    ;   content_text(Content, Text, Context), % "as a plain literal"???
        Obj0 = literal(Text)
    ),
    (   Obj0 = literal(Text),
        atomic(Text),
        Context.lang \== ''
    ->  Obj = literal(lang(Context.lang, Text))
    ;   Obj = Obj0
    ),
    (   memberchk(inlist=_, Attrs)
    ->  maplist(add_property_list(Context, Obj), Preds)
    ;   NewSubject = Context.new_subject,
        maplist(add_property(Context, NewSubject, Obj), Preds)
    ).
update_property_value(_, _).

add_property_list(Context, Obj, Pred) :-
    LM = Context.list_mapping,
    (   get_list_mapping(Pred, LM, LL)
    ->  LL = list(Old),
        setarg(1, LL, [Obj|Old])
    ;   add_list_mapping(Pred, LM, list([Obj]))
    ).

add_property(Context, Subject, Object, Pred) :-
    add_triple(Context, Subject, Pred, Object).

content_text(element(_,Attrs,_), Text, _Context) :-
    memberchk(content=Text, Attrs),
    !.
content_text(element(_,Attrs,_), Text, Context) :-
    memberchk(datetime=Text, Attrs),
    html_markup(Context.eval_context.markup),
    !.
content_text(element(_,_,Content), Text, _Context) :-
    !,
    phrase(text_nodes(Content), Texts),
    atomic_list_concat(Texts, Text).
content_text(Content, Text, _Context) :-
    !,
    phrase(text_nodes(Content), Texts),
    atomic_list_concat(Texts, Text).

text_nodes([]) --> !.
text_nodes([H|T]) --> !, text_nodes(H), text_nodes(T).
text_nodes(element(_,_,Content)) --> !, text_nodes(Content).
text_nodes(CDATA) --> [CDATA].

content_xml(DOM, Text) :-
    with_output_to(atom(Text), xml_write(DOM, [header(false)])).

%!  complete_triples(+Context)
%
%   7.5.12: Complete incomplete triples

complete_triples(Context) :-
    Context.skip_element == false,
    Context.new_subject \== null,
    Context.eval_context.incomplete_triples \== [],
    !,
    reverse(Context.eval_context.incomplete_triples, Incomplete),
    maplist(complete_triple(Context), Incomplete).
complete_triples(_).

complete_triple(Context, Dict) :-
    complete_triple(Dict.direction, Dict, Context).

complete_triple(none, Dict, Context) :-
    List = Dict.list,
    List = list(Old),
    setarg(1, List, [Context.new_subject|Old]).
complete_triple(forward, Dict, Context) :-
    add_triple(Context,
               Context.eval_context.parent_subject,
               Dict.predicate,
               Context.new_subject).
complete_triple(reverse, Dict, Context) :-
    add_triple(Context,
               Context.new_subject,
               Dict.predicate,
               Context.eval_context.parent_subject).


%!  descent(DOM, Context)
%
%   7.5.13: Descent into the children

descent(element(_,_,Content), Context) :-
    (   Context.skip_element == true
    ->  maplist(descent_skip(Context), Content)
    ;   maplist(descent_no_skip(Context), Content)
    ).

descent_skip(Context, DOM) :-
    DOM = element(E,_,_),
    !,
    debug(rdfa(descent), 'skip: ~w: new_subject=~p',
          [E, Context.new_subject]),
    process_node(DOM, Context.eval_context.put(
                          _{ lang:Context.lang,
                             vocab:Context.vocab,
                             iri_mapping:Context.iri_mapping
                           })).
descent_skip(_, _).

descent_no_skip(Context, DOM) :-
    DOM = element(E,_,_),
    !,
    (   ParentSubject = Context.new_subject,
        ParentSubject \== null
    ->  true
    ;   ParentSubject = Context.eval_context.parent_subject
    ),
    (   ParentObject = Context.current_object_resource,
        ParentObject \== null
    ->  true
    ;   ParentObject = ParentSubject
    ),
    debug(rdfa(descent), 'no skip: ~w: parent subject = ~p, object = ~p',
          [E, ParentSubject, ParentObject]),
    process_node(DOM, Context.eval_context.put(
                          _{ parent_subject:ParentSubject,
                             parent_object:ParentObject,
                             iri_mapping:Context.iri_mapping,
                             incomplete_triples:Context.incomplete_triples,
                             list_mapping:Context.list_mapping,
                             lang:Context.lang,
                             vocab:Context.vocab
                            })).
descent_no_skip(_, _).

%!  complete_lists(+Context) is det.
%
%   7.5.14: Complete possibly pending lists

complete_lists(Context) :-
    empty_list_mapping(Context.list_mapping),
    !.
complete_lists(Context) :-
    (   CurrentSubject = Context.new_subject,
        CurrentSubject \== null
    ->  true
    ;   CurrentSubject = Context.eval_context.base
    ),
    list_mapping_pairs(Context.list_mapping, Pairs),
    maplist(complete_list(Context, CurrentSubject), Pairs).

complete_list(Context, _, IRI-_) :-
    get_list_mapping(IRI, Context.eval_context.list_mapping, _),
    !.
complete_list(Context, CurrentSubject, IRI-list(List0)) :-
    reverse(List0, List),
    emit_list(List, ListURI, Context),
    add_triple(Context, CurrentSubject, IRI, ListURI).

emit_list([], NIL, _) :-
    rdf_equal(NIL, rdf:nil).
emit_list([H|T], URI, Context) :-
    emit_list(T, TailURI, Context),
    new_bnode(URI, Context),
    add_triple(Context, URI, rdf:first, H),
    add_triple(Context, URI, rdf:rest, TailURI).


%!  has_attribute(+Name, +Attrs, +Context) is semidet.
%!  has_attribute(+Name, +Attrs, -Value, +Context) is semidet.
%
%   True if Attrs contains Name.  We sometimes need to ignore
%   Attributes if their value is invalid.
%
%   @see HTML+RDFa, 3.1 Additional RDFa Processing Rules, point 7.

has_attribute(Name, Attrs, Context) :-
    has_attribute(Name, Attrs, _, Context).

has_attribute(rel, Attrs, Rel, Context) :-
    memberchk(rel=Rel, Attrs),
    html_markup(Context.eval_context.markup),
    memberchk(property=_, Attrs),
    !,
    html_non_empty_rel(Rel, Context).
has_attribute(rev, Attrs, Rev, Context) :-
    memberchk(rev=Rev, Attrs),
    html_markup(Context.eval_context.markup),
    memberchk(property=_, Attrs),
    !,
    html_non_empty_rel(Rev, Context).
has_attribute(Name, Attrs, Value, _Context) :-
    memberchk(Name=Value, Attrs).

html_non_empty_rel(Spec, Context) :-
    Sep = "\s\t\n\r",
    split_string(Spec, Sep, Sep, SpecList),
    member(Spec1, SpecList),
    safe_curie_or_curie_or_absiri(Spec1, _, Context),
    !.


%!  iri_attr(+AttName, +Attrs, -IRI, +Context) is semidet.

iri_attr(Name, Attrs, IRI, Context) :-
    memberchk(Name=IRI0, Attrs),
    attr_convert(Name, IRI0, IRI, Context).

attr_convert(about, Spec, IRI, Context) :-
    safe_curie_or_curie_or_iri(Spec, IRI, Context).
attr_convert(href, Spec, IRI, Context) :-
    iri(Spec, IRI, Context).
attr_convert(src, Spec, IRI, Context) :-
    iri(Spec, IRI, Context).
attr_convert(resource, Spec, IRI, Context) :-
    safe_curie_or_curie_or_iri(Spec, IRI, Context).
attr_convert(vocab, Spec, IRI, Context) :-
    iri(Spec, IRI, Context).
attr_convert(datatype, Spec, IRI, Context) :-
    term_or_curie_or_absiri(Spec, IRI, Context).


about(DOM, About, Context) :-
    DOM=element(_,Attrs,_),
    (   memberchk(about=About0, Attrs)
    ->  safe_curie_or_curie_or_iri(About0, About, Context)
    ;   DOM == Context.eval_context.root
    ->  iri('', About, Context)
    ).

%!  new_bnode(-BNode, +Context) is det.
%
%   Create a new blank node. Note that the   current id is kept in a
%   term to avoid copying the counter on the descent step.

new_bnode(BNode, Context) :-
    EvalCtx = Context.eval_context,
    Node = EvalCtx.bnode_id,
    arg(1, Node, Id),
    succ(Id, Id1),
    nb_setarg(1, Node, Id1),
    Prefix = EvalCtx.anon_prefix,
    (   atom(Prefix)
    ->  atom_concat(Prefix, Id, BNode)
    ;   BNode = bnode(Id)
    ).

%!  iri_list(+Spec, -IRIs, +Context) is det.
%
%   True when IRIs is a list of fulfy qualified IRIs from Spec

iri_list(Spec, IRIs, Context) :-
    Sep = "\s\t\n\r",
    split_string(Spec, Sep, Sep, SpecList),
    (   SpecList == [""]
    ->  IRIs = []
    ;   maplist(ctx_to_iri(Context), SpecList, IRIs0),
        exclude(==(null), IRIs0, IRIs)
    ).

ctx_to_iri(Context, Spec, IRI) :-
    term_or_curie_or_absiri(Spec, IRI, Context).

%!  iri(+Spec, -IRI, +Context)
%
%   Used for @href and @src attributes

iri(Spec, IRI, Context) :-
    iri_normalized(Spec, Context.eval_context.base, IRI).

abs_iri(Spec, IRI) :-
    uri_components(Spec, Components),
    uri_data(authority, Components, Authority), nonvar(Authority),
    uri_data(scheme,    Components, Scheme),    nonvar(Scheme),
    !,
    iri_normalized(Spec, IRI).


%!  safe_curie_or_curie_or_iri(+Spec, -IRI, +Context) is det.
%
%   Implement section 7.4, CURIE and IRI Processing.  Used for
%   @about and @resource

safe_curie_or_curie_or_iri(Spec, IRI, Context) :-
    safe_curie_or_curie_or_absiri(Spec, IRI, Context),
    !.
safe_curie_or_curie_or_iri(Spec, IRI, Context) :-
    uri_normalized(Spec, Context.eval_context.base, IRI).

safe_curie_or_curie_or_absiri(Spec, IRI, _Context) :-
    abs_iri(Spec, IRI0),
    !,
    IRI = IRI0.
safe_curie_or_curie_or_absiri(Spec, IRI, Context) :-
    atom_codes(Spec, Codes),
    (   safe_curie(Codes, Curie)
    ->  (   phrase(curie(IRI, Context), Curie)
        ->  true
        ;   IRI = null
        )
    ;   phrase(curie(IRI, Context), Codes)
    ).

safe_curie(Codes, Curie) :-
    append([0'[|Curie], `]`, Codes).

curie(IRI, Context) -->
    "_:", !, reference_or_empty(Reference),
    {   IRI = Context.eval_context.named_bnodes.v.get(Reference)
    ->  true
    ;   new_bnode(IRI, Context),
        b_set_dict(v, Context.eval_context.named_bnodes,
                   Context.eval_context.named_bnodes.v.put(Reference, IRI))
    }.
curie(IRI, Context) -->
    ":", !, reference_or_empty(Reference),
    { atom_concat(Context.iri_mapping.get(''), Reference, IRI) }.
curie(IRI, Context) -->
    nc_name(Prefix), ":", !, reference_or_empty(Reference),
    {   atom_concat(Context.iri_mapping.get(Prefix), Reference, IRI0)
    ->  IRI = IRI0
    ;   rdfa_core_prefix(Prefix, URIPrefix)
    ->  atom_concat(URIPrefix, Reference, IRI)
    }.

%!  term_or_curie_or_absiri(+Spec, -IRI, +Context) is det.
%
%   Used for @datatype and @property, @typeof, @rel and @rev

term_or_curie_or_absiri(Spec, IRI, _Context) :-
    abs_iri(Spec, IRI0),
    !,
    IRI = IRI0.
term_or_curie_or_absiri(Spec, IRI, Context) :-
    atom_codes(Spec, Codes),
    (   phrase(term(Term), Codes),
        downcase_atom(Term, LwrCase)
    ->  (   Vocab = Context.vocab,
            Vocab \== ''
        ->  atom_concat(Vocab, Term, IRI)
        ;   term_iri(LwrCase, Context.eval_context.markup, IRI0)
        ->  IRI = IRI0
        ;   IRI = Context.term_mapping.get(Term)
        ->  true
        ;   dict_pairs(Context.term_mapping, _Tag, Pairs),
            member(TermCaps-IRI, Pairs),
            downcase_atom(TermCaps, LwrCase)
        ->  true
        ;   IRI = null
        )
    ;   phrase(curie(IRI, Context), Codes)
    ->  true
    ;   uri_normalized(Spec, Context.eval_context.base, IRI)
    ).

%!  term_iri(?Term, ?Markup, ?IRI)
%
%   @see http://www.w3.org/2011/rdfa-context/xhtml-rdfa-1.1

term_expansion(term_iri(Term, Markup), term_iri(Term, Markup, URI)) :-
    default_prefix_mapping(Markup, Prefix),
    atom_concat(Prefix, Term, URI).

term_iri(alternate,  xhtml).
term_iri(appendix,   xhtml).
term_iri(cite,       xhtml).
term_iri(bookmark,   xhtml).
term_iri(contents,   xhtml).
term_iri(chapter,    xhtml).
term_iri(copyright,  xhtml).
term_iri(first,      xhtml).
term_iri(glossary,   xhtml).
term_iri(help,       xhtml).
term_iri(icon,       xhtml).
term_iri(index,      xhtml).
term_iri(last,       xhtml).
term_iri(meta,       xhtml).
term_iri(next,       xhtml).
term_iri(prev,       xhtml).
term_iri(previous,   xhtml).
term_iri(section,    xhtml).
term_iri(start,      xhtml).
term_iri(stylesheet, xhtml).
term_iri(subsection, xhtml).
term_iri(top,        xhtml).
term_iri(up,         xhtml).
term_iri(p3pv1,      xhtml).

term_iri(describedby, _, 'http://www.w3.org/2007/05/powder-s#describedby').
term_iri(license,     _, 'http://www.w3.org/1999/xhtml/vocab#license').
term_iri(role,        _, 'http://www.w3.org/1999/xhtml/vocab#role').

                 /*******************************
                 *           GRAMMARS           *
                 *******************************/

prefixes(Dict0, Dict) -->
    ws, nc_name(Name), ws, ":", ws, reference(IRI), !, ws,
    prefixes(Dict0.put(Name,IRI), Dict).
prefixes(Dict, Dict) --> [].

ws --> ws1, !, ws.
ws --> [].

ws1 --> " ".
ws1 --> "\t".
ws1 --> "\r".
ws1 --> "\n".

nc_name(Name) -->
    [H], {nc_name_start_code(H)},
    nc_name_codes(Codes),
    { atom_codes(Name0, [H|Codes]),
      downcase_atom(Name0, Name)
    }.

%!  term(-Term)//
%
%   7.4.3

term(Term) -->
    [H], {nc_name_start_code(H)},
    term_codes(Codes),
    { atom_codes(Term, [H|Codes])
    }.


nc_name_codes([H|T]) --> nc_name_code(H), !, nc_name_codes(T).
nc_name_codes([]) --> [].

nc_name_code(H) --> [H], {nc_name_code(H)}.

term_codes([H|T]) --> term_code(H), !, term_codes(T).
term_codes([]) --> [].

term_code(H) --> [H], {term_code(H)}.

nc_name_start_code(0':) :- !, fail.
nc_name_start_code(C) :- xml_basechar(C), !.
nc_name_start_code(C) :- xml_ideographic(C).

nc_name_code(0':) :- !, fail.
nc_name_code(C) :- xml_basechar(C), !.
nc_name_code(C) :- xml_digit(C), !.
nc_name_code(C) :- xml_ideographic(C), !.
nc_name_code(C) :- xml_combining_char(C), !.
nc_name_code(C) :- xml_extender(C), !.

term_code(0'/) :- !.
term_code(C) :- nc_name_code(C).

reference(IRI) -->
    [H],
    reference_codes(T),
    { atom_codes(IRI, [H|T]) }.

reference_codes([])    --> ws1, !.
reference_codes([H|T]) --> [H], !, reference_codes(T).
reference_codes([]) --> [].

reference_or_empty(IRI) -->
    reference_codes(Codes),
    { atom_codes(IRI, Codes) }.


%!  date_time_type(+DateTime, -DataType) is semidet.
%
%   True when DataType is the  xsd   type  that  matches the lexical
%   representation of DateTime

date_time_type(DateTime, DataType) :-
    atom_codes(DateTime, Codes),
    phrase(date_time_type(DataType), Codes).

date_time_type(DT) --> duration,   !, { rdf_equal(DT, xsd:duration) }.
date_time_type(DT) --> date_time,  !, { rdf_equal(DT, xsd:dateTime) }.
date_time_type(DT) --> date,       !, { rdf_equal(DT, xsd:date) }.
date_time_type(DT) --> time,       !, { rdf_equal(DT, xsd:time) }.
date_time_type(DT) --> gyearmonth, !, { rdf_equal(DT, xsd:gYearMonth) }.
date_time_type(DT) --> gyear,      !, { rdf_equal(DT, xsd:gYear) }.

duration   --> opt_minus, "P",
    opt_dy, opt_dm, opt_dd,
    (   "T"
    ->  opt_dh, opt_dm, opt_ds
    ;   ""
    ).

date_time  --> opt_minus, yyyy, "-", !, mM, "-", dd,
    "T", hh, ":", mm, ":", ss, opt_fraction, opt_zzzzzz.
date       --> opt_minus, yyyy, "-", !, mM, "-", dd.
time       --> hh, ":", mm, ":", ss, opt_fraction.
gyearmonth --> opt_minus, yyyy, "-", !, mM.
gyear      --> opt_minus, yyyy.

opt_minus --> "-", !.
opt_minus --> "".

yyyy --> dnzs, d, d, d, d.

dnzs --> "".
dnzs --> dnz, dnzs.

opt_fraction --> ".", !, ds.
opt_fraction --> "".

mM --> d(V1), d(V2), { M is V1*10+V2, M >= 1, M =< 12 }.
dd --> d(V1), d(V2), { M is V1*10+V2, M >= 1, M =< 31 }.
hh --> d(V1), d(V2), { M is V1*10+V2, M =< 23 }.
mm --> d(V1), d(V2), { M is V1*10+V2, M =< 59 }.
ss --> d(V1), d(V2), { M is V1*10+V2, M =< 59 }.

d(V) --> [D], { between(0'0, 0'9, D), V is D-0'0 }.
d    --> [D], { between(0'0, 0'9, D) }.
dnz  --> [D], { between(0'1, 0'9, D) }.

ds --> d, !, ds.
ds --> "".

opt_zzzzzz --> sign, hh, ":", mm.
opt_zzzzzz --> "Z".
opt_zzzzzz --> "".

sign --> "+".
sign --> "-".

opt_dy --> ( int, "Y" | "" ).
opt_dm --> ( int, "M" | "" ).
opt_dd --> ( int, "D" | "" ).
opt_dh --> ( int, "H" | "" ).
opt_ds --> ( int, ("." -> int ; ""), "S" | "" ).

int --> d, ds.

                 /*******************************
                 *           TRIPLES            *
                 *******************************/

%!  add_triple(+Context, +S, +P, +O) is det.
%
%   Add a triple to  the  global   evaluation  context.  Triples are
%   embedded in a term, so we can   use  setarg/3 on the list, while
%   the  evaluation  context  is  copied  for  descending  the  node
%   hierarchy.

add_triple(Context, S, P, O) :-
    (   debugging(rdfa(triple))
    ->  debug(rdfa(triple), 'Added { ~p ~p ~p }', [S,P,O]),
        backtrace(4)
    ;   true
    ),
    valid_subject(S),
    valid_predicate(P),
    valid_object(O),
    !,
    Triples = Context.eval_context.triples,
    arg(1, Triples, Old),
    setarg(1, Triples, [rdf(S,P,O)|Old]).
add_triple(_, _, _, _).                 % ignored invalid triple.

valid_subject(S)   :- S \== null.
valid_predicate(P) :- P \== null, \+ rdf_is_bnode(P).
valid_object(O)    :- O \== null, ( atom(O) -> true ; valid_literal(O) ).

valid_literal(literal(Plain)) :-
    atom(Plain),
    !.
valid_literal(literal(type(T, _))) :-
    !,
    T \== null.
valid_literal(literal(lang(_,_))).

add_incomplete_triple(Context, Dict) :-
    debug(rdfa(incomplete), 'Incomplete: ~p', [Dict]),
    b_set_dict(incomplete_triples, Context,
               [ Dict
               | Context.incomplete_triples
               ]).


                 /*******************************
                 *            PATTERNS          *
                 *******************************/

%!  apply_patterns(+TriplesIn, -TriplesOut) is det.
%
%   Apply RDFa patterns.  We need several passes do deal with ordering
%   issues and the possibility that patterns are invalid:
%
%     1. find patterns from rdf(_,rdfa:copy,Pattern)
%     2. collect the properties for these patterns and delete
%        patterns that do not have rdf:type rdfa:Pattern.
%     3. Actually copy the patterns and delete the patterns themselves.

apply_patterns(TriplesIn, TriplesOut) :-
    referenced_patterns(TriplesIn, Pairs),
    (   Pairs == []
    ->  TriplesOut = TriplesIn
    ;   sort(Pairs, UniquePairs),
        dict_pairs(Dict, _, UniquePairs),
        pattern_properties(TriplesIn, Dict),
        delete_invalid_patterns(Dict, Patterns),
        phrase(apply_patterns(TriplesIn, Patterns), TriplesOut)
    ).

term_expansion(TIn, TOut) :-
    rdf_global_term(TIn, TOut).

referenced_patterns([], []).
referenced_patterns([rdf(_,rdfa:copy,O)|T0], [O-[]|T]) :-
    !,
    referenced_patterns(T0, T).
referenced_patterns([_|T0], T) :-
    referenced_patterns(T0, T).

pattern_properties([], _).
pattern_properties([rdf(S,P,O)|T], Dict) :-
    ignore(b_set_dict(S, Dict, [P-O|Dict.get(S)])),
    pattern_properties(T, Dict).

delete_invalid_patterns(Patterns0, Patterns) :-
    dict_pairs(Patterns0, Tag, Pairs0),
    include(rdfa_pattern, Pairs0, Pairs),
    dict_pairs(Patterns,  Tag, Pairs).

rdfa_pattern(_-PO) :-
    memberchk((rdf:type)-(rdfa:'Pattern'), PO).

apply_patterns([], _) --> [].
apply_patterns([rdf(S,rdfa:copy,O)|T0], Dict) -->
    !,
    copy_pattern(Dict.O, S),
    apply_patterns(T0, Dict).
apply_patterns([rdf(S,_,_)|T0], Dict) -->
    { _ = Dict.get(S) },
    !,
    apply_patterns(T0, Dict).
apply_patterns([H|T], Dict) -->
    [H],
    apply_patterns(T, Dict).

copy_pattern([], _) --> [].
copy_pattern([(rdf:type)-(rdfa:'Pattern')|T], S) -->
    !,
    copy_pattern(T, S).
copy_pattern([P-O|T], S) -->
    [rdf(S,P,O)],
    copy_pattern(T, S).


                 /*******************************
                 *       HOOK INTO RDF-DB       *
                 *******************************/

:- multifile
    rdf_db:rdf_load_stream/3,
    rdf_db:rdf_file_type/2.

%!  rdf_db:rdf_load_stream(+Format, +Stream, :Options)
%
%   Register library(semweb/rdfa) as loader for HTML RDFa files.
%
%   @tbd    Which options need to be forwarded to read_rdfa/3?

rdf_db:rdf_load_stream(rdfa, Stream, _Module:Options1):-
    rdf_db:graph(Options1, Graph),
    atom_concat('__', Graph, BNodePrefix),
    merge_options([anon_prefix(BNodePrefix)], Options1, Options2),
    read_rdfa(Stream, Triples, Options2),
    rdf_transaction(( forall(member(rdf(S,P,O), Triples),
                             rdf_assert(S, P, O, Graph)),
                      rdf_set_graph(Graph, modified(false))
                    ),
                    parse(Graph)).

rdf_db:rdf_file_type(html, rdfa).

