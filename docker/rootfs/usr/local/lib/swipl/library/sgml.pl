/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2020, University of Amsterdam
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

:- module(sgml,
          [ load_html/3,                % +Input, -DOM, +Options
            load_xml/3,                 % +Input, -DOM, +Options
            load_sgml/3,                % +Input, -DOM, +Options

            load_sgml_file/2,           % +File, -ListOfContent
            load_xml_file/2,            % +File, -ListOfContent
            load_html_file/2,           % +File, -Document

            load_structure/3,           % +File, -Term, +Options

            load_dtd/2,                 % +DTD, +File
            load_dtd/3,                 % +DTD, +File, +Options
            dtd/2,                      % +Type, -DTD
            dtd_property/2,             % +DTD, ?Property

            new_dtd/2,                  % +Doctype, -DTD
            free_dtd/1,                 % +DTD
            open_dtd/3,                 % +DTD, +Options, -Stream

            new_sgml_parser/2,          % -Parser, +Options
            free_sgml_parser/1,         % +Parser
            set_sgml_parser/2,          % +Parser, +Options
            get_sgml_parser/2,          % +Parser, +Options
            sgml_parse/2,               % +Parser, +Options

            sgml_register_catalog_file/2, % +File, +StartOrEnd

            xml_quote_attribute/3,      % +In, -Quoted, +Encoding
            xml_quote_cdata/3,          % +In, -Quoted, +Encoding
            xml_quote_attribute/2,      % +In, -Quoted
            xml_quote_cdata/2,          % +In, -Quoted
            xml_name/1,                 % +In
            xml_name/2,                 % +In, +Encoding

            xsd_number_string/2,        % ?Number, ?String
            xsd_time_string/3,          % ?Term, ?Type, ?String

            xml_basechar/1,             % +Code
            xml_ideographic/1,          % +Code
            xml_combining_char/1,       % +Code
            xml_digit/1,                % +Code
            xml_extender/1,             % +Code

            iri_xml_namespace/2,        % +IRI, -Namespace
            iri_xml_namespace/3,        % +IRI, -Namespace, -LocalName
            xml_is_dom/1                % +Term
          ]).
:- autoload(library(error),[instantiation_error/1]).
:- autoload(library(iostream),[open_any/5,close_any/1]).
:- autoload(library(lists),[member/2,selectchk/3]).
:- autoload(library(option),[select_option/3,merge_options/3]).

:- meta_predicate
    load_structure(+, -, :),
    load_html(+, -, :),
    load_xml(+, -, :),
    load_sgml(+, -, :).

:- predicate_options(load_structure/3, 3,
                     [ charpos(integer),
                       cdata(oneof([atom,string])),
                       defaults(boolean),
                       dialect(oneof([html,html4,html5,sgml,xhtml,xhtml5,xml,xmlns])),
                       doctype(atom),
                       dtd(any),
                       encoding(oneof(['iso-8859-1', 'utf-8', 'us-ascii'])),
                       entity(atom,atom),
                       keep_prefix(boolean),
                       file(atom),
                       line(integer),
                       offset(integer),
                       number(oneof([token,integer])),
                       qualify_attributes(boolean),
                       shorttag(boolean),
                       case_sensitive_attributes(boolean),
                       case_preserving_attributes(boolean),
                       system_entities(boolean),
                       max_memory(integer),
                       ignore_doctype(boolean),
                       space(oneof([sgml,preserve,default,remove,strict])),
                       xmlns(atom),
                       xmlns(atom,atom),
                       pass_to(sgml_parse/2, 2)
                     ]).
:- predicate_options(load_html/3, 3,
                     [ pass_to(load_structure/3, 3)
                     ]).
:- predicate_options(load_xml/3, 3,
                     [ pass_to(load_structure/3, 3)
                     ]).
:- predicate_options(load_sgml/3, 3,
                     [ pass_to(load_structure/3, 3)
                     ]).
:- predicate_options(load_dtd/3, 3,
                     [ dialect(oneof([sgml,xml,xmlns])),
                       pass_to(open/4, 4)
                     ]).
:- predicate_options(sgml_parse/2, 2,
                     [ call(oneof([begin,end,cdata,pi,decl,error,xmlns,urlns]),
                            callable),
                       cdata(oneof([atom,string])),
                       content_length(integer),
                       document(-any),
                       max_errors(integer),
                       parse(oneof([file,element,content,declaration,input])),
                       source(any),
                       syntax_errors(oneof([quiet,print,style])),
                       xml_no_ns(oneof([error,quiet]))
                     ]).
:- predicate_options(new_sgml_parser/2, 2,
                     [ dtd(any)
                     ]).


/** <module> SGML, XML and HTML parser

This library allows you to parse SGML, XML   and HTML data into a Prolog
data structure. The library defines several families of predicates:

  $ High-level predicates :
  Most users will only use load_html/3, load_xml/3 or load_sgml/3 to
  parse arbitrary input into a _DOM_ structure.  These predicates all
  call load_structure/3, which provides more options and may be
  used for processing non-standard documents.

  The DOM structure can be used by library(xpath) to extract information
  from the document.

  $ The low-level parser :
  The actual parser is written in C and consists of two parts: one for
  processing DTD (Document Type Definitions) and one for parsing data.
  The data can either be parsed to a Prolog (_DOM_) term or the parser
  can perform callbacks for the DOM _events_.

  $ Utility predicates :
  Finally, this library provides prmitives for classifying characters
  and strings according to the XML specification such as xml_name/1 to
  verify whether an atom is a valid XML name (identifier).  It also
  provides primitives to quote attributes and CDATA elements.
*/

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(dtd, '.').
user:file_search_path(dtd, swi('library/DTD')).

sgml_register_catalog_file(File, Location) :-
    prolog_to_os_filename(File, OsFile),
    '_sgml_register_catalog_file'(OsFile, Location).

:- use_foreign_library(foreign(sgml2pl)).

register_catalog(Base) :-
    absolute_file_name(dtd(Base),
                           [ extensions([soc]),
                             access(read),
                             file_errors(fail)
                           ],
                           SocFile),
    sgml_register_catalog_file(SocFile, end).

:- initialization
    ignore(register_catalog('HTML4')).


                 /*******************************
                 *         DTD HANDLING         *
                 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that concurrent access to DTD objects  is not allowed, and hence we
will allocate and destroy them in each   thread.  Possibibly it would be
nicer to find out why  concurrent  access   to  DTD's  is  flawed. It is
diagnosed to mess with the entity resolution by Fabien Todescato.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- thread_local
    current_dtd/2.
:- volatile
    current_dtd/2.
:- thread_local
    registered_cleanup/0.
:- volatile
    registered_cleanup/0.

:- multifile
    dtd_alias/2.

:- create_prolog_flag(html_dialect, html5, [type(atom)]).

dtd_alias(html4, 'HTML4').
dtd_alias(html5, 'HTML5').
dtd_alias(html,  DTD) :-
    current_prolog_flag(html_dialect, Dialect),
    dtd_alias(Dialect, DTD).

%!  dtd(+Type, -DTD) is det.
%
%   DTD is a DTD object created from  the file dtd(Type). Loaded DTD
%   objects are cached. Note that  DTD   objects  may  not be shared
%   between threads. Therefore, dtd/2  maintains   the  pool  of DTD
%   objects  using  a  thread_local  predicate.    DTD  objects  are
%   destroyed if a thread terminates.
%
%   @error existence_error(source_sink, dtd(Type))

dtd(Type, DTD) :-
    current_dtd(Type, DTD),
    !.
dtd(Type, DTD) :-
    new_dtd(Type, DTD),
    (   dtd_alias(Type, Base)
    ->  true
    ;   Base = Type
    ),
    absolute_file_name(dtd(Base),
                       [ extensions([dtd]),
                         access(read)
                       ], DtdFile),
    load_dtd(DTD, DtdFile),
    register_cleanup,
    asserta(current_dtd(Type, DTD)).

%!  load_dtd(+DTD, +DtdFile, +Options)
%
%   Load DtdFile into a DTD.  Defined options are:
%
%           * dialect(+Dialect)
%           Dialect to use (xml, xmlns, sgml)
%
%           * encoding(+Encoding)
%           Encoding of DTD file
%
%   @param  DTD is a fresh DTD object, normally created using
%           new_dtd/1.

load_dtd(DTD, DtdFile) :-
    load_dtd(DTD, DtdFile, []).
load_dtd(DTD, DtdFile, Options) :-
    sgml_open_options(sgml:Options, OpenOptions, sgml:DTDOptions),
    setup_call_cleanup(
        open_dtd(DTD, DTDOptions, DtdOut),
        setup_call_cleanup(
            open(DtdFile, read, DtdIn, OpenOptions),
            copy_stream_data(DtdIn, DtdOut),
            close(DtdIn)),
        close(DtdOut)).

%!  destroy_dtds
%
%   Destroy  DTDs  cached  by  this  thread   as  they  will  become
%   unreachable anyway.

destroy_dtds :-
    (   current_dtd(_Type, DTD),
        free_dtd(DTD),
        fail
    ;   true
    ).

%!  register_cleanup
%
%   Register cleanup of DTDs created for this thread.

register_cleanup :-
    registered_cleanup,
    !.
register_cleanup :-
    (   thread_self(main)
    ->  at_halt(destroy_dtds)
    ;   current_prolog_flag(threads, true)
    ->  prolog_listen(this_thread_exit, destroy_dtds)
    ;   true
    ),
    assert(registered_cleanup).


                 /*******************************
                 *          EXAMINE DTD         *
                 *******************************/

prop(doctype(_), _).
prop(elements(_), _).
prop(entities(_), _).
prop(notations(_), _).
prop(entity(E, _), DTD) :-
    (   nonvar(E)
    ->  true
    ;   '$dtd_property'(DTD, entities(EL)),
        member(E, EL)
    ).
prop(element(E, _, _), DTD) :-
    (   nonvar(E)
    ->  true
    ;   '$dtd_property'(DTD, elements(EL)),
        member(E, EL)
    ).
prop(attributes(E, _), DTD) :-
    (   nonvar(E)
    ->  true
    ;   '$dtd_property'(DTD, elements(EL)),
        member(E, EL)
    ).
prop(attribute(E, A, _, _), DTD) :-
    (   nonvar(E)
    ->  true
    ;   '$dtd_property'(DTD, elements(EL)),
        member(E, EL)
    ),
    (   nonvar(A)
    ->  true
    ;   '$dtd_property'(DTD, attributes(E, AL)),
        member(A, AL)
    ).
prop(notation(N, _), DTD) :-
    (   nonvar(N)
    ->  true
    ;   '$dtd_property'(DTD, notations(NL)),
        member(N, NL)
    ).

dtd_property(DTD, Prop) :-
    prop(Prop, DTD),
    '$dtd_property'(DTD, Prop).


                 /*******************************
                 *             SGML             *
                 *******************************/

%!  load_structure(+Source, -ListOfContent, :Options) is det.
%
%   Parse   Source   and   return   the   resulting   structure   in
%   ListOfContent. Source is handed to  open_any/5, which allows for
%   processing an extensible set of input sources.
%
%   A proper XML document contains only   a  single toplevel element
%   whose name matches the document type.   Nevertheless,  a list is
%   returned for consistency with  the   representation  of  element
%   content.
%
%   The  encoding(+Encoding)  option   is    treated   special   for
%   compatibility reasons:
%
%     - If `Encoding` is one of =iso-8859-1=, =us-ascii= or =utf-8=,
%       the stream is opened in binary mode and the option is passed
%       to the SGML parser.
%     - If `Encoding` is present, but not one of the above, the
%       stream is opened in text mode using the given encoding.
%     - Otherwise (no `Encoding`), the stream is opened in binary
%       mode and doing the correct decoding is left to the parser.

load_structure(Spec, DOM, Options) :-
    sgml_open_options(Options, OpenOptions, SGMLOptions),
    setup_call_cleanup(
        open_any(Spec, read, In, Close, OpenOptions),
        load_structure_from_stream(In, DOM, SGMLOptions),
        close_any(Close)).

sgml_open_options(Options, OpenOptions, SGMLOptions) :-
    Options = M:Plain,
    (   select_option(encoding(Encoding), Plain, NoEnc)
    ->  (   sgml_encoding(Encoding)
        ->  merge_options(NoEnc, [type(binary)], OpenOptions),
            SGMLOptions = Options
        ;   OpenOptions = Plain,
            SGMLOptions = M:NoEnc
        )
    ;   merge_options(Plain, [type(binary)], OpenOptions),
        SGMLOptions = Options
    ).

sgml_encoding(Enc) :-
    downcase_atom(Enc, Enc1),
    sgml_encoding_l(Enc1).

sgml_encoding_l('iso-8859-1').
sgml_encoding_l('us-ascii').
sgml_encoding_l('utf-8').
sgml_encoding_l('utf8').
sgml_encoding_l('iso_latin_1').
sgml_encoding_l('ascii').

load_structure_from_stream(In, Term, M:Options) :-
    (   select_option(dtd(DTD), Options, Options1)
    ->  ExplicitDTD = true
    ;   ExplicitDTD = false,
        Options1 = Options
    ),
    move_front(Options1, dialect(_), Options2), % dialect sets defaults
    setup_call_cleanup(
        new_sgml_parser(Parser,
                        [ dtd(DTD)
                        ]),
        parse(Parser, M:Options2, TermRead, In),
        free_sgml_parser(Parser)),
    (   ExplicitDTD == true
    ->  (   DTD = dtd(_, DocType),
            dtd_property(DTD, doctype(DocType))
        ->  true
        ;   true
        )
    ;   free_dtd(DTD)
    ),
    Term = TermRead.

move_front(Options0, Opt, Options) :-
    selectchk(Opt, Options0, Options1),
    !,
    Options = [Opt|Options1].
move_front(Options, _, Options).


parse(Parser, M:Options, Document, In) :-
    set_parser_options(Options, Parser, In, Options1),
    parser_meta_options(Options1, M, Options2),
    set_input_location(Parser, In),
    sgml_parse(Parser,
               [ document(Document),
                 source(In)
               | Options2
               ]).

set_parser_options([], _, _, []).
set_parser_options([H|T], Parser, In, Rest) :-
    (   set_parser_option(H, Parser, In)
    ->  set_parser_options(T, Parser, In, Rest)
    ;   Rest = [H|R2],
        set_parser_options(T, Parser, In, R2)
    ).

set_parser_option(Var, _Parser, _In) :-
    var(Var),
    !,
    instantiation_error(Var).
set_parser_option(Option, Parser, _) :-
    def_entity(Option, Parser),
    !.
set_parser_option(offset(Offset), _Parser, In) :-
    !,
    seek(In, Offset, bof, _).
set_parser_option(Option, Parser, _In) :-
    parser_option(Option),
    !,
    set_sgml_parser(Parser, Option).
set_parser_option(Name=Value, Parser, In) :-
    Option =.. [Name,Value],
    set_parser_option(Option, Parser, In).


parser_option(dialect(_)).
parser_option(shorttag(_)).
parser_option(case_sensitive_attributes(_)).
parser_option(case_preserving_attributes(_)).
parser_option(system_entities(_)).
parser_option(max_memory(_)).
parser_option(ignore_doctype(_)).
parser_option(file(_)).
parser_option(line(_)).
parser_option(space(_)).
parser_option(number(_)).
parser_option(defaults(_)).
parser_option(doctype(_)).
parser_option(qualify_attributes(_)).
parser_option(encoding(_)).
parser_option(keep_prefix(_)).


def_entity(entity(Name, Value), Parser) :-
    get_sgml_parser(Parser, dtd(DTD)),
    xml_quote_attribute(Value, QValue),
    setup_call_cleanup(open_dtd(DTD, [], Stream),
                       format(Stream, '<!ENTITY ~w "~w">~n',
                              [Name, QValue]),
                       close(Stream)).
def_entity(xmlns(URI), Parser) :-
    set_sgml_parser(Parser, xmlns(URI)).
def_entity(xmlns(NS, URI), Parser) :-
    set_sgml_parser(Parser, xmlns(NS, URI)).

%!  parser_meta_options(+Options0, +Module, -Options)
%
%   Qualify meta-calling options to the parser.

parser_meta_options([], _, []).
parser_meta_options([call(When, Closure)|T0], M, [call(When, M:Closure)|T]) :-
    !,
    parser_meta_options(T0, M, T).
parser_meta_options([H|T0], M, [H|T]) :-
    parser_meta_options(T0, M, T).


%!  set_input_location(+Parser, +In:stream) is det.
%
%   Set the input location if this was not set explicitly

set_input_location(Parser, _In) :-
    get_sgml_parser(Parser, file(_)),
    !.
set_input_location(Parser, In) :-
    stream_property(In, file_name(File)),
    !,
    set_sgml_parser(Parser, file(File)),
    stream_property(In, position(Pos)),
    set_sgml_parser(Parser, position(Pos)).
set_input_location(_, _).

                 /*******************************
                 *           UTILITIES          *
                 *******************************/

%!  load_sgml_file(+File, -DOM) is det.
%
%   Load SGML from File and unify   the resulting DOM structure with
%   DOM.
%
%   @deprecated     New code should use load_sgml/3.

load_sgml_file(File, Term) :-
    load_sgml(File, Term, []).

%!  load_xml_file(+File, -DOM) is det.
%
%   Load XML from File and unify   the  resulting DOM structure with
%   DOM.
%
%   @deprecated     New code should use load_xml/3.

load_xml_file(File, Term) :-
    load_xml(File, Term, []).

%!  load_html_file(+File, -DOM) is det.
%
%   Load HTML from File and unify   the resulting DOM structure with
%   DOM.
%
%   @deprecated     New code should use load_html/3.

load_html_file(File, DOM) :-
    load_html(File, DOM, []).

%!  load_html(+Input, -DOM, +Options) is det.
%
%   Load HTML text from Input and  unify the resulting DOM structure
%   with DOM. Options are passed   to load_structure/3, after adding
%   the following default options:
%
%     - dtd(DTD)
%     Pass the DTD for HTML as obtained using dtd(html, DTD).
%     - dialect(Dialect)
%     Current dialect from the Prolog flag =html_dialect=
%     - max_errors(-1)
%     - syntax_errors(quiet)
%     Most HTML encountered in the wild contains errors. Even in the
%     context of errors, the resulting DOM term is often a
%     reasonable guess at the intent of the author.
%
%   You may also want to use  the library(http/http_open) to support
%   loading from HTTP and HTTPS URLs. For example:
%
%   ==
%   :- use_module(library(http/http_open)).
%   :- use_module(library(sgml)).
%
%   load_html_url(URL, DOM) :-
%       load_html(URL, DOM, []).
%   ==

load_html(File, Term, M:Options) :-
    current_prolog_flag(html_dialect, Dialect),
    dtd(Dialect, DTD),
    merge_options(Options,
                  [ dtd(DTD),
                    dialect(Dialect),
                    max_errors(-1),
                    syntax_errors(quiet)
                  ], Options1),
    load_structure(File, Term, M:Options1).

%!  load_xml(+Input, -DOM, +Options) is det.
%
%   Load XML text from Input and   unify the resulting DOM structure
%   with DOM. Options are passed   to load_structure/3, after adding
%   the following default options:
%
%     - dialect(xml)

load_xml(Input, DOM, M:Options) :-
    merge_options(Options,
                  [ dialect(xml)
                  ], Options1),
    load_structure(Input, DOM, M:Options1).

%!  load_sgml(+Input, -DOM, +Options) is det.
%
%   Load SGML text from Input and  unify the resulting DOM structure
%   with DOM. Options are passed   to load_structure/3, after adding
%   the following default options:
%
%     - dialect(sgml)

load_sgml(Input, DOM, M:Options) :-
    merge_options(Options,
                  [ dialect(sgml)
                  ], Options1),
    load_structure(Input, DOM, M:Options1).



                 /*******************************
                 *            ENCODING          *
                 *******************************/

%!  xml_quote_attribute(+In, -Quoted) is det.
%!  xml_quote_cdata(+In, -Quoted) is det.
%
%   Backward  compatibility  for  versions  that  allow  to  specify
%   encoding. All characters that cannot fit the encoding are mapped
%   to XML character entities (&#dd;).  Using   ASCII  is the safest
%   value.

xml_quote_attribute(In, Quoted) :-
    xml_quote_attribute(In, Quoted, ascii).

xml_quote_cdata(In, Quoted) :-
    xml_quote_cdata(In, Quoted, ascii).

%!  xml_name(+Atom) is semidet.
%
%   True if Atom is a valid XML name.

xml_name(In) :-
    xml_name(In, ascii).


                 /*******************************
                 *    XML CHARACTER CLASSES     *
                 *******************************/

%!  xml_basechar(+CodeOrChar) is semidet.
%!  xml_ideographic(+CodeOrChar) is semidet.
%!  xml_combining_char(+CodeOrChar) is semidet.
%!  xml_digit(+CodeOrChar) is semidet.
%!  xml_extender(+CodeOrChar) is semidet.
%
%   XML  character  classification   predicates.    Each   of  these
%   predicates accept both a character   (one-character  atom) and a
%   code (integer).
%
%   @see http://www.w3.org/TR/2006/REC-xml-20060816


                 /*******************************
                 *         TYPE CHECKING        *
                 *******************************/

%!  xml_is_dom(@Term) is semidet.
%
%   True  if  term  statisfies   the    structure   as  returned  by
%   load_structure/3 and friends.

xml_is_dom(0) :- !, fail.               % catch variables
xml_is_dom(List) :-
    is_list(List),
    !,
    xml_is_content_list(List).
xml_is_dom(Term) :-
    xml_is_element(Term).

xml_is_content_list([]).
xml_is_content_list([H|T]) :-
    xml_is_content(H),
    xml_is_content_list(T).

xml_is_content(0) :- !, fail.
xml_is_content(pi(Pi)) :-
    !,
    atom(Pi).
xml_is_content(CDATA) :-
    atom(CDATA),
    !.
xml_is_content(CDATA) :-
    string(CDATA),
    !.
xml_is_content(Term) :-
    xml_is_element(Term).

xml_is_element(element(Name, Attributes, Content)) :-
    dom_name(Name),
    dom_attributes(Attributes),
    xml_is_content_list(Content).

dom_name(NS:Local) :-
    atom(NS),
    atom(Local),
    !.
dom_name(Local) :-
    atom(Local).

dom_attributes(0) :- !, fail.
dom_attributes([]).
dom_attributes([H|T]) :-
    dom_attribute(H),
    dom_attributes(T).

dom_attribute(Name=Value) :-
    dom_name(Name),
    atomic(Value).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/
:- multifile
    prolog:message/3.

%       Catch messages.  sgml/4 is generated by the SGML2PL binding.

prolog:message(sgml(Parser, File, Line, Message)) -->
    { get_sgml_parser(Parser, dialect(Dialect))
    },
    [ 'SGML2PL(~w): ~w:~w: ~w'-[Dialect, File, Line, Message] ].


                 /*******************************
                 *         XREF SUPPORT         *
                 *******************************/

:- multifile
    prolog:called_by/2.

prolog:called_by(sgml_parse(_, Options), Called) :-
    findall(Meta, meta_call_term(_, Meta, Options), Called).

meta_call_term(T, G+N, Options) :-
    T = call(Event, G),
    pmember(T, Options),
    call_params(Event, Term),
    functor(Term, _, N).

pmember(X, List) :-                     % member for partial lists
    nonvar(List),
    List = [H|T],
    (   X = H
    ;   pmember(X, T)
    ).

call_params(begin, begin(tag,attributes,parser)).
call_params(end,   end(tag,parser)).
call_params(cdata, cdata(cdata,parser)).
call_params(pi,    pi(cdata,parser)).
call_params(decl,  decl(cdata,parser)).
call_params(error, error(severity,message,parser)).
call_params(xmlns, xmlns(namespace,url,parser)).
call_params(urlns, urlns(url,url,parser)).

                 /*******************************
                 *           SANDBOX            *
                 *******************************/

:- multifile
    sandbox:safe_primitive/1,
    sandbox:safe_meta_predicate/1.

sandbox:safe_meta_predicate(sgml:load_structure/3).
sandbox:safe_primitive(sgml:dtd(Dialect, _)) :-
    dtd_alias(Dialect, _).
sandbox:safe_primitive(sgml:xml_quote_attribute(_,_,_)).
sandbox:safe_primitive(sgml:xml_quote_cdata(_,_,_)).
sandbox:safe_primitive(sgml:xml_name(_,_)).
sandbox:safe_primitive(sgml:xml_basechar(_)).
sandbox:safe_primitive(sgml:xml_ideographic(_)).
sandbox:safe_primitive(sgml:xml_combining_char(_)).
sandbox:safe_primitive(sgml:xml_digit(_)).
sandbox:safe_primitive(sgml:xml_extender(_)).
sandbox:safe_primitive(sgml:iri_xml_namespace(_,_,_)).
sandbox:safe_primitive(sgml:xsd_number_string(_,_)).
sandbox:safe_primitive(sgml:xsd_time_string(_,_,_)).
