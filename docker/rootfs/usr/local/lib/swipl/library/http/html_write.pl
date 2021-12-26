/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2020, University of Amsterdam
                              VU University Amsterdam
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

:- module(html_write,
          [ reply_html_page/2,          % :Head, :Body
            reply_html_page/3,          % +Style, :Head, :Body

                                        % Basic output routines
            page//1,                    % :Content
            page//2,                    % :Head, :Body
            page//3,                    % +Style, :Head, :Body
            html//1,                    % :Content

                                        % Option processing
            html_set_options/1,         % +OptionList
            html_current_option/1,      % ?Option

                                        % repositioning HTML elements
            html_post//2,               % +Id, :Content
            html_receive//1,            % +Id
            html_receive//2,            % +Id, :Handler
            xhtml_ns//2,                % +Id, +Value
            html_root_attribute//2,     % +Name, +Value

            html/4,                     % {|html||quasi quotations|}

                                        % Useful primitives for expanding
            html_begin//1,              % +EnvName[(Attribute...)]
            html_end//1,                % +EnvName
            html_quoted//1,             % +Text
            html_quoted_attribute//1,   % +Attribute

                                        % Emitting the HTML code
            print_html/1,               % +List
            print_html/2,               % +Stream, +List
            html_print_length/2,        % +List, -Length

                                        % Extension support
            (html_meta)/1,              % +Spec
            op(1150, fx, html_meta)
          ]).
:- use_module(html_quasiquotations, [html/4]).
:- autoload(library(apply),[maplist/3,maplist/4]).
:- autoload(library(debug),[debug/3]).
:- autoload(library(error),
	    [must_be/2,domain_error/2,instantiation_error/1]).
:- autoload(library(lists),
	    [permutation/2,selectchk/3,append/3,select/4,list_to_set/2]).
:- autoload(library(option),[option/2]).
:- autoload(library(pairs),[group_pairs_by_key/2]).
:- autoload(library(sgml),[xml_quote_cdata/3,xml_quote_attribute/3]).
:- autoload(library(uri),[uri_encoded/3]).
:- autoload(library(url),[www_form_encode/2]).
:- autoload(library(http/http_dispatch), [http_location_by_id/2]).

% Quote output
:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
    reply_html_page(+, :, :),
    reply_html_page(:, :),
    html(:, -, +),
    page(:, -, +),
    page(:, :, -, +),
    pagehead(+, :, -, +),
    pagebody(+, :, -, +),
    html_receive(+, 3, -, +),
    html_post(+, :, -, +).

:- multifile
    expand//1,                      % +HTMLElement
    expand_attribute_value//1.      % +HTMLAttributeValue


/** <module> Write HTML text

Most   code   doesn't   need  to   use  this   directly;  instead   use
library(http/http_server),  which  combines   this  library  with   the
typical HTTP libraries that most servers need.

The purpose of this library  is  to   simplify  writing  HTML  pages. Of
course, it is possible to  use  format/3   to  write  to the HTML stream
directly, but this is generally not very satisfactory:

        * It is a lot of typing
        * It does not guarantee proper HTML syntax.  You have to deal
          with HTML quoting, proper nesting and reasonable layout.
        * It is hard to use satisfactory abstraction

This module tries to remedy these problems.   The idea is to translate a
Prolog term into  an  HTML  document.  We   use  DCG  for  most  of  the
generation.

---++ International documents

The library supports the generation of international documents, but this
is currently limited to using UTF-8 encoded HTML or XHTML documents.  It
is strongly recommended to use the following mime-type.

==
Content-type: text/html; charset=UTF-8
==

When generating XHTML documents, the output stream must be in UTF-8
encoding.
*/


                 /*******************************
                 *            SETTINGS          *
                 *******************************/

%!  html_set_options(+Options) is det.
%
%   Set options for the HTML output.   Options  are stored in prolog
%   flags to ensure proper multi-threaded behaviour where setting an
%   option is local to the thread  and   new  threads start with the
%   options from the parent thread. Defined options are:
%
%     * dialect(Dialect)
%       One of =html4=, =xhtml= or =html5= (default). For
%       compatibility reasons, =html= is accepted as an
%       alias for =html4=.
%
%     * doctype(+DocType)
%       Set the =|<|DOCTYPE|= DocType =|>|= line for page//1 and
%       page//2.
%
%     * content_type(+ContentType)
%       Set the =|Content-type|= for reply_html_page/3
%
%   Note that the doctype and  content_type   flags  are  covered by
%   distinct  prolog  flags:  =html4_doctype=,  =xhtml_doctype=  and
%   =html5_doctype= and similar for the   content  type. The Dialect
%   must be switched before doctype and content type.

html_set_options(Options) :-
    must_be(list, Options),
    set_options(Options).

set_options([]).
set_options([H|T]) :-
    html_set_option(H),
    set_options(T).

html_set_option(dialect(Dialect0)) :-
    !,
    must_be(oneof([html,html4,xhtml,html5]), Dialect0),
    (   html_version_alias(Dialect0, Dialect)
    ->  true
    ;   Dialect = Dialect0
    ),
    set_prolog_flag(html_dialect, Dialect).
html_set_option(doctype(Atom)) :-
    !,
    must_be(atom, Atom),
    current_prolog_flag(html_dialect, Dialect),
    dialect_doctype_flag(Dialect, Flag),
    set_prolog_flag(Flag, Atom).
html_set_option(content_type(Atom)) :-
    !,
    must_be(atom, Atom),
    current_prolog_flag(html_dialect, Dialect),
    dialect_content_type_flag(Dialect, Flag),
    set_prolog_flag(Flag, Atom).
html_set_option(O) :-
    domain_error(html_option, O).

html_version_alias(html, html4).

%!  html_current_option(?Option) is nondet.
%
%   True if Option is an active option for the HTML generator.

html_current_option(dialect(Dialect)) :-
    current_prolog_flag(html_dialect, Dialect).
html_current_option(doctype(DocType)) :-
    current_prolog_flag(html_dialect, Dialect),
    dialect_doctype_flag(Dialect, Flag),
    current_prolog_flag(Flag, DocType).
html_current_option(content_type(ContentType)) :-
    current_prolog_flag(html_dialect, Dialect),
    dialect_content_type_flag(Dialect, Flag),
    current_prolog_flag(Flag, ContentType).

dialect_doctype_flag(html4, html4_doctype).
dialect_doctype_flag(html5, html5_doctype).
dialect_doctype_flag(xhtml, xhtml_doctype).

dialect_content_type_flag(html4, html4_content_type).
dialect_content_type_flag(html5, html5_content_type).
dialect_content_type_flag(xhtml, xhtml_content_type).

option_default(html_dialect, html5).
option_default(html4_doctype,
               'HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" \c
               "http://www.w3.org/TR/html4/loose.dtd"').
option_default(html5_doctype,
               'html').
option_default(xhtml_doctype,
               'html PUBLIC "-//W3C//DTD XHTML 1.0 \c
               Transitional//EN" \c
               "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"').
option_default(html4_content_type, 'text/html; charset=UTF-8').
option_default(html5_content_type, 'text/html; charset=UTF-8').
option_default(xhtml_content_type, 'application/xhtml+xml; charset=UTF-8').

%!  init_options is det.
%
%   Initialise the HTML processing options.

init_options :-
    (   option_default(Name, Value),
        (   current_prolog_flag(Name, _)
        ->  true
        ;   create_prolog_flag(Name, Value, [])
        ),
        fail
    ;   true
    ).

:- init_options.

%!  xml_header(-Header)
%
%   First line of XHTML document.  Added by print_html/1.

xml_header('<?xml version=\'1.0\' encoding=\'UTF-8\'?>').

%!  ns(?Which, ?Atom)
%
%   Namespace declarations

ns(xhtml, 'http://www.w3.org/1999/xhtml').


                 /*******************************
                 *             PAGE             *
                 *******************************/

%!  page(+Content:dom)// is det.
%!  page(+Head:dom, +Body:dom)// is det.
%
%   Generate a page including the   HTML  =|<!DOCTYPE>|= header. The
%   actual doctype is read from the   option =doctype= as defined by
%   html_set_options/1.

page(Content) -->
    doctype,
    html(html(Content)).

page(Head, Body) -->
    page(default, Head, Body).

page(Style, Head, Body) -->
    doctype,
    content_type,
    html_begin(html),
    pagehead(Style, Head),
    pagebody(Style, Body),
    html_end(html).

%!  doctype//
%
%   Emit the =|<DOCTYPE ...|= header.  The   doctype  comes from the
%   option doctype(DOCTYPE) (see html_set_options/1).   Setting  the
%   doctype to '' (empty  atom)   suppresses  the header completely.
%   This is to avoid a IE bug in processing AJAX output ...

doctype -->
    { html_current_option(doctype(DocType)),
      DocType \== ''
    },
    !,
    [ '<!DOCTYPE ', DocType, '>' ].
doctype -->
    [].

content_type -->
    { html_current_option(content_type(Type))
    },
    !,
    html_post(head, meta([ 'http-equiv'('content-type'),
                           content(Type)
                         ], [])).
content_type -->
    { html_current_option(dialect(html5)) },
    !,
    html_post(head, meta('charset=UTF-8')).
content_type -->
    [].

pagehead(_, Head) -->
    { functor(Head, head, _)
    },
    !,
    html(Head).
pagehead(Style, Head) -->
    { strip_module(Head, M, _),
      hook_module(M, HM, head//2)
    },
    HM:head(Style, Head),
    !.
pagehead(_, Head) -->
    { strip_module(Head, M, _),
      hook_module(M, HM, head//1)
    },
    HM:head(Head),
    !.
pagehead(_, Head) -->
    html(head(Head)).


pagebody(_, Body) -->
    { functor(Body, body, _)
    },
    !,
    html(Body).
pagebody(Style, Body) -->
    { strip_module(Body, M, _),
      hook_module(M, HM, body//2)
    },
    HM:body(Style, Body),
    !.
pagebody(_, Body) -->
    { strip_module(Body, M, _),
      hook_module(M, HM, body//1)
    },
    HM:body(Body),
    !.
pagebody(_, Body) -->
    html(body(Body)).


hook_module(M, M, PI) :-
    current_predicate(M:PI),
    !.
hook_module(_, user, PI) :-
    current_predicate(user:PI).

%!  html(+Content:dom)// is det
%
%   Generate HTML from Content.  Generates a token sequence for
%   print_html/2.

html(Spec) -->
    { strip_module(Spec, M, T) },
    qhtml(T, M).

qhtml(Var, _) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
qhtml([], _) -->
    !,
    [].
qhtml([H|T], M) -->
    !,
    html_expand(H, M),
    qhtml(T, M).
qhtml(X, M) -->
    html_expand(X, M).

html_expand(Var, _) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
html_expand(Term, Module) -->
    do_expand(Term, Module),
    !.
html_expand(Term, _Module) -->
    { print_message(error, html(expand_failed(Term))) }.


do_expand(Token, _) -->                 % call user hooks
    expand(Token),
    !.
do_expand(Fmt-Args, _) -->
    !,
    { format(string(String), Fmt, Args)
    },
    html_quoted(String).
do_expand(\List, Module) -->
    { is_list(List)
    },
    !,
    raw(List, Module).
do_expand(\Term, Module, In, Rest) :-
    !,
    call(Module:Term, In, Rest).
do_expand(Module:Term, _) -->
    !,
    qhtml(Term, Module).
do_expand(&(Entity), _) -->
    !,
    {   integer(Entity)
    ->  format(string(String), '&#~d;', [Entity])
    ;   format(string(String), '&~w;', [Entity])
    },
    [ String ].
do_expand(Token, _) -->
    { atomic(Token)
    },
    !,
    html_quoted(Token).
do_expand(element(Env, Attributes, Contents), M) -->
    !,
    (   { Contents == [],
          html_current_option(dialect(xhtml))
        }
    ->  xhtml_empty(Env, Attributes)
    ;   html_begin(Env, Attributes),
        qhtml(Env, Contents, M),
        html_end(Env)
    ).
do_expand(Term, M) -->
    { Term =.. [Env, Contents]
    },
    !,
    (   { layout(Env, _, empty)
        }
    ->  html_begin(Env, Contents)
    ;   (   { Contents == [],
              html_current_option(dialect(xhtml))
            }
        ->  xhtml_empty(Env, [])
        ;   html_begin(Env),
            qhtml(Env, Contents, M),
            html_end(Env)
        )
    ).
do_expand(Term, M) -->
    { Term =.. [Env, Attributes, Contents],
      check_non_empty(Contents, Env, Term)
    },
    !,
    (   { Contents == [],
          html_current_option(dialect(xhtml))
        }
    ->  xhtml_empty(Env, Attributes)
    ;   html_begin(Env, Attributes),
        qhtml(Env, Contents, M),
        html_end(Env)
    ).

qhtml(Env, Contents, M) -->
    { cdata_element(Env),
      phrase(cdata(Contents, M), Tokens)
    },
    !,
    [ cdata(Env, Tokens) ].
qhtml(_, Contents, M) -->
    qhtml(Contents, M).


check_non_empty([], _, _) :- !.
check_non_empty(_, Tag, Term) :-
    layout(Tag, _, empty),
    !,
    print_message(warning,
                  format('Using empty element with content: ~p', [Term])).
check_non_empty(_, _, _).

cdata(List, M) -->
    { is_list(List) },
    !,
    raw(List, M).
cdata(One, M) -->
    raw_element(One, M).

%!  raw(+List, +Module)// is det.
%
%   Emit unquoted (raw) output used for scripts, etc.

raw([], _) -->
    [].
raw([H|T], Module) -->
    raw_element(H, Module),
    raw(T, Module).

raw_element(Var, _) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
raw_element(\List, Module) -->
    { is_list(List)
    },
    !,
    raw(List, Module).
raw_element(\Term, Module, In, Rest) :-
    !,
    call(Module:Term, In, Rest).
raw_element(Module:Term, _) -->
    !,
    raw_element(Term, Module).
raw_element(Fmt-Args, _) -->
    !,
    { format(string(S), Fmt, Args) },
    [S].
raw_element(Value, _) -->
    { must_be(atomic, Value) },
    [Value].


%!  html_begin(+Env)// is det.
%!  html_end(+End)// is det
%
%   For  html_begin//1,  Env  is   a    term   Env(Attributes);  for
%   html_end//1  it  is  the  plain    environment  name.  Used  for
%   exceptional  cases.  Normal  applications    use   html//1.  The
%   following two fragments are identical, where we prefer the first
%   as it is more concise and less error-prone.
%
%   ==
%           html(table(border=1, \table_content))
%   ==
%   ==
%           html_begin(table(border=1)
%           table_content,
%           html_end(table)
%   ==

html_begin(Env) -->
    { Env =.. [Name|Attributes]
    },
    html_begin(Name, Attributes).

html_begin(Env, Attributes) -->
    pre_open(Env),
    [<],
    [Env],
    attributes(Env, Attributes),
    (   { layout(Env, _, empty),
          html_current_option(dialect(xhtml))
        }
    ->  ['/>']
    ;   [>]
    ),
    post_open(Env).

html_end(Env)   -->                     % empty element or omited close
    { layout(Env, _, -),
      html_current_option(dialect(html))
    ; layout(Env, _, empty)
    },
    !,
    [].
html_end(Env)   -->
    pre_close(Env),
    ['</'],
    [Env],
    ['>'],
    post_close(Env).

%!  xhtml_empty(+Env, +Attributes)// is det.
%
%   Emit element in xhtml mode with empty content.

xhtml_empty(Env, Attributes) -->
    pre_open(Env),
    [<],
    [Env],
    attributes(Attributes),
    ['/>'].

%!  xhtml_ns(+Id, +Value)//
%
%   Demand an xmlns:id=Value in the outer   html  tag. This uses the
%   html_post/2 mechanism to  post  to   the  =xmlns=  channel. Rdfa
%   (http://www.w3.org/2006/07/SWD/RDFa/syntax/), embedding RDF   in
%   (x)html provides a typical  usage  scenario   where  we  want to
%   publish the required namespaces in the header. We can define:
%
%   ==
%   rdf_ns(Id) -->
%           { rdf_global_id(Id:'', Value) },
%           xhtml_ns(Id, Value).
%   ==
%
%   After which we can use rdf_ns//1 as  a normal rule in html//1 to
%   publish namespaces from library(semweb/rdf_db).   Note that this
%   macro only has effect if  the  dialect   is  set  to =xhtml=. In
%   =html= mode it is silently ignored.
%
%   The required =xmlns= receiver  is   installed  by  html_begin//1
%   using the =html= tag and thus is   present  in any document that
%   opens the outer =html= environment through this library.

xhtml_ns(Id, Value) -->
    { html_current_option(dialect(xhtml)) },
    !,
    html_post(xmlns, \attribute(xmlns:Id=Value)).
xhtml_ns(_, _) -->
    [].

%!  html_root_attribute(+Name, +Value)//
%
%   Add an attribute to the  HTML  root   element  of  the page. For
%   example:
%
%     ==
%         html(div(...)),
%         html_root_attribute(lang, en),
%         ...
%     ==

html_root_attribute(Name, Value) -->
    html_post(html_begin, \attribute(Name=Value)).

%!  attributes(+Env, +Attributes)// is det.
%
%   Emit attributes for Env. Adds XHTML namespace declaration to the
%   html tag if not provided by the caller.

attributes(html, L) -->
    !,
    (   { html_current_option(dialect(xhtml)) }
    ->  (   { option(xmlns(_), L) }
        ->  attributes(L)
        ;   { ns(xhtml, NS) },
            attributes([xmlns(NS)|L])
        ),
        html_receive(xmlns)
    ;   attributes(L),
        html_noreceive(xmlns)
    ),
    html_receive(html_begin).
attributes(_, L) -->
    attributes(L).

attributes([]) -->
    !,
    [].
attributes([H|T]) -->
    !,
    attribute(H),
    attributes(T).
attributes(One) -->
    attribute(One).

attribute(Name=Value) -->
    !,
    [' '], name(Name), [ '="' ],
    attribute_value(Value),
    ['"'].
attribute(NS:Term) -->
    !,
    { Term =.. [Name, Value]
    },
    !,
    attribute((NS:Name)=Value).
attribute(Term) -->
    { Term =.. [Name, Value]
    },
    !,
    attribute(Name=Value).
attribute(Atom) -->                     % Value-abbreviated attribute
    { atom(Atom)
    },
    [ ' ', Atom ].

name(NS:Name) -->
    !,
    [NS, :, Name].
name(Name) -->
    [ Name ].

%!  attribute_value(+Value) is det.
%
%   Print an attribute value. Value is either   atomic or one of the
%   following terms:
%
%     * A+B
%     Concatenation of A and B
%     * encode(V)
%     Emit URL-encoded version of V.  See www_form_encode/2.
%     * An option list
%     Emit ?Name1=encode(Value1)&Name2=encode(Value2) ...
%     * A term Format-Arguments
%     Use format/3 and emit the result as quoted value.
%
%   The hook html_write:expand_attribute_value//1 can  be defined to
%   provide additional `function like'   translations.  For example,
%   http_dispatch.pl  defines  location_by_id(ID)  to   refer  to  a
%   location on the current server  based   on  the  handler id. See
%   http_location_by_id/2.

attribute_value(List) -->
    { is_list(List) },
    !,
    attribute_value_m(List).
attribute_value(Value) -->
    attribute_value_s(Value).

% emit a single attribute value

attribute_value_s(Var) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
attribute_value_s(A+B) -->
    !,
    attribute_value(A),
    (   { is_list(B) }
    ->  (   { B == [] }
        ->  []
        ;   [?], search_parameters(B)
        )
    ;   attribute_value(B)
    ).
attribute_value_s(encode(Value)) -->
    !,
    { uri_encoded(query_value, Value, Encoded) },
    [ Encoded ].
attribute_value_s(Value) -->
    expand_attribute_value(Value),
    !.
attribute_value_s(Fmt-Args) -->
    !,
    { format(string(Value), Fmt, Args) },
    html_quoted_attribute(Value).
attribute_value_s(Value) -->
    html_quoted_attribute(Value).

search_parameters([H|T]) -->
    search_parameter(H),
    (   {T == []}
    ->  []
    ;   ['&amp;'],
        search_parameters(T)
    ).

search_parameter(Var) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
search_parameter(Name=Value) -->
    { www_form_encode(Value, Encoded) },
    [Name, =, Encoded].
search_parameter(Term) -->
    { Term =.. [Name, Value],
      !,
      www_form_encode(Value, Encoded)
    },
    [Name, =, Encoded].
search_parameter(Term) -->
    { domain_error(search_parameter, Term)
    }.

%!  attribute_value_m(+List)//
%
%   Used for multi-valued attributes, such as class-lists.  E.g.,
%
%     ==
%           body(class([c1, c2]), Body)
%     ==
%
%     Emits =|<body class="c1 c2"> ...|=

attribute_value_m([]) -->
    [].
attribute_value_m([H|T]) -->
    attribute_value_s(H),
    (   { T == [] }
    ->  []
    ;   [' '],
        attribute_value_m(T)
    ).


                 /*******************************
                 *         QUOTING RULES        *
                 *******************************/

%!  html_quoted(Text)// is det.
%
%   Quote  the  value  for  normal  (CDATA)  text.  Note  that  text
%   appearing in the document  structure   is  normally quoted using
%   these rules. I.e. the following emits  properly quoted bold text
%   regardless of the content of Text:
%
%   ==
%           html(b(Text))
%   ==
%
%   @tbd    Assumes UTF-8 encoding of the output.

html_quoted(Text) -->
    { xml_quote_cdata(Text, Quoted, utf8) },
    [ Quoted ].

%!  html_quoted_attribute(+Text)// is det.
%
%   Quote the value  according  to   the  rules  for  tag-attributes
%   included in double-quotes.  Note   that  -like  html_quoted//1-,
%   attributed   values   printed   through   html//1   are   quoted
%   atomatically.
%
%   @tbd    Assumes UTF-8 encoding of the output.

html_quoted_attribute(Text) -->
    { xml_quote_attribute(Text, Quoted, utf8) },
    [ Quoted ].

%!  cdata_element(?Element)
%
%   True when Element contains declared CDATA   and thus only =|</|=
%   needs to be escaped.

cdata_element(script).
cdata_element(style).


                 /*******************************
                 *      REPOSITIONING HTML      *
                 *******************************/

%!  html_post(+Id, :HTML)// is det.
%
%   Reposition HTML to  the  receiving   Id.  The  html_post//2 call
%   processes HTML using html//1. Embedded   \-commands are executed
%   by mailman/1 from  print_html/1   or  html_print_length/2. These
%   commands are called in the calling   context of the html_post//2
%   call.
%
%   A typical usage scenario is to  get   required  CSS links in the
%   document head in a reusable fashion. First, we define css//1 as:
%
%   ==
%   css(URL) -->
%           html_post(css,
%                     link([ type('text/css'),
%                            rel('stylesheet'),
%                            href(URL)
%                          ])).
%   ==
%
%   Next we insert the _unique_ CSS links, in the pagehead using the
%   following call to reply_html_page/2:
%
%   ==
%           reply_html_page([ title(...),
%                             \html_receive(css)
%                           ],
%                           ...)
%   ==

html_post(Id, Content) -->
    { strip_module(Content, M, C) },
    [ mailbox(Id, post(M, C)) ].

%!  html_receive(+Id)// is det.
%
%   Receive posted HTML tokens. Unique   sequences  of tokens posted
%   with  html_post//2  are  inserted   at    the   location   where
%   html_receive//1 appears.
%
%   @see    The local predicate sorted_html//1 handles the output of
%           html_receive//1.
%   @see    html_receive//2 allows for post-processing the posted
%           material.

html_receive(Id) -->
    html_receive(Id, sorted_html).

%!  html_receive(+Id, :Handler)// is det.
%
%   This extended version of html_receive//1   causes  Handler to be
%   called to process all messages posted to the channal at the time
%   output  is  generated.  Handler  is    called  as  below,  where
%   `PostedTerms` is a list of  Module:Term   created  from calls to
%   html_post//2. Module is the context module of html_post and Term
%   is the unmodified term. Members  in   `PostedTerms`  are  in the
%   order posted and may contain duplicates.
%
%     ==
%       phrase(Handler, PostedTerms, HtmlTerms, Rest)
%     ==
%
%   Typically, Handler collects the posted   terms,  creating a term
%   suitable for html//1 and finally calls html//1.

html_receive(Id, Handler) -->
    { strip_module(Handler, M, P) },
    [ mailbox(Id, accept(M:P, _)) ].

%!  html_noreceive(+Id)// is det.
%
%   As html_receive//1, but discard posted messages.

html_noreceive(Id) -->
    [ mailbox(Id, ignore(_,_)) ].

%!  mailman(+Tokens) is det.
%
%   Collect  posted  tokens  and  copy    them  into  the  receiving
%   mailboxes. Mailboxes may produce output for  each other, but not
%   cyclic. The current scheme to resolve   this is rather naive: It
%   simply permutates the mailbox resolution order  until it found a
%   working one. Before that, it puts   =head= and =script= boxes at
%   the end.

mailman(Tokens) :-
    (   html_token(mailbox(_, accept(_, Accepted)), Tokens)
    ->  true
    ),
    var(Accepted),                 % not yet executed
    !,
    mailboxes(Tokens, Boxes),
    keysort(Boxes, Keyed),
    group_pairs_by_key(Keyed, PerKey),
    move_last(PerKey, script, PerKey1),
    move_last(PerKey1, head, PerKey2),
    (   permutation(PerKey2, PerKeyPerm),
        (   mail_ids(PerKeyPerm)
        ->  !
        ;   debug(html(mailman),
                  'Failed mail delivery order; retrying', []),
            fail
        )
    ->  true
    ;   print_message(error, html(cyclic_mailboxes))
    ).
mailman(_).

move_last(Box0, Id, Box) :-
    selectchk(Id-List, Box0, Box1),
    !,
    append(Box1, [Id-List], Box).
move_last(Box, _, Box).

%!  html_token(?Token, +Tokens) is nondet.
%
%   True if Token is a token in the  token set. This is like member,
%   but the toplevel list may contain cdata(Elem, Tokens).

html_token(Token, [H|T]) :-
    html_token_(T, H, Token).

html_token_(_, Token, Token) :- !.
html_token_(_, cdata(_,Tokens), Token) :-
    html_token(Token, Tokens).
html_token_([H|T], _, Token) :-
    html_token_(T, H, Token).

%!  mailboxes(+Tokens, -MailBoxes) is det.
%
%   Get all mailboxes from the token set.

mailboxes(Tokens, MailBoxes) :-
    mailboxes(Tokens, MailBoxes, []).

mailboxes([], List, List).
mailboxes([mailbox(Id, Value)|T0], [Id-Value|T], Tail) :-
    !,
    mailboxes(T0, T, Tail).
mailboxes([cdata(_Type, Tokens)|T0], Boxes, Tail) :-
    !,
    mailboxes(Tokens, Boxes, Tail0),
    mailboxes(T0, Tail0, Tail).
mailboxes([_|T0], T, Tail) :-
    mailboxes(T0, T, Tail).

mail_ids([]).
mail_ids([H|T0]) :-
    mail_id(H, NewPosts),
    add_new_posts(NewPosts, T0, T),
    mail_ids(T).

mail_id(Id-List, NewPosts) :-
    mail_handlers(List, Boxes, Content),
    (   Boxes = [accept(MH:Handler, In)]
    ->  extend_args(Handler, Content, Goal),
        phrase(MH:Goal, In),
        mailboxes(In, NewBoxes),
        keysort(NewBoxes, Keyed),
        group_pairs_by_key(Keyed, NewPosts)
    ;   Boxes = [ignore(_, _)|_]
    ->  NewPosts = []
    ;   Boxes = [accept(_,_),accept(_,_)|_]
    ->  print_message(error, html(multiple_receivers(Id))),
        NewPosts = []
    ;   print_message(error, html(no_receiver(Id))),
        NewPosts = []
    ).

add_new_posts([], T, T).
add_new_posts([Id-Posts|NewT], T0, T) :-
    (   select(Id-List0, T0, Id-List, T1)
    ->  append(List0, Posts, List)
    ;   debug(html(mailman), 'Stuck with new posts on ~q', [Id]),
        fail
    ),
    add_new_posts(NewT, T1, T).


%!  mail_handlers(+Boxes, -Handlers, -Posters) is det.
%
%   Collect all post(Module,HTML) into Posters  and the remainder in
%   Handlers.  Handlers  consists  of  accept(Handler,  Tokens)  and
%   ignore(_,_).

mail_handlers([], [], []).
mail_handlers([post(Module,HTML)|T0], H, [Module:HTML|T]) :-
    !,
    mail_handlers(T0, H, T).
mail_handlers([H|T0], [H|T], C) :-
    mail_handlers(T0, T, C).

extend_args(Term, Extra, NewTerm) :-
    Term =.. [Name|Args],
    append(Args, [Extra], NewArgs),
    NewTerm =.. [Name|NewArgs].

%!  sorted_html(+Content:list)// is det.
%
%   Default  handlers  for  html_receive//1.  It  sorts  the  posted
%   objects to create a unique list.
%
%   @bug    Elements can differ just on the module.  Ideally we
%           should phrase all members, sort the list of list of
%           tokens and emit the result.  Can we do better?

sorted_html(List) -->
    { sort(List, Unique) },
    html(Unique).

%!  head_html(+Content:list)// is det.
%
%   Handler for html_receive(head). Unlike  sorted_html//1, it calls
%   a user hook  html_write:html_head_expansion/2   to  process  the
%   collected head material into a term suitable for html//1.
%
%   @tbd  This  has  been  added   to  facilitate  html_head.pl,  an
%   experimental  library  for  dealing  with   css  and  javascript
%   resources. It feels a bit like a hack, but for now I do not know
%   a better solution.

head_html(List) -->
    { list_to_set(List, Unique),
      html_expand_head(Unique, NewList)
    },
    html(NewList).

:- multifile
    html_head_expansion/2.

html_expand_head(List0, List) :-
    html_head_expansion(List0, List1),
    List0 \== List1,
    !,
    html_expand_head(List1, List).
html_expand_head(List, List).


                 /*******************************
                 *             LAYOUT           *
                 *******************************/

pre_open(Env) -->
    { layout(Env, N-_, _)
    },
    !,
    [ nl(N) ].
pre_open(_) --> [].

post_open(Env) -->
    { layout(Env, _-N, _)
    },
    !,
    [ nl(N) ].
post_open(_) -->
    [].

pre_close(head) -->
    !,
    html_receive(head, head_html),
    { layout(head, _, N-_) },
    [ nl(N) ].
pre_close(Env) -->
    { layout(Env, _, N-_)
    },
    !,
    [ nl(N) ].
pre_close(_) -->
    [].

post_close(Env) -->
    { layout(Env, _, _-N)
    },
    !,
    [ nl(N) ].
post_close(_) -->
    [].

%!  layout(+Tag, -Open, -Close) is det.
%
%   Define required newlines before and after   tags.  This table is
%   rather incomplete. New rules can  be   added  to  this multifile
%   predicate.
%
%   @param Tag      Name of the tag
%   @param Open     Tuple M-N, where M is the number of lines before
%                   the tag and N after.
%   @param Close    Either as Open, or the atom - (minus) to omit the
%                   close-tag or =empty= to indicate the element has
%                   no content model.
%
%   @tbd    Complete table

:- multifile
    layout/3.

layout(table,      2-1, 1-2).
layout(blockquote, 2-1, 1-2).
layout(pre,        2-1, 0-2).
layout(textarea,   1-1, 0-1).
layout(center,     2-1, 1-2).
layout(dl,         2-1, 1-2).
layout(ul,         1-1, 1-1).
layout(ol,         2-1, 1-2).
layout(form,       2-1, 1-2).
layout(frameset,   2-1, 1-2).
layout(address,    2-1, 1-2).

layout(head,       1-1, 1-1).
layout(body,       1-1, 1-1).
layout(script,     1-1, 1-1).
layout(style,      1-1, 1-1).
layout(select,     1-1, 1-1).
layout(map,        1-1, 1-1).
layout(html,       1-1, 1-1).
layout(caption,    1-1, 1-1).
layout(applet,     1-1, 1-1).

layout(tr,         1-0, 0-1).
layout(option,     1-0, 0-1).
layout(li,         1-0, 0-1).
layout(dt,         1-0, -).
layout(dd,         0-0, -).
layout(title,      1-0, 0-1).

layout(h1,         2-0, 0-2).
layout(h2,         2-0, 0-2).
layout(h3,         2-0, 0-2).
layout(h4,         2-0, 0-2).

layout(iframe,     1-1, 1-1).

layout(hr,         1-1, empty).         % empty elements
layout(br,         0-1, empty).
layout(img,        0-0, empty).
layout(meta,       1-1, empty).
layout(base,       1-1, empty).
layout(link,       1-1, empty).
layout(input,      0-0, empty).
layout(frame,      1-1, empty).
layout(col,        0-0, empty).
layout(area,       1-0, empty).
layout(input,      1-0, empty).
layout(param,      1-0, empty).

layout(p,          2-1, -).             % omited close
layout(td,         0-0, 0-0).

layout(div,        1-0, 0-1).

                 /*******************************
                 *           PRINTING           *
                 *******************************/

%!  print_html(+List) is det.
%!  print_html(+Out:stream, +List) is det.
%
%   Print list of atoms and layout instructions.  Currently used layout
%   instructions:
%
%           * nl(N)
%           Use at minimum N newlines here.
%
%           * mailbox(Id, Box)
%           Repositioned tokens (see html_post//2 and
%           html_receive//2)

print_html(List) :-
    current_output(Out),
    mailman(List),
    write_html(List, Out).
print_html(Out, List) :-
    (   html_current_option(dialect(xhtml))
    ->  stream_property(Out, encoding(Enc)),
        (   Enc == utf8
        ->  true
        ;   print_message(warning, html(wrong_encoding(Out, Enc)))
        ),
        xml_header(Hdr),
        write(Out, Hdr), nl(Out)
    ;   true
    ),
    mailman(List),
    write_html(List, Out),
    flush_output(Out).

write_html([], _).
write_html([nl(N)|T], Out) :-
    !,
    join_nl(T, N, Lines, T2),
    write_nl(Lines, Out),
    write_html(T2, Out).
write_html([mailbox(_, Box)|T], Out) :-
    !,
    (   Box = accept(_, Accepted)
    ->  write_html(Accepted, Out)
    ;   true
    ),
    write_html(T, Out).
write_html([cdata(Env, Tokens)|T], Out) :-
    !,
    with_output_to(string(CDATA), write_html(Tokens, current_output)),
    valid_cdata(Env, CDATA),
    write(Out, CDATA),
    write_html(T, Out).
write_html([H|T], Out) :-
    write(Out, H),
    write_html(T, Out).

join_nl([nl(N0)|T0], N1, N, T) :-
    !,
    N2 is max(N0, N1),
    join_nl(T0, N2, N, T).
join_nl(L, N, N, L).

write_nl(0, _) :- !.
write_nl(N, Out) :-
    nl(Out),
    N1 is N - 1,
    write_nl(N1, Out).

%!  valid_cdata(+Env, +String) is det.
%
%   True when String is valid content for   a  CDATA element such as
%   =|<script>|=. This implies  it   cannot  contain  =|</script/|=.
%   There is no escape for this and  the script generator must use a
%   work-around using features of the  script language. For example,
%   when  using  JavaScript,  "</script>"   can    be   written   as
%   "<\/script>".
%
%   @see write_json/2, js_arg//1.
%   @error domain_error(cdata, String)

valid_cdata(Env, String) :-
    atomics_to_string(['</', Env, '>'], End),
    sub_atom_icasechk(String, _, End),
    !,
    domain_error(cdata, String).
valid_cdata(_, _).

%!  html_print_length(+List, -Len) is det.
%
%   Determine the content length of  a   token  list  produced using
%   html//1. Here is an example on  how   this  is used to output an
%   HTML compatible to HTTP:
%
%   ==
%           phrase(html(DOM), Tokens),
%           html_print_length(Tokens, Len),
%           format('Content-type: text/html; charset=UTF-8~n'),
%           format('Content-length: ~d~n~n', [Len]),
%           print_html(Tokens)
%   ==

html_print_length(List, Len) :-
    mailman(List),
    (   html_current_option(dialect(xhtml))
    ->  xml_header(Hdr),
        atom_length(Hdr, L0),
        L1 is L0+1                  % one for newline
    ;   L1 = 0
    ),
    html_print_length(List, L1, Len).

html_print_length([], L, L).
html_print_length([nl(N)|T], L0, L) :-
    !,
    join_nl(T, N, Lines, T1),
    L1 is L0 + Lines,               % assume only \n!
    html_print_length(T1, L1, L).
html_print_length([mailbox(_, Box)|T], L0, L) :-
    !,
    (   Box = accept(_, Accepted)
    ->  html_print_length(Accepted, L0, L1)
    ;   L1 = L0
    ),
    html_print_length(T, L1, L).
html_print_length([cdata(_, CDATA)|T], L0, L) :-
    !,
    html_print_length(CDATA, L0, L1),
    html_print_length(T, L1, L).
html_print_length([H|T], L0, L) :-
    atom_length(H, Hlen),
    L1 is L0+Hlen,
    html_print_length(T, L1, L).


%!  reply_html_page(:Head, :Body) is det.
%!  reply_html_page(+Style, :Head, :Body) is det.
%
%   Provide the complete reply as required  by http_wrapper.pl for a
%   page constructed from Head and   Body. The HTTP =|Content-type|=
%   is provided by html_current_option/1.

reply_html_page(Head, Body) :-
    reply_html_page(default, Head, Body).
reply_html_page(Style, Head, Body) :-
    html_current_option(content_type(Type)),
    phrase(page(Style, Head, Body), HTML),
    format('Content-type: ~w~n~n', [Type]),
    print_html(HTML).


                 /*******************************
                 *     META-PREDICATE SUPPORT   *
                 *******************************/

%!  html_meta(+Heads) is det.
%
%   This directive can be used  to   declare  that an HTML rendering
%   rule takes HTML content as  argument.   It  has  two effects. It
%   emits  the  appropriate  meta_predicate/1    and  instructs  the
%   built-in editor (PceEmacs) to provide   proper colouring for the
%   arguments.  The  arguments  in  Head  are    the   same  as  for
%   meta_predicate or can be constant =html=.  For example:
%
%     ==
%     :- html_meta
%           page(html,html,?,?).
%     ==

html_meta(Spec) :-
    throw(error(context_error(nodirective, html_meta(Spec)), _)).

html_meta_decls(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
html_meta_decls((A,B), (MA,MB), [MH|T]) :-
    !,
    html_meta_decl(A, MA, MH),
    html_meta_decls(B, MB, T).
html_meta_decls(A, MA, [MH]) :-
    html_meta_decl(A, MA, MH).

html_meta_decl(Head, MetaHead,
               html_write:html_meta_head(GenHead, Module, Head)) :-
    functor(Head, Name, Arity),
    functor(GenHead, Name, Arity),
    prolog_load_context(module, Module),
    Head =.. [Name|HArgs],
    maplist(html_meta_decl, HArgs, MArgs),
    MetaHead =.. [Name|MArgs].

html_meta_decl(html, :) :- !.
html_meta_decl(Meta, Meta).

system:term_expansion((:- html_meta(Heads)),
                      [ (:- meta_predicate(Meta))
                      | MetaHeads
                      ]) :-
    html_meta_decls(Heads, Meta, MetaHeads).

:- multifile
    html_meta_head/3.

html_meta_colours(Head, Goal, built_in-Colours) :-
    Head =.. [_|MArgs],
    Goal =.. [_|Args],
    maplist(meta_colours, MArgs, Args, Colours).

meta_colours(html, HTML, Colours) :-
    !,
    html_colours(HTML, Colours).
meta_colours(I, _, Colours) :-
    integer(I), I>=0,
    !,
    Colours = meta(I).
meta_colours(_, _, classify).

html_meta_called(Head, Goal, Called) :-
    Head =.. [_|MArgs],
    Goal =.. [_|Args],
    meta_called(MArgs, Args, Called, []).

meta_called([], [], Called, Called).
meta_called([html|MT], [A|AT], Called, Tail) :-
    !,
    phrase(called_by(A), Called, Tail1),
    meta_called(MT, AT, Tail1, Tail).
meta_called([0|MT], [A|AT], [A|CT0], CT) :-
    !,
    meta_called(MT, AT, CT0, CT).
meta_called([I|MT], [A|AT], [A+I|CT0], CT) :-
    integer(I), I>0,
    !,
    meta_called(MT, AT, CT0, CT).
meta_called([_|MT], [_|AT], Called, Tail) :-
    !,
    meta_called(MT, AT, Called, Tail).


:- html_meta
    html(html,?,?),
    page(html,?,?),
    page(html,html,?,?),
    page(+,html,html,?,?),
    pagehead(+,html,?,?),
    pagebody(+,html,?,?),
    reply_html_page(html,html),
    reply_html_page(+,html,html),
    html_post(+,html,?,?).


                 /*******************************
                 *      PCE EMACS SUPPORT       *
                 *******************************/

:- multifile
    prolog_colour:goal_colours/2,
    prolog_colour:style/2,
    prolog_colour:message//1,
    prolog:called_by/2.

prolog_colour:goal_colours(Goal, Colours) :-
    html_meta_head(Goal, _Module, Head),
    html_meta_colours(Head, Goal, Colours).
prolog_colour:goal_colours(html_meta(_),
                           built_in-[meta_declarations([html])]).

                                        % TBD: Check with do_expand!
html_colours(Var, classify) :-
    var(Var),
    !.
html_colours(\List, html_raw-[list-Colours]) :-
    is_list(List),
    !,
    list_colours(List, Colours).
html_colours(\_, html_call-[dcg]) :- !.
html_colours(_:Term, built_in-[classify,Colours]) :-
    !,
    html_colours(Term, Colours).
html_colours(&(Entity), functor-[entity(Entity)]) :- !.
html_colours(List, list-ListColours) :-
    List = [_|_],
    !,
    list_colours(List, ListColours).
html_colours(Format-Args, functor-[FormatColor,ArgsColors]) :-
    !,
    format_colours(Format, FormatColor),
    format_arg_colours(Args, Format, ArgsColors).
html_colours(Term, TermColours) :-
    compound(Term),
    compound_name_arguments(Term, Name, Args),
    Name \== '.',
    !,
    (   Args = [One]
    ->  TermColours = html(Name)-ArgColours,
        (   layout(Name, _, empty)
        ->  attr_colours(One, ArgColours)
        ;   html_colours(One, Colours),
            ArgColours = [Colours]
        )
    ;   Args = [AList,Content]
    ->  TermColours = html(Name)-[AColours, Colours],
        attr_colours(AList, AColours),
        html_colours(Content, Colours)
    ;   TermColours = error
    ).
html_colours(_, classify).

list_colours(Var, classify) :-
    var(Var),
    !.
list_colours([], []).
list_colours([H0|T0], [H|T]) :-
    !,
    html_colours(H0, H),
    list_colours(T0, T).
list_colours(Last, Colours) :-          % improper list
    html_colours(Last, Colours).

attr_colours(Var, classify) :-
    var(Var),
    !.
attr_colours([], classify) :- !.
attr_colours(Term, list-Elements) :-
    Term = [_|_],
    !,
    attr_list_colours(Term, Elements).
attr_colours(Name=Value, built_in-[html_attribute(Name), VColour]) :-
    !,
    attr_value_colour(Value, VColour).
attr_colours(NS:Term, built_in-[ html_xmlns(NS),
                                 html_attribute(Name)-[classify]
                               ]) :-
    compound(Term),
    compound_name_arity(Term, Name, 1).
attr_colours(Term, html_attribute(Name)-[VColour]) :-
    compound(Term),
    compound_name_arity(Term, Name, 1),
    !,
    Term =.. [Name,Value],
    attr_value_colour(Value, VColour).
attr_colours(Name, html_attribute(Name)) :-
    atom(Name),
    !.
attr_colours(Term, classify) :-
    compound(Term),
    compound_name_arity(Term, '.', 2),
    !.
attr_colours(_, error).

attr_list_colours(Var, classify) :-
    var(Var),
    !.
attr_list_colours([], []).
attr_list_colours([H0|T0], [H|T]) :-
    attr_colours(H0, H),
    attr_list_colours(T0, T).

attr_value_colour(Var, classify) :-
    var(Var).
attr_value_colour(location_by_id(ID), sgml_attr_function-[Colour]) :-
    !,
    location_id(ID, Colour).
attr_value_colour(#(ID), sgml_attr_function-[Colour]) :-
    !,
    location_id(ID, Colour).
attr_value_colour(A+B, sgml_attr_function-[CA,CB]) :-
    !,
    attr_value_colour(A, CA),
    attr_value_colour(B, CB).
attr_value_colour(encode(_), sgml_attr_function-[classify]) :- !.
attr_value_colour(Atom, classify) :-
    atomic(Atom),
    !.
attr_value_colour([_|_], classify) :- !.
attr_value_colour(_Fmt-_Args, classify) :- !.
attr_value_colour(Term, classify) :-
    compound(Term),
    compound_name_arity(Term, '.', 2),
    !.
attr_value_colour(_, error).

location_id(ID, classify) :-
    var(ID),
    !.
location_id(ID, Class) :-
    (   catch(http_location_by_id(ID, Location), _, fail)
    ->  Class = http_location_for_id(Location)
    ;   Class = http_no_location_for_id(ID)
    ).
location_id(_, classify).

format_colours(Format, format_string) :- atom(Format), !.
format_colours(Format, format_string) :- string(Format), !.
format_colours(_Format, type_error(text)).

format_arg_colours(Args, _Format, classify) :- is_list(Args), !.
format_arg_colours(_, _, type_error(list)).

:- op(990, xfx, :=).                    % allow compiling without XPCE
:- op(200, fy, @).

prolog_colour:style(html(_),                    [colour(magenta4), bold(true)]).
prolog_colour:style(entity(_),                  [colour(magenta4)]).
prolog_colour:style(html_attribute(_),          [colour(magenta4)]).
prolog_colour:style(html_xmlns(_),              [colour(magenta4)]).
prolog_colour:style(format_string(_),           [colour(magenta4)]).
prolog_colour:style(sgml_attr_function,         [colour(blue)]).
prolog_colour:style(http_location_for_id(_),    [bold(true)]).
prolog_colour:style(http_no_location_for_id(_), [colour(red), bold(true)]).


prolog_colour:message(html(Element)) -->
    [ '~w: SGML element'-[Element] ].
prolog_colour:message(entity(Entity)) -->
    [ '~w: SGML entity'-[Entity] ].
prolog_colour:message(html_attribute(Attr)) -->
    [ '~w: SGML attribute'-[Attr] ].
prolog_colour:message(sgml_attr_function) -->
    [ 'SGML Attribute function'-[] ].
prolog_colour:message(http_location_for_id(Location)) -->
    [ 'ID resolves to ~w'-[Location] ].
prolog_colour:message(http_no_location_for_id(ID)) -->
    [ '~w: no such ID'-[ID] ].


%       prolog:called_by(+Goal, -Called)
%
%       Hook into library(pce_prolog_xref).  Called is a list of callable
%       or callable+N to indicate (DCG) arglist extension.


prolog:called_by(Goal, Called) :-
    html_meta_head(Goal, _Module, Head),
    html_meta_called(Head, Goal, Called).

called_by(Term) -->
    called_by(Term, _).

called_by(Var, _) -->
    { var(Var) },
    !,
    [].
called_by(\G, M) -->
    !,
    (   { is_list(G) }
    ->  called_by(G, M)
    ;   {atom(M)}
    ->  [(M:G)+2]
    ;   [G+2]
    ).
called_by([], _) -->
    !,
    [].
called_by([H|T], M) -->
    !,
    called_by(H, M),
    called_by(T, M).
called_by(M:Term, _) -->
    !,
    (   {atom(M)}
    ->  called_by(Term, M)
    ;   []
    ).
called_by(Term, M) -->
    { compound(Term),
      !,
      Term =.. [_|Args]
    },
    called_by(Args, M).
called_by(_, _) -->
    [].

:- multifile
    prolog:hook/1.

prolog:hook(body(_,_,_)).
prolog:hook(body(_,_,_,_)).
prolog:hook(head(_,_,_)).
prolog:hook(head(_,_,_,_)).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(html(expand_failed(What))) -->
    [ 'Failed to translate to HTML: ~p'-[What] ].
prolog:message(html(wrong_encoding(Stream, Enc))) -->
    [ 'XHTML demands UTF-8 encoding; encoding of ~p is ~w'-[Stream, Enc] ].
prolog:message(html(multiple_receivers(Id))) -->
    [ 'html_post//2: multiple receivers for: ~p'-[Id] ].
prolog:message(html(no_receiver(Id))) -->
    [ 'html_post//2: no receivers for: ~p'-[Id] ].
