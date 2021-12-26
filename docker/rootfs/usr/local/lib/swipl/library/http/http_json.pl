/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2018, University of Amsterdam
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

:- module(http_json,
          [ reply_json/1,               % +JSON
            reply_json/2,               % +JSON, Options
            reply_json_dict/1,          % +JSON
            reply_json_dict/2,          % +JSON, Options
            http_read_json/2,           % +Request, -JSON
            http_read_json/3,           % +Request, -JSON, +Options
            http_read_json_dict/2,      % +Request, -Dict
            http_read_json_dict/3,      % +Request, -Dict, +Options

            is_json_content_type/1      % +HeaderValue
          ]).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_stream)).
:- use_module(library(http/json)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(memfile)).

:- multifile
    http_client:http_convert_data/4,
    http:post_data_hook/3,
    json_type/1.

:- public
    json_type/1.

:- predicate_options(http_read_json/3, 3,
                     [ content_type(any),
                       false(ground),
                       null(ground),
                       true(ground),
                       value_string_as(oneof([atom, string])),
                       json_object(oneof([term,dict]))
                     ]).
:- predicate_options(reply_json/2, 2,
                     [ content_type(any),
                       status(integer),
                       json_object(oneof([term,dict])),
                       pass_to(json:json_write/3, 3)
                     ]).


/** <module> HTTP JSON Plugin module

Most   code   doesn't   need  to   use  this   directly;  instead   use
library(http/http_server),  which  combines   this  library  with   the
typical HTTP libraries that most servers need.

This module adds hooks to several parts   of  the HTTP libraries, making
them JSON-aware.  Notably:

  - Make http_read_data/3 convert `application/json` and
    `application/jsonrequest` content to a JSON term.
  - Cause http_open/3 to accept post(json(Term)) to issue a POST
    request with JSON content.
  - Provide HTTP server and client utility predicates for reading
    and replying JSON:
    - http_read_json/2
    - http_read_json/3
    - http_read_json_dict/2
    - http_read_json_dict/3
    - reply_json/1
    - reply_json/2
    - reply_json_dict/1
    - reply_json_dict/2
  - Reply to exceptions in the server using an JSON document rather
    then HTML if the =|Accept|= header prefers application/json over
    text/html.

Typically JSON is used by Prolog HTTP  servers. This module supports two
JSON  representations:  the  classical  representation    and   the  new
representation supported by  the  SWI-Prolog   version  7  extended data
types. Below is a skeleton for  handling   a  JSON request, answering in
JSON using the classical interface.

  ==
  handle(Request) :-
        http_read_json(Request, JSONIn),
        json_to_prolog(JSONIn, PrologIn),
        <compute>(PrologIn, PrologOut),         % application body
        prolog_to_json(PrologOut, JSONOut),
        reply_json(JSONOut).
  ==

When using dicts, the conversion step is   generally  not needed and the
code becomes:

  ==
  handle(Request) :-
        http_read_json_dict(Request, DictIn),
        <compute>(DictIn, DictOut),
        reply_json(DictOut).
  ==

This module also integrates JSON support   into the http client provided
by http_client.pl. Posting a JSON query   and  processing the JSON reply
(or any other reply understood  by   http_read_data/3)  is  as simple as
below, where Term is a JSON term as described in json.pl and reply is of
the same format if the server replies with JSON.

  ==
        ...,
        http_post(URL, json(Term), Reply, [])
  ==

@see    JSON Requests are discussed in http://json.org/JSONRequest.html
@see    json.pl describes how JSON objects are represented in Prolog terms.
@see    json_convert.pl converts between more natural Prolog terms and json
terms.
*/

%!  http_client:http_convert_data(+In, +Fields, -Data, +Options)
%
%   Hook implementation that supports  reading   JSON  documents. It
%   processes the following option:
%
%     * json_object(+As)
%     Where As is one of =term= or =dict=.  If the value is =dict=,
%     json_read_dict/3 is used.

http_client:http_convert_data(In, Fields, Data, Options) :-
    memberchk(content_type(Type), Fields),
    is_json_content_type(Type),
    !,
    (   memberchk(content_length(Bytes), Fields)
    ->  setup_call_cleanup(
            ( stream_range_open(In, Range, [size(Bytes)]),
              set_stream(Range, encoding(utf8))
            ),
            json_read_to(Range, Data, Options),
            close(Range))
    ;   set_stream(In, encoding(utf8)),
        json_read_to(In, Data, Options)
    ).


%!  is_json_content_type(+ContentType) is semidet.
%
%   True  if  ContentType  is  a  header  value  (either  parsed  or  as
%   atom/string) that denotes a JSON value.

is_json_content_type(String) :-
    http_parse_header_value(content_type, String,
                            media(Type, _Attributes)),
    json_type(Type),
    !.

json_read_to(In, Data, Options) :-
    memberchk(json_object(dict), Options),
    !,
    json_read_dict(In, Data, Options).
json_read_to(In, Data, Options) :-
    json_read(In, Data, Options).

%!  json_type(?MediaType) is semidet.
%
%   True if MediaType is a JSON media type. http_json:json_type/1 is
%   a  multifile  predicate  and  may   be  extended  to  facilitate
%   non-conforming clients.
%
%   @arg MediaType is a term `Type`/`SubType`, where both `Type` and
%   `SubType` are atoms.

json_type(application/jsonrequest).
json_type(application/json).


%!  http:post_data_hook(+Data, +Out:stream, +HdrExtra) is semidet.
%
%   Hook implementation that allows   http_post_data/3  posting JSON
%   objects using one of the  forms   below.
%
%     ==
%     http_post(URL, json(Term), Reply, Options)
%     http_post(URL, json(Term, Options), Reply, Options)
%     ==
%
%   If Options are passed, these are handed to json_write/3. In
%   addition, this option is processed:
%
%     * json_object(As)
%     If As is =dict=, json_write_dict/3 is used to write the
%     output.  This is default if json(Dict) is passed.
%
%   @tbd avoid creation of intermediate data using chunked output.

http:post_data_hook(json(Dict), Out, HdrExtra) :-
    is_dict(Dict),
    !,
    http:post_data_hook(json(Dict, [json_object(dict)]),
                        Out, HdrExtra).
http:post_data_hook(json(Term), Out, HdrExtra) :-
    http:post_data_hook(json(Term, []), Out, HdrExtra).
http:post_data_hook(json(Term, Options), Out, HdrExtra) :-
    option(content_type(Type), HdrExtra, 'application/json'),
    setup_call_cleanup(
        ( new_memory_file(MemFile),
          open_memory_file(MemFile, write, Handle)
        ),
        ( format(Handle, 'Content-type: ~w~n~n', [Type]),
          json_write_to(Handle, Term, Options)
        ),
        close(Handle)),
    setup_call_cleanup(
        open_memory_file(MemFile, read, RdHandle,
                         [ free_on_close(true)
                         ]),
        http_post_data(cgi_stream(RdHandle), Out, HdrExtra),
        close(RdHandle)).

json_write_to(Out, Term, Options) :-
    memberchk(json_object(dict), Options),
    !,
    json_write_dict(Out, Term, Options).
json_write_to(Out, Term, Options) :-
    json_write(Out, Term, Options).


%!  http_read_json(+Request, -JSON) is det.
%!  http_read_json(+Request, -JSON, +Options) is det.
%
%   Extract JSON data posted  to  this   HTTP  request.  Options are
%   passed to json_read/3.  In addition, this option is processed:
%
%     * json_object(+As)
%     One of =term= (default) to generate a classical Prolog
%     term or =dict= to exploit the SWI-Prolog version 7 data type
%     extensions.  See json_read_dict/3.
%
%   @error  domain_error(mimetype, Found) if the mimetype is
%           not known (see json_type/1).
%   @error  domain_error(method, Method) if the request method is not
%           a =POST=, =PUT= or =PATCH=.

http_read_json(Request, JSON) :-
    http_read_json(Request, JSON, []).

http_read_json(Request, JSON, Options) :-
    select_option(content_type(Type), Options, Rest),
    !,
    delete(Request, content_type(_), Request2),
    request_to_json([content_type(Type)|Request2], JSON, Rest).
http_read_json(Request, JSON, Options) :-
    request_to_json(Request, JSON, Options).

request_to_json(Request, JSON, Options) :-
    option(method(Method), Request),
    option(content_type(Type), Request),
    (   data_method(Method)
    ->  true
    ;   domain_error(method, Method)
    ),
    (   is_json_content_type(Type)
    ->  true
    ;   domain_error(mimetype, Type)
    ),
    http_read_data(Request, JSON, Options).

data_method(post).
data_method(put).
data_method(patch).

%!  http_read_json_dict(+Request, -Dict) is det.
%!  http_read_json_dict(+Request, -Dict, +Options) is det.
%
%   Similar to http_read_json/2,3, but by default uses the version 7
%   extended datatypes.

http_read_json_dict(Request, Dict) :-
    http_read_json_dict(Request, Dict, []).

http_read_json_dict(Request, Dict, Options) :-
    merge_options([json_object(dict)], Options, Options1),
    http_read_json(Request, Dict, Options1).

%!  reply_json(+JSONTerm) is det.
%!  reply_json(+JSONTerm, +Options) is det.
%
%   Formulate a JSON  HTTP  reply.   See  json_write/2  for details.
%   The processed options are listed below.  Remaining options are
%   forwarded to json_write/3.
%
%       * content_type(+Type)
%       The default =|Content-type|= is =|application/json;
%       charset=UTF8|=. =|charset=UTF8|= should not be required
%       because JSON is defined to be UTF-8 encoded, but some
%       clients insist on it.
%
%       * status(+Code)
%       The default status is 200.  REST API functions may use
%       other values from the 2XX range, such as 201 (created).
%
%       * json_object(+As)
%       One of =term= (classical json representation) or =dict=
%       to use the new dict representation.  If omitted and Term
%       is a dict, =dict= is assumed.  SWI-Prolog Version 7.

reply_json(Dict) :-
    is_dict(Dict),
    !,
    reply_json_dict(Dict).
reply_json(Term) :-
    format('Content-type: application/json; charset=UTF-8~n~n'),
    json_write(current_output, Term).

reply_json(Dict, Options) :-
    is_dict(Dict),
    !,
    reply_json_dict(Dict, Options).
reply_json(Term, Options) :-
    reply_json2(Term, Options).

%!  reply_json_dict(+JSONTerm) is det.
%!  reply_json_dict(+JSONTerm, +Options) is det.
%
%   As reply_json/1 and reply_json/2, but assumes the new dict based
%   data representation. Note that this is  the default if the outer
%   object is a dict. This predicate is   needed to serialize a list
%   of   objects   correctly   and     provides   consistency   with
%   http_read_json_dict/2 and friends.

reply_json_dict(Dict) :-
    format('Content-type: application/json; charset=UTF-8~n~n'),
    json_write_dict(current_output, Dict).

reply_json_dict(Dict, Options) :-
    merge_options([json_object(dict)], Options, Options1),
    reply_json2(Dict, Options1).

reply_json2(Term, Options) :-
    select_option(content_type(Type), Options, Rest0, 'application/json'),
    (   select_option(status(Code), Rest0, Rest)
    ->  format('Status: ~d~n', [Code])
    ;   Rest = Rest0
    ),
    format('Content-type: ~w~n~n', [Type]),
    json_write_to(current_output, Term, Rest).


		 /*******************************
		 *       STATUS HANDLING	*
		 *******************************/

:- multifile
    http:status_reply/3,
    http:serialize_reply/2.

http:serialize_reply(json(Term), body(application/json, utf8, Content)) :-
    with_output_to(string(Content),
                   json_write_dict(current_output, Term, [])).

http:status_reply(Term, json(Reply), Options) :-
    prefer_json(Options.get(accept)),
    json_status_reply(Term, Lines, Extra),
    phrase(txt_message_lines(Lines), Codes),
    string_codes(Message, Codes),
    Reply = _{code:Options.code, message:Message}.put(Extra).

txt_message_lines([]) -->
    [].
txt_message_lines([nl|T]) -->
    !,
    "\n",
    txt_message_lines(T).
txt_message_lines([flush]) -->
    !.
txt_message_lines([FmtArgs|T]) -->
    dcg_format(FmtArgs),
    txt_message_lines(T).

dcg_format(Fmt-Args, List, Tail) :-
    !,
    format(codes(List,Tail), Fmt, Args).
dcg_format(ansi(_Style, Fmt,Args), List, Tail) :-
    !,
    format(codes(List,Tail), Fmt, Args).
dcg_format(url(Pos), List, Tail) :-
    !,
    dcg_url(Pos, List, Tail).
dcg_format(url(_URL, Label), List, Tail) :-
    !,
    format(codes(List,Tail), '~w', [Label]).
dcg_format(Fmt, List, Tail) :-
    format(codes(List,Tail), Fmt, []).

dcg_url(File:Line:Column, List, Tail) :-
    !,
    format(codes(List,Tail), '~w:~d:~d', [File, Line, Column]).
dcg_url(File:Line, List, Tail) :-
    !,
    format(codes(List,Tail), '~w:~d', [File, Line]).
dcg_url(File, List, Tail) :-
    !,
    format(codes(List,Tail), '~w', [File]).


%!  prefer_json(+Accept)
%
%   True when the accept encoding prefers JSON.

prefer_json(Accept) :-
    memberchk(media(application/json, _, JSONP,  []), Accept),
    (   member(media(text/html, _, HTMLP,  []), Accept)
    ->  JSONP > HTMLP
    ;   true
    ).

%!  json_status_reply(+Term, -MsgLines, -ExtraJSON) is semidet.

json_status_reply(created(Location),
                  [ 'Created: ~w'-[Location] ],
                  _{location:Location}).
json_status_reply(moved(Location),
                  [ 'Moved to: ~w'-[Location] ],
                  _{location:Location}).
json_status_reply(moved_temporary(Location),
                  [ 'Moved temporary to: ~w'-[Location] ],
                  _{location:Location}).
json_status_reply(see_other(Location),
                  [ 'See: ~w'-[Location] ],
                  _{location:Location}).
json_status_reply(bad_request(ErrorTerm), Lines, _{}) :-
    '$messages':translate_message(ErrorTerm, Lines, []).
json_status_reply(authorise(Method),
                  [ 'Authorization (~p) required'-[Method] ],
                  _{}).
json_status_reply(forbidden(Location),
                  [ 'You have no permission to access: ~w'-[Location] ],
                  _{location:Location}).
json_status_reply(not_found(Location),
                  [ 'Path not found: ~w'-[Location] ],
                  _{location:Location}).
json_status_reply(method_not_allowed(Method,Location),
                  [ 'Method not allowed: ~w'-[UMethod] ],
                  _{location:Location, method:UMethod}) :-
    upcase_atom(Method, UMethod).
json_status_reply(not_acceptable(Why),
                  [ 'Request is not acceptable: ~p'-[Why]
                  ],
                  _{}).
json_status_reply(server_error(ErrorTerm), Lines, _{}) :-
    '$messages':translate_message(ErrorTerm, Lines, []).
json_status_reply(service_unavailable(Why),
                  [ 'Service unavailable: ~p'-[Why]
                  ],
                  _{}).
