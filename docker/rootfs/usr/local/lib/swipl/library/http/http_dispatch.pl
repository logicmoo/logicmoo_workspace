/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2020, University of Amsterdam
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

:- module(http_dispatch,
          [ http_dispatch/1,            % +Request
            http_handler/3,             % +Path, +Predicate, +Options
            http_delete_handler/1,      % +Path
            http_request_expansion/2,   % :Goal, +Rank
            http_reply_file/3,          % +File, +Options, +Request
            http_redirect/3,            % +How, +Path, +Request
            http_404/2,                 % +Options, +Request
            http_switch_protocol/2,     % :Goal, +Options
            http_current_handler/2,     % ?Path, ?Pred
            http_current_handler/3,     % ?Path, ?Pred, -Options
            http_location_by_id/2,      % +ID, -Location
            http_link_to_id/3,          % +ID, +Parameters, -HREF
            http_reload_with_parameters/3, % +Request, +Parameters, -HREF
            http_safe_file/2            % +Spec, +Options
          ]).
:- use_module(library(lists),
              [ select/3, append/3, append/2, same_length/2, member/2,
                last/2, delete/3
              ]).
:- autoload(library(apply),
	    [partition/4,maplist/3,maplist/2,include/3,exclude/3]).
:- autoload(library(broadcast),[listen/2]).
:- autoload(library(error),
	    [ must_be/2,
	      domain_error/2,
	      type_error/2,
	      instantiation_error/1,
	      existence_error/2,
	      permission_error/3
	    ]).
:- autoload(library(filesex),[directory_file_path/3]).
:- autoload(library(option),[option/3,option/2,merge_options/3]).
:- autoload(library(pairs),[pairs_values/2]).
:- if(exists_source(library(time))).
:- autoload(library(time),[call_with_time_limit/2]).
:- endif.
:- autoload(library(uri),
	    [ uri_encoded/3,
	      uri_data/3,
	      uri_components/2,
	      uri_query_components/2
	    ]).
:- autoload(library(http/http_header),[http_timestamp/2]).
:- autoload(library(http/http_path),[http_absolute_location/3]).
:- autoload(library(http/mimetype),
	    [file_content_type/2,file_content_type/3]).
:- if(exists_source(library(http/thread_httpd))).
:- autoload(library(http/thread_httpd),[http_spawn/2]).
:- endif.
:- use_module(library(settings),[setting/4,setting/2]).

:- predicate_options(http_404/2, 1, [index(any)]).
:- predicate_options(http_reply_file/3, 2,
                     [ cache(boolean),
                       mime_type(any),
                       static_gzip(boolean),
                       cached_gzip(boolean),
                       pass_to(http_safe_file/2, 2),
                       headers(list)
                     ]).
:- predicate_options(http_safe_file/2, 2, [unsafe(boolean)]).
:- predicate_options(http_switch_protocol/2, 2, []).

/** <module> Dispatch requests in the HTTP server

Most   code   doesn't   need  to   use  this   directly;  instead   use
library(http/http_server),  which  combines   this  library  with   the
typical HTTP libraries that most servers need.

This module can be placed between   http_wrapper.pl  and the application
code to associate HTTP _locations_ to   predicates that serve the pages.
In addition, it associates parameters  with   locations  that  deal with
timeout handling and user authentication.  The typical setup is:

==
server(Port, Options) :-
        http_server(http_dispatch,
                    [ port(Port)
                    | Options
                    ]).

:- http_handler('/index.html', write_index, []).

write_index(Request) :-
        ...
==
*/

:- setting(http:time_limit, nonneg, 300,
           'Time limit handling a single query (0=infinite)').

%!  http_handler(+Path, :Closure, +Options) is det.
%
%   Register Closure as a handler for HTTP   requests. Path is either an
%   absolute path such as =|'/home.html'|=   or  a term Alias(Relative).
%   Where Alias is associated with a concrete path using http:location/3
%   and resolved using http_absolute_location/3.  `Relative`   can  be a
%   single atom or a term `Segment1/Segment2/...`, where each element is
%   either an atom or a variable. If a  segment is a variable it matches
%   any segment and the binding may  be   passed  to the closure. If the
%   last segment is a variable  it   may  match  multiple segments. This
%   allows registering REST paths, for example:
%
%      ```
%      :- http_handler(root(user/User), user(Method, User),
%                      [ method(Method),
%                        methods([get,post,put])
%                      ]).
%
%      user(get, User, Request) :-
%          ...
%      user(post, User, Request) :-
%          ...
%      ```
%
%   If an HTTP request arrives at the  server that matches Path, Closure
%   is called as below, where `Request` is the parsed HTTP request.
%
%       call(Closure, Request)
%
%   Options  is  a  list containing the following options:
%
%     - authentication(+Type)
%       Demand authentication. Authentication methods are pluggable. The
%       library http_authenticate.pl provides a plugin for user/password
%       based =Basic= HTTP authentication.
%
%     - chunked
%       Use =|Transfer-encoding: chunked|= if the client allows for it.
%
%     - condition(:Goal)
%       If present, the handler is ignored if Goal does not succeed.
%
%     - content_type(+Term)
%       Specifies the content-type of the reply. This value is currently
%       not used by this library. It enhances the reflexive capabilities
%       of this library through http_current_handler/3.
%
%     - id(+Atom)
%       Identifier of the handler. The default identifier is the
%       predicate name. Used by http_location_by_id/2 and
%       http_link_to_id/3.
%
%     - hide_children(+Bool)
%       If =true= on a prefix-handler (see prefix), possible children
%       are masked. This can be used to (temporary) overrule part of the
%       tree.
%
%     - method(+Method)
%       Declare that the handler processes Method. This is equivalent to
%       methods([Method]). Using method(*) allows for all methods.
%
%     - methods(+ListOfMethods)
%       Declare that the handler processes all of the given methods. If
%       this option appears multiple times, the methods are combined.
%
%     - prefix
%       Call Pred on any location that is a specialisation of Path. If
%       multiple handlers match, the one with the longest path is used.
%       Options defined with a prefix handler are the default options
%       for paths that start with this prefix. Note that the handler
%       acts as a fallback handler for the tree below it:
%
%       ==
%       :- http_handler(/, http_404([index('index.html')]),
%                       [spawn(my_pool),prefix]).
%       ==
%
%     - priority(+Integer)
%       If two handlers handle the same path, the one with the highest
%       priority is used. If equal, the last registered is used. Please
%       be aware that the order of clauses in multifile predicates can
%       change due to reloading files. The default priority is 0 (zero).
%
%     - spawn(+SpawnOptions)
%       Run the handler in a separate thread. If SpawnOptions is an
%       atom, it is interpreted as a thread pool name (see
%       create_thread_pool/3). Otherwise the options are passed to
%       http_spawn/2 and from there to thread_create/3. These options
%       are typically used to set the stack limits.
%
%     - time_limit(+Spec)
%       One of =infinite=, =default= or a positive number (seconds). If
%       =default=, the value from the setting =http:time_limit= is
%       taken. The default of this setting is 300 (5 minutes). See
%       setting/2.
%
%   Note that http_handler/3 is normally  invoked   as  a  directive and
%   processed using term-expansion. Using  term-expansion ensures proper
%   update through make/0 when the specification is modified.
%
%   @error  existence_error(http_location, Location)
%   @error  permission_error(http_method, Method, Location)
%   @see    http_reply_file/3 and http_redirect/3 are generic
%           handlers to serve files and achieve redirects.

:- dynamic handler/4.                   % Path, Action, IsPrefix, Options
:- multifile handler/4.
:- dynamic generation/1.

:- meta_predicate
    http_handler(+, :, +),
    http_current_handler(?, :),
    http_current_handler(?, :, ?),
    http_request_expansion(3, +),
    http_switch_protocol(2, +).

http_handler(Path, Pred, Options) :-
    compile_handler(Path, Pred, Options, Clause),
    next_generation,
    assert(Clause).

:- multifile
    system:term_expansion/2.

system:term_expansion((:- http_handler(Path, Pred, Options)), Clause) :-
    \+ current_prolog_flag(xref, true),
    prolog_load_context(module, M),
    compile_handler(Path, M:Pred, Options, Clause),
    next_generation.


%!  http_delete_handler(+Spec) is det.
%
%   Delete handler for Spec. Typically, this should only be used for
%   handlers that are registered dynamically. Spec is one of:
%
%       * id(Id)
%       Delete a handler with the given id.  The default id is the
%       handler-predicate-name.
%
%       * path(Path)
%       Delete handler that serves the given path.

http_delete_handler(id(Id)) :-
    !,
    clause(handler(_Path, _:Pred, _, Options), true, Ref),
    functor(Pred, DefID, _),
    option(id(Id0), Options, DefID),
    Id == Id0,
    erase(Ref),
    next_generation.
http_delete_handler(path(Path)) :-
    !,
    retractall(handler(Path, _Pred, _, _Options)),
    next_generation.
http_delete_handler(Path) :-
    http_delete_handler(path(Path)).


%!  next_generation is det.
%!  current_generation(-G) is det.
%
%   Increment the generation count.

next_generation :-
    retractall(id_location_cache(_,_,_,_)),
    with_mutex(http_dispatch, next_generation_unlocked).

next_generation_unlocked :-
    retract(generation(G0)),
    !,
    G is G0 + 1,
    assert(generation(G)).
next_generation_unlocked :-
    assert(generation(1)).

current_generation(G) :-
    with_mutex(http_dispatch, generation(G)),
    !.
current_generation(0).


%!  compile_handler(+Path, :Pred, +Options, -Clause) is det.
%
%   Compile a handler specification.

compile_handler(Path, Pred, Options0,
                http_dispatch:handler(Path1, Pred, IsPrefix, Options)) :-
    check_path(Path, Path1, PathOptions),
    check_id(Options0),
    (   memberchk(segment_pattern(_), PathOptions)
    ->  IsPrefix = true,
        Options1 = Options0
    ;   select(prefix, Options0, Options1)
    ->  IsPrefix = true
    ;   IsPrefix = false,
        Options1 = Options0
    ),
    partition(ground, Options1, Options2, QueryOptions),
    Pred = M:_,
    maplist(qualify_option(M), Options2, Options3),
    combine_methods(Options3, Options4),
    (   QueryOptions == []
    ->  append(PathOptions, Options4, Options)
    ;   append(PathOptions, ['$extract'(QueryOptions)|Options4], Options)
    ).

qualify_option(M, condition(Pred), condition(M:Pred)) :-
    Pred \= _:_, !.
qualify_option(_, Option, Option).

%!  combine_methods(+OptionsIn, -Options) is det.
%
%   Combine method(M) and  methods(MList)  options   into  a  single
%   methods(MList) option.

combine_methods(Options0, Options) :-
    collect_methods(Options0, Options1, Methods),
    (   Methods == []
    ->  Options = Options0
    ;   append(Methods, Flat),
        sort(Flat, Unique),
        (   memberchk('*', Unique)
        ->  Final = '*'
        ;   Final = Unique
        ),
        Options = [methods(Final)|Options1]
    ).

collect_methods([], [], []).
collect_methods([method(M)|T0], T, [[M]|TM]) :-
    !,
    (   M == '*'
    ->  true
    ;   must_be_method(M)
    ),
    collect_methods(T0, T, TM).
collect_methods([methods(M)|T0], T, [M|TM]) :-
    !,
    must_be(list, M),
    maplist(must_be_method, M),
    collect_methods(T0, T, TM).
collect_methods([H|T0], [H|T], TM) :-
    !,
    collect_methods(T0, T, TM).

must_be_method(M) :-
    must_be(atom, M),
    (   method(M)
    ->  true
    ;   domain_error(http_method, M)
    ).

method(get).
method(put).
method(head).
method(post).
method(delete).
method(patch).
method(options).
method(trace).


%!  check_path(+PathSpecIn, -PathSpecOut, -Options) is det.
%
%   Validate the given path specification.  We want one of
%
%     - AbsoluteLocation
%     - Alias(Relative)
%
%   Similar  to  absolute_file_name/3,   Relative   can    be   a   term
%   ``Component/Component/...``. Relative may be a `/` separated list of
%   path segments, some of which may   be  variables. A variable patches
%   any segment and its binding can be passed  to the handler. If such a
%   pattern     is     found      Options       is      unified     with
%   `[segment_pattern(SegmentList)]`.
%
%   @error  domain_error, type_error
%   @see    http_absolute_location/3

check_path(Path, Path, []) :-
    atom(Path),
    !,
    (   sub_atom(Path, 0, _, _, /)
    ->  true
    ;   domain_error(absolute_http_location, Path)
    ).
check_path(Alias, AliasOut, Options) :-
    compound(Alias),
    Alias =.. [Name, Relative],
    !,
    local_path(Relative, Local, Options),
    (   sub_atom(Local, 0, _, _, /)
    ->  domain_error(relative_location, Relative)
    ;   AliasOut =.. [Name, Local]
    ).
check_path(PathSpec, _, _) :-
    type_error(path_or_alias, PathSpec).

local_path(Atom, Atom, []) :-
    atom(Atom),
    !.
local_path(Path, Atom, Options) :-
    phrase(path_to_list(Path), Components),
    !,
    (   maplist(atom, Components)
    ->  atomic_list_concat(Components, '/', Atom),
        Options = []
    ;   append(Pre, [Var|Rest], Components),
        var(Var)
    ->  append(Pre, [''], PreSep),
        atomic_list_concat(PreSep, '/', Atom),
        Options = [segment_pattern([Var|Rest])]
    ).
local_path(Path, _, _) :-
    ground(Path),
    !,
    type_error(relative_location, Path).
local_path(Path, _, _) :-
    instantiation_error(Path).

path_to_list(Var) -->
    { var(Var) },
    !,
    [Var].
path_to_list(A/B) -->
    !,
    path_to_list(A),
    path_to_list(B).
path_to_list(Atom) -->
    { atom(Atom) },
    !,
    [Atom].
path_to_list(Value) -->
    { must_be(atom, Value) }.

check_id(Options) :-
    memberchk(id(Id), Options),
    !,
    must_be(atom, Id).
check_id(_).


%!  http_dispatch(Request) is det.
%
%   Dispatch a Request using http_handler/3   registrations. It performs
%   the following steps:
%
%     1. Find a matching handler based on the `path` member of Request.
%        If multiple handlers match due to the `prefix` option or
%        variables in path segments (see http_handler/3), the longest
%        specification is used.  If multiple specifications of equal
%        length match the one with the highest priority is used.
%     2. Check that the handler matches the `method` member of the
%        Request or throw permission_error(http_method, Method, Location)
%     3. Expand the request using expansion hooks registered by
%        http_request_expansion/3.  This may add fields to the request,
%        such the authenticated user, parsed parameters, etc.  The
%        hooks may also throw exceptions, notably using http_redirect/3
%        or by throwing `http_reply(Term, ExtraHeader, Context)`
%        exceptions.
%     4. Extract possible fields from the Request using e.g.
%        method(Method) as one of the options.
%     5. Call the registered _closure_, optionally spawning the
%        request to a new thread or enforcing a time limit.

http_dispatch(Request) :-
    memberchk(path(Path), Request),
    find_handler(Path, Closure, Options),
    supports_method(Request, Options),
    expand_request(Request, Request1, Options),
    extract_from_request(Request1, Options),
    action(Closure, Request1, Options).

extract_from_request(Request, Options) :-
    memberchk('$extract'(Fields), Options),
    !,
    extract_fields(Fields, Request).
extract_from_request(_, _).

extract_fields([], _).
extract_fields([H|T], Request) :-
    memberchk(H, Request),
    extract_fields(T, Request).


%!  http_request_expansion(:Goal, +Rank:number)
%
%   Register Goal for expanding the HTTP request handler. Goal is called
%   as below. If Goal fail the request   is passed to the next expansion
%   unmodified.
%
%       call(Goal, Request0, Request, Options)
%
%   If multiple goals are  registered  they   expand  the  request  in a
%   pipeline starting with the expansion hook with the lowest rank.
%
%   Besides rewriting the request, for example   by  validating the user
%   identity based on HTTP authentication or  cookies and adding this to
%   the request, the hook may raise HTTP exceptions to indicate a bad
%   request, permission error, etc.  See http_status_reply/4.
%
%   Initially, auth_expansion/3 is registered with   rank  `100` to deal
%   with the older http:authenticate/3 hook.

http_request_expansion(Goal, Rank) :-
    throw(error(context_error(nodirective, http_request_expansion(Goal, Rank)), _)).

:- multifile
    request_expansion/2.

system:term_expansion((:- http_request_expansion(Goal, Rank)),
                      http_dispatch:request_expansion(M:Callable, Rank)) :-
    must_be(number, Rank),
    prolog_load_context(module, M0),
    strip_module(M0:Goal, M, Callable),
    must_be(callable, Callable).

request_expanders(Closures) :-
    findall(Rank-Closure, request_expansion(Closure, Rank), Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Closures).

%!  expand_request(+Request0, -Request, +Options)
%
%   Expand an HTTP request.  Options  is   a  list  of  combined options
%   provided with the handler registration (see http_handler/3).

expand_request(Request0, Request, Options) :-
    request_expanders(Closures),
    expand_request(Closures, Request0, Request, Options).

expand_request([], Request, Request, _).
expand_request([H|T], Request0, Request, Options) :-
    expand_request1(H, Request0, Request1, Options),
    expand_request(T, Request1, Request, Options).

expand_request1(Closure, Request0, Request, Options) :-
    call(Closure, Request0, Request, Options),
    !.
expand_request1(_, Request, Request, _).


%!  http_current_handler(+Location, :Closure) is semidet.
%!  http_current_handler(-Location, :Closure) is nondet.
%
%   True if Location is handled by Closure.

http_current_handler(Path, Closure) :-
    atom(Path),
    !,
    path_tree(Tree),
    find_handler(Tree, Path, Closure, _).
http_current_handler(Path, M:C) :-
    handler(Spec, M:C, _, _),
    http_absolute_location(Spec, Path, []).

%!  http_current_handler(+Location, :Closure, -Options) is semidet.
%!  http_current_handler(?Location, :Closure, ?Options) is nondet.
%
%   Resolve the current handler and options to execute it.

http_current_handler(Path, Closure, Options) :-
    atom(Path),
    !,
    path_tree(Tree),
    find_handler(Tree, Path, Closure, Options).
http_current_handler(Path, M:C, Options) :-
    handler(Spec, M:C, _, _),
    http_absolute_location(Spec, Path, []),
    path_tree(Tree),
    find_handler(Tree, Path, _, Options).


%!  http_location_by_id(+ID, -Location) is det.
%
%   True when Location represents the  HTTP   path  to which the handler
%   with identifier ID is bound. Handler   identifiers  are deduced from
%   the http_handler/3 declaration as follows:
%
%       $ Explicit id :
%       If a term id(ID) appears in the option list of the handler, ID
%       it is used and takes preference over using the predicate.
%       $ Using the handler predicate :
%       ID matches a handler if the predicate name matches ID.  The
%       ID may have a module qualification, e.g., `Module:Pred`
%
%   If the handler is declared with   a  pattern, e.g., root(user/User),
%   the location to access a  particular   _user_  may be accessed using
%   e.g., user('Bob'). The number of arguments to the compound term must
%   match the number of variables in the path pattern.
%
%   A plain atom ID can be used to   find  a handler with a pattern. The
%   returned location is the  path  up   to  the  first  variable, e.g.,
%   =|/user/|= in the example above.
%
%   User code is adviced to  use   http_link_to_id/3  which can also add
%   query parameters to  the  URL.  This   predicate  is  a  helper  for
%   http_link_to_id/3.
%
%   @error existence_error(http_handler_id, Id).
%   @see http_link_to_id/3 and the library(http/html_write) construct
%   location_by_id(ID) or its abbreviation `#(ID)`

:- dynamic
    id_location_cache/4.                        % Id, Argv, Location, Segments

http_location_by_id(ID, _) :-
    \+ ground(ID),
    !,
    instantiation_error(ID).
http_location_by_id(M:ID, Location) :-
    compound(ID),
    !,
    compound_name_arguments(ID, Name, Argv),
    http_location_by_id(M:Name, Argv, Location).
http_location_by_id(M:ID, Location) :-
    atom(ID),
    must_be(atom, M),
    !,
    http_location_by_id(M:ID, -, Location).
http_location_by_id(ID, Location) :-
    compound(ID),
    !,
    compound_name_arguments(ID, Name, Argv),
    http_location_by_id(Name, Argv, Location).
http_location_by_id(ID, Location) :-
    atom(ID),
    !,
    http_location_by_id(ID, -, Location).
http_location_by_id(ID, _) :-
    type_error(location_id, ID).

http_location_by_id(ID, Argv, Location) :-
    id_location_cache(ID, Argv, Segments, Path),
    !,
    add_segments(Path, Segments, Location).
http_location_by_id(ID, Argv, Location) :-
    findall(t(Priority, ArgvP, Segments, Prefix),
            location_by_id(ID, Argv, ArgvP, Segments, Prefix, Priority),
            List),
    sort(1, >=, List, Sorted),
    (   Sorted = [t(_,ArgvP,Segments,Path)]
    ->  assert(id_location_cache(ID,ArgvP,Segments,Path)),
        Argv = ArgvP
    ;   List == []
    ->  existence_error(http_handler_id, ID)
    ;   List = [t(P0,ArgvP,Segments,Path),t(P1,_,_,_)|_]
    ->  (   P0 =:= P1
        ->  print_message(warning,
                          http_dispatch(ambiguous_id(ID, Sorted, Path)))
        ;   true
        ),
        assert(id_location_cache(ID,Argv,Segments,Path)),
        Argv = ArgvP
    ),
    add_segments(Path, Segments, Location).

add_segments(Path0, [], Path) :-
    !,
    Path = Path0.
add_segments(Path0, Segments, Path) :-
    maplist(uri_encoded(path), Segments, Encoded),
    atomic_list_concat(Encoded, '/', Rest),
    atom_concat(Path0, Rest, Path).

location_by_id(ID, -, _, [], Location, Priority) :-
    !,
    location_by_id_raw(ID, L0, _Segments, Priority),
    to_path(L0, Location).
location_by_id(ID, Argv, ArgvP, Segments, Location, Priority) :-
    location_by_id_raw(ID, L0, Segments, Priority),
    include(var, Segments, ArgvP),
    same_length(Argv, ArgvP),
    to_path(L0, Location).

to_path(prefix(Path0), Path) :-         % old style prefix notation
    !,
    add_prefix(Path0, Path).
to_path(Path0, Path) :-
    atomic(Path0),                      % old style notation
    !,
    add_prefix(Path0, Path).
to_path(Spec, Path) :-                  % new style notation
    http_absolute_location(Spec, Path, []).

add_prefix(P0, P) :-
    (   catch(setting(http:prefix, Prefix), _, fail),
        Prefix \== ''
    ->  atom_concat(Prefix, P0, P)
    ;   P = P0
    ).

location_by_id_raw(ID, Location, Pattern, Priority) :-
    handler(Location, _, _, Options),
    option(id(ID), Options),
    option(priority(P0), Options, 0),
    option(segment_pattern(Pattern), Options, []),
    Priority is P0+1000.            % id(ID) takes preference over predicate
location_by_id_raw(ID, Location, Pattern, Priority) :-
    handler(Location, M:C, _, Options),
    option(priority(Priority), Options, 0),
    functor(C, PN, _),
    (   ID = M:PN
    ->  true
    ;   ID = PN
    ),
    option(segment_pattern(Pattern), Options, []).

%!  http_link_to_id(+HandleID, +Parameters, -HREF)
%
%   HREF is a link on the local server   to a handler with given ID,
%   passing the given Parameters. This   predicate is typically used
%   to formulate a HREF that resolves   to  a handler implementing a
%   particular predicate. The code below provides a typical example.
%   The predicate user_details/1 returns a page with details about a
%   user from a given id. This predicate is registered as a handler.
%   The DCG user_link//1 renders a link   to  a user, displaying the
%   name and calling user_details/1  when   clicked.  Note  that the
%   location (root(user_details)) is irrelevant in this equation and
%   HTTP locations can thus be moved   freely  without breaking this
%   code fragment.
%
%     ```
%     :- http_handler(root(user_details), user_details, []).
%
%     user_details(Request) :-
%         http_parameters(Request,
%                         [ user_id(ID)
%                         ]),
%         ...
%
%     user_link(ID) -->
%         { user_name(ID, Name),
%           http_link_to_id(user_details, [id(ID)], HREF)
%         },
%         html(a([class(user), href(HREF)], Name)).
%     ```
%
%   @arg HandleID is either an atom, possibly module qualified
%   predicate or a compound term if the hander is defined using
%   a pattern.  See http_handler/3 and http_location_by_id/2.
%
%   @arg Parameters is one of
%
%     - path_postfix(File) to pass a single value as the last
%       segment of the HTTP location (path). This way of
%       passing a parameter is commonly used in REST APIs.
%
%       New code should use a path pattern in the handler declaration
%       and a term `HandleID(Arg, ...)`
%
%     - A list of search parameters for a =GET= request.
%
%   @see    http_location_by_id/2 and http_handler/3 for defining and
%           specifying handler IDs.

http_link_to_id(HandleID, path_postfix(File), HREF) :-
    !,
    http_location_by_id(HandleID, HandlerLocation),
    uri_encoded(path, File, EncFile),
    directory_file_path(HandlerLocation, EncFile, Location),
    uri_data(path, Components, Location),
    uri_components(HREF, Components).
http_link_to_id(HandleID, Parameters, HREF) :-
    must_be(list, Parameters),
    http_location_by_id(HandleID, Location),
    (   Parameters == []
    ->  HREF = Location
    ;   uri_data(path, Components, Location),
        uri_query_components(String, Parameters),
        uri_data(search, Components, String),
        uri_components(HREF, Components)
    ).

%!  http_reload_with_parameters(+Request, +Parameters, -HREF) is det.
%
%   Create a request on the current handler with replaced search
%   parameters.

http_reload_with_parameters(Request, NewParams, HREF) :-
    memberchk(path(Path), Request),
    (   memberchk(search(Params), Request)
    ->  true
    ;   Params = []
    ),
    merge_options(NewParams, Params, AllParams),
    uri_query_components(Search, AllParams),
    uri_data(path, Data, Path),
    uri_data(search, Data, Search),
    uri_components(HREF, Data).


%       hook into html_write:attribute_value//1.

:- multifile
    html_write:expand_attribute_value//1.

html_write:expand_attribute_value(location_by_id(ID)) -->
    { http_location_by_id(ID, Location) },
    html_write:html_quoted_attribute(Location).
html_write:expand_attribute_value(#(ID)) -->
    { http_location_by_id(ID, Location) },
    html_write:html_quoted_attribute(Location).


%!  authentication(+Options, +Request, -Fields) is det.
%
%   Verify  authentication  information.   If    authentication   is
%   requested through Options, demand it. The actual verification is
%   done by the multifile predicate http:authenticate/3. The library
%   http_authenticate.pl provides an implementation thereof.
%
%   @error  permission_error(access, http_location, Location)
%   @deprecated This hook predates the extensible request
%   expansion provided by http_request_expansion/2. New hooks should use
%   http_request_expansion/2 instead of http:authenticate/3.

:- multifile
    http:authenticate/3.

authentication([], _, []).
authentication([authentication(Type)|Options], Request, Fields) :-
    !,
    (   http:authenticate(Type, Request, XFields)
    ->  append(XFields, More, Fields),
        authentication(Options, Request, More)
    ;   memberchk(path(Path), Request),
        permission_error(access, http_location, Path)
    ).
authentication([_|Options], Request, Fields) :-
    authentication(Options, Request, Fields).

:- http_request_expansion(auth_expansion, 100).

%!  auth_expansion(+Request0, -Request, +Options) is semidet.
%
%   Connect  the  HTTP  authentication  infrastructure    by   means  of
%   http_request_expansion/2.
%
%   @see http:authenticate/3, http_digest.pl and http_authenticate.pl

auth_expansion(Request0, Request, Options) :-
    authentication(Options, Request0, Extra),
    append(Extra, Request0, Request).

%!  find_handler(+Path, -Action, -Options) is det.
%
%   Find the handler to call from Path.  Rules:
%
%           * If there is a matching handler, use this.
%           * If there are multiple prefix(Path) handlers, use the
%             longest.
%
%   If there is a handler for =|/dir/|=   and  the requested path is
%   =|/dir|=, find_handler/3 throws a  http_reply exception, causing
%   the wrapper to generate a 301 (Moved Permanently) reply.
%
%   @error  existence_error(http_location, Location)
%   @throw  http_reply(moved(Dir))
%   @tbd    Introduce automatic redirection to indexes here?

find_handler(Path, Action, Options) :-
    path_tree(Tree),
    (   find_handler(Tree, Path, Action, Options),
        eval_condition(Options)
    ->  true
    ;   \+ sub_atom(Path, _, _, 0, /),
        atom_concat(Path, /, Dir),
        find_handler(Tree, Dir, Action, Options)
    ->  throw(http_reply(moved(Dir)))
    ;   throw(error(existence_error(http_location, Path), _))
    ).


find_handler([node(prefix(Prefix), PAction, POptions, Children)|_],
             Path, Action, Options) :-
    sub_atom(Path, 0, _, After, Prefix),
    !,
    (   option(hide_children(false), POptions, false),
        find_handler(Children, Path, Action, Options)
    ->  true
    ;   member(segment_pattern(Pattern, PatAction, PatOptions), POptions),
        copy_term(t(Pattern,PatAction,PatOptions), t(Pattern2,Action,Options)),
        match_segments(After, Path, Pattern2)
    ->  true
    ;   PAction \== nop
    ->  Action = PAction,
        path_info(After, Path, POptions, Options)
    ).
find_handler([node(Path, Action, Options, _)|_], Path, Action, Options) :- !.
find_handler([_|Tree], Path, Action, Options) :-
    find_handler(Tree, Path, Action, Options).

path_info(0, _, Options,
          [prefix(true)|Options]) :- !.
path_info(After, Path, Options,
          [path_info(PathInfo),prefix(true)|Options]) :-
    sub_atom(Path, _, After, 0, PathInfo).

match_segments(After, Path, [Var]) :-
    !,
    sub_atom(Path, _, After, 0, Var).
match_segments(After, Path, Pattern) :-
    sub_atom(Path, _, After, 0, PathInfo),
    split_string(PathInfo, "/", "", Segments),
    match_segment_pattern(Pattern, Segments).

match_segment_pattern([], []).
match_segment_pattern([Var], Segments) :-
    !,
    atomic_list_concat(Segments, '/', Var).
match_segment_pattern([H0|T0], [H|T]) :-
    atom_string(H0, H),
    match_segment_pattern(T0, T).


eval_condition(Options) :-
    (   memberchk(condition(Cond), Options)
    ->  catch(Cond, E, (print_message(warning, E), fail))
    ;   true
    ).


%!  supports_method(+Request, +Options) is det.
%
%   Verify that the asked http method   is supported by the handler.
%   If not, raise an error that will be  mapped to a 405 page by the
%   http wrapper.
%
%   @error permission_error(http_method, Method, Location).

supports_method(Request, Options) :-
    (   option(methods(Methods), Options)
    ->  (   Methods == '*'
        ->  true
        ;   memberchk(method(Method), Request),
            memberchk(Method, Methods)
        )
    ;   true
    ),
    !.
supports_method(Request, _Options) :-
    memberchk(path(Location), Request),
    memberchk(method(Method), Request),
    permission_error(http_method, Method, Location).


%!  action(+Action, +Request, +Options) is det.
%
%   Execute the action found.  Here we take care of the options
%   =time_limit=, =chunked= and =spawn=.
%
%   @error  goal_failed(Goal)

action(Action, Request, Options) :-
    memberchk(chunked, Options),
    !,
    format('Transfer-encoding: chunked~n'),
    spawn_action(Action, Request, Options).
action(Action, Request, Options) :-
    spawn_action(Action, Request, Options).

:- if(current_predicate(http_spawn/2)).
spawn_action(Action, Request, Options) :-
    option(spawn(Spawn), Options),
    !,
    spawn_options(Spawn, SpawnOption),
    http_spawn(time_limit_action(Action, Request, Options), SpawnOption).
:- endif.
spawn_action(Action, Request, Options) :-
    time_limit_action(Action, Request, Options).

spawn_options([], []) :- !.
spawn_options(Pool, Options) :-
    atom(Pool),
    !,
    Options = [pool(Pool)].
spawn_options(List, List).

:- if(current_predicate(call_with_time_limit/2)).
time_limit_action(Action, Request, Options) :-
    (   option(time_limit(TimeLimit), Options),
        TimeLimit \== default
    ->  true
    ;   setting(http:time_limit, TimeLimit)
    ),
    number(TimeLimit),
    TimeLimit > 0,
    !,
    call_with_time_limit(TimeLimit, call_action(Action, Request, Options)).
:- endif.
time_limit_action(Action, Request, Options) :-
    call_action(Action, Request, Options).


%!  call_action(+Action, +Request, +Options)
%
%   @tbd    reply_file is normal call?

call_action(reply_file(File, FileOptions), Request, _Options) :-
    !,
    http_reply_file(File, FileOptions, Request).
call_action(Pred, Request, Options) :-
    memberchk(path_info(PathInfo), Options),
    !,
    call_action(Pred, [path_info(PathInfo)|Request]).
call_action(Pred, Request, _Options) :-
    call_action(Pred, Request).

call_action(Pred, Request) :-
    (   call(Pred, Request)
    ->  true
    ;   extend(Pred, [Request], Goal),
        throw(error(goal_failed(Goal), _))
    ).

extend(Var, _, Var) :-
    var(Var),
    !.
extend(M:G0, Extra, M:G) :-
    extend(G0, Extra, G).
extend(G0, Extra, G) :-
    G0 =.. List,
    append(List, Extra, List2),
    G =.. List2.

%!  http_reply_file(+FileSpec, +Options, +Request) is det.
%
%   Options is a list of
%
%           * cache(+Boolean)
%           If =true= (default), handle If-modified-since and send
%           modification time.
%
%           * mime_type(+Type)
%           Overrule mime-type guessing from the filename as
%           provided by file_mime_type/2.
%
%           * static_gzip(+Boolean)
%           If `true` (default `false`) and, in addition to the plain
%           file, there is a ``.gz`` file that is not older than the
%           plain file and the client acceps =gzip= encoding, send
%           the compressed file with ``Transfer-encoding: gzip``.
%
%           * cached_gzip(+Boolean)
%           If `true` (default `false`) the system maintains cached
%           gzipped files in a directory accessible using the file
%           search path `http_gzip_cache` and serves these similar
%           to the `static_gzip(true)` option.  If the gzip file
%           does not exist or is older than the input the file is
%           recreated.
%
%           * unsafe(+Boolean)
%           If =false= (default), validate that FileSpec does not
%           contain references to parent directories.  E.g.,
%           specifications such as =|www('../../etc/passwd')|= are
%           not allowed.
%
%           * headers(+List)
%           Provides additional reply-header fields, encoded as a
%           list of _|Field(Value)|_.
%
%   If caching is not disabled,  it   processes  the request headers
%   =|If-modified-since|= and =Range=.
%
%   @throws http_reply(not_modified)
%   @throws http_reply(file(MimeType, Path))

http_reply_file(File, Options, Request) :-
    http_safe_file(File, Options),
    absolute_file_name(File, Path,
                       [ access(read)
                       ]),
    (   option(cache(true), Options, true)
    ->  (   memberchk(if_modified_since(Since), Request),
            time_file(Path, Time),
            catch(http_timestamp(Time, Since), _, fail)
        ->  throw(http_reply(not_modified))
        ;   true
        ),
        (   memberchk(range(Range), Request)
        ->  Reply = file(Type, Path, Range)
        ;   option(static_gzip(true), Options),
            accepts_encoding(Request, gzip),
            file_name_extension(Path, gz, PathGZ),
            access_file(PathGZ, read),
            time_file(PathGZ, TimeGZ),
            time_file(Path, Time),
            TimeGZ >= Time
        ->  Reply = gzip_file(Type, PathGZ)
        ;   option(cached_gzip(true), Options),
            accepts_encoding(Request, gzip),
            gzip_cached(Path, PathGZ)
        ->  Reply = gzip_file(Type, PathGZ)
        ;   Reply = file(Type, Path)
        )
    ;   Reply = tmp_file(Type, Path)
    ),
    (   option(mime_type(MediaType), Options)
    ->  file_content_type(Path, MediaType, Type)
    ;   file_content_type(Path, Type)
    ->  true
    ;   Type = text/plain           % fallback type
    ),
    option(headers(Headers), Options, []),
    throw(http_reply(Reply, Headers)).

accepts_encoding(Request, Enc) :-
    memberchk(accept_encoding(Accept), Request),
    split_string(Accept, ",", " ", Parts),
    member(Part, Parts),
    split_string(Part, ";", " ", [EncS|_]),
    atom_string(Enc, EncS).

gzip_cached(Path, PathGZ) :-
    with_mutex(http_reply_file, gzip_cached_sync(Path, PathGZ)).

gzip_cached_sync(Path, PathGZ) :-
    time_file(Path, Time),
    variant_sha1(Path, SHA1),
    (   absolute_file_name(http_gzip_cache(SHA1),
                           PathGZ,
                           [ access(read),
                             file_errors(fail)
                           ]),
        time_file(PathGZ, TimeGZ),
        TimeGZ >= Time
    ->  true
    ;   absolute_file_name(http_gzip_cache(SHA1),
                           PathGZ,
                           [ access(write),
                             file_errors(fail)
                           ])
    ->  setup_call_cleanup(
            gzopen(PathGZ, write, Out, [type(binary)]),
            setup_call_cleanup(
                open(Path, read, In, [type(binary)]),
                copy_stream_data(In, Out),
                close(In)),
            close(Out))
    ).

%!  http_safe_file(+FileSpec, +Options) is det.
%
%   True if FileSpec is considered _safe_.  If   it  is  an atom, it
%   cannot  be  absolute  and  cannot   have  references  to  parent
%   directories. If it is of the   form  alias(Sub), than Sub cannot
%   have references to parent directories.
%
%   @error instantiation_error
%   @error permission_error(read, file, FileSpec)

http_safe_file(File, _) :-
    var(File),
    !,
    instantiation_error(File).
http_safe_file(_, Options) :-
    option(unsafe(true), Options, false),
    !.
http_safe_file(File, _) :-
    http_safe_file(File).

http_safe_file(File) :-
    compound(File),
    functor(File, _, 1),
    !,
    arg(1, File, Name),
    safe_name(Name, File).
http_safe_file(Name) :-
    (   is_absolute_file_name(Name)
    ->  permission_error(read, file, Name)
    ;   true
    ),
    safe_name(Name, Name).

safe_name(Name, _) :-
    must_be(atom, Name),
    prolog_to_os_filename(FileName, Name),
    \+ unsafe_name(FileName),
    !.
safe_name(_, Spec) :-
    permission_error(read, file, Spec).

unsafe_name(Name) :- Name == '..'.
unsafe_name(Name) :- sub_atom(Name, 0, _, _, '../').
unsafe_name(Name) :- sub_atom(Name, _, _, _, '/../').
unsafe_name(Name) :- sub_atom(Name, _, _, 0, '/..').


%!  http_redirect(+How, +To, +Request) is det.
%
%   Redirect to a new  location.  The   argument  order,  using  the
%   Request as last argument, allows for  calling this directly from
%   the handler declaration:
%
%       ```
%       :- http_handler(root(.),
%                       http_redirect(moved, myapp('index.html')),
%                       []).
%       ```
%
%   @param How is one of `moved`, `moved_temporary` or `see_other`
%   @param To is an atom, a aliased path as defined by
%   http_absolute_location/3. or a term location_by_id(Id) or its
%   abbreviations `#(Id)` or `#(Id)+Parameters`. If To is not absolute,
%   it is resolved relative to the current location.

http_redirect(How, To, Request) :-
    must_be(oneof([moved, moved_temporary, see_other]), How),
    must_be(ground, To),
    (   id_location(To, URL)
    ->  true
    ;   memberchk(path(Base), Request),
        http_absolute_location(To, URL, [relative_to(Base)])
    ),
    Term =.. [How,URL],
    throw(http_reply(Term)).

id_location(location_by_id(Id), URL) :-
    http_location_by_id(Id, URL).
id_location(#(Id), URL) :-
    http_location_by_id(Id, URL).
id_location(#(Id)+Parameters, URL) :-
    http_link_to_id(Id, Parameters, URL).


%!  http_404(+Options, +Request) is det.
%
%   Reply using an "HTTP  404  not   found"  page.  This  handler is
%   intended as fallback handler  for   _prefix_  handlers.  Options
%   processed are:
%
%       * index(Location)
%       If there is no path-info, redirect the request to
%       Location using http_redirect/3.
%
%   @error http_reply(not_found(Path))

http_404(Options, Request) :-
    option(index(Index), Options),
    \+ ( option(path_info(PathInfo), Request),
         PathInfo \== ''
       ),
    !,
    http_redirect(moved, Index, Request).
http_404(_Options, Request) :-
    option(path(Path), Request),
    !,
    throw(http_reply(not_found(Path))).
http_404(_Options, Request) :-
    domain_error(http_request, Request).


%!  http_switch_protocol(:Goal, +Options)
%
%   Send an =|"HTTP 101 Switching  Protocols"|= reply. After sending
%   the  reply,  the  HTTP  library    calls   call(Goal,  InStream,
%   OutStream), where InStream and OutStream are  the raw streams to
%   the HTTP client. This allows the communication to continue using
%   an an alternative protocol.
%
%   If Goal fails or throws an exception,  the streams are closed by
%   the server. Otherwise  Goal  is   responsible  for  closing  the
%   streams. Note that  Goal  runs  in   the  HTTP  handler  thread.
%   Typically, the handler should be   registered  using the =spawn=
%   option if http_handler/3 or Goal   must  call thread_create/3 to
%   allow the HTTP worker to return to the worker pool.
%
%   The streams use binary  (octet)  encoding   and  have  their I/O
%   timeout set to the server  timeout   (default  60  seconds). The
%   predicate set_stream/2 can  be  used   to  change  the encoding,
%   change or cancel the timeout.
%
%   This predicate interacts with the server  library by throwing an
%   exception.
%
%   The following options are supported:
%
%     - header(+Headers)
%     Backward compatible.  Use headers(+Headers).
%     - headers(+Headers)
%     Additional headers send with the reply. Each header takes the
%     form Name(Value).

%       @throws http_reply(switch_protocol(Goal, Options))

http_switch_protocol(Goal, Options) :-
    throw(http_reply(switching_protocols(Goal, Options))).


                 /*******************************
                 *        PATH COMPILATION      *
                 *******************************/

%!  path_tree(-Tree) is det.
%
%   Compile paths into  a  tree.  The   treee  is  multi-rooted  and
%   represented as a list of nodes, where each node has the form:
%
%           node(PathOrPrefix, Action, Options, Children)
%
%   The tree is a potentially complicated structure. It is cached in
%   a global variable. Note that this   cache is per-thread, so each
%   worker thread holds a copy of  the   tree.  If handler facts are
%   changed the _generation_ is  incremented using next_generation/0
%   and each worker thread will  re-compute   the  tree  on the next
%   ocasion.

path_tree(Tree) :-
    current_generation(G),
    nb_current(http_dispatch_tree, G-Tree),
    !. % Avoid existence error
path_tree(Tree) :-
    path_tree_nocache(Tree),
    current_generation(G),
    nb_setval(http_dispatch_tree, G-Tree).

path_tree_nocache(Tree) :-
    findall(Prefix, prefix_handler(Prefix, _, _, _), Prefixes0),
    sort(Prefixes0, Prefixes),
    prefix_tree(Prefixes, [], PTree),
    prefix_options(PTree, [], OPTree),
    add_paths_tree(OPTree, Tree).

prefix_handler(Prefix, Action, Options, Priority-PLen) :-
    handler(Spec, Action, true, Options),
    (   memberchk(priority(Priority), Options)
    ->  true
    ;   Priority = 0
    ),
    (   memberchk(segment_pattern(Pattern), Options)
    ->  length(Pattern, PLen)
    ;   PLen = 0
    ),
    Error = error(existence_error(http_alias,_),_),
    catch(http_absolute_location(Spec, Prefix, []), Error,
          (   print_message(warning, Error),
              fail
          )).

%!  prefix_tree(PrefixList, +Tree0, -Tree)
%
%   @param Tree     list(Prefix-list(Children))

prefix_tree([], Tree, Tree).
prefix_tree([H|T], Tree0, Tree) :-
    insert_prefix(H, Tree0, Tree1),
    prefix_tree(T, Tree1, Tree).

insert_prefix(Prefix, Tree0, Tree) :-
    select(P-T, Tree0, Tree1),
    sub_atom(Prefix, 0, _, _, P),
    !,
    insert_prefix(Prefix, T, T1),
    Tree = [P-T1|Tree1].
insert_prefix(Prefix, Tree, [Prefix-[]|Tree]).


%!  prefix_options(+PrefixTree, +DefOptions, -OptionTree)
%
%   Generate the option-tree for all prefix declarations.
%
%   @tbd    What to do if there are more?

prefix_options([], _, []).
prefix_options([Prefix-C|T0], DefOptions,
               [node(prefix(Prefix), Action, PrefixOptions, Children)|T]) :-
    findall(h(A,O,P), prefix_handler(Prefix,A,O,P), Handlers),
    sort(3, >=, Handlers, Handlers1),
    Handlers1 = [h(_,_,P0)|_],
    same_priority_handlers(Handlers1, P0, Same),
    option_patterns(Same, SegmentPatterns, Action),
    last(Same, h(_, Options0, _-_)),
    merge_options(Options0, DefOptions, Options),
    append(SegmentPatterns, Options, PrefixOptions),
    exclude(no_inherit, Options, InheritOpts),
    prefix_options(C, InheritOpts, Children),
    prefix_options(T0, DefOptions, T).

no_inherit(id(_)).
no_inherit('$extract'(_)).

same_priority_handlers([H|T0], P, [H|T]) :-
    H = h(_,_,P0-_),
    P = P0-_,
    !,
    same_priority_handlers(T0, P, T).
same_priority_handlers(_, _, []).

option_patterns([], [], nop).
option_patterns([h(A,_,_-0)|_], [], A) :-
    !.
option_patterns([h(A,O,_)|T0], [segment_pattern(P,A,O)|T], AF) :-
    memberchk(segment_pattern(P), O),
    option_patterns(T0, T, AF).


%!  add_paths_tree(+OPTree, -Tree) is det.
%
%   Add the plain paths.

add_paths_tree(OPTree, Tree) :-
    findall(path(Path, Action, Options),
            plain_path(Path, Action, Options),
            Triples),
    add_paths_tree(Triples, OPTree, Tree).

add_paths_tree([], Tree, Tree).
add_paths_tree([path(Path, Action, Options)|T], Tree0, Tree) :-
    add_path_tree(Path, Action, Options, [], Tree0, Tree1),
    add_paths_tree(T, Tree1, Tree).


%!  plain_path(-Path, -Action, -Options) is nondet.
%
%   True if {Path,Action,Options} is registered and  Path is a plain
%   (i.e. not _prefix_) location.

plain_path(Path, Action, Options) :-
    handler(Spec, Action, false, Options),
    catch(http_absolute_location(Spec, Path, []), E,
          (print_message(error, E), fail)).


%!  add_path_tree(+Path, +Action, +Options, +Tree0, -Tree) is det.
%
%   Add a path to a tree. If a  handler for the same path is already
%   defined, the one with the highest   priority or the latest takes
%   precedence.

add_path_tree(Path, Action, Options0, DefOptions, [],
              [node(Path, Action, Options, [])]) :-
    !,
    merge_options(Options0, DefOptions, Options).
add_path_tree(Path, Action, Options, _,
              [node(prefix(Prefix), PA, DefOptions, Children0)|RestTree],
              [node(prefix(Prefix), PA, DefOptions, Children)|RestTree]) :-
    sub_atom(Path, 0, _, _, Prefix),
    !,
    delete(DefOptions, id(_), InheritOpts),
    add_path_tree(Path, Action, Options, InheritOpts, Children0, Children).
add_path_tree(Path, Action, Options1, DefOptions, [H0|T], [H|T]) :-
    H0 = node(Path, _, Options2, _),
    option(priority(P1), Options1, 0),
    option(priority(P2), Options2, 0),
    P1 >= P2,
    !,
    merge_options(Options1, DefOptions, Options),
    H = node(Path, Action, Options, []).
add_path_tree(Path, Action, Options, DefOptions, [H|T0], [H|T]) :-
    add_path_tree(Path, Action, Options, DefOptions, T0, T).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(http_dispatch(ambiguous_id(ID, _List, Selected))) -->
    [ 'HTTP dispatch: ambiguous handler ID ~q (selected ~q)'-[ID, Selected]
    ].


                 /*******************************
                 *            XREF              *
                 *******************************/

:- multifile
    prolog:meta_goal/2.
:- dynamic
    prolog:meta_goal/2.

prolog:meta_goal(http_handler(_, G, _), [G+1]).
prolog:meta_goal(http_current_handler(_, G), [G+1]).


                 /*******************************
                 *             EDIT             *
                 *******************************/

% Allow edit(Location) to edit the implementation for an HTTP location.

:- multifile
    prolog_edit:locate/3.

prolog_edit:locate(Path, Spec, Location) :-
    atom(Path),
    sub_atom(Path, 0, _, _, /),
    Pred = _M:_H,
    catch(http_current_handler(Path, Pred), _, fail),
    closure_name_arity(Pred, 1, PI),
    prolog_edit:locate(PI, Spec, Location).

closure_name_arity(M:Term, Extra, M:Name/Arity) :-
    !,
    callable(Term),
    functor(Term, Name, Arity0),
    Arity is Arity0 + Extra.
closure_name_arity(Term, Extra, Name/Arity) :-
    callable(Term),
    functor(Term, Name, Arity0),
    Arity is Arity0 + Extra.


                 /*******************************
                 *        CACHE CLEANUP         *
                 *******************************/

:- listen(settings(changed(http:prefix, _, _)),
          next_generation).

:- multifile
    user:message_hook/3.
:- dynamic
    user:message_hook/3.

user:message_hook(make(done(Reload)), _Level, _Lines) :-
    Reload \== [],
    next_generation,
    fail.
