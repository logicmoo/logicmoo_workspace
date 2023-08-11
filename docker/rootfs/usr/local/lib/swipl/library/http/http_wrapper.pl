/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2017, University of Amsterdam
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

:- module(httpd_wrapper,
          [ http_wrapper/5,             % :Goal, +In, +Out, -Conn, +Options
            http_current_request/1,     % -Request
            http_peer/2,                % +Request, -PeerIP
            http_send_header/1,         % +Term
            http_relative_path/2,       % +AbsPath, -RelPath
                                        % Internal API
            http_wrap_spawned/3,        % :Goal, -Request, -Connection
            http_spawned/1              % +ThreadId
          ]).
:- use_module(http_header).
:- use_module(http_stream).
:- use_module(http_exception).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(broadcast)).

:- meta_predicate
    http_wrapper(0, +, +, -, +).
:- multifile
    http:request_expansion/2.

/** <module> Server processing of an HTTP request

Most   code   doesn't   need  to   use  this   directly;  instead   use
library(http/http_server),  which  combines   this  library  with   the
typical HTTP libraries that most servers need.

This library provides  the  core  of   the  implementation  of  the HTTP
protocol at the server side and is   mainly intended for *internal use*.
It   is   used   by    library(thread_httpd)   and   library(inet_httpd)
(deprecated).

Still, it provides a few  predicates   that  are  occasinally useful for
applications:

  - http_current_request/1 finds the current request for occasional
    usage in places where it is not avaialable otherwise.
  - http_peer/2 finds the (IP4) peer address, getting the original
    address if we are behind a proxy (=X-Forwarded-For=)
  - http_relative_path/2 can be used to find a relative path from
    the current request.
*/

%!  http_wrapper(:Goal, +In, +Out, -Close, +Options) is det.
%
%   Simple wrapper to read and decode an HTTP header from `In', call
%   :Goal while watching for exceptions and send the result to the
%   stream `Out'.
%
%   The goal is assumed  to  write   the  reply  to =current_output=
%   preceeded by an HTTP header, closed by  a blank line. The header
%   *must* contain a Content-type: <type>   line.  It may optionally
%   contain a line =|Transfer-encoding: chunked|= to request chunked
%   encoding.
%
%   Options:
%
%           * request(-Request)
%           Return the full request to the caller
%           * peer(+Peer)
%           IP address of client
%
%   @param Close    Unified to one of =close=, =|Keep-Alive|= or
%                   spawned(ThreadId).

http_wrapper(Goal, In, Out, Close, Options) :-
    status(Id, State0),
    catch(http_read_request(In, Request0), ReqError, true),
    (   Request0 == end_of_file
    ->  Close = close,
        extend_request(Options, [], _) % return request
    ;   var(ReqError)
    ->  extend_request(Options, Request0, Request1),
        cgi_open(Out, CGI, cgi_hook, [request(Request1)]),
        cgi_property(CGI, id(Id)),
        (   debugging(http(request))
        ->  memberchk(method(Method), Request1),
            memberchk(path(Location), Request1),
            debug(http(request), "[~D] ~w ~w ...", [Id,Method,Location])
        ;   true
        ),
        handler_with_output_to(Goal, Id, Request1, CGI, Error),
        cgi_close(CGI, Request1, State0, Error, Close)
    ;   Id = 0,
        add_header_context(ReqError),
        (   debugging(http(request))
        ->  print_message(warning, ReqError)
        ;   true
        ),
        send_error(Out, [], State0, ReqError, Close),
        extend_request(Options, [], _)
    ).

add_header_context(error(_,context(_,in_http_request))) :- !.
add_header_context(_).

status(Id, state0(Thread, CPU, Id)) :-
    thread_self(Thread),
    thread_cputime(CPU).


%!  http_wrap_spawned(:Goal, -Request, -Close) is det.
%
%   Internal  use  only.  Helper  for    wrapping  the  handler  for
%   http_spawn/2.
%
%   @see http_spawned/1, http_spawn/2.

http_wrap_spawned(Goal, Request, Close) :-
    current_output(CGI),
    cgi_property(CGI, id(Id)),
    handler_with_output_to(Goal, Id, -, current_output, Error),
    (   retract(spawned(ThreadId))
    ->  Close = spawned(ThreadId),
        Request = []
    ;   cgi_property(CGI, request(Request)),
        status(Id, State0),
        catch(cgi_close(CGI, Request, State0, Error, Close),
              _,
              Close = close)
    ).


:- thread_local
    spawned/1.

%!  http_spawned(+ThreadId)
%
%   Internal use only. Indicate that the request is handed to thread
%   ThreadId.

http_spawned(ThreadId) :-
    assert(spawned(ThreadId)).


%!  cgi_close(+CGI, +Request, +State0, +Error, -Close) is det.
%
%   The wrapper has completed. Finish the  CGI output. We have three
%   cases:
%
%       * The wrapper delegated the request to a new thread
%       * The wrapper succeeded
%       * The wrapper threw an error, non-200 status reply
%       (e.g., =not_modified=, =moved=) or a request to reply with
%       the content of a file.
%
%   @error socket I/O errors.

cgi_close(_, _, _, _, Close) :-
    retract(spawned(ThreadId)),
    !,
    Close = spawned(ThreadId).
cgi_close(CGI, _, State0, ok, Close) :-
    !,
    catch(cgi_finish(CGI, Status, Close, Bytes), E, true),
    (   var(E)
    ->  http_done(Status, ok, Bytes, State0)
    ;   http_done(500, E, 0, State0),       % TBD: amount written?
        throw(E)
    ).
cgi_close(CGI, Request, Id, http_reply(Status), Close) :-
    !,
    cgi_close(CGI, Request, Id, http_reply(Status, []), Close).
cgi_close(CGI, Request, Id, http_reply(Status, ExtraHdrOpts), Close) :-
    cgi_property(CGI, header_codes(Text)),
    Text \== [],
    !,
    http_parse_header(Text, ExtraHdrCGI),
    cgi_property(CGI, client(Out)),
    cgi_discard(CGI),
    close(CGI),
    append(ExtraHdrCGI, ExtraHdrOpts, ExtraHdr),
    send_error(Out, Request, Id, http_reply(Status, ExtraHdr), Close).
cgi_close(CGI, Request, Id, Error, Close) :-
    cgi_property(CGI, client(Out)),
    cgi_discard(CGI),
    close(CGI),
    send_error(Out, Request, Id, Error, Close).

cgi_finish(CGI, Status, Close, Bytes) :-
    flush_output(CGI),                      % update the content-length
    cgi_property(CGI, connection(Close)),
    cgi_property(CGI, content_length(Bytes)),
    (   cgi_property(CGI, header(Header)),
        memberchk(status(Status), Header)
    ->  true
    ;   Status = 200
    ),
    close(CGI).

%!  send_error(+Out, +Request, +State0, +Error, -Close)
%
%   Send status replies and  reply   files.  The =current_output= no
%   longer points to the CGI stream, but   simply to the socket that
%   connects us to the client.
%
%   @param  State0 is start-status as returned by status/1.  Used to
%           find CPU usage, etc.

send_error(Out, Request, State0, Error, Close) :-
    map_exception_to_http_status(Error, Reply, HdrExtra0, Context),
    update_keep_alive(HdrExtra0, HdrExtra, Request),
    catch(http_reply(Reply,
                     Out,
                     [ content_length(CLen)
                     | HdrExtra
                     ],
                     Context,
                     Request,
                     Code),
          E, true),
    (   var(E)
    ->  http_done(Code, Error, CLen, State0)
    ;   http_done(500,  E, 0, State0),
        throw(E)                    % is that wise?
    ),
    (   Error = http_reply(switching_protocols(Goal, SwitchOptions), _)
    ->  Close = switch_protocol(Goal, SwitchOptions)
    ;   memberchk(connection(Close), HdrExtra)
    ->  true
    ;   Close = close
    ).

update_keep_alive(Header0, Header, Request) :-
    memberchk(connection(C), Header0),
    !,
    (   C == close
    ->  Header = Header0
    ;   client_wants_close(Request)
    ->  selectchk(connection(C),     Header0,
                  connection(close), Header)
    ;   Header = Header0
    ).
update_keep_alive(Header, Header, _).

client_wants_close(Request) :-
    memberchk(connection(C), Request),
    !,
    C == close.
client_wants_close(Request) :-
    \+ ( memberchk(http_version(Major-_Minor), Request),
         Major >= 1
       ).


%!  http_done(+Code, +Status, +BytesSent, +State0) is det.
%
%   Provide feedback for logging and debugging   on  how the request
%   has been completed.

http_done(Code, Status, Bytes, state0(_Thread, CPU0, Id)) :-
    thread_cputime(CPU1),
    CPU is CPU1 - CPU0,
    (   debugging(http(request))
    ->  debug_request(Code, Status, Id, CPU, Bytes)
    ;   true
    ),
    broadcast(http(request_finished(Id, Code, Status, CPU, Bytes))).


%!  handler_with_output_to(:Goal, +Id, +Request, +Output, -Status) is det.
%
%   Run Goal with output redirected to   Output. Unifies Status with
%   =ok=, the error from catch/3  or a term error(goal_failed(Goal),
%   _).
%
%   @param Request  The HTTP request read or '-' for a continuation
%                   using http_spawn/2.

handler_with_output_to(Goal, Id, Request, current_output, Status) :-
    !,
    (   catch(call_handler(Goal, Id, Request), Status, true)
    ->  (   var(Status)
        ->  Status = ok
        ;   true
        )
    ;   Status = error(goal_failed(Goal),_)
    ).
handler_with_output_to(Goal, Id, Request, Output, Error) :-
    stream_property(OldOut, alias(current_output)),
    set_output(Output),
    handler_with_output_to(Goal, Id, Request, current_output, Error),
    set_output(OldOut).

call_handler(Goal, _, -) :-            % continuation through http_spawn/2
    !,
    call(Goal).
call_handler(Goal, Id, Request0) :-
    expand_request(Request0, Request),
    current_output(CGI),
    cgi_set(CGI, request(Request)),
    broadcast(http(request_start(Id, Request))),
    call(Goal, Request).

%!  thread_cputime(-CPU) is det.
%
%   CPU is the CPU time used by the calling thread.

thread_cputime(CPU) :-
    statistics(cputime, CPU).

%!  cgi_hook(+Event, +CGI) is det.
%
%   Hook called from the CGI   processing stream. See http_stream.pl
%   for details.

:- public cgi_hook/2.

cgi_hook(What, _CGI) :-
    debug(http(hook), 'Running hook: ~q', [What]),
    fail.
cgi_hook(header, CGI) :-
    cgi_property(CGI, header_codes(HeadText)),
    cgi_property(CGI, header(Header0)), % see http_send_header/1
    http_parse_header(HeadText, CgiHeader0),
    append(Header0, CgiHeader0, CgiHeader),
    cgi_property(CGI, request(Request)),
    http_update_connection(CgiHeader, Request, Connection, Header1),
    http_update_transfer(Request, Header1, Transfer, Header2),
    http_update_encoding(Header2, Encoding, Header),
    set_stream(CGI, encoding(Encoding)),
    cgi_set(CGI, connection(Connection)),
    cgi_set(CGI, header(Header)),
    debug(http(transfer_encoding), 'Transfer-encoding: ~w', [Transfer]),
    cgi_set(CGI, transfer_encoding(Transfer)). % must be LAST
cgi_hook(send_header, CGI) :-
    cgi_property(CGI, header(Header)),
    debug(http(cgi), 'Header: ~q', [Header]),
    cgi_property(CGI, client(Out)),
    (   redirect(Header, Action, RedirectHeader)
    ->  http_status_reply(Action, Out, RedirectHeader, _),
        cgi_discard(CGI)
    ;   cgi_property(CGI, transfer_encoding(chunked))
    ->  http_reply_header(Out, chunked_data, Header)
    ;   cgi_property(CGI, content_length(Len))
    ->  http_reply_header(Out, cgi_data(Len), Header)
    ).
cgi_hook(close, _).

%!  redirect(+Header, -Action, -RestHeader) is semidet.
%
%   Detect the CGI =Location=  and   optional  =Status=  headers for
%   formulating a HTTP redirect.  Redirection is only established if
%   no =Status= is provided, or =Status= is 3XX.

redirect(Header, Action, RestHeader) :-
    selectchk(location(To), Header, Header1),
    (   selectchk(status(Status), Header1, RestHeader)
    ->  between(300, 399, Status)
    ;   RestHeader = Header1,
        Status = 302
    ),
    redirect_action(Status, To, Action).

redirect_action(301, To, moved(To)).
redirect_action(302, To, moved_temporary(To)).
redirect_action(303, To, see_other(To)).


%!  http_send_header(+Header)
%
%   This API provides an alternative for writing the header field as
%   a CGI header. Header has the  format Name(Value), as produced by
%   http_read_header/2.
%
%   @deprecated     Use CGI lines instead

http_send_header(Header) :-
    current_output(CGI),
    cgi_property(CGI, header(Header0)),
    cgi_set(CGI, header([Header|Header0])).


%!  expand_request(+Request0, -Request)
%
%   Allow  for  general   rewrites   of    a   request   by  calling
%   http:request_expansion/2.

expand_request(R0, R) :-
    http:request_expansion(R0, R1),         % Hook
    R1 \== R0,
    !,
    expand_request(R1, R).
expand_request(R, R).


%!  extend_request(+Options, +RequestIn, -Request)
%
%   Merge options in the request.

extend_request([], R, R).
extend_request([request(R)|T], R0, R) :-
    !,
    extend_request(T, R0, R).
extend_request([H|T], R0, R) :-
    request_option(H),
    !,
    extend_request(T, [H|R0], R).
extend_request([_|T], R0, R) :-
    extend_request(T, R0, R).

request_option(peer(_)).
request_option(protocol(_)).
request_option(pool(_)).


%!  http_current_request(-Request) is semidet.
%
%   Returns  the  HTTP  request  currently  being  processed.  Fails
%   silently if there is no current  request. This typically happens
%   if a goal is run outside the HTTP server context.

http_current_request(Request) :-
    current_output(CGI),
    is_cgi_stream(CGI),
    cgi_property(CGI, request(Request)).


%!  http_peer(+Request, -PeerIP:atom) is semidet.
%
%   True when PeerIP is the IP address   of  the connection peer. If the
%   connection is established via a proxy  or   CDN  we  try to find the
%   initiating peer.  Currently supports:
%
%     - =Fastly-client-ip=
%     - =X-real-ip=
%     - =X-forwarded-for=
%     - Direct connections
%
%   @bug The =X-forwarded-for=  header  is   problematic.  According  to
%   [Wikipedia](https://en.wikipedia.org/wiki/X-Forwarded-For),      the
%   original   client   is   the    _first_,     while    according   to
%   [AWS](http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/x-forwarded-headers.html)
%   it is the _last_.

http_peer(Request, Peer) :-
    memberchk(fastly_client_ip(Peer), Request), !.
http_peer(Request, Peer) :-
    memberchk(x_real_ip(Peer), Request), !.
http_peer(Request, IP) :-
    memberchk(x_forwarded_for(IP0), Request),
    !,
    atomic_list_concat(Parts, ', ', IP0),
    last(Parts, IP).
http_peer(Request, IP) :-
    memberchk(peer(Peer), Request),
    !,
    peer_to_ip(Peer, IP).

peer_to_ip(ip(A,B,C,D), IP) :-
    atomic_list_concat([A,B,C,D], '.', IP).


%!  http_relative_path(+AbsPath, -RelPath) is det.
%
%   Convert an absolute path (without host, fragment or search) into
%   a path relative to the current page.   This  call is intended to
%   create reusable components returning relative   paths for easier
%   support of reverse proxies.

http_relative_path(Path, RelPath) :-
    http_current_request(Request),
    memberchk(path(RelTo), Request),
    http_relative_path(Path, RelTo, RelPath),
    !.
http_relative_path(Path, Path).

http_relative_path(Path, RelTo, RelPath) :-
    atomic_list_concat(PL, /, Path),
    atomic_list_concat(RL, /, RelTo),
    delete_common_prefix(PL, RL, PL1, PL2),
    to_dot_dot(PL2, DotDot, PL1),
    atomic_list_concat(DotDot, /, RelPath).

delete_common_prefix([H|T01], [H|T02], T1, T2) :-
    !,
    delete_common_prefix(T01, T02, T1, T2).
delete_common_prefix(T1, T2, T1, T2).

to_dot_dot([], Tail, Tail).
to_dot_dot([_], Tail, Tail) :- !.
to_dot_dot([_|T0], ['..'|T], Tail) :-
    to_dot_dot(T0, T, Tail).


                 /*******************************
                 *         DEBUG SUPPORT        *
                 *******************************/

%!  debug_request(+Code, +Status, +Id, +CPU0, Bytes)
%
%   Emit debugging info after a request completed with Status.

debug_request(Code, ok, Id, CPU, Bytes) :-
    !,
    debug(http(request), '[~D] ~w OK (~3f seconds; ~D bytes)',
          [Id, Code, CPU, Bytes]).
debug_request(Code, Status, Id, _, Bytes) :-
    map_exception(Status, Reply),
    !,
    debug(http(request), '[~D] ~w ~w; ~D bytes',
          [Id, Code, Reply, Bytes]).
debug_request(Code, Except, Id, _, _) :-
    Except = error(_,_),
    !,
    message_to_string(Except, Message),
    debug(http(request), '[~D] ~w ERROR: ~w',
          [Id, Code, Message]).
debug_request(Code, Status, Id, _, Bytes) :-
    debug(http(request), '[~D] ~w ~w; ~D bytes',
          [Id, Code, Status, Bytes]).

map_exception(http_reply(Reply), Reply).
map_exception(http_reply(Reply, _), Reply).
map_exception(error(existence_error(http_location, Location), _Stack),
              error(404, Location)).
