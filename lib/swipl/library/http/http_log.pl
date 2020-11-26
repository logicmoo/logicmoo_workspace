/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2018, University of Amsterdam
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

:- module(http_log,
          [ http_log_stream/1,          % -Stream
            http_log/2,                 % +Format, +Args
            http_log_close/1,           % +Reason
            post_data_encoded/2,        % ?Bytes, ?Encoded
            http_logrotate/1,           % +Options
            http_schedule_logrotate/2   % +When, +Options
          ]).
:- use_module(library(http/http_header)).
:- use_module(library(settings)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(broadcast)).

:- setting(http:logfile, callable, 'httpd.log',
           'File in which to log HTTP requests').
:- setting(http:log_post_data, integer, 0,
           'Log POST data up to N bytes long').
:- setting(http:on_log_error, any, retry,
           'Action if logging fails').

/** <module> HTTP Logging module

Simple module for logging HTTP requests to a file. Logging is enabled by
loading this file and ensure the setting   http:logfile is not the empty
atom. The default  file  for  writing   the  log  is  =|httpd.log|=. See
library(settings) for details.

The  level of  logging  can be modified  using  the  multifile predicate
http_log:nolog/1 to hide HTTP  request  fields   from  the  logfile  and
http_log:password_field/1   to   hide   passwords   from   HTTP   search
specifications (e.g. =|/topsecret?password=secret|=).
*/

:- multifile
    nolog/1,
    password_field/1,
    nolog_post_content_type/1.

% If the log settings change,  simply  close   the  log  and  it will be
% reopened with the new settings.

:- listen(settings(changed(http:logfile, _, New)),
          http_log_close(changed(New))).
:- listen(http(Message),
          http_message(Message)).
:- listen(logrotate,
          http_log_close(logrotate)).

http_message(Message) :-
    log_message(Message),
    http_log_stream(Stream),
    catch(http_message(Message, Stream), E,
          log_error(E)).

log_message(request_start(_Id, _Request)).
log_message(request_finished(_Id, _Code, _Status, _CPU, _Bytes)).

http_message(request_start(Id, Request), Stream) :-
    log_started(Request, Id, Stream).
http_message(request_finished(Id, Code, Status, CPU, Bytes), Stream) :-
    log_completed(Code, Status, Bytes, Id, CPU, Stream).

%!  log_error(+Error)
%
%   There was an error writing the  log   file.  The  message is printed
%   using print_message/2 and  execution  continues   according  to  the
%   setting `http:on_log_error`, which is one of:
%
%     - retry
%     Close the log file. The system will try to reopen it on the
%     next log event, recovering from the error. Note that the
%     most common case for this is probably running out of disc space.
%     - exit
%     - exit(Code)
%     Stop the server using halt(Code). The `exit` variant is equivalent
%     to exit(1).
%
%   The best choice depends on  your   priorities.  Using  `retry` gives
%   priority to keep the server running.  Using `exit` guarantees proper
%   log files and  thus  the  ability   to  examine  these  for security
%   reasons. An attacker may try to flood the disc, causing a successful
%   DoS attack if `exit` is used  and   the  ability to interact without
%   being logged if `retry` is used.

log_error(E) :-
    print_message(warning, E),
    log_error_continue.

log_error_continue :-
    setting(http:on_log_error, Action),
    log_error_continue(Action).

log_error_continue(retry) :-
    http_log_close(error).
log_error_continue(exit) :-
    log_error_continue(exit(1)).
log_error_continue(exit(Code)) :-
    halt(Code).



                 /*******************************
                 *         LOG ACTIVITY         *
                 *******************************/

:- dynamic
    log_stream/2.                   % Stream, TimeTried

%!  http_log_stream(-Stream) is semidet.
%
%   True when Stream is a stream to the  opened HTTP log file. Opens the
%   log file in =append= mode if the file  is not yet open. The log file
%   is determined from the setting =|http:logfile|=.  If this setting is
%   set to the empty atom (''), this predicate fails.
%
%   If  a  file  error  is   encountered,    this   is   reported  using
%   print_message/2, after which this predicate  silently fails. Opening
%   is retried every minute when a new message arrives.
%
%   Before opening the log  file,   the  message  http_log_open(Term) is
%   broadcasted.  This  message  allows  for   creating  the  directory,
%   renaming, deleting or truncating an existing log file.

http_log_stream(Stream) :-
    log_stream(Stream, _Opened),
    !,
    Stream \== [].
http_log_stream([]) :-
    setting(http:logfile, ''),
    !,
    get_time(Now),
    assert(log_stream([], Now)).
http_log_stream(Stream) :-
    setting(http:logfile, Term),
    broadcast(http_log_open(Term)),
    catch(absolute_file_name(Term, File,
                             [ access(append)
                             ]),
          E, open_error(E)),
    with_mutex(http_log, open_log(File, Stream0)),
    Stream = Stream0.

open_log(_File, Stream) :-
    log_stream(Stream, Opened),
    (   Stream == []
    ->  (   get_time(Now),
            Now - Opened > 60
        ->  retractall(log_stream(_,_)),
            fail
        ;   !, fail
        )
    ;   true
    ), !.
open_log(File, Stream) :-
    catch(open(File, append, Stream,
               [ close_on_abort(false),
                 encoding(utf8),
                 buffer(line)
               ]), E, open_error(E)),
    get_time(Time),
    format(Stream,
           'server(started, ~0f).~n',
           [ Time ]),
    assert(log_stream(Stream, Time)),
    at_halt(close_log(stopped)).

open_error(E) :-
    print_message(error, E),
    log_open_error_continue.

log_open_error_continue :-
    setting(http:on_log_error, Action),
    log_open_error_continue(Action).

log_open_error_continue(retry) :-
    !,
    get_time(Now),
    assert(log_stream([], Now)),
    fail.
log_open_error_continue(Action) :-
    log_error_continue(Action).


%!  http_log_close(+Reason) is det.
%
%   If there is a currently open HTTP logfile, close it after adding
%   a term server(Reason, Time).  to  the   logfile.  This  call  is
%   intended for cooperation with the Unix logrotate facility
%   using the following schema:
%
%       * Move logfile (the HTTP server keeps writing to the moved
%       file)
%       * Inform the server using an HTTP request that calls
%       http_log_close/1
%       * Compress the moved logfile
%
%   @author Suggested by Jacco van Ossenbruggen

http_log_close(Reason) :-
    with_mutex(http_log, close_log(Reason)).

close_log(Reason) :-
    retract(log_stream(Stream, _Opened)),
    !,
    (   is_stream(Stream)
    ->  get_time(Time),
        catch(( format(Stream, 'server(~q, ~0f).~n', [ Reason, Time ]),
                close(Stream)
              ), E, print_message(warning, E))
    ;   true
    ).
close_log(_).

%!  http_log(+Format, +Args) is det.
%
%   Write message from Format and Args   to log-stream. See format/2
%   for details. Succeed without side  effects   if  logging  is not
%   enabled.

http_log(Format, Args) :-
    (   http_log_stream(Stream)
    ->  system:format(Stream, Format, Args) % use operators from `system`
    ;   true
    ).


%!  log_started(+Request, +Id, +Stream) is det.
%
%   Write log message that Request was started to Stream.
%
%   @param  Filled with sequence identifier for the request

log_started(Request, Id, Stream) :-
    get_time(Now),
    add_post_data(Request, Request1),
    log_request(Request1, LogRequest),
    format_time(string(HDate), '%+', Now),
    format(Stream,
           '/*~s*/ request(~q, ~3f, ~q).~n',
           [HDate, Id, Now, LogRequest]).

%!  log_request(+Request, -Log)
%
%   Remove passwords from the request to avoid sending them to the
%   logfiles.

log_request([], []).
log_request([search(Search0)|T0], [search(Search)|T]) :-
    !,
    mask_passwords(Search0, Search),
    log_request(T0, T).
log_request([H|T0], T) :-
    nolog(H),
    !,
    log_request(T0, T).
log_request([H|T0], [H|T]) :-
    log_request(T0, T).

mask_passwords([], []).
mask_passwords([Name=_|T0], [Name=xxx|T]) :-
    password_field(Name),
    !,
    mask_passwords(T0, T).
mask_passwords([H|T0], [H|T]) :-
    mask_passwords(T0, T).

%!  password_field(+Field) is semidet.
%
%   Multifile predicate that can be defined to hide passwords from
%   the logfile.

password_field(password).
password_field(pwd0).
password_field(pwd1).
password_field(pwd2).


%!  nolog(+HTTPField)
%
%   Multifile  predicate  that  can  be   defined  to  hide  request
%   parameters from the request logfile.

nolog(input(_)).
nolog(accept(_)).
nolog(accept_language(_)).
nolog(accept_encoding(_)).
nolog(accept_charset(_)).
nolog(pool(_)).
nolog(protocol(_)).
nolog(referer(R)) :-
    sub_atom(R, _, _, _, password),
    !.

%!  nolog_post_content_type(+Type) is semidet.
%
%   Multifile hook called with the   =|Content-type|= header. If the
%   hook succeeds, the POST data is not logged. For example, to stop
%   logging  anything  but  application/json   messages:
%
%     ==
%     :- multifile http_log:nolog_post_content_type/1.
%
%     http_log:nolog_post_content_type(Type) :-
%        Type \= (application/json).
%     ==
%
%   @arg Type is a term MainType/SubType

%!  add_post_data(+Request0, -Request) is det.
%
%   Add   a   request   field   post_data(Data)   if   the   setting
%   http:log_post_data is an integer > 0,  the content length < this
%   setting and nolog_post_content_type/1 does not   succeed  on the
%   provided content type.

add_post_data(Request0, Request) :-
    setting(http:log_post_data, MaxLen),
    integer(MaxLen), MaxLen > 0,
    memberchk(input(In), Request0),
    memberchk(content_length(CLen), Request0),
    CLen =< MaxLen,
    memberchk(content_type(Type), Request0),
    http_parse_header_value(content_type, Type, media(MType/MSubType, _)),
    \+ nolog_post_content_type(MType/MSubType),
    catch(peek_string(In, CLen, PostData), _, fail),
    !,
    post_data_encoded(PostData, Encoded),
    Request = [post_data(Encoded)|Request0].
add_post_data(Request, Request).

%!  post_data_encoded(?Bytes:string, ?Encoded:string) is det.
%
%   Encode the POST body for inclusion into   the HTTP log file. The
%   POST data is (in/de)flated  using   zopen/3  and  base64 encoded
%   using base64//1. The encoding makes   long text messages shorter
%   and keeps readable logfiles if binary data is posted.

post_data_encoded(Bytes, Hex) :-
    nonvar(Bytes),
    !,
    setup_call_cleanup(
        new_memory_file(HMem),
        ( setup_call_cleanup(
              ( open_memory_file(HMem, write, Out, [encoding(octet)]),
                zopen(Out, ZOut, [])
              ),
              format(ZOut, '~s', [Bytes]),
              close(ZOut)),
          memory_file_to_codes(HMem, Codes, octet)
        ),
        free_memory_file(HMem)),
    phrase(base64(Codes), EncCodes),
    string_codes(Hex, EncCodes).
post_data_encoded(Bytes, Hex) :-
    string_codes(Hex, EncCodes),
    phrase(base64(Codes), EncCodes),
    string_codes(ZBytes, Codes),
    setup_call_cleanup(
        open_string(ZBytes, In),
        zopen(In, Zin, []),
        read_string(Zin, _, Bytes)).

%!  log_completed(+Code, +Status, +Bytes, +Id, +CPU, +Stream) is det.
%
%   Write log message to Stream from a call_cleanup/3 call.
%
%   @param Status   2nd argument of call_cleanup/3
%   @param Id       Term identifying the completed request
%   @param CPU0     CPU time at time of entrance
%   @param Stream   Stream to write to (normally from http_log_stream/1).

log_completed(Code, Status, Bytes, Id, CPU, Stream) :-
    is_stream(Stream),
    log_check_deleted(Stream),
    !,
    log(Code, Status, Bytes, Id, CPU, Stream).
log_completed(Code, Status, Bytes, Id, CPU0, _) :-
    http_log_stream(Stream),       % Logfile has changed!
    !,
    log_completed(Code, Status, Bytes, Id, CPU0, Stream).
log_completed(_,_,_,_,_,_).


log(Code, ok, Bytes, Id, CPU, Stream) :-
    !,
    format(Stream, 'completed(~q, ~q, ~q, ~q, ok).~n',
           [ Id, CPU, Bytes, Code ]).
log(Code, Status, Bytes, Id, CPU, Stream) :-
    (   map_exception(Status, Term)
    ->  true
    ;   message_to_string(Status, String),
        Term = error(String)
    ),
    format(Stream, 'completed(~q, ~q, ~q, ~q, ~W).~n',
           [ Id, CPU, Bytes, Code,
             Term, [ quoted(true),
                     ignore_ops(true),
                     blobs(portray),
                     portray_goal(write_blob)
                   ]
           ]).

:- public write_blob/2.
write_blob(Blob, _Options) :-
    format(string(S), '~q', [Blob]),
    writeq(blob(S)).

map_exception(http_reply(bytes(ContentType,Bytes),_), bytes(ContentType,L)) :-
    string_length(Bytes, L).        % also does lists
map_exception(http_reply(Reply), Reply).
map_exception(http_reply(Reply, _), Reply).
map_exception(error(existence_error(http_location, Location), _Stack),
              error(404, Location)).


                 /*******************************
                 *      LOGROTATE SUPPORT       *
                 *******************************/

%!  log_check_deleted(+Stream) is semidet.
%
%   If the link-count of the stream has   dropped  to zero, the file
%   has been deleted/moved. In this case the  log file is closed and
%   log_check_deleted/6 will open a  new   one.  This  provides some
%   support for cleaning up the logfile   without  shutting down the
%   server.
%
%   @see logrotate(1) to manage logfiles on Unix systems.

log_check_deleted(Stream) :-
    stream_property(Stream, nlink(Links)),
    Links == 0,
    !,
    http_log_close(log_file_deleted),
    fail.
log_check_deleted(_).

%!  http_logrotate(+Options) is det.
%
%   Rotate the available log files. Note that  there are two ways to
%   deal with the rotation of log files:
%
%     1. Use the OS log rotation facility. In that case the OS must
%     (1) move the logfile and (2) have something calling
%     http_log_close/1 to close the (moved) file and make this
%     server create a new one on the next log message.  If
%     library(http/http_unix_daemon) is used, closing is
%     achieved by sending SIGHUP or SIGUSR1 to the process.
%
%     2. Call this predicate at scheduled intervals.  This can
%     be achieved by calling http_schedule_logrotate/2 in the
%     context of library(http/http_unix_daemon) which schedules
%     the maintenance actions.
%
%   Options:
%
%     - min_size(+Bytes)
%     Do not rotate if the log file is smaller than Bytes.
%     The default is 1Mbytes.
%     - keep_logs(+Count)
%     Number of rotated log files to keep (default 10)
%     - compress_logs(+Format)
%     Compress the log files to the given format.
%     - background(+Boolean)
%     If `true`, rotate the log files in the background.

http_logrotate(Options) :-
    select_option(background(true), Options, Options1),
    !,
    thread_create(http_logrotate(Options1), _,
                  [ alias('__logrotate'),
                    detached(true)
                  ]).
http_logrotate(Options) :-
    option(keep_logs(Keep), Options, 10),
    option(compress_logs(Format), Options, gzip),
    compress_extension(Format, ZExt),
    log_file_and_ext(Base, Ext),
    (   log_too_small(Base, Ext, Options)
    ->  true
    ;   rotate_logs(Base, Ext, ZExt, Keep)
    ).

rotate_logs(Base, Ext, ZExt, N1) :-
    N1 > 0,
    !,
    N0 is N1 - 1,
    old_log_file(Base, Ext, N0, ZO, Old),
    (   exists_file(Old)
    ->  new_log_file(Base, Ext, N1, ZO, ZExt, ZN, New),
        rename_log_file(ZO, Old, ZN, New)
    ;   true
    ),
    rotate_logs(Base, Ext, ZExt, N0).
rotate_logs(_, _, _, _).

rename_log_file(ZExt, Old, ZExt, New) :-
    !,
    debug(logrotate, 'Rename ~p --> ~p', [Old, New]),
    rename_file(Old, New).
rename_log_file('', Old, ZExt, New) :-
    file_name_extension(NoExt, ZExt, New),
    debug(logrotate, 'Rename ~p --> ~p', [Old, NoExt]),
    rename_file(Old, NoExt),
    debug(logrotate, 'Closing log file', []),
    http_log_close(logrotate),
    compress_extension(Format, ZExt),
    debug(logrotate, 'Compressing (~w) ~p', [Format, NoExt]),
    compress_file(NoExt, Format).

old_log_file(Base, Ext, N, ZExt, File) :-
    log_file(Base, Ext, N, File0),
    (   compress_extension(_, ZExt),
        file_name_extension(File0, ZExt, File1),
        exists_file(File1)
    ->  File = File1
    ;   ZExt = '',
        File = File0
    ).

new_log_file(Base, Ext, N, '', '', '', File) :-
    !,
    log_file(Base, Ext, N, File).
new_log_file(Base, Ext, N, '', ZExt, ZExt, File) :-
    !,
    log_file(Base, Ext, N, File0),
    file_name_extension(File0, ZExt, File).
new_log_file(Base, Ext, N, ZExt, _, ZExt, File) :-
    log_file(Base, Ext, N, File0),
    file_name_extension(File0, ZExt, File).

log_file(Base, Ext, 0, File) :-
    !,
    file_name_extension(Base, Ext, File).
log_file(Base, Ext, N, File) :-
    atomic_list_concat([Base, -, N], Base1),
    file_name_extension(Base1, Ext, File).

log_file_and_ext(Base, Ext) :-
    setting(http:logfile, Term),
    catch(absolute_file_name(Term, File,
                             [ access(exist)
                             ]), _, fail),
    file_name_extension(Base, Ext, File).

log_too_small(Base, Ext, Options) :-
    DefMin is 1024*1024,
    option(min_size(MinBytes), Options, DefMin),
    file_name_extension(Base, Ext, File),
    (   exists_file(File)
    ->  size_file(File, Bytes),
        Bytes < MinBytes,
        debug(logrotate, '~w has ~D bytes; not rotating', [File, Bytes])
    ;   debug(logrotate, '~w does not exist; not rotating', [File])
    ).

%!  compress_file(+File, +Format)
%
%   Compress a file according  to   Format.  Currently only supports
%   gzip.

compress_file(File, Format) :-
    (   compress_extension(Format, Ext)
    ->  true
    ;   domain_error(compress_format, Format)
    ),
    file_name_extension(File, Ext, ZFile),
    catch(setup_call_cleanup(
              gzopen(ZFile, write, Out, [type(binary)]),
              setup_call_cleanup(
                  open(File, read, In, [type(binary)]),
                  copy_stream_data(In, Out),
                  close(In)),
              close(Out)),
          Error,
          ( print_message(error, Error),
            catch(delete_file(Out), _, true),
            throw(Error)
          )),
    delete_file(File).

compress_extension(gzip, gz).

:- dynamic
    scheduled_logrotate/2.  % Schedule, Options

%!  http_schedule_logrotate(When, Options)
%
%   Schedule log rotation based on maintenance broadcasts.  When
%   is one of:
%
%     - daily(Hour:Min)
%     Run each day at Hour:Min.  Min is rounded to a multitude
%     of 5.
%     - weekly(Day, Hour:Min)
%     Run at the given Day and Time each week.  Day is either a
%     number 1..7 (1 is Monday) or a weekday name or abbreviation.
%     - monthly(DayOfTheMonth, Hour:Min)
%     Run each month at the given Day (1..31).  Note that not all
%     months have all days.
%
%   This  must  be  used   with   a    timer   that   broadcasts   a
%   maintenance(_,_) message (see broadcast/1). Such a timer is part
%   of library(http/http_unix_daemon).

http_schedule_logrotate(When, Options) :-
    listen(maintenance(_,_), http_consider_logrotate),
    compile_schedule(When, Schedule),
    retractall(scheduled_logrotate(_,_)),
    asserta(scheduled_logrotate(Schedule, Options)).

compile_schedule(Var, _) :-
    var(Var),
    !,
    instantiation_error(Var).
compile_schedule(daily(Time0), daily(Time)) :-
    compile_time(Time0, Time).
compile_schedule(weekly(Day0, Time0), weekly(Day, Time)) :-
    compile_weekday(Day0, Day),
    compile_time(Time0, Time).
compile_schedule(monthly(Day, Time0), monthly(Day, Time)) :-
    must_be(between(0, 31), Day),
    compile_time(Time0, Time).

compile_time(HH:MM0, HH:MM) :-
    must_be(between(0, 23), HH),
    must_be(between(0, 59), MM0),
    MM is ((MM0+4)//5)*5.

compile_weekday(N, _) :-
    var(N),
    !,
    instantiation_error(N).
compile_weekday(N, N) :-
    integer(N),
    !,
    must_be(between(1,7), N).
compile_weekday(Day, N) :-
    downcase_atom(Day, Lwr),
    (   sub_atom(Lwr, 0, 3, _, Abbr),
        day(N, Abbr)
    ->  !
    ;   domain_error(day, Day)
    ).

%!  http_consider_logrotate
%
%   Perform a log rotation if the schedule is met

http_consider_logrotate :-
    scheduled_logrotate(Schedule, Options),
    get_time(NowF),
    Now is round(NowF/60.0)*60,
    scheduled(Schedule, Now),
    !,
    http_logrotate([background(true)|Options]).

scheduled(daily(HH:MM), Now) :-
    stamp_date_time(Now, DateTime, local),
    date_time_value(time, DateTime, time(HH,MM,_)).
scheduled(weekly(Day, Time), Now) :-
    stamp_date_time(Now, DateTime, local),
    date_time_value(date, DateTime, Date),
    day_of_the_week(Date, Day),
    scheduled(daily(Time), Now).
scheduled(monthly(Day, Time), Now) :-
    stamp_date_time(Now, DateTime, local),
    date_time_value(day, DateTime, Day),
    scheduled(daily(Time), Now).

day(1, mon).
day(2, tue).
day(3, wed).
day(4, thu).
day(5, fri).
day(6, sat).
day(7, sun).
