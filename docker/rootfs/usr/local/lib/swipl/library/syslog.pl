/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2020, VU University Amsterdam
                              CWI Amsterdam
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

:- module(syslog,
          [ openlog/3,                  % +Ident, +Options, +Facility
            syslog/2,                   % +Priority, +Message
            syslog/3,                   % +Priority, +Format, +Args
            closelog/0
          ]).
:- autoload(library(lists),[member/2]).

/** <module> Unix syslog interface

This library provides an interface to   the  Unix syslog() facility. The
interface is an almost direct translation of  the POSIX syslog API, with
two additions:

  - syslog/3 exploits format/3 to format syslog messages
  - The library integrates into library(debug) using
    prolog:debug_print_hook/3, where debug _topics_ are mapped to
    syslog _priorities_ and remaining debug _topics_ are mapped
    to the syslog _priority_ =debug=.

Note that this interface  makes  no   attempt  to  abstract over logging
facilities of operating systems. We expect   that such abstractions will
be implemented at the Prolog  level   using  multiple  integrations into
library(debug).

@see    detach_IO/1 to detach normal I/O of the process and remove it
        from the process group.
@see    fork/1 to create a daemon process.
@see    library(uid) to manage user identifiers (e.g., drop root
        privileges).
*/

:- use_foreign_library(foreign(syslog)).

:- dynamic syslog/1.

%!  openlog(+Ident:atom, +Options:list(atom), +Facility:atom) is det.
%
%   Open system log. This predicate provides a direct interface into
%   the openlog() library call. If the   library call is successful,
%   it runs at_halt(closelog) to ensure closing   the  system log on
%   clean exit.
%
%   @param  Ident prepended to every message, and is typically set
%           to the program name.
%   @param  Options is a list of options.  Values are corresponding
%           C options, after removing =LOG_= and translation to
%           lower case: =cons=, =ndelay=, =nowait=, =odelay=,
%           =perror=, =pid=.
%   @param  Facility is one of =auth=, =authpriv=, =cron=, =daemon=,
%           =ftp=, =kern=, =local0= ... =local7=, =lpr=, =mail=,
%           =news=, =syslog=, =user= or =uucp=.

openlog(Ident, Options, Facility) :-
    '$openlog'(Ident, Options, Facility),
    asserta(syslog(Ident)),
    at_halt(closelog).

%!  syslog(+Priority, +Message) is det.
%
%   Send a message to the system  log. Note that syslog/2 implicitly
%   opens a connection to the system log   if  such a connection has
%   not been opened explicitly using openlog/3.
%
%   @param  Priority is one of =emerg=, =alert=, =crit=, =err=,
%           =warning=, =notice=, =info= or =debug=.

%!  syslog(+Priority, +Format, +Args) is det.
%
%   Send a formatted message to the system  log if system logging is
%   opened using openlog/3. This predicate   combined  format/3 with
%   syslog/2. If there is no open  syslog connection, syslog/3 calls
%   print_message/2.

syslog(Priority, Format, Args) :-
    syslog(_),
    !,
    format(string(Msg), Format, Args),
    syslog(Priority, Msg).
syslog(Priority, Format, Args) :-
    syslog_priority(Priority, Kind),
    print_message(Kind, format(Format, Args)).

%!  closelog is det.
%
%   Close the system log.

closelog :-
    retractall(syslog(_)),
    '$closelog'.


                 /*******************************
                 *     DEBUG INTEGRATION        *
                 *******************************/

:- multifile
    prolog:debug_print_hook/3.

%!  prolog:debug_print_hook(+Topic, +Format, +Args) is semidet.
%
%   Integration of debug/3 with the syslog   facility.  If syslog is
%   enabled, debug/3 is re-routed to use   the syslog facilities. If
%   the _topic_ of the  debug  message   matches  one  of  the sylog
%   _priority_ values (see syslog/2), the message   is sent with the
%   corresponding syslog priority. Otherwise  it   it  sent with the
%   =debug= priority.

prolog:debug_print_hook(Topic, Format, Args) :-
    syslog(_),
    debug_priority(Topic, Priority),
    syslog(Priority, Format, Args).

debug_priority(Topic, Priority) :-
    (   syslog_priority(Topic, _Kind)
    ->  Priority = Topic
    ;   Priority = debug
    ).

syslog_priority(emerg,   error).
syslog_priority(alert,   warning).
syslog_priority(crit,    error).
syslog_priority(err,     error).
syslog_priority(warning, warning).
syslog_priority(notice,  informational).
syslog_priority(info,    informational).
syslog_priority(debug,   debug).


                 /*******************************
                 *      MESSAGE INTEGRATION     *
                 *******************************/

user:message_hook(Term, Kind, _) :-
    syslog(_),
    kind_syslog_priority(Kind, Level),
    message_to_string(Term, Message),
    atomic_list_concat(Lines, '\n', Message),
    forall(member(Line, Lines),
           syslog(Level, Line)),
    fail.

kind_syslog_priority(error,         err).
kind_syslog_priority(warning,       warning).
kind_syslog_priority(informational, info).
