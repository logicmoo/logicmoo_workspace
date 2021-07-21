/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2020, University of Amsterdam
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

:- module(time,
          [ alarm/3,                    % +Time, :Callable, -Id
            alarm/4,                    % +Time, :Callable, -Id, +Options
            alarm_at/3,                 % +Time, :Callable, -Id
            alarm_at/4,                 % +Time, :Callable, -Id, +Options
            remove_alarm/1,             % +Id
            install_alarm/1,            % +Id
            install_alarm/2,            % +Id, +Time
            uninstall_alarm/1,          % +Id
            current_alarm/4,            % ?At, ?:Goal, ?Id, ?Status
            call_with_time_limit/2      % +Time, :Callable
          ]).
:- autoload(library(lists),[member/2]).

:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
    call_with_time_limit(+, 0),
    alarm(+, 0, -),
    alarm(+, 0, -, +),
    alarm_at(+, 0, -, +),
    current_alarm(?, :, ?, ?).

:- predicate_options(alarm/4, 4,
                     [ remove(boolean),
                       install(boolean)
                     ]).


/** <module> Time and alarm library
*/

%!  alarm(+Time, :Callable, -Id) is det.
%!  alarm(+Time, :Callable, -Id, +Options) is det.
%
%   Set up an alarm to be  signaled   Time  seconds from now. If the
%   alarm expires, Callable is called   asynchronously. Callable can
%   be used to raise  an  exception   using  throw/1  to  abort some
%   execution.
%
%   Options is a list of Name(Value) options.  Currently defined
%   options are:
%
%           * remove(Bool)
%           If =true= (default =false=), remove the alarm-event (as
%           remove_alarm/1) after it has been fired.
%           * install(Bool)
%           If =false= (default =true=) do not install the alarm.
%           It must be installed separately using install_alarm/1.

%!  alarm_at(+Time, :Callable, -Id) is det.
%!  alarm_at(+Time, :Callable, -Id, +Options) is det.
%
%   As alarm/3 and alarm/4, but schedule   the  alarm at an absolute
%   point in time.
%
%   @see date_time_stamp/2.

%!  install_alarm(+Id) is det.
%!  install_alarm(+Id, +RelTime) is det.
%
%   Install an alarm allocated using alarm/4 with the install(false)
%   option or de-activated using  uninstall_alarm/1.   With  a given
%   RelTime, the alarm  is  scheduled  at   the  RelTime  from  now.
%   Otherwise it is scheduled on the   same (absolute) time on which
%   is was created.

%!  uninstall_alarm(+Id) is det.
%
%   De-activate an alarm. This does _not_ invalidate Id, but ensures
%   that the alarm will not fire. The alarm can be rescheduled to
%   the original time using install_alarm/1 or to a new time using
%   install_alarm/2.

%!  remove_alarm(+Id) is det.
%
%   Remove an alarm.  If it has not yet been fired, it never will.

%!  current_alarm(?Time, :Goal, ?Id, ?Status) is nondet.
%
%   Enumerate the alarms in the schedule.  Time is the absolute time
%   the event is scheduled for (see   also  get_time/1). Goal is the
%   goal to execute,  Id  is  the   identifier  and  Status  is  the
%   scheduling status. It takes the value   =done=  if the alarm has
%   been fired, =next= if the event is   the next to be executed and
%   =scheduled= otherwise.

:- use_foreign_library(foreign(time)).
:- public time_debug/1.                 % set debugging

%!  call_with_time_limit(+Time, :Goal) is semidet.
%
%   Call Goal, while watching out for   a (wall-time) limit. If this
%   limit  is  exceeded,  the   exception  =time_limit_exceeded=  is
%   raised. Goal is called as in once/1.
%
%   @throws =time_limit_exceeded=

call_with_time_limit(Time, Goal) :-
    Time > 0,
    !,
    setup_call_cleanup(alarm(Time, time_limit_exceeded(Time),
                             Id, [install(false)]),
                       run_alarm_goal(Id, Goal),
                       remove_alarm_notrace(Id)).
call_with_time_limit(_Time, _Goal) :-
    throw(time_limit_exceeded).

run_alarm_goal(AlarmID, Goal) :-
    install_alarm(AlarmID),
    Goal,
    !.

time_limit_exceeded(_Time) :-
    throw(time_limit_exceeded).

current_alarm(Time, Goal, Id, Status) :-
    current_alarms(Time, Goal, Id, Status, List),
    member(alarm(Time, Goal, Id, Status), List).

                 /*******************************
                 *        HANDLE MESSAGES       *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(time_limit_exceeded) -->
    [ 'Time limit exceeded' ].

                 /*******************************
                 *             ALARM            *
                 *******************************/

:- multifile sandbox:safe_meta_predicate/1.

sandbox:safe_meta_predicate(time:call_with_time_limit/2).
