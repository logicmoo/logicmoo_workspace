/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
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

:- module(unix_sched, []).
:- use_foreign_library(foreign(sched)).
:- if(current_predicate(setpriority/3)).
:- export(setpriority/3).
:- export(getpriority/3).
:- endif.

/** <module> Access process scheduling

This library provides an interface to  the process scheduling facilities
of the operating system. It is based   on  the sched(7) manual page from
Linux.
*/


%!  setpriority(+Which, +Who, +Priority) is det.
%!  getpriority(+Which, +Who, +Priority) is det.
%
%   Get/set the priority of a single process   or set of processed. Note
%   that on Linux threads are similar   to  processes and this interface
%   also applies to threads. The PID of   a  Prolog thread is accessible
%   through thread_property/2.  Which is one of
%
%     - process
%       Specify a single process/thread.  Who is the PID of the process.
%     - pgrp
%       Specify a _process group_. Who is the process group indentifier.
%       Currently SWI-Prolog has no interface to process groups.
%     - user
%       Specify all processes owned by a user.  Who is the numeric
%       user id of the target user.
%
%   Priority is the _nice_ value of the process and is an integer in the
%   range -20..20, where lower numbers denote a higher priority. Who can
%   be `0` (zero) to specify the calling process, process group or user.
%
%   Please consult the scheduler documentation  of your operating system
%   before  using  setpriority/3.  Unix  systems  generally  schedule  a
%   process at a given priority  only  if   there  is  no process with a
%   higher priority (lower nice value) in _runnable_ state.
%
%   For example, to lower the priority of the `gc` thread we can use the
%   call below.  Note that this may cause GC to never run.
%
%   ```
%   ?- thread_property(gc, system_thread_id(PID)),
%      setpriority(process, PID, 5).
%   ```
%
%   @error existence_error(Which, Who)
%   @error permission_error(setpriority, Which, Who)
