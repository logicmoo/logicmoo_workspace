/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2022, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(process,
          [ process_create/3,           % +Exe, +Args, +Options
            process_wait/2,             % +PID, -Status
            process_wait/3,             % +PID, -Status, +Options
            process_id/1,               % -PID
            process_id/2,               % +Process, -PID
            is_process/1,               % +PID
            process_release/1,          % +PID
            process_kill/1,             % +PID
            process_group_kill/1,       % +PID
            process_group_kill/2,       % +PID, +Signal
            process_kill/2,             % +PID, +Signal

            process_set_method/1        % +CreateMethod
          ]).
:- autoload(library(apply),[maplist/3]).
:- autoload(library(error),[must_be/2,existence_error/2]).
:- autoload(library(option),[select_option/3]).


:- use_foreign_library(foreign(process)).

:- predicate_options(process_create/3, 3,
                     [ stdin(any),
                       stdout(any),
                       stderr(any),
                       cwd(atom),
                       env(list(any)),
                       environment(list(any)),
                       priority(+integer),
                       process(-integer),
                       detached(+boolean),
                       window(+boolean)
                     ]).

/** <module> Create processes and redirect I/O

The module library(process) implements interaction  with child processes
and unifies older interfaces such   as  shell/[1,2], open(pipe(command),
...) etc. This library is modelled after SICStus 4.

The main interface is formed by process_create/3.   If the process id is
requested the process must be waited for using process_wait/2. Otherwise
the process resources are reclaimed automatically.

In addition to the predicates, this module   defines  a file search path
(see user:file_search_path/2 and absolute_file_name/3) named =path= that
locates files on the system's  search   path  for  executables. E.g. the
following finds the executable for =ls=:

    ==
    ?- absolute_file_name(path(ls), Path, [access(execute)]).
    ==

*|Incompatibilities and current limitations|*

    * Where SICStus distinguishes between an internal process id and
    the OS process id, this implementation does not make this
    distinction. This implies that is_process/1 is incomplete and
    unreliable.

    * It is unclear what the detached(true) option is supposed to do. Disable
    signals in the child? Use setsid() to detach from the session?  The
    current implementation uses setsid() on Unix systems.

    * An extra option env([Name=Value, ...]) is added to
    process_create/3.  As of version 4.1 SICStus added
    environment(List) which _modifies_ the environment.  A
    compatible option was added to SWI-Prolog 7.7.23.

@tbd    Implement detached option in process_create/3
@compat SICStus 4
*/


%!  process_create(+Exe, +Args:list, +Options) is det.
%
%   Create a new process running the   file  Exe and using arguments
%   from the given list. Exe is a   file  specification as handed to
%   absolute_file_name/3. Typically one use the =path= file alias to
%   specify an executable file on the current   PATH. Args is a list
%   of arguments that  are  handed  to   the  new  process.  On Unix
%   systems, each element in the list becomes a separate argument in
%   the  new  process.  In  Windows,    the   arguments  are  simply
%   concatenated to form the commandline.   Each  argument itself is
%   either a primitive or  a  list   of  primitives.  A primitive is
%   either atomic or a term file(Spec). Using file(Spec), the system
%   inserts a filename using the OS   filename  conventions which is
%   properly quoted if needed.
%
%   Options:
%
%       * stdin(Spec)
%       * stdout(Spec)
%       * stderr(Spec)
%       Bind the standard streams of the new process. Spec is one of
%       the terms below. If pipe(Pipe) is used, the Prolog stream is
%       a stream in text-mode using the encoding of the default
%       locale.  The encoding can be changed using set_stream/2,
%       or by using the two-argument form of =pipe=, which accepts an
%       encoding(Encoding) option.
%       The options =stdout= and =stderr= may use the same stream,
%       in which case both output streams are connected to the same
%       Prolog stream.
%
%           * std
%           Just share with the Prolog I/O streams.  On Unix,
%           if the `user_input`, etc. are bound to a file handle
%           but not to 0,1,2 the process I/O is bound to the file
%           handles of these streams.
%           * null
%           Bind to a _null_ stream. Reading from such a stream
%           returns end-of-file, writing produces no output
%           * pipe(-Stream)
%           * pipe(-Stream, +StreamOptions)
%           Attach input and/or output to a Prolog stream.
%           The optional StreamOptions argument is a list of options
%           that affect the stream. Currently only the options
%           type(+Type) and encoding(+Encoding) are supported,
%           which have the same meaning as the stream properties
%           of the same name (see stream_property/2).
%           StreamOptions is provided mainly for SICStus compatibility -
%           the SWI-Prolog predicate set_stream/2 can be used
%           for the same purpose.
%           * stream(+Stream)
%           Attach input or output to an existing Prolog stream.
%           This stream must be associated with an OS file
%           handle (see stream_property/2, property `file_no`).
%           This option is __not__ provided by the SICStus
%           implementation.
%
%       * cwd(+Directory)
%       Run the new process in Directory.  Directory can be a
%       compound specification, which is converted using
%       absolute_file_name/3.  See also process_set_method/1.
%       * env(+List)
%       As environment(List), but _only_ the specified variables
%       are passed, i.e., no variables are _inherited_.
%       * environment(+List)
%       Specify _additional_ environment variables for the new process.
%       List is a list of `Name=Value` terms, where `Value` is expanded
%       the same way as the Args argument. If neither `env` nor
%       `environment` is passed the environment is inherited from the
%       Prolog process.  At most one env(List) or environment(List) term
%       may appear in the options. If multiple appear a
%       `permission_error` is raised for the second option.
%       * process(-PID)
%       Unify PID with the process id of the created process.
%       * detached(+Bool)
%       In Unix: If =true=, detach the process from the terminal
%       Currently mapped to setsid();
%       Also creates a new process group for the child
%       In Windows: If =true=, detach the process from the current
%       job via the CREATE_BREAKAWAY_FROM_JOB flag. In Vista and beyond,
%       processes launched from the shell directly have the 'compatibility
%       assistant' attached to them automatically unless they have a UAC
%       manifest embedded in them. This means that you will get a
%       permission denied error if you try and assign the newly-created
%       PID to a job you create yourself.
%       * window(+Bool)
%       If =true=, create a window for the process (Windows only)
%       * priority(+Priority)
%       In Unix: specifies the process priority for the newly
%       created process. Priority must be an integer between -20
%       and 19. Positive values are nicer to others, and negative
%       values are less so. The default is zero. Users are free to
%       lower their own priority. Only the super-user may _raise_ it
%       to less-than zero.
%
%   If the user specifies the process(-PID)   option, he *must* call
%   process_wait/2 to reclaim the process.  Without this option, the
%   system will wait for completion of   the  process after the last
%   pipe stream is closed.
%
%   If the process is not waited for, it must succeed with status 0.
%   If not, an process_error is raised.
%
%   *|Windows notes|*
%
%   On Windows this call is an interface to the CreateProcess() API.
%   The  commandline  consists  of  the  basename  of  Exe  and  the
%   arguments formed from Args. Arguments are  separated by a single
%   space. If all characters satisfy iswalnum()   it is unquoted. If
%   the argument contains a double-quote it   is quoted using single
%   quotes. If both single and double   quotes appear a domain_error
%   is raised, otherwise double-quote are used.
%
%   The CreateProcess() API has  many   options.  Currently only the
%   =CREATE_NO_WINDOW=   options   is   supported     through    the
%   window(+Bool) option. If omitted, the  default   is  to use this
%   option if the application has no   console.  Future versions are
%   likely to support  more  window   specific  options  and replace
%   win_exec/2.
%
%   *Examples*
%
%   First,  a  very  simple  example  that    behaves  the  same  as
%   =|shell('ls -l')|=, except for error handling:
%
%   ==
%   ?- process_create(path(ls), ['-l'], []).
%   ==
%
%   The following example uses grep to find  all matching lines in a
%   file.
%
%   ==
%   grep(File, Pattern, Lines) :-
%           setup_call_cleanup(
%               process_create(path(grep), [ Pattern, file(File) ],
%                              [ stdout(pipe(Out))
%                              ]),
%               read_lines(Out, Lines),
%               close(Out)).
%
%   read_lines(Out, Lines) :-
%           read_line_to_codes(Out, Line1),
%           read_lines(Line1, Out, Lines).
%
%   read_lines(end_of_file, _, []) :- !.
%   read_lines(Codes, Out, [Line|Lines]) :-
%           atom_codes(Line, Codes),
%           read_line_to_codes(Out, Line2),
%           read_lines(Line2, Out, Lines).
%   ==
%
%   @error  process_error(Exe, Status) where Status is one of
%           exit(Code) or killed(Signal).  Raised if the process
%           is waited for (i.e., Options does not include
%           process(-PID)), and does not exit with status 0.
%   @bug    On Windows, environment(List) is handled as env(List),
%           i.e., the environment is not inherited.

process_create(Exe, Args, Options) :-
    (   exe_options(ExeOptions),
        absolute_file_name(Exe, PlProg, ExeOptions)
    ->  true
    ),
    must_be(list, Args),
    maplist(map_arg, Args, Av),
    prolog_to_os_filename(PlProg, Prog),
    Term =.. [Prog|Av],
    expand_cwd_option(Options, Options1),
    expand_env_option(env, Options1, Options2),
    expand_env_option(environment, Options2, Options3),
    process_create(Term, Options3).

%!  exe_options(-Options) is multi.
%
%   Get options for absolute_file_name to find   an  executable file. On
%   Windows we first look for a  readable   file,  but  if this does not
%   exist we are happy with a existing file because the file may be a
%   [reparse point](https://docs.microsoft.com/en-us/windows/win32/fileio/reparse-points-and-file-operations)

exe_options(Options) :-
    current_prolog_flag(windows, true),
    !,
    (   Options = [ extensions(['',exe,com]), access(read), file_errors(fail) ]
    ;   Options = [ extensions(['',exe,com]), access(exist) ]
    ).
exe_options(Options) :-
    Options = [ access(execute) ].

expand_cwd_option(Options0, Options) :-
    select_option(cwd(Spec), Options0, Options1),
    !,
    (   compound(Spec)
    ->  absolute_file_name(Spec, PlDir, [file_type(directory), access(read)]),
        prolog_to_os_filename(PlDir, Dir),
        Options = [cwd(Dir)|Options1]
    ;   exists_directory(Spec)
    ->  Options = Options0
    ;   existence_error(directory, Spec)
    ).
expand_cwd_option(Options, Options).

expand_env_option(Name, Options0, Options) :-
    Term =.. [Name,Value0],
    select_option(Term, Options0, Options1),
    !,
    must_be(list, Value0),
    maplist(map_env, Value0, Value),
    NewOption =.. [Name,Value],
    Options = [NewOption|Options1].
expand_env_option(_, Options, Options).

map_env(Name=Value0, Name=Value) :-
    map_arg(Value0, Value).

%!  map_arg(+ArgIn, -Arg) is det.
%
%   Map an individual argument. Primitives  are either file(Spec) or
%   an atomic value (atom, string, number).  If ArgIn is a non-empty
%   list,  all  elements  are   converted    and   the  results  are
%   concatenated.

map_arg([], []) :- !.
map_arg(List, Arg) :-
    is_list(List),
    !,
    maplist(map_arg_prim, List, Prims),
    atomic_list_concat(Prims, Arg).
map_arg(Prim, Arg) :-
    map_arg_prim(Prim, Arg).

map_arg_prim(file(Spec), File) :-
    !,
    (   compound(Spec)
    ->  absolute_file_name(Spec, PlFile)
    ;   PlFile = Spec
    ),
    prolog_to_os_filename(PlFile, File).
map_arg_prim(Arg, Arg).


%!  process_id(-PID) is det.
%
%   True if PID is the process id of the running Prolog process.
%
%   @deprecated     Use current_prolog_flag(pid, PID)

process_id(PID) :-
    current_prolog_flag(pid, PID).

%!  process_id(+Process, -PID) is det.
%
%   PID is the process id of Process.  Given that they are united in
%   SWI-Prolog, this is a simple unify.

process_id(PID, PID).

%!  is_process(+PID) is semidet.
%
%   True if PID might  be  a   process.  Succeeds  for  any positive
%   integer.

is_process(PID) :-
    integer(PID),
    PID > 0.

%!  process_release(+PID)
%
%   Release process handle.  In this implementation this is the same
%   as process_wait(PID, _).

process_release(PID) :-
    process_wait(PID, _).

%!  process_wait(+PID, -Status) is det.
%!  process_wait(+PID, -Status, +Options) is det.
%
%   True if PID completed with  Status.   This  call normally blocks
%   until the process is finished.  Options:
%
%       * timeout(+Timeout)
%       Default: =infinite=.  If this option is a number, the
%       waits for a maximum of Timeout seconds and unifies Status
%       with =timeout= if the process does not terminate within
%       Timeout.  In this case PID is _not_ invalidated.  On Unix
%       systems only timeout 0 and =infinite= are supported.  A
%       0-value can be used to poll the status of the process.
%
%       * release(+Bool)
%       Do/do not release the process.  We do not support this flag
%       and a domain_error is raised if release(false) is provided.
%
%   @arg  Status is one of exit(Code) or killed(Signal), where
%         Code and Signal are integers.  If the `timeout` option
%         is used Status is unified with `timeout` after the wait
%         timed out.

process_wait(PID, Status) :-
    process_wait(PID, Status, []).

%!  process_kill(+PID) is det.
%!  process_kill(+PID, +Signal) is det.
%
%   Send signal to process PID.  Default   is  =term=.  Signal is an
%   integer, Unix signal name (e.g. =SIGSTOP=)   or  the more Prolog
%   friendly variation one gets after   removing  =SIG= and downcase
%   the result: =stop=. On Windows systems,   Signal  is ignored and
%   the process is terminated using   the TerminateProcess() API. On
%   Windows systems PID must  be   obtained  from  process_create/3,
%   while any PID is allowed on Unix systems.
%
%   @compat SICStus does not accept the prolog friendly version.  We
%           choose to do so for compatibility with on_signal/3.

process_kill(PID) :-
    process_kill(PID, term).


%!  process_group_kill(+PID) is det.
%!  process_group_kill(+PID, +Signal) is det.
%
%   Send signal to the group containing process PID.  Default   is
%   =term=.   See process_wait/1  for  a  description  of  signal
%   handling. In Windows, the same restriction on PID applies: it
%   must have been created from process_create/3, and the the group
%   is terminated via the TerminateJobObject API.

process_group_kill(PID) :-
    process_group_kill(PID, term).


%!  process_set_method(+Method) is det.
%
%   Determine how the process is created on  Unix systems. Method is one
%   of `spawn` (default), `fork` or `vfork`.   If  the method is `spawn`
%   but this cannot be used because it is either not supported by the OS
%   or the cwd(Dir) option is given `fork` is used.
%
%   The problem is to be understood   as  follows. The official portable
%   and safe method to create a process is using the fork() system call.
%   This call however copies the process   page tables and get seriously
%   slow  as  the  (Prolog)  process  is   multiple  giga  bytes  large.
%   Alternatively, we may use vfork() which   avoids copying the process
%   space. But, the safe usage as guaranteed   by  the POSIX standard of
%   vfork() is insufficient for our purposes.  On practical systems your
%   mileage may vary. Modern posix   systems also provide posix_spawn(),
%   which provides a safe and portable   alternative  for the fork() and
%   exec() sequence that may be implemented using   fork()  or may use a
%   fast  but  safe  alternative.  Unfortunately  posix_spawn()  doesn't
%   support the option to specify the   working  directory for the child
%   and we cannot use working_directory/2 as   the  working directory is
%   shared between threads.
%
%   Summarizing, the default is  safe  and  tries   to  be  as  fast  as
%   possible. On some scenarios and on some   OSes  it is possible to do
%   better. It is generally a good  idea   to  avoid  using the cwd(Dir)
%   option of process_create/3 as without we can use posix_spawn().


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:error_message/3.

prolog:error_message(process_error(File, exit(Status))) -->
    [ 'Process "~w": exit status: ~w'-[File, Status] ].
prolog:error_message(process_error(File, killed(Signal))) -->
    [ 'Process "~w": killed by signal ~w'-[File, Signal] ].
prolog:error_message(existence_error(source_sink, path(Exe))) -->
    [ 'Could not find executable file "~p" in '-[Exe] ],
    path_var.

path_var -->
    (   { current_prolog_flag(windows, true) }
    ->  [ '%PATH%'-[] ]
    ;   [ '$PATH'-[] ]
    ).
