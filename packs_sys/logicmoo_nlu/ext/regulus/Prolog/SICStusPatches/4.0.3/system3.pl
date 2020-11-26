/* Copyright (c) 1995-2008, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : system.pl                                                       %
%   Purpose: Operating system utilities SP3 compatibility                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(system3, [
        now/1,
        datime/1,
        datime/2,
        environ/2,
        sleep/1,
        host_name/1,
        host_id/1,
        file_exists/1,
        file_exists/2,
        file_property/2,
        rename_file/2,
        delete_file/1,
        delete_file/2,
        make_directory/1,
        working_directory/2,
        directory_files/2,
        mktemp/2,
        tmpnam/1,
        system/0,
        system/1,
        system/2,
        shell/0,
        shell/1,
        shell/2,
        exec/3,
        popen/3,
        pid/1,
        kill/2,
        wait/2
        ]).

:- use_module(library(types), [
        must_be/4
        ]).

:- use_module(library(system), [
        now/1,
        datime/1,
        datime/2,
        environ/2,
        sleep/1
        ]).

:- use_module(library(file_systems), [
        rename_file/2,
        make_directory/1,
        delete_directory/2,
        directory_members_of_directory/2,
        file_members_of_directory/2
        ]).

:- use_module(library(sockets), [
        current_host/1
        ]).

:- use_module(library(lists), [
        keys_and_values/3
        ]).

:- use_module(library(process)).

host_name(HostName) :-
        current_host(HostName).

host_id(HostName) :-
        current_host(HostName).

file_exists(FileName) :-
        file_exists(FileName, []).

file_exists(FileName, Permissions) :-
        file_systems:file_exists(FileName, Permissions), !.
file_exists(FileName, Permissions) :-
        file_systems:directory_exists(FileName, Permissions), !.

file_property(FileName, Property) :-
        existing_file_or_directory(FileName, What),
        file_property(What, FileName, Property).

file_property(file, FileName, Property) :-
        file_systems:file_property(FileName, Property).
file_property(directory, FileName, Property) :-
        file_systems:directory_property(FileName, Property).

delete_file(Filename) :-
        delete_file(Filename, [recursive]).

delete_file(Filename, Options) :-
        existing_file_or_directory(Filename, What),
        delete_file(What, Filename, Options).

delete_file(directory, Filename, Options) :-
        memberchk(recursive, Options), !,
        delete_directory(Filename, [if_nonempty(delete)]).
delete_file(directory, Filename, _Options) :-
        delete_directory(Filename, [if_nonempty(ignore)]).
delete_file(file, Filename, _) :-
        file_systems:delete_file(Filename).
delete_file(neither, Filename, Options) :-
        nonmember(ignore, Options),
        absolute_file_name(Filename, _, [access(exist),file_errors(error)]).

working_directory(Old, New) :-
        prolog:'$unix_cd'(Old, New).

directory_files(Directory, RelFiles) :-
        directory_members_of_directory(Directory, Set1),
        file_members_of_directory(Directory, Set2),
        append(Set1, Set2, KL),
        keys_and_values(KL, RelFiles, _).


existing_file_or_directory(Filename, What) :-
        (   absolute_file_name(Filename, Abs, [access(exist),file_type(directory),file_errors(fail)]) ->
            What = directory
        ;   absolute_file_name(Filename, Abs, [access(exist),file_errors(fail)]) ->
            What = file
        ;   What = neither
        ).

proc_call(Binary) :-
        process_create(Binary, [], [process(Pid)]),
        process_wait(Pid, _).

mktemp(Template, FileName) :-
        atom_codes(Template, FileCodes),
        basename(FileCodes, BaseCodes),
        atom_codes(BaseName, BaseCodes),
        open(temp(BaseName), write, S, [if_exists(generate_unique_name)]),
        current_stream(FileName, _, S),
        close(S).

basename(File, Base) :-
        basename(File, S, S, Base).

basename([], Base, [], Base).
basename([0'/|File], _, _, Base) :- !,
        basename(File, S, S, Base).
basename([C|File], S0, [C|S], Base) :- !,
        basename(File, S0, S, Base).

tmpnam(FileName) :-
        open(temp(sp), write, S, [if_exists(generate_unique_name)]),
        current_stream(FileName, _, S),
        close(S).

system :-
        interactive_shell_binary(Binary),
        proc_call(Binary).

system(Cmd) :-
        system(Cmd, 0).

system(Cmd, Status) :-
        system_exec(Cmd, [], exit(Status)).

shell :-
        system.

shell(Cmd) :-
        system(Cmd).

shell(Cmd, Status) :-
        system(Cmd, Status).

interactive_shell_binary(Binary) :- prolog_flag(host_type, 'x86-win32-nt-4'), !,
	environ('COMSPEC', Binary).
interactive_shell_binary(Binary) :- % UNIX
	environ('SHELL', Binary1), Binary1 \== '', !,
	Binary = Binary1.
interactive_shell_binary('/bin/sh').


%% [PM] 4.0.3+ The shell used by system an popen.
system_shell_binary(Binary, DashC) :- prolog_flag(host_type, 'x86-win32-nt-4'), !,
	DashC = '/C',
	environ('COMSPEC', Binary).
system_shell_binary(Binary, DashC) :- % UNIX
	environ('SHELL', Binary1), Binary1 \== '', !,
	DashC = '-c',
	Binary = Binary1.
system_shell_binary('/bin/sh', '-c').



exec(Cmd, [Stdin,Stdout,Stderr], Pid) :-
        shell_exec(Cmd, [stdin(Stdin),stdout(Stdout),stderr(Stderr),process(Process)]),
        %% [PM] 4.0.2+ unfortunately waiting on Pid instead of Process does not work on some UNIX platforms.
        %% xref prolog_process_wait@Emulator/file.c
        %%
        %% process_id(Process, Pid1), % return numeric process id
        %% process_release(Process),
        %% Pid = Pid1.
        Pid = Process.

popen(Cmd, Mode, Stream) :-
        Goal = popen(Cmd,Mode,Stream),
        must_be(Mode, oneof([read,write]), Goal, 2),
        (   Mode==read -> Opt = stdout(pipe(Stream))
        ;   Opt = stdin(pipe(Stream))
        ),
        system_exec(Cmd, [Opt]).

shell_exec(Cmd, Opts) :-
        process_create(Cmd, [], [commandline(true)|Opts]).

shell_exec(Cmd, Opts, Status) :-
        shell_exec(Cmd, [process(Process)|Opts]),
        process_wait(Process, Status).


%% [PM] 4.0.3+ We need to invoke the shell explicitly to ensure things
%% like system('dir > foo.txt') works (SPRM 10682)
%% Win32: this version, like SP3 and SP <= 4.0.2 will open a command
%%        window when running a command. This is an artifact of how
%%        CMD invokes sub-processes.
system_exec(Cmd, Opts) :-
	system_shell_binary(Shell, DashC),
        process_create(Shell, [DashC, Cmd], Opts).

system_exec(Cmd, Opts, Status) :-
        system_exec(Cmd, [process(Process)|Opts]),
        process_wait(Process, Status).

pid(Pid) :-
        process_id(Pid).

kill(Pid, Sig) :-
        process_kill(Pid, Sig).

wait(Pid, Status) :-
        process_wait(Pid, exit(Status)).

