:- dynamic
	pre_files/1.
:- findall(F, source_file(F), FL),
   assertz(pre_files(FL)).
:- doc_collect(true).
:- attach_packs(packs, [duplicate(replace)]).
:- load_files([ library(pldoc/doc_library),
		library(thread_pool),
		library(http/http_session),
		library(http/http_unix_daemon),
		library(http/http_dyn_workers),
		library(prolog_source),
		plweb,
                examples,
                blog,
                api,
		wiki_edit,
		stats,
		pack,
		register,
		changelog,
		tagit,
		forum,
		make,
		test_recaptcha,
		watchdog
	      ],
	      [ silent(true)
	      ]).

:- if(exists_source(library(ssh_server))).
:- use_module(library(ssh_server)).
:- use_module(library(broadcast)).
:- listen(http(pre_server_start),
          start_sshd).

start_sshd :-
    ssh_server([ port(2022),
                 authorized_keys_file('etc/ssh/authorized_keys')
               ]).
:- endif.

%%	read_comments(+File)
%
%	Reads PlDoc comments for a file that was already loaded before
%	the server was started.

read_comments(File) :-
	source_file_property(File, module(M)),  !,
	setup_call_cleanup(
	    ( prolog_open_source(File, In),
	      set_prolog_flag(xref, true),
	      '$set_source_module'(Old, M)
	    ),
	    ( repeat,
	        prolog_read_source_term(In, Term, _,
					[ process_comment(true)
					]),
	        Term == end_of_file,
	      !
	    ),
	    ( '$set_source_module'(_, Old),
	      set_prolog_flag(xref, false),
	      prolog_close_source(In)
	    )).
read_comments(_).				% not a module, we do not care

reload_pre_files :-
	pre_files(FL),
	forall(member(F, FL),
	       read_comments(F)).

:- reload_pre_files.
:- doc_load_library.
:- http_set_session_options([enabled(false)]).
:- send(@(pce), catch_error_signals, @(off)).

%%	show_fd
%
%	Show open file descriptors.  Sanity-check   that  works  only on
%	Linux systems.

show_fd :-
        current_prolog_flag(pid, Pid),
        format(string(Cmd),
               '/bin/sh -c "(cd /proc/~w/fd && ls -l | grep socket)"',
               [Pid]),
        shell(Cmd).

show_pools :-
	format('~`-t~52|~n'),
	format('~w~t~20|~t~w~8+~t~w~8+~t~w~8+~t~w~8+~n',
	       [ 'Pool name', 'Running', 'Size', 'Waiting', 'Backlog' ]),
	format('~`-t~52|~n'),
	forall(current_thread_pool(Pool), show_pool(Pool)),
	format('~`-t~52|~n').

show_pool(Pool) :-
	findall(P, thread_pool_property(Pool, P), List),
	memberchk(size(Size), List),
	memberchk(running(Running), List),
	memberchk(backlog(Waiting), List),
	memberchk(options(Options), List),
	option(backlog(MaxBackLog), Options, infinite),
	format('~w~t~20|~t~D  ~8+~t~D ~8+~t~D  ~8+~t~w  ~8+~n',
	       [Pool, Running, Size, Waiting, MaxBackLog]).

stop :-
	halt(42).
