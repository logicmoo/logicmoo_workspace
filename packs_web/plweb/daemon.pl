:- use_module(library(http/http_log)).

% Avoid that XPCE creates a thread because that stops us forking.
:- set_prolog_flag(xpce_threaded, false).
:- [load].

:- http_schedule_logrotate(monthly(1, 04:00),
			   [ keep_logs(6)
			   ]).
% library(http/http_unix_daemon) is loaded when loading the library
:- initialization(http_daemon, main).

