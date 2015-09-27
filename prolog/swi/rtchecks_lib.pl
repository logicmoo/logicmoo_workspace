:- module(rtchecks_lib, []).

user:file_search_path(rtchecks, library(rtchecks)).

:- expects_dialect(swi).

:- reexport(rtchecks(rtchecks_utils)).
:- reexport(rtchecks(rtchecks_gen)).
:- reexport(rtchecks(rtchecks_eval)).
:- reexport(rtchecks(rtchecks_rt)).
