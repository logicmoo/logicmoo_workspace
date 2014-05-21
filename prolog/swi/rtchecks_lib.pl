:- module(rtchecks_lib, []).

user:file_search_path(rtchecks, library(rtchecks)).
:- expects_dialect(ciao).
:- reexport(rtchecks(rtchecks_rt)).
:- reexport(rtchecks(rtchecks_utils)).
