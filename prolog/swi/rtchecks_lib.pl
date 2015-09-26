:- module(rtchecks_lib, []).

user:file_search_path(rtchecks, library(rtchecks)).

:- reexport(rtchecks(rtchecks_utils)).

:- expects_dialect(ciao).
% The following libraries can not be accessed directly from swi, because they
% require ciao emulation:
:- reexport(rtchecks(rtchecks_eval)).
:- reexport(rtchecks(rtchecks_rt)).
:- reexport(rtchecks(rtchecks_gen)).
