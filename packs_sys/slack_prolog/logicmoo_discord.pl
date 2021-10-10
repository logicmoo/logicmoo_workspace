%#!/usr/bin/swipl 

:- module(logicmoo_discord,[]).

% ==============================================
% [Required] Load the Logicmoo Common Utils
% ==============================================
:- ensure_loaded(library(logicmoo_common)).

:- whenever_flag_permits(load_network,load_library_system(library(logicmoo_network))).
