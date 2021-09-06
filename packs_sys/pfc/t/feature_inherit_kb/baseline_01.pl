
:- use_module(library(pfc_lib)).

:- module(user).

:- expects_dialect(pfc).

:- guess_source_to(_).

:- use_module(baseKB,[]).

baseKB:loves(x,y).
