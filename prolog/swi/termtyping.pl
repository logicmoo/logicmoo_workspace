:- module(termtyping, []).

%% A wrapper for the Ciao native_props library

:- expects_dialect(ciao).

:- reexport(library(engine/term_typing)).
