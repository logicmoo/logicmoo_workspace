:- module(nativeprops, []).

%% A wrapper for the Ciao native_props library

:- expects_dialect(ciao).

:- reexport(library(assertions(native_props))).
