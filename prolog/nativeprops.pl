:- module(nativeprops, []).

%% A wrapper for the Ciao native_props library

:- expects_dialect(ciao).

:- use_package(nativeprops).

:- reexport(library(assertions(native_props))).
