:- module(nativeprops, []).

%% A wrapper for the Ciao native_props library

:- expects_dialect(ciao).

:- [library(assertions/native_props)].
:- reexport(library(assertions(native_props))).
