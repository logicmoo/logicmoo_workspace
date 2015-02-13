:- module(basicprops, []).

%% A wrapper for the Ciao basic_props library

:- expects_dialect(ciao).
:- use_module(library(assertions/assrt_lib), []).
:- reexport(engine(basic_props)).
