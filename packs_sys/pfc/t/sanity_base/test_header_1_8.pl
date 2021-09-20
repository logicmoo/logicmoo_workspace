
%:- if((prolog_load_context(module,user))).
:- module(header_sane,[test_header_include/0]).
test_header_include.
%:- endif.

:- expects_dialect(pfc).

%:- ensure_loaded(library(pfc_test)).
%:- module(header_sane).


