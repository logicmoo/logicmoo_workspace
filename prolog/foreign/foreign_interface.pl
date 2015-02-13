:- module(foreign_interface, []).

:- use_module(library(change_alias)).
:- use_module(library(compound_expand)).
:- use_module(library(swi/assertions)).
:- use_module(library(foreign/foreign_generator)).

term_expansion((:- gen_foreign_library(AliasSO)),
	       foreign_generator:gen_foreign_library(M, AliasSO)) :-
    '$set_source_module'(M, M).
term_expansion((:- pkg_foreign_config(Package)),
	       foreign_generator:pkg_foreign_config(M, Package)) :-
    '$set_source_module'(M, M).
term_expansion((:- use_foreign_source(FAlias)),
	       foreign_generator:use_foreign_source(M, FAlias)) :-
    '$set_source_module'(M, M).
term_expansion((:- use_foreign_header(FAlias)),
	       foreign_generator:use_foreign_header(M, FAlias)) :-
    '$set_source_module'(M, M).
term_expansion((:- include_foreign_dir(DAlias)),
	       foreign_generator:include_foreign_dir(M, DAlias)) :-
    '$set_source_module'(M, M).
term_expansion((:- extra_compiler_opts(COpt)),
	       foreign_generator:extra_compiler_opts(M, COpt)) :-
    '$set_source_module'(M, M).
term_expansion((:- link_foreign_library(Lib)),
	       foreign_generator:link_foreign_library(M, Lib)) :-
    '$set_source_module'(M, M).

term_expansion(end_of_file, Decl) :-
    '$set_source_module'(M, M),
    current_module(M, File),
    prolog_load_context(file, File), !,
    gen_foreign_library(M, AliasSO),
    change_alias(add_suffix('_so'), AliasSO, AliasSOPl),
    generate_library(M, AliasSO, AliasSOPl, File),
    Decl = [(:- use_module(AliasSOPl))].

add_suffix(Suffix, Name0, Name) :-
    file_name_extension(Name1, _, Name0),
    atom_concat(Name1, Suffix, Name).
