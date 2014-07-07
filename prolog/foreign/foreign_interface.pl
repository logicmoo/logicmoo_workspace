:- module(foreign_interface, []).

:- use_module(library(compound_expand)).
:- use_module(library(swi/assertions)).
:- use_module(foreign(foreign_props)).
:- use_module(foreign(foreign_generator)).

is_newer(File1, File2) :-
    exists_file(File1),
    exists_file(File2),
    time_file(File1, Time1),
    time_file(File2, Time2),
    Time1 > Time2.

term_expansion((:- gen_foreign_library(AliasSO)), []) :-
    '$set_source_module'(M, M),
    assertz(gen_foreign_library(M, AliasSO)).
term_expansion(end_of_file, (:- Decl)) :-
    '$set_source_module'(M, M),
    current_module(M, File),
    prolog_load_context(file, File),
    retract(gen_foreign_library(M, AliasSO)),
    !,
    absolute_file_name(AliasSO, FileSO),
    ( is_newer(FileSO, File)
    ->print_message(informational,
		    format('Skipping build of ~w: is up to date', [FileSO]))
    ; generate_library(M, FileSO)
    ),
    ( forall(read_foreign_properties(Head, Module, _, _, _, _, _, _),
	     ( predicate_property(Module:Head, number_of_clauses(X)),
	       X>0
	     ))
    ->atom_concat(Module, '$impl', IModule),
      Decl = IModule:load_foreign_library(AliasSO)
    ; Decl = load_foreign_library(AliasSO)
    ).
