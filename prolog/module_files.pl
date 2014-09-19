:- module(module_files, [module_files/2]).

:- use_module(library(included_files)).
:- use_module(library(remove_dups)).

% NOTE: Files are not unique
module_files(M, Files) :-
    module_file_list(M, UFilesL),
    append(UFilesL, UFiles),
    remove_dups(UFiles, Files).

module_file_list(M, Files) :-
    findall(F, module_file_1(M, F), UFiles),
    remove_dups(UFiles, Files0),
    included_files(Files0, Files, [Files0]).

module_file_1(M, File) :-
    module_property(M, file(File)).
module_file_1(M, File) :-
    '$load_context_module'(File, M, _),
    \+ module_property(_, file(File)).
