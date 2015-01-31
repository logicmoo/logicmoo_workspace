:- module(call_in_module_file, [call_in_module_file/2]).

:- use_module(library(filesex)).
:- use_module(library(shell)).

:- meta_predicate call_in_module_file(+,0).
call_in_module_file(M, G) :-
    module_property(M, file(Path)),
    working_directory(WD, WD),
    directory_file_path(Dir, _, Path),
    setup_call_cleanup(cd(Dir), G, cd(WD)).
