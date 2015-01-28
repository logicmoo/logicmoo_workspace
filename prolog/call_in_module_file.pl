:- module(call_in_module_file, [call_in_module_file/2]).

:- meta_predicate call_in_module_file(+,0).
call_in_module_file(M, G) :-
    current_module(M, Path),
    working_directory(WD, WD),
    directory_file_path(Dir, _, Path),
    setup_call_cleanup(cd(Dir), G, cd(WD)).
