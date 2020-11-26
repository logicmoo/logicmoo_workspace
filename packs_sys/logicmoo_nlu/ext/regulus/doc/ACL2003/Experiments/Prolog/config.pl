% config.pl

% Libraries

% Library directories

:- asserta(library_directory('$REGULUS/RegulusSpeechServer/Prolog')).
:- asserta(library_directory('$REGULUS/Prolog')).  
:- asserta(library_directory('$REGULUS/PrologLib')).
:- asserta(library_directory('$REGULUS/Examples/PSA/Prolog')).
:- asserta(library_directory('.')).

% File search paths 

:- asserta(file_search_path(acl_regulus_grammars,'$REGULUS/doc/ACL2003/Experiments/Regulus')).
:- asserta(file_search_path(acl_regulus_iteration_grammars,'$REGULUS/doc/ACL2003/Experiments/RegulusForIteration')).
:- asserta(file_search_path(corpora,'$REGULUS/doc/ACL2003/Experiments/corpora')).
:- asserta(file_search_path(runtime,'$REGULUS/doc/ACL2003/Experiments/GeneratedFiles')).


