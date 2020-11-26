% library_declarations.pl

% Libraries

% $REGULUS = development version

:- asserta(library_directory('$REGULUS/RegulusSpeechServer/Prolog')).
:- asserta(library_directory('$REGULUS/PrologLib')).
:- asserta(library_directory('$REGULUS/Prolog')).
:- asserta(library_directory('.')).

% File search paths

:- asserta(file_search_path(general_regulus_grammars,'$REGULUS/Grammar')).
:- asserta(file_search_path(psa_regulus_grammars,'$REGULUS/Examples/PSA/Regulus')).
:- asserta(file_search_path(corpora,'$REGULUS/Examples/PSA/corpora')).
:- asserta(file_search_path(psa_runtime,'$REGULUS/Examples/PSA/GeneratedFiles')).
:- asserta(file_search_path(psa_prolog,'$REGULUS/Examples/PSA/Prolog')).

:- asserta(file_search_path(tmp,'C:/Temp')).

