
:- asserta(library_directory('$VSS_ROOT/Leo/Regulus/Prolog')).
:- asserta(library_directory('$VSS_ROOT/Leo/PrologLib')).

% File search paths

:- asserta(file_search_path(toy_regulus_grammars,'$VSS_ROOT/Leo/Regulus/Grammars/Toy')).
:- asserta(file_search_path(psa_grammars,'$VSS_ROOT/Leo/Regulus/Grammars/PSA')).
:- asserta(file_search_path(general_eng_grammars,'$VSS_ROOT/Leo/Regulus/Grammars/GeneralEng')).
:- asserta(file_search_path(nuance_grammars,'$VSS_ROOT/Leo/Regulus/NuanceFiles')).
:- asserta(file_search_path(dcg_grammars,'$VSS_ROOT/Leo/Regulus/DCGGrammars')).
:- asserta(file_search_path(gemini_grammars,'$VSS_ROOT/Leo/Regulus/Gemini')).

:- asserta(file_search_path(ah0_swe_regulus_grammars,'$VSS_ROOT/Dhomme/Phase2/src/swedish/Grammar')).
:- asserta(file_search_path(ah1_swe_regulus_grammars,'$VSS_ROOT/Leo/Regulus/Grammars/swe_cantona2')).

:- asserta(file_search_path(ah0_eng_regulus_grammars,'$VSS_ROOT/Dhomme/VisualHouse/src/english/Grammar')).
:- asserta(file_search_path(devruntime_swe, '$VSS_ROOT/DHomme/Phase2/src/swedish/swedishhouse')).
:- asserta(file_search_path(devruntime_eng, '$VSS_ROOT/DHomme/Phase2/src/english/englishhouse')).

:- asserta(file_search_path(tmp,'C:/Temp')).

:- asserta(file_search_path(bpm,'$VSS_ROOT/Leo/Regulus/Grammars/BusinessProcessManagement')).
