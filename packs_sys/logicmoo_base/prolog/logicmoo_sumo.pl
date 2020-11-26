:- module(logicmoo_sumo,[]).

/** <module> logicmoo_sumo - special module hooks into the logicmoo engine allow
   SUMO to be recocogized via our CycL/KIF handlers 
 
 Logicmoo Project: A LarKC Server written in Prolog
 Maintainer: Douglas Miles
 Dec 13, 2035

 ?- ensure_loaded(library(logicmoo_sumo)).

*/

:- ensure_loaded(library(logicmoo_clif)).

:- baseKB:ensure_loaded(library('logicmoo/common_logic/common_logic_sumo.pfc')).

% SETUP SUMO KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- set_prolog_flag(do_renames,term_expansion).

:- during_boot(set_prolog_flag(do_renames,restore)).

sumo_ain2(documentation(_, xtChineseLanguage,_)).
sumo_ain2(CycLOut):-
    delay_rule_eval(CycLOut,sumo_rule,NewAsserts),
    dmsg(NewAsserts),
    ain(NewAsserts).

loadSumo(File):- \+ exists_file(File),!,wdmsg(no_such_file(File)),!.
loadSumo(File):- with_lisp_translation_cached(File,sumo_to_pdkb,nop).


%skip_sumo:- app_argv('--nosumo'),!.
skip_sumo:- app_argv(List), (member('--nosumo',List) ; (\+ member('--sumo',List), \+ member('--snark',List), \+ member('--all',List))),!.

clone_ontologyportal_sumo:- skip_sumo,!.
clone_ontologyportal_sumo:- exists_directory('./ontologyportal_sumo'),!.
clone_ontologyportal_sumo:- shell('git clone https://github.com/ontologyportal/sumo.git ./ontologyportal_sumo'),shell('touch _*.tmp').

:- during_boot(clone_ontologyportal_sumo).

loadSumo1:- skip_sumo,!.
loadSumo1:- 
   loadSumo('./ontologyportal_sumo/Merge.kif'),
   loadSumo('./ontologyportal_sumo/Mid-level-ontology.kif'),
   !.

loadSumo2:- skip_sumo,!.
loadSumo2:- 
   loadSumo('./ontologyportal_sumo/Translations/relations-en.txt'),
   loadSumo('./ontologyportal_sumo/english_format.kif'),
   loadSumo('./ontologyportal_sumo/domainEnglishFormat.kif'),
   !.

loadSumo3:- skip_sumo,!.
loadSumo3:- 
   ensure_loaded(baseKB:library('logicmoo/common_logic/common_logic_sumo.pfc')),
   !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAVE SUMO KB EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- during_boot(loadSumo1).

%:- during_boot(loadSumo2).

%:- during_boot(loadSumo3).

:- fixup_exports.


