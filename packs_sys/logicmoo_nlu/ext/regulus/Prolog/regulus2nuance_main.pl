  
:- ensure_loaded('$REGULUS/PrologLib/compatibility').
 
%---------------------------------------------------------------
 
:- :- module(regulus2nuance_main,
	  [regulus2nuance1/2]
      ).

%---------------------------------------------------------------

'LOAD_DYNAMIC_LEXICON_SUPPORT_IF_AVAILABLE'.

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus_read').
:- use_module('$REGULUS/Prolog/strcat_semantics').
:- use_module('$REGULUS/Prolog/regulus_expand').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/regulus_write_nuance').
:- use_module('$REGULUS/Prolog/regulus_eliminate_empties').
:- use_module('$REGULUS/Prolog/regulus_compact').
:- use_module('$REGULUS/Prolog/regulus_check_recursion').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
 
%---------------------------------------------------------------

regulus2nuance1(InFile, GrammarTag, OutFile) :-
	on_exception(
	Exception, 
	regulus2nuance2(InFile, GrammarTag, OutFile),
	inform_about_top_level_regulus_error(Exception)
    ).

%---------------------------------------------------------------

regulus2nuance2(InFileOrFiles, GrammarTag, OutFile) :-
	%regulus2nuance_init,
	zero_intermediate_dynamic_lex_associations,
	read_regulus_file_or_files(InFileOrFiles, ReadRules0),
	add_lexical_feature_defaults(ReadRules0, ReadRules1),
	convert_rules_to_strcat_semantics_if_necessary(ReadRules1, ReadRules2),
	expand_abbreviations_in_rules(ReadRules2, ReadRules),
	all_sem_features(ReadRules, SemFeats),
	expand_and_filter_regulus_rules(ReadRules, FilteredGroupedRules),
	eliminate_empties(FilteredGroupedRules, FilteredGroupedRulesWithoutEmpties),
	filter_grouped_regulus_rules(FilteredGroupedRulesWithoutEmpties, FilteredGroupedRulesWithoutEmpties1),
	compact_rules(FilteredGroupedRulesWithoutEmpties1, CompactedRules),
	load_intermediate_dynamic_lex_associations_file_if_defined(_Defined),
	check_rules_for_recursion(CompactedRules),
	write_rules_and_feats_to_nuance_files(CompactedRules, SemFeats, GrammarTag, OutFile),
	!.
regulus2nuance2(_InFile, _GrammarTag, _OutFile) :-
	inform_about_top_level_regulus_error(failed).

%---------------------------------------------------------------

%regulus2nuance_init :-
%	retract_all_regulus_preds.

