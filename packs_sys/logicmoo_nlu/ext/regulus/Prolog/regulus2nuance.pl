% regulus2nuance.pl

% Top-level predicates for regulus2nuance

:- ensure_loaded('$REGULUS/PrologLib/compatibility').
 
%---------------------------------------------------------------

'LOAD_DYNAMIC_LEXICON_SUPPORT_IF_AVAILABLE'.

:- use_module('$REGULUS/Prolog/regulus_declarations').
:- use_module('$REGULUS/Prolog/regulus2nuance_main').
:- use_module('$REGULUS/Prolog/regulus_read').
:- use_module('$REGULUS/Prolog/regulus_expand').
:- use_module('$REGULUS/Prolog/ebl_postprocess').
:- use_module('$REGULUS/Prolog/regulus_utilities').
:- use_module('$REGULUS/Prolog/regulus_write_nuance').

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).

%---------------------------------------------------------------

declared_regulus_switch(bup_filtering, [off, naive, linear]).
declared_regulus_switch(tdown_filtering, [off, on]).
declared_regulus_switch(compaction, [off, on]).
declared_regulus_switch(filtering_language, [cpp, prolog]).
declared_regulus_switch(intermediate_rule_set_storage, [term, file]).

%---------------------------------------------------------------

regulus2nuance(InFile, OutFile) :-
	regulus2nuance_specifying_grammar_tag(InFile, default, OutFile).

regulus2nuance_specifying_grammar_tag(InFile, GrammarTag, OutFile) :-
	init_dynamic_lex_associations_table(OutFile),
	regulus2nuance(InFile, GrammarTag, OutFile),
	write_dynamic_lex_associations_table.

regulus2nuance(InFile, GrammarTag, OutFile) :-
	timed_call(regulus2nuance1(InFile, GrammarTag, OutFile), TimeTaken),
	format('~NCompilation finished, ~2f secs~n', [TimeTaken]).

regulus2nuance_for_multiple_grammars(BaseInFile, IgnoreFeatsFile, Tags, OutFile) :-
	init_dynamic_lex_associations_table(OutFile),
	regulus2nuance_for_multiple_grammars1(Tags, BaseInFile, IgnoreFeatsFile, OutFile, OutComponentFiles),
	write_top_level_multiple_grammar_nuance_files(OutComponentFiles, OutFile),
	write_dynamic_lex_associations_table.

%---------------------------------------------------------------

regulus2nuance_for_multiple_grammars1([], _BaseInFile, _IgnoreFeatsFile, _BaseOutFile, []).
regulus2nuance_for_multiple_grammars1([Tag | Tags], BaseInFile, IgnoreFeatsFile, BaseOutFile, [OutNuanceFile | OutNuanceFiles]) :-
	regulus2nuance_for_multiple_grammars2(Tag, BaseInFile, IgnoreFeatsFile, BaseOutFile, OutNuanceFile),
	!,
	regulus2nuance_for_multiple_grammars1(Tags, BaseInFile, IgnoreFeatsFile, BaseOutFile, OutNuanceFiles).

regulus2nuance_for_multiple_grammars2(Tag, BaseInFile, IgnoreFeatsFile, BaseOutFile, OutFile) :-
	safe_trimcore,
	format('~N~n==========================================================================~n', []),
	format('~N~nCompiling grammar for tag "~w"~n~n', [Tag]),
	add_tag_to_files_in_list([BaseInFile, BaseOutFile], Tag, [InFile, OutFile]),
	regulus2nuance([InFile, IgnoreFeatsFile], Tag, OutFile),
	!.


