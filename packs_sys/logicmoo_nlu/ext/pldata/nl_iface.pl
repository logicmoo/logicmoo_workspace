:- module(nl_iface, []).

:- use_module(library(logicmoo_common)).

:- prolog_load_context(file, File),
   absolute_file_name('.', X, [relative_to(File), file_type(directory)]),
   (user:file_search_path(nldata, X)-> true ; asserta(user:file_search_path(nldata, X))),
   (user:file_search_path(pldata, X)-> true ; asserta(user:file_search_path(pldata, X))).


:- export(qcompile_external/1).
qcompile_external(File) :-
        absolute_file_name(File, R, [access(read), file_type(prolog)]),
        file_directory_name(R, LPWD),
        format(atom(A), 'qcompile(~q)', [R]),
        process_create(path(swipl), ['-g', A, '-t', halt], [cwd(LPWD)]).


:- export(rexport_qlf/2).
rexport_qlf(Module, FileName):-
  setup_call_cleanup((
   atom_concat(FileName, '.qlf', QLF),
   atom_concat(FileName, '.pl', PLF)),
   rexport_qlf(Module, FileName, PLF, QLF),
   format(user_error, '~N% Done with ~w. ~n', [FileName])).

module_reexport(Module,File):- Module:ensure_loaded(File).
%module_reexport(Module,File):- Module:reexport(File).

rexport_qlf(Module, _Name, _PLF, QLF):-  exists_source(QLF),
   format(user_error, '~N% Loading ~w  ... ~n', [QLF]),
   import(prolog_statistics:time/1),
   time(catch(((module_reexport(Module,QLF))), E, (dmsg(E-->QLF), fail))), !.
rexport_qlf(Module, Name, _PLF, QLF):- \+ exists_source(QLF),
   format(user_error, '~N% Compiling Quickload ~w (this may take 60-120 seconds the very first time) ... ~n', [QLF]),
   %catch((prolog_statistics:time(load_files(PLF, [qcompile(always)]))), E, (dmsg(E-->Nmae), fail)),
   prolog_statistics:time(catch(((nl_iface:qcompile_external(Name))), E, (dmsg(E-->Name), fail))),
   % prolog_statistics:time(qcompile(QLF)),
   format(user_error, '~N% Made ~w ~n', [QLF]),
   module_reexport(Module,QLF).
   %rexport_qlf(Module, Name, PLF, QLF).
rexport_qlf(_M, _Name, _PLF, QLF):- \+  exists_source(QLF),
   format(user_error, '~N% Missing ~w  ... ~n', [QLF]), fail.
rexport_qlf(Module, _Name, PLF, _QLF):- exists_source(PLF), !,
   format(user_error, '~N% Loading ~w instead  ... ~n', [PLF]),
   module_reexport(Module,PLF).
rexport_qlf(Module, Name, _PLF, _QLF):- exists_source(Name), !,
   format(user_error, '~N% Loading Stem ~w instead  ... ~n', [Name]),
   module_reexport(Module,Name).


:- set_prolog_flag(verbose_load, true).

:- set_prolog_flag(encoding, iso_latin_1).

:- system:reexport(tt0_iface).
:- system:reexport(ac_xnl_iface).
:- system:reexport(clex_iface).
:- system:reexport(talk_db).
:- system:reexport(verbnet_iface).
:- system:reexport(framenet).
:- system:reexport(nldata_cycl_pos0).

% :- system:reexport(nldata_BRN_WSJ_LEXICON, [text_bpos/2]).
:- system:load_files(nldata_dictionary_some01, [reexport(true), qcompile(large)]).

/*
:- system:load_files(nldata_colloc_pdat, [reexport(true), qcompile(large)]).
:- system:load_files(nldata_freq_pdat, [reexport(true), qcompile(large)]).
:- system:load_files(nldata_BRN_WSJ_LEXICON, [reexport(true), qcompile(large)]).
*/





% :- ensure_loaded('../candc/src/prolog/boxer/boxer').

		 /*******************************
		 *          FIND WORDNET	*
		 *******************************/

set_rel_path_from_here:-
   prolog_load_context(file, File),
   absolute_file_name('../../data/WNprolog-3.0/prolog/', WNDB, [relative_to(File), file_type(directory)]),
   setenv('WNDB', WNDB).

:- (getenv('WNDB', WNDB), exists_directory(WNDB)) -> true ; set_rel_path_from_here.

:- nl_iface:rexport_qlf(nl_iface, wn_frames).
% :- load_wordnet.

%:- system:consult(pldata(kb_0988)).

:- fixup_exports.

end_of_file.


true.

mu:  ?- xlisting(happy).
% wordnet:s(301000442, 1, happy, s, 4, 0).
% wordnet:s(301048406, 2, happy, s, 2, 2).
% wordnet:s(301148283, 1, happy, a, 1, 37).
% wordnet:s(302565583, 2, happy, s, 3, 0).
tt0:ttholds(isa, happy, 'Predicate').
tt0:ttholds(isa, happy, 'VariableArityRelation').
tt0:ttholds(emotionOf, go_for_picnic_tt, happy(picnicker_tt)).
tt0:ttholds(emotionOf, go_for_picnic_tt, happy(picnicking_companion_tt)).
negprefix:negprefix(a, unhappy, un, happy).
clex:adj_itr_comp(happier, happy).
clex:adj_itr_sup(happiest, happy).
clex:adj_itr(happy, happy).
talkdb:talk_db(superl, happy, happiest).
talkdb:talk_db(comp, happy, happier).
talkdb:talk_db(adj, happy).
true.

mu:  ?- xlisting("happy").
tt0:ttholds(inflAdjective, 'TTWord_Happy', "happy").
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_HappySurprise', s("happy", "surprise")).
tt0:ttholds(inflNounPluralUnchecked, 'TTWord_HappySurprise', s("happy", "surprises")).
nlkb7166:assertion_content(compoundString, xPersonTheWord, uU(vTheListFn, ["who", "is", "happy"]), xtCountNoun, mobHappy, 894007).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["happy", "birthday"]), xCardTheWord, xtCountNoun, tObjectBirthdayCard, 470153).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["the", "happy", "birthday"]), xSongTheWord, xtCountNoun, iHappyBirthdayToYouTheSong, 4693486).
nlkb7166:assertion_content(bellaStudentBioBlurb, iBELLAStudentWallace, uU(xUnicodeStringFn, s("Wallace", "is", "a", "people", "pleaser", ".", "It", "makes", "him", "especially", "happy", "when", "his", "parents", "are", "happy", ".", "He", "doesn", "&", "u2019", ";", "t", "particularly", "like", "sports", ", ", "or", "math", ", ", "or", "sharing", ", ", "but", "his", "parents", "do", ".", "Since", "his", "parents", "like", "these", "things", ", ", "Wallace", "participates", "in", "all", "three", "(", "among", "other", "things", ")", "just", "enough", "to", "say", "he", "did", ".")), 6633094).
nlkb7166:assertion_content(comment, xAdjectiveModifyingFrame, s("This", "frame", "is", "used", "for", "adverbs", "which", "modify", "adjectives", ".", "Example", ":", "'", "very", "'", ", ", "as", "in", "'", "He", "is", "very", "happy", "'"), 1055892).
nlkb7166:assertion_content(comment, xRegularAdjFrame, s("Frames", "for", "adjectives", "that", "can", "be", "used", "both", "attributively", "and", "predicatively", ", ", "like", "'", "happy", "'", "in", "'", "a", "happy", "person", "'", "and", "'", "Chris", "is", "happy", "'"), 1072903).
nlkb7166:assertion_content(comment, xtAdjectiveGradable, s("#$", "Adjectives", "that", "can", "be", "used", "comparatively", ".", "They", "either", "inflect", "for", "comparison", "using", "either", "-er", "or", "-est", "(", "such", "as", "'", "happy", "'", ")", "or", "they", "occur", "with", "the", "adverbs", "'", "more", "'", "and", "'", "most", "'", "(", "e.g", ".", "'", "delicious", "'", ", ", "'", "successful", "'"), 3095533).
nlkb7166:assertion_content(comment, xtAttributiveFrame, s("Frames", "for", "adjectives", "that", "can", "be", "used", "attributively", ", ", "like", "'", "happy", "'", "in", "'", "a", "happy", "person", "'"), 1072533).
nlkb7166:assertion_content(comment, xtNegativePrefix, s("The", "collection", "of", "prefixes", "which", "serve", "to", "negate", "the", "meaning", "|", "|", "of", "the", "words", "they", "combine", "with", ".", "Example", ":", "#$", "Un_Neg-Pre", "+", "|", "|", "#$", "Happy-TheWord", "means", "\"", "not", "\"", "happy", "."), 835865).
nlkb7166:assertion_content(comment, xtPredicativeFrame, s("Frames", "for", "adjectives", "that", "can", "be", "used", "predicatively", ", ", "like", "'", "happy", "'", "in", "'", "Chris", "is", "happy", "'"), 1072167).
nlkb7166:assertion_content(comment, xTransitiveForNPInfinitivePhraseFrame, s("This", "frame", "is", "used", "for", "verbs", "and", "adjectives", "which", "can", "be", "|", "|", "used", "with", "a", "'", "for-NP-infinitival", "'", "phrase", ".", "Examples", ":", "'", "I", "would", "hate", "for", "him", "to", "leave", "'", "and", "'", "I", "would", "be", "happy", "for", "you", "to", "join", "us", "'"), 1056432).
nlkb7166:assertion_content(preferredBaseForm, xHappyTheWord, "happy", 4540039).
nlkb7166:assertion_content(preferredBaseForm, xHappyTheWord, "happy", 834787).
nlkb7166:assertion_content(regularDegree, xHappyTheWord, "happy", 4542183).
nlkb7166:assertion_content(regularDegree, xHappyTheWord, "happy", 632296).
nlkb7166:assertion_content(sentenceParameterValueInSpecification, uU(xQuoteFn, supportsResponseToOf(s("Sue", "ran", "down", "to", "McDonald", "'", "s", "and", "got", "a", "hamburger", "happy", "meal", "with", "a", "large", "Diet", "Coke", "."), s("Did", "Sue", "get", "a", "cup", "?"), "Yes.")), nartR(iCW_TestQueryFn, iCW_KBCTForCycorp_006_TEP_Response_1), 2620350).
true.

mu:  ?- xlisting( xHappyTheWord).
nlkb7166:assertion_content(prepCollocation, xHappyTheWord, xtAdjectiveGradable, xWithTheWord, 710966).
nlkb7166:assertion_content(adjSemTrans, xHappyTheWord, 0, xRegularAdjFrame, feelsEmotion('NOUN', nartR(vMediumToVeryHighAmountFn, vtHappiness)), 2550768).
nlkb7166:assertion_content(denotationPlaceholder, xHappyTheWord, xtAdjectiveGradable, 1, nartR(iMeaningInSystemFn, iCW_WordNet_1997Version, "A01099314"), 622717).
nlkb7166:assertion_content(denotationPlaceholder, xHappyTheWord, xtAdjectiveGradable, 2, nartR(iMeaningInSystemFn, iCW_WordNet_1997Version, "A00998186"), 622718).
nlkb7166:assertion_content(denotationPlaceholder, xHappyTheWord, xtAdjectiveGradable, 3, nartR(iMeaningInSystemFn, iCW_WordNet_1997Version, "A00556743"), 622719).
nlkb7166:assertion_content(denotationPlaceholder, xHappyTheWord, xtAdjectiveGradable, 5, nartR(iMeaningInSystemFn, iCW_WordNet_1997Version, "A00948198"), 622720).
nlkb7166:assertion_content(denotationRelatedTo, xHappyTheWord, xtAdverb, 0, mobHappy, 894001).
nlkb7166:assertion_content(denotationRelatedTo, xHappyTheWord, xtAdverb, 0, vtHappiness, 704245).
nlkb7166:assertion_content(denotationRelatedTo, xHappyTheWord, xtMassNoun, 0, mobHappy, 894002).
nlkb7166:assertion_content(denotation, xHappyTheWord, xtAdjectiveGradable, 0, nartR(vMediumToVeryHighAmountFn, vtHappiness), 695610).
nlkb7166:assertion_content(denotation, xHappyTheWord, xtMassNoun, 0, vtHappiness, 704453).
nlkb7166:assertion_content(subcatFrame, xHappyTheWord, xtAdjectiveGradable, 0, xTransitiveForNPInfinitivePhraseFrame, 654046).
nlkb7166:assertion_content(subcatFrame, xHappyTheWord, xtAdjectiveGradable, 0, xTransitiveInfinitivePhraseFrame, 643033).
nlkb7166:assertion_content(subcatFrame, xHappyTheWord, xtAdverb, 0, xClauseModifyingFrame, 648937).
nlkb7166:assertion_content(subcatFrame, xHappyTheWord, xtAdverb, 0, xVerbPhraseModifyingFrame, 649007).
nlkb7166:assertion_content(baseForm, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), "happier", 3463683).
nlkb7166:assertion_content(baseForm, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), "happier", 4542180).
nlkb7166:assertion_content(baseForm, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), "happiest", 3468205).
nlkb7166:assertion_content(baseForm, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), "happiest", 4542181).
nlkb7166:assertion_content(comparativeDegree, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), "happier", 3463691).
nlkb7166:assertion_content(comparativeDegree, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), "happier", 4544061).
nlkb7166:assertion_content(derivedUsingSuffix, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xEr_ComparativeTheSuffix, 3463675).
nlkb7166:assertion_content(derivedUsingSuffix, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xEst_SuperlativeTheSuffix, 3468197).
nlkb7166:assertion_content(generalSemantics, xHappyTheWord, nartR(vMediumToVeryHighAmountFn, vtHappiness), 834887).
nlkb7166:assertion_content(isa, nartR(tColOfThingDescribableAsFn, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtAdjectiveGradable), tCol, 3463687).
nlkb7166:assertion_content(isa, nartR(tColOfThingDescribableAsFn, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtAdjectiveGradable), tIntermediateVocabTerm, 3463688).
nlkb7166:assertion_content(isa, nartR(tColOfThingDescribableAsFn, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtAdjectiveGradable), ttSetOrCollection, 3463689).
nlkb7166:assertion_content(isa, nartR(tColOfThingDescribableAsFn, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtAdjectiveGradable), tCol, 3468209).
nlkb7166:assertion_content(isa, nartR(tColOfThingDescribableAsFn, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtAdjectiveGradable), tIntermediateVocabTerm, 3468210).
nlkb7166:assertion_content(isa, nartR(tColOfThingDescribableAsFn, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtAdjectiveGradable), ttSetOrCollection, 3468211).
nlkb7166:assertion_content(isa, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), tIndividual, 3463678).
nlkb7166:assertion_content(isa, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtDerivedWord, 3463676).
nlkb7166:assertion_content(isa, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtEnglishWord, 3463681).
nlkb7166:assertion_content(isa, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtLexicalWord, 3463677).
nlkb7166:assertion_content(isa, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), tIndividual, 3468200).
nlkb7166:assertion_content(isa, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtDerivedWord, 3468198).
nlkb7166:assertion_content(isa, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtEnglishWord, 3468203).
nlkb7166:assertion_content(isa, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtLexicalWord, 3468199).
nlkb7166:assertion_content(isa, xHappyTheWord, xtEnglishWord, 628815).
nlkb7166:assertion_content(isa, xHappyTheWord, xtLexicalWord, 1143614).
nlkb7166:assertion_content(massNumber, xHappyTheWord, "happiness", 634149).
nlkb7166:assertion_content(morphologicallyDerivedFrom, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xHappyTheWord, 3463679).
nlkb7166:assertion_content(morphologicallyDerivedFrom, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xHappyTheWord, 3468201).
nlkb7166:assertion_content(morphologicallyDerivedFrom, xUnhappyTheWord, xHappyTheWord, 703880).
nlkb7166:assertion_content(posBaseForms, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtComparativeAdjective, 3463680).
nlkb7166:assertion_content(posBaseForms, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtSuperlativeAdjective, 3468202).
nlkb7166:assertion_content(posBaseForms, xHappyTheWord, xtAdjectiveGradable, 834831).
nlkb7166:assertion_content(posBaseForms, xHappyTheWord, xtPositiveAdjective, 3462600).
nlkb7166:assertion_content(posBaseForms, xHappyTheWord, xtPositiveAdjective, 4542185).
nlkb7166:assertion_content(posForms, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtAdjectiveGradable, 3463690).
nlkb7166:assertion_content(posForms, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtAdjectiveGradable, 4544641).
nlkb7166:assertion_content(posForms, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtComparativeAdjective, 3463692).
nlkb7166:assertion_content(posForms, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtComparativeAdjective, 4544642).
nlkb7166:assertion_content(posForms, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtAdjectiveGradable, 3468212).
nlkb7166:assertion_content(posForms, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtAdjectiveGradable, 4544639).
nlkb7166:assertion_content(posForms, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtSuperlativeAdjective, 3468214).
nlkb7166:assertion_content(posForms, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtSuperlativeAdjective, 4544640).
nlkb7166:assertion_content(posForms, xHappyTheWord, xtAdjectiveGradable, 4544059).
nlkb7166:assertion_content(posForms, xHappyTheWord, xtAdjectiveGradable, 635206).
nlkb7166:assertion_content(posForms, xHappyTheWord, xtAdverb, 635950).
nlkb7166:assertion_content(posForms, xHappyTheWord, xtMassNoun, 655292).
nlkb7166:assertion_content(preferredBaseForm, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), "happier", 3463682).
nlkb7166:assertion_content(preferredBaseForm, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), "happier", 4542187).
nlkb7166:assertion_content(preferredBaseForm, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), "happiest", 3468204).
nlkb7166:assertion_content(preferredBaseForm, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), "happiest", 4542186).
nlkb7166:assertion_content(preferredBaseForm, xHappyTheWord, "happy", 4540039).
nlkb7166:assertion_content(preferredBaseForm, xHappyTheWord, "happy", 834787).
nlkb7166:assertion_content(quotedIsa, nartR(tColOfThingDescribableAsFn, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtAdjectiveGradable), xtTermUnacceptableForQueryStringMapping, 3951595).
nlkb7166:assertion_content(quotedIsa, nartR(tColOfThingDescribableAsFn, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtAdjectiveGradable), xtTermUnacceptableForQueryStringMapping, 3951971).
nlkb7166:assertion_content(quotedIsa, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtTermSuggestorExpertOnlyTerm, 3463684).
nlkb7166:assertion_content(quotedIsa, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtTermSuggestorExpertOnlyTerm, 3463685).
nlkb7166:assertion_content(quotedIsa, nartR(xWordWithSuffixFn, xHappyTheWord, xEr_ComparativeTheSuffix), xtTermSuggestorExpertOnlyTerm, 3463686).
nlkb7166:assertion_content(quotedIsa, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtTermSuggestorExpertOnlyTerm, 3468206).
nlkb7166:assertion_content(quotedIsa, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtTermSuggestorExpertOnlyTerm, 3468207).
nlkb7166:assertion_content(quotedIsa, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), xtTermSuggestorExpertOnlyTerm, 3468208).
nlkb7166:assertion_content(regularAdverb, xHappyTheWord, "happily", 633814).
nlkb7166:assertion_content(regularDegree, xHappyTheWord, "happy", 4542183).
nlkb7166:assertion_content(regularDegree, xHappyTheWord, "happy", 632296).
nlkb7166:assertion_content(superlativeDegree, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), "happiest", 3468213).
nlkb7166:assertion_content(superlativeDegree, nartR(xWordWithSuffixFn, xHappyTheWord, xEst_SuperlativeTheSuffix), "happiest", 4544060).
true.

mu:  ?- xlisting('TTWord_HappySurprise').
tt0:ttholds(thetaRoleFeat_Frequent, 'TTWord_HappySurprise', xtNoun, 0).
tt0:ttholds(denotation, 'TTWord_HappySurprise', xtNoun, 0, happy_surprise_tt).
tt0:ttholds(isa, 'TTWord_HappySurprise', xtEnglishWord).
tt0:ttholds(posForms, 'TTWord_HappySurprise', xtNoun).
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_HappySurprise', s("happy", "surprise")).
tt0:ttholds(posForms, 'TTWord_HappySurprise', xtNoun).
tt0:ttholds(inflNounPluralUnchecked, 'TTWord_HappySurprise', s("happy", "surprises")).
true.

mu:  ?- xlisting(shower).
% wordnet:s(100257580, 1, shower, n, 2, 4).
% wordnet:s(104208936, 1, shower, n, 1, 5).
% wordnet:s(107363668, 1, shower, n, 4, 1).
% wordnet:s(108255795, 1, shower, n, 6, 0).
% wordnet:s(110070563, 3, shower, n, 5, 0).
% wordnet:s(111502497, 1, shower, n, 3, 1).
% wordnet:s(200035259, 1, shower, v, 3, 2).
% wordnet:s(201184333, 1, shower, v, 5, 0).
% wordnet:s(201372994, 1, shower, v, 2, 2).
% wordnet:s(202264601, 2, shower, v, 1, 2).
% wordnet:s(202757651, 1, shower, v, 4, 0).
verbnet:verbnet(shower, \(s:_, np), ['Agent'], [41, '.', 1, '.', 1]).
verbnet:verbnet(shower, \(s:_, np)/np, ['Patient', 'Agent'], [41, '.', 1, '.', 1]).
verbnet:verbnet(shower, \(s:_, np)/np, ['Patient', 'Agent'], [41, '.', 1, '.', 1]).
verbnet:verbnet(shower, \(s:_, np)/pp/np, ['Destination', 'Agent'], [17, '.', 2]).
verbnet:verbnet(shower, \(s:_, np)/np, ['Destination', 'Agent'], [17, '.', 2]).
verbnet:verbnet(shower, \(s:_, np)/pp, ['Theme'], [9, '.', 7, -, 1]).
verbnet:verbnet(shower, \(s:_, np)/pp/np, ['Theme', 'Agent'], [9, '.', 7, -, 1]).
verbnet:verbnet(shower, \(s:_, np)/pp/np, ['Theme', 'Agent'], [9, '.', 7]).
verbnet:verbnet(shower, \(s:_, np)/pp/np, ['Destination', 'Agent'], [9, '.', 7]).
verbnet:verbnet(shower, \(s:_, np)/np, ['Theme', 'Agent'], [9, '.', 7]).
verbnet:verbnet(shower, \(s:_, np)/np, ['Destination', 'Agent'], [9, '.', 7]).
verbnet:verbnet(shower, \(s:_, lex:it)/np, ['Theme'], [57]).
clex:noun_sg(shower, shower, neutr).
clex:tv_finsg(showers, shower).
clex:tv_pp(showered, shower).
clex:iv_infpl(shower, shower).
clex:tv_infpl(shower, shower).
clex:iv_finsg(showers, shower).
clex:noun_pl(showers, shower, neutr).
talkdb:talk_db(noun1, shower, showers).
talkdb:talk_db(intransitive, shower, showers, showered, showering, showered).
talkdb:talk_db(transitive, shower, showers, showered, showering, showered).
true.

mu:  ?- xlisting("shower").
tt0:ttholds(thetaRole, 'TTWord_Have', xtVerb, 84, take_shower_tt, -1, expl_tt, concept_tt, s("a", "shower"), "V_O", 0).
tt0:ttholds(thetaRole, 'TTWord_Take', xtVerb, 5, take_shower_tt, -1, expl_tt, concept_tt, s("a", "shower"), "V_O", 0).
tt0:ttholds(inflExpletiveUnchecked, 'TTWord_AShower', s("a", "shower")).
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_BathShower', s("bath", "shower")).
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_FoldingShowerDoor', s("folding", "shower", "door")).
tt0:ttholds(inflNounPluralUnchecked, 'TTWord_FoldingShowerDoor', s("folding", "shower", "doors")).
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_HailShower', s("hail", "shower")).
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_PortableShowerHead', s("portable", "shower", "head")).
tt0:ttholds(inflNounPluralUnchecked, 'TTWord_PortableShowerHead', s("portable", "shower", "heads")).
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_RainShower', s("rain", "shower")).
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_SeparateShower', s("separate", "shower")).
tt0:ttholds(inflNounSingular, 'TTWord_Shower', "shower").
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_ShowerCurtain', s("shower", "curtain")).
tt0:ttholds(inflNounPluralUnchecked, 'TTWord_ShowerCurtain', s("shower", "curtains")).
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_ShowerDoor', s("shower", "door")).
tt0:ttholds(inflNounPluralUnchecked, 'TTWord_ShowerDoor', s("shower", "doors")).
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_ShowerHead', s("shower", "head")).
tt0:ttholds(inflNounPluralUnchecked, 'TTWord_ShowerHead', s("shower", "heads")).
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_ShowerStall', s("shower", "stall")).
tt0:ttholds(inflNounPluralUnchecked, 'TTWord_ShowerStall', s("shower", "stalls")).
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_ShowerSwitch', s("shower", "switch")).
tt0:ttholds(inflNounPluralUnchecked, 'TTWord_ShowerSwitch', s("shower", "switches")).
tt0:ttholds(inflNounSingularUnchecked, 'TTWord_SnowShower', s("snow", "shower")).
nlkb7166:assertion_content(compoundString, xInstallTheWord, uU(vTheListFn, [s("a", "shower", "head")]), xtVerb, nartR(actInstallingFn, tObjectShowerHead), 5576679).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["aristocrat"]), xShowerTheWord, xtCountNoun, nartR(iDefaultSemanticsForStringFn, s("aristocrat", "shower")), 2683303).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["aristocrat"]), xShowTheWord, xtAgentiveNoun, nartR(iDefaultSemanticsForStringFn, s("aristocrat", "shower")), 2683304).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["boulder"]), xShowerTheWord, xtCountNoun, nartR(iDefaultSemanticsForStringFn, s("boulder", "shower")), 2705388).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["boulder"]), xShowTheWord, xtAgentiveNoun, nartR(iDefaultSemanticsForStringFn, s("boulder", "shower")), 2705389).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["fairy"]), xShowerTheWord, xtCountNoun, nartR(iDefaultSemanticsForStringFn, s("fairy", "shower")), 2683414).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["fairy"]), xShowTheWord, xtAgentiveNoun, nartR(iDefaultSemanticsForStringFn, s("fairy", "shower")), 2683415).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["shower", "curtain"]), xRingTheWord, xtCountNoun, tObjectShowerCurtainRing, 351852).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["shower", "curtain"]), xRodTheWord, xtCountNoun, tObjectShowerCurtainRod, 356310).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["shower"]), xCenturyTheWord, xtCountNoun, nartR(iDefaultSemanticsForStringFn, s("shower", "centuries")), 2719044).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["shower"]), xCurtainTheWord, xtCountNoun, tPartTypeShowerCurtain, 354092).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["shower"]), xCurtainTheWord, xtCountNoun, tPartTypeShowerCurtain, 575628).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["shower"]), xHeadTheWord, xtCountNoun, tObjectShowerHead, 354992).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["shower"]), xHeadTheWord, xtCountNoun, tObjectShowerHead, 575743).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["shower"]), xMuseumTheWord, xtCountNoun, nartR(iDefaultSemanticsForStringFn, s("shower", "museums")), 2700332).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["shower"]), xStallTheWord, xtCountNoun, tPartTypeShowerStall, 352505).
nlkb7166:assertion_content(infinitive, xShowerTheWord, "shower", 631957).
nlkb7166:assertion_content(singular, xShowerTheWord, "shower", 633584).
true.

mu:  ?- xlisting(take_shower_tt).
tt0:ttholds(roleOf, take_shower_tt, 3, cold_faucet_tt).
tt0:ttholds(eventOf, take_shower_tt, 1, ptrans(human_tt, na_tt, shower_tt)).
tt0:ttholds(eventOf, take_shower_tt, 2, get_in(human_tt, na_tt, shower_tt)).
tt0:ttholds(eventOf, take_shower_tt, 3, flip_to(human_tt, cold_faucet_tt, knob_position_5_tt)).
tt0:ttholds(eventOf, take_shower_tt, 4, flip_to(human_tt, hot_faucet_tt, knob_position_5_tt)).
tt0:ttholds(eventOf, take_shower_tt, 5, flip_to_on(human_tt, shower_switch_tt)).
tt0:ttholds(eventOf, take_shower_tt, 6, pick_up(human_tt, shampoo_tt)).
tt0:ttholds(eventOf, take_shower_tt, 7, pour_onto(human_tt, shampoo_tt, hair_tt)).
tt0:ttholds(eventOf, take_shower_tt, 8, set_on(human_tt, shampoo_tt, na_tt)).
tt0:ttholds(eventOf, take_shower_tt, 9, rub(human_tt, hair_tt)).
tt0:ttholds(eventOf, take_shower_tt, 10, move_to(hair_tt, hair_tt, shower_head_tt)).
tt0:ttholds(eventOf, take_shower_tt, 11, rub(human_tt, hair_tt)).
tt0:ttholds(eventOf, take_shower_tt, 12, pick_up(human_tt, soap_tt)).
tt0:ttholds(eventOf, take_shower_tt, 13, rub(human_tt, part_of_human_tt)).
tt0:ttholds(eventOf, take_shower_tt, 14, put_on(human_tt, soap_tt, soap_dish_tt)).
tt0:ttholds(eventOf, take_shower_tt, 15, rub(human_tt, part_of_human_tt)).
tt0:ttholds(eventOf, take_shower_tt, 16, flip_to_off(human_tt, shower_switch_tt)).
tt0:ttholds(eventOf, take_shower_tt, 17, pick_up(human_tt, towel_tt)).
tt0:ttholds(eventOf, take_shower_tt, 18, rub(human_tt, part_of_human_tt)).
tt0:ttholds(eventOf, take_shower_tt, 19, put_on(human_tt, towel_tt, towel_rack_tt)).
tt0:ttholds(roleOf, take_shower_tt, 8, hair_tt).
tt0:ttholds(roleOf, take_shower_tt, 4, hot_faucet_tt).
tt0:ttholds(roleOf, take_shower_tt, 1, human_tt).
tt0:ttholds(roleOf, take_shower_tt, 11, part_of_human_tt).
tt0:ttholds(roleOf, take_shower_tt, 2, shower_tt).
tt0:ttholds(roleOf, take_shower_tt, 5, shower_switch_tt).
tt0:ttholds(roleOf, take_shower_tt, 6, shower_head_tt).
tt0:ttholds(roleOf, take_shower_tt, 7, shampoo_tt).
tt0:ttholds(roleOf, take_shower_tt, 9, soap_dish_tt).
tt0:ttholds(roleOf, take_shower_tt, 10, soap_tt).
tt0:ttholds(roleOf, take_shower_tt, 12, towel_tt).
tt0:ttholds(thetaRole, 'TTWord_Have', xtVerb, 84, take_shower_tt, 1, subj_tt, concept_tt, "", "", 0).
tt0:ttholds(thetaRole, 'TTWord_Have', xtVerb, 84, take_shower_tt, -1, expl_tt, concept_tt, s("a", "shower"), "V_O", 0).
tt0:ttholds(thetaRole, 'TTWord_Take', xtVerb, 5, take_shower_tt, 1, subj_tt, concept_tt, "", "", 0).
tt0:ttholds(thetaRole, 'TTWord_Take', xtVerb, 5, take_shower_tt, -1, expl_tt, concept_tt, s("a", "shower"), "V_O", 0).
tt0:ttholds(performed_in, take_shower_tt, bathroom_tt).
tt0:ttholds(costOf, take_shower_tt, 1).
tt0:ttholds(durationOf, take_shower_tt, 600).
tt0:ttholds(goalOf, take_shower_tt, attribute_clean(human_tt)).
tt0:ttholds(periodOf, take_shower_tt, 86400).
tt0:ttholds(isa, take_shower_tt, ttCollection).
tt0:ttholds(genls, take_shower_tt, personal_cleaning_script_tt).
tt0:ttholds(genls, take_shower_no_hairwash_tt, take_shower_tt).
tt0:ttholds(genls, take_shower_with_hairwash_tt, take_shower_tt).
true.

mu:  ?- xlisting('TTWord_Shower').
tt0:ttholds(thetaRoleFeat_Frequent, 'TTWord_Shower', xtNoun, 0).
tt0:ttholds(denotation, 'TTWord_Shower', xtNoun, 0, shower_tt).
tt0:ttholds(isa, 'TTWord_Shower', xtEnglishWord).
tt0:ttholds(posForms, 'TTWord_Shower', xtNoun).
tt0:ttholds(inflNounSingular, 'TTWord_Shower', "shower").
tt0:ttholds(posForms, 'TTWord_Shower', xtNoun).
tt0:ttholds(inflNounPlural, 'TTWord_Shower', "showers").
true.

mu:  ?- xlisting(xShowerTheWord).
nlkb7166:assertion_content(verbSemTransCanonical, xShowerTheWord, 0, xIntransitiveVerbFrame, actTakingAShower, uU(vTheListFn, [performedBy]), 1628214).
nlkb7166:assertion_content(denotation, xShowerTheWord, xtCountNoun, 0, actTakingAShower, 630560).
nlkb7166:assertion_content(denotation, xShowerTheWord, xtCountNoun, 1, eventRainProcess, 631391).
nlkb7166:assertion_content(denotation, xShowerTheWord, xtCountNoun, 2, actBabyShower, 630506).
nlkb7166:assertion_content(denotation, xShowerTheWord, xtCountNoun, 3, actBridalShower, 696563).
nlkb7166:assertion_content(denotation, xShowerTheWord, xtVerb, 0, actTakingAShower, 631355).
nlkb7166:assertion_content(denotation, xShowerTheWord, xtVerb, 1, eventRainProcess, 630852).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["afternoon"]), xShowerTheWord, xtCountNoun, nartR(iDefaultSemanticsForStringFn, s("afternoon", "showers")), 2708001).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["aristocrat"]), xShowerTheWord, xtCountNoun, nartR(iDefaultSemanticsForStringFn, s("aristocrat", "shower")), 2683303).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["baby"]), xShowerTheWord, xtCountNoun, actBabyShower, 351892).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["boulder"]), xShowerTheWord, xtCountNoun, nartR(iDefaultSemanticsForStringFn, s("boulder", "shower")), 2705388).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["bridal"]), xShowerTheWord, xtCountNoun, actBridalShower, 507981).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["brown"]), xShowerTheWord, xtCountNoun, actCoprophilia, 498217).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["daybreak"]), xShowerTheWord, xtCountNoun, nartR(iDefaultSemanticsForStringFn, s("daybreak", "showers")), 2707543).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["fairy"]), xShowerTheWord, xtCountNoun, nartR(iDefaultSemanticsForStringFn, s("fairy", "shower")), 2683414).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["golden"]), xShowerTheWord, xtCountNoun, actUrolagnia, 498216).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["meteor"]), xShowerTheWord, xtCountNoun, eventMeteorShower, 423480).
nlkb7166:assertion_content(multiWordString, uU(vTheListFn, ["wedding"]), xShowerTheWord, xtCountNoun, actWeddingShower, 5597441).
nlkb7166:assertion_content(nounSemTrans, xShowerTheWord, 77, xGenitiveFrame, and(isa('NOUN', actTakingAShower), performedBy('NOUN', 'POSSESSOR')), 2890218).
nlkb7166:assertion_content(subcatFrame, xShowerTheWord, xtCountNoun, 77, xGenitiveFrame, 2890651).
nlkb7166:assertion_content(subcatFrame, xShowerTheWord, xtVerb, 0, xIntransitiveVerbFrame, 637329).
nlkb7166:assertion_content(subcatFrame, xShowerTheWord, xtVerb, 1, xIntransitiveVerbFrame, 637336).
nlkb7166:assertion_content(verbSemTrans, xShowerTheWord, 0, xIntransitiveVerbFrame, and(isa('ACTION', actTakingAShower), performedBy('ACTION', 'SUBJECT')), 639086).
nlkb7166:assertion_content(verbSemTrans, xShowerTheWord, 1, xIntransitiveVerbFrame, isa('ACTION', eventRainProcess), 639059).
nlkb7166:assertion_content(infinitive, xShowerTheWord, "shower", 631957).
nlkb7166:assertion_content(isa, xShowerTheWord, xtEnglishWord, 629473).
nlkb7166:assertion_content(isa, xShowerTheWord, xtLexicalWord, 1132446).
nlkb7166:assertion_content(posForms, xShowerTheWord, xtCountNoun, 636873).
nlkb7166:assertion_content(posForms, xShowerTheWord, xtVerb, 636578).
nlkb7166:assertion_content(singular, xShowerTheWord, "shower", 633584).
true.

mu:  ?-

