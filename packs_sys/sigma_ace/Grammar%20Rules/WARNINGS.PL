% CELT warnings

% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:- write('Loading CELT Version 2(b) ...translation warnings code...'),nl.

%-------------------------------
% CODE FOR TRANSLATION WARNINGS
%   FOR WORD-SENSES SELECTED
%-------------------------------

% gather_translation_warnings(+Nonterminal,+Features,-Formatted_Warnings)
% takes a nonterminal in [np, sentence, query] and its features (frames)
% and returns a list of strings. Each string is a translation warning
% warning that CELT translated a particular word using a particular WordNet
% synset ID and to a particular SUMO concept.

gather_translation_warnings(Nonterminal,Features,Formatted_Warnings) :-
	collect_warnings_from_semantic_frames(Nonterminal,Features,Word_Synset_SUMO_Triples),
	list_to_set(Word_Synset_SUMO_Triples,Unique_Triples), % toss out duplicate warnings
	generate_word_sense_selection_warnings(Unique_Triples,Formatted_Warnings).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE TO FIRST COLLECT LIST OF WORD-SYNSET_ID PAIRS %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%$


% collect_warnings_from_semantic_frames(+PART_OF_SPEECH,+SEMANTICS,-OUT)
% traverses all the of the following semantic features:
% subj -- subject NP
% dobj -- direct object NP
% iobj -- indirect object NP
% adjs -- adjuncts for sentence
% mod  -- if sentence is copula and a modifier is provided
% to collect warnings where a word was interpreted by a particular WordNet Sysnet-ID

% PART_OF_SPEECH is one of [np,sentence,query].
% SEMANTICS is a sentence, query, or np frame constructed
% from parsing. OUT is a list of triples. Each triple is a Word,
% Synset ID, SUMO triple. E.g., OUT might look like [[can,202374411,'Artifact'],[toss,201039125,'Motion']].

collect_warnings_from_semantic_frames(np,Features,Word_Synset_SUMO_Triples) :- % Handles np
	Features = syn<->SYNTAX..sem<->SEMANTICS,
	collect_np_warnings(SEMANTICS,Word_Synset_SUMO_Triples),
	!.

collect_warnings_from_semantic_frames(_,Sentence_Features,ALL_WARNINGS) :- % Handles sentence or query
	Sentence_Features = syn<->SYNTAX..sem<->SEMANTICS,
	SEMANTICS = subj<->SUBJ..dobj<->DOBJ..iobj<->IOBJ,
        % write('Predicate semantics:'),write(PRED),nl,
        % write('Subject semantics:'),write(SUBJ),nl,
        % write('Direct object semantics:'),write(DOBJ),nl,
        % write('Indirect object semantics:'),write(IOBJ),nl,			
	collect_predicate_warnings(SYNTAX,SEMANTICS,PRED_WARNINGS),
        collect_np_warnings(SUBJ,SUBJ_WARNINGS),
        collect_np_warnings(DOBJ,DOBJ_WARNINGS),
        collect_np_warnings(IOBJ,IOBJ_WARNINGS),
	append(PRED_WARNINGS,SUBJ_WARNINGS,PRIMARY_WARNINGS),
	append(PRIMARY_WARNINGS,DOBJ_WARNINGS,SECONDARY_WARNINGS),
	append(SECONDARY_WARNINGS,IOBJ_WARNINGS,ALL_WARNINGS),
	!.

% collect_predicate_warnings(+Predicate,-Warnings) returns all
% the word selection warnings from the predicate.

collect_predicate_warnings(Syntax_Features,Semantics_Features,[[WORD,Synset_ID,SUMO]]) :-
	Syntax_Features = (act<->WORD),
	Semantics_Features = (pred<->SUMO..id<->Synset_ID),
	not(var(WORD)),not(var(SUMO)),not(var(Synset_ID)),!.

collect_predicate_warnings(Syntax_Features,Semantics_Features,[]). % e.g., for copula verbs...

% collect_np_warnings(+NP,-Warnings) returns all
% the word selection warnings from a noun phrase
% frame for the np semantics.

collect_np_warnings(NP,[[WORD,Synset_ID,SUMO]]) :-
	not(var(NP)),
	NP = (noun<->WORD..type<->SUMO..id<->Synset_ID),
	not(var(WORD)),not(var(SUMO)),not(var(Synset_ID)),
	!.

collect_np_warnings(empty,[]) :- !. % the case where there was no NP of this kind (e.g., no direct object)

collect_np_warnings(NP,[]).         % the case where there was an NP but it did not have the right parts to unify (e.g., 'he' as np)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE TO GENERATE WARNINGS GIVEN LIST OF WORD-SYNSET_ID PAIRS %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% generate_word_sense_selection_warnings(+List_Of_Synset_IDs,-Warnings)
% generates word sense warnings for each [Word, WordNet Synset ID, SUMO] triple in the list
% given. Each warning looks like this:
%     Warning: interpreted the meaning of the verb 'sing' as WordNet word sense #3, ID 200724908, meaning:
%     to make melodious sounds; "The nightingale was singing". This word sense maps to SUMO concept 'Communication'.
%
% Each separate warning is a single list element in Warnings.

generate_word_sense_selection_warnings([],[]) :- !. 

generate_word_sense_selection_warnings([[Word,empty,SUMO]|Rest_Of_Synset_IDs],Rest_Of_Warnings) :-
	!,generate_word_sense_selection_warnings(Rest_Of_Synset_IDs,Rest_Of_Warnings).

generate_word_sense_selection_warnings([[Word,Synset_ID,SUMO]|Rest_Of_Synset_IDs],[Warning|Rest_Of_Warnings]) :-
	generate_one_word_sense_selection_warning(Synset_ID,Word,SUMO,Warning),
	generate_word_sense_selection_warnings(Rest_Of_Synset_IDs,Rest_Of_Warnings).

% generate_one_word_sense_selection_warning(+Synset_ID,+Word,+SUMO,-Warning)
% generates one word sense warnings for the WordNet Synset ID
% given. The  warning looks like this:
%     Warning: interpreted the meaning of the verb 'sing' as WordNet word sense #3, ID 200724908, meaning:
%     to make melodious sounds; "The nightingale was singing". This word sense maps to SUMO concept 'Communication'.
% e.g., generate_one_word_sense_selection_warning(200724908,sing,'Communicate',Warning).

generate_one_word_sense_selection_warning(Synset_ID,Word,SUMO,Warning) :-
	doublecheck_synset_ID(Synset_ID,Word) ->
	% proceed with more detailed warnings after double-checking that the synset ID is a legal one.
	(find_part_of_speech(Synset_ID,Part_Of_Speech_Tag),
	    find_short_gloss(Synset_ID,GLOSS),
	    wordnet_letter_for_tag(Part_Of_Speech_Tag,Part_Of_Speech_Abbreviation),
	    find_sense_number(Word,Synset_ID,Word_Sense_Number),
	    find_total_number_of_senses(Word,Part_Of_Speech_Abbreviation,Total_Senses),
	    ((Total_Senses > 1) ->
		sformat(WORD_SENSE_STRING,
			"Warning: interpreted the meaning of the ~w '~w' as  WordNet word sense #~w of ~w senses, ID ~w, meaning ~w. ",
			[Part_Of_Speech_Tag,Word,Word_Sense_Number,Total_Senses,Synset_ID,GLOSS]);
		sformat(WORD_SENSE_STRING,
			"Note: interpreted the ~w '~w' using WordNet synset ID ~w, meaning ~w. ",
			[Part_Of_Speech_Tag,Word,Synset_ID,GLOSS])),	    
	    sformat(SUMO_STRING,
		    " Maps to SUMO '~w'.",
		    [SUMO]),
	    sformat(Warning,"~w~w",[WORD_SENSE_STRING,SUMO_STRING])
	);
	sformat(Warning,"~w is not the correct synset ID entered in the lexicon for the word ~w. Please double check it.",
		 [Synset_ID,Word]).