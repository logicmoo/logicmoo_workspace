% CELT interface tools, utility code, etc.

% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading CELT Parser Version 2(b) ...utility code and tools...syntactic and semantic translation'),nl.
:-write('Use top_level/0 for a top-level read-parse-translate loop.'),nl.
:-write('Use lookup_word_in_lexicon/1 to check to see if a word is in the lexicon.'),nl.

%-------------------------------
% INTERFACE CODE
%-------------------------------

% eng2log(+Sentence)
% prints out the parse of the sentence given and then the SUMO clauses it generates.

eng2log(Sentence) :-
  sentenceToList(Sentence,Words_In,KindOfSentence),  % convert Sentence, a string, to list and note kind of sentence
  check_sentence_words(KindOfSentence,               % pass along the kind of sentence for morphological processing of words:
		       Words_In,                     % check_sentence_words drops first word to lowercase if necessary, it also
		       Words_Out,                    % maps words not in base form to base form and generates preprocessor warnings, too
		       Lexical_Warnings),            % all preprocessor, not in lexicon, wrong base form, etc.  warnings are returned here
  print_preprocessor_results(Words_In,Words_Out,Lexical_Warnings),
  !,                                                 % if parse fails do not back up past here
  ((KindOfSentence == sentence) -> phrase(sentence(Features),Words_Out); phrase(query(Features),Words_Out)),
  nl,nl,write('Parsed '),write(KindOfSentence),write(' OK:'),write_sent(Words_Out),nl,
  display_feature_structure(Features),
  !,                                                 % if parse succeeds but logic fails do not back up and parse again
  traverse_features_and_return_existential(KindOfSentence,Features,SUO,SPEECH_ACT), % now translate semantic features to logic
  nl,write('Translation to Logic:'),nl,
  % pretty_print(SUO),nl,nl,
  pretty_print_to_string(SUO,yes,Translation),
  write(Translation),nl,
  ask_query_or_make_assertion(Translation,KindOfSentence,SPEECH_ACT),
  describe_speech_act(SPEECH_ACT),
  gather_translation_warnings(KindOfSentence,Features,Translation_Warnings),
  write_numbered_list(Translation_Warnings).

% print_preprocessor_results(+Words_In,+Words_Out,+Lexical_Warnings)
% prints the results of CELT's preprocessing to detect words not in lexicon
% and to coerce words to their proper form when they are in the lexicon but
% are not the right tense, plurality, or capitalization.

print_preprocessor_results(Words_In,Words_Out,Lexical_Warnings) :-
  format("****************************************~n"),	
  ((Words_In == Words_Out) ->
      format("CELT preprocessing: No changes to the input sentence.~n");
      format("CELT preprocessing: The input sentence has been changed.~n"),
      format("~nGiven: "),write_sent(Words_In),
      format("~nRewrite: "),write_sent(Words_Out)),
  ((Lexical_Warnings == []) ->
      format("~nNo lexical errors found in preprocessing.~n");
      format("~nLexical Errors found in preprocessing:~n"),
      write_numbered_list(Lexical_Warnings)),
  format("****************************************~n").

% eng2log(+Sentence,-Translation,-ParseTree,-KindOfSpeechAct,-Lexical_Warnings) :-
% is like eng2log/1 except that it prints its results to String
% and only provides the final translation, not the intermediate parse.

eng2log(Sentence,Translation,ParseTree,KindOfSpeechAct,All_Warnings) :-
  sentenceToList(Sentence,Words_In,KindOfSentence),   % convert Sentence, a string, to list and note kind of sentence
  check_sentence_words(KindOfSentence,                % pass along the kind of sentence for morphological processing of words:
                       Words_In,                      % check_sentence_words drops first word to lowercase if necessary, it also
		       Words_Out,                     % maps words not in base form to base form and generates preprocessor warnings, too
		       Lexical_Warnings),             % all preprocessor, not in lexicon, wrong base form, etc.  warnings are returned here
  !,                                                  % if parse fails do not back up past here
  ((KindOfSentence == sentence) -> phrase(sentence(Features),Words_Out); phrase(query(Features),Words_Out)),
  display_feature_structure(Features,ParseTree),
  !, % if parse succeeds but logic fails do not back up and parse again
  traverse_features_and_return_existential(KindOfSentence,Features,SUO,SPEECH_ACT), % now translate semantic features to logic
  pretty_print_to_string(SUO,yes,Translation),
  ask_query_or_make_assertion(Translation,KindOfSentence,SPEECH_ACT),
  describe_speech_act(SPEECH_ACT,KindOfSpeechAct),
  gather_translation_warnings(KindOfSentence,Features,Translation_Warnings),
  append(Lexical_Warnings,Translation_Warnings,All_Warnings).

% sentenceToList(+Sentence,-List,-KindOfSentence)
%   Sentence is a sentence like "a block is green" or a query like "is there a green block?"
%   List is a list representation of the sentence
%   KindOfSentence is either 'sentence' or 'query'

% Converts a string sentence delimited with spaces into a list
% Example: sentenceToList("Hello there Adam",X,_) succeeds with
% X = [hello, there, adam]. Also handles possessives. For example,
% sentenceToList("John's dog is more expensive than Mary's dog.",X,K)
% succeeds with X = ['John','\'',s,dog,is,more,expensive,than,'Mary','\'',s,dog].

sentenceToList(Sentence,FinalListOfTokens,KindOfSentence) :- % turn the sentence into a list of chars
  string_to_list(Sentence,AsciiCharCodes),
  asciiToTokens(AsciiCharCodes,[],[],FinalListOfTokens,KindOfSentence).
  
% asciiToTokens(+Sentence,+ListOfTokenCharCodes,ListOfTokensSoFar,-FinalListOfTokens,-KindOfSentence)
%  Sentence is the remainder of the sentence string to be converted (a list of chars)
%  ListOfTokenCharCodes is the current token being constructed (a list of chars)
%  ListOfTokensSoFar is the list constructed so far
%  FinalListOfTokens is the final output
%  KindOfSentence if 'query' if end of sentence is a '?'; 'sentence' if end is a '.', 'imperative' for '!', and 'unknown' otherwise.

% if the list characters and the current token are empty, stop.
asciiToTokens([],[],Result,Result,unknown).

% if the list of characters is empty, just add the current token to the list
asciiToTokens(Sentence,ListOfTokenCharCodes,ListOfTokensSoFar,FinalListOfTokens,KindOfSentence) :-
  end_of_sentence(Sentence,KindOfSentence),!,
  string_to_list(TokenString,ListOfTokenCharCodes),  % convert list of char codes for token first to a string...
  string_to_atom(TokenString,TokenAtom),             % and then to an atom, then add it to the list of tokens so far...
  append(ListOfTokensSoFar,[TokenAtom],FinalListOfTokens).
             
% if the next character is not a space or other word delimiter, add it to the current characters in list of token char codes so far
asciiToTokens([FirstChar|RestOfChars],ListOfTokenCharCodes,ListOfTokensSoFar,FinalListOfTokens,KindOfSentence) :-
  not(word_delimiter(FirstChar)),!,
  append(ListOfTokenCharCodes,[FirstChar],UpdatedListOfTokenChars),
  asciiToTokens(RestOfChars,UpdatedListOfTokenChars,ListOfTokensSoFar,FinalListOfTokens,KindOfSentence).

% if the next two characters are apostrophe and lower-case s and they
% are followed by a single space then add the apostrophe and lower-case s
% each as a separate token to indicate a possessive, after adding the
% current token to the list
asciiToTokens([39,115,32|Tail],ListOfTokenCharCodes,ListOfTokensSoFar,FinalListOfTokens,KindOfSentence) :-
  !,
  string_to_list(TokenString,ListOfTokenCharCodes),    % convert list of char codes for token first to a string...
  string_to_atom(TokenString,TokenAtom),               % and then to an atom, then add it to the list of tokens so far, along with 's
  append(ListOfTokensSoFar,[TokenAtom,'\'',s],UpdatedListOfTokens),
  asciiToTokens(Tail,[],UpdatedListOfTokens,FinalListOfTokens,KindOfSentence).

% if the next character is a space or other word delimiter, add the current token to the list if it is not empty
asciiToTokens([FirstChar|RestOfChars],ListOfTokenCharCodes,ListOfTokensSoFar,FinalListOfTokens,KindOfSentence) :-
  word_delimiter(FirstChar), !,
  (ListOfTokenCharCodes == [] ->
        asciiToTokens(RestOfChars,[],ListOfTokensSoFar,FinalListOfTokens,KindOfSentence); % if empty token chars just skip token
        (
	  string_to_list(TokenString,ListOfTokenCharCodes), % convert list of char codes for token first to a string...
	  string_to_atom(TokenString,TokenAtom), % and then to an atom, then add it to the list of tokens so far...
	  append(ListOfTokensSoFar,[TokenAtom],UpdatedListOfTokens),
	  asciiToTokens(RestOfChars,[],UpdatedListOfTokens,FinalListOfTokens,KindOfSentence)
	)).

end_of_sentence([46],sentence).                    % period
end_of_sentence([63],query).                       % question mark
end_of_sentence([33],imperative).                  % exclamation point, note: imperatives not handled in ACE.
end_of_sentence([],  sentence).                    % let sentence be the default sentence type
                      
word_delimiter(CharCode) :- not(code_type(CharCode,alnum)). % any non alphanumeric character is now a word delimiter and skipped over.

% used to be...
% word_delimiter(32).                       % space
% word_delimiter(44).                       % comma
% word_delimiter(58).                       % colon
% word_delimiter(59).                       % semicolon
% word_delimiter(34).                       % double quote
% word_delimiter(39).                       % single quote
% word_delimiter(40).                       % left parens
% word_delimiter(41).                       % right parens

% check_sentence_words(+Type_Of_Input, +Words_In,-Rewritten,-Output)
% if the first part of the sentence is already capitalized
% and is not a determiner or other start of a CELT sentence,
% and is in the proper noun lexicon then continue with the
% sentence as is, i.e., leave the capitalization alone. Otherwise,
% drop any capitalization.

% handle nouns that are not capitalized but should be
% handle nouns that are capitalized but should not be
% restrict words immediately preceded by determiners to be nouns

% Examples:

% trace([check_sentence_words,not_capitalized,check_words_in_lexicon,check_lexicon,rewrite_word_to_root_forms,
% generate_lexicon_warning, dynamically_introduce_proper_noun]).

% rewriting verb and noun forms...
% check_sentence_words(sentence,[the,'Zulus',attack,the,boxes],Out,Warnings).
% check_sentence_words(sentence,['John',entered,the,bank],Out,Warnings).
% check_sentence_words(query,['Does','John',robs,the,banks],Out,Warnings).
% check_sentence_words(query,['Who',robbed,the,banks],Out,Warnings).


% handling capitalization and undeclared proper nouns...
% check_sentence_words(sentence,[the,'Deboxer',attack,the,boxes],Out,Warnings).
% check_sentence_words(sentence,['John',travels,to,the,united,states],Out,Warnings).

% if the first word isn't capitalized anyway then just go onto check_lexicon.
check_sentence_words(Type_Of_Input,[First_Word|Others],Rewritten,Output) :-
	not_capitalized(First_Word),
	!,
	check_words_in_lexicon(Type_Of_Input,[First_Word|Others],Rewritten,Output).

% if the first word is capitalized but is a normal word starting a CELT sentence
% that is not a proper noun then just replace it with its uncapitalized version.
check_sentence_words(Type_Of_Input,[First_Word|Others],Rewritten,Output) :-
	start_of_CELT_sentence(First_Word,LowerCased),
	!,
	check_words_in_lexicon(Type_Of_Input,[LowerCased|Others],Rewritten,Output).

% if the first word is a recognized proper noun just continue leaving the capitalization alone.
check_sentence_words(Type_Of_Input,[Name|Rest],Rewritten,Output) :-
	proper_noun_in_lexicon(Name,Type,Gender,Number,SUO_type,SUO_constant,Synset_ID),
	!,
	check_words_in_lexicon(Type_Of_Input,[Name|Rest],Rewritten,Output).

% if the first word sequence is a recognized proper noun sequence just continue leaving the capitalization alone.
check_sentence_words(Type_Of_Input,[First_Word,Second_Word|RestOfSentence],Rewritten,Output) :-
	proper_noun_in_lexicon([First_Word,Second_Word|RestOfSequence],Type,Gender,Number,SUO_type,SUO_constant,Synset_ID),
	!,
	check_words_in_lexicon(Type_Of_Input,[First_Word,Second_Word|RestOfSequence],Rewritten,Output).

% If we reach here we have a capitalized word not recognized as a proper noun, proper noun
% sequence, or common CELT start of sentence. In this case remove the capitalization and continue
% if we recognize the word anywhere in CELT's lexicon.
check_sentence_words(Type_Of_Input,[First_Word|Others],Rewritten,Output) :-
	lowercase(First_Word,Uncapitalized_First_Word),
	word_in_lexicon(Uncapitalized_First_Word),
	!,
	check_words_in_lexicon(Type_Of_Input,[Uncapitalized_First_Word|Others],Rewritten,Output).

% If we reach here we have a capitalized word not recognized as a proper noun, proper noun
% sequence, or common CELT start of sentence. Also, if we remove the capitalization and continue
% the word will not be recognized as part of CELT's lexicon. In this case assume the word is a proper
% noun to be introduced. Then start checking the rest of the sentence after this word, making note
% that the first word just processed was a noun. (Basically we skip the call to check_words_in_lexicon
% and change the sentence type here if necessary, then call check_lexicon directly starting on the
% next word in the sentence after the newly introduced proper noun).

check_sentence_words(Type_Of_Input,[First_Word|Others],[First_Word|Rewritten],[Warning_String|Output]) :-
	!,
	dynamically_introduce_proper_noun(First_Word),
	generate_lexicon_warning(First_Word,First_Word,undeclared_proper_noun,Type_Of_Input,Warning_String),
	((Type_Of_Input == query, memberchk(does,Others)) -> Kind_Of_Sentence = query_with_does; Kind_Of_Sentence = Type_Of_Input),
	check_lexicon(Kind_Of_Sentence,noun,Others,Rewritten,List_Of_Warnings).	

:-dynamic proper_noun_in_lexicon/7.

% dynamically_introduce_proper_noun(+Name)
% adds a new assertion at the beginning of the others in the lexicon that declares that the
% name given is a proper name for a man, woman, time, or object. For the time being we make
% the simplifying assumption that all new proper names are those for men.

dynamically_introduce_proper_noun(Word) :-
	asserta(proper_noun_in_lexicon(Word,person,masculine,singular,['Human','Male','FullyFormed'],'Person-1',empty)).

% start_of_CELT_sentence(+Captitalized,-Lowercase).
% enumerates common starts of CELT sentences and
% prevents confusion of 'A' starting a sentence with
% the blood group type 'A' as a proper noun starting
% the sentence.

start_of_CELT_sentence('A',a).
start_of_CELT_sentence('The',the).
start_of_CELT_sentence('He',he).
start_of_CELT_sentence('She',she).
start_of_CELT_sentence('Any',any).
start_of_CELT_sentence('Every',every).
start_of_CELT_sentence('All',all).
start_of_CELT_sentence('There',there).
start_of_CELT_sentence('If',if).
start_of_CELT_sentence('No',no).
start_of_CELT_sentence('Does',does).
start_of_CELT_sentence('What',what).
start_of_CELT_sentence('Who',who).
start_of_CELT_sentence('Which',which).
start_of_CELT_sentence('How',how).
start_of_CELT_sentence('Since',since).
start_of_CELT_sentence('Until',until).

% check_words_in_lexicon(+Kind_Of_Sentence,+Words_In,-Rewritten,-List_Of_Warnings)
% changes 'query' to 'query_with_does' if the kind of sentence is 'query' and
% the word 'does' in is in the sentence. Otherwise it just calls check_lexicon to
% to check each word in the sentence, rewriting to drop suffixes where necessary.

% this fn is just an 'adapter' to change 'query' to 'query_with_does' if necessary.

check_words_in_lexicon(query,Words_In,Rewritten,List_Of_Warnings) :-
	memberchk(does,Words_In),
	!,
	check_lexicon(query_with_does,start_of_sentence,Words_In,Rewritten,List_Of_Warnings).

check_words_in_lexicon(Kind_Of_Sentence,Words_In,Rewritten,List_Of_Warnings) :-
	!,
	check_lexicon(Kind_Of_Sentence,start_of_sentence,Words_In,Rewritten,List_Of_Warnings).

% check_lexicon(+Type_Of_Input,+Type_Of_Last_Word,+Words_In,-Rewritten,-List_Of_Warnings)

% checks that each word in Words_In is in the lexicon.

% Type_Of_Input and Type_Of_Last_Word are used to provide simple heuristics
% to limit the possible parts of speech of the current word, both for checking
% in the lexicon and for considering what rewrite rules to use.

% Type_Of_Input is either sentence, query, np, or query_with_does.
% A sentence can only have verbs that are present indicative form.
% A query can have verbs of either base root form or present indicative form.
% A query with does can only have verbs of base root form.

% Type_Of_Last_Word can be 'start_of_sentence', 'unknown', 'noun', 'verb', 'determiner',
% 'adjective', 'adverb', 'preposition', or 'possessive'. If the last
% word was a determiner or a possessive then the next word is an adjective
% adverb or noun, but definitely not a verb. This can be useful in distinguishing
% between 'boxes' as a verb or noun for example in 'the boxes' versus 'he boxes'.
% If the type of the last word is 'start_of_sentence' then the current word should
% be a determiner, proper noun, or other acceptable start of CELT sentence, but definitely
% not a verb.

% also does morphological processing: words not in base form
% are rewritten to base form and a warning is generated.

% Special provision is made for the apostrophe-s ( ' s )
% combination that occurs in possessives.

% base case
check_lexicon(Type_Of_Input,Type_Of_Last_Word,[],[],[]).

% apostrophe-s are next two characters (part of a possessive construction)
check_lexicon(Type_Of_Input,Type_Of_Last_Word,['\'',s|Rest],['\'',s|Rewritten],Warnings) :-
	!,check_lexicon(Type_Of_Input,possessive,Rest,Rewritten,Warnings).

% Is the next word sequence (2 or more words) in the lexicon?
% If so then check the words in the sentence after the sequence.

% Distinguish between nouns and verbs to aid in identifying the word following the sequence.
% Here we check for compound nouns...

check_lexicon(Type_Of_Input,Type_Of_Last_Word,[First_Word,Second_Word|RestOfSentence],Rewritten,Warnings) :-
	word_in_lexicon([any_noun],[First_Word,Second_Word|RestOfSequence]),
	subsequence_matches(RestOfSequence,RestOfSentence,SentenceLeftOver),
	!,
	check_lexicon(Type_Of_Input,noun,SentenceLeftOver,Rewrites_for_Rest,Warnings),
	append([First_Word,Second_Word|RestOfSequence],Rewrites_for_Rest,Rewritten).

% Distinguish between nouns and verbs to aid in identifying the word following the sequence.
% Here we check for compound verbs...

check_lexicon(Type_Of_Input,Type_Of_Last_Word,[First_Word,Second_Word|RestOfSentence],Rewritten,Warnings) :-
	word_in_lexicon([any_verb],[First_Word,Second_Word|RestOfSequence]),
	subsequence_matches(RestOfSequence,RestOfSentence,SentenceLeftOver),
	!,
	check_lexicon(Type_Of_Input,verb,SentenceLeftOver,Rewrites_for_Rest,Warnings),
	append([First_Word,Second_Word|RestOfSequence],Rewrites_for_Rest,Rewritten).

% Distinguish between nouns and verbs to aid in identifying the word following the sequence.
% Here we cover all other compound entries.

check_lexicon(Type_Of_Input,Type_Of_Last_Word,[First_Word,Second_Word|RestOfSentence],Rewritten,Warnings) :-
	word_in_lexicon([any_word],[First_Word,Second_Word|RestOfSequence]),
	subsequence_matches(RestOfSequence,RestOfSentence,SentenceLeftOver),
	!,
	check_lexicon(Type_Of_Input,unknown,SentenceLeftOver,Rewrites_for_Rest,Warnings),
	append([First_Word,Second_Word|RestOfSequence],Rewrites_for_Rest,Rewritten).

% word is 'which' or 'what', which acts like a determiner when starting a query, carry on with other words
check_lexicon(query,start_of_sentence,[Word|Rest],[Word|Rewritten],Warnings) :-
	memberchk(Word,[which,what]),
	!,check_lexicon(Type_Of_Input,determiner,Rest,Rewritten,Warnings).

% word is a determiner, carry on with other words
check_lexicon(Type_Of_Input,Type_Of_Last_Word,[Word|Rest],[Word|Rewritten],Warnings) :-
	determiner_in_lexicon(Word,DefiniteOrNot),
	!,check_lexicon(Type_Of_Input,determiner,Rest,Rewritten,Warnings).

% word is a preposition, carry on with other words
check_lexicon(Type_Of_Input,Type_Of_Last_Word,[Word|Rest],[Word|Rewritten],Warnings) :-
	preposition_in_lexicon(Word,Modification_type,Noun_type,SUO_concept),
	!,check_lexicon(Type_Of_Input,preposition,Rest,Rewritten,Warnings).

% word is a noun in the lexicon, carry on with other words
check_lexicon(Type_Of_Input,Type_Of_Last_Word,[Word|Rest],[Word|Rewritten],Warnings) :-
	not(memberchk(Type_Of_Last_Word,[noun,adverb])), % a noun cannot follow an adverb or another separate noun in CELT
	word_in_lexicon([noun],Word),!,check_lexicon(Type_Of_Input,noun,Rest,Rewritten,Warnings).

% word is a verb in the present indicative form in the lexicon, which is correct for a sentence, carry on with other words
check_lexicon(sentence,Type_Of_Last_Word,[Word|Rest],[Word|Rewritten],Warnings) :-
	not(memberchk(Type_Of_Last_Word,[determiner,preposition,adjective, % a verb cannot follow a det, prep, or adj in CELT
					 start_of_sentence,verb])),        % a verb cannot start a sentence or follow another verb
	word_in_lexicon([present_indicative],Word),!,check_lexicon(Type_Of_Input,verb,Rest,Rewritten,Warnings).

% word is a verb in the present indicative form in the lexicon, which is correct for a WH-query, carry on with other words
check_lexicon(query,Type_Of_Last_Word,[Word|Rest],[Word|Rewritten],Warnings) :-
	not(memberchk(Type_Of_Last_Word,[determiner,preposition,adjective, % a verb cannot follow a det, prep, or adj in CELT queries
					 verb])),                          % a verb cannot follow a verb in CELT queries
	word_in_lexicon([present_indicative],Word),!,check_lexicon(Type_Of_Input,verb,Rest,Rewritten,Warnings).

% word is a verb in the base root form in the lexicon, which is correct for a query with 'does', carry on with other words
check_lexicon(query_with_does,Type_Of_Last_Word,[Word|Rest],[Word|Rewritten],Warnings) :-
	not(memberchk(Type_Of_Last_Word,[determiner,preposition,adjective, % a verb cannot follow a det, prep, or adj in CELT queries
					 verb])),                          % a verb cannot follow a verb in CELT queries
	word_in_lexicon([verb_root],Word),!,check_lexicon(Type_Of_Input,verb,Rest,Rewritten,Warnings).

% word is any other word in the lexicon, carry on with other words
check_lexicon(Type_Of_Input,Type_Of_Last_Word,[Word|Rest],[Word|Rewritten],Warnings) :-
	memberchk(Type_Of_Last_Word,[start_of_sentence,unknown]),          % if we could not identify the last word no constraints apply
	word_in_lexicon(Word),!,check_lexicon(Type_Of_Input,unknown,Rest,Rewritten,Warnings).

% Word does not appear to be in the lexicon -- try morphological rules to see if it is a base form + a suffix
% Restrict the rules to apply by what is consistent with the sentence type and last word part of speech.

% Rule for last word was a determiner--
%     If the last word was a determiner then this word should be a noun, adjective, or adverb.

% E.g., "the ___" predicts examples like "the dog", "the red dog", or "the very hungry dog"

check_lexicon(Type_Of_Input,determiner,[Word|Rest],[Rewritten_Word|Rest_Of_Rewrites],All_Warnings) :-
        rewrite_word_to_root_forms(Word,[noun,adjective,adverb],Type_Of_Input,Rewritten_Word,Part_Of_Speech,[],New_Warnings),
	check_lexicon(Type_Of_Input,Part_Of_Speech,Rest,Rest_Of_Rewrites,Other_Warnings),
	append(New_Warnings,Other_Warnings,All_Warnings).

% Rule for last word was a possessive construct--
%     If the last characters indicated a possessive construct then this word should be a noun, adjective, or adverb.

% E.g., "'s  ___" predicts examples like "John's dog", John's red dog", or "John's very hungry dog"

check_lexicon(Type_Of_Input,possessive,[Word|Rest],[Rewritten_Word|Rest_Of_Rewrites],All_Warnings) :-
        rewrite_word_to_root_forms(Word,[noun,adjective,adverb],Type_Of_Input,Rewritten_Word,Part_Of_Speech,[],New_Warnings),
	check_lexicon(Type_Of_Input,Part_Of_Speech,Rest,Rest_Of_Rewrites,Other_Warnings),
	append(New_Warnings,Other_Warnings,All_Warnings).

% Rule for this word is the first word in the sentence--
%     This word should be a noun, determiner, or other CELT start of sentence word.

% Determiners and other normal CELT start of sentence words are already picked up the
% earlier check for word_in_lexicon in the earlier check_lexicon rules.

% E.g., "The...", "Who...", "John...", "Money...", "How...", "Every..."

check_lexicon(Type_Of_Input,start_of_sentence,[Word|Rest],[Rewritten_Word|Rest_Of_Rewrites],All_Warnings) :-
        rewrite_word_to_root_forms(Word,[noun],Type_Of_Input,Rewritten_Word,Part_Of_Speech,[],New_Warnings),
	check_lexicon(Type_Of_Input,Part_Of_Speech,Rest,Rest_Of_Rewrites,Other_Warnings),
	append(New_Warnings,Other_Warnings,All_Warnings).

% Rule for last word was a noun with no possessive construct
%     The next word should be a preposition or a verb. 

% Prepositions and other normal CELT predefined words are already picked up the
% earlier check for word_in_lexicon in the earlier check_lexicon rules.

% Note: it cannot be part of a word sequence since word sequences are skipped over entirely.

% E.g., "...John ___" and "...the monkey ____" predict examples like "John runs.", "...the monkey in the park."

check_lexicon(Type_Of_Input,noun,[Word|Rest],[Rewritten_Word|Rest_Of_Rewrites],All_Warnings) :-
        rewrite_word_to_root_forms(Word,[verb],Type_Of_Input,Rewritten_Word,Part_Of_Speech,[],New_Warnings),
	check_lexicon(Type_Of_Input,Part_Of_Speech,Rest,Rest_Of_Rewrites,Other_Warnings),
	append(New_Warnings,Other_Warnings,All_Warnings).

% Rule for last word was a verb
%     The next word should be a preposition, noun, adverb, adjective, or determiner

% Note: it cannot be part of a word sequence since word sequences are skipped over entirely.

% Prepositions, determiners, and other normal CELT predefined words are already picked up the
% earlier check for word_in_lexicon in the earlier check_lexicon rules.

% E.g., "...enters  ___" and "...is ____" predict examples like "...enters the bank.", "...is angry.",
% "...is in the park.", ...enters in the dark", "...enters money in the machine.", "...enters small change..."
% "...enters punctually"

check_lexicon(Type_Of_Input,verb,[Word|Rest],[Rewritten_Word|Rest_Of_Rewrites],All_Warnings) :-
        rewrite_word_to_root_forms(Word,[noun,adjective,adverb],Type_Of_Input,Rewritten_Word,Part_Of_Speech,[],New_Warnings),
	check_lexicon(Type_Of_Input,Part_Of_Speech,Rest,Rest_Of_Rewrites,Other_Warnings),
	append(New_Warnings,Other_Warnings,All_Warnings).

% If last word was not any of the above then no restrictions apply to the rewriting rules.

check_lexicon(Type_Of_Input,Type_Of_Last_Word,[Word|Rest],[Rewritten_Word|Rest_Of_Rewrites],All_Warnings) :-
        rewrite_word_to_root_forms(Word,[noun,verb,adjective,adverb],Type_Of_Input,Rewritten_Word,Part_Of_Speech,[],New_Warnings),
	check_lexicon(Type_Of_Input,Part_Of_Speech,Rest,Rest_Of_Rewrites,Other_Warnings),
	append(New_Warnings,Other_Warnings,All_Warnings).

% subsequence_matches(+Words,+Sentence,-SentenceLeftOver)
% succeeds if the first words in Sentence match the words
% in Words. If they match then SentenceLeftOver is returned
% as that left over.

subsequence_matches(Words,Sentence,LeftOver) :- append(Words,LeftOver,Sentence).

% is_capitalized(+Word) succeeds if Word, an atom, is capitalized.
is_capitalized(Word) :- atom_codes(Word,[First_Char|Rest]),code_type(First_Char,upper).

% not_capitalized(+Word) succeeds if Word, an atom, is not capitalized.
not_capitalized(Word) :- atom_codes(Word,[First_Char|Rest]),code_type(First_Char,lower).

% FUNCTION WORDS IN DCG RULES BUT NOT EXPLICITLY IN LEXICON

% words handled specially in DCG rules: 'is', 'to', 'of', 'who', 'what', 'than', 'does', etc.
predefined_word(is).
predefined_word(to).
predefined_word(of).
predefined_word(than).
predefined_word(does).
predefined_word(who).
predefined_word(what).
predefined_word(where).
predefined_word(when).
predefined_word(how).
predefined_word(more).
predefined_word(most).
predefined_word(and).
predefined_word(or).
predefined_word(any).
predefined_word(every).
predefined_word(all).
predefined_word(if).
predefined_word(no).
predefined_word(does).
predefined_word(for).
predefined_word(there).
predefined_word(since).
predefined_word(until).
predefined_word(often).
predefined_word(long).

% word_in_lexicon(+Word) succeeds if Word is in the lexicon
% with the case provided and as a single entry (e.g., 'card' not 'credit card').
word_in_lexicon(Word):-
	predefined_word(Word);
	determiner_in_lexicon(Word,DefiniteOrNot);
	noun_in_lexicon(Word,Type,Gender,Countability,Number,SUO_concept,Synset_ID);
	pronoun_in_lexicon(Word,Case,Gender,Type);
	relative_pronoun_in_lexicon(Word,Rel_Type);
	proper_noun_in_lexicon(Word,Type,Gender,Number,SUO_constant,SUO_type,Synset_ID);
	adjective_in_lexicon(Word,Root,Kind,Grade,SUO_concept);
	preposition_in_lexicon(Word,Modification_type,Noun_type,SUO_concept);
	verb_in_lexicon(Word,Root,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID); % e.g., 'enters'
	verb_in_lexicon(_,Word,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID);	  % e.g., 'enter'
	adverb_in_lexicon(Word,Modification_type,Sigma_Concept).

% word_in_lexicon_for_part_of_speech(+Part_Of_Speech,+Word) succeeds if Word is in the lexicon
% with the case provided and as a single entry (e.g., 'card' not 'credit card') and for the part
% of speech given, which should be a list containing one or more of
% [any_word, any_noun, common_noun, proper_noun, any_verb, present_indicative, verb_root,
%                                                adjective, adverb, determiner, pronoun, relative_pronoun, noun, verb].
% where any_word includes all the others
%       any_noun (or just noun) includes common_noun, proper_noun, pronoun, and relative_pronoun.
%       any_verb (or just verb) includes present_indicative, and verb_root.

word_in_lexicon(Part_Of_Speech,Word):-
	intersection([any_word],Part_Of_Speech,Intersection),
	(Intersection\==[]) -> predefined_word(Word).

word_in_lexicon(Part_Of_Speech,Word):-
	intersection([any_word,determiner],Part_Of_Speech,Intersection),
	(Intersection\==[]) -> determiner_in_lexicon(Word,DefiniteOrNot).

word_in_lexicon(Part_Of_Speech,Word):-  % e.g., 'enters'
	intersection([any_word,any_verb,verb,present_indicative],Part_Of_Speech,Intersection),
	(Intersection\==[]) -> 	verb_in_lexicon(Word,Root,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID).

word_in_lexicon(Part_Of_Speech,Word):-  % e.g., 'enter'
	intersection([any_word,any_verb,verb,verb_root],Part_Of_Speech,Intersection),
	(Intersection\==[]) -> verb_in_lexicon(_,Word,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID).

word_in_lexicon(Part_Of_Speech,Word):-
	intersection([any_word,any_noun,noun,common_noun],Part_Of_Speech,Intersection),
	(Intersection\==[]) -> 	noun_in_lexicon(Word,Type,Gender,Countability,Number,SUO_concept,Synset_ID).

word_in_lexicon(Part_Of_Speech,Word):-
	intersection([any_word,any_noun,noun,proper_noun],Part_Of_Speech,Intersection),
	(Intersection\==[]) -> proper_noun_in_lexicon(Word,Type,Gender,Number,SUO_constant,SUO_type,Synset_ID).

word_in_lexicon(Part_Of_Speech,Word):-
	intersection([any_word,any_noun,noun,pronoun],Part_Of_Speech,Intersection),
	(Intersection\==[]) -> pronoun_in_lexicon(Word,Case,Gender,Type).

word_in_lexicon(Part_Of_Speech,Word):-
	intersection([any_word,any_noun,noun,relative_pronoun],Part_Of_Speech,Intersection),
	(Intersection\==[]) -> relative_pronoun_in_lexicon(Word,Rel_Type).

word_in_lexicon(Part_Of_Speech,Word):-
	intersection([any_word,adjective],Part_Of_Speech,Intersection),
	(Intersection\==[]) -> adjective_in_lexicon(Word,Root,Kind,Grade,SUO_concept).

word_in_lexicon(Part_Of_Speech,Word):-
	intersection([any_word,preposition],Part_Of_Speech,Intersection),
	(Intersection\==[]) -> preposition_in_lexicon(Word,Modification_type,Noun_type,SUO_concept).

word_in_lexicon(Part_Of_Speech,Word):-  
	intersection([any_word,adverb],Part_Of_Speech,Intersection),
	(Intersection\==[]) -> adverb_in_lexicon(Word,Modification_type,Sigma_Concept).


%-------------------------------
% UTILITY CODE
%-------------------------------

% lowercase(+WordIn,-WordOut)
% To convert an atom (e.g., 'Red') to lowercase (e.g., 'red')
% e.g., call lowercase(red,X).

lowercase(In,Out) :-
	atom_chars(In,CharList),lower_case_all(CharList,LowerCasedChars),atom_chars(Out,LowerCasedChars).

lower_case_all([],[]).
lower_case_all([FirstChar|Rest],[FirstCharLowered|Others]) :-
	char_type(FirstChar,upper),!,char_type(FirstChar,upper(FirstCharLowered)),
	lower_case_all(Rest,Others).
lower_case_all([FirstChar|Rest],[FirstChar|Others]) :-
	lower_case_all(Rest,Others).

% capitalize(+WordIn,-WordOut)
% To convert an atom (e.g., 'Red') to capitalize (e.g., 'red')
% e.g., call capitalize(red,X).

capitalize(In,Out) :-
	atom_chars(In,[FirstChar|CharList]),
	char_type(FirstChar,lower),!,char_type(FirstChar,lower(FirstCharCapitalized)),
	lower_case_all(CharList,CapitalizedChars),atom_chars(Out,[FirstCharCapitalized|CapitalizedChars]).

% concat_all(+Names,-One_name)
% joins together a list of names (e.g., [visa,credit,card])
% into one name (e.g., 'visa_credit_card').

concat_all([Name],Name) :- !.
concat_all([First|Rest],Complete_name) :-
	concat_all(Rest,Rest_of_name),
	concat(First,'_',First_w_hyphen),
	concat(First_w_hyphen, Rest_of_name, Complete_name).

% get_return is used to pause long print outs for the user to catch up.
get_return :- !,write('[Paused, hit a carriage return to continue]'), get0(X).

% get_return(File) is used to pause long print outs for the user to catch up
% if the file name is 'empty', 'stdio', or is just an uninstantiated variable.
% This function is essentially just skipped over if a file name was given.
get_return(File) :- (var(File);File=empty;File=stdio),!,write('[Paused, hit a carriage return to continue]'), get0(X).
get_return(File). 

% pretty_print(+Formula)
% prints out a formula in a LISP-like format.
% from Adam Pease's original code.

pretty_print(Formula) :- sformat(String,'~w',[Formula]), string_to_list(String,List), !, interior(List,0).

% interior(+Formula,+Indent) prints out the formula at the current level
% of indentation.

interior([],Indent) :- !.
  
% handle strings like ", b, c ..."
% don't print commas (ASCII 44)
interior([44|Rest],Indent) :- !, interior(Rest,Indent).
  
% handle strings like "[a, b, c ..."
% 91 is left bracket
interior([91|T],Indent) :-           % convert left bracket to left paren
  !,
  NewIndent is Indent + 2,
  nl,
  writespaces(Indent),
  write('('),
  interior(T,NewIndent).

% handle strings like "], a, b..."
% 93 is right bracket
interior([93|T],Indent) :-           % convert right bracket to right paren
  !,
  NewIndent is Indent - 2,
  write(')'),  
  interior(T,NewIndent).

% handle all other strings, e.g., "a, b..."
interior([FirstChar|T],Indent) :-
  put(FirstChar),
  interior(T,Indent),
  !.
  
% writespaces(+N) indents over N spaces.

writespaces(0) :-  !.
  
writespaces(Indent) :-
  Indent > 0, !, write(' '), NewIndent is Indent - 1, writespaces(NewIndent).

% pretty_print_to_string(+Formula,-PrettyPrint,-KIF)

% takes a KIF formula in standard Prolog list notation, and then writes
% out a KIF string, suitable for reading in by a LISP reader, in KIF.

% Note: Code adapted from prettyPrint. If PrettyPrint is yes then indenting
% spaces and carriage returns are included for a string that can be
% displayed nicely. If PrettyPrint is no they are not, for input that
% is more compact and that is acceptable to the Sigma API, which currently
% doesn't take newlines in input.

%  Formula--is a list representing a formula (e.g., [exists, [X], [instance, X, cat]]).
%  KIF--The formula is returned suitably for Sigma input: "(exists (?G302) (instance ?G302 cat))"

pretty_print_to_string(Formula,PrettyPrint,KIF) :- sformat(FormulaString,'~w',[Formula]),
	string_to_list(FormulaString,FormulaListOfChars),!,
	interior(FormulaListOfChars,PrettyPrint,0,[],KIF).

% interior(+Formula,+Indent,-KIF_so_far,-KIF) prints out the formula at the current level
% of indentation, adds the formula's printout to KIF_so_far, and ultimately returns the
% complete formula as KIF. KIF_so_far and KIF are both lists of ASCII characters.
interior([],PrettyPrint,Indent,KIF_so_far,KIF_so_far) :- !.
  
% handle strings like ", b, c ..."
% don't print commas (ASCII 44)
interior([44|Rest],PrettyPrint,Indent,KIF_so_far,KIF) :- !, interior(Rest,PrettyPrint,Indent,KIF_so_far,KIF).

% handle strings like "[a, b, c ..."
% convert left bracket to left paren, 91 is left bracket
interior([91|T],PrettyPrint,Indent,KIF_so_far,KIF) :-
  !, NewIndent is Indent + 2,
  % add new line to KIF output so far if generating a string for pretty printing...
  ( (PrettyPrint = yes) -> sformat(New_KIF,"~s~n",[KIF_so_far]); 
                           sformat(New_KIF,"~s",[KIF_so_far])
  ),
  writespaces(Indent,PrettyPrint,New_KIF,Padded_KIF),
  sformat(Final_KIF,"~s(",[Padded_KIF]), % add ( to end of KIF so far
  interior(T,PrettyPrint,NewIndent,Final_KIF,KIF).

% handle strings like "], a, b..."
% convert right bracket to right paren (ASCII 93 is right bracket)
interior([93|T],PrettyPrint,Indent,KIF_so_far,KIF) :-
  !,
  NewIndent is Indent - 2,
  sformat(Final_KIF,"~w)",[KIF_so_far]), % add ) to end of KIF so far
  interior(T,PrettyPrint,NewIndent,Final_KIF,KIF).

% handle all other strings, e.g., "a, b..."
interior([FirstChar|T],PrettyPrint,Indent,KIF_so_far,KIF) :-
  sformat(Final_KIF,"~s~c",[KIF_so_far,FirstChar]), % add FirstChar to end of KIF so far
  interior(T,PrettyPrint,Indent,Final_KIF,KIF),
  !.
  
%------------------
% writespaces(+N,+PrettyPrint,+Input,-Output) adds N spaces to the end of the
% input string Input producing the output string Output if PrettyPrint is yes,
% otherwise it just adds one space if N > 0.

writespaces(0,PrettyPrint,Input,Input) :- !.
  
writespaces(Indent,yes,Input,Output) :-
  Indent > 0, !,
  sformat(Padded,"~s~c",[Input,32]),  % 32 is space
  NewIndent is Indent - 1,
  writespaces(NewIndent,yes,Padded,Output).

writespaces(Indent,no,Input,PlusOneSpace) :-
  Indent > 0, !,
  sformat(PlusOneSpace,"~s~c",[Input,32]).  % 32 is space

% write_numbered_list[+Strings] takes a list of strings
% and displays them in a numbered format.

write_numbered_list(Strings) :- !,write_numbered_items(0,Strings).

% write_numbered_items(+N,+Items)
% writes the sublist starting from number N+1
% of the strings in Items.

write_numbered_items(_,[]) :- !.

write_numbered_items(N,[Item|Rest]) :-
	Next is N + 1,!,
	format("~n~w. ~w~n", [Next, Item]),
	write_numbered_items(Next,Rest).
	       
% print_list(+List) prints a long list in numbered format. The items
% are not necessarily lists but this is just a convenience function
% to display long lists of items, such as instances of a SUMO class.

% e.g., instances('Relation',I),print_list(I).

print_list(List) :- length(List,N),write('A list of '),write(N),write(' items.'),nl,print_each_item(List,0).

print_each_item([],Index) :- nl,!.

print_each_item([Item|Rest],Index) :-
	NewIndex is Index + 1, write(NewIndex),write('. '),write(Item),nl,print_each_item(Rest,NewIndex).

% rehyphenate_compound_words(+Words,-With_Hyphens)
% converts compound words like [credit,card] to
% hyphenated atoms like credit_card, which is the
% way WordNet stores them. Atomic words are unaffected.

rehyphenate_compound_words(Words,With_Hyphens) :-
	is_list(Words),!,
	concat_atom(Words,'_',With_Hyphens).
	
rehyphenate_compound_words(Word,Word).

% LAMBDA ABSTRACTION

% is_lambda_expression(+EXP)
% is_lambda_expression succeeds if EXP is a lambda expression
% and fails otherwise.

is_lambda_expression(EXP) :- atom(EXP),!,fail.
is_lambda_expression(EXP) :- EXP = X^BODY,!.
is_lambda_expression(EXP) :- fail.

% apply_lambda_expression_of_one_argument(+LAMBDA_EXPRESSION,+ARGUMENT,-RESULTS)
% substitutes the argument given into the lambda expression, which should
% take just one argument, and returns this as the results.

% e.g., apply_lambda_expression_of_one_argument(X^[plus,X,1],3,RESULTS) 
% succeeds with RESULTS = [plus,3,1]

apply_lambda_expression_of_one_argument(LAMBDA_EXPRESSION,ARGUMENT,RESULTS) :-
	supply_argument(LAMBDA_EXPRESSION,ARGUMENT,RESULTS).

% apply_lambda_expression_of_two_arguments(+LAMBDA_EXPRESSION,+ARG1,+ARG2,-RESULTS)
% substitutes the two arguments given into the lambda expression, which should
% take exactly two arguments, and returns this as the results.

% e.g., apply_lambda_expression_of_two_arguments(X^Y^[expt,X,Y],3,5,RESULTS) 
% succeeds with RESULTS = [expt,3,5]

apply_lambda_expression_of_two_arguments(LAMBDA_EXPRESSION,ARG1,ARG2,RESULTS) :-
	supply_argument(LAMBDA_EXPRESSION,ARG1,INTERMEDIATE_RESULTS),
	supply_argument(INTERMEDIATE_RESULTS,ARG2,RESULTS).

% apply_lambda_expression_of_three_arguments(+LAMBDA_EXPRESSION,+ARG1,+ARG2,+ARG3,-RESULTS)
% substitutes the three arguments given into the lambda expression, which should
% take exactly three arguments, and returns this as the results.

% e.g., apply_lambda_expression_of_three_arguments(X^Y^Z^append(X,Y,Z),[a,b],[c,d],[a,b,c,d],RESULTS).
% succeeds with RESULTS = append([a,b],[c,d],[a,b,c,d])

apply_lambda_expression_of_three_arguments(LAMBDA_EXPRESSION,ARG1,ARG2,ARG3,FINAL_RESULTS) :-
	supply_argument(LAMBDA_EXPRESSION,ARG1,INTERMEDIATE_RESULTS),
	supply_argument(INTERMEDIATE_RESULTS,ARG2,PENULTIMATE_RESULTS),
	supply_argument(PENULTIMATE_RESULTS,ARG3,FINAL_RESULTS).	

% Rather than write lambda(X,lambda(Y,[lessThan,X,Y])) we
% adopt the notation suggested by Michael Covington in
% Natural Language Processing for Prolog Programmers. Instead,
% we would write that example as X^Y^[lessThan,X,Y].

% supply_argument(+Var^Form,+Arg,-Result).
% takes a lambda argument (Arg) and substitutes it
% into Form, returned the instantiated result.

% e.g., supply_argument(X^[green(X)],kermit,green(kermit)).

supply_argument(X^Form,X,Form).

% TOP-LEVEL READ-TRANSLATE LOOP

top_level :- write('Enter sentences or queries in CELT to translate, ending with a period or question mark.'),nl,
             write('Your input can span multiple lines. Exit the loop by entering ''quit.''. You can '),nl,
	     write('also find out if words are in the lexicon and what part of speech they are by typing the word then a period.'),nl,
             !,
	     top_level_loop.

top_level_loop :- repeat,write('>>'),read_sentence([],SentenceCharCodes),process_input(SentenceCharCodes).

process_input(SentenceCharCodes) :-
	sentenceToList(SentenceCharCodes,[Word],sentence),                  % Was the input only a single word? (command or lexicon lookup)
	!,
	(memberchk(Word,[quit,bye,halt,stop,abort]) -> write_ln('Goodbye'); % Succeed (and thus stop the top-level loop) for a 'quit'
	    lookup_word_in_lexicon(Word), fail).                            % Otherwise lookup the word in the lexicon but continue.

process_input(SentenceCharCodes) :- nl,eng2log(SentenceCharCodes),!,fail. % Intentionally fail to reinvoke the top-level loop's repeat.

process_input(SentenceCharCodes) :- write('Sorry, but the parse fails on that input.'),nl,fail. 

% read_sentence(-Sentence) reads one or more lines until
% a sentence delimiter is read (currently a period, question
% mark, or exclamation mark). It then immediately returns
% the characters read.

read_sentence(CharsSoFar,FinalSentence) :-
	get0(Char),handle_next_char(Char,CharsSoFar,FinalSentence).

handle_next_char(10,CharsSoFar,FinalSentence) :-
	% write('Skip newline'),nl,
	!,
	read_sentence(CharsSoFar,FinalSentence).

handle_next_char(Char,CharsSoFar,FinalSentence) :-
	end_of_sentence([Char],_),!,
	reverse([Char|CharsSoFar],FinalSentence).

handle_next_char(Char,CharsSoFar,FinalSentence) :-
	read_sentence([Char|CharsSoFar],FinalSentence).

% CHECKING THE LEXICON

% lookup_word_in_lexicon(+Word) returns all entries
% of word in lexicon to help a user diagnose why a parse
% may be failing.

lookup_word_in_lexicon(Word):-
	(predefined_word(Word) ->
	    FLAG = found,
	    format('~w is a predefined function word in CELT.~n',[Word]);true),
	(determiner_in_lexicon(Word,DefiniteOrNot2) ->
	    FLAG = found,
	    format('~w is a determiner in CELT.~n',[Word]);true),
	(noun_in_lexicon(Word,Type3,Gender3,Countability3,Number3,SUO_concept3,Synset_ID3) ->
	    FLAG = found,
	    format('~w is a common ~w noun in CELT.~n',[Word,Countability3]);true),
	(pronoun_in_lexicon(Word,Case4,Gender4,Type4) ->
	    FLAG = found,
	    format('~w is a pronoun in CELT.~n',[Word]);true),
	(relative_pronoun_in_lexicon(Word,Rel_Type5) ->
	    FLAG = found,
	    format('~w is a relative pronoun in CELT.~n',[Word]);true),
	(proper_noun_in_lexicon(Word,Type6,Gender6,Number6,SUO_constant6,SUO_type6,Synset_ID6) ->
	    FLAG = found,
	    format('~w is a proper noun in CELT.~n',[Word]);true),
	(adjective_in_lexicon(Word,Root7,Kind7,Grade7,SUO_concept7) ->
	    FLAG = found,
	    format('~w is an adjective in CELT.~n',[Word]);true),	    
	(preposition_in_lexicon(Word,Modification_type8,Noun_type8,SUO_concept8) ->
	    FLAG = found,
	    format('~w is a preposition in CELT.~n',[Word]);true),	    
	(verb_in_lexicon(Word,Root9,TransitivityList9,Number9,Kind_of_verb9,Event_or_state9,SUO_concept9,Synset_ID9) ->  % e.g., 'enters'
	    delete(TransitivityList9,no,Transitivities9),
	    FLAG = found,
	    format('~w [WordNet sense ~d] is a verb with root ~w and transitivies ~w in CELT.~n',[Word,Synset_ID9,Root9,Transitivities9]);
	    true),
	(verb_in_lexicon(_,Word,TransitivityList10,Number10,Kind_of_verb10,Event_or_state10,SUO_concept10,Synset_ID10) -> % e.g., 'enter'
	    delete(TransitivityList10,no,Transitivities10),
	    FLAG = found,
	    format('~w [WordNet sense ~d] is a verb root with transitivies ~w in CELT.~n',[Word,Synset_ID10,Transitivities10]);
	    true),	    
	(adverb_in_lexicon(Word,Modification_type11,Sigma_Concept11) ->
	    FLAG = found,
	    format('~w is an adverb in CELT.~n',[Word]);true),
	(not(FLAG == found) ->
	    format('~w was not found in the lexicon.~n',[Word]);true).

% is_KIF_var(+VAR) succeeds if var is a Prolog atom starting with a '?'
% such as '?x' or '?banana'. It fails otherwise.

is_KIF_var(VAR) :- atom(VAR),atom_chars(VAR,['?'|REST]).
