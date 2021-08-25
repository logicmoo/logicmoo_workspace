%-------------------------------
% CODE FOR GETTING ROOT FORMS OF WORDS
%       USING WORDNET's MORPHY
%              RULES
%-------------------------------

% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement
:- style_check(-atom).           % allows strings more than 5 lines long

% These are the rules used in WordNet's Morphy
%  (see /MAN/HTML/MORPHY.HTM in your WordNet directory)

% POS   Suffix  Ending 
% NOUN "s"      "" 
% NOUN "ses"    "s" 
% NOUN "xes"    "x" 
% NOUN "zes"    "z" 
% NOUN "ches"   "ch" 
% NOUN "shes"   "sh" 
% VERB "s"       "" 
% VERB "ies"     "y" 
% VERB "es"      "e" 
% VERB "es"      "" 
% VERB "ed"      "e" 
% VERB "ed"      "" 
% VERB "ing"     "e" 
% VERB "ing"     "" 
% ADJ "er"       "" 
% ADJ "est"      "" 
% ADJ "er"       "e" 
% ADJ "est"      "e" 

word_ending_rule(noun,[s],[]).
word_ending_rule(noun,[s,e,s],[s]).
word_ending_rule(noun,[x,e,s],[x]).
word_ending_rule(noun,[z,e,s],[z]).
word_ending_rule(noun,[c,h,e,s],[c,h]).
word_ending_rule(noun,[s,h,e,s],[s,h]).

word_ending_rule(verb,[s],[]).
word_ending_rule(verb,[i,e,s],[y]).
word_ending_rule(verb,[e,s],[e]).
word_ending_rule(verb,[e,s],[]).
word_ending_rule(verb,[e,d],[e]).
word_ending_rule(verb,[e,d],[]).
word_ending_rule(verb,[i,n,g],[e]).
word_ending_rule(verb,[i,n,g],[]).

word_ending_rule(adjective,[e,r],[]).
word_ending_rule(adjective,[e,s,t],[]).
word_ending_rule(adjective,[e,r],[e]).
word_ending_rule(adjective,[e,s,t],[e]).

% drop_ending(+Given,-Root,?PartOfSpeech)
% returns Root if there is a suffix rule that
% matches the ending of Given. Root is the word
% before the suffix transformation.

drop_ending(Given,Root,PartOfSpeech) :-
	atom_chars(Given,CharsOfGiven),
	word_ending_rule(PartOfSpeech,Suffix,RevisedSuffix),
	append(CharsOfGivenLessSuffix,Suffix,CharsOfGiven),
	append(CharsOfGivenLessSuffix,RevisedSuffix,RootChars),
	atom_chars(Root,RootChars).

% find_exception(+Given,-Root,?PartOfSpeech)
% returns Root if there is a pair in the exception list
% matches Given and Root. Root is the word before any
% suffix transformation.

find_exception(Given,Root,noun) :- noun_exception(Given,Root).
find_exception(Given,Root,verb) :- verb_exception(Given,Root).
find_exception(Given,Root,adjective)  :- adjective_exception(Given,Root).
find_exception(Given,Root,adverb)  :- adverb_exception(Given,Root).

% noun_in_lexicon(Common_noun,Type,Gender,Countability,Number,SUO_concept,Synset_ID).
% proper_noun_in_lexicon(+Name,-Type,-Gender,-Number,-SUO_type,-SUO_constant,-Synset_ID).
verify_root_in_lexicon(Root,noun) :-
	noun_in_lexicon(Root,Type,Gender,Countability,Number,SUO_concept,Synset_ID);
	proper_noun_in_lexicon(Root,Type,Gender,Number,SUO_type,SUO_constant,Synset_ID).

% verb_in_lexicon(Verb,Root,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID).
verify_root_in_lexicon(Root,verb) :- verb_in_lexicon(Verb,Root,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID).

% adjective_in_lexicon(Adjective,Root,Kind,Grade,SUO_concept)
verify_root_in_lexicon(Root,adjective) :- adjective_in_lexicon(Adjective,Root,Kind,Grade,SUO_concept).

% adverb(Adverb,Modification_type,Sigma_Concept).
verify_root_in_lexicon(Root,adverb) :- adverb(Root,Modification_type,Sigma_Concept).

% rewrite_words_to_root_forms(+List,-Rewritten,-Warnings,-Type_Of_Input)
% takes a list of words from a sentence, query, or np
% and rewrites words that are not in the lexicon, but whose
% root form is, as their root form. A warning is also generated
% for rewritten words. Words that are not in the lexicon and cannot
% be recognized as a variant form of a word in the lexicon are
% completely unrecognized and also generate warning messages.

% Type_Of_Input is either:
%   sentence
%   query
%   np

% e.g., rewrite_words_to_root_forms([foo],sentence,[foo],
%                    ["Warning: This word is not in CELT's lexicon: foo."]).
% e.g., rewrite_words_to_root_forms([boxes],sentence,[box],
%                    [Warning: Rewrote noun boxes to its singular form box. CELT only supports singular nouns at present."].
% e.g., rewrite_words_to_root_forms([zigzagged],sentence,[zigzag],
%                    [Warning: Rewrote verb zigzagged to its root form zigzag. CELT only supports present indicative at present.",
% e.g., rewrite_words_to_root_forms([does,'John',entering],query,Rewritten,Warnings).
% e.g., rewrite_words_to_root_forms([who,entering],query,Rewritten,Warnings).
% e.g., rewrite_words_to_root_forms(['John',entering,the,bank],sentence,Rewritten,Warnings).

% handles queries with 'does' specially as the verb form to use is the root form, whereas otherwise
% it is the present indicative. Other parts of speech are always rewritten to the root form.
rewrite_words_to_root_forms(In,query,Out,Warnings) :- 
	memberchk(does,In),
	!,
	rewrite_words_to_root_forms(In,query_with_does,Out,[],Warnings).

rewrite_words_to_root_forms(In,Type_Of_Input,Out,Warnings) :- rewrite_words_to_root_forms(In,Type_Of_Input,Out,[],Warnings).

% rewrite_words_to_root_forms(+List,+Type_Of_Input,-Rewritten,+Warnings_In,-Warnings_Out).

rewrite_words_to_root_forms([],Type_Of_Input,[],Warnings_In,Warnings_In) :- !.

% just skip over words already in the lexicon
rewrite_words_to_root_forms([Word|Rest],Type_Of_Input,[Word|Rewritten_Rest],Warnings_In,Warnings_Out) :-
	word_in_lexicon(Word),
	!,
	rewrite_words_to_root_forms(Rest,Type_Of_Input,Rewritten_Rest,Warnings_Modified,Warnings_Out).

rewrite_words_to_root_forms([First|Rest],Type_Of_Input,[Rewritten|Rewritten_Rest],Warnings_In,Warnings_Out) :-
	rewrite_word_to_root_forms(First,[verb,noun,adjective,adverb],
				   Type_Of_Input,Rewritten,Part_Of_Speech,Warnings_In,Warnings_Modified),
	rewrite_words_to_root_forms(Rest,Type_Of_Input,Rewritten_Rest,Warnings_Modified,Warnings_Out).

% rewrite_word_to_root_forms(+Word,+Filter,+Type_Of_Input,-Rewritten_Word,-Part_Of_Speech,+Warnings_In,-Warnings_Out)

% rewrites word, which has already checked and is not in the lexicon, to its base root form
% if it turns out to be a lexicon word to which a suffix has been added. Part_Of_Speech is the
% part of speech of the word rewritten, one of [noun,verb,adjective,adverb] and is a returned value.

% Filter is a list of acceptable parts of speech for the rewriting. It is restricted by location in
% sentence, the last word prior to this word, and the type of CELT sentence.

% First try predefined words and skip over them.

rewrite_word_to_root_forms(Word,Filter,Type_Of_Input,Word,unknown,Warnings_In,Warnings_In) :-
	predefined_word(Word),
	!.

% Next try the exception lists.

% Note: handle verbs specially as we rewrite them usually to their present indicative forms but sometimes to the root form.
% For queries like 'Does John entering the room?' rewrite to 'Does John enter the room?' use the verb base form.
rewrite_word_to_root_forms(Word,Filter,query_with_does,Root,verb,Warnings_In,[Warning_String|Warnings_In]) :-
	memberchk(verb,Filter),
	find_exception(Word,Root,verb),
	verb_in_lexicon(Verb,Root,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID),
	!,
	generate_lexicon_warning(Word,Root,verb,query_with_does,Warning_String).

% Note: handle verbs specially as we rewrite them usually to their present indicative forms but sometimes to the root form.
% For queries without 'does' like 'Who entering the room?' rewrite to 'Who enters the room?' use the verb present indicative form.
% Similarly for all sentences, like 'John entered the room.' rewrite to 'John enters the room.'
rewrite_word_to_root_forms(Word,Filter,Type_Of_Input,Verb,verb,Warnings_In,[Warning_String|Warnings_In]) :-
	memberchk(verb,Filter),
	memberchk(Type_Of_Input,[sentence,query]),
	find_exception(Word,Root,verb),
	verb_in_lexicon(Verb,Root,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID),
	!,
	generate_lexicon_warning(Word,Verb,verb,Type_Of_Input,Warning_String).

% ...also try word ending rules for verbs not in the exception list where we want present indicative form returned...
rewrite_word_to_root_forms(Word,Filter,Type_Of_Input,Verb,verb,Warnings_In,[Warning_String|Warnings_In]) :-
	memberchk(verb,Filter),
	memberchk(Type_Of_Input,[sentence,query]),
        drop_ending(Word,Root,verb), % otherwise return present indicative, not base form
	verb_in_lexicon(Verb,Root,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID),
	!,
	generate_lexicon_warning(Word,Verb,verb,Type_Of_Input,Warning_String).

% ...also cover the case where the wrong verb form was used, i.e., present indicative instead of root form
% e.g., 'Does the Zulu attacks the box?' should become 'Does the Zulu attack the box?'
rewrite_word_to_root_forms(Word,Filter,query_with_does,Verb,verb,Warnings_In,[Warning_String|Warnings_In]) :-
	memberchk(verb,Filter),
	verb_in_lexicon(Word,Verb,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID),
	!,
	generate_lexicon_warning(Word,Verb,verb,query_with_does,Warning_String).

% ...also cover the case where the wrong verb form was used, i.e., root form instead of present indicative
% e.g., 'the Zulu attack the box' should become 'the Zulu attacks the box' and for wh-queries,
% e.g., 'Which Zulu attack the box?' should become 'Which Zulu attacks the box?'
rewrite_word_to_root_forms(Word,Filter,Type_Of_Input,Verb,verb,Warnings_In,[Warning_String|Warnings_In]) :-
	memberchk(verb,Filter),
	memberchk(Type_Of_Input,[sentence,query]),
	verb_in_lexicon(Verb,Word,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID),
	!,
	generate_lexicon_warning(Word,Verb,verb,Type_Of_Input,Warning_String).

% Note: other parts of speech (nouns, adjectives, adverbs) are always rewritten to their root forms.
rewrite_word_to_root_forms(Word,Filter,Type_Of_Input,Root,PartOfSpeech,Warnings_In,[Warning_String|Warnings_In]) :-
	find_exception(Word,Root,PartOfSpeech),
	memberchk(PartOfSpeech,Filter),
	verify_root_in_lexicon(Root,PartOfSpeech),
	!,
	generate_lexicon_warning(Word,Root,PartOfSpeech,Type_Of_Input,Warning_String).

% If exception lists fail then try word ending rules
rewrite_word_to_root_forms(Word,Filter,Type_Of_Input,Root,PartOfSpeech,Warnings_In,[Warning_String|Warnings_In]) :-
        drop_ending(Word,Root,PartOfSpeech),
	memberchk(PartOfSpeech,Filter),
	verify_root_in_lexicon(Root,PartOfSpeech),
	!,
	generate_lexicon_warning(Word,Root,PartOfSpeech,Type_Of_Input,Warning_String).

% Provide special handling for undeclared proper nouns
rewrite_word_to_root_forms(Word,Filter,Type_Of_Input,Word,noun,Warnings_In,[Warning_String|Warnings_In]) :-
	memberchk(noun,Filter),
	is_capitalized(Word),
	!,
	dynamically_introduce_proper_noun(Word),
	generate_lexicon_warning(Word,Word,undeclared_proper_noun,Type_Of_Input,Warning_String).

% Provide special handling for proper nouns not capitalized properly
rewrite_word_to_root_forms(Word,Filter,Type_Of_Input,Capitalized_Word,noun,Warnings_In,[Warning_String|Warnings_In]) :-
	memberchk(noun,Filter),
	not_capitalized(Word),
	capitalize(Word,Capitalized_Word),
	proper_noun_in_lexicon(Capitalized_Word,Type,Gender,Number,SUO_type,SUO_constant,Synset_ID),
	!,
	generate_lexicon_warning(Word,Capitalized_Word,uncapitalized_proper_noun,Type_Of_Input,Warning_String).

% ...finally give up...
rewrite_word_to_root_forms(Word,Filter,Type_Of_Input,Word,unknown,Warnings_In,[Warning_String|Warnings_In]) :-
	!,
	sformat(Warning_String,"Warning: This word is not in CELT's lexicon: '~w'.",[Word]).

% generate_lexicon_warning(+Word,+Root,+PartOfSpeech,+Type_Of_Input,-Warning).

% generates a lexical warning that a word was rewritten to its root
% that is specific to the part of speech.

generate_lexicon_warning(Word,Root,undeclared_proper_noun,Type_Of_Input,Warning_String) :-
	!,
	sformat(Warning_String,"Warning: Interpreted (guessed) '~w' as a proper noun for a male person and added a lexicon entry for it.",
		[Word,Root]).

generate_lexicon_warning(Word,Capitalized_Word,uncapitalized_proper_noun,Type_Of_Input,Warning_String) :-
	!,
	capitalize(Word,Capitalized_Word),
	sformat(Warning_String,"Warning: Interpreted '~w' as the proper noun '~w' without capitalization so added it.",
		[Word,Capitalized_Word]).

generate_lexicon_warning(Word,Root,verb,query_with_does,Warning_String) :-
	!,
	sformat(Warning_String,"Warning: Rewrote verb '~w' to its root form '~w' as this is a query with the auxiliary 'does'.",
		[Word,Root]).

generate_lexicon_warning(Word,Root,verb,Type_Of_Input,Warning_String) :-
	!,
	sformat(Warning_String,"Warning: Rewrote verb '~w' to its present indicative form '~w'. CELT only supports present indicative at present.",
		[Word,Root]).


generate_lexicon_warning(Word,Root,noun,Type_Of_Input,Warning_String) :-
	!,
	sformat(Warning_String,"Warning: Rewrote noun '~w' to its singular form '~w'. CELT only supports singular nouns at present.",
		[Word,Root]).

generate_lexicon_warning(Word,Root,adjective,Type_Of_Input,Warning_String) :-
	!,
	sformat(Warning_String,"Warning: Rewrote adjective '~w' to its root form '~w'.",
		[Word,Root]).

generate_lexicon_warning(Word,Root,adverb,Type_Of_Input,Warning_String) :-
	!,
	sformat(Warning_String,"Warning: Rewrote adverb '~w' to its root form '~w'.",
		[Word,Root]).



