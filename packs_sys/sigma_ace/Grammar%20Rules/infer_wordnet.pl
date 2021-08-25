%-------------------------------
% CODE FOR USING WORDNET
%-------------------------------

% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

% DECLARATIONS

% :-multifile test/3.
:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

% CELT Version 2(b)--Interface to WordNet.

:-write('Loading CELT Interface to WordNet ...'),nl.

:-write('synonym/2 -- finds all synonyms of a word'),nl.

:-write('Call init/0 to load all WordNet files'),nl.

%-------------------------------
% CODE FOR WordNet Interface
%-------------------------------

% wn_s = syn sets
%    s(synset_id,w_num,'word',ss_type,sense_number,tag_state).
%       A s operator is present for every word sense in WordNet. In
%       wn_s.pl , w_num specifies the word number for word in the synset.

% wn_g = gloss
%    g(synset_id,'(gloss)').
%       The g operator specifies the gloss (parenthentical descriptions) for a synset.

% wn_ent = entails
%    ent(synset_id,synset_id).
%      The ent operator specifies that the second synset is an entailment
%      of first synset. This relation only holds for verbs.


% wn_hyp = hyponym

%    hyp(synset_id,synset_id).

%      The hyp operator specifies that the second synset is a hypernym of
%      the first synset. This relation holds for nouns and verbs. The
%      reflexive operator, hyponym, implies that the first synset is a
%      hyponym of the second synset.

% mm(synset_id,synset_id).

%     The mm operator specifies that the second synset is a member
%     meronym of the first synset. This relation only holds for nouns.
%     The reflexive operator, member holonym, can be implied.

% ms(synset_id,synset_id).

%      The ms operator specifies that the second synset is a substance
%      meronym of the first synset. This relation only holds for nouns.
%      The reflexive operator, substance holonym, can be implied.

% mp(synset_id,synset_id).

%     The mp operator specifies that the second synset is a part meronym
%     of the first synset. This relation only holds for nouns. The
%     reflexive operator, part holonym, can be implied.

% cs(synset_id,synset_id).

%      The cs operator specifies that the second synset is a cause of the
%      first synset. This relation only holds for verbs.

% fr(synset_id,f_num,w_num).

%      The fr operator specifies a generic sentence frame for one or all
%      words in a synset. The operator is defined only for verbs.

% at(synset_id,synset_id).

%     The at operator defines the attribute relation between noun and
%     adjective synset pairs in which the adjective is a value of the
%     noun. For each pair, both relations are listed (ie. each synset_id
%     is both a source and target).

% ant(synset_id,w_num,synset_id,w_num).

%     The ant operator specifies antonymous word s. This is a lexical
%     relation that holds for all syntactic categories. For each
%     antonymous pair, both relations are listed (ie. each
%     synset_id,w_num pair is both a source and target word.)

% synonyms(+Word,-Synonyms,-Gloss)
% each result is a set of synonyms for one word sense, as defined by the gloss.

synonyms(Word,Others,Gloss) :-
	rehyphenate_compound_words(Word,Hyphenated_Word),
	s(Synset_id,Word_sense,Hyphenated_Word,_,_,_),
	findall(Other,s(Synset_id,Other_word_sense,Other,_,_,_),Others),
	g(Synset_id,Gloss).


% describe_synset(+Synset_id)
% prints out a description of the meaning of synset_id

describe_synset(Synset_id) :-
	s(Synset_id,_,Word,_,_,_),
	findall(Other,s(Synset_id,Other_word_sense,Other,_,_,_),Others),
	g(Synset_id,Gloss),
        write('Synset id '),write(Synset_id),
	write(' = '),write(Others),write(' '),write(Gloss),!.

% describe_synsets(+Synsets)
% describe_synsets calls describe_synset repeatedly on each synset in the set given.
describe_synsets([]).
describe_synsets([First|Rest]) :- describe_synset(First),nl,describe_synsets(Rest).

% get_first_word_to_describe_synset(+Synset_id,-Word)
% gets a word that describes the synset.

get_first_word_to_describe_synset(Synset_id,Word) :-
	s(Synset_id,1,Word,_,_,_).

% get_all_words_to_describe_synset(+Synset_id,-Words)
% gets all the words that describes the synset.

get_all_words_to_describe_synset(Synset_id,Words) :- findall(Word,s(Synset_id,_,Word,_,_,_),Words).

% get_all_defs(+Word)
% gets all the definitions of a word

get_all_defs(Word) :-
	rehyphenate_compound_words(Word,Hyphenated_Word),
	get_next_def(Hyphenated_Word,1).

get_next_def(Word,Word_Sense) :-	
	s(Synset_id,Word_Sense,Word,_,_,_),
	describe_synset(Synset_id),nl,
	Next_Sense is Word_Sense + 1,
	get_next_def(Word,Next_Sense).

% get_all_verbs(+Word)
% gets all the definitions of a verb
% e.g., get_all_verbs(can).

get_all_verbs(Word) :-
	rehyphenate_compound_words(Word,Hyphenated_Word),
	get_next_verb(Hyphenated_Word,1).

get_next_verb(Word,Word_Sense_Number) :-	
	s(Synset_id,_,Word,v,Word_Sense_Number,_),!,
	describe_synset(Synset_id),nl,
	Next_Sense is Word_Sense_Number + 1,
	get_next_verb(Word,Next_Sense).

% get_all_nouns(+Word)
% gets all the definitions of a noun
% e.g., get_all_nouns(can).

get_all_nouns(Word) :-
	rehyphenate_compound_words(Word,Hyphenated_Word),
	get_next_noun(Hyphenated_Word,1).

get_next_noun(Word,Word_Sense_Number) :-	
	s(Synset_id,_,Word,n,Word_Sense_Number,_),!,
	describe_synset(Synset_id),nl,
	Next_Sense is Word_Sense_Number + 1,
	get_next_noun(Word,Next_Sense).

% get_all_adjectives(+Word)
% gets all the definitions of a adjective
% e.g., get_all_adjectives(red).

get_all_adjectives(Word) :-
	rehyphenate_compound_words(Word,Hyphenated_Word),
	get_next_adjective(Hyphenated_Word,1).

get_next_adjective(Word,Word_Sense_Number) :- % an adjective can have code either 's' or 'a'
	(s(Synset_id,_,Word,a,Word_Sense_Number,_);s(Synset_id,_,Word,s,Word_Sense_Number,_)),!,
	describe_synset(Synset_id),nl,
	Next_Sense is Word_Sense_Number + 1,
	get_next_adjective(Word,Next_Sense).

% get_all_adverbs(+Word)
% gets all the definitions of a adverb
% e.g., get_all_adverbs(pretty).

get_all_adverbs(Word) :-
	rehyphenate_compound_words(Word,Hyphenated_Word),
	get_next_adverb(Hyphenated_Word,1).

get_next_adverb(Word,Word_Sense_Number) :-	
	s(Synset_id,_,Word,r,Word_Sense_Number,_),!,
	describe_synset(Synset_id),nl,
	Next_Sense is Word_Sense_Number + 1,
	get_next_adverb(Word,Next_Sense).

% ancestors(+Synset_id)
% lists all ancestor word senses of a word sense

ancestors(Synset_id) :-
	describe_synset(Synset_id),nl,nl,
	hyp(Synset_id,Parent_Synset),
	ancestors(Parent_Synset).

% descendants(+Synset_id)
% lists all descendant word senses of a word sense

descendants(Synset_id) :-
	describe_synset(Synset_id),nl,nl,
	hyp(Child_Synset,Synset_id),
	descendants(Child_Synset).

% is_hypernym_of(+Synset,+Ancestor_Synset)
% succeeds if Synset is a the same as, or is
% a descendant of Ancestor_Synset.

% is_hypernym_of(201450677,201442173) succeeds
%   as 201450677 is to glimpse,
%   and 201442173 is to perceive.

% is_hypernym_of(201036035,201442173) fails
%   as 201036035 is to throw,
%   and 201442173 is to perceive.

is_hypernym_of(Synset,Synset) :- !.
is_hypernym_of(Synset,Ancestor_Synset) :-
	hyp(Synset,Parent_Synset),
	is_hypernym_of(Parent_Synset,Ancestor_Synset).

% is_hypernym_in(+Synset,+Set_of_Synsets)
% succeeds if Synset is a member of Set_of_Synsets,
% or if any ancestor of Synset is a member of Set_of_Synsets.

% e.g.,	is_hypernym_in(Synset,[110875931,110843624,110856821])
% to see if Synset is a kind of time as the set of Synsets refer
% to time units, time measures, time of day, etc.

% e.g., is_hypernym_in(110888065,[110875931,110843624,110856821])
% succeeds as 110888065 is the 'time' sense of 'twilight'.

% e.g., is_hypernym_in(105775435,[110875931,110843624,110856821])
% fails as 105775435 is an 'apple'.

is_hypernym_in(Synset,Set_of_Synsets) :-
	memberchk(Synset,Set_of_Synsets),!.
is_hypernym_in(Synset,Set_of_Synsets) :-
	hyp(Synset,Parent_Synset),
	is_hypernym_in(Parent_Synset,Set_of_Synsets).

% is_hypernym_careful(+Synset,+Ancestor_Synset,+Synsets_Investigated)
% succeeds if Synset is a the same as, or is
% a descendant of Ancestor_Synset, and is not a member of Synsets_Investigated.

% is_hypernym_careful is the same as is_hypernym_of
% but handles loops in WordNet hypernyms. So far this
% only happens in WordNet 1.6 with the word 'burdens'
% The other version, is_hypernym_of, should be faster
% but will loop if there are cycles in the database.

% is_hypernym_careful(201450677,201442173) succeeds
%   as 201450677 is to glimpse,
%   and 201442173 is to perceive.

% is_hypernym_careful(201036035,201442173) fails
%   as 201036035 is to throw,
%   and 201442173 is to perceive.

is_hypernym_careful(Synset,Synset,Synsets_Investigated) :- !.

is_hypernym_careful(Synset,Ancestor_Synset,Synsets_Investigated) :-
	not(memberchk(Synset,Synsets_Investigated)),
	hyp(Synset,Parent_Synset),
	is_hypernym_careful(Parent_Synset,Ancestor_Synset,[Synset|Synsets_Investigated]).

% is_hypernym_in_set_careful(+Synset,+Set_of_Synsets)
% succeeds if Synset is a member of Set_of_Synsets,
% or if any ancestor of Synset is a member of Set_of_Synsets.

% The only difference from is_hypernym_in is that the database
% can contain cycles and this function will not loop.

% e.g.,	is_hypernym_in_set_careful(Synset,[110875931,110843624,110856821])
% to see if Synset is a kind of time as the set of Synsets refer
% to time units, time measures, time of day, etc.

% e.g., is_hypernym_in_set_careful(110888065,[110875931,110843624,110856821])
% succeeds as 110888065 is the 'time' sense of 'twilight'.

% e.g., is_hypernym_in_set_careful(105775435,[110875931,110843624,110856821])
% fails as 105775435 is an 'apple'.

is_hypernym_in_set_careful(Synset,Set_of_Synsets,Synsets_Investigated) :-
	memberchk(Synset,Set_of_Synsets),!.
is_hypernym_in_set_careful(Synset,Set_of_Synsets,Synsets_Investigated) :-
	not(memberchk(Synset,Synsets_Investigated)),
	hyp(Synset,Parent_Synset),
	is_hypernym_in_set_careful(Parent_Synset,Set_of_Synsets,[Synset|Synsets_Investigated]).


% Unique beginners

% root(+X) succeeds if X is a synset that is a unique beginner, i.e., a root.
% and furthermore it is not an isolated node with no children.
root(Synset_ID) :- not(hyp(Synset_ID,_)).

has_child(Synset_ID) :- hyp(_,Synset_ID),!.

% root_noun(ID) succeeds if ID is a synset that is a noun that is also a root.
root_noun(Synset_ID) :-
	s(Synset_ID,_,Word,n,_,_),
        root(Synset_ID),
	has_child(Synset_ID).

% all_root_nouns returns the list of synset IDs for all unique beginners for nouns.
all_root_nouns(Roots) :- setof(X,root_noun(X),Roots).

% describe_root_nouns returns the list of synset IDs for all unique beginners for nouns.
describe_root_nouns :- all_root_nouns(Roots),describe_synsets(Roots).

% root_verb(ID) succeeds if ID is a synset that is a verb that is also a root.
root_verb(Synset_ID) :-
	s(Synset_ID,_,Word,v,_,_),
        root(Synset_ID),
	has_child(Synset_ID).

% all_root_verbs returns the list of synset IDs for all unique begiiners for verbs.
all_root_verbs(Roots) :- setof(X,root_verb(X),Roots).

% describe_root_verbs returns the list of synset IDs for all unique begiiners for verbs.
describe_root_verbs :- all_root_verbs(Roots),describe_synsets(Roots).

% find_root climbs the hypernyms until the root is found, works for either noun or verb Synsets.

%    hyp(synset_id,synset_id).

%      The hyp operator specifies that the second synset is a hypernym of
%      the first synset. This relation holds for nouns and verbs. The
%      reflexive operator, hyponym, implies that the first synset is a
%      hyponym of the second synset.

% e.g., find_root(201013332) to find the root of the verb 'barricade'.

find_root(Synset_ID) :-
	describe_synset(Synset_ID),nl,
	hyp(Synset_ID,Parent_Synset),
	find_root(Parent_Synset).

find_root(Synset_ID) :-
	write('Root Synset: '),write(Synset_ID).

% list_word_sense(+Word,+Part_Of_Speech,-Synset)
% list_word_sense lists the senses of a word and lets the user select
% one sense.
% 
% Part_Of_Speech is v, for verbs,
%                   n, for nouns,
%                   a, or s, for adjectives (try both)
%                   r, for adverbs
%
% E.g., list_word_sense(bar,v,Synset) for verb senses of 'bar', ordered by frequency
% E.g., list_word_sense(bar,n,Synset) for noun senses of 'bar', ordered by frequency

list_word_sense(Word,Part_Of_Speech,Synset) :-
	rehyphenate_compound_words(Word,Hyphenated_Word),
	findall(Synset_ID,s(Synset_ID,_,Hyphenated_Word,Part_Of_Speech,_,_),Senses),
	length(Senses,N),!,
	nl,write(Senses),nl,
	write('The word '),write(Word),write(' has '),write(N),write(' senses.'),nl,
	describe_word_senses(Hyphenated_Word,Part_Of_Speech,0,N).

describe_word_senses(Word,Part_Of_Speech,Max,Max) :- !.

describe_word_senses(Word,Part_Of_Speech,I,Max) :-
	I < Max,!,
	Next_Word_Sense_Number is I + 1,
	write('Sense '),write(Next_Word_Sense_Number),write(' of '),write(Word),write(': '),
	s(Synset_ID,Word_Number,Word,Part_Of_Speech,Next_Word_Sense_Number,Tag_State),
	describe_synset(Synset_ID),nl,!,
	describe_word_senses(Word,Part_Of_Speech,Next_Word_Sense_Number,Max).

% find_part_of_speech(+Synset_ID,-Part_Of_Speech) returns the part of speech [verb, noun, adjective, adverb]
% for a given WordNet synset ID.

% e.g., find_part_of_speech(201212998,verb).
% e.g., find_part_of_speech(100014887,noun).

find_part_of_speech(Synset_ID,Part_Of_Speech) :-
	string_to_atom(Synset_ID,ID_as_Atom),
	sub_atom(ID_as_Atom,0,1,After_Chars,Subatom),
	word_net_prefix_to_part_of_speech(Subatom,Part_Of_Speech).

word_net_prefix_to_part_of_speech('1',noun).
word_net_prefix_to_part_of_speech('2',verb).
word_net_prefix_to_part_of_speech('3',adjective).
word_net_prefix_to_part_of_speech('4',adverb).

% doublecheck_synset_ID(+Synset_ID,+Word)
% succeeds if there is a synset ID with the
% number given and word given. This is a double
% check for transcription errors in case the
% wrong number was typed, which would cause
% problems in other predicates that are easier
% to catch here.

doublecheck_synset_ID(Synset_ID,Word) :-
	rehyphenate_compound_words(Word,Hyphenated_Word),
	s(Synset_ID,_,Hyphenated_Word,_,_,_).

% find_gloss(+Synset_ID,-Gloss)
% returns the gloss of a WordNet synset ID.

find_gloss(Synset_ID,Gloss) :- g(Synset_ID,Gloss),!.

% find_short_gloss(+Synset_ID,-Gloss)
% returns the gloss of a WordNet synset ID, without
% its surrounding parentheses.

find_short_gloss(Synset_ID,Short_Gloss) :-
	g(Synset_ID,Gloss),!,
	string_length(Gloss,Length),
	sub_atom(Gloss,1,Minus2,1,Short_Gloss).

% Examples of the two word entries for the synset ID 104506767,
% both of these are synonyms for that synset (words 1 and 2).
% The 1 at the end means the data is from tagged text and
% synset 104506767 is the first meaning for both the noun
% 'abstraction' and the noun 'abstract'.
% s(104506767,1,'abstraction',n,1,1).
% s(104506767,2,'abstract',n,1,1).

% find_sense_number(+Word,+Synset_ID,-Word_Sense_Number)
% finds the WordNet sense number of this word meaning.
find_sense_number(Word,Synset_ID,Word_Sense_Number) :-
	rehyphenate_compound_words(Word,Hyphenated_Word),
	s(Synset_ID,_,Hyphenated_Word,_,Word_Sense_Number,_).

% find_total_number_of_senses(+Word,+Part_Of_Speech,-Total_Senses) finds
% all the senses for a word in a particular part of
% speech (n,v,s,r) for noun, verb, adjective, or adverb.

% Note: 'a' is also used for some adjectives so this should
% only be used for nouns, verbs, or adverbs for the time being.

% e.g., find_total_number_of_senses(abstract,s,N) binds N to 4
% since 'abstract' has 4 senses as an adjective in WordNet 1.6.

find_total_number_of_senses(Word,Part_Of_Speech,Total_Senses) :-
	rehyphenate_compound_words(Word,Hyphenated_Word),
	findall(Word_Sense_Number,s(Synset_ID,Synoynm_Number,Hyphenated_Word,Part_Of_Speech,Word_Sense_Number,Tagged_Or_Not),
		Word_Sense_Numbers),
	length(Word_Sense_Numbers,Total_Senses).

% find_all_synsets(+Word,+Part_Of_Speech) is a utility for listing all the word sets
% of a word given its part of speech.

% e.g., find_all_synsets(abstract,s) lists all adjective senses of 'abstract'.

find_all_synsets(Word,Part_Of_Speech) :-
	rehyphenate_compound_words(Word,Hyphenated_Word),
	s(Synset_ID,Synoynm_Number,Hyphenated_Word,Part_Of_Speech,Word_Sense_Number,Tagged_Or_Not),
	write('Synset '),write(Synset_ID),write(' provides meaning '),write(Word_Sense_Number),
	write(' for '),write(Word),nl,
	fail.

% wordnet_letter_for_tag(?Part_Of_Speech_Tag,?Part_Of_Speech_Abbreviation)
% allows translation between the WordNet abbreviations and their full tags.

wordnet_letter_for_tag(verb,v).
wordnet_letter_for_tag(noun,n).
wordnet_letter_for_tag(adjective,s).
wordnet_letter_for_tag(adjective,a). % ?? How do these differ ??
wordnet_letter_for_tag(adverb,r).

