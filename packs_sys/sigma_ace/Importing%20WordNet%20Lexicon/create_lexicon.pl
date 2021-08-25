% File to test WordNet access in Prolog
% from processed Perl files.

% Initial input, the files mapped_nouns, and mapped_verbs,
% comes from running the Perl program WN2assoc2 on the
% annotated noun and verb files that Ian has produced.

% Annotated Noun.Dat --> Perl WN2assoc2 --> mapped_nouns.pl
% Annotated Verb.Dat --> Perl WN2assoc2 --> mapped_verbs.pl

% Next we can load this file and run load_all/0.
% Then...write_all_lexicon_files/0 writes all
% CELT lexicon files, one for proper nouns, one
% for common nouns, and one for verbs at present.

% some details...
%     noun_for_lexicon(Noun,Gloss,Sumo) can retrieve all nouns to add to the Celt lexicon.
%     verb_for_lexicon(Noun,Gloss,Sumo) can retrieve all verbs to add to the Celt lexicon.

% And the additional info we need to enter these terms can be provided ...

%    for verbs from...

%     root_to_present(Root,Present)
%           which determines the present indicative form of the verb given its root form,

%     transitivity(Root,Types)
%           which determines which types among [intransitive,transitive,ditransitive] apply

% and for nouns from...

%     celt_object_type(Noun,CELT_Type)
%           which determines whether a noun is a person, time, or object, as CELT needs for
%           determining how to interpret the adverbial prepositions 'in', 'at', and 'on'.

%     count_or_mass(Noun,Count_Or_Mass_Type)
%           which determines whether a noun is a count noun or not.

% DECLARATIONS

:-multifile sumo/6.
:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading CELT Lexicon Generator...'),nl.
:-write('use load_all/0 to load SUMO and WordNet files'),nl.
l:-write('use write_mapped_common_nouns_to_file(Max) to write out common nouns'),nl.
:-write('use write_mapped_proper_nouns_to_file(Max) to write out proper nouns'),nl.
:-write('use write_mappped_verbs_to_file(Max) to write out verbs'),nl.

% sumo(SUMO_concept, kind_of_relationship, WordNet_word, Synset_ID, Part_of_speech, Gloss).

% sumo('Breathing',synonymousExternalConcept,breathe,200001740 ,verb,
%      "draw air into, and expel out of, the lungs; 'I can breathe better when the air is clean' ").

% sumo('Physical',synonymousExternalConcept,entity,100001740 ,noun,
%      "anything having existence (living or nonliving)").

load_all :- consult([kif,infer_sumo,                                        % these are for loading the SUMO and reasoning about it
                     infer_wordnet,wn_s,wn_g,wn_hyp,wn_fr,wn_at,            % these are for loading WordNet and reasoning about it
                     mapped_nouns,mapped_verbs,                             % these are the mapped nouns and verbs from Perl,
		     verb_forms,transitivity,noun_types,                    % these are for mapping verb tenses, determining verb
                     mass_vs_count]).                                       % transitivity and determining noun types & countability,
                                                                            % CELT requires nouns to be one of (person, thing, or time).
% noun_for_lexicon(-WordNet_noun,-SUMO_Concept_Or_Concepts,-Synset_ID,+Proper_Or_Not).
% e.g., noun_for_lexicon(bird,Sumo,Synset,common).
% Proper_Or_Not is either 'proper' to retrieve proper nouns only or 'common' to retrieve common nouns only.

% Note: that this retrieves all word senses. In functions
% such as write_mapped_common_nouns_to_file/1 only the most
% frequent sense (usually sense 1) is listed, the others are
% included but commented out.

noun_for_lexicon(WordNet_noun,Sumo_Concept_Or_Concepts,Synset_ID,Proper_Or_Not) :-
  sumo(Sumo_Concept,Relationship,WordNet_noun,Synset_ID,noun,Gloss),
  (   Proper_Or_Not == proper -> is_proper_noun(WordNet_noun); not(is_proper_noun(WordNet_noun)) ),
  not(one_to_many_mapping(WordNet_noun,Synset_ID,Mappings)),
  get_all_mapped_concepts_for_noun(Sumo_Concept,WordNet_noun,Sumo_Concept_Or_Concepts,Synset_ID).

% one_to_many_mapping/3 is used below in get_all_mapped_concepts to keep track of synset IDs that map to multiple concepts
% e.g., there are 3 entries for 'man' with synset 107391044, this mapping is recorded with this assertion:
%    one_to_many_mapping(man, 107391044, ['FullyFormed', 'Human', 'Male']).
% so that the other mappings for the same synset ID will be passed over as they have already been taken into account.
:- dynamic one_to_many_mapping/3. 

% get_all_mapped_concepts_for_noun(Sumo_Concept,WordNet_noun,Sumo_Concept_Or_Concepts,Synset_ID)
% returns one or more SUMO concepts in a list for the variable Sumo_Concept_Or_Concepts. Typically,
% a WordNet noun will map to just one SUMO concept and the list returned will look like this: ['ColorAttribute']
% but sometimes a noun will map to multiple SUMO concepts (e.g., 'man') and return a list like this:
% ['Male','Human','FullyFormed'].

get_all_mapped_concepts_for_noun(Sumo_Concept,WordNet_noun,Sumo_Concept_Or_Concepts,Synset_ID) :-
  setof(Concept,Gloss^Relationship^sumo(Concept,Relationship,WordNet_noun,Synset_ID,noun,Gloss), % do not bind other vars just Concept
	All_Concepts),
  length(All_Concepts,N),
  !,
  ((N > 1) ->
      (format("~n% WordNet noun '~w', maps to multiple SUMO concepts: ~w.~n", [WordNet_noun,All_Concepts]),
       asserta(one_to_many_mapping(WordNet_noun,Synset_ID,All_Concepts)),
	Sumo_Concept_Or_Concepts = All_Concepts);
      Sumo_Concept_Or_Concepts = Sumo_Concept).

% clear_mappings removes the asserted one_to_many_mapping clauses generated during running noun_for_lexicon.
clear_mappings :- retractall(one_to_many_mapping(WordNet_noun,Synset_ID,All_Concepts)).

% most_frequent_noun_sense(+WordNet_noun,+Synset_ID,-Most_frequent_noun_sense,-Sense_Number)
% determines if WordNet_noun is the most frequent word sense among those nouns mapped
% so far. If it is sense 1 then it is. If it is not sense 1, then it is only if
% all other senses of the same word that *have* been mapped have sense numbers that
% are higher. Since not all of WordNet has been mapped yet, we need to ensure we
% only pull out the most frequent sense for the lexicon, not just the first listed.

% Note: WordNet only has frequencies for words in tagged texts. For words that
% do not appear we use the same ordering that WordNet does, which may not correspond
% to the frequency of the word sense in discourse.

% The difference between most_frequent_noun_sense1 and most_frequent_noun_sense is that the former
% expects multiple words to appear like this: 'life_form' whereas the latter expects them
% to appear with spaces instead of hyphens (e.g., 'life form').

most_frequent_noun_sense(WordNet_word,Synset_ID,Most_frequent_noun_sense,Sense_Number) :-
	concat_atom(List_Of_Words,' ',WordNet_word),                 % Separate out words separated by spaces
	concat_atom(List_Of_Words,'_',Hyphenated_WordNet_word),      % Put the word back together using hyphens instead
	most_frequent_noun_sense1(WordNet_word,Hyphenated_WordNet_word,Synset_ID,Most_frequent_noun_sense,Sense_Number).

% most_frequent_noun_sense1(+WordNet_verb,+Hyphenated_WordNet_word,+Synset_ID,-Most_frequent_noun_sense,-Sense_Number)
% determines if WordNet_verb is the most frequent sense among those mapped
% so far. If it is sense 1 then it is. If it is not sense 1, then it is only if
% all other senses of the same word that *have* been mapped have sense numbers that
% are higher. Since not all of WordNet has been mapped yet, we need to ensure we
% only pull out the most frequent sense for the lexicon, not just the first listed.

% if this is sense number one then we always answer 'yes' as there can be no more
% frequent occurrence if tagged data was used, and this is the preferred sense if
% tagged data was not used.
most_frequent_noun_sense1(WordNet_word,Hyphenated_WordNet_word,Synset_ID,yes,1) :-
	s(Synset_ID,_,Hyphenated_WordNet_word,n,1,_),!.

% if there is another more frequent noun word sense, then answer 'no' as to whether this one is the most frequent.
most_frequent_noun_sense1(WordNet_word,Hyphenated_WordNet_word,Synset_ID,no,Given_Sense_Number) :-
	s(Synset_ID,_,Hyphenated_WordNet_word,n,Given_Sense_Number,_),
        % there is another SUMO entry for the same word with a sense number that is less than the one given.
	sumo(Sumo,Relationship,WordNet_word,Other_Synset_ID,noun,Gloss),
	s(Other_Synset_ID,_,Hyphenated_WordNet_word,n,Other_Sense_Number,_),
	Other_Sense_Number < Given_Sense_Number,
	!.

% for more debugging info replace the last line with...
%	write('\% Synset ID '),write(Other_Synset_ID),
%	write(' is more frequent than '),write(Synset_ID),
%	write(' for noun: '),write(WordNet_word),write('.'),nl,
%	!.

% otherwise, if there is no more frequent noun, then this word sense is the most frequent noun sense, so answer 'yes'.
most_frequent_noun_sense1(WordNet_word,Hyphenated_WordNet_word,Synset_ID,yes,Given_Sense_Number) :-
	s(Synset_ID,_,Hyphenated_WordNet_word,n,Given_Sense_Number,_),!.

% this last clause is just to catch errors...
most_frequent_noun_sense1(WordNet_word,Hyphenated_WordNet_word,Synset_ID,no,0) :-
	write('WARNING: Could not find the synset for '),write(WordNet_word),write('!'),nl.

% not_most_frequent_nouns lists those common or proper nouns that are mapped but not the most frequent in the mapping.

not_most_frequent_nouns(Proper_Or_Not) :-
	noun_for_lexicon(WordNet_noun,Sumo,Synset_ID,Proper_Or_Not),
	most_frequent_noun_sense(WordNet_verb,Synset_ID,noun,Most_Frequent,Sense_Number),
	( (Most_Frequent == no) ->
	    (write('Verb '),write(WordNet_noun),write(' has WordNet sense # '),write(Sense_Number),
		write(' and a more frequent sense for that noun has already been mapped.'),nl);
	    true ),
	fail.

% unmapped_nouns list those nouns that are not yet mapped.

unmapped_nouns :-
  repeat,
  s(Synset_ID,_,WordNet_noun,_,1,_),
  \+ sumo(Sumo,Relationship,WordNet_noun,Synset_ID,noun,Gloss),
  write(WordNet_noun),write(' is not yet mapped. '),
  describe_synset(Synset_ID),nl,
  fail.

% unmapped_nouns lists all unmapped nouns to a file.

unmapped_nouns_to_file :- tell(unmapped_nouns),unmapped_nouns,told.



% verb_for_lexicon(-WordNet_verb,-Sumo_Concept_Or_Concepts,-Synset_ID).

% Note: compound verbs appear like this 'grow together'
% instead of hyphenated like grow_together, which is how
% they appear in wn_s.pl.

verb_for_lexicon(WordNet_verb,Sumo_Concept_Or_Concepts,Synset_ID) :-
  sumo(Sumo_Concept,Relationship,WordNet_verb,Synset_ID,verb,Gloss),
  not(one_to_many_mapping(WordNet_verb,Synset_ID,Mappings)),
  get_all_mapped_concepts_for_verb(Sumo_Concept,WordNet_verb,Sumo_Concept_Or_Concepts,Synset_ID).

% get_all_mapped_concepts_for_verb(Sumo_Concept,WordNet_verb,Sumo_Concept_Or_Concepts,Synset_ID)
% returns one or more SUMO concepts in a list for the variable Sumo_Concept_Or_Concepts. Typically,
% a WordNet verb will map to just one SUMO concept and the list returned will look like this: ['ColorAttribute']
% but sometimes a verb will map to multiple SUMO concepts (e.g., 'man') and return a list like this:
% ['Male','Human','FullyFormed'].

get_all_mapped_concepts_for_verb(Sumo_Concept,WordNet_verb,Sumo_Concept_Or_Concepts,Synset_ID) :-
  setof(Concept,Gloss^Relationship^sumo(Concept,Relationship,WordNet_verb,Synset_ID,verb,Gloss), % do not bind other vars just Concept
	All_Concepts),
  length(All_Concepts,N),
  !,
  ((N > 1) ->
      (format("~n% WordNet verb '~w', maps to multiple SUMO concepts: ~w.~n", [WordNet_verb,All_Concepts]),
       asserta(one_to_many_mapping(WordNet_noun,Synset_ID,All_Concepts)),
	Sumo_Concept_Or_Concepts = All_Concepts);
      Sumo_Concept_Or_Concepts = Sumo_Concept).

% unmapped_verbs lists all unmapped verbs.

unmapped_verbs :-
  repeat,
  s(Synset_ID,_,WordNet_verb,_,1,_),
  \+ sumo(Sumo,Relationship,WordNet_verb,Synset_ID,verb,Gloss),
  write(WordNet_verb),write(' is not yet mapped. '),
  describe_synset(Synset_ID),nl,
  fail.

% unmapped_verbs lists all unmapped verbs to a file.

unmapped_verbs_to_file :- tell(unmapped_verbs),unmapped_verbs,told.

% write_mapped_nouns_to_file writes mapped nouns to a file
% using this lexical entry template for nouns:

% noun_in_lexicon(Common_noun,Type,Gender,Countability,Number,SUO_concept,Synset_ID).

write_mapped_common_nouns_to_file(Max) :-
	% open common noun files
	tell(celt_common_noun_lexicon),
	flag(total_entries, _, 0),         % number of lines written, both lexicon entries and entries commented out
	flag(commented_out, _, 0),	   % lexicon entries that were commented out; word senses that are not the most frequent
	clear_mappings,                    % clear record of any one_to_many mapping clauses previously saved.
	write_all_noun_templates(Max,common),
	summarize_entries_written('common noun'),
	told.

% summarize_entries_written(+Kind_OF_Entries) writes out a summary like this:

% Wrote 2456 lines, including 2200 valid common noun entries for the lexicon,
% and 256 common noun entries that were commented out. The 10.4% of entries
% that were commented out were less frequent entries that would conflict with
% the more frequent lexical entries.

summarize_entries_written(Kind_Of_Entries) :-
	flag(total_entries, N, N),
	flag(commented_out, Omitted, Omitted),
	Valid is N - Omitted,
	(N > 0 -> Ratio_Suppressed is (Omitted / N) * 100; Ratio_Suppressed is 0),
	format("~n~n% Wrote ~D entries, including ~D valid ~w entries for the lexicon,~n",
	       [N,Valid,Kind_Of_Entries]),
	format("% and ~D entries that were commented out. The ~2f % of entries~n",
	       [Omitted,Ratio_Suppressed]),
	format("% that were commented out were less frequent word senses that would conflict with~n"),
	format("% more frequent word senses entered in the lexicon for the same word.~n").

write_mapped_proper_nouns_to_file(Max) :-
	% open proper noun files
	tell(celt_proper_noun_lexicon),
	flag(total_entries, _, 0),         % number of lines written, both lexicon entries and entries commented out
	flag(commented_out, _, 0),         % lexicon entries that were commented out; word senses that are not the most frequent
	clear_mappings,                    % clear record of any one_to_many mapping clauses previously saved.
	write_all_noun_templates(Max,proper),
	summarize_entries_written('proper noun'),
	told.

% write_all_lexicon_files writes all 3 lexicon files.

write_all_lexicon_files :- write_all_lexicon_files(1000000).

write_all_lexicon_files(Max) :-
	time(write_mapped_verbs_to_file(Max)),
	time(write_mapped_common_nouns_to_file(Max)),
	time(write_mapped_proper_nouns_to_file(Max)).

write_all_noun_templates(Max,Proper_Or_Not) :-
	noun_for_lexicon(WordNet_noun,SUMO_Concept,Synset_ID,Proper_Or_Not),
	Gender = neuter, Number = singular,

	% determine additional properties of verb entries required for lexicon
	celt_noun_type_from_SUMO(SUMO_Concept,Synset_ID,Noun_type),   % determine noun type, one of [person, object, time]

	% Split atoms containing multiple words like 'credit card' into lists like [credit,card].
	% Atoms with only one word like 'car' are not turned into lists.
	split_multiple_words_into_a_list(WordNet_noun,Noun_Word_Or_Words),	


	(   Proper_Or_Not == proper ->

		% proper nouns, include only if at least one word is capitalized.
		% proper_noun_in_lexicon(Name,Type,Gender,Number,SUO_constant,SUO_type,Synset_ID).

		(
		most_frequent_noun_sense(WordNet_noun,Synset_ID,Most_Frequent_Sense,Sense_Number),
		(Most_Frequent_Sense == no ->
		    (format("~n% Hiding sense ~w of proper noun ~w, synset ID ~w.~n% ",[Sense_Number,WordNet_noun,Synset_ID]),
			flag(commented_out, Omitted, Omitted+1));
		    true),
% To just skip those that are not the most frequent replace the above conditional with...
%        Most_Frequent_Sense == yes,
		  write('proper_noun_in_lexicon('),
		  writeq(Noun_Word_Or_Words),  % A proper noun, like 'Secretary of State'	
		  write(','),
		  write(Noun_type),
		  write(','),
		  write(Gender),
		  write(','),
		  write(Number),
		  write(','),
		  writeq(SUMO_Concept),
		  write(','),
                  create_constant_name(Noun_Word_Or_Words,Constant_Name),
		  writeq(Constant_Name),
		  write(','),
		  write(Synset_ID),
		  write(').'),nl,
		  flag(total_entries, N, N+1)
		);

                (
	        % common nouns, include only if no words are capitalized.
		can_be_mass_noun(WordNet_noun,Synset_ID,SUMO_Concept,Countability), % determine if 'mass' or 'count' as best possible.

		most_frequent_noun_sense(WordNet_noun,Synset_ID,Most_Frequent_Sense,Sense_Number),
		(Most_Frequent_Sense == no ->
		    (format("~n% Hiding sense ~w of common noun ~w, synset ID ~w.~n% ",[Sense_Number,WordNet_noun,Synset_ID]),
			flag(commented_out, Omitted, Omitted+1));
		    true),
% To just skip those that are not the most frequent replace the above conditional with...
%        Most_Frequent_Sense == yes,
		  write('noun_in_lexicon('),  % A common noun, like 'garden' or 'cat'
		  writeq(Noun_Word_Or_Words),	
		  write(','),
		  write(Noun_type),
		  write(','),
		  write(Gender),
		  write(','),
		  write(Countability),
		  write(','),
		  write(Number),
		  write(','),
		  writeq(SUMO_Concept),
		  write(','),
		  write(Synset_ID),
		  write(').'),nl,
		  flag(total_entries, N, N+1))
	),
	    

        % Succeed (and thus stop) when we have written out Max nouns
	Max is N+1.

write_all_noun_templates(Max,_). % Or succeed this way if we reach the end of file first.

% create_constant_name(+Noun_Word_Or_Words,-Constant_Name)
% creates a constant name for a WordNet proper noun.
% e.g., create_constant_name('Monday','Monday')
% e.g., create_constant_name(['Secretary',of,'State'],'Secretary_of_State').

create_constant_name(Noun_Word,Noun_Word) :- atom(Noun_Word),!.
create_constant_name(Noun_Words,Constant_Name) :- concat_atom(Noun_Words,'_',Constant_Name).

% verb_in_lexicon(Verb,Root,Transitivity,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID).
% Verb is the present indicative form (e.g., 'enters')
% Root is the root form (e.g., 'enter')
% Kind_of_verb is one of [simple,compound,phrasal,prepositional].
% Event_or_state is one of [event,state]

% write_mapped_verbs_to_file writes mapped verbs to a file
% using this lexical entry template for verbs:

write_mapped_verbs_to_file(Max) :-
	tell(celt_verb_lexicon),
	flag(total_entries, _, 0),         % number of lines written, both lexicon entries and entries commented out
	flag(commented_out, _, 0),         % lexicon entries that were commented out; word senses that are not the most frequent
	clear_mappings,                    % clear record of any one_to_many mapping clauses previously saved.
	write_all_verb_templates(Max),
	summarize_entries_written('verb'),
	told.

write_all_verb_templates(Max) :-
	verb_for_lexicon(WordNet_verb,SUMO_Concept,Synset_ID),

	Number = singular, Event_or_State = event, Kind_of_verb = simple, 

	% determine additional properties of verb entries required for lexicon,
	% such as present indicative, transitivity, whether an event or not, etc.
	root_form_to_present_indicative(WordNet_verb,Indicative),          % determine present indicative (e.g., 'tries' for 'try', etc.)
	transitivity(WordNet_verb,Synset_ID,Transitivities),               % list with 3 items: e.g.,[intransitive,transitive,no]
        most_frequent_verb_sense(WordNet_verb,Synset_ID,Transitivities,Most_Frequent_Sense,Sense_Number),
	determine_event_or_state(WordNet_verb,Synset_ID,Event_or_State),   % one of [event, state]

	% Split atoms containing multiple words like 'go crazy' into lists like [go,crazy].
	% Atoms with only one word like 'buy' are not turned into lists.
	split_multiple_words_into_a_list(Indicative,Indicative_Word_Or_Words),
	split_multiple_words_into_a_list(WordNet_verb,Base_Form_Word_Or_Words),	

	% determine additional properties of verb entries required for lexicon,
        % such as the kind of verb [simple, compound, phrasal, prepositional]
	kind_of_verb(Base_Form_Word_Or_Words,Kind_Of_Verb),

	(Most_Frequent_Sense == no ->
	    (format("~n% Hiding sense ~w of verb ~w, synset ID ~w.~n% ",[Sense_Number,WordNet_verb,Synset_ID]),
		flag(commented_out, Omitted, Omitted+1));
	    true),
	
% To just skip those that are not the most frequent replace the above conditional with...
%        Most_Frequent_Sense == yes,

	write('verb_in_lexicon('),
	writeq(Indicative_Word_Or_Words),
	write(','),
	writeq(Base_Form_Word_Or_Words),
	write(','),
	write(Transitivities), % e.g., [intransitive, transitive]
	write(','),
	write(Number),
	write(','),
	write(Event_or_State),
	write(','),
	write(Kind_Of_Verb),
	write(','),
	writeq(SUMO_Concept),
	write(','),
	write(Synset_ID),
	write(').'),nl,
	flag(total_entries, N, N+1),
	Max is N+1.           % this goal fails and we continue if we have not yet recorded Max entries.

write_all_verb_templates(Max).

% split_multiple_words_into_a_list(+Word,-Output)
% takes a single atom input, which can contain one
% or more words (e.g., 'bar' or 'go crazy') and returns
% either the original atom, if it only contains one word,
% (e.g., 'bar') or the list of words, if it contains more
% than one word (e.g., ['go','crazy']).

% e.g., split_multiple_words_into_a_list('go','go').
% e.g., split_multiple_words_into_a_list('go crazy',[go,crazy]).

split_multiple_words_into_a_list(Word,Output) :-
	divide_words(Word,List_Output),
	length(List_Output,LENGTH),
	( LENGTH == 1 -> List_Output = [Output]; Output = List_Output).

% divide_words(+Words,-List_Output)
% takes an atom containing one or more words and returns
% a list with one item for each word
% e.g., divide_words('buy the farm',[buy,the,farm]).

% Note: that concat_atom/3 can be used for the same functionality, too.

divide_words(Words,List_Output) :-
	atom_codes(Words,Words_String),
	divide_word_codes(Words_String,[],List_Output).

% divide_word_codes(+Word_Codes_Left,+Token_Chars_So_Far,-List_Output)
% takes a list of ASCII codes for the characters in an atom, plus
% the (reversed) list of characters in the current token as built up
% so far, and returns a list of all the words.
%   e.g.,
%     atom_codes('buy the farm',X),divide_word_codes(X,[],Y).
%   binds Y to [buy,the,farm].

divide_word_codes([],[],[]) :- !.

divide_word_codes([],Current_Token_Chars,[Current_Token]) :-
	!,
	reverse(Current_Token_Chars,Reversed_Token_Chars),
	atom_codes(Current_Token,Reversed_Token_Chars).

divide_word_codes([32|Word_Codes],Current_Token_Chars,[Current_Token|Output_From_Rest]) :-
	!,
	reverse(Current_Token_Chars,Reversed_Token_Chars),
	atom_codes(Current_Token,Reversed_Token_Chars),
	divide_word_codes(Word_Codes,[],Output_From_Rest).

divide_word_codes([Char|Word_Codes],[],Output) :-
	Char \== 32,
	!,
	divide_word_codes(Word_Codes,[Char],Output).

divide_word_codes([Char|Word_Codes],Token_Chars_So_Far,Output) :-
	Char \== 32,
	!,
	divide_word_codes(Word_Codes,[Char|Token_Chars_So_Far],Output).

% is_proper_noun(+Noun)
% succeeds if Noun is a capitalized word
% or contains any capitalized letter. Noun
% is given as an atom.

% e.g., is_proper_noun(cat) fails,
%       is_proper_noun('Delaware') succeeds,
%       is_proper_noun('Secretary of State') succeeds, etc.
is_proper_noun(Noun_Atom) :- atom_codes(Noun_Atom,Char_Codes), contains_uppercase_letter(Char_Codes).

% contains_uppercase_letter(+Char_Codes) succeeds if any char code is an uppercase letter (A-Z).
contains_uppercase_letter([Code|Rest]) :- code_type(Code,upper); contains_uppercase_letter(Rest).

% Character and string manipulation in SWI.

% Tests:

% alphabetic?
%              char_type('a',alnum).
% digit?
%              char_type('a',digit).

% To generate digits:
%              char_type(X,digit).
% To generate letters, both uppercase and lowercase:
%              char_type(X,alpha).

% To convert to uppercase:
%              char_type('a',lower(X)).

% To convert to lowercase:
%              char_type('A',upper(X)).

% To convert an atom (e.g., 'red') to capitalized (e.g., 'Red')
% e.g., call convert(red,X).

convert(In,Capitalized) :- atom_chars(In,[FirstChar|RestChars]),
	                   char_type(FirstChar,lower(CapitalizedChar)),
			   atom_chars(Capitalized,[CapitalizedChar|RestChars]).

% THE PROBLEM OF SHADOWING VERB SENSES
%
% The previous version of the WordNet lexicon importing files
% suppressed all word senses of a verb that had more frequent
% senses listed elsewhere.

% Unfortunately, this sometimes leads to certain transitivities of verbs being
% suppressed. For example, 'sing' had 3 senses and the most common was the transitive
% sense. The verb sense of 'sing' that was intransitive had a less common sense so
% was suppressed, as shown below...

% WordNet Sense Number: 3,
%        ID 200724908 verb_in_lexicon(sings,sing,[intransitive, transitive, no],singular,event,simple,'Music',200724908).
% WordNet Sense Number: 2,
%        ID 201183701 verb_in_lexicon(sings,sing,[intransitive, no, no],singular,event,simple,'Music',201183701).
% verb_in_lexicon(sings,sing,[no, transitive, no],singular,event,simple,'Music',201184598).

% ...what we really want is to allow both the last two senses since they do not conflict: one handles only transitive
% uses of 'sing' and the other only handles intransitive uses of 'sing'. Similarly, for 'cry' the entries under the
% old approach would provide the intransitive meaning but suppress the transitive meaning...

% verb_in_lexicon(cries,cry,[intransitive, no, no],singular,event,simple,'PhysiologicProcess',200046397).
% WordNet Sense Number: 7, ID 200046687 verb_in_lexicon(cries,cry,[no, transitive, no],singular,event,simple,'PhysiologicProcess',200046687).
% WordNet Sense Number: 4, ID 200659902 verb_in_lexicon(cries,cry,[no, transitive, no],singular,event,simple,'Disseminating',200659902).
% WordNet Sense Number: 6, ID 200666828 verb_in_lexicon(cries,cry,[intransitive, no, no],singular,event,simple,'RadiatingSound',200666828).
% WordNet Sense Number: 5, ID 200808268 verb_in_lexicon(cries,cry,[no, no, no],singular,event,simple,'Process',200808268).

% ...so what we really want, at least for verbs, is to suppress all word senses of
% a verb that have a more frequent sense listed elsewhere *that also has a non-null
% intersection of transitivities*. If they have a null intersection then they apply
% for different transitivities and there is no conflict so both can be present.

% so most_frequent_sense, which used to apply to both verbs and nouns was rewritten
% to most_frequent_noun_sense and most_frequent_verb_sense so the latter can take
% into account verb transitivities and the 'shadowing' that can occur.

% most_frequent_verb_sense(+WordNet_verb,+Synset_ID,+Transitivities,-Most_Frequent_Sense,-Sense_Number)
% determines if WordNet_verb is the most frequent sense among those mapped
% so far. If it is sense 1 then it is. If it is not sense 1, then it is only if
% all other senses of the same word that *have* been mapped have sense numbers that
% are higher. Since not all of WordNet has been mapped yet, we need to ensure we
% only pull out the most frequent sense for the lexicon, not just the first listed.

% Note: WordNet only has frequencies for words in tagged texts. For words that
% do not appear we use the same ordering that WordNet does, which may not correspond
% to the frequency of the word sense in discourse.

% The difference between most_frequent_verb_sense1 and most_frequent_verb_sense is that the former
% expects multiple words to appear like this: 'life_form' whereas the latter expects them
% to appear with spaces instead of hyphens (e.g., 'life form').

% Using data...
% WordNet Sense Number: 3,
%        ID 200724908 verb_in_lexicon(sings,sing,[intransitive, transitive, no],singular,event,simple,'Music',200724908).
% WordNet Sense Number: 2,
%        ID 201183701 verb_in_lexicon(sings,sing,[intransitive, no, no],singular,event,simple,'Music',201183701).
% verb_in_lexicon(sings,sing,[no, transitive, no],singular,event,simple,'Music',201184598).

% e.g., most_frequent_verb_sense(sing,201184598,[no,transitive,no],Yes_Or_No,N) should succeed
% with Yes_Or_No = 'yes' as this is the most frequent sense of 'sing' anyway.

% e.g., most_frequent_verb_sense(sing,200724908,[intransitive,transitive,no],Yes_Or_No,N) should succeed
% but with Yes_Or_No = 'no' since there is a more frequent sense of sing (201184598) that conflicts with it.

% e.g., most_frequent_verb_sense(sing,201183701,[intransitive,no,no],Yes_Or_No,N) should succeed but with Yes_Or_No = 'yes'
% there is a more frequent sense of 'sing' (ID 201184598), that verb is only transitive while this one
% is only intransitive.

most_frequent_verb_sense(WordNet_word,Synset_ID,Transitivities,Most_frequent_verb_sense,Sense_Number) :-
	concat_atom(List_Of_Words,' ',WordNet_word),                 % Separate out words separated by spaces
	concat_atom(List_Of_Words,'_',Hyphenated_WordNet_word),      % Put the word back together using hyphens instead
	most_frequent_verb_sense1(WordNet_word,Hyphenated_WordNet_word,Synset_ID,Transitivities,Most_frequent_verb_sense,Sense_Number),
	!.

% most_frequent_verb_sense1(+WordNet_verb,+Hyphenated_WordNet_word,+Synset_ID,+Transitivities,-Most_frequent_verb_sense,-Sense_Number)
% determines if WordNet_verb is the most frequent sense among those mapped
% so far. If it is sense 1 then it is. If it is not sense 1, then it is only if
% all other senses of the same word that *have* been mapped have sense numbers that
% are higher. Since not all of WordNet has been mapped yet, we need to ensure we
% only pull out the most frequent sense for the lexicon, not just the first listed.

% if this is sense number one then we always answer 'yes' as there can be no more
% frequent occurrence if tagged data was used, and this is the preferred sense if
% tagged data was not used.
most_frequent_verb_sense1(WordNet_word,Hyphenated_WordNet_word,Synset_ID,Transitivities,yes,1) :-
	s(Synset_ID,_,Hyphenated_WordNet_word,v,1,_),!.

% if there is another more frequent verb word sense, AND THERE IS A NON-NULL INTERSECTION OF
% TRANSITIVITIES BETWEEN THE SENSES, then answer 'no' as to whether this one is the most frequent
% verb (if there is no overlap in verb transitivities then there is no conflict and both could be
% entered into the lexicon).
most_frequent_verb_sense1(WordNet_word,Hyphenated_WordNet_word,Synset_ID,Transitivities,no,Given_Sense_Number) :-
	s(Synset_ID,_,Hyphenated_WordNet_word,v,Given_Sense_Number,_),
        % there is another SUMO entry for the same word with a sense number that is less than the one given.
	sumo(Sumo,Relationship,WordNet_word,Other_Synset_ID,verb,Gloss),
	s(Other_Synset_ID,_,Hyphenated_WordNet_word,v,Other_Sense_Number,_),
	Other_Sense_Number < Given_Sense_Number,
	% now check that the verb transitivities overlap, so that there would be a conflict...
	verb_transitivities_overlap(WordNet_word,Transitivities,Other_Synset_ID),
	!.

% for more debugging info replace the last line with...
%	write('\% Synset ID '),write(Other_Synset_ID),
%	write(' is more frequent than '),write(Synset_ID),
%	write(' for verb: '),write(WordNet_word),write('.'),nl,
%	!.

% otherwise, if there is no more frequent verb, then this word sense is the most frequent verb sense, so answer 'yes'.
most_frequent_verb_sense1(WordNet_word,Hyphenated_WordNet_word,Synset_ID,Transitivities,yes,Given_Sense_Number) :-
	s(Synset_ID,_,Hyphenated_WordNet_word,v,Given_Sense_Number,_),!.

% this last clause is just to catch errors...
most_frequent_verb_sense1(WordNet_word,Hyphenated_WordNet_word,Synset_ID,Transitivities,no,0) :-
	write('WARNING: Could not find the synset for '),write(WordNet_word),write('!'),nl.

% verb_transitivities_overlap(+WordNet_Verb,+Synset_ID,+Transitivities)
% succeeds if the transitivities for the word and synset given overlap those in the
% list Transitivities, which is a list of three items (e.g., [no,transitive,no] for
% a verb that is only transitive; or [intransitive,transitive,no] for a verb (sense) that
% can be used either an intransitive or transitive manner, but not as ditransitive.

% Using data...
% WordNet Sense Number: 3,
%        ID 200724908 verb_in_lexicon(sings,sing,[intransitive, transitive, no],singular,event,simple,'Music',200724908).
% WordNet Sense Number: 2,
%        ID 201183701 verb_in_lexicon(sings,sing,[intransitive, no, no],singular,event,simple,'Music',201183701).
% verb_in_lexicon(sings,sing,[no, transitive, no],singular,event,simple,'Music',201184598).

% e.g., verb_transitivities_overlap(sing,[no,transitive,no],201183701) should fail since
% the intransitive sense of 'sing', ID 201183701, does not overlap with [no,transitive,no],
% which indicates the transitive sense of 'sing'.

% e.g., verb_transitivities_overlap(sing,[no,transitive,no],200724908) should succeed since
% word sense ID has transitivities [intransitive, transitive, no] which overlaps with [no,transitive,no].

verb_transitivities_overlap(WordNet_Verb,Transitivities,Synset_ID) :-
	transitivity(WordNet_Verb,Synset_ID,Other_Transitivities), 
	!, % do not backtrack if the next fails...
	transitivities_overlap(Transitivities,Other_Transitivities).

% transitivities_overlap(+Transitivities1,+Transitivities2) succeeds
% if the only intersection between each of the two lists is something
% other than 'no', i.e., one of [intransitive,transitive,ditransitive].

% e.g., transitivities([no,transitive,no],[intransitive,no,no]) fails
% as there is no overlap; both verb entries can co-exist with no conflict.

% e.g., transitivities([intransitive,transitive,no],[intransitive,no,no]) succeeds
% as there is overlap; both verb entries cannot co-exist with conflict as both can be intransitive.

transitivities_overlap(Transitivities1,Transitivities2) :-
	% format('Comparing ~w to ~w.~n',[Transitivities1,Transitivities2]),
	intersection(Transitivities1,Transitivities2,Intersection),    % see what items are present in both lists
	% format('Intersection is ~w.~n',[Intersection]),
	delete(Intersection,no,Intersecting_Transitivities),           % strip out 'no' elements
	length(Intersecting_Transitivities,L),                         % all that is left are 
	!,                                                             % members in [intransitive,transitive,ditransitive]
	% format('Length of intersection is ~w.~n',[L]),
	L > 0.                                                         % transitivities overlap if there are any such elements

% not_most_frequent_verbs lists those verbs that are mapped but not the most frequent in the mapping.

not_most_frequent_verbs :-
	verb_for_lexicon(WordNet_verb,SUMO_Concept,Synset_ID),
	most_frequent_verb_sense(WordNet_verb,Synset_ID,verb,Most_Frequent,Sense_Number),
	( (Most_Frequent == no) ->
	    (write('Verb '),write(WordNet_verb),write(' has WordNet sense # '),write(Sense_Number),
		write(' and a more frequent sense for that verb has already been mapped.'),nl);
	    true ),
	fail.
