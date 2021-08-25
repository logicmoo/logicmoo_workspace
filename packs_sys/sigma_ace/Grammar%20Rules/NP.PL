%-------------------------------
% CELT NOUN PHRASES TRANSLATED USING GULP
%-------------------------------

% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

% Test parsing with test_all_NP to test stored parse tests.
% Test generation of noun phrases with a specified length and
% number of words with test_gen_1 ... to test_gen_all.

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading CELT Parser Version 2(b) ...noun phrase rules...syntactic and semantic translation'),nl.

%-------------------------------
% DCG RULES FOR ACE NOUN PHRASES
%-------------------------------

% FEATURES used in DCG rules

/*

syn,    Syntactic Features--

case,   is the case of a noun phrase, one of [nominative,accusative]
gender, is the gender of a noun phrase, one of [male,female]. Use _ if neuter.
act,    is the action of a predicate, one of the verb roots in the lexicon.
vcat,   is the verb category, either 'copula' or a 3-element list like [intransitive,transitive,ditransitive] or [no,transitive,no]
gcat,   is the gap category, either 'empty' if there is none, or one of the same categories as for nouns [person,thing,time]
gap,    is the relative pronoun that signals the gap construction (e.g., 'who' in a relative sentences; e.g., 'what' in a query)
rel,    is whether or not the grammatical category is inside a relative sentence or not, one of [yes,no] *
max,    is the maximum number of repeated consecutive structures, which currently only applies to adjuncts.
ncat,   is the noun category for the head noun in a noun phrase, one of [person,thing,time].
advp,   is the adverbial preposition or adverb used in an adjunct (e.g., 'inside' in 'inside the house', or 'slowly' in 'slowly').
role,   is the role this speech act plays, one of [assertion, query, command] for sentences, questions, or imperatives respectively.
det,    is the determiner for the head of this noun phrase (e.g., the, a, which, what...)
aux,    is 'yes' if an auxiliary such as 'does' is present, and 'no', if no axiliary is present
qvars,  is a list of query variables to be found in a query (e.g., [?who,?what]). It is [] for assertions and yes/no queries.
reply,  is the full-sentence reply for a query using the qvars as place holders for answers to be determined

* - regardless of whether a gap can be used in the construction, instead this just determines whether a relative sentence
may be embedded inside this structure, if rel is 'yes' then the construction is already inside a relative sentence and another
may not be used. If rel is 'no' then the construction is top-level or otherwise not inside a relative sentence and may allow
one to be used internally, i.e., we know that it will not be doubly-embedded. E.g., rel can be 'yes' inside a predicate inside
a relative sentence even if the gap construction is to be filled in elsewhere, i.e., in the subject, as in this sentence: 'The
boy sees who gives the book to the man.' where the gap 'who' is used in the embedded sentence 'gives the book to the man' in
the subject position, so the gap is empty for the predicate 'gives the book to the man' even though rel is 'yes'.

sem,    Semantic Features--

 of predicates,

pred, --see verb phrases--    
subj, --see verb phrases--       
dobj, --see verb phrases--        
iobj, --see verb phrases--        
adjs, --see adjuncts--        
mod,  used as in NPs, for an adjective in a copula, e.g., 'is wealthy'

 [N.B. sentences have the same semantic features as predicates but predicates may have some features missing initially]

 of verb phrases,

pred,     is the SUO name of the action in the verb phrase
subj,     is the subject, agent, or experiencer (first object) of the verb
dobj,     is the direct object (second object) of the verb, or 'empty' if there is none.
iobj,     is the indirect object (second object) of the verb, or 'empty' if there is none.
id,       is the WordNet Synset ID for the action in the verb phrase

N.B. except for the last each of these arguments is the feature value package of a complement.

 of noun phrase complements,

noun,       is the actual noun that is accepted as the main noun in the noun phrase (e.g., 'man')
head,       is the variable name created for the primary object referred to in this complement (e.g., '?Man')
type,       is the Sigma concept name for the type of thing which the head is (e.g., 'Man')
id,         is the WordNet Synset ID for the noun that was used to determine the Sigma concept name for the type
mod,        is the Sigma name for a modifier (adjective) that applies to the head
sub,        is the sentence semantics of a relative sentence that modifies the head ('sub' for subordinate sentence).
poss,       is the Sigma Concept name for the type of object in the possessive position for the head (e.g., 'John' in 'John's dog')
owner,      is the WordNet Synset ID for the object that is in the possessive (owner) position for the head (e.g., 'John' in 'John's dog')
of,         is the Sigma Concept name for the type of object in the of-preposition positions of the head (e.g., 'John' in 'dog of John')
of_id,      is the WordNet Synset ID for the object that is in the of-preposition positions of the head (e.g., 'John' in 'dog of John')
quan,       is either 'universal' or (more commonly) 'existential' depending on the determiner that modifies the head
apos,       is a common noun used in aposition to the proper noun that is the head of this NP.

 of adjuncts,

adjs,       is a (possibly empty) list of adjuncts (either adverbs or adverbial prepositional phrases)

 for each adjunct,

adv,        is the Sigma concept name for the adverb in the adjunct, if there is one, or empty, if not
prep,       is the adverbial preposition if there is no adverb
aobj,       is the noun phrase part of the adverbial preposition, aobj stands for adverbial object

Convention: an unknown feature value is the same name as the feature,
only in all caps. E.g., CASE is the unknown value of the 'case'
feature in act<->be..case<->CASE. Also, when the features of a
category are unknown, or represented by a variable, they are
represented by the category name in title case, e.g., 'Sentence' for
the features of a sentence,and 'Relative_Sentence' for a
relative_sentence, etc. If the category is a common abbreviation, like
np, then all caps can be used, so 'NP' would be used for the features
of an np.

*/

%-------------------------------
% NOTE: ALL nouns and determiners
% are singular as per the ACE spec.
%-------------------------------

% pronouns
np(NP) -->
	{ NP = syn<->(det<->empty..case<->CASE..gender<->GENDER..ncat<->NCAT..gcat<->empty..reply<->[Pronoun]) ..
	       sem<->(noun<->NOUN..head<->HEAD..type<->SUO_concept),
	  Pronoun = syn<->(case<->CASE..gender<->GENDER) ..
	            sem<->(noun<->NOUN..head<->HEAD..type<->SUO_concept) },
	pronoun(Pronoun).             % he...she...him...her...it...

% proper nouns
np(NP) -->
	{ NP = syn<->(det<->empty..case<->CASE..gender<->GENDER..ncat<->NCAT..gcat<->empty..rel<->INSIDE_RELATIVE..reply<->[NOUN]) ..
	       sem<->(noun<->NOUN..head<->HEAD..type<->SUO_concept..id<->Synset_ID..
                      poss<->OWNER..owner<->OWNER_ID..
                      of<->OF..of_id<->OF_ID..
		      sub<->SUBORDINATE_SEMANTICS),
	  Noun = syn<->(ncat<->NCAT) ..
	         sem<->(noun<->NOUN..head<->HEAD..type<->SUO_concept..id<->Synset_ID..
		       poss<->OWNER..owner<->OWNER_ID..
		       of<->OF..of_id<->OF_ID),
	  Relative_Sentence = syn<->(gcat<->NCAT..gap<->GAP..rel<->INSIDE_RELATIVE)..sem<->(SUBORDINATE_SEMANTICS)
	  },	  
	(determiner(DET); ( {DET = 'empty'}, empty) ),           % we allow determiners here, e.g., 'he arrives in the United States'
	(proper_noun(Noun) ; possessive1(Noun)),                 % add in parameters for owner and ownee if present
	(of_prep_phrase(OF_PHRASE), { Noun = OF_PHRASE }; empty),% add in parameters for owner in of-prep phrase if present
	(relative_sentence(Relative_Sentence); { SUBORDINATE_SEMANTICS = 'empty'}, empty).

% common nouns
np(NP) -->
	{ NP = syn<->(det<->DET..case<->CASE..gender<->GENDER..ncat<->NCAT..
		     gcat<->empty..rel<->INSIDE_RELATIVE..reply<->REPLY) ..
	       sem<->(noun<->NOUN..head<->HEAD..type<->SUO_concept..mod<->MODIFIER..id<->Synset_ID..
		       poss<->OWNER..owner<->OWNER_ID..
		       of<->OF..of_id<->OF_ID..
		      sub<->SUBORDINATE_SEMANTICS),
	  Noun = syn<->(ncat<->NCAT..count<->COUNT) ..
	         sem<->(noun<->NOUN..head<->HEAD..type<->SUO_concept..id<->Synset_ID..
		       poss<->OWNER..owner<->OWNER_ID..
		       of<->OF..of_id<->OF_ID),
	  Relative_Sentence = syn<->(gcat<->NCAT..gap<->GAP..rel<->INSIDE_RELATIVE)..sem<->(SUBORDINATE_SEMANTICS)
	  },	  
        % we allow the determiner to be omitted, but ONLY for mass nouns: 'he gave money to the thief'
	(determiner(DET); ( {COUNT = mass, DET = 'empty'}, empty) ),           
	allow_modifier(MODIFIER,MODIFIER_REPLY),                     % allow one optional adjective, but do not require it
	(common_noun(Noun); apposition(Noun); possessive2(Noun)),
	(of_prep_phrase(OF_PHRASE), { Noun = OF_PHRASE }; empty),    % add in parameters for owner in of-prep phrase if present
	(relative_sentence(Relative_Sentence); { SUBORDINATE_SEMANTICS = 'empty'}, empty),
        % for queries construct the reply from all the parts parsed above, except currently we do not use of-phrases or subordinates.
	{ construct_reply(DET,MODIFIER_REPLY,NOUN,REPLY) }.

% allow_modifier(-MODIFIER,-MODIFIER_REPLY)
% allows but does not require a modifier to be parsed. If one is parsed
% then MODIFIER is set to the SUMO concept for the modifier and the actual
% modifer, the word itself, is returned as MODIFIER_REPLY, which can be
% used in queries in constructing the query reply.

allow_modifier(MOD,MODIFIER_REPLY) -->
	modifier(Modifier),
	{ Modifier = sem<->(mod<->MOD)..syn<->(reply<->MODIFIER_REPLY) }.

allow_modifier(empty,[]) --> empty.

% construct_reply(+DET,+MODIFIER_REPLY,+NOUN,-REPLY)
% constructs a reply like [the,green,dog] from the parameters given.
construct_reply(DET,MODIFIER_REPLY,NOUN,REPLY) :-
	(DET \== empty -> REPLY1 = [DET]; REPLY1 = []),
	(MODIFIER_REPLY \== [] -> REPLY2 = [MODIFIER_REPLY|REPLY1]; REPLY2 = REPLY1),
	REPLY3 = [NOUN|REPLY2],
	reverse(REPLY3,REPLY).
	
% special case: the 'gap' grammatical feature can be used to replace a missing NP in a relative sentence
% E.g., ...N.B. parses *nothing* but uses up the gap feature, can only happen if gap feature is not empty...
% Note: the rel feature must be 'yes' since a gap can only be used up inside relative sentences as this version
% does not yet handle queries.
np(NP) -->
	{ NP = syn<->(det<->empty..case<->CASE..gender<->GENDER..ncat<->GCAT..gcat<->GCAT..gap<->GAP..rel<->yes..reply<->[Sigma_Name]) ..
	       sem<->(head<->Sigma_Name..type<->SUO_concept),
	       not(var(GCAT)), not(GCAT=empty),
	       create_var_name(GAP,Sigma_Name),
               map_gcat_to_suo(GCAT,SUO_concept) },
	empty.                                               % Nothing is parsed! Instead, use the gap construct to fill in the NP.

% (optional) adjective modifiers (only one allowed, ACE does not allow these to be strung together)
% E.g., ...valid...Swiss...new...fastest...most expensive...
% Note: MODIFIER is the SUO name of the modifier.
modifier(MODIFIER) -->
	adjective(MODIFIER); superlative(MODIFIER).

% possessives

% Proper noun, not requiring a determiner preceding it.
% E.g., ...John's card...Mary's bank...
possessive1(Noun) -->
	proper_noun(Owner),
	apostrophe_s,
	common_noun(Possession),
	{ 
          Owner = sem<->(head<->OWNER_HEAD..type<->OWNER_TYPE..id<->OWNER_ID),
	  Possession = sem<->(noun<->OBJECT..head<->POSS_HEAD..type<->POSS_TYPE..id<->POSS_ID),
	  Noun = sem<->(noun<->OBJECT..head<->POSS_HEAD..type<->POSS_TYPE..id<->POSS_ID..         % first describe what is owned
		       poss<->OWNER_HEAD..owner<->OWNER_ID)                                       % then add descriptions of the owner
	}.
	
% Common noun, requiring a determiner preceding it.
% E.g., ...bank's card...customer's bank...machine's slot...
possessive2(Noun) -->
	common_noun(Owner),
	apostrophe_s,
	common_noun(Possession),
	{ 
          Owner = sem<->(head<->OWNER_HEAD..type<->OWNER_TYPE..id<->OWNER_ID),
	  Possession = sem<->(noun<->OBJECT..head<->POSS_HEAD..type<->POSS_TYPE..id<->POSS_ID),
	  Noun = sem<->(noun<->OBJECT..head<->POSS_HEAD..type<->POSS_TYPE..id<->POSS_ID..         % first describe what is owned
		       poss<->OWNER_HEAD..owner<->OWNER_ID)                                       % then add descriptions of the owner
	}.

% prepositional phrase used to modify a noun, ACE requires that it begins with 'of'

% E.g., ...of Mary...of John...
of_prep_phrase(OF_PHRASE) -->
	[of],
	proper_noun(NOUN),
	{
	 NOUN = sem<->(head<->POSS_HEAD..type<->POSS_TYPE..id<->POSS_ID),
	 OF_PHRASE = syn<->ANY..sem<->(of<->POSS_TYPE..of_id<->POSS_ID)
	}.

% E.g., ...of the bank...of the customer...
of_prep_phrase(OF_PHRASE) -->
	[of],
	determiner(DET),
	common_noun(NOUN),
	{
	 NOUN = sem<->(head<->POSS_HEAD..type<->POSS_TYPE..id<->POSS_ID),
	 OF_PHRASE = syn<->ANY..sem<->(of<->POSS_TYPE..of_id<->POSS_ID)
	}.	

% apostrophe followed by s
% E.g., ...'s...
apostrophe_s --> ['\'',s].                 

% (optional) appositions, i.e., constructions like 'customer Mr Miller' or 'a bank Y'
apposition(Noun) --> common_noun(Specifier_Noun), proper_noun(Proper_Noun),
	{ Specifier_Noun = sem<->(type<->Explicit_type),              % copy the noun specifier's type to the type of the apposition.
	  Proper_Noun = syn<->(ncat<->NCAT) ..
                        sem<->(head<->Name..type<->Implicit_type),    % overriding the implicit type (e.g., Male) of the proper noun.
	  Noun = syn<->(ncat<->NCAT) ..
	         sem<->(head<->Name..type<->Explicit_type) }.

% dynamic labels
apposition(Noun) --> common_noun(Specifier_Noun), new_label(Noun),
	{ Specifier_Noun = syn<->(ncat<->NCAT)..sem<->(type<->SUO_concept), % copy the noun specifier's type to the type of the apposition.
	  Noun = syn<->(ncat<->NCAT)..sem<->(type<->SUO_concept) }.

% new labels
% E.g., ...X...Y...Z...[N.B. none others at present]...
new_label(Noun) --> unknown_proper_noun(Noun).               % currently restricted to 'X', 'Y', or 'Z' to facilitate generation

% relative sentences, note these require the 'gap' grammatical feature
% E.g., ...who checks it...who gives the card to the employee...that the customer inserts...
% Note: the rel features is 'no' for the relative sentence since it cannot itself be embedded
% within another surrounding relative sentence. On the other hand the rel feature is 'yes' for
% the embedded sentence since that sentence is indeed within a relative sentence.
relative_sentence(Relative_Sentence) -->        
	{ Relative_Sentence = syn<->(gcat<->GCAT..gap<->GAP..rel<->no) ..
	                      sem<->EMBEDDED_SENTENCE_SEMANTICS,
	  Relative_Pronoun  = syn<->(ncat<->GCAT..gap<->GAP),
	  Embedded_Sentence = syn<->(gcat<->GCAT..gap<->GAP..act<->ACT..vcat<->VCAT..rel<->yes..max<->3) ..
	                      sem<->EMBEDDED_SENTENCE_SEMANTICS },
	relative_pronoun(Relative_Pronoun),
	embedded_sentence(Embedded_Sentence).
        
% optional element
% E.g., ...parses nothing!...
empty --> [].

%-------------------------------
% PARTS OF SPEECH TO LEXICON
%-------------------------------

determiner(Determiner)  --> [Determiner],  { determiner_in_lexicon(Determiner,Definite_Or_Not) }.

% handle either single word or compound word nouns
common_noun(Noun) --> 
                [Word],				% single word noun
               {
		 % Syntactic grammatical features...
		 Noun = syn<->(ncat<->NCAT..count<->Countability) ..
	                sem<->(noun<->Word..head<->Sigma_Name..type<->SUO_concept..id<->Synset_ID),
		 noun_in_lexicon(Word,NCAT,Gender,Countability,Number,SUO_concept,Synset_ID),
		 atom(Word),
		 % Semantic grammatical features...
		 create_var_name(Word,Sigma_Name)
	       }.


common_noun(Noun) -->
	       [First],
	       { Noun = syn<->(ncat<->NCAT..count<->Countability) ..
	                sem<->(noun<->[First,Second|Rest]..head<->Sigma_Name..type<->SUO_concept..id<->Synset_ID),
		 noun_in_lexicon([First,Second|Rest],NCAT,Gender,Countability,Number,SUO_concept,Synset_ID),
		 % Semantic grammatical features...
		 create_var_name([First,Second|Rest],Sigma_Name)
		 },
               [Second|Rest].     % compound word common noun

% handle either single word or compound proper nouns
proper_noun(Proper_Noun) --> 
		      [Name],       % single word proper noun
	              { Proper_Noun = syn<->(ncat<->NCAT) ..
		                      sem<->(noun<->Name..head<->SUO_constant..type<->Sigma_type..id<->Synset_ID),
			atom(Name),
			proper_noun_in_lexicon(Name,NCAT,Gender,Number,Sigma_type,SUO_constant,Synset_ID)
		      }.

proper_noun(Proper_Noun) -->
	            [First],
	            { Proper_Noun = syn<->(ncat<->NCAT) ..
		                    sem<->(noun<->[First,Second|Rest]..head<->SUO_constant..type<->Sigma_type..id<->Synset_ID),
		      proper_noun_in_lexicon([First,Second|Rest],NCAT,Gender,Number,Sigma_type,SUO_constant,Synset_ID)
		    },
		    [Second|Rest]. % compound word proper noun


pronoun(Pronoun) -->
	{ Pronoun = syn<->(case<->CASE..gender<->GENDER) ..
	            sem<->(noun<->Word..head<->Sigma_Name..type<->Sigma_type),
	  pronoun_in_lexicon(Word,CASE,GENDER,Sigma_type),
   	  % Semantic grammatical features...
          create_var_name(Word,Sigma_Name)
	  },
        [Word].

relative_pronoun(Relative_Pronoun) -->
	{ Relative_Pronoun = syn<->(ncat<->NCAT..gap<->Word),  % NCAT is one of [person, time, object]
	  relative_pronoun_in_lexicon(Word,NCAT) },
	[Word].

% Handle either single word (e.g., red) or compound word (e.g., first class) common adjectives
% Note: MOD is the SUO name of the modifier.
adjective(MODIFIER) --> 
	      { adjective_in_lexicon(Word,Root,normal,positive,MOD),
		atom(Word),
		MODIFIER = sem<->(mod<->MOD)..syn<->(reply<->Word)},		% single word adjective
              [Word].				

adjective(MODIFIER) -->
	{ adjective_in_lexicon([First|Rest],Root,normal,positive,MOD),
	  MODIFIER = sem<->(mod<->MOD)..syn<->(reply<->[First|Rest])
	},
	      [First|Rest].                     % compound word adjective
                                                % [Note: two-place, comparative, & superlative adjectives are in Sentence DCG rules.]

% Note that the type feature and the noun category feature (ncat) are set by the noun specifier, not here.
unknown_proper_noun(Proper_Noun) -->
	      [Name],
	      { member(Name,['X','Y','Z']),
		Proper_Noun = syn<->(ncat<->_)..sem<->(head<->Name..type<->_) }.


%-------------------------------
% TESTS
%-------------------------------

% Phrases that should parse OK:

% pronouns
test(np,[he],yes).
test(np,[her],yes).

% determiner, no modifier, and common noun
test(np,[the,card],yes).

% determiner, modifier, and common noun
test(np,[a,new,customer],yes).
test(np,[the,'Swiss',customer],yes).

% of prepositional phrase
test(np,[the,customer,of,the,bank],yes).

% compound noun
test(np,[the,valid,credit,card],yes).

% appositions
test(np,[the,customer,'Mr','Miller'],yes).

% dynamic labels
test(np,[the,customer,'X'],yes).

% proper nouns
test(np,['John'],yes).
test(np,['Monday'],yes).

% possessives
test(np,['John','\'',s,card],yes).
test(np,[the,card,'\'',s,code],yes).
test(np,['Monday','\'',s,customer],yes).
test(np,['John','\'',s,code],yes).

% Phrases that should not be accepted:
test(np,[valid,credit,card],no).
test(np,[the,'Swiss'],yes).
test(np,[a,new],no).
test(np,[a,new,customer,smith],no).
test(np,[a,new,customer,mary],no).

% Phrases with relative sentences or of-prepositional phrases:
test(np,[the,customer,who,enters,the,bank],yes).
test(np,[a,bank,that,the,customer,enters],yes).
test(np,[a,valid,credit,card,that,the,customer,inserts],yes).
test(np,[the,'Swiss',customer,who,enters,the,bank],yes).
test(np,[a,'Swiss',customer,of,the,bank],yes).
test(np,[a,customer,who,is,'Swiss'],yes).
test(np,[a,credit,card,that,is,new],yes).
test(np,[a,customer,that,enters,the,bank],yes).
test(np,[the,customer,'\'',s,code,that,is,valid],yes).
test(np,[the,man,'\'',s,suit,which,is,old],yes).
test(np,[the,man,'\'',s,suit,that,is,cotton],yes).
test(np,['John','\'',s,customer,who,is,'Swiss'],yes).

