% CELT simple sentences translated using GULP

% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

% Test parsing with test_parse_sent_all to test stored parse tests.
% Test generation of simple sentences with a specified length and
% number of words with test_gen_S1 ... to test_gen_all_S.

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading CELT Parser Version 2(b) ...sentence rules...syntactic and semantic translation'),nl.

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
rel,    is whether or not the grammatical category is inside a relative sentence or not, one of [yes,no], *
max,    is the maximum number of repeated consecutive structures, which currently only applies to adjuncts.
ncat,   is the noun category for the head noun in a noun phrase, one of [person,thing,time].
advp,   is the adverbial preposition or adverb used in an adjunct (e.g., 'inside' in 'inside the house', or 'slowly' in 'slowly').
role,   is the role this speech act plays, one of [assertion, query, command] for sentences, questions, or imperatives respectively.
det,    is the determiner for the head of this noun phrase (e.g., the, a, which, what...)
aux,    is 'yes' if an auxiliary such as 'does' is present, and 'no', if no axiliary is present
qvars,  is a list of query variables to be found in a query (e.g., [?who,?what]). It is [] for assertions and yes/no queries.
count,  is either 'mass' or 'count' according to whether the main noun in the noun phrase can be a mass noun normally, or not.
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

 of complements,

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
% DCG RULES FOR SIMPLE SENTENCES
%-------------------------------

% SENTENCE

% A top-level sentence...e.g., 'A customer inserts a card.'
sentence(Sentence) --> 
	           { Sentence = syn<->(role<->assertion..act<->ACT..vcat<->VCAT..gcat<->empty..gap<->empty..rel<->no..max<->3) ..
		                sem<->PREDICATE_SEMANTICS,
		     Subject = syn<->(role<->assertion..gcat<->empty..gap<->empty..rel<->no) ..
		               sem<->SUBJ,
	             Predicate = syn<->(role<->assertion..act<->ACT..vcat<->VCAT..gcat<->empty..gap<->empty..rel<->no..max<->3) ..
		                 sem<->PREDICATE_SEMANTICS,
		     Predicate = sem<->(subj<->SUBJ) % add the subj feature to the semantics of Predicate
		     },
		   subject(Subject),
		   ( noncopula_predicate(Predicate); copula_predicate(Predicate) ).

% A relational sentence...e.g., 'John has a start date of Friday for Bob in accounting at the Los Angeles office with no insurance.'
% where the translation is to a single relation with many arguments, e.g., (employee 'Friday' 'Bob' 'accounting' 'LA' 'no')
sentence(Sentence) --> 
	           { Sentence = syn<->(role<->assertion..act<->be..vcat<->relation..gcat<->empty..gap<->empty..rel<->no..max<->3) ..
		                sem<->(subj<->SUBJ..specs<->Specifications..pred<->Target_Relation),
		     Subject = syn<->(role<->assertion..gcat<->empty..gap<->empty..rel<->no) ..
		               sem<->SUBJ,
	             Predicate = syn<->(role<->assertion..act<->be..vcat<->relation..gcat<->empty..gap<->empty..rel<->no..max<->3) ..
		                 sem<->(subj<->SUBJ..specs<->Specifications..pred<->Target_Relation)
		     },
		   subject(Subject),
		   [is],
		   defined_relation(Relation_Name,Target_Relation),
		   ([who];[that];[which]),
		   parameter_specifications(Relation_Name,Specifications).

% Note 1: For now preposition should not be 'with' or 'of' and each preposition should be unique for a relation.
% Note 2: For now the keyword, preposition, and attribute name should all be individual atoms, not lists.
% Note 3: All parameter specs joined by 'and' at present.	

% Air Mission 1 is a new air mission that has a primary target Target A and second target Target B and
% refuels at Location 5 and mission type combat air patrol.

% test(sentence,['Mission','1',is,a,new,air,mission,that,has,a,primary,target,'Target','A',
%	         and,second,target,'Target','B',and,refuels,at,'Location','5',and,mission,type,combat,air,patrol],yes).


defined_relation(Relation_Name,Target_Relation) --> 
	{ relation(Relation_Name,Relation_Keywords,Target_Relation,Specs) },
	Relation_Keywords.

% Example:

%relation(air_mission_relation,[a,new,air,mission],ato_mission,
%	 [
%	  spec(keywords([has,a,primary,target]),attribute([first,target]),type('ATOTarget')),
%	  spec(keywords([has,a,secondary,target]),attribute([second,target]),type('ATOTarget')),	  
%	  spec(keywords([refuels,at]),attribute([refueling,point]),type('Place')),
%	  spec(keywords([has,a,waypoint,at]),attribute([waypoint]),type('Place')),
%	  spec(keywords([has,type]),attribute([mission,type]),type('ATOMissionType'))
%	 ]).

parameter_specifications(Relation_Name,Specifications) -->
	([has]; empty),
	parameter_value_specification(Relation_Name,Parameter_Spec,Value),
	([and], parameter_specifications(Relation_Name,Other_Parameter_Specs); { Other_Parameter_Specs = [] } ),
	{ Specifications = [pair(Parameter_Spec,Value)|Other_Parameter_Specs] }.

parameter_value_specification(Relation_Name,Specification,NP_Value) -->
	parameter_specification(Relation_Name,Specification),
	np(NP_Value).

parameter_specification(Relation_Name,Specification) -->
	{ relation(Relation_Name,Relation_Keywords,Target_Relation,Specs),
	  member(Specification,Specs),
	  Specification = spec(keywords(Keyword_Words),attribute(Attribute_Words),type(SUMO_type_restriction)) },
        % e.g., assume the spec has Keyword=[works,for]  Attribute=[supervisor]
	( Keyword_Words;         % would handle 'works for'
	  Attribute_Words;       % would handle 'supervisor'
          { append(Keyword_Words,Attribute_Words,Everything) },
	  Everything ).          % would handle 'works for supervisor'

% Embedded sentences, such as relative sentences or queries have a non-empty
% gap, i.e., the gap GCAT is either 'person', 'object', or 'time', 
% always matching the type of the noun moved.

embedded_sentence(Sentence) --> % here the gap is used for a missing NP in the subject
	           { Sentence = syn<->(role<->ROLE..act<->ACT..vcat<->VCAT..gap<->GAP..gcat<->GCAT..
				      reply<->[GAP|PRED_REPLY]..rel<->yes..max<->3) ..
		                sem<->PREDICATE_SEMANTICS,
		     Subject  = syn<->(role<->ROLE..gcat<->GCAT..gap<->GAP..rel<->yes) ..
		                sem<->SUBJECT_SEMANTICS,
		     Predicate = syn<->(role<->ROLE..act<->ACT..vcat<->VCAT..gap<->empty..
				       reply<->PRED_REPLY..gcat<->empty..rel<->yes..max<->3) ..
				 sem<->PREDICATE_SEMANTICS,
		     PREDICATE_SEMANTICS = subj<->SUBJECT_SEMANTICS }, % add subject semantics into predicate semantics
		   subject(Subject),
		   ( noncopula_predicate(Predicate); copula_predicate(Predicate) ).


% Gaps in Relative Sentences

% ...gap in subject in embedded sentence: 'The customer who enters the bank inserts the card.'
%    embedded sentence: 'enters the bank', with gap filled in: '[the customer] enters the bank'

% ...gap in direct object in embedded sentence: 'Mr Miller enters the card that the customer inserts.'
%    embedded sentence: 'the customer inserts', with gap filled in: 'the customer inserts [the card]'

% ...gap in direct object in embedded sentence with indirect object: 'The teller enters the card that the customer gives to him.'
%    embedded sentence: 'the customer gives to him', with gap filled in: 'the customer gives [the card] to him'

% ...gap in indirect object in embedded sentence: 'John gives the card to the teller who the customer gives the code to.'
%    embedded sentence: 'the customer gives the code to', with gap filled in: 'the customer gives the code to [the teller]'

% Gaps in Queries

% ...gap in direct object in embedded sentence: 'What does Mary give to John?'
%    embedded sentence: 'Mary give to John', with gap filled in: 'Mary give[s] what to John'

% ...gap in direct object in embedded sentence with indirect object: 'What does the customer give to him?'
%    embedded sentence: 'the customer give to him', with gap filled in: 'the customer give[s] [what] to him'

% ...gap in indirect object in embedded sentence: 'Who does John give a card to?'
%    embedded sentence: 'John give a card to', with gap filled in: 'John give[s] a card to who'

embedded_sentence(Sentence) --> % here the gap is used for a missing NP in the predicate
	           { Sentence = syn<->(role<->ROLE..act<->ACT..vcat<->VCAT..gap<->GAP..gcat<->GCAT..
				      reply<->REPLY..rel<->yes..max<->3) ..
		                sem<->PREDICATE_SEMANTICS,
		     Subject  = syn<->(role<->ROLE..gcat<->empty..gap<->empty..rel<->yes..reply<->SUBJ_REPLY) ..
		                sem<->SUBJECT_SEMANTICS,
		     Predicate = syn<->(role<->ROLE..act<->ACT..vcat<->VCAT..gcat<->GCAT..gap<->GAP..
				       reply<->PRED_REPLY..rel<->yes..max<->3) ..
		                 sem<->PREDICATE_SEMANTICS,
    		     PREDICATE_SEMANTICS = subj<->SUBJECT_SEMANTICS }, % add subject semantics into predicate semantics
		   subject(Subject),
		   ( noncopula_predicate(Predicate); copula_predicate(Predicate) ),
		   { (is_list(SUBJ_REPLY),is_list(PRED_REPLY)) -> append(SUBJ_REPLY,PRED_REPLY,REPLY); REPLY=[SUBJ_REPLY|PRED_REPLY] }.

% SUBJECT

% 'John', 'the customer', 'the Swiss customer at the bank'
subject(Subject) -->
	{ Subject = syn<->(role<->ROLE..ncat<->NCAT..gcat<->GCAT..gap<->GAP..rel<->REL..reply<->REPLY) ..
	            sem<->SEMANTICS,
	  NP = syn<->(role<->ROLE..case<->nominative..gender<->GENDER..ncat<->NCAT..gcat<->GCAT..gap<->GAP..rel<->REL..reply<->REPLY) ..
	       sem<->SEMANTICS },
	np(NP).

% DIRECT OBJECT

% 'the card', 'her', 'him' in sentences
% also handles 'what' in questions like 'John gives what to Mary?' and 'John rides what?'
direct_object(Direct_Object) -->
	{ Direct_Object = syn<->(role<->ROLE..ncat<->NCAT..gcat<->GCAT..gap<->GAP..rel<->REL..qvars<->QVARS..reply<->REPLY) ..
	                  sem<->SEMANTICS,
	  NP = syn<->(role<->ROLE..case<->accusative..gender<->GENDER..ncat<->NCAT..gcat<->GCAT..gap<->GAP..
		     rel<->REL..qvars<->QVARS..reply<->REPLY) ..
	       sem<->SEMANTICS },
	np(NP).

% COPULA: 

copula_predicate(Predicate) -->         % 'is a customer of the bank'
	{ Predicate = syn<->(role<->ROLE..act<->be..vcat<->copula..gcat<->GCAT..gap<->GAP..rel<->REL..reply<->[is|REPLY]) ..
	              sem<->(pred<->isa..dobj<->DIRECT_OBJECT_SEMANTICS),
	  Verb = syn<->(act<->be..vcat<->copula..role<->ROLE) ..
	         sem<->(pred<->isa),
	  Direct_Object = syn<->(gcat<->GCAT..gap<->GAP..rel<->REL..reply<->REPLY) ..
	                  sem<->DIRECT_OBJECT_SEMANTICS },
	copula(Verb),
	direct_object(Direct_Object).

copula_predicate(Predicate) -->         % 'is at the bank'
	{ Predicate = syn<->(role<->ROLE..act<->be..vcat<->copula..gcat<->GCAT..gap<->GAP..rel<->REL..reply<->[is|REPLY]) ..
	              sem<->(pred<->isa..adjs<->Adjuncts),
	  Verb = syn<->(act<->be..vcat<->copula..role<->ROLE..reply<->REPLY) ..
	         sem<->(pred<->isa),
	  Adjunct = syn<->(advp<->PREP) ..
	            sem<->ADJUNCT_SEMANTICS,
	  Adjuncts = [ADJUNCT_SEMANTICS] },
	copula(Verb),
	adverbial_prep_phrase(Adjunct).

copula_predicate(Predicate) -->         % 'is rich'
	{ Predicate = syn<->(role<->ROLE..act<->be..vcat<->copula..gcat<->GCAT..gap<->GAP..rel<->REL..reply<->[is|MOD_REPLY])..
	              sem<->(pred<->isa..mod<->MOD),
	  MODIFIER = sem<->(mod<->MOD)..syn<->(reply<->MOD_REPLY),
	  Verb = syn<->(act<->be..vcat<->copula..role<->ROLE) ..
	         sem<->(pred<->isa) },
	copula(Verb),
	adjective(MODIFIER).

% Note: a superlative cannot appear where the adjective is. See P. 53 of ACE manual, superlatives may not
% be used alone, only in the attributive position. So "John's car is the biggest." is disallowed although
% "John's car is the biggest car." is acceptable.

% handle two place adjectives, e.g., 'is identical to', 'is different from',...
copula_predicate(Predicate) -->        % 'is greater than'...
	{ Predicate = syn<->(role<->ROLE..act<->be..vcat<->copula..gcat<->GCAT..gap<->GAP..rel<->REL) ..
	              sem<->(pred<->isa..dobj<->DOBJ..mod<->Two_Arg_Lambda_Expression),
	  Verb = syn<->(act<->be..vcat<->copula..role<->ROLE),
	  NP = syn<->(gcat<->GCAT..gap<->GAP..rel<->REL) ..
	       sem<->DOBJ },
	copula(Verb),
	two_place_adjective(Two_Arg_Lambda_Expression),     % e.g., 'is identical to', 'is different from...'
	np(NP).

% handle comparative adjectives, e.g., 'is older than', 'is faster than', 'is more expensive than'
% ACE requires comparatives to have 'than' followed by a noun phrase
copula_predicate(Predicate) --> 
	{ Predicate = syn<->(role<->ROLE..act<->be..vcat<->copula..gcat<->GCAT..gap<->GAP..rel<->REL) ..
	              sem<->(pred<->isa..dobj<->DOBJ..mod<->COMPARATOR),
	  Verb = syn<->(act<->be..vcat<->copula..role<->ROLE),
	  NP = syn<->(gcat<->GCAT..gap<->GAP..rel<->REL) ..
	       sem<->DOBJ,
	  COMPARATOR = X^Y^[greaterThan, [measure, X, SUO_concept], [measure, Y, SUO_concept]] },
	copula(Verb),
	comparative_adjective(SUO_concept),  % e.g., 'is older than', 'is faster than', 'is more expensive than'
        [than],                              % ACE requires comparatives to have 'than' followed by a noun phrase
	np(NP).

% INTRANSITIVE VERBS: 'he stays'...'she arrives'...

noncopula_predicate(Predicate) --> % intransitive verbs
	{ Predicate = syn<->(role<->ROLE..act<->ACT..vcat<->[intransitive,_,_]..max<->Max..reply<->[ACT]..
			    gap<->empty..gcat<->empty) ..
	         sem<->(pred<->SUO_concept..adjs<->SEMANTICS_OF_ADJUNCTS..id<->Synset_ID),
	  Verb = syn<->(act<->ACT..vcat<->[intransitive,_,_]..role<->ROLE) ..
	         sem<->(pred<->SUO_concept..id<->Synset_ID),
	  Adjuncts = syn<->(max<->Max) ..
	             sem<->SEMANTICS_OF_ADJUNCTS },	  	  
	verb(Verb),
	adjuncts(Adjuncts).

% N.B. TRANSITIVE VERBS: 'enters the bank', 'drops the card', 'closes the door' are listed
% following DITRANSITIVE verbs so that sentences like 'He gives the book to Mary' are
% parsed first as ditransitive verbs with an indirect object and only secondly as transitive
% verbs with adjuncts.

% NOTE: Transitive verbs after Ditransitive verbs.

% DITRANSITIVE VERBS: 

% ...possibly part of an embedded sentence with a gap...
% but if part of such a sentence then do not use the gap here, instead it must
% be used in the subject part of the sentence.
% [Note: the rel attribute must still be a variable in Predicate (rel<->REL), not rel<->no,
% since the predicate may be part of a relative sentence (e.g., 'gives a book to John' in
% 'the boy who gives a book to John' where 'who' is used to plug the gap in 'gives a book to John'
% by providing the subject)

% DITRANSITIVE VERBS: RULES for DIRECT OBJECT + to + INDIRECT OBJECT
% Rules for ditransitive verbs when the direct object precedes the indirect object and that has
% a 'to' prepositional phrase: e.g., 'John gives the book to Mary.'

% nongapped, in which case query variables can arise, as in 'Who gives what to whom?'
noncopula_predicate(Predicate) --> 
	{ Predicate = syn<->(role<->ROLE..act<->ACT..vcat<->[_,_,ditransitive]..
			    gcat<->empty..gap<->empty..rel<->REL..
			    max<->MAX..qvars<->QVARS3..
			    reply<->REPLY) ..
	         sem<->(pred<->SUO_concept..dobj<->DIRECT_OBJECT_SEMANTICS ..
		       iobj<->INDIRECT_OBJECT_SEMANTICS..adjs<->SEMANTICS_OF_ADJUNCTS..id<->Synset_ID),
	  Verb = syn<->(act<->ACT..vcat<->[_,_,ditransitive]..role<->ROLE) ..
	         sem<->(pred<->SUO_concept..id<->Synset_ID),
	  Direct_Object = syn<->(gcat<->empty..gap<->empty..rel<->no..qvars<->QVARS1..reply<->DO_REPLY) ..
	                  sem<->DIRECT_OBJECT_SEMANTICS,
	  Indirect_Object_Prep_Phrase = syn<->(gcat<->empty..rel<->no..qvars<->QVARS2..reply<->IO_REPLY) ..
	                                sem<->INDIRECT_OBJECT_SEMANTICS,
	  Adjuncts = syn<->(max<->MAX)..sem<->SEMANTICS_OF_ADJUNCTS },
	verb(Verb),
	direct_object(Direct_Object),
	indirect_object_prep_phrase(Indirect_Object_Prep_Phrase),
	% after obtaining any QVARs from both the direct objects and indirect objects now we can pass these up to the predicate...
	{ ((is_list(QVARS1), is_list(QVARS2)) -> append(QVARS1,QVARS2,QVARS3); QVARS3 = QVARS1) },
	adjuncts(Adjuncts),
	% construct reply for queries from verb, direct object, and indirect object
	{ REPLY = [ACT,DO_REPLY,to,IO_REPLY] }.

% ...if part of an embedded sentence with a gap, then use the gap in the direct obj...
noncopula_predicate(Predicate) --> 
        { Predicate = syn<->(role<->ROLE..act<->ACT..vcat<->[_,_,ditransitive]..gcat<->GCAT..
			    gap<->GAP..rel<->yes..max<->MAX..reply<->REPLY) ..
	              sem<->(pred<->SUO_concept..dobj<->DIRECT_OBJECT_SEMANTICS ..
			    iobj<->INDIRECT_OBJECT_SEMANTICS..adjs<->SEMANTICS_OF_ADJUNCTS..id<->Synset_ID),
	  not(var(GCAT)), not(GCAT = empty),
	  Verb = syn<->(act<->ACT..vcat<->[_,_,ditransitive]..role<->ROLE) ..
	         sem<->(pred<->SUO_concept..id<->Synset_ID),
	  Direct_Object = syn<->(gcat<->GCAT..gap<->GAP..rel<->yes) ..
	                  sem<->DIRECT_OBJECT_SEMANTICS,
	  Indirect_Object_Prep_Phrase = syn<->(gcat<->empty..gap<->empty..rel<->yes..reply<->IO_REPLY) ..
	                                sem<->INDIRECT_OBJECT_SEMANTICS,
	  Adjuncts = syn<->(max<->MAX)..sem<->SEMANTICS_OF_ADJUNCTS },	  	  
	verb(Verb),
	direct_object(Direct_Object),
	indirect_object_prep_phrase(Indirect_Object_Prep_Phrase),
	adjuncts(Adjuncts),
	% construct reply for queries from verb, direct object, and indirect object
	{ REPLY = [ACT,GAP,to,IO_REPLY] }.

% ...if part of an embedded sentence with a gap, then use the gap in the indirect obj...
noncopula_predicate(Predicate) --> 
        { Predicate = syn<->(role<->ROLE..act<->ACT..vcat<->[_,_,ditransitive]..gcat<->GCAT..
			    reply<->REPLY..gap<->GAP..rel<->yes..max<->MAX) ..
	              sem<->(pred<->SUO_concept..dobj<->DIRECT_OBJECT_SEMANTICS ..
			    iobj<->INDIRECT_OBJECT_SEMANTICS..adjs<->SEMANTICS_OF_ADJUNCTS..id<->Synset_ID),
	  not(var(GCAT)), not(GCAT = empty),
	  Verb = syn<->(act<->ACT..vcat<->[_,_,ditransitive]..role<->ROLE) ..
	         sem<->(pred<->SUO_concept..id<->Synset_ID),
	  Direct_Object = syn<->(gcat<->empty..gap<->empty..rel<->yes..reply<->DO_REPLY) ..
	                  sem<->DIRECT_OBJECT_SEMANTICS,
	  Indirect_Object_Prep_Phrase = syn<->(gcat<->GCAT..gap<->GAP..rel<->yes..reply<->IO_REPLY) ..
	                                sem<->INDIRECT_OBJECT_SEMANTICS,
	  Adjuncts = syn<->(max<->MAX)..sem<->SEMANTICS_OF_ADJUNCTS },
	verb(Verb),
	direct_object(Direct_Object),
	indirect_object_prep_phrase(Indirect_Object_Prep_Phrase),
	adjuncts(Adjuncts),
	% construct reply for queries from verb, direct object, and indirect object
	{ append([ACT|DO_REPLY],IO_REPLY,REPLY) }.

% DITRANSITIVE VERBS: RULES for INDIRECT OBJECT + DIRECT OBJECT
% Rules for ditransitive verbs when the direct object precedes the indirect object and that has
% a 'to' prepositional phrase: e.g., 'John gives the book to Mary.'

% nongapped, in which case query variables can arise, as in 'Who gives what to whom?'
noncopula_predicate(Predicate) --> 
	{ Predicate = syn<->(role<->ROLE..act<->ACT..vcat<->[_,_,ditransitive]..
			    gcat<->empty..gap<->empty..rel<->REL..
			    reply<->REPLY..max<->MAX..qvars<->QVARS3) ..
	         sem<->(pred<->SUO_concept..dobj<->DIRECT_OBJECT_SEMANTICS ..
		       iobj<->INDIRECT_OBJECT_SEMANTICS..adjs<->SEMANTICS_OF_ADJUNCTS..id<->Synset_ID),
	  Verb = syn<->(act<->ACT..vcat<->[_,_,ditransitive]..role<->ROLE) ..
	         sem<->(pred<->SUO_concept..id<->Synset_ID),
	  Direct_Object = syn<->(gcat<->empty..gap<->empty..rel<->no..qvars<->QVARS1..reply<->DO_REPLY) ..
	                  sem<->DIRECT_OBJECT_SEMANTICS,
	  Indirect_Object = syn<->(gcat<->empty..rel<->no..qvars<->QVARS2..reply<->IO_REPLY) ..
	                                sem<->INDIRECT_OBJECT_SEMANTICS,
	  Adjuncts = syn<->(max<->MAX)..sem<->SEMANTICS_OF_ADJUNCTS },
	verb(Verb),
	indirect_object(Indirect_Object),
	direct_object(Direct_Object),
	% after obtaining any QVARs from both the direct objects and indirect objects now we can pass these up to the predicate...
	{ ((is_list(QVARS1), is_list(QVARS2)) -> append(QVARS1,QVARS2,QVARS3); QVARS3 = QVARS1) },
	adjuncts(Adjuncts),
	% construct reply for queries from verb, direct object, and indirect object
	{ REPLY = [ACT,DO_REPLY,to,IO_REPLY] }.

% ...if part of an embedded sentence with a gap, then use the gap in the direct obj...
noncopula_predicate(Predicate) --> 
        { Predicate = syn<->(role<->ROLE..act<->ACT..vcat<->[_,_,ditransitive]..gcat<->GCAT..gap<->GAP..rel<->yes..max<->MAX) ..
	              sem<->(pred<->SUO_concept..dobj<->DIRECT_OBJECT_SEMANTICS ..
			    iobj<->INDIRECT_OBJECT_SEMANTICS..adjs<->SEMANTICS_OF_ADJUNCTS..id<->Synset_ID),
	  not(var(GCAT)), not(GCAT = empty),
	  Verb = syn<->(act<->ACT..vcat<->[_,_,ditransitive]..role<->ROLE) ..
	         sem<->(pred<->SUO_concept..id<->Synset_ID),
	  Direct_Object = syn<->(gcat<->GCAT..gap<->GAP..rel<->yes) ..
	                  sem<->DIRECT_OBJECT_SEMANTICS,
	  Indirect_Object = syn<->(gcat<->empty..gap<->empty..rel<->yes..reply<->IO_REPLY) ..
	                                sem<->INDIRECT_OBJECT_SEMANTICS,
	  Adjuncts = syn<->(max<->MAX)..sem<->SEMANTICS_OF_ADJUNCTS },	  	  
	verb(Verb),
	indirect_object(Indirect_Object),
	direct_object(Direct_Object),
	adjuncts(Adjuncts),
	% construct reply for queries from verb, direct object, and indirect object
	{ REPLY = [ACT,GAP,to,IO_REPLY] }.	

% ...if part of an embedded sentence with a gap, then use the gap in the indirect obj...
noncopula_predicate(Predicate) --> 
        { Predicate = syn<->(role<->ROLE..act<->ACT..vcat<->[_,_,ditransitive]..
			    gcat<->GCAT..gap<->GAP..rel<->yes..max<->MAX..reply<->REPLY) ..
	              sem<->(pred<->SUO_concept..dobj<->DIRECT_OBJECT_SEMANTICS ..
			    iobj<->INDIRECT_OBJECT_SEMANTICS..adjs<->SEMANTICS_OF_ADJUNCTS..id<->Synset_ID),
	  not(var(GCAT)), not(GCAT = empty),
	  Verb = syn<->(act<->ACT..vcat<->[_,_,ditransitive]..role<->ROLE) ..
	         sem<->(pred<->SUO_concept..id<->Synset_ID),
	  Direct_Object = syn<->(gcat<->empty..gap<->empty..rel<->yes..reply<->DO_REPLY) ..
	                  sem<->DIRECT_OBJECT_SEMANTICS,
	  Indirect_Object = syn<->(gcat<->GCAT..gap<->GAP..rel<->yes..reply<->IO_REPLY) ..
	                          sem<->INDIRECT_OBJECT_SEMANTICS,
	  Adjuncts = syn<->(max<->MAX)..sem<->SEMANTICS_OF_ADJUNCTS
	  },	  	  
	verb(Verb),
	indirect_object(Indirect_Object),
	direct_object(Direct_Object),
	adjuncts(Adjuncts),
	% construct reply for queries from verb, direct object, and indirect object
	{ append([ACT|DO_REPLY],IO_REPLY,REPLY) }.

% TRANSITIVE VERBS: 'enters the bank', 'drops the card', 'closes the door'

% ...possibly part of an embedded sentence with a gap...
noncopula_predicate(Predicate) --> 
	{ Predicate = syn<->(role<->ROLE..act<->ACT..vcat<->[_,transitive,_]..gcat<->GCAT..gap<->GAP..
			    rel<->REL..max<->MAX..qvars<->QVARS..reply<->[ACT|DO_REPLY]) ..
	         sem<->(pred<->SUO_concept..dobj<->DIRECT_OBJECT_SEMANTICS..adjs<->SEMANTICS_OF_ADJUNCTS..id<->Synset_ID),
	  Verb = syn<->(act<->ACT..vcat<->[_,transitive,_]..role<->ROLE) ..
	         sem<->(pred<->SUO_concept..id<->Synset_ID),
	  Direct_Object = syn<->(gcat<->GCAT..gap<->GAP..rel<->REL..qvars<->QVARS..reply<->DO_REPLY) ..
	                  sem<->DIRECT_OBJECT_SEMANTICS,
	  Adjuncts = syn<->(max<->MAX)..sem<->SEMANTICS_OF_ADJUNCTS },
	verb(Verb),
	direct_object(Direct_Object),
	adjuncts(Adjuncts).

% ADJUNCTS (ZERO OR MORE)

% Max below limits the maximum number of adjuncts; it is needed for generation to terminate.
adjuncts(syn<->(max<->MAX)..sem<->[]) --> empty. 

adjuncts(syn<->(max<->MAX)..sem<->[ADJUNCT_SEMANTICS|REST_SEMANTICS]) --> 
	{ MAX > 0, Adjusted_Limit is MAX - 1, ADJUNCT = syn<->ADJUNCT_SYNTAX..sem<->ADJUNCT_SEMANTICS },
	adjunct(ADJUNCT), adjuncts(syn<->(max<->Adjusted_Limit)..sem<->REST_SEMANTICS).

% AN INDIVIDUAL ADVERB OR ADJUNCT
% adjuncts are either adverbs or adverbial prepositional phrases.

adjunct(ADJUNCT) --> adverb(ADJUNCT); adverbial_prep_phrase(ADJUNCT). 

% PREPOSITIONAL PHRASES

% INDIRECT OBJECT and prepositional phrases (must begin with 'to' in ACE and CELT) with an indirect object

% in sentences ...to introduce an indirect object (e.g., 'John gives a book to Mary.')
% in queries ... to ask about an indirect object (e.g., 'John gives a book to who?')
indirect_object_prep_phrase(IO) -->
	{ NP = syn<->(role<->ROLE..case<->accusative..gender<->GENDER..gcat<->GCAT..
		     gap<->GAP..rel<->REL..qvars<->QVARS..reply<->REPLY) ..
	       sem<->SEMANTICS,
	  IO = syn<->(role<->ROLE..case<->accusative..gender<->GENDER..gcat<->GCAT..
		     gap<->GAP..rel<->REL..qvars<->QVARS..reply<->[to|REPLY]) ..
	       sem<->SEMANTICS
	},
	[to],
	 np(NP).

%  in sentences...an indirect object without a preposition preceding it (e.g., 'John gives Mary a book.')
%  in queries...an indirect object asked about without a preposition preceding it (e.g., 'John gives who a book?')
indirect_object(IO) -->
	{ NP = syn<->(role<->ROLE..case<->accusative..gender<->GENDER..ncat<->NCAT..gcat<->GCAT..gap<->GAP..rel<->REL..qvars<->QVARS) ..
	       sem<->SEMANTICS,
	  IO = syn<->(role<->ROLE..case<->accusative..gender<->GENDER..gcat<->GCAT..
		     gap<->GAP..rel<->REL..qvars<->QVARS..reply<->[to|REPLY]) ..
	       sem<->SEMANTICS
	},
	 np(NP). 

% adv,        is the Sigma concept name for the adverb in the adjunct, if there is one, or empty, if not
% prep,       is the adverbial preposition if there is no adverb
% aobj,       is the noun phrase part of the adverbial preposition, aobj stands for adverbial object
% ncat,   is the noun category for the head noun in a noun phrase, one of [person,thing,time].

% Note: NCAT is one of [person, thing, time] and is used to classify common nouns in the lexicon.
% Here is it also used to select the proper sense of a preposition. For example, 'in', 'at', and 'on'
% can refer to either locations or times. By requiring that NCAT be the same in the NP and in the
% adjunct (adverbial preposition) we ensure the proper sense is selected.

% ...to modify a verb...(note: relative sentences disabled within these by setting 'rel' feature to 'no')
adverbial_prep_phrase(ADJUNCT) -->
	{ NP = syn<->(case<->accusative..gender<->GENDER..ncat<->NCAT..gcat<->empty..gap<->empty..rel<->yes) ..
	       sem<->OBJECT_SEMANTICS,
	  PREP = syn<->(advp<->Preposition..ncat<->NCAT)..sem<->(prep<->SUO_PREP),
	  ADJUNCT = syn<->(advp<->Preposition..ncat<->NCAT) ..
	            sem<->(prep<->SUO_PREP..aobj<->OBJECT_SEMANTICS) },
	adverbial_preposition(PREP),
	np(NP).

%-------------------------------
% PARTS OF SPEECH TO LEXICON
%-------------------------------

% COPULA VERBS

% Note: That the copula verb 'be' is handled directly in the grammar
% rules below and is not a verb lexicon entry. This separation allows
% handling copula and non-copula verb forms esp. in queries more easily.

% the only copula verb is 'be', which cannot be skipped over in sentences
copula(Verb) -->
	{ Verb = syn<->(act<->be..vcat<->copula..role<->assertion) ..
	         sem<->(pred<->isa) },
	[is].

% NON-COPULA VERBS

% Note that in sentences the verbs should be like 'enters', 'inserts', 'opens', etc.
% whereas in queries the verbs take on their base form like 'enter', 'insert', 'open', etc.

% NON-COPULA VERBS IN SENTENCES

% handle either single word (e.g., enter) or compound word (e.g., dry clean) verbs
verb(Verb) -->
	[Word],			% single word verb
	{ Verb = syn<->(act<->Root..vcat<->VCAT..role<->assertion) ..
	         sem<->(pred<->SUO_concept..id<->Synset_ID),
	  atom(Word),
	  verb_in_lexicon(Word,Root,VCAT,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID)
	}.


verb(Verb) -->
	[First],
	{ Verb = syn<->(act<->Root..vcat<->VCAT..role<->assertion) ..
	         sem<->(pred<->SUO_concept..id<->Synset_ID),
	  verb_in_lexicon([First,Second|Rest],Root,VCAT,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID) },
	[Second|Rest].		% compound word verb

% a preposition other than 'of' or 'to', although 'to' is allowed in 'from...to'
% e.g., 'in the slot', 'from London', 'in the morning', 'until midnight', 'with a customer'...

% Note: prepositions that can apply to multiple object types (e.g., 'on', 'at', and 'in')
% are disambiguated by requiring the noun category (one of [person,object,thing]) to match
% the noun that follows the preposition.

adverbial_preposition(PREP)  --> 
	{ preposition_in_lexicon(Preposition,Modification_type,Noun_type,SUO_concept),
	  PREP = syn<->(advp<->Preposition..ncat<->Noun_type)..sem<->(prep<->SUO_concept) },
	[Preposition].

% adverbs
% 'slowly', 'carefully', 'manually', ...
adverb(ADVERB_ADJUNCT) -->
	{ adverb_in_lexicon(Adverb,Modification_type,Sigma_Concept) },
	[Adverb],
	{ ADVERB_ADJUNCT  = syn<->(advp<->Adverb) ..
	                    sem<->(adv<->Sigma_Concept) }.

% two place adjectives (see NP rules for regular adjectives)
two_place_adjective(Two_Arg_Lambda_Expression) -->
        { adjective_in_lexicon([Adjective,Preposition],Root,two_place,ungraded,Two_Arg_Lambda_Expression) },
	[Adjective, Preposition].

% comparative adjectives, either one or two words.
comparative_adjective(SUO_concept) --> 
	      { adjective_in_lexicon(Word,Root,normal,comparative,SUO_concept),
		atom(Word) },                   % single word comparative
              [Word].				

comparative_adjective(SUO_concept) -->
	      { adjective_in_lexicon([More,Comparative],Root,normal,comparative,SUO_concept) },
	      [More,Comparative].               % two word comparative adjective (e.g., 'more expensive')

% superlative adjectives, either one or two words.
superlative(MODIFIER) --> 
	      { adjective_in_lexicon(Word,Root,normal,superlative,SUO_concept),
		MOD = superlative(SUO_concept),   % use 'superlative' to indicate this is a superlative for later SUO form generation
		atom(Word),
		MODIFIER = sem<->(mod<->MOD)..syn<->(reply<->[Word])
	      },		
              [Word].		% single word superlative		

superlative(MODIFIER) -->
	      { adjective_in_lexicon([Most|Superlative],Root,normal,superlative,SUO_concept),
		MOD = superlative(SUO_concept),
		MODIFIER = sem<->(mod<->MOD)..syn<->(reply<->[Most|Superlative])
	      },                                       % use 'superlative' to indicate this is a superlative for later SUO form generation
	      [Most|Superlative].                      % compound word superlative

%-------------------------------
% TESTS
%-------------------------------

% Tests on adjuncts

test(adjuncts,[on,the,train],yes).
test(adjuncts,[on,'Monday'],yes).
test(adjuncts,[manually],yes).
test(adjuncts,[],yes).
test(adjuncts,[punctually,on,'Monday'],yes).
test(adjuncts,[punctually,on,'Monday',on,the,train],yes).
test(adjuncts,[correctly,on,'Monday',on,the,train,in,the,station],yes).

% Simple sentences that should parse OK:

% Tests on code generation for stative verbs in sentences like 'owns'
test(sentence,['John',owns,a,dog],yes).

% intransitive verbs
test(sentence,['John',arrives],yes).
test(sentence,['Mary',stays],yes).

% using copula verb 'be'...
test(sentence,['John',is,a,customer,of,the,bank],yes).
test(sentence,[the,customer,is,at,the,bank],yes).
test(sentence,[the,card,is,valid],yes).

% copula using two-place adjectives
test(sentence,['John','\'',s,code,is,identical,to,'Mary','\'',s,code],yes).
test(sentence,['John','\'',s,card,is,compatible,with,the,'Swiss',bank],yes).

% copula using comparative adjectives
test(sentence,['John','\'',s,bank,is,faster,than,'Mary','\'',s,bank],yes).
test(sentence,['John','\'',s,card,is,more,expensive,than,'Mary','\'',s,bank],yes).

% using other verbs...
test(sentence,[the,'Swiss',customer,enters,the,bank],yes).
test(sentence,[a,new,customer,gives,the,card,to,the,teller],yes).
test(sentence,['John',gives,the,card,to,'Mary'],yes).
test(sentence,[she,gives,the,card,to,'Mary'],yes).

% alternate ditransitive constructions...
test(sentence,[a,new,customer,gives,the,teller,the,card],yes).
test(sentence,['John',gives,'Mary',the,card],yes).

% superlative adjectives
test(sentence,['Mr','Miller',is,the,oldest,teller],yes).
test(sentence,[the,oldest,customer,enters,an,invalid,card],yes).

% using compounds...

test(sentence,['Mr','Miller',enters,the,bank],yes).
test(sentence,['The','New','York','Times',is,at,the,bank],yes).
test(sentence,['John',dry,cleans,the,credit,card],yes).
test(sentence,[the,'Swiss',customer,enters,the,first,class,bank],yes).

% using appositions

test(sentence,[the,customer,'Mr','Miller',enters,the,bank],yes).

% using dynamic labels (currently only X, Y, or Z)

test(sentence,[the,customer,'Mr','Miller',enters,a,bank,'X'],yes).

% using possessives

test(sentence,['John','\'',s,card,is,at,the,bank],yes).
test(sentence,[the,card,'\'',s,code,is,valid],yes).
test(sentence,['Monday','\'',s,customer,arrives,punctually],yes).

% sentences with relative sentences embedded:

% ...gap in subject in embedded sentence
test(sentence,[the,customer,who,enters,the,bank,inserts,the,card],yes).
test(sentence,[a,customer,who,is,'Swiss',inserts,the,card],yes).
test(sentence,[the,card,which,is,old,is,valid],yes).
test(sentence,[the,card,that,is,old,is,valid],yes).
test(sentence,[a,customer,inserts,a,credit,card,which,is,old],yes).

% ...gap in direct object in embedded sentence
test(sentence,['Mr','Miller',enters,the,card,that,the,customer,inserts],yes).
test(sentence,['Mr','Miller',enters,the,card,that,the,customer,inserts,into,the,slot],yes).

% ...gap in subject in embedded sentence with indirect object
test(sentence,['John',gives,the,card,to,the,teller,who,gives,the,code,to,the,customer],yes).

% ...gap in subject in embedded sentence with indirect object, alternate ditransitive construction in relative clause
test(sentence,['John',gives,the,card,to,the,teller,who,gives,the,customer,the,code],yes).

% ...gap in direct object in embedded sentence with indirect object
test(sentence,[the,teller,enters,the,card,that,the,customer,gives,to,him],yes).

% ...gap in direct object in embedded sentence with indirect object, alternate ditransitive construction in relative clause
test(sentence,[the,teller,enters,the,card,that,the,customer,gives,him],yes).

% ...gap in indirect object in embedded sentence with indirect object
test(sentence,['John',gives,the,card,to,the,teller,who,the,customer,gives,the,code,to],yes).

% ...gap in subject in embedded sentence with indirect object, alternate ditransitive construction in relative clause
test(sentence,['John',gives,the,card,to,the,teller,who,gives,the,customer,the,code],yes).

% ...multiple relative sentences
test(sentence,[a,customer,who,is,old,inserts,a,credit,card,that,is,valid],yes).

% sentences with adjuncts:

test(sentence,['John',uses,the,credit,card,correctly],yes).
test(sentence,[the,customer,inserts,a,card,slowly],yes).
test(sentence,[the,customer,who,inserts,a,card,slowly,enters,a,code],yes).
test(sentence,[the,train,arrives,in,the,station],yes).
test(sentence,[a,customer,enters,a,code,manually],yes).
test(sentence,[a,customer,arrives,on,'Monday'],yes).
test(sentence,[a,customer,arrives,on,the,train],yes).
test(sentence,[a,customer,arrives,on,'Monday',punctually],yes).
test(sentence,[the,fast,train,arrives,punctually],yes).
test(sentence,[the,fast,train,arrives,punctually,on,'Monday'],yes).
test(sentence,[the,fast,train,arrives,punctually,on,'Monday',in,the,station],yes).

% sentences with many of the above:
test(sentence,['John','\'',s,customer,inserts,a,valid,card],yes).

test(sentence,['John','\'',s,customer,inserts,a,valid,card,manually],yes).

test(sentence,['John','\'',s,customer,inserts,a,valid,card,manually,into,a,slot,'X'],yes).

test(sentence,['John','\'',s,customer,inserts,a,valid,card,of,'Mary',manually,into,a,slot,'X'],yes).

test(sentence,['John','\'',s,customer,who,is,new,inserts,a,valid,card,of,'Mary',manually,into,a,slot,'X'],yes).

% EXAMPLES OF ERRORS, THESE SHOULD FAIL TO PARSE

% ungrammatical sentences that should not be accepted.

test(sentence,[her,gives,the,card,to,'Mary'],no).
test(sentence,[she,gives,the,card,to,he],no).
test(sentence,['John',correctly,uses,the,credit,card],no). % Note: the adjunct must appear at the end, after the VP.

% missing word from lexicon

test(sentence,[the,froboz,arrives],no).  % froboz, not in lexicon...used to be 'aardvark', but that now is not in lexicon.

% grammatical constructions *NOT* in ACE, even though they are in complete English.
% perhaps generate warnings for some of these kinds of sentences...

test(sentence,[punctually,the,fast,train,arrives,on,'Monday'],no). % you cannot start a sentence with an adverb
test(sentence,[the,red,fast,train,arrives],no). % you cannot string adjectives, instead use 'and'
test(sentence,['John','\'',s,bank,is,the,fastest],no).  % can only use superlatives with a noun, not alone, must be in attributive position.
test(sentence,['John','\'',s,bank,is,the,most,expensive],no).

% constructions added to test translations and SUO form generation
test(sentence,[the,dog,is,red],yes).
test(sentence,['John',is,red],yes).
test(sentence,['John','\'',s,dog,is,different,from,'Mary','\'',s,dog],yes).
test(sentence,['John','\'',s,dog,is,more,expensive,than,'Mary','\'',s,dog],yes).
test(sentence,[the,oldest,dog,enters,the,bank],yes).
test(sentence,['John','\'',s,dog,is,the,most,expensive,dog],yes).

% constructions added to test translation of adverbial prepositions
test(sentence,[the,visitor,arrives,at,midnight],yes).    % 'at' refers to time here
test(sentence,[the,visitor,arrives,at,the,station],yes). % 'at' refers to location here
test(sentence,[the,train,arrives,on,'Monday'],yes).      % 'on' refers to time here
test(sentence,[the,boat,floats,on,the,water],yes).       % 'on' refers to location here

% different types of possessives

test(sentence,[the,farmer,sees,'John','\'',s,dog],yes).                        % John's dog   (a possession)
test(sentence,[the,banker,pokes,'John','\'',s,nose],yes).                      % John's nose  (a body part)
test(sentence,[the,thief,kills,'John','\'',s,father],yes).                     % John's father  (a relative)
test(sentence,[the,captain,of,the,ship,throws,the,cannon,into,the,sea],yes).   % captain of the ship (occupational position)
test(sentence,['John','\'',s,swimming,triggers,'Mary','\'',s,distress],yes).   % John's swimming, Mary's distress
test(sentence,['Jane','\'',s,height,amazes,'John'],yes).                       % Jane's height

% different uses of 'to' and newly introduced prepositions

test(sentence,[the,airplane,flies,to,the,airport],yes).                        % 'to' a location (ACE restricts 'to' to indirect objects)
test(sentence,['John',enters,the,bank,with,'Sarah'],yes).                      % comitative relationship in CELT maps to two agent roles
test(sentence,['Karla',falls,below,the,branch],yes).
test(sentence,[he,drives,toward,the,bridge],yes).

% mass versus count nouns, determiners can be omitted for mass nouns

test(sentence,[the,spy,gives,money,to,the,thief],yes).                         % note: a 'the' determiner is not required for 'money'
test(sentence,[money,talks],yes).                                              % note: a 'the' determiner is not required for 'money'
test(sentence,[boy,talks],no).                                                 % note: a 'the' determiner is required for 'boy'
test(sentence,[the,boy,talks],yes).                                            % note: a 'the' determiner is required for 'boy'
test(sentence,[the,waiter,pours,soup,into,the,bowl],yes).                      % note: 'soup' does not to have a determiner before it

% optional determiners for proper nouns

test(sentence,[he,travels,to,'Sudan'],yes).                                    % a proper noun may be preceded by a determiner
test(sentence,[he,travels,to,the,'Sudan'],yes).                                % so either 'Sudan' or 'the Sudan' is accepted here.
