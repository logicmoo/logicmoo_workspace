% CELT queries translated using GULP

% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

% Test parsing with test_parse_query_all to test stored parse tests.

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading CELT Parser Version 2(b) ...query rules...syntactic and semantic translation'),nl.

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

N.B. each of the arguments is the feature value package of a complement.

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
% DCG RULES FOR QUERIES
%-------------------------------

% A top-level yes-no question...e.g., 'Does a customer insert a card?'
% with a reply such as 'Yes, a customer inserts a card.' or 'I do not know if a customer inserts a card.'
% The actual reply constructed is just 'a customer inserts a card.' to which the assertion or negation
% can be added to precede it.

% WRM: needs verb change in reply: current reply is like [a, customer, insert, a, card].

query(Query) --> 
	           { Query = syn<->(role<->query(yes_no)..aux<->yes..act<->ACT..vcat<->VCAT..gcat<->empty
				                        ..qvars<->[]..reply<->REPLY..gap<->empty..rel<->no..max<->3) ..
		             sem<->PREDICATE_SEMANTICS,
		     Subject = syn<->(gcat<->empty..gap<->empty..rel<->no..reply<->SUBJ_REPLY) ..
		               sem<->SUBJ,
	             Predicate = syn<->(role<->query(yes_no)..aux<->yes..act<->ACT..vcat<->VCAT..gcat<->empty..
				        gap<->empty..rel<->no..max<->3..reply<->PRED_REPLY) ..
		                 sem<->PREDICATE_SEMANTICS,
		     Predicate = sem<->(subj<->SUBJ) % add the subj feature to the semantics of Predicate
		     },
		   auxiliary, /* just the word 'does' for now */
		   subject(Subject),
		   noncopula_predicate(Predicate),
		   % construct query reply from subject and predicate parts
		   { (is_list(SUBJ_REPLY),is_list(PRED_REPLY)) -> append(SUBJ_REPLY,PRED_REPLY,REPLY); REPLY=[SUBJ_REPLY|PRED_REPLY] }.

% A top-level yes-no question...e.g., 'Is Jill a banker?'
% with a reply such as 'Yes, Jill is a banker.' or 'I do not know if Jill is a banker.' The actual reply
% constructed is just 'Jill is a banker.' to which the assertion or negation can be added to it.

%     In the standard copula predicate used for
% sentences we can reuse the same rules here for a query if we allow copula(Verb) to
% parse nothing, like a 'gap', using a special rule for copula verbs for
% queries, see the copula rules for copula(Verb)-->[] later in this file

query(Query) --> 
	           { Query = syn<->(role<->query(yes_no)..aux<->no..act<->ACT..vcat<->VCAT..gcat<->empty
				                        ..qvars<->[]..reply<->REPLY..gap<->empty..rel<->no..max<->3) ..
		             sem<->PREDICATE_SEMANTICS,
		     Subject = syn<->(gcat<->empty..gap<->empty..rel<->no..reply<->SUBJ_REPLY) ..
		               sem<->SUBJ,
	             Predicate = syn<->(role<->query(yes_no)..aux<->no..act<->ACT..vcat<->VCAT..
				       gcat<->empty..gap<->empty..rel<->no..max<->3..reply<->PRED_REPLY) ..
		                 sem<->PREDICATE_SEMANTICS,
		     Predicate = sem<->(subj<->SUBJ) % add the subj feature to the semantics of Predicate
		     },
		   copula(_),
		   subject(Subject),
		   copula_predicate(Predicate), % we use copula rules for copula(Verb)-->[] later in this file, see note above.
		   % construct query reply from subject and predicate parts
		   { (is_list(SUBJ_REPLY),is_list(PRED_REPLY)) -> append(SUBJ_REPLY,PRED_REPLY,REPLY); REPLY=[SUBJ_REPLY|PRED_REPLY] }.

% A top-level WH-query...e.g., 'Who enters a card?', 'What is valid?', 'Which customer enters a VisaCard?'
% Also handles WH-query with multiple WH-words like 'Who gives what to Mary?' and 'Who gives what to whom?'
% Note: these constructions are all non-gapped.

% Replies are like this: 'John enters a card.', 'The code is valid.', 'The Swiss customer enters a VisaCard.', etc.		   

% WRM: needs verb change in reply: current reply is like [?who, enter, a, card]

query(Query) -->
	           { Query = syn<->(role<->query(wh)..aux<->no..act<->ACT..vcat<->VCAT..reply<->REPLY..
				    qvars<->ALL_QVARS..gcat<->empty..gap<->empty..rel<->no..max<->3) ..
		                sem<->PREDICATE_SEMANTICS,
		     Subject = syn<->(gcat<->WH_GAP_CAT..gap<->WH_GAP..rel<->no..qvars<->[WH_VAR]) ..
		               sem<->SUBJ,
	             Predicate = syn<->(role<->query(wh)..aux<->no..act<->ACT..vcat<->VCAT..reply<->PREDICATE_REPLY..
				        gcat<->empty..gap<->empty..rel<->no..max<->3..qvars<->PREDICATE_QVARS) ..
		                 sem<->PREDICATE_SEMANTICS,
		     Predicate = sem<->(subj<->SUBJ) }, % add the subj feature to the semantics of Predicate
		   wh_query(Subject),
		   ( noncopula_predicate(Predicate); copula_predicate(Predicate) ),
		   { 
		     % put all QVARS together for questions like 'Who gives what to whom?'
		     (is_list(PREDICATE_QVARS) -> ALL_QVARS = [WH_VAR|PREDICATE_QVARS]; ALL_QVARS = [WH_VAR]),
		     % also construct the reply from the WH-variable plus the predicate
		     REPLY = [WH_VAR|PREDICATE_REPLY]
		   }.

		   

% QUERIES WITH GAP CONSTRUCTIONS

% A top-level WH-query with an auxiliary that uses a gapped construction in the query
% E.g., 'Who does John give a card to?' is interpreted as 'John gives a card to who?'
% and 'What does Mary give to John?' is interpreted as 'Mary gives what to John?'

% Note: we are setting 'rel' to 'yes' in the embedded sentence even though the queries are not inside relative
% sentences and in fact the combination is disallowed. But, by setting rel<->yes we can reuse
% the DCG rules that apply to the handling of gaps in embedded sentences.

% Replies are like this: 'John gives a card to Mary.' and 'Mary gives the book to John.'

% WRM: needs verb change in reply: current reply is like ['John',give,a,card,to,'?who']

query(Query) -->
	           { Query = syn<->(role<->query(wh)..aux<->yes..act<->ACT..vcat<->VCAT..reply<->REPLY..
				   qvars<->[WH_VAR]..gcat<->empty..gap<->empty..max<->3) ..
		                sem<->PREDICATE_SEMANTICS,

		     WH_WORD = syn<->(gcat<->WH_GAP_CAT..gap<->WH_GAP..rel<->yes..qvars<->[WH_VAR]),

	             Sentence = syn<->(role<->query(wh)..aux<->yes..act<->ACT..vcat<->VCAT..reply<->REPLY..
				       gcat<->WH_GAP_CAT..gap<->WH_GAP..rel<->yes..max<->3) ..
		                sem<->PREDICATE_SEMANTICS

		     },
		   wh_query(WH_WORD),
		   auxiliary, /* just the word 'does' for now */
		   embedded_sentence(Sentence).

% NOUN PHRASES

% pronouns (WHAT, WHO) in questions
np(NP) -->
	{ NP = syn<->(role<->query(wh)..det<->empty..case<->CASE..gender<->GENDER..ncat<->NCAT..
		     reply<->[HEAD]..qvars<->[HEAD]..gcat<->empty) ..
	       sem<->(head<->HEAD..type<->SUO_concept),
	  Pronoun = syn<->(case<->CASE..gender<->GENDER) ..
	            sem<->(head<->HEAD..type<->SUO_concept) },
	wh_pronoun(Pronoun).             % e.g., 'Who enters what?'

% SUBJECT

% WH_QUERY constructions, such as 'who', 'what', 'what key', 'which man', etc.
% can be used in gap constructions so they set the GAP and GCAT features. GAP
% is set to the word or words used (e.g., who, [what,key],what,[which,man], etc.).
% GCAT is set to the category (person, thing, or object) that this should be used
% to fill in for.

% 'Who', 'What', etc.
% WRM: replies to 'Who enters the bank?' is currently [?who,enter,the,bank]
% WRM: replies to 'What opens the door?' is currently [?what,open,the,door]

wh_query(Subject) -->
	{ Subject = syn<->(det<->empty..case<->nominative..gender<->GENDER..ncat<->NCAT..gcat<->GCAT..qvars<->[HEAD]..gap<->GAP) ..
	            sem<->(head<->HEAD..type<->SUO_concept..mod<->empty..poss<->empty..of<->empty..sub<->empty),
	  Noun = syn<->(det<->empty..ncat<->NCAT..gcat<->GCAT..gap<->GAP) ..
	         sem<->(head<->HEAD..type<->SUO_concept..poss<->empty)
	  },	  
	wh_pronoun(Noun).

% 'Which card', 'Which man', 'What key', etc.

% WRM: replies to 'What key opens the door?' is currently [?key,open,the,door]
% WRM: replies to 'Which man enters the bank?' are currently [?man,enter,the,bank]

wh_query(Subject) -->
	{ Subject = syn<->(det<->DET..case<->nominative..gender<->GENDER..
			   ncat<->NCAT..gcat<->NCAT..qvars<->[HEAD]..gap<->[DET,SUO_concept]) ..
	            sem<->(head<->HEAD..type<->SUO_concept..mod<->MODIFIER..poss<->POSS..of<->OF..sub<->empty),
	  Noun = syn<->(ncat<->NCAT..det<->DET) ..
	         sem<->(head<->HEAD..type<->SUO_concept..poss<->POSS)
	  },	  
	wh_determiner(DET),
	allow_modifier(MODIFIER,MODIFIER_REPLY), % N.B. The modifier is currently not used in the reply,
	                                         % i.e., we are NOT currently handling queries like 'Which green key opens the door?'
	(common_noun(Noun), { POSS = 'empty' }; apposition(Noun), { POSS = 'empty' }; possessive2(Noun)),
	(of_prep_phrase(OF); {OF = 'empty' }, empty).

% ADVERBIAL PREPOSITIONAL PHRASES IN ADJUNCTS
% PART I: NON-COPULA PREDICATES ONLY

% query_verb_phrase_modifier([WH_WORD],QVAR_NAME,no,MODIFICATION).
%   handles sentences like 'WH_WORD...?' (e.g., 'Where...?', 'When...?', and 'How...?')

% Single word adjuncts (where, when, how)
% E.g., 'Where does John work?', 'When does the man arrive?', 'How does the dog run?'

% Replies are like these: 'John works ?where', 'The man arrives ?when'., and 'The dog runs ?how.'
% WRM: replies to 'Where does John work?' is ['John',work,'?where'].
% WRM: replies to 'When does the train arrive?' is [the,train,arrive,'?when']
% WRM: replies to 'How does the dog run?' is [the,dog,run,'?how']

query(Query) -->
	{ query_verb_phrase_modifier([WH_WORD],QVAR_NAME,no,MODIFICATION) },
	[WH_WORD],                          % e.g., 'Where...'
	auxiliary,                          % e.g., '...does...'
	subject(Subject),                   % e.g., '......John...'
	{ Query = syn<->(role<->query(wh)..aux<->yes..act<->ACT..vcat<->VCAT..
		         qvars<->[QVAR_NAME]..gcat<->empty..gap<->empty..rel<->no..max<->3..reply<->REPLY) ..
	          sem<->PREDICATE_SEMANTICS,
	  Subject = syn<->(reply<->SUBJ_REPLY)..sem<->SUBJ,
	  Predicate = syn<->(role<->query(wh)..aux<->yes..act<->ACT..vcat<->VCAT..
			     gcat<->empty..gap<->empty..rel<->no..max<->3..reply<->PRED_REPLY) ..
	              sem<->PREDICATE_SEMANTICS,
	  Predicate = sem<->(subj<->SUBJ) % add the subj feature to the semantics of Predicate
	},
	noncopula_predicate(Predicate),     % e.g., '.........work', '...give the book to Mary', 'throw the ball', etc.
	{ (is_list(SUBJ_REPLY),is_list(PRED_REPLY)) ->
	  append(SUBJ_REPLY,PRED_REPLY,REPLY1), append(REPLY1,[QVAR_NAME],REPLY); REPLY=[SUBJ_REPLY,PRED_REPLY,QVAR_NAME] }.

% query_verb_phrase_modifier([WH1,WH2],QVAR_NAME,no,MODIFICATION).
%   handles sentences like 'WH1 WH2 ...?'        (e.g., 'Since when...?','Until when...?','How long...?','How often...?','With whom...?')

% Multiple word adjuncts that occur in sequence only at the start of a query.
% E.g., 'Since when does the employee work?', 'How long does the train stop?'
% E.g., 'How often does the milkman come?', 'With whom does the employee enter the room?'

% Replies are like these: 'The employee works since ?when.','The train stops for ?how_long.',
% 'The milkman comes ?frequency.', 'The employee enters the room with ?whom.'

% [Note: that the first word of the two query words is in front of the query var unless it is 'how'.]

% WRM: replies to 'How long does the train stop?' is [the,train,stop,'?how_long']
% WRM: replies to 'How often does the milkman come?' is [the,milkman,come,'?how_often']
% WRM: replies to 'Since when does the employee work?' is [the,employee,work,since,'?since_when']

query(Query) -->
	{ query_verb_phrase_modifier([WH1,WH2],QVAR_NAME,no,MODIFICATION) },
	[WH1,WH2],                          % e.g., 'Until when...'
	auxiliary,                          % e.g., '...does...'
	subject(Subject),                   % e.g., '......John...'
	{ Query = syn<->(role<->query(wh)..aux<->yes..act<->ACT..vcat<->VCAT..reply<->REPLY..
		         qvars<->[QVAR_NAME]..gcat<->empty..gap<->empty..rel<->no..max<->3) ..
	          sem<->PREDICATE_SEMANTICS,
	  Subject = syn<->(reply<->SUBJ_REPLY)..sem<->SUBJ,
	  Predicate = syn<->(role<->query(wh)..aux<->yes..act<->ACT..vcat<->VCAT..
			     gcat<->empty..gap<->empty..rel<->no..max<->3..reply<->PRED_REPLY) ..
	              sem<->PREDICATE_SEMANTICS,
	  Predicate = sem<->(subj<->SUBJ) % add the subj feature to the semantics of Predicate
	},
	noncopula_predicate(Predicate),     % e.g., '.........work', '...loan the book to Mary', 'hold the ball', etc.
	% check to make sure subject and predicate replies are lists, and then also add the first query word if it is not 'how'
	{ (is_list(SUBJ_REPLY),is_list(PRED_REPLY)) -> 
	  (append(SUBJ_REPLY,PRED_REPLY,REPLY1),
	         ((WH1==how)->append(REPLY1,[QVAR_NAME],REPLY);append(REPLY1,[WH1,QVAR_NAME],REPLY)));
	  REPLY=[SUBJ_REPLY,PRED_REPLY,QVAR_NAME] }.

% query_verb_phrase_modifier([WH_WORD,Particle],separated,MODIFICATION).
%   handles ONLY sentences like 'WH_WORD...Particle?' (e.g., 'Where...to?' and 'Where...into?')

% Multiple word adjuncts that occur with one word at the start of the query and the other at the end only.
% E.g., 'Where does John insert the card into?' and 'Where does the machine move to?'

% Replies are like these: 'John inserts the card into the ATM.' and 'The machine moves to the dock.'

% WRM: replies to 'Where does John insert the card into?' and ['John',insert,the,card,?where_into]
% WRM: replies to 'Where does the machine move to?' and [the,machine,move,to,?where_to]

query(Query) -->
	{ query_verb_phrase_modifier([WH_WORD,PARTICLE],QVAR_NAME,separated,MODIFICATION) }, % e.g., [where,to] or [where,into]
	[WH_WORD],                          % e.g., 'Where...'
	auxiliary,                          % e.g., '...does...'
	subject(Subject),                   % e.g., '......John...'
	{ Query = syn<->(role<->query(wh)..aux<->yes..act<->ACT..vcat<->VCAT..reply<->REPLY..
		         qvars<->[WH_WORD]..gcat<->empty..gap<->empty..rel<->no..max<->3) ..
	          sem<->PREDICATE_SEMANTICS,
	  Subject = syn<->(reply<->SUBJ_REPLY)..sem<->SUBJ,
	  Predicate = syn<->(role<->query(wh)..aux<->yes..act<->ACT..vcat<->VCAT..
			     gcat<->empty..gap<->empty..rel<->no..max<->3..reply<->PRED_REPLY) ..
	              sem<->PREDICATE_SEMANTICS,
	  Predicate = sem<->(subj<->SUBJ) % add the subj feature to the semantics of Predicate
	},
	noncopula_predicate(Predicate),     % e.g., '.........insert the card'
	[PARTICLE],                         % e.g., '........................into'
	% check to make sure subject and predicate replies are lists, and then also add the particle to the reply
	{ (is_list(SUBJ_REPLY),is_list(PRED_REPLY)) -> 
	  (append(SUBJ_REPLY,PRED_REPLY,REPLY1), append(REPLY1,[PARTICLE,QVAR_NAME],REPLY));
	  REPLY=[SUBJ_REPLY,PRED_REPLY,PARTICLE,QVAR_NAME] }.

% query_verb_phrase_modifier([WH_WORD,Particle],reversible,MODIFICATION).
%   handles sentences like 'WH_WORD...Particle?' (e.g., 'Where...from?', 'Who...with?')
%   and sentences like 'Particle WH_WORD...?'    (e.g., 'From where...?', 'Who with...?)

% Multiple word adjuncts that can appear separated in a query (at the front and end), or
% in reverse order at the beginning.

% Rule for Case 1, the two words separated at the front and the end of a query.
% E.g., case 1, 'Where does the train arrive from?'
% Reply is like: 'The train arrives from Scotland.'

query(Query) -->
	{ query_verb_phrase_modifier([WH_WORD,PARTICLE],QVAR_NAME,reversible,MODIFICATION) }, % e.g., [where,from] or [who,with]
	[WH_WORD],                          % e.g., 'Where...'
	auxiliary,                          % e.g., '...does...'
	subject(Subject),                   % e.g., '......the train...'
	{ Query = syn<->(role<->query(wh)..aux<->yes..act<->ACT..vcat<->VCAT..reply<->REPLY..
		         qvars<->[WH_WORD]..gcat<->empty..gap<->empty..rel<->no..max<->3) ..
	          sem<->PREDICATE_SEMANTICS,
	  Subject = syn<->(reply<->SUBJ_REPLY)..sem<->SUBJ,
	  Predicate = syn<->(role<->query(wh)..aux<->yes..act<->ACT..vcat<->VCAT..
			     gcat<->empty..gap<->empty..rel<->no..max<->3..reply<->PRED_REPLY) ..
	              sem<->PREDICATE_SEMANTICS,
	  Predicate = sem<->(subj<->SUBJ) % add the subj feature to the semantics of Predicate
	},
	noncopula_predicate(Predicate),     % e.g., '.........arrive'
	[PARTICLE],                         % e.g., '................from'
	% check to make sure subject and predicate replies are lists, and then also add the particle to the reply
	{ (is_list(SUBJ_REPLY),is_list(PRED_REPLY)) -> 
	  (append(SUBJ_REPLY,PRED_REPLY,REPLY1), append(REPLY1,[PARTICLE,QVAR_NAME],REPLY));
	  REPLY=[SUBJ_REPLY,PRED_REPLY,PARTICLE,QVAR_NAME] }.

% Rule for Case 2, the two words together, in reverse order, at the front of a query.
% E.g., case 2, 'From where does the train arrive?'
% Reply is like: 'The train arrives from London.'

query(Query) -->
	{ query_verb_phrase_modifier([WH_WORD,PARTICLE],QVAR_NAME,reversible,MODIFICATION) }, % e.g., [where,from] or [who,with]
	[PARTICLE,WH_WORD],                 % e.g., 'From where...'
	auxiliary,                          % e.g., '...does...'
	subject(Subject),                   % e.g., '......the train...'
	{ Query = syn<->(role<->query(wh)..aux<->yes..act<->ACT..vcat<->VCAT..reply<->REPLY..
		         qvars<->[WH_WORD]..gcat<->empty..gap<->empty..rel<->no..max<->3) ..
	          sem<->PREDICATE_SEMANTICS,
	  Subject = syn<->(reply<->SUBJ_REPLY)..sem<->SUBJ,
	  Predicate = syn<->(role<->query(wh)..aux<->yes..act<->ACT..vcat<->VCAT..
			     gcat<->empty..gap<->empty..rel<->no..max<->3..reply<->PRED_REPLY) ..
	              sem<->PREDICATE_SEMANTICS,
	  Predicate = sem<->(subj<->SUBJ)   % add the subj feature to the semantics of Predicate
	},
	noncopula_predicate(Predicate),     % e.g., '.........arrive'
	% check to make sure subject and predicate replies are lists, and then also add the particle to the reply
	{ (is_list(SUBJ_REPLY),is_list(PRED_REPLY)) -> 
	  (append(SUBJ_REPLY,PRED_REPLY,REPLY1), append(REPLY1,[PARTICLE,QVAR_NAME],REPLY));
	  REPLY=[SUBJ_REPLY,PRED_REPLY,PARTICLE,QVAR_NAME] }.

% ADVERBIAL PREPOSITIONAL PHRASES IN ADJUNCTS
% PART II: COPULA PREDICATES ONLY

% query_verb_phrase_modifier([WH_WORD],no,MODIFICATION).
%   handles sentences like 'WH_WORD...?' (e.g., 'Where...?', 'When...?', and 'How...?')

% Single word adjuncts (where, when, how)

% E.g., 'Where is the bank?', 'How is John?'
% Replies: 'The bank is at the station.', 'John is angry.'

query(Query) -->
	{ query_verb_phrase_modifier([WH_WORD],QVAR_NAME,no,MODIFICATION) },
	[WH_WORD],                          % e.g., 'Where...'
	{ Query = syn<->(role<->query(wh)..aux<->no..act<->be..vcat<->copula..reply<->REPLY..
		         qvars<->[QVAR_NAME]..gcat<->empty..gap<->empty..rel<->no..max<->3) ..
	          sem<->(pred<->isa..mod<->MODIFIER..subj<->SUBJECT_SEMANTICS..dobj<->DIRECT_OBJECT_SEMANTICS),
	  Predicate = syn<->(role<->query(wh)..act<->be..vcat<->copula..
			    reply<->PRED_REPLY..gcat<->empty..gap<->empty..rel<->no) ..
	              sem<->(pred<->isa..subj<->SUBJECT_SEMANTICS..dobj<->DIRECT_OBJECT_SEMANTICS)
	},
	query_copula_predicate(Predicate),
	% check to make sure subject and direct object replies are lists, and then put the reply together
	{ is_list(PRED_REPLY) ->
	      ((WH_WORD \== how) -> append(PRED_REPLY,[at,QVAR_NAME],REPLY); append(PRED_REPLY,[QVAR_NAME],REPLY));
	  REPLY=[PRED_REPLY,at,QVAR_NAME] }.	

% Multiple word adjuncts ('until when', 'how long', 'how often')

% E.g., 'Until when is the shop open?', 'How long is the bank open?', 'How often does the bank open?'
% Replies: 'The shop is open until Sunday.', 'The bank is open until 5.', 'The bank is open daily.'

% WRM: replies to 'Until when is the shop open?'

query(Query) -->
	{ query_verb_phrase_modifier([WH1,WH2],QVAR_NAME,no,MODIFICATION) },
	[WH1,WH2],                          % e.g., 'Until when...'
	{ Query = syn<->(role<->query(wh)..aux<->no..act<->be..vcat<->copula..reply<->REPLY..
		         qvars<->[QVAR_NAME]..gcat<->empty..gap<->empty..rel<->no..max<->3) ..
	          sem<->(pred<->isa..mod<->MODIFIER..subj<->SUBJECT_SEMANTICS..dobj<->DIRECT_OBJECT_SEMANTICS),
	  Predicate = syn<->(role<->query(wh)..act<->be..vcat<->copula..
			    reply<->PRED_REPLY..gcat<->empty..gap<->empty..rel<->no) ..
	              sem<->(pred<->isa..subj<->SUBJECT_SEMANTICS..dobj<->DIRECT_OBJECT_SEMANTICS)
	},
	query_copula_predicate(Predicate),
	% check to make sure subject and direct object replies are lists, and then put the reply together
	{ is_list(PRED_REPLY) ->
	      ((WH1 \== how) -> append(PRED_REPLY,[WH1,QVAR_NAME],REPLY); REPLY = PRED_REPLY);
	  REPLY=[PRED_REPLY,WH1,QVAR_NAME] }.

% QUERY COPULA PREDICATES
% patterned after the copula predicates, but with subject inversion added for the query.

% Added this first rule to handle questions like 'Where is the bank?' and 'Who is John?'
% even though ACE did not appear to support them.

query_copula_predicate(Predicate) -->         % 'is John?'
	{ Predicate = syn<->(role<->ROLE..act<->be..vcat<->copula..
			    reply<->REPLY..gcat<->GCAT..gap<->GAP..rel<->REL) ..
	              sem<->(pred<->isa),
	  Subject = syn<->(reply<->SUBJ_REPLY)..sem<->SUBJ,
	  Predicate = sem<->(subj<->SUBJ), % add the subj feature to the semantics of Predicate
	  Verb = syn<->(act<->be..vcat<->copula..role<->ROLE) ..
	         sem<->(pred<->isa)
	},
	copula(Verb),
	subject(Subject),                   % e.g., '......John...', '.......she'
	% check to make sure subject replies is a list, and then put the reply together
	{ is_list(SUBJ_REPLY) ->
	  append(SUBJ_REPLY,[is],REPLY);
	  REPLY=[SUBJ_REPLY,is] }.

query_copula_predicate(Predicate) -->         % 'is John a customer of the bank'
	{ Predicate = syn<->(role<->ROLE..act<->be..vcat<->copula..
			    reply<->REPLY..gcat<->GCAT..gap<->GAP..rel<->REL) ..
	              sem<->(pred<->isa..dobj<->DIRECT_OBJECT_SEMANTICS),
	  Subject = syn<->(reply<->SUBJ_REPLY)..sem<->SUBJ,
	  Predicate = sem<->(subj<->SUBJ), % add the subj feature to the semantics of Predicate
	  Verb = syn<->(act<->be..vcat<->copula..role<->ROLE) ..
	         sem<->(pred<->isa),
	  Direct_Object = syn<->(gcat<->GCAT..gap<->GAP..rel<->REL..reply<->DIRECT_OBJECT_REPLY) ..
	                  sem<->DIRECT_OBJECT_SEMANTICS },
	copula(Verb),
	subject(Subject),                   % e.g., '......John...', '.......she'
	direct_object(Direct_Object),
	% check to make sure subject and direct object replies are lists, and then put the reply together
	{ (is_list(SUBJ_REPLY),is_list(DIRECT_OBJECT_REPLY)) -> 
	  append(SUBJ_REPLY,[is|DIRECT_OBJECT_REPLY],REPLY);
	  REPLY=[SUBJ_REPLY,is,DIRECT_OBJECT_REPLY] }.
	  

query_copula_predicate(Predicate) -->         % 'is John at the bank'
	{ Predicate = syn<->(role<->ROLE..act<->be..vcat<->copula..
			    reply<->REPLY..gcat<->GCAT..gap<->GAP..rel<->REL) ..
	              sem<->(pred<->isa..adjs<->Adjuncts),
	  Subject = syn<->(reply<->SUBJ_REPLY)..sem<->SUBJ,
	  Predicate = sem<->(subj<->SUBJ), % add the subj feature to the semantics of Predicate
	  Verb = syn<->(act<->be..vcat<->copula..role<->ROLE) ..
	         sem<->(pred<->isa),
	  Adjunct = syn<->(advp<->PREP..reply<->ADJUNCT_REPLY) ..
	            sem<->ADJUNCT_SEMANTICS,
	  Adjuncts = [ADJUNCT_SEMANTICS] },
	copula(Verb),
	subject(Subject),                   % e.g., '......John...', '.......she'
	adverbial_prep_phrase(Adjunct),
	% check to make sure subject and adjunct replies are lists, and then put the reply together
	{ (is_list(SUBJ_REPLY),is_list(ADJUNCT_REPLY)) -> 
	  append(SUBJ_REPLY,[is|ADJUNCT_REPLY],REPLY);
	  REPLY=[SUBJ_REPLY,is,ADJUNCT_REPLY] }.	

query_copula_predicate(Predicate) -->         % 'is John rich'
	{ Predicate = syn<->(role<->ROLE..act<->be..vcat<->copula..
			    reply<->REPLY..gcat<->GCAT..gap<->GAP..rel<->REL) ..
	              sem<->(pred<->isa..mod<->MOD),
	  Subject = syn<->(reply<->SUBJ_REPLY)..sem<->SUBJ,
	  Predicate = sem<->(subj<->SUBJ), % add the subj feature to the semantics of Predicate
	  Modifier = sem<->(mod<->MOD)..syn<->(reply<->MODIFIER_REPLY),
	  Verb = syn<->(act<->be..vcat<->copula..role<->ROLE) ..
	         sem<->(pred<->isa) },
	copula(Verb),
	subject(Subject),                   % e.g., '......John...', '.......she'
	adjective(Modifier),
	% check to make sure subject and modifier replies are lists, and then put the reply together
	{ is_list(SUBJ_REPLY) -> 
	  append(SUBJ_REPLY,[is,MODIFIER_REPLY],REPLY);
	  REPLY=[SUBJ_REPLY,is,MODIFIER_REPLY] }.	

% WRM--stopped adding query replies here on 10/4/02.

% Note: a superlative cannot appear where the adjective is. See P. 53 of ACE manual, superlatives may not
% be used alone, only in the attributive position. So "John's car is the biggest." is disallowed although
% "John's car is the biggest car." is acceptable.

% handle two place adjectives, e.g., 'is identical to', 'is different from',...
query_copula_predicate(Predicate) -->        % 'is the account identical to'...
	{ Predicate = syn<->(role<->ROLE..act<->be..vcat<->copula..gcat<->GCAT..gap<->GAP..rel<->REL) ..
	              sem<->(pred<->isa..dobj<->DOBJ..mod<->Two_Arg_Lambda_Expression),
	  Subject = sem<->SUBJ,
	  Predicate = sem<->(subj<->SUBJ), % add the subj feature to the semantics of Predicate
	  Verb = syn<->(act<->be..vcat<->copula..role<->ROLE),
	  NP = syn<->(gcat<->GCAT..gap<->GAP..rel<->REL) ..
	       sem<->DOBJ },
	copula(Verb),
	subject(Subject),                   % e.g., '......John...', '.......she'
	two_place_adjective(Two_Arg_Lambda_Expression),     % e.g., 'is identical to', 'is different from...'
	np(NP).

% handle comparative adjectives, e.g., 'is older than', 'is faster than', 'is more expensive than'
% ACE requires comparatives to have 'than' followed by a noun phrase
query_copula_predicate(Predicate) --> 
	{ Predicate = syn<->(role<->ROLE..act<->be..vcat<->copula..gcat<->GCAT..gap<->GAP..rel<->REL) ..
	              sem<->(pred<->isa..dobj<->DOBJ..mod<->COMPARATOR),
	  Subject = sem<->SUBJ,
	  Predicate = sem<->(subj<->SUBJ), % add the subj feature to the semantics of Predicate
	  Verb = syn<->(act<->be..vcat<->copula..role<->ROLE),
	  NP = syn<->(gcat<->GCAT..gap<->GAP..rel<->REL) ..
	       sem<->DOBJ,
	  COMPARATOR = X^Y^[greaterThan, [measure, X, SUO_concept], [measure, Y, SUO_concept]] },
	copula(Verb),
	subject(Subject),                   % e.g., '......John...', '.......she'
	comparative_adjective(SUO_concept),  % e.g., 'is older than', 'is faster than', 'is more expensive than'
        [than],                              % ACE requires comparatives to have 'than' followed by a noun phrase
	np(NP).


%-------------------------------
% PARTS OF SPEECH TO LEXICON
%-------------------------------

auxiliary --> [does].

wh_determiner(which)-->[which].  % e.g., in 'which boy'
wh_determiner(what)-->[what].    % e.g., in 'what card'


wh_pronoun(Noun)--> % e.g., 'Who enters the bank?'
	{
	% Syntactic grammatical features...
	 Noun = syn<->(ncat<->person..gcat<->person..gap<->who) ..
	        sem<->(head<->'?who'..type<->SUO_concept),
	 map_gcat_to_suo(person,SUO_concept)
	},
	[who].

wh_pronoun(Noun)--> % e.g., 'Who gives what to whom?'
	{
	% Syntactic grammatical features...
	 Noun = syn<->(ncat<->person..gcat<->person..gap<->whom) ..
	        sem<->(head<->'?whom'..type<->SUO_concept),
	 map_gcat_to_suo(person,SUO_concept)
	},
	[whom].

wh_pronoun(Noun)--> % e.g., 'What enters the bank?'
	{
	% Syntactic grammatical features...
	 Noun = syn<->(ncat<->object..gcat<->object..gap<->what) ..
	        sem<->(head<->'?what'..type<->SUO_concept),
	 map_gcat_to_suo(object,SUO_concept)
	},
	[what].

% query_verb_phrase_modifier(+WH_Words,-QVar_Name,+Contiguous,-Modifier).

% WH_Words is a list of one or two elements.
% Continguous is one of [no,reversible,separated] where
% --no means that there is only one word or if there are two they must occur in sequence
%              either one word like 'Where' or 'When', or
%              two words in sequence like 'How long' or 'How often' that are not separable
% --reversible means that there are two words [WH, Particle] that appears either in
%              sequence separated as in 'WH...Particle?' (e.g., 'Where...from?') or in
%              reverse sequence not separated as in 'Particle WH...?' (e.g., 'From where...?')
% --separated means that there are two words [A,B] that must appear in
%              sequence separated as in 'WH...Particle?' (e.g., 'Where...to?')
%              

% See Table 8, pages 61-62.

% query_verb_phrase_modifier([WH_WORD],VAR_NAME,no,MODIFICATION).
%   handles sentences like 'WH_WORD...?' (e.g., 'Where...?', 'When...?', and 'How...?')

% query_verb_phrase_modifier([WH1,WH2],VAR_NAME,no,MODIFICATION).
%   handles sentences like 'WH1 WH2 ...?'        (e.g., 'Since when...?','Until when...?','How long...?','How often...?','With whom...?')

% query_verb_phrase_modifier([WH_WORD,Particle],VAR_NAME,reversible,MODIFICATION).
%   handles sentences like 'WH_WORD...Particle?' (e.g., 'Where...from?', 'Who...with?')
%   and sentences like 'Particle WH_WORD...?'    (e.g., 'From where...?', 'Who with...?)

% query_verb_phrase_modifier([WH_WORD,Particle],VAR_NAME,separated,MODIFICATION).
%   handles ONLY sentences like 'WH_WORD...Particle?' (e.g., 'Where...to?' and 'Where...into?')

% location
query_verb_phrase_modifier([where],'?where',no,location).

% origin
query_verb_phrase_modifier([where,from],'?where_from',reversible,location).   % 'Where...from?' or 'From where...?'
 
% direction
query_verb_phrase_modifier([where,to],'?where_to',separated,direction).     % 'Where...to?' but NOT 'To...where?'
query_verb_phrase_modifier([where,into],'?where_into',separated,direction).   % 'Where...into?' but NOT 'Into...where?'

% time
query_verb_phrase_modifier([when],'?when',no,time).

% start
query_verb_phrase_modifier([since,when],'?since_when',no,start).

% end
query_verb_phrase_modifier([until,when],'?until_when',no,end).

% duration
query_verb_phrase_modifier([how,long],'?how_long',no,duration).

% frequency
query_verb_phrase_modifier([how,often],'?how_often',no,frequency).

% instrument
query_verb_phrase_modifier([what,with],'?what_with',reversible,instrument).  % 'What...with?' or 'With what...?'

% instrument
query_verb_phrase_modifier([how],'?how',no,manner).

% comitative
query_verb_phrase_modifier([who,with],'?who_with',reversible,comitative).   % 'Who...with?' or 'With who...?'
query_verb_phrase_modifier([with,whom],'?with_whom',no,comitative).         % 'With whom...?' but NOT 'With...whom?' and NOT 'Whom...with?'

% COPULA VERBS

% Note: That the copula verb 'be' is handled directly in the grammar
% rules below and is not a verb lexicon entry. This separation allows
% handling copula and non-copula verb forms esp. in queries more easily.

% In queries like 'Is Jill a banker?' we can treat the part after 'Is' (here 'Jill a banker?') 
% as a copula predicate if we allow 'is' to be skipped over (i.e., treat 'Jill a banker?' as if
% if it were 'Jill [is] a banker'.  Similarly for other copula constructions: 'Is Fred's dog larger than
% Bill's dog?', 'Is Joe's car the fastest car?', etc.
copula(Verb) -->
	{ Verb = syn<->(act<->be..vcat<->copula..role<->query(yes_no)..aux<->no) ..
	         sem<->(pred<->isa) },
	[].

% In queries like 'Who is the banker?' we treat copula 'is' normally, just as in
% declarative sentences.
copula(Verb) -->
	{ Verb = syn<->(act<->be..vcat<->copula..role<->query(wh)..aux<->no) ..
	         sem<->(pred<->isa) },
	[is].

% NON-COPULA VERBS

% Note that in queries without an auxiliary (e.g., 'Who enters the store?')  the verbs should be
% like 'enters', 'inserts', 'opens', etc.  whereas in queries with auxiliaries (e.g., 'Does John enter the store?')
% the verbs take on their base form like 'enter', 'insert', 'open', etc.

% NON-COPULA VERBS IN YES/NO QUERIES OR WH- QUERIES WITH AUXILIARIES
% (e.g., 'Does John enter the store?') or (e.g., 'Who does John beat?')

% handle either single word (e.g., enter) or compound word (e.g., dry clean) verbs
% KIND_OF_QUERY can be either yes_no or wh.
verb(Verb) -->
	{ Verb = syn<->(act<->Root..vcat<->VCAT..role<->query(KIND_OF_QUERY)..aux<->yes) ..
	         sem<->(pred<->SUO_concept),
	  verb_in_lexicon(Word,Root,VCAT,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID),
	  atom(Root) },
	[Root].			% single word verb

% Currently compound word verbs in queries with auxiliaries are not handled.

% NON-COPULA VERBS IN WH-QUERIES WITHOUT AUXILIARIES (e.g., 'Who enters the store?')

% handle either single word (e.g., enter) or compound word (e.g., dry clean) verbs
verb(Verb) -->
	{ Verb = syn<->(act<->Root..vcat<->VCAT..role<->query(wh)..aux<->no) ..
	         sem<->(pred<->SUO_concept),
	  verb_in_lexicon(Word,Root,VCAT,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID),
	  atom(Word) },
	[Word].			% single word verb

verb(Verb) -->
	{ Verb = syn<->(act<->Root..vcat<->VCAT..role<->query(wh)..aux<->no) ..
	         sem<->(pred<->SUO_concept),
	  verb_in_lexicon([First|Rest],Root,VCAT,Number,Kind_of_verb,Event_or_state,SUO_concept,Synset_ID) },
	[First|Rest].		% compound word verb

%-------------------------------
% TESTS
%-------------------------------

% yes-no queries to test

% copula: 'is...?' queries
test(query,[is,the,banker,rich],yes).
test(query,[is,the,banker,a,man],yes).
test(query,[is,the,dog,red],yes).
test(query,[is,'John','\'',s,dog,different,from,'Mary','\'',s,dog],yes).
test(query,[is,'John','\'',s,dog,more,expensive,than,'Mary','\'',s,dog],yes).
test(query,[is,'John','\'',s,dog,the,most,expensive,dog],yes).

% non-copula: 'does...?' queries

test(query,[does,'John',breathe],yes).
test(query,[does,'John',beat,'Mr','Miller'],yes).
test(query,[does,'Mary',give,a,book,to,'John'],yes).
test(query,[does,'Mary',give,'John',a,book],yes).

% WH queries to test

% copula: 'wh-- is...?' queries
test(query,[what,is,valid],yes).
test(query,[who,is,the,teller],yes).
test(query,[what,is,in,the,slot],yes).

% non-copula: 'wh-- verb...?' queries
test(query,[who,enters,a,card],yes).
test(query,[which,customer,enters,a,card],yes).
test(query,[who,enters,what],yes).

% Gapped constructions
test(query,[who,does,'Mary',beat],yes).              % Mary beats who?
test(query,[what,does,the,dog,eat],yes).             % The dog eats what?

test(query,[who,does,'John',give,a,card,to],yes).    % John gives a card to who?
test(query,[what,does,'Mary',give,to,'John'],yes).   % Mary gives what to John?

% Query verb phrase modifiers

%...non-copula...

test(query,[where,does,'John',work],yes).
test(query,[when,does,the,man,arrive],yes).
test(query,[how,does,the,dog,run],yes).

test(query,[since,when,does,the,employee,work],yes).
test(query,[how,long,does,the,train,stop],yes).
test(query,[how,often,does,the,milkman,come],yes).
test(query,[with,whom,does,the,employee,enter,the,room],yes).

test(query,[where,does,'John',insert,the,card,into],yes).
test(query,[where,does,the,machine,move,to],yes).

test(query,[where,does,the,train,arrive,from],yes).
test(query,[from,where,does,the,train,arrive],yes).
test(query,[with,what,does,'John',open,the,door],yes).
test(query,[what,does,'John',open,the,door,with],yes).
test(query,[who,does,'John',enter,the,room,with],yes).
test(query,[with,whom,does,'John',enter,the,room],yes).

%...copula...

test(query,[until,when,is,the,shop,open],yes).
test(query,[when,is,the,cat,hungry],yes).
test(query,[how,long,is,the,cat,hungry],yes).
test(query,[how,often,is,the,cat,hungry],yes).
test(query,[since,when,is,the,cat,thirsty],yes).

%...copulas using where, when, and how...recently added (not in ACE?)
test(query,[where,is,'John'],yes).
test(query,[when,is,the,party],yes).
test(query,[how,is,the,dog],yes).

% Specific tests for handling embedded sentences in queries with auxiliaries
test_parse_q1 :- Words = ['Mary',beat],
	Features = syn<->(gcat<->person..gap<->who..rel<->yes..role<->query(yes_no))..sem<->SEMANTICS,
	phrase(embedded_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_q2 :- Words = [who,does,'Mary',beat],
	phrase(query(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_q3 :- Words = [the,dog,eat],
	Features = syn<->(gcat<->object..gap<->what..rel<->yes)..sem<->SEMANTICS,
	phrase(embedded_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_q4 :- Words = [what,does,the,dog,eat],
	phrase(query(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_q5 :- Words = ['John',give,a,card,to],
	Features = syn<->(gcat<->person..gap<->who..rel<->yes)..sem<->SEMANTICS,
	phrase(embedded_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_q6 :- Words = [who,does,'John',give,a,card,to],
	phrase(query(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).

test_parse_q7 :- Words = ['Mary',give,to,'John'],
	Features = syn<->(gcat<->object..gap<->what..rel<->yes)..sem<->SEMANTICS,
	phrase(embedded_sentence(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).	

test_parse_q8 :- Words = [what,does,'Mary',give,to,'John'],
	phrase(query(Features),Words),
	write('Parsed OK:'),write_sent(Words),
	display_feature_structure(Features).	

% Query complements of prepositions
% test(query,[what,does,'John',work,with],yes).
% test(query,[with,what,does,'John',work],yes).
% test(query,[who,does,'John',talk,with],yes).
% test(query,[with,whom,does,'John',talk,with],yes).

% Query of-noun phrase
% test(query,[whose,card,is,valid],yes).
% test(query,[the,code,of,what,is,valid],yes).

% Using WHOM
% test(query,[with,whom,does,the,employee,enter,the,room],yes).
