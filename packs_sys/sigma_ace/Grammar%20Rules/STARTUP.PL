% System definition and startup file for CELT.
% CELT Version 2(b)--Simple sentences, WH queries, and Yes/No queries. Both syntax and semantics. 

% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

% DECLARATIONS

:-multifile test/3.
:-multifile copula/3.
:-multifile verb/3.
:-multifile np/3.
:-multifile embedded_sentence/3.
:-multifile noun_in_lexicon/7.
:-multifile proper_noun_in_lexicon/7.
:-multifile verb_in_lexicon/8.
:-multifile adjective_in_lexicon/5.
:-multifile adverb_in_lexicon/3.
:-multifile translation_template/4.
:-multifile relation/4.

% declare syntactic and semantic features used in ACE / CELT rules, see
% end of this file for documentation of each feature.

g_features([syn,case,gender,act,vcat,ncat,gcat,gap,rel,max,advp,       %syntactic features
	    role,det,aux,qvars,count,reply,                            %syntactic features (added for queries, mass vs count nouns...)
	    sem,noun,head,type,mod,sub,poss,quan,of,apos,id,           %semantic features (np)
	    pred,subj,dobj,iobj,                                       %semantic features (predicate, vp, sent)
            adjs,adv,prep,aobj]).                                      %semantic features (adjuncts, adverbs, prepositions in adjuncts)

% FILES

all_celt_files([% First file is for TEK GULP 3.1, support for unification grammars
   	        swi_gulp_iff_sign, 
	        % Next files are DCG rules and supporting
		tools,lexicon,np,sentence,query,semantics,tests,warnings,
		% Next files are lexicon files generated from WordNet using the Perl 
		% program wn2assoc3 and the Prolog create_lexicon program.
		celt_common_noun_lexicon,celt_proper_noun_lexicon,celt_verb_lexicon,
		% Latest SUMO...
		kif,
		% WordNet 1.6 files...
		wn_s,wn_g,wn_hyp,
		% Files for inheritance reasoning over either SUMO or WordNet
		infer_sumo,infer_wordnet,
		% Files for sample user lexicon extensions, primarily for the Air Task Order domain,
		% these do not need to be loaded when efficiency is paramount.
		user_lexicon,
		% Files for interfacing with the automatic theorem prover (e.g., SNARK, OTTER, or VAMPIRE)
		atp_interface,
		% Next are files for catching morphological variants of items in the lexicon
		morphy,verb_excepts,noun_excepts,adj_excepts,adv_excepts,
		% Last are files for regression testing, these do not need to be loaded when efficiency is paramount.
		test_suite]).

load_CELT :- all_celt_files(CELT_FILES),consult(CELT_FILES).

:-load_CELT.

compile_CELT :- write_ln('Compiling all CELT files...this may take several minutes...'),
                all_celt_files(CELT_FILES),
                qcompile(CELT_FILES),
		write_ln('All CELT files compiled! Ready!').

% FLAGS

:-no_atp_interface. % ATP interface is absent or disabled.

% Optional Flags, Replace the ones above with these if appropriate:
% :-use_atp_interface. % ATP interface is present.

:-write_ln('To test translation of the test sentences try test_v_2b. To try your own, use eng2log.').
:-write_ln('E.g., eng2log("John arrives at the bank."),  eng2log("The old customer enters the Swiss bank.").').
:-write_ln('and questions... eng2log("What dog enters the bank?"),  eng2log("Does a dog enter the bank?").').
:-write_ln('and for string output use eng2log/5, e.g., try...eng2log("John pokes the dog.",X,Y,Z,W),write(X),write(Y),write(Z),write(W).').
:-write_ln('Use top_level/0 as a convenience function to avoid repeated calls to eng2log. It can also check lexicon entries.').

test_v_2b :- test_parse_sent_all. % Note: running test_parse_np_all is also useful for checking NPs.

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
role,   is the role this speech act plays, one of [assertion, query(WH), command] for sentences, questions, or imperatives respectively.
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





