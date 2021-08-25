%-------------------------------
% CODE FOR SEMANTICS
%-------------------------------

% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading CELT Parser Version 2(b) ...semantics...syntactic and semantic translation for simple sentences and queries'),nl.

%-------------------------------
% The code in this file takes the semantic grammatical
% features of a successful parse and generates SUO
% code from those features.
%-------------------------------

% create_var_name(+Common_noun,-Sigma_var_name)
% takes a common noun (e.g., cat) and creates a Sigma
% variable name from it (e.g., '?cat'). [Note: 63 is the char code for ?]

create_var_name(Common_noun,Sigma_var_name) :- atom(Common_noun),name(Common_noun,Letters),name(Sigma_var_name,[63|Letters]).

create_var_name(Common_noun,Sigma_var_name) :- var(Common_noun),!,fail. % Should not happen. This clause prevents infinite loops if it does

% also allows a list of words (e.g., [credit,card])
% and creates a name like '?creditcard'.

create_var_name([First|Rest],Sigma_Name) :- concat_all([First|Rest],All),create_var_name(All,Sigma_Name).

% create_constant_name(+Common_noun,-Sigma_constant_name)
% takes a proper noun (e.g., Felix) and creates a Sigma
% constant name from it (e.g., 'Felix').

create_constant_name(Proper_noun,Proper_noun) :- atom(Proper_noun).

% also allows a list of words (e.g., ['George','Brown'])
% and creates a name like 'George_Brown'.

create_constant_name([First|Rest],Sigma_Name) :- concat_all([First|Rest],All),create_constant_name(All,Sigma_Name).

% map_gcat_to_suo(+gcat, -SUO_concept)
% maps GCAT, which is one of [person, object, time] or 'entity' for any of these.

map_gcat_to_suo('time','TimePosition').
map_gcat_to_suo('person','Human').
map_gcat_to_suo('object','Physical').
map_gcat_to_suo('entity','Entity').

% traverse_features_and_return_existential(+Nonterminal,+Features,-EXISTENTIAL, -SPEECH_ACT)
% traverse feature structure semantics and build up SUO clauses
% as each constituent is traversed, finally returning an existential expression
% of the variables if there are any, as is typically the case.

% +Nonterminal is either 'sentence', 'query',  or 'np', that is all that is handled now.

% -SPEECH_ACT is one of [assertion, query, imperative]

traverse_features_and_return_existential(query,Sentence_Features,EXISTENTIAL,SPEECH_ACT) :-

	!, % only use this clause once for queries

	Sentence_Features = syn<->(role<->query(QUERY_TYPE)..qvars<->QVARS)..sem<->SEMANTICS,

	SPEECH_ACT = query(QUERY_TYPE,QVARS),

	% for queries generate SUMO forms as if they were from a sentence...
	% generate a list of all the SUMO forms for the semantics and a list of all variables referred to
	traverse_features_and_build_forms(sentence,Sentence_Features,SUMO_FORMS,[],VARS),
	
        % filter out constants in variable list and return output in form like (exists (?x ?y) ...)
	generate_existential(VARS,QVARS,SUMO_FORMS,VARS_OUT,EXISTENTIAL).


traverse_features_and_return_existential(Nonterminal,Sentence_Features,EXISTENTIAL,SPEECH_ACT) :-

	% check that Nonterminal is either 'sentence', 'query',  or 'np'
	((not(memberchk(Nonterminal,[sentence, query, np])) ->
	  write('Only sentence, query, or np allowed, not feature: '),write(Nonterminal),nl); true),

	SPEECH_ACT = assertion,

	(check_for_multi_arg_sentence_relation(Sentence_Features) ->

		    % handle predefined sentences where multiple args are used to build a single relation specially:
	            build_multi_arg_relation(Sentence_Features,EXISTENTIAL);

	            % otherwise, for most all normal sentences, build the resulting forms in this way:

		    % first generate a list of all the SUMO forms for the semantics and a list of all variables referred to
		    (traverse_features_and_build_forms(Nonterminal,Sentence_Features,SUMO_FORMS,[],VARS),
	
		    % filter out constants in variable list and return output in form like (exists (?x ?y) ...)
		    generate_existential(VARS,[],SUMO_FORMS,VARS_OUT,EXISTENTIAL))).

% A multi-arg sentence where many arguments to a relation are specified all at once
% has a non-empty 'specs' grammatical feature in its semantics.
check_for_multi_arg_sentence_relation(Sentence_Features) :-
	Sentence_Features = sem<->(specs<->SPECS), not(var(SPECS)), not(SPECS==empty).

% traverse_features_and_build_forms(+Nonterminal,+Sentence_Features,-SUMO_FORMS,+VARS_IN,-VARS_OUT)
% traverse feature structure semantics and build up SUO clauses
% as each constituent is traversed, also keeping track of variables
% referred to in all the clauses generated.

% +Nonterminal is either 'sentence' or 'np', that is all that is handled now.

traverse_features_and_build_forms(sentence,Sentence_Features,SUMO_FORMS,VARS_IN,VARS_OUT) :-
	Sentence_Features = syn<->SYNTAX..sem<->SEMANTICS,
        traverse_sentence_semantic_frames(SEMANTICS,[],SUMO_FORMS,VARS_IN,VARS_OUT).

traverse_features_and_build_forms(np,NP_Features,SUMO_FORMS,VARS_IN,VARS_OUT) :-
	NP_Features = syn<->SYNTAX..sem<->SEMANTICS,
	traverse_complement_if_present(SEMANTICS,[],SUMO_FORMS,VARS_IN,VARS_OUT,NP_VAR).

% generate_existential(+VARS_IN,+QUERY_VARS,+FORMS_IN,-VARS_OUT,-FORMS_OUT)
% takes a list of Sigma variables VARS_IN and removes any query variables in QUERY_VARS
% and drops any constants remaining in the variable list to return VARS_OUT. It also creates an existential expression
% like (exists (?dog ?cat) (and (instance ?dog Canine) (instance ?cat Feline)))
% if the final variable list is not empty. QUERY_VARS is [] for assertions and consists
% of those free variables to be searched for in the case of a query.

generate_existential(VARS_IN,QUERY_VARS,FORMS_IN,VARS_OUT,FORMS_OUT) :-

	% filter out constants (e.g., John-1) from other variables (e.g., ?customer) in VARS
	drop_constants_and_query_variables(VARS_IN,QUERY_VARS,VARS_OUT),

        % ...and wrap it with a there exists clause for the variables...
        ((VARS_OUT == []) -> FORMS_OUT = [and|FORMS_IN]; FORMS_OUT = [exists, VARS_OUT, [and|FORMS_IN]]).	

% traverse_sentence_semantic_frames(+SEMANTICS,+IN,-OUT).
% traverses all the of the following semantic features:
% subj -- subject NP
% dobj -- direct object NP
% iobj -- indirect object NP
% adjs -- adjuncts for sentence
% mod  -- if sentence is copula and a modifier is provided


% COPULA SENTENCES

% handle copula sentences without a direct object: e.g., 'John is rich', 'The customer is at the bank', etc.
traverse_sentence_semantic_frames(SEMANTICS,IN,OUT,VARS_IN,VARS_OUT) :-
	SEMANTICS = pred<->isa..subj<->SUBJECT..dobj<->DIRECT_OBJECT..
                    adjs<->ADJUNCTS..mod<->MODIFIER,

		    (DIRECT_OBJECT == 'empty'; var(DIRECT_OBJECT)),
		    !,		% Handle sentences with 'is' and no direct object uniquely.		    

        % identify subject variable
        traverse_subject_of_sentence(SUBJECT,IN,OUT1,VARS_IN,VARS_OUT1,SUBJECT_VAR),

        % add clauses for copula modifier if present
        traverse_modifier_if_present(MODIFIER,SUBJECT_VAR,OUT1,OUT2,VARS_OUT1,VARS_OUT2),

        % add clauses for adjuncts if present
        traverse_adjuncts_if_present(ADJUNCTS,SUBJECT_VAR,OUT2,OUT,VARS_OUT2,VARS_OUT).

% handle copula sentences with a direct object and a modifier that
% is a two-place lambda expression for comparative adjectives or two
% place adjectives: 'he is older than John', 'the box is different from the gift', etc.
traverse_sentence_semantic_frames(SEMANTICS,IN,OUT,VARS_IN,VARS_OUT) :-
	SEMANTICS = pred<->isa..subj<->SUBJECT..dobj<->DIRECT_OBJECT ..
                    adjs<->ADJUNCTS..mod<->MODIFIER,

        % Handle sentences with 'is' and a direct object uniquely.
        not(DIRECT_OBJECT == 'empty'), not(var(DIRECT_OBJECT)),

        % Also require that the modifier be present and a two-argument lambda expression (e.g., X^Y^[equal,X,Y] )
	not(MODIFIER == 'empty'), not(var(MODIFIER)), MODIFIER = X^Y^Lambda_Expression,!,

        % identify subject variable
        traverse_subject_of_sentence(SUBJECT,IN,OUT1,VARS_IN,VARS_OUT1,SUBJECT_VAR),

        % identify direct object variable
	traverse_complement_if_present(DIRECT_OBJECT,OUT1,OUT2,VARS_OUT1,VARS_OUT,DIRECT_OBJECT_VAR),

	% now apply lambda expression of modifier to the subject...
	supply_argument(MODIFIER,SUBJECT_VAR,ONE_PLACE_LAMBDA_EXPRESSION),

	% now apply lambda expression of modifier to the direct object...
	supply_argument(ONE_PLACE_LAMBDA_EXPRESSION,DIRECT_OBJECT_VAR,LAMBDA_RESULTS),	

	% and add this expression to those in forms generated so far...
	OUT = [LAMBDA_RESULTS|OUT2].

% handle copula sentences with a direct object: 'The old customer is a regular employee at the bank'
traverse_sentence_semantic_frames(SEMANTICS,IN,OUT,VARS_IN,VARS_OUT) :-
	SEMANTICS = pred<->isa..subj<->SUBJECT..dobj<->DIRECT_OBJECT ..
                    adjs<->ADJUNCTS..mod<->MODIFIER,

        % Handle sentences with 'is' and a direct object uniquely.
        not(DIRECT_OBJECT == 'empty'), not(var(DIRECT_OBJECT)), !,

        % identify subject variable
        traverse_subject_of_sentence(SUBJECT,IN,OUT1,VARS_IN,VARS_OUT1,SUBJECT_VAR),

        % add clauses for direct object if present
	% example sentence: 'John is a customer'
        traverse_object_of_copula(DIRECT_OBJECT,SUBJECT_VAR,OUT1,OUT2,VARS_OUT1,VARS_OUT2),

        % add clauses for adjuncts if present
        traverse_adjuncts_if_present(ADJUNCTS,SUBJECT_VAR,OUT2,OUT,VARS_OUT2,VARS_OUT).

% traverse_subject_of_sentence(+COMPLEMENT,+SUO_IN,-SUO_OUT,+VARS_IN,-VARS_OUT, -COMPLEMENT_VAR)
% traverses the semantic features of the subject of a copula sentence:
%
% noun,       is the actual noun that is accepted as the main noun in the noun phrase (e.g., 'man')
% head,       is the variable name created for the primary object referred to in this complement (e.g., '?Man')
% type,       is the Sigma concept name for the type of thing which the head is (e.g., 'Man')
% id,         is the WordNet Synset ID for the noun that was used to determine the Sigma concept name for the type
% mod,        is the Sigma name for a modifier (adjective) that applies to the head
% sub,        is the sentence semantics of a relative sentence that modifies the head ('sub' for subordinate sentence).
% poss,       is the Sigma Concept name for the type of object in the possessive position for the head (e.g., 'John' in 'John's dog')
% owner,      is the WordNet Synset ID for the object in the possessive (owner) position for the head (e.g., 'John' in 'John's dog')
% of,         is the Sigma Concept name for the type of object in the of-preposition positions of the head (e.g., 'John' in 'dog of John')
% of_id,      is the WordNet Synset ID for the object that is in the of-preposition positions of the head (e.g., 'John' in 'dog of John')
% quan,       is either 'universal' or (more commonly) 'existential' depending on the determiner that modifies the head
% apos,       is a common noun used in aposition to the proper noun that is the head of this NP.

traverse_subject_of_sentence(Complement,IN,OUT,VARS_IN,VARS_OUT,SUBJ_VAR) :-
	Complement = head<->HEAD..type<->TYPE..id<->ID..mod<->MOD..sub<->SUB..
                     poss<->OWNER..owner<->OWNER_ID..           % owner in a possessive construct like "X's Y"
                     of<->OF..of_id<->OF_ID..apos<->APOS,       % owner in a possessive construct like "Y of X"

	%ensure variable name in HEAD is unique given that some variable names are already chosen in VARS
	ensure_unique(HEAD,VARS_IN,SUBJ_VAR),

        %add the new variable name for the subject variable to the others already accumulated
	VARS_W_SUBJ_VAR = [SUBJ_VAR|VARS_IN],

	%handle subordinate sentence information
	% add_subordinate_descriptors will update the variables (from VARS_W_SUBJ_VAR to UPDATED_VARS),
	% and the forms (from IN to RESULT1) according to the semantics of the subordinate sentence.
	(not(var(SUB)),not(SUB=='empty') ->
	    add_subordinate_descriptors(SUBJ_VAR,TYPE,SUB,IN,RESULT1,VARS_W_SUBJ_VAR,UPDATED_VARS);
	    RESULT1 = IN, UPDATED_VARS = VARS_W_SUBJ_VAR),

	% handle possessive constructs
	handle_possessives_and_of_constructs(OWNER_ID,OWNER,
					     SUBJ_VAR,ID,TYPE,RESULT1,RESULT2),

	% handle of-preposition constructs just like possessive
	handle_possessives_and_of_constructs(OF_ID,OF,
					     SUBJ_VAR,ID,TYPE,RESULT2,RESULT3),

	% handle mod feature
	(not(var(MOD)),not(MOD=='empty') -> modify_noun_phrase(SUBJ_VAR,TYPE,MOD,RESULT3,RESULT4); RESULT4 = RESULT3),

	% handle apos (aposition) like extra type info
	(not(var(APOS)),not(APOS=='empty') -> RESULT5 = [[instance,SUBJ_VAR,APOS]|RESULT4]; RESULT5 = RESULT4),

	%handle type feature
	specify_type(SUBJ_VAR,TYPE,RESULT5,OUT,UPDATED_VARS,VARS_OUT).


% handle_possessives_and_of_constructs(+POSSESSOR_SYNSET_ID,+POSSESSOR,
%                                      +OWNED_ITEM_VAR,+OWNED_ITEM_ID,+OWNED_ITEM_TYPE,+FORMS_IN,-FORMS_OUT)
% handles possessive and of-constructs in CELT. These are currently treated identically,
% just as they are in ACE.

% POSSESSOR_SYNSET_ID is the WordNet ID of the word that is the possessor. It
% can be undefined or 'empty' if there is no WordNet correspondence as this is
% just used for testing if the possessor can be an agent.

% POSSESSOR is either a SUMO instance, e.g., for a specific person or owner, or
% a SUMO subclass for a definite or indefinite reference. The first case occurs
% "John's dog" and the second in "the man's dog" or "a man's dog".

% OWNED_ITEM_VAR is the variable for the item that is owned. OWNED_ITEM_TYPE
% is the SUMO type of the owned item.

% OWNED_ITEM_ID is the WordNet ID of the word that is the owned.

% OWNED_ITEM_TYPE is the SUMO type of the object that is the owned.

handle_possessives_and_of_constructs(POSSESSOR_SYNSET_ID,POSSESSOR,OWNED_ITEM_VAR,OWNED_ITEM_ID,OWNED_ITEM_TYPE,FORMS_IN,FORMS_OUT) :-
	(var(POSSESSOR);POSSESSOR=='empty') -> FORMS_OUT = FORMS_IN;
	        get_possessive_construct_template(POSSESSOR_SYNSET_ID,POSSESSOR,OWNED_ITEM_ID,OWNED_ITEM_TYPE,TEMPLATE),
		apply_lambda_expression_of_two_arguments(TEMPLATE,POSSESSOR,OWNED_ITEM_VAR,RESULT),
		FORMS_OUT = [RESULT|FORMS_IN].

% get_possessive_construct_template(+POSSESSOR,+OWNED_ITEM_TYPE,-TEMPLATE) takes a possessor (X) and
% a type of owned item (Y) that appear either in a possessive construct such as "X's Y" or in an of-construct
% such as "the Y of X", both of which are treated the same in ACE and currently in CELT. It returns
% a two-arg lambda expression to generate the code for the construct. The POSSESSOR is a SUMO instance or
% subclass and the OWNED_ITEM_TYPE is a SUMO subclass.

% e.g., to handle "John's swimming" or "John's arriving" or "the farmer's work"
get_possessive_construct_template(POSSESSOR_SYNSET_ID,POSSESSOR,OWNED_ITEM_ID,OWNED_ITEM_TYPE,TEMPLATE) :-
	is_subclass_or_equal(OWNED_ITEM_TYPE,'Process'),
	!,
	% format("Possessive construct: John's swimming, John's arriving, the farmer's work, etc.~n"),
	(can_be_agent(POSSESSOR_SYNSET_ID,POSSESSOR) ->
	    TEMPLATE = Owner^Activity^[exists,['?Process'],
				           [and, [instance,'?Process',Activity],
			                         [agent,'?Process',Owner]]];
	    TEMPLATE = Owner^Activity^[exists,['?Process'],
				           [and, [instance,'?Process',Activity],
			                         [patient,'?Process',Owner]]]).

% e.g., to handle "John's father"
get_possessive_construct_template(POSSESSOR_SYNSET_ID,POSSESSOR,OWNED_ITEM_ID,OWNED_ITEM_TYPE,TEMPLATE) :-
	is_subrelation(OWNED_ITEM_TYPE,'familyRelation'),
	TEMPLATE = Owner^Relation^[OWNED_ITEM_TYPE,Relation,Owner],   % e.g., [father,'?Father','John']
	% format("Possessive construct: John's father, John's wife, etc.~n"),
	!.

% e.g., to handle "John's nose"
get_possessive_construct_template(POSSESSOR_SYNSET_ID,POSSESSOR,OWNED_ITEM_ID,OWNED_ITEM_TYPE,TEMPLATE) :-
	is_subclass_or_equal(OWNED_ITEM_TYPE,'BodyPart'),             % e.g., [part,'?nose','John']
	TEMPLATE = Owner^Body_Part^[part,Body_Part,Owner],
	% format("Possessive construct: John's nose, John's stomach, etc.~n"),
	!.

% e.g., to handle "the ship's captain", "captain of the ship", "the company's president", etc.
get_possessive_construct_template(POSSESSOR_SYNSET_ID,POSSESSOR,OWNED_ITEM_ID,OWNED_ITEM_TYPE,TEMPLATE) :-
	is_subclass_or_equal(POSSESSOR,'Object'),                                    % e.g., transportation device for 'ship'
	get_first_word_to_describe_synset(POSSESSOR_SYNSET_ID,Word),                 % e.g., 'ship' and var is '?ship'
	create_var_name(Word,VAR),
	is_subclass_or_equal(OWNED_ITEM_TYPE,'OccupationalRole'),                    % e.g., 'captain' is an occupational role
	TEMPLATE = Owner^Position^[exists,[VAR],[and,[instance,VAR,POSSESSOR],
						     [possesses,Position,VAR]]],
	% format("Possessive construct: John's captain, the ship's officer, president of the company, etc.~n"),
	!.


% e.g., to handle "John's blood" or "John's muscle"
get_possessive_construct_template(POSSESSOR_SYNSET_ID,POSSESSOR,OWNED_ITEM_ID,OWNED_ITEM_TYPE,TEMPLATE) :-
	is_subclass_or_equal(OWNED_ITEM_TYPE,'BodySubstance'),
	TEMPLATE = Owner^Body_Substance^[part,Body_Substance,Owner],  
	% format("Possessive construct: John's blood, John's muscle, etc.~n"),
	!.

% e.g., to handle "John's height" or some other attribute...
get_possessive_construct_template(POSSESSOR_SYNSET_ID,POSSESSOR,OWNED_ITEM_ID,OWNED_ITEM_TYPE,TEMPLATE) :-
	is_subclass_or_equal(OWNED_ITEM_TYPE,'Attribute'),
	TEMPLATE = Owner^Attribute^[property,Owner,Attribute],
	% format("Possessive construct: John's height, John's weight, etc.~n"),
	!.

% e.g., to handle "John's dog", to handle "John's car", and everything else.
get_possessive_construct_template(POSSESSOR_SYNSET_ID,POSSESSOR,OWNED_ITEM_ID,OWNED_ITEM_TYPE,TEMPLATE) :-
	TEMPLATE = Owner^Ownee^[possesses,Owner,Ownee].
	% format("Possessive construct: John's dog, John's car, and all other possessive constructs (default interpretation).~n").

% specify_type(+VAR,+TYPE,+IN,-OUT,+VARS_IN,VARS_OUT)
% generates either a single clause or more specifying that VAR is of type TYPE. One or more clauses
% may be added to the input clauses IN to produce the output clauses OUT. One or more variables may
% be added to VARS_IN to produce VARS_OUT. Typically TYPE will be a class like Dog and only one variable
% (e.g., ?dog) and one clause (e.g., (instance ?dog Dog)) will be added. For nouns that are roles,
% more than one clause may be added (e.g., (customer ?customer ?CognitiveAgent)
% (instance ?CognitiveAgent CognitiveAgent)) and more than one variable (?customer ?CognitiveAgent).

% Also, if the type is a list of SUMO concepts, such as ['Male','Human','FullyFormed'] (for 'man'),
% then multiple type clauses are generated.

% N.B. that VARS_IN should already include VAR and we have already guaranteed that VAR is a unique
% Sigma variable name.
		    
specify_type(VAR,TYPE,IN,IN,VARS,VARS) :- (var(TYPE);TYPE=='empty'),!.

% TYPE is a role or requires other special translation
specify_type(VAR,TYPE,IN,OUT,VARS_IN,VARS_OUT) :-
	not(var(TYPE)),not(TYPE=='empty'),
	translation_template(TYPE,POSITION,ARG_NAMES,ARG_TYPES),!, 
	fill_in_template(VAR,POSITION,TYPE,ARG_NAMES, ARG_TYPES,
			 FILLED_IN_TEMPLATE,TYPE_RESTRICTION_CLAUSES,VARS_IN,VARS_OUT),
	append(IN,[FILLED_IN_TEMPLATE|TYPE_RESTRICTION_CLAUSES],OUT).

% Translation when TYPE is a list of SUMO concepts (e.g., ['Male','Human','FullyFormed'] (for 'man').
% Note that VAR is already a member of VARS_IN so it does not need to be added.

specify_type(VAR,[],IN,IN,VARS_IN,VARS_IN) :- !.

specify_type(VAR,[FIRST_TYPE|REST_TYPES],IN,OUT,VARS_IN,VARS_IN) :-
	not(var(FIRST_TYPE)),is_list(REST_TYPES),!,
	THIS_TYPE = [instance,VAR,FIRST_TYPE],
	specify_type(VAR,REST_TYPES,IN,REST_OF_FORMS,VARS_IN,VARS_IN),
	OUT = [THIS_TYPE|REST_OF_FORMS].

% Standard translation (e.g., (instance ?dog Dog))
% Note that VAR is already a member of VARS_IN so it does not need to be added.
specify_type(VAR,TYPE,IN,[[instance,VAR,TYPE]|IN],VARS_IN,VARS_IN) :-
	not(var(TYPE)),not(TYPE=='empty'),!.

% To test specify_type
test_specify1 :- specify_type('?customer','customer',[],OUT,['?customer'],VARS),
	write('Forms = '),write(OUT),nl,
	write('Vars = '),write(VARS), nl.

test_specify2 :- specify_type('?buyer','customerRepresentative',[],OUT,['?buyer'],VARS),
	write('Forms = '),write(OUT),nl,
	write('Vars = '),write(VARS), nl.

test_specify3 :- specify_type('?card','card',[],OUT,['?card'],VARS),
	write('Forms = '),write(OUT),nl,
	write('Vars = '),write(VARS), nl.

test_fill_in :- fill_in_template('?customer',1,customer,['?buyer','?salesperson'], ['CognitiveAgent','CognitiveAgent'],
			 TEMPLATE,TYPE_RESTRICTIONS,[],VARS),
	write('Template = '),write(TEMPLATE),nl,
	write('Forms = '),write(TYPE_RESTRICTIONS),nl,
	write('Vars = '),write(VARS), nl.

test_fill_in2 :- fill_in_template('John-1',1,customer,['?customer','?salesperson'], ['CognitiveAgent','CognitiveAgent'],
			 TEMPLATE,TYPE_RESTRICTIONS,[],VARS),
	write('Template = '),write(TEMPLATE),nl,
	write('Forms = '),write(TYPE_RESTRICTIONS),nl,
	write('Vars = '),write(VARS), nl.

% fill_in_template(+VAR,+POSITION,+TYPE,+ARG_NAMES,+ARG_TYPES,
%                  -FILLED_IN_TEMPLATE,-TYPE_RESTRICTION_CLAUSES,+VARS_IN,-VARS_OUT)
% Examples:
%  fill_in_template('?customer',1,'customer',['?customer','?salesperson'],
%                                            ['CognitiveAgent','CognitiveAgent'],FILLED,RESTRICTIONS,[],VARS)
%  FILLED = [customer,'?customer','?organization']
%  CLAUSES = [[instance,'?customer','CognitiveAgent'],[instance,'?salesperson','CognitiveAgent']]
%  given that the translation template for customer is:
%  translation_template(customer,1,
%		     ['?customer','?salesperson'],           % Informal documentation for each arg
%		     ['CognitiveAgent',CognitiveAgent']).    % Type restrictions

fill_in_template(VAR,POSITION,TYPE,ARG_NAMES,ARG_TYPES,FILLED_IN_TEMPLATE,RESTRICTIONS,VARS_IN,VARS_OUT) :-
	length(ARG_TYPES,NUMBER_OF_ARGS),
	fill_in_template_positions(NUMBER_OF_ARGS,VAR,POSITION,TYPE,ARG_NAMES,
				   ARG_TYPES,REVERSED_TEMPLATE,RESTRICTIONS,VARS_IN,VARS_OUT),
	reverse(REVERSED_TEMPLATE,FILLED_IN_TEMPLATE).

% fill_in_template_positions(+INDEX,+VAR,+TARGET_POSITION,+ROLE_TYPE,+ARG_NAMES,
%                            +ARG_TYPES,-LIST_FOR_TEMPLATE,-TYPE_RESTRICTION_CLAUSES,+VARS_IN,-VARS_OUT)
% INDEX is what position of the template is being filled in. 0 is the function/role name, 1 is the first argument, etc.
%       All positions from this one down to 0 are to be filled in.
% VAR is the name of the variable we are trying to specify to be of type ROLE_TYPE, for example,
%       ?customer for type 'customer'. It is guaranteed unique and is already a member of VARS_IN.
% TARGET_POSITION is where this variable should be plugged in, i.e., which argument,
%       e.g., 1 for ?customer in (customer ?customer ?org).
% ROLE_TYPE is the type of role we are specifying (e.g., 'customer' or 'customerRepresentative'). Note this may
%       not actually be a role, but could be anything where the type is not just a simple SUO Class.
% ARG_NAMES is a list of suggested variable names for the arguments in the template. This list acts as informal
%       documentation.
% ARG_TYPES is a list of the argument type restrictions for each argument
%      in the predicate.
% LIST_FOR_TEMPLATE is the list of items built so far for the template. Note that it is built in reverse order.
%     E.g., ['?org','?customer',customer] will be the final template returned for the 'customer' template.
% TYPE_RESTRICTION_CLAUSES is a list of new clauses that restrict the type of newly introduced variables,
%     E.g., [[instance,'?org','?Organization],[instance,'?customer','CognitiveAgent']]
% VARS_IN are Sigma variables already in use. These are
%     provided so we do not duplicate any names.
% VARS_OUT adds to VARS_IN the newly introduced variables that refer to other entities in the role clause.
%     They are restricted by the type restriction clauses.

% for testing fill_in_template_positions/10
try1:- fill_in_template_positions(0,'?customer',1,customer,['?buyer','?salesperson'],
                  ['CognitiveAgent','CognitiveAgent'],TEMPLATE_RESULTS,RESTRICTIONS,['?customer'],VARS),
       write('Template (reversed) = '),write(TEMPLATE_RESULTS),nl, % should be [customer]
       write('Type restrictions = '),write(RESTRICTIONS),nl,       % should be []
       write('All variables = '),write(VARS),nl.                   % should be []

try2:- fill_in_template_positions(1,'?customer',1,customer,['?buyer','?salesperson'],
                  ['CognitiveAgent','CognitiveAgent'],TEMPLATE_RESULTS,RESTRICTIONS,['?customer'],VARS),
       write('Template (reversed) = '),write(TEMPLATE_RESULTS),nl, % should be ['?customer',customer]
       write('Type restrictions = '),write(RESTRICTIONS),nl, % should be [[instance,'?customer','CognitiveAgent']]
       write('All variables = '),write(VARS),nl. % should be ['?customer']

try3:- fill_in_template_positions(2,'?customer',1,customer,['?buyer','?salesperson'],
                  ['CognitiveAgent','CognitiveAgent'],TEMPLATE_RESULTS,RESTRICTIONS,['?customer'],VARS),
       write('Template (reversed) = '),write(TEMPLATE_RESULTS),nl, % should be ['?salesperson','?customer',customer]
       write('Type restrictions = '),write(RESTRICTIONS),nl, % should be [[instance,'?salesperson','CognitiveAgent']|ABOVE]
       write('All variables = '),write(VARS),nl. % should be ['?customer','?salesperson'].      

try4 :- fill_in_template_positions(2,'?customer',1,customer,['?buyer','?salesperson'],
				   ['CognitiveAgent','CognitiveAgent'],
				   TEMPLATE,TYPE_RESTRICTIONS,['?customer'],VARS),
	write('Template = '),write(TEMPLATE),nl,
	write('Forms = '),write(TYPE_RESTRICTIONS),nl,
	write('Vars = '),write(VARS), nl.

% if we are at the start of the template, just plug in the function name, i.e., the type (e.g., 'dog').
% Example: fill_in_template_positions(0,'?customer',1,customer,['?buyer','?salesperson'],
%                  ['CognitiveAgent','CognitiveAgent'],[customer],[],['?customer'],['?customer']).
fill_in_template_positions(0,VAR,POSITION,TYPE,ARG_NAMES,ARG_TYPES,[TYPE],[],VARS_IN,VARS_IN).

% if we are at the right position to plug in the variable do so, e.g., ...?customer...for role customer

% Example: fill_in_template_positions(1,'?customer',1,customer,['?buyer','?salesperson'],
%                  ['CognitiveAgent','CognitiveAgent'],['?customer',customer],
%                  [[instance,'?customer','CognitiveAgent']],
%                  [],['?customer']).

fill_in_template_positions(INDEX,VAR,POSITION,TYPE,ARG_NAMES,ARG_TYPES,
			   [VAR|REST_OF_TEMPLATE],
			   [NEW_RESTRICTION|REST_OF_TYPE_RESTRICTION_CLAUSES],
			    VARS_IN,VARS_OUT) :-
	% recursive call fills in remaining positions down to 0.
	INDEX == POSITION, !, NEXT is INDEX - 1, not(NEXT < 0),

        nth1(POSITION,ARG_TYPES,ARG_POS_TYPE), % VAR has type restriction, e.g., 'CognitiveAgent'

	NEW_RESTRICTION = [instance,VAR,ARG_POS_TYPE], % Add new type restriction

	fill_in_template_positions(NEXT,VAR,POSITION,TYPE,ARG_NAMES,ARG_TYPES,
				   REST_OF_TEMPLATE,
				   REST_OF_TYPE_RESTRICTION_CLAUSES,
				   VARS_IN,VARS_OUT).

% if we are not at the right position to plug in the variable do so, then introduce a variable for that position and
% add a type restriction clause, e.g., ...?organization...for role customer adds restriction
% (instance ?organization Organization).

% Example: fill_in_template_positions(2,'?customer',1,customer,['?buyer','?salesperson'],
%                  ['CognitiveAgent','CognitiveAgent'],['?salesperson','?customer',customer],
%                  [[instance,'?salesperson','CognitiveAgent],[instance,'?customer','CognitiveAgent']],
%                  [],['?salesperson','?customer']).

fill_in_template_positions(INDEX,VAR,POSITION,TYPE,ARG_NAMES,ARG_TYPES,
			   [NEW_VAR|REST_OF_TEMPLATE],
			   [NEW_RESTRICTION|REST_OF_TYPE_RESTRICTION_CLAUSES],
			   VARS_IN,VARS_OUT) :-
	not(INDEX == POSITION), !, NEXT is INDEX - 1, not(NEXT < 0),

	%get suggested variable name from ARG_NAMES
	nth1(INDEX,ARG_NAMES,ARG_NAME), % ARG_NAME is suggested ARG name, like '?org' or '?salesperson'

	%ensure this variable name is unique given that some variable names are already chosen in VARS
	ensure_unique(ARG_NAME,VARS_IN,NEW_VAR),    % NEW_VAR will be something like '?organization1'

	UPDATED_VARS_IN = [NEW_VAR|VARS_IN],        % Add this new variable to those already present

        nth1(INDEX,ARG_TYPES,ARG_POS_TYPE), % NEW_VAR has type restriction, e.g., 'CognitiveAgent'

	NEW_RESTRICTION = [instance,NEW_VAR,ARG_POS_TYPE], % Add new type restriction

	fill_in_template_positions(NEXT,VAR,POSITION,TYPE,ARG_NAMES,ARG_TYPES,
				   REST_OF_TEMPLATE,
				   REST_OF_TYPE_RESTRICTION_CLAUSES,
				   UPDATED_VARS_IN,VARS_OUT).

% traverse_object_of_copula(+Complement,+SUBJECT_VAR,+IN,-OUT)

% generates SUO clauses for the direct object of a copula (e.g., the object in
% 'The customer is a bank employee'). It has all clauses refer to the variable
% of the subject of the copula rather than introduce a new variable and make the
% new one co-ref or be equal to the subject variable. Other variables may be introduced
% by roles, however.

traverse_object_of_copula(Complement,SUBJECT_VAR,IN,OUT,VARS_IN,VARS_OUT) :-
	Complement = head<->HEAD..type<->TYPE..id<->ID..mod<->MOD..sub<->SUB..
                     poss<->OWNER..owner<->OWNER_ID..           % owner in a possessive construct like "X's Y"
                     of<->OF..of_id<->OF_ID..apos<->APOS,       % owner in a possessive construct like "Y of X"


        %add the new variable name for the subject variable to the others already accumulated
	VARS_W_SUBJ_VAR = [SUBJECT_VAR|VARS_IN],

	%handle subordinate sentence information
	% add_subordinate_descriptors will update the variables (from VARS_W_SUBJ_VAR to UPDATED_VARS),
	% and the forms (from IN to RESULT1) according to the semantics of the subordinate sentence.
	(not(var(SUB)),not(SUB=='empty') ->
	    add_subordinate_descriptors(SUBJECT_VAR,TYPE,SUB,IN,RESULT1,VARS_IN,UPDATED_VARS);
	    RESULT1 = IN, UPDATED_VARS = VARS_IN),

	% handle possessive constructs
	handle_possessives_and_of_constructs(OWNER_ID,OWNER,
					     SUBJ_VAR,ID,TYPE,RESULT1,RESULT2),

	% handle of-preposition constructs just like possessive
	handle_possessives_and_of_constructs(OF_ID,OF,
					     SUBJ_VAR,ID,TYPE,RESULT2,RESULT3),

	% handle mod feature
	(not(var(MOD)),not(MOD=='empty') -> modify_noun_phrase(SUBJECT_VAR,TYPE,MOD,RESULT3,RESULT4); RESULT4 = RESULT3),

	% handle apos (aposition) like extra type info
	(not(var(APOS)),not(APOS=='empty') -> RESULT5 = [[instance,SUBJECT_VAR,APOS]|RESULT4]; RESULT5 = RESULT4),

	%handle type feature
	specify_type(SUBJECT_VAR,TYPE,RESULT5,OUT,UPDATED_VARS,VARS_OUT).

% NON-COPULA SENTENCES

% handle non-copula verbs, i.e., those other than 'be'...
traverse_sentence_semantic_frames(SEMANTICS,IN,OUT,VARS_IN,VARS_OUT) :-
	SEMANTICS = pred<->PREDICATE..id<->VERB_ID..subj<->SUBJECT..dobj<->DIRECT_OBJECT ..
                    iobj<->INDIRECT_OBJECT..adjs<->ADJUNCTS, % need not handle MODIFIER as that is only for copula constructs

        % Find WordNet IDs for the objects and determine the transitivity at the same time...
        SUBJECT = id<->SUBJ_ID..type<->Subject_Concept,

	((not(var(DIRECT_OBJECT)), DIRECT_OBJECT = id<->DOBJ_ID..type<->Direct_Object_Concept) ->
	          ((not(var(INDIRECT_OBJECT)), INDIRECT_OBJECT = id<->IOBJ_ID..type<->Indirect_Object_Concept) ->
		      Transitivity = ditransitive;
		      Transitivity = transitive);
		  Transitivity = intransitive),

	% handle special case translations of verbs here, e.g., 'X owns Y' translates to '[possesses,X,Y]'
	% rather than an event of the verb plus case roles for the subject and other objects

	(check_for_special_case(PREDICATE,Transitivity,SUBJECT,DIRECT_OBJECT,INDIRECT_OBJECT,IN,OUT,VARS_IN,VARS_OUT) ->
	    true; % all variable bindings have already been effected in the conditional test if it succeeded so we can just put true here.
	    handle_event_verbs(PREDICATE,Transitivity,VERB_ID,SUBJECT,DIRECT_OBJECT,INDIRECT_OBJECT,ADJUNCTS,
			       IN,OUT,VARS_IN,VARS_OUT)).

% check_for_special_case(+PREDICATE,+TRANSITIVITY,+SUBJECT,+DIRECT_OBJECT,+INDIRECT_OBJECT,+IN,-OUT,+VARS_IN,-VARS_OUT)
% handles verbs that do not follow the usual translation into an instance of an event and then
% expressions for case roles; instead, these special case verbs have verb translation templates
% that indicate precisely how to translate them.

% For example, to translate 'X owns Y' to '[possesses, X, Y]'
% instead of ...[event,'?event','possesses'],[agent,'?event',X],[patient,'?event',Y]...
% we add the following to a lexicon file...
% verb_translation_template(owns,transitive,X^Y^[possesses,X,Y]).

check_for_special_case(PREDICATE,intransitive,SUBJECT,DIRECT_OBJECT,INDIRECT_OBJECT,IN,OUT,VARS_IN,VARS_OUT) :-
	verb_translation_template(PREDICATE,intransitive,LAMBDA_EXPRESSION),

        % add clauses and declare discourse variable for subject
        traverse_complement_if_present(SUBJECT,IN,OUT1,VARS_IN,VARS_OUT,SUBJECT_VAR),
	
	% now add code for stative verb or relation
	apply_lambda_expression_of_one_argument(LAMBDA_EXPRESSION,SUBJECT_VAR,RESULTS),

	% put var declarations and types before relation for ease in reading generated code
	append(OUT1,[RESULTS],OUT).

check_for_special_case(PREDICATE,transitive,SUBJECT,DIRECT_OBJECT,INDIRECT_OBJECT,IN,OUT,VARS_IN,VARS_OUT2) :-
	verb_translation_template(PREDICATE,transitive,LAMBDA_EXPRESSION),

        % add clauses and declare discourse variable for subject
        traverse_complement_if_present(SUBJECT,IN,OUT1,VARS_IN,VARS_OUT1,SUBJECT_VAR),

        % add clauses for and declare discourse variable for direct object
        traverse_complement_if_present(DIRECT_OBJECT,OUT1,OUT2,VARS_OUT1,VARS_OUT2,DIRECT_OBJECT_VAR),

	% now add code for stative verb or relation
	apply_lambda_expression_of_two_arguments(LAMBDA_EXPRESSION,SUBJECT_VAR,DIRECT_OBJECT_VAR,RESULTS),

	% put var declarations and types before relation for ease in reading generated code
	append(OUT2,[RESULTS],OUT).

check_for_special_case(PREDICATE,ditransitive,SUBJECT,DIRECT_OBJECT,INDIRECT_OBJECT,IN,OUT,VARS_IN,VARS_OUT3) :-
	verb_translation_template(PREDICATE,ditransitive,LAMBDA_EXPRESSION),

        % add clauses and declare discourse variable for subject
        traverse_complement_if_present(SUBJECT,IN,OUT1,VARS_IN,VARS_OUT1,SUBJECT_VAR),

        % add clauses for and declare discourse variable for direct object
        traverse_complement_if_present(DIRECT_OBJECT,OUT1,OUT2,VARS_OUT1,VARS_OUT2,DIRECT_OBJECT_VAR),

        % add clauses for and declare discourse variable for indirect object
        traverse_complement_if_present(INDIRECT_OBJECT,OUT2,OUT3,VARS_OUT2,VARS_OUT3,INDIRECT_OBJECT_VAR),

	% now add code for stative verb or relation
	apply_lambda_expression_of_three_arguments(LAMBDA_EXPRESSION,SUBJECT_VAR,DIRECT_OBJECT_VAR,INDIRECT_OBJECT_VAR,RESULTS),

	% put var declarations and types before relation for ease in reading generated code
	append(OUT3,[RESULTS],OUT).

% handle_event_verbs(+PREDICATE,+TRANSITIVITY,+VERB_ID,+SUBJECT,+DIRECT_OBJECT,+INDIRECT_OBJECT,+ADJUNCTS,+IN,-OUT,+VARS_IN,-VARS_OUT) 
% handles most verbs that represent events (e.g., 'enter', 'run', 'insert', etc.) by translating them to
% instances of the corresponding event plus expressions for the case roles of the objects. It also handles adjuncts
% describing the event. MODIFIER

handle_event_verbs(PREDICATE,TRANSITIVITY,VERB_ID,SUBJECT,DIRECT_OBJECT,INDIRECT_OBJECT,ADJUNCTS,IN,OUT,VARS_IN,VARS_OUT) :-

        SUBJECT = id<->SUBJ_ID..type<->Subject_Concept,

	% we continue here for normal processing of verbs that are events and where the subject and
	% other objects should be translated to case role expressions

	% add a clause like [instance,'?event','Putting'] to describe the predicate
        traverse_predicate(PREDICATE,IN,OUT1,VARS_IN,VARS_OUT1,EVENT_VAR),

        % add clauses for subject
        traverse_subject_of_sentence(SUBJECT,OUT1,OUT2,VARS_OUT1,VARS_OUT2,SUBJECT_VAR),

        % by examining the verb and object determine the case role for the subject and now add these
        determine_case_role(subject,TRANSITIVITY,VERB_ID,PREDICATE,SUBJ_ID,Subject_Concept,Subject_Case_Role,Subject_Warnings),

	(not(var(SUBJECT_VAR)),not(SUBJECT_VAR=='empty') -> OUT3 = [[Subject_Case_Role,EVENT_VAR,SUBJECT_VAR]|OUT2];
	                                                    OUT3 = OUT2),

        % add clauses for direct object if present
        traverse_complement_if_present(DIRECT_OBJECT,OUT3,OUT4,VARS_OUT2,VARS_OUT3,DIRECT_OBJECT_VAR),

        % add the case role for a direct object, which is always 'patient'
	(not(var(DIRECT_OBJECT_VAR)),not(DIRECT_OBJECT_VAR=='empty') -> OUT5 = [[patient,EVENT_VAR,DIRECT_OBJECT_VAR]|OUT4];
	                                                                OUT5 = OUT4),

        % add clauses for indirect object if present, note that an indirect object always has a 'to' preposition,
	% or for ditransitive verbs where this is not the case, as in 'John gave Mary a book.' then the parsing
	% has mapped the objects into the right categories (direct or indirect object).
        traverse_complement_if_present(INDIRECT_OBJECT,OUT5,OUT6,VARS_OUT3,VARS_OUT4,INDIRECT_OBJECT_VAR),

        % add the case role for an indirect object, which is always 'destination'
	(not(var(INDIRECT_OBJECT_VAR)),not(INDIRECT_OBJECT_VAR=='empty') -> OUT7 = [[destination,EVENT_VAR,INDIRECT_OBJECT_VAR]|OUT6];
	                                                                    OUT7 = OUT6),

        % add clauses for adjuncts if present
        traverse_adjuncts_if_present(ADJUNCTS,EVENT_VAR,OUT7,OUT8,VARS_OUT4,VARS_OUT),

	% finally reverse results for easier reading
	reverse(OUT8,OUT).

% traverse_predicate(+PREDICATE,+SUO_IN,-SUO_OUT,+VARS_IN,-VARS_OUT,-EVENT_VAR)
% traverses the pred semantic feature of a sentence.
traverse_predicate(SUO_predicate,IN,[FORM|IN],VARS,[EVENT_NAME|VARS],EVENT_NAME) :-
	SUO_var_name = '?event', 

	%ensure event name is unique in case events are also referred to in relative sentences
	ensure_unique(SUO_var_name,VARS,EVENT_NAME),
	
	FORM = [instance,EVENT_NAME,SUO_predicate].

% traverse_complement_if_present(+COMPLEMENT,+SUO_IN,-SUO_OUT,+VARS_IN,-VARS_OUT, -COMPLEMENT_VAR)
% traverses the semantic features of a noun complement:
%
% head,       is the variable name created for the primary object referred to in this complement (e.g., '?Man')
% type,       is the Sigma concept name for the type of thing which the head is (e.g., 'Man')
% mod,        is the Sigma name for a modifier (adjective) that applies to the head
% sub,        is the sentence semantics of a relative sentence that modifies the head ('sub' for subordinate sentence).
% poss,       is an object that is in the possessive position for the head (e.g., 'John' in 'John's dog')
% of,         is an object that is in the of-preposition positions of the head (e.g., 'John' in 'dog of John')
% quan,       is either 'universal' or (more commonly) 'existential' depending on the determiner that modifies the head
% apos,       is a common noun used in aposition to the proper noun that is the head of this NP.


traverse_complement_if_present('empty',IN,IN,VARS,VARS,'empty') :- !.

traverse_complement_if_present(Complement,IN,OUT,VARS_IN,VARS_OUT,NP_VAR) :-
	Complement = head<->HEAD..type<->TYPE..id<->ID..mod<->MOD..sub<->SUB..
                     poss<->OWNER..owner<->OWNER_ID..           % owner in a possessive construct like "X's Y"
                     of<->OF..of_id<->OF_ID..apos<->APOS,       % owner in a possessive construct like "Y of X"

	%ensure variable name in HEAD is unique given that some variable names are already chosen in VARS
	ensure_unique(HEAD,VARS_IN,NP_VAR),

        %add the new variable name for the complement's variable to the others already accumulated
	UPDATED_VARS = [NP_VAR|VARS_IN],

	%handle subordinate sentence information
	% add_subordinate_descriptors will update the variables (from VARS_W_SUBJ_VAR to UPDATED_VARS),
	% and the forms (from IN to RESULT1) according to the semantics of the subordinate sentence.
	(not(var(SUB)),not(SUB=='empty') ->
	    add_subordinate_descriptors(NP_VAR,TYPE,SUB,IN,RESULT1,UPDATED_VARS,VARS_INTERMEDIATE);
	    RESULT1 = IN, VARS_INTERMEDIATE = UPDATED_VARS),

	% handle possessive constructs
	handle_possessives_and_of_constructs(OWNER_ID,OWNER,
					     NP_VAR,ID,TYPE,RESULT1,RESULT2),

	% handle of-preposition constructs just like possessive
	handle_possessives_and_of_constructs(OF_ID,OF,
					     NP_VAR,ID,TYPE,RESULT2,RESULT3),

	% handle mod feature
	(not(var(MOD)),not(MOD=='empty') -> modify_noun_phrase(NP_VAR,TYPE,MOD,RESULT3,RESULT4); RESULT4 = RESULT3),

	% handle apos (aposition) like extra type info
	(not(var(APOS)),not(APOS=='empty') -> RESULT5 = [[instance,NP_VAR,APOS]|RESULT4]; RESULT5 = RESULT4),

	%handle type feature
	specify_type(NP_VAR,TYPE,RESULT5,OUT,VARS_INTERMEDIATE,VARS_OUT).
	

% modify_noun_phrase(+VAR,+TYPE,+MOD,+IN,-OUT)
% takes a modifier, either an SUO concept MOD or one inside
% a superlative structure (e.g., superlative(oldness) ) and
% generates SUO forms that essentially apply the modifier to
% the noun phrase.

modify_noun_phrase(VAR,TYPE,superlative(MOD),IN,OUT) :-   % handle superlatives here
	!,
        FORALL = [forall,['?X'],
	              [implies,[and, [instance,'?X',TYPE],
                                     [not, [equal, '?X', VAR]]],
		               [greaterThan, [measure, [attribute, VAR, MOD]],
				             [measure, [attribute, '?X', MOD]]]]],
	OUT = [FORALL|IN].

modify_noun_phrase(VAR,TYPE,MOD,IN,OUT) :-                % handle non-superlatives here
	OUT = [[attribute,VAR,MOD]|IN].

% traverse_adjuncts_if_present(+ADJUNCTS,+EVENT_VAR,+SUO_IN,-SUO_OUT,+VARS_IN,-VARS_OUT)
% traverses the semantic features of zero or more adjuncts. Each adjunct has these
% features:	
%
% adv,        is the Sigma concept name for the adverb in the adjunct, if there is one, or empty, if not
% prep,       is the adverbial preposition if there is no adverb
% aobj,       is the noun phrase part of the adverbial preposition, aobj stands for adverbial object

traverse_adjuncts_if_present([],_,SUO_IN,SUO_IN,VARS_IN,VARS_IN) :- !.

traverse_adjuncts_if_present([ADJUNCT|OTHERS],EVENT_VAR,SUO_IN,SUO_OUT,VARS_IN,VARS_OUT) :-
	traverse_adjunct(ADJUNCT,EVENT_VAR,SUO_IN,SUO_AFTER_ADJUNCT,VARS_IN,VARS_AFTER_ADJUNCT),
	traverse_adjuncts_if_present(OTHERS,EVENT_VAR,SUO_AFTER_ADJUNCT,SUO_OUT,VARS_AFTER_ADJUNCT,VARS_OUT).
	
% traverse_adjunct(+ADJUNCT,+EVENT_VAR,+SUO_IN,-SUO_OUT,+VARS_IN,-VARS_OUT)
% traverses the semantic features of one adjunct. Each adjunct has these
% features:	
%
% adv,        is the Sigma concept name for the adverb in the adjunct, if there is one, or empty, if not
% prep,       is the adverbial preposition if there is no adverb
% aobj,       is the noun phrase part of the adverbial preposition, aobj stands for adverbial object


traverse_adjunct(ADJUNCT,EVENT_VAR,IN,[FORMS|IN],VARS,VARS) :-
	ADJUNCT = adv<->ADV..prep<->PREP..aobj<->AOBJ,

	% handle adverb
	not(var(ADV)),not(ADV=='empty'),!,FORMS = [manner,EVENT_VAR,ADV].

traverse_adjunct(ADJUNCT,EVENT_VAR,IN,OUT,VARS,NEW_VARS) :-
	ADJUNCT = adv<->ADV..prep<->PREP..aobj<->AOBJ,

	% handle adverbial prepositional phrase
	not(var(PREP)),not(PREP=='empty'),!,
	traverse_complement_if_present(AOBJ,IN,COMPLEMENT_FORMS,VARS,NEW_VARS,COMPLEMENT_VAR),

	% not generate and add in an expression representing the relationship between the event and prepositional object
        generate_code_for_adverbial_preposition(PREP,EVENT_VAR,COMPLEMENT_VAR,COMPLEMENT_FORMS,OUT).

% generate_code_for_adverbial_preposition(+PREP_TEMPLATE,+EVENT_VAR,+PREP_OBJECT_VAR,+FORMS_IN,-FORMS_OUT)
% generates one or more SUMO expressions for a preposition. If PREPOSITIONAL_TEMPLATE
% is just a symbol, then it represents a SUMO concept PREP and the output code is simply
% the preposition, the event variable, and the prepositional object variable, e.g.,
% if the template is 'located' then the output might be [located,'?Event','Station1'].
% If more complex code is required then the prepositional template should be a lambda
% expression of two arguments, the first being the event and the second being the object
% of the preposition. E.g., if the template is Event^Time^[lessThan,[BeginFn,Event],[BeginFn,Time]]
% then the output might appear like this: [lessThan,[BeginFn,?Event],[BeginFn,?Time]]

generate_code_for_adverbial_preposition(PREP_TEMPLATE,EVENT_VAR,PREP_OBJECT_VAR,FORMS_IN,FORMS_OUT) :-
	(is_lambda_expression(PREP_TEMPLATE) -> 
	    apply_lambda_expression_of_two_arguments(PREP_TEMPLATE,EVENT_VAR,PREP_OBJECT_VAR,CODE);
	    CODE = [PREP_TEMPLATE,EVENT_VAR,PREP_OBJECT_VAR]),
	FORMS_OUT = [CODE|FORMS_IN].

% traverse_modifier_if_present(+MODIFIER,+SUBJECT_VAR,FORMS_IN,FORMS_OUT,VARS_IN,VARS_OUT)

traverse_modifier_if_present('empty',SUBJECT_VAR,FORMS_IN,FORMS_IN,VARS_IN,VARS_IN) :- !.

traverse_modifier_if_present(MODIFIER,SUBJECT_VAR,FORMS_IN,FORMS_OUT,VARS_IN,VARS_IN) :-
	FORMS_OUT = [[attribute,SUBJECT_VAR,MODIFIER]|FORMS_IN].

% drop_constants_and_query_variables(+VARS,+QUERY_VARS,-SIGMA_VARS_ONLY)
% takes a list of items referred to in a sentence (e.g., [?customer,John-1,?card])
% and removes those items that are constants, thus leaving only the Sigma variables
% (e.g., in this example [?customer, ?card]). It also removes all variables in QUERY_VARS
% from VARS since the query variables should be free variables (i.e., we will try to
% find values for them, they are not closed in the existential).

drop_constants_and_query_variables([],QUERY_VARS,[]).

% Note: 63 is '?', first clause handles lists like [?customer, John-1...]
% the variable is included in the results if it is not part of the query vars
drop_constants_and_query_variables([VAR|REST],QUERY_VARS,FINAL) :- name(VAR,[63|OTHER_CHARS]), !,
	(memberchk(VAR,QUERY_VARS) -> FINAL = RESULTS; FINAL = [VAR|RESULTS]),
	drop_constants_and_query_variables(REST,QUERY_VARS,RESULTS).

% Note: 63 is '?', second clause handles lists like [John-1, ?customer...]
drop_constants_and_query_variables([VAR|REST],QUERY_VARS,RESULTS) :- name(VAR,[FIRST_CHAR|OTHER_CHARS]), not(FIRST_CHAR == 63),
	drop_constants_and_query_variables(REST,QUERY_VARS,RESULTS).

% ensure_unique/3(+VAR_NAME,+VARS,-GUARANTEED_UNIQUEVAR_NAME)

% ensure_unique ensures that the variable HEAD is unique by adding a numeric
% suffix as necessary to distinguish it from any variables in VARS. E.g., if
% HEAD is '?code' and VARS is ['?customer','?code'] then we will add '1' to
% the end of '?code' to form '?code1'. If that still was not unique then we
% would try adding '2' after that, then adding '3' after that, and so on...

ensure_unique(HEAD,VARS,HEAD) :- name(HEAD,[FIRST_CHAR|REST]),not(FIRST_CHAR==63),!. % not a Sigma variable
ensure_unique(HEAD,VARS,HEAD) :- not(memberchk(HEAD,VARS)),!.                        % not part of VARS
ensure_unique(HEAD,VARS,NP_VAR) :- memberchk(HEAD,VARS),!,                           % Make it unique if name already used
	ensure_unique(HEAD,VARS,0,10,NP_VAR). 

% ensure_unique/4(+VAR_NAME,+VARS,+NUMERIC_SUFFIX_TO_TRY,+MAX_TRIES,-GUARANTEED_UNIQUEVAR_NAME)

ensure_unique(HEAD,VARS,INDEX,MAX,HEAD) :- not(memberchk(HEAD,VARS)),!.
ensure_unique(HEAD,VARS,INDEX,MAX,NP_VAR) :-
	INDEX < MAX,
	memberchk(HEAD,VARS), name(HEAD, CHARS),
	BUMPED_INDEX is INDEX + 1,name(BUMPED_INDEX,SUFFIX_CHARS),
	append(CHARS,SUFFIX_CHARS,NEW_CHARS), name(NEW_NAME, NEW_CHARS),!,
	ensure_unique(NEW_NAME,VARS,BUMPED_INDEX,MAX,NP_VAR).
ensure_unique(HEAD,VARS,INDEX,MAX,HEAD) :- INDEX >= MAX.

% HANDLING SUBORDINATE SENTENCES

% add_subordinate_descriptors(+VAR,+TYPE,+SUB,+IN,-OUT,+VARS_IN,-VARS_OUT)
% adds subordinate descriptors (SUMO forms) to the current list of forms in IN
% to produce a new list of forms adding the semantics of the sentence, this
% amended list is returned as OUT. VARS_IN are the variables defined so far
% in the sentence (must be referred to to avoid duplication) and any new variables
% added are guaranteed unique and the complete list is returned as VARS_OUT.
% SUB is the semantic frame describing the subordinate sentence and has the
% exact same components as for top-level sentences (e.g., predicate, subject,
% and direct object). VAR is the subject of the noun phrase that is being modified
% by this subordinate sentence.

% Example: in "The teller who John likes enters the store." the noun phrase
% with the subordinate sentence is "the teller who John likes" and the subject
% of the noun phrase is "the teller", which would be represented as VAR '?teller'
% with TYPE 'Teller'.

% In the subordinate sentence "who John likes" the gap (the missing direct object)
% can be plugged with 'who' as if the sentence were "John likes who" producing
% the SUMO clauses:  (likes John ?who) and (instance ?who Person).

% The SUMO clauses from "The teller enters the store." would be:
%    (agent ?enters ?teller)
%    (patient ?enters ?store)
%    (instance ?enters Enters)
%    (instance ?teller Teller)

% and to combine these we add (equal ?teller ?who).

% Note: the reason we do not just generate the subordinate clauses
% using ?teller instead of ?who is because later, when processing
% queries, we will want the query variable to remain in place. Overall
% we want the handling of sentence semantics to be as context-independent
% as possible and not dependent on whether they are embedded in an NP
% or not. So in either case, subordinate sentences or queries, we first
% generate clauses with the gap variable (e.g., ?who) in place.

add_subordinate_descriptors(VAR,TYPE,SUBORDINATE_SENTENCE_SEMANTIC_FEATURES,IN,OUT,VARS_IN,VARS_OUT) :-
	extract_gap_variable(SUBORDINATE_SENTENCE_SEMANTIC_FEATURES,GAP), % extract GAP feature
	COREFERENCE = [equal,VAR,GAP],
	traverse_sentence_semantic_frames(SUBORDINATE_SENTENCE_SEMANTIC_FEATURES,[],SUMO_FORMS,VARS_IN,VARS_OUT),
        SUBORDINATE_FORMS = [COREFERENCE|SUMO_FORMS],
	reverse(SUBORDINATE_FORMS,REORDERED_FORMS),
	append(IN,REORDERED_FORMS,OUT).

% extract_gap_variable(+SEMANTICS,-VAR)
% given the semantic frames of a subordinate sentence (pred, subj, and possibly dobj and possibly iobj)
% return the gap variable used.

extract_gap_variable(subj<->SUBJECT,HEAD) :- SUBJECT = head<->HEAD, memberchk(HEAD,['?who','?which','?that']),!.

extract_gap_variable(dobj<->DIRECT_OBJECT,HEAD) :- DIRECT_OBJECT = head<->HEAD, memberchk(HEAD,['?who','?which','?that']),!.

extract_gap_variable(iobj<->INDIRECT_OBJECT,HEAD) :- INDIRECT_OBJECT = head<->HEAD, memberchk(HEAD,['?who','?which','?that']),!.

extract_gap_variable(ALL_ELSE,'???who'). % Should not fall through to here but provide a distinctive var if it does.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CASE ROLE DETERMINATION FOR VERBS  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CELT used to map the subject grammatical position
% always to the agent case role, the direct object position
% to the patient case role, and the indirect object position to
% the destination case role in SUMO.
% 
% Obviously that is not right for these examples:
% 
%     'The door shuts.' (here 'door' should take the 'patient' role)
% 
%     'He drives up Highway 101.' (here 'Highway 101' should 
% take the 'path' role)
% 
%     'The cat smells the bird.' (here 'cat' takes the role 
% 'experiencer')
% 
% So these changes were made to CELT:
% 
% 1. By default each verb maps grammatical positions
% [subject, direct_object, indirect_object] to
% case roles [agent, patient, destination] except ...
% 
% 2. All verbs of perceiving map [subject, direct_object] map to 
% [experiencer, patient]
% 
% 3. All verbs of traveling or locomotion map [subject, 
% direct_object] to [agent, path] if the subject can be an agent (e.g., 'The man follows the 
% trail.') or to  [patient, path] if the subject is not an agent (e.g., 'The stream follows the 
% trail.') and in both cases if the direct object can be a path.
% 
% 4. All intransitive verbs map [subject] to [agent] if the 
% subject can be an agent (e.g., 'The bird flies over the lake.') but to [patient] 
% otherwise (e.g., 'The plane flies over the lake.') Similarly, 'The door shuts.' would map 
% 'door' to the patient role.
% 
% 5. Similarly for transitive and ditransitive verbs:
% 
% E.g., 'The man shuts the door.' maps to case roles [agent, 
% patient] but 'The wind shuts the door.' maps to case roles [patient, destination]
% 
% E.g., 'He gives the book to John.' maps to case roles [agent, 
% patient, destination] but 'The final result gives the win to John.' maps to case roles 
% [patient,instrument,destination]
% 
% 6. Any object in a 'with...' prepositional phrase is mapped to 
% 'instrument'. [Note: SUMO does not have a commitative, otherwise this rule would map agents to 
% commitatives and non-agents to instruments.]
% 
% 7. In a 'from ... to ... ' combination of prepositional 
% expressions the  object in the from preposition maps to the 'origin' role and the object of the to 
% preposition maps to the 'destination' role.
% 
% 8. Objects that are directions map to the 'direction' role 
% regardless of the other rules above. (e.g., 'John walks to the west.')

% determine_case_role(+GrammaticalPosition, +Transitivity,
%                     +Verb_WordNet_Sense, +SUMO_Verb_Concept,
%                     +Noun_WordNet_Sense, +SUMO_Noun_Concept, 
%                     -SUMO_Case_Role,-Warnings)
% determines the case role for a one of the objects of a verb
% GrammaticalPosition is one of [subject,direct_object,indirect_object]
% The result returned is a SUMO case role, one of [agent,destination,patient,path,direction]
% The other parameters provide WordNet Synset IDs and the SUMO concepts they are mapped to.
% Warnings flags when a noun should be an agent but cannot be shown to be one.

% handle verbs of perception specially, regardless of verb transitivity

determine_case_role(subject,_,                      % general properties
		    Verb_Synset_ID,SUMO_Verb,       % verb properties
		    Noun_Synset_ID,SUMO_Noun,       % object properties
		    'experiencer',Warnings) :-      % case role assigned, e.g., for 'John' in 'John sees the cat.'
		    verb_of_perception(Verb_Synset_ID,SUMO_Verb),
		    !,
		    % now generate a translation warning if the object cannot be shown to be a member of SUMO class 'Agent'
		    ( can_be_agent(Noun_Synset_ID,SUMO_Noun) ->
			Warnings = [];
			sformat(Warning,"Noun ~w should be an agent, but it maps to SUMO type ~w, which is not an instance of 'Agent'.",
			       [Noun_Synset_ID,SUMO_Noun]),
			Warnings = [Warning]
		    ).

% the subject of an intransitive verb is always an agent (e.g., 'The man sings.')
% except for when the noun is a thing (e.g., 'The iron rusts.')

determine_case_role(subject,intransitive,           % general properties
		    _,_,                            % verb properties
		    Noun_Synset_ID,SUMO_Noun,       % object properties
		    'agent',Warnings) :-            % case role assigned, e.g., for 'John' in 'John sings.'
		    can_be_agent(Noun_Synset_ID,SUMO_Noun),
		    !.

determine_case_role(subject,intransitive,   % general properties
		    _,_,                    % verb properties
		    _,_,                    % object properties
		    'patient',Warnings) :-  % case role assigned, e.g., for 'door' in 'The door effloresces.'
		    !.

% now handle direct objects or indirect objects that are directions

determine_case_role(direct_object,_,                % general properties
		    _,_,                            % verb properties
		    Noun_Synset_ID,SUMO_Noun,       % object properties
		    'direction',Warnings) :-        % case role assigned
		    noun_that_is_a_direction(Noun_Synset_ID,SUMO_Noun),
		    !.

% now handle transitive or ditransitive verbs of traveling and locomotion specially
% if the object can be a path	    

determine_case_role(direct_object,_,                % general properties
		    Verb_Synset_ID,SUMO_Verb,       % verb properties
		    Noun_Synset_ID,SUMO_Noun,       % object properties
		    'path',Warnings) :-             % case role assigned
		    verb_of_locomotion(Verb_Synset_ID,SUMO_Verb),
		    noun_that_is_a_path(Noun_Synset_ID,SUMO_Noun),
		    !.

% Default: a subject maps to the 'Agent' case role.

determine_case_role(subject,_,                   % general properties
		    _,_,                         % verb properties
		    _,_,                         % object properties
		    'agent',Warnings) :-         % case role assigned & translation warnings generated
		    % now generate a translation warning if the object cannot be shown to be a member of SUMO class 'Agent'
		    ( can_be_agent(Noun_Synset_ID,SUMO_Noun) ->
			Warnings = [];
			sformat(Warning,"Noun ~w should be an agent, but it maps to SUMO type ~w, which is not an instance of 'Agent'.",
			       [Noun_Synset_ID,SUMO_Noun]),
			Warnings = [Warning]
		    ).

% Default: a direct object maps to the patient role.

determine_case_role(direct_object,_,             % general properties
		    _,_,                         % verb properties
		    _,_,                         % object properties
		    'patient',Warnings).         % case role assigned & translation warnings generated

% Default: an indirect object maps to the destination role.

determine_case_role(indirect_object,_,           % general properties
		    _,_,                         % verb properties
		    _,_,                         % object properties
		    'destination',Warnings).     % case role assigned & translation warnings generated


% We should never fall thru to here, if so generate a warning.

determine_case_role(Position,_,                     % general properties
		    Verb,Verb_Synset_ID,SUMO_Verb,  % verb properties
		    Noun,Noun_Synset_ID,SUMO_Noun,  % object properties
		    unknown,[Warning]) :-           % case role assigned & translation warnings generated
		    sformat(Warning,"Error, CELT could not assign a case role for the ~w position of verb ~w, occupied by noun ~w.",
			    [Position,Verb,Noun]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type determination functions used in determining case roles.%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% verb_of_perception(+Verb_Synset_ID,+SUMO_Verb)
% succeeds if the verb is a verb of perception.

% Note: Reasoning is over WordNet hierarchy first as
% that defines more verbs of perception at present then
% SUMO. But the SUMO hierarchy is also checked in case
% new terms are defined, as in a new lower-level ontology,
% as those new terms would not be part of WordNet but
% would be mapped onto SUMO.

verb_of_perception(Verb_Synset_ID,SUMO_Verb) :-
	is_hypernym_in(Verb_Synset_ID,[201442173,          % Note: WordNet 201442173 is to perceive.	
                                       201444597,          % Note: WordNet 201444597 is to experience, e.g., to go or live through
				       201209074,          % Note: WordNet 201209074 is to feel an emotion (e.g., to love or hate)
				       201245362,          % Note: WordNet 201245362 is to desire or want
				       201213391,          % ...a sense of 'like'...
				       201213205,          % ...a sense of 'like'...
				       200452184,          % ...a sense of 'like'...
				       201212998,          % ...a sense of 'dislike'...
				       201211759]);        % ...a sense of 'love'...
	is_subclass_or_equal(SUMO_Verb,'Perception').      % Note: SUMO class 'Perception' includes verbs of perceiving

% verb_of_locomotion(+Verb_Synset_ID,+SUMO_Verb)
% succeeds if the verb is a verb of locomotion.

verb_of_locomotion(Verb_Synset_ID,SUMO_Verb) :-
	is_hypernym_in(Verb_Synset_ID,[201253107,201263706,201249365,201267341]);  % Note: these synsets are the first 3 senses of 'move'
	is_subclass_or_equal(SUMO_Verb,'Motion').          % Note: SUMO class 'Motion' includes verbs of moving

% noun_that_is_a_direction(+Noun_Synset_ID,+SUMO_Noun).
% succeeds if the noun is a direction (east, west, up, down...)

noun_that_is_a_direction(Noun_Synset_ID,SUMO_Noun) :-
	is_hypernym_of(Noun_Synset_ID,109959689);                    % Note: WordNet 109959689 is to perceive.	
	is_subclass_or_equal(SUMO_Noun,'DirectionalAttribute').      % Note: SUMO class 'DirectionalAttribute' includes directions

% noun_that_is_a_path(+Noun_Synset_ID,+SUMO_Noun).
% succeeds if the noun can be a path (trail, perimeter, edge, sidewalk...)

noun_that_is_a_path(Noun_Synset_ID,SUMO_Noun) :-
	is_hypernym_of(Noun_Synset_ID,100014887);                    % Note: WordNet 100014887 is location, a pt or extent in space
	is_subclass_or_equal(SUMO_Noun,'GraphPath').                 % Note: SUMO class 'GraphPath' includes directions	

% can_be_agent(+Noun_Synset_ID,+SUMO_Noun).
% succeeds if the noun can be an agent (man, girl, dog, robot, etc.)
% under normal conditions. Objects such as (rock, leaf, etc.)
% fail.

% Note: Reasoning is over SUMO hierarchy as that defines
% what a SUMO agent is, i.e., it must be an instance of
% SUMO class 'Agent'.

% We also allow for proper nouns found in Wordnet: these
% inherit from person, synset ID, 100004123.

can_be_agent(Noun_Synset_ID,SUMO_Noun) :-
	not(var(SUMO_Noun)),                      % To prevent binding the SUMO noun if it is unbound.
	is_subclass_or_equal(SUMO_Noun,'Agent').

can_be_agent(Noun_Synset_ID,SUMO_Noun) :-
	not(var(Noun_Synset_ID)),                 % To prevent binding the synset ID if it is unbound.
	is_hypernym_of(Noun_Synset_ID,109959689). % Note: WordNet 100004123 is a particular living person

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Multi-arg sentences for predefined relations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Some domains, such as the ATO domain, require the code for
% a single relation expression to be expressed in one sentence
% which has multiple arguments, each corresponding to a parameter
% for the relation.
		    
% relation(air_mission_relation,[a,new,air,mission],ato_mission,
%	 [
%	  spec(keywords([has,a,primary,target]),attribute([first,target]),type('ATOTarget')),    % first arg of air_mission_relation
%	  spec(keywords([has,a,secondary,target]),attribute([second,target]),type('ATOTarget')), % second arg of air_mission_relation
%	  spec(keywords([refuels,at]),attribute([refueling,point]),type('Place')),               % third arg of air_mission_relation
%	  spec(keywords([has,a,waypoint,at]),attribute([waypoint]),type('Place')),               % fourth arg of air_mission_relation
%	  spec(keywords([has,type]),attribute([mission,type]),type('ATOMissionType'))            % fifth arg of air_mission_relation
%	 ]).

% Air Mission 1 is a new air mission that has a primary target Target A and second target Target B and  refuels at Location 5 and mission type combat air patrol.

% test(sentence,['Mission','1',is,a,new,air,mission,that,has,a,primary,target,'Target','A',
%	         and,second,target,'Target','B',and,refuels,at,'Location','5',and,mission,type,combat,air,patrol],yes).

% build_multi_arg_relation(+Sentence_Features,-EXISTENTIAL)
% builds the multi-arg relation described in the sentence by looking up
% the relation given its relation name and then pulling the actual parameter
% values from the parsed list, given the target parameter specifications.

build_multi_arg_relation(Sentence_Features,EXISTENTIAL) :-
	Sentence_Features = sem<->(specs<->ACTUAL_VALUES..pred<->RELATION_NAME),
	relation(RELATION_LONG_NAME,RELATION_WORDS,RELATION_NAME,PARAMETER_SPECS),
	build_relation(RELATION_NAME,PARAMETER_SPECS,ACTUAL_VALUES,EXISTENTIAL).

% build_relation(+RELATION_NAME,+PARAMETER_SPECS,+ACTUAL_VALUES,-EXISTENTIAL)
% builds the relation predicate given its name, its parameter specs and the
% actual value parsed.

build_relation(RELATION_NAME,PARAMETER_SPECS,ACTUAL_VALUES,EXISTENTIAL) :-
	build_arg_list(PARAMETER_SPECS,ACTUAL_VALUES,ARG_LIST),
	EXISTENTIAL = [RELATION_NAME|ARG_LIST].

% build_arg_list(+PARM_SPECS,+PARM_VALUES,-ARG_LIST)
% builds the argument list for the relation by pulling the proper
% value for each parameter in the parameter specs.

build_arg_list([],ACTUAL_VALUES,[]).
build_arg_list([SPEC|REST_OF_SPECS],ACTUAL_VALUES,[NEW_ARG|REST_OF_ARGS]) :-
	build_arg(SPEC,ACTUAL_VALUES,NEW_ARG),
	build_arg_list(REST_OF_SPECS,ACTUAL_VALUES,REST_OF_ARGS).

% build_arg(+TARGET_SPEC,+ACTUAL_VALUES,-ARG_VALUE)
% build_arg attempts to pull the argument value for the given parameter spec
% from the parameter values actually parsed.

build_arg(TARGET_SPEC,ACTUAL_VALUES,ARG_VALUE) :-
	TARGET_SPEC = spec(keywords(KEYWORDS),attribute(ATTRIBUTE_WORDS),type(TYPE)),
	( find_arg_val(TARGET_SPEC,ACTUAL_VALUES,ARG_VALUE) -> true;
	    format("Could not find a value for argument with keywords [~w] and attribute words [~w].~n",
		   [KEYWORDS,ATTRIBUTE_WORDS]),
	    ARG_VALUE = empty).

% find_arg_val(+TARGET_SPEC,+ACTUAL_VALUES,-ARG_VALUE)
% find_arg_val succeeds if it finds a parameter spec with a parsed noun phrase
% to match the target spec. If so it returns the head of the noun phrase as the
% value to use for the argument.	

find_arg_val(TARGET_SPEC,[pair(TARGET_SPEC,NOUN_PHRASE_PARSE)|OTHER_VALS],NOUN) :-
	!,
	NOUN_PHRASE_PARSE = sem<->(noun<->NOUN_WORDS..head<->NOUN_OR_VAR),
	(is_KIF_var(NOUN_OR_VAR) ->
	      (NOUN = Hyphenated_Parameter_Value,
		  (is_list(NOUN_WORDS) -> 
		      concat_atom(NOUN_WORDS,'_',Hyphenated_Parameter_Value);   % Put a list of words together as one atomic value
		      Hyphenated_Parameter_Value = NOUN_WORDS)          	% A single atom can be used as is
		  );
	      NOUN = NOUN_OR_VAR).   % A constant from a proper noun can just be passed along.

find_arg_val(TARGET_SPEC,[pair(_,_)|OTHER_VALS],NOUN) :- find_arg_val(TARGET_SPEC,OTHER_VALS,NOUN).

	