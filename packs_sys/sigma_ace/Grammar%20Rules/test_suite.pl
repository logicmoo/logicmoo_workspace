% Copyright © 2002 Teknowledge Corporation
% This software released under the GNU Public License <http://www.gnu.org/copyleft/gpl.html>.

% This file provides a test suite to test any ACE implementation.

:- style_check(-atom).           % allows strings more than 5 lines long
:- style_check(-discontiguous).  % allows more creative predicate placement

% Versions of ACE / CELT:
% v1 -- simple sentences only, can include relative sentences, but not conjunctions, disjunctions, quantifiers, or queries.
% v2 -- adds queries, both wh-queries and Yes/No queries for simple sentences in v1.
% v3 -- adds if-then sentences and sentences with quantifiers, including every, 'there is', 'does not' and 'No...'.
% v4 -- adds multiple sentences and other the remaining compounds for single sentences, including conjunction and disjunction.

% Test suite to test ACE implementations:
%   --are valid ACE sentences parsed correctly?
%   --are valid ACE sentences paraphrased correctly when a paraphrase is appropriate?
%   --are valid ACE questions answered correctly?
%   --are ambiguous quantifiers, binding hierarchies, and coordinated constructs
%     interpreted as specified in the ACE manual?
%   --are KIF translations as expected?

% parse(+version, +testID,+sentence,+comment,-KIF).
% each sentence in the test should parse successfully and produce the KIF formula given.

% test_paraphrase(+version, +paraphraseID,+given_sentence,-paraphrased_sentence,+comment,-KIF)
% each given sentence should parse correctly and provide the paraphrase and KIF given.

% test_query(+version, +queryID,+spec,+query,+answer,+comment,
%                -KIFspec,-KIFquery,-KIFanswer)
% tests to see if the query given can be asked and answered with the information given in the spec.
% e.g., test_query(1,"John enters a card.,"Who enters a card?","John."). If "John enters a card." has been
% entered then "Who enters a card?" should be answered with the answer given ("John.") or as the complete
% sentence in the original spec ("John enters a card."). The comment describes the kind of query tested.

% test_interpret(+version,+interpretationID,+ambiguous_sentence,+ACE_interpretation,
%                                  +alternate_possibly_intended_interpretation,
%                                  -KIF_for_ACE_interpretation)
% tests that the logical formula produced by the ambiguous sentence is the same as the ACE interpretation and
% different from the alternate possibly intended interpretation.

% disallowed(+disallowedID,+testID,+disallowed_sentence,+comment)
% the disallowed sentence should fail to parse but should generate a clear error message
% using the comment that explains the problem and a workaround, using the sentence in testID
% as an example of how to reexpress the same meaning in a way that ACE can accept.

% note(+noteID,-note)
% provides text that can be used to further explain how ACE works.
% note(1.0,"The textural occurrence of a quantifier opens its scope that extends to the end of the sentence.").

% Translation notes in KIF formulas below:
%    Many examples involve inserting cards into an ATM. For these examples, typically the
%    grammatical terms to the left correspond to the case roles in the SUO given below:
%        subject         -- agent,
%        direct object   -- instrument,
%        indirect object -- destination

% Uses SUO + Financial Ontology, the latter for:

% BankCard, which includes CreditCard and DebitCard,
% MasterCard, a kind of CreditCard.
% VisaCard, a kind of CreditCard.
% cardCode, where (cardCode ?Code ?Card) means that ?Code represents the account number of the BankCard ?Card.
% customer, where (customer ?Agent1 ?Agent2) for two cognitive agents.
% ValidCard, the class of valid bank cards.
% EnteringAPin, a kind of authorization of transaction.
% VerifyingCardCode, also a kind of authorization of transaction.
% ATMMachine, a kind of stationary artifact.
% Manager, a kind of position.
% ATMSlot, a kind of hole.

% KIF Terms not present in the SUO or Financial Ontology that must be added:

% Swiss, as in a Swiss customer
% Manually, as in "a customer inserts a card manually".
% Morning, as in "...in the morning", using Ian's HourIntervalFn and specific times.

%%%%%%%%%%%%%%%%%%%
% SENTENCE PARSES %
%%%%%%%%%%%%%%%%%%%

% simple sentences
test_parse(v1, 1.0,"A customer inserts a card.","a simple sentence describing an event",
"(exists
   (?customer ?service ?card ?event) 
   (and
      (customer ?customer ?service)
      (instance ?card BankCard)
      (instance ?event Putting)
      (agent ?event ?customer)
      (instrument ?event ?card))))").

test_parse(v1, 2.0,"A card is valid.","a simple sentence describing a state",
"(exists
   (?card) 
     (and
       (instance ?card BankCard)
       (instance ?card ValidCard))").


% Enhancements

test_parse(v1, 3.0,"A Swiss customer inserts a valid card.","adjectives in a simple sentence",
"(exists
   (?customer ?service ?card ?event)
   (and
      (customer ?customer ?service)
      (member ?customer Swiss)
      (instance ?card BankCard)
      (instance ?card ValidCard)
      (instance ?event Putting)
      (agent ?event ?customer)
      (instrument ?event ?card)))").


test_parse(v1, 4.0,"John's customer inserts a card of Mary.","possessive nouns and of-prepositional phrases",
"(exists
      (?customer ?card ?event ?organization)
      (and
         (instance ?card BankCard)
         (instance ?event Putting)
         (customerRepresentative John ?customer ?organization)
         (possesses Mary ?card)
         (agent ?event ?customer)
         (instrument ?event ?card)))").

test_parse(v1, 5.0,"The customer Mr Miller inserts a card A.","proper nouns and dynamic names as appositions",
"(exists
   (?service ?event)
   (and
      (customer MrMiller ?service)
      (instance CardA BankCard)
      (instance ?event Putting)
      (agent ?event MrMiller)
      (instrument ?event ?card)))").

% Notes on test 5:
%   1. ACE does not allow a period within a sentence so no period should follow "Mr" in "Mr Miller".
%   2. We do not have a (name ... ) predicate since the name info is stored separately when the
%   constant "Mr Miller" is created. When ACE recognizes the proper noun it should create a constant
%   if one has not already been created.


test_parse(v1, 6.0,"A customer who is Swiss inserts a card that he owns.","relative sentences",
"(exists
    (?customer ?service ?card ?event)
    (and
       (customer ?customer ?service)
       (member ?customer Swiss)
       (instance ?card BankCard)
       (instance ?event Putting)
       (agent ?event ?customer)
       (instrument ?event ?card)
       (possesses ?customer ?card)))").

% Notes on test 6:
%   1. Original sentence was "A customer who is new..." but 'Swiss' is easier to translate than 'new'.


test_parse(v1, 7.0,"A customer inserts a card manually.","adverb",
"(exists
     (?customer ?service ?card ?event)
     (and
        (customer ?customer ?service)
        (instance ?card BankCard)
        (instance ?event Putting)
        (agent ?event ?customer)
        (instrument ?event ?card)
        (manner ?event Manually)))").

% Notes on test 7:
%   1. Manually is not in the SUO, nor is Hand, need to add a definition here!

test_parse(v1, 8.0,"A customer manually inserts a card.","adverb", % same KIF as above...
"(exists
    (?customer ?service ?card ?event)
    (and
       (customer ?customer ?service)
       (instance ?card BankCard)
       (instance ?event Putting)
       (agent ?event ?customer)
       (instrument ?event ?card)
       (manner ?event Manually)))").

% Notes on test 8:
%   1. Manually is not in the SUO, nor is Hand, need to add a definition here!

test_parse(v1, 9.0,"A customer inserts a card into a slot.","adverb",
"(exists
    (?customer ?service ?card ?ATMSlot ?event)
    (and
       (customer ?customer ?service)
       (instance ?card BankCard)
       (instance ?event Putting)
       (instance ?ATMSlot ATMSlot)
       (agent ?event ?customer)
       (instrument ?event ?card)
       (destination ?event ?ATMSlot)))").

% Notes on test 9:
%   1. Since the context of conversation is ATM operation, 'slot' is interpreted as 'ATMSlot'
%   as opposed to 'trash slot' or some other slot.

test_parse(v1, 10.0,"A customer inserts a card in the morning.","adverb",
"(exists
   (?customer ?service ?card ?event ?morning)
   (and
      (customer ?customer ?service)
      (instance ?card BankCard)
      (instance ?event Putting)
      (agent ?event ?customer)
      (instrument ?event ?card)
      (instance ?morning Morning)
      (temporalPart (WhenFn ?event) ?morning)))").

test_parse(v1, 11.0,"John's customer who is Swiss inserts a valid card of Mary manually into the slot A.",
          "a simple sentence with all enhancements",
"(exists
    (?customer ?organization ?card ?event)
    (and
       (customerRepresentative John ?customer ?organization)
       (member ?customer Swiss)
       (instance ?card BankCard)
       (instance ?card ValidCard)
       (possesses Mary ?card)
       (instance ?event Putting)
       (agent ?event ?customer)
       (instrument ?event ?card)
       (manner ?event Manually)
       (destination ?event SlotA))").

% composite / compound sentences

test_parse(v4, 12.0,"A customer inserts a card and the machine checks the code.",
          "coordination by AND between phrases of the same syntactic type",
          "(exists (?customer ?service ?card ?machine ?code ?event1 ?event2)
		   (customer ?customer ?service)
                   (instance ?card BankCard)
                   (instance ?event1 Putting)
                   (agent ?event1 ?customer)
                   (instrument ?event1 ?card)
                   (instance ?machine ATMMachine)
                   (instance ?code Code)
                   (instance ?event2 VerifyingCardCode)
                   (agent ?event2 ?machine)
                   (instrument ?event2 ?code)
                   (before (WhenFn ?event1) (WhenFn ?event2)))").



test_parse(v4, 13.0,"A customer inserts a card or enters a PIN.",  
          "coordination by OR between phrases of the same syntactic type",
          % KIF note: one of the two events happens. Same as "A customer inserts a card or a customer enters a PIN."
          "(or (exists (?customer ?service ?card ?event)
		       (and (customer ?customer ?service)
			    (instance ?card BankCard)
			    (instance ?event Putting)
			    (agent ?event ?customer)
			    (instrument ?event ?card)))
	       (exists (?customer ?service ?card ?pin ?event)
		       (and (customer ?customer ?service)
			    (pin ?pin ?card)
			    (instance ?event EnteringAPin)
			    (agent ?event ?customer))))))").

% Notes on test 13:
% 1. The original sentence Was "A customer inserts a card or enters a code." but we are using
% code to mean the account code on the bank card, so PIN seems to be what was intended here.

test_parse(v4, 14.0,"An armed and uniformed customer enters a card and a PIN in the morning or in the evening.",
          "coordination by AND or OR between phrases of the same syntactic type",
          "(exists (?customer ?service ?card ?code ?event1 ?event2 ?time)
                   (and
		      (customer ?customer ?service)
                      (attribute ?customer WellDressed)
                      (attribute ?customer Rich)
                      (instance ?event1 Putting)
                      (instrument ?event1 ?card)
                      (agent ?event1 ?customer)
                      (instance ?event2 EnteringAPin)
                      (agent ?event2 ?customer)
		      (before (WhenFn ?event1) (WhenFn ?event2))
		      (or 
                        (and (instance ?time Morning)
			     (temporalPart (WhenFn ?event2) ?time))
                        (and (instance ?time Evening)
			     (temporalPart (WhenFn ?event2) ?time)))))").

% Notes on test 14:
% 1. The original sentence Was "an old and trusted customer", but "an armed and uniformed customer" is easier to translate.
% 2. The sentence expands to "A rich and well-dressed customer enters a card and enters a PIN in the morning or in the evening."
%    "In the morning or in the evening" refers to the second verb in ACE, the second event or entering, by Principle of Right Attachment.
% 3. In the auxiliary KIF definitions for this ACE test suite, we
%    define morning as between any time after midnight and 12 noon, and
%    evening as between 5 PM and 8 PM.

test_parse(v4, 15.0,"A customer inserts a card in the morning into the slot.",
          "the AND-coordination of prepositional phrases can omit the AND",
          "(exists (?customer ?service ?card ?ATMSlot ?event)
                   (and
		      (customer ?customer ?service)
                      (instance ?card BankCard)
		      (instance ?ATMSlot ATMSlot)
		      (instance ?event Putting)
		      (agent ?event ?customer)
		      (instrument ?event ?card)
		      (destination ?event ?ATMSlot)
		      (time ?event Morning)))").


test_parse(v4, 16.0,"A customer inserts a card in the morning and into the slot.",
          "the AND-coordination of prepositional phrases can omit the AND or include it, as here",
          "(exists (?customer ?service ?card ?ATMSlot ?event ?morning) % same KIF as above
                   (and
		      (customer ?customer ?service)
                      (instance ?card BankCard)
		      (instance ?ATMSlot ATMSlot)
		      (instance ?event Putting)
		      (agent ?event ?customer)
		      (instrument ?event ?card)
		      (destination ?event ?ATMSlot)
		      (instance ?morning Morning)
		      (temporalPart (WhenFn ?event) ?morning)))").

test_parse(v4, 17.0,"A customer inserts a VisaCard and inserts a Mastercard.",
          "coordination of verb phrases can be simplified by omitting the repeated verb",
          % KIF note: two events happen, one for each card insertion.
          "(exists (?customer ?service ?card1 ?card2 ?event1 ?event2)
                   (and 
			(customer ?customer ?service)
                        (instance ?card1 VisaCard)
		        (instance ?event1 Putting)
		        (agent ?event1 ?customer)
		        (instrument ?event1 ?card1)		   
		        (instance ?event2 Putting)
		        (agent ?event2 ?customer)
		        (instance ?card2 MasterCard)
		        (instrument ?event2 ?card2)
			(before (WhenFn ?event1) (WhenFn ?event2))))").



test_parse(v4, 18.0,"A customer inserts a VisaCard and a Mastercard.", 
           % paraphrased as: "A customer inserts a VisaCard and [inserts] a Mastercard.",
          "coordination of verb phrases can be simplified by omitting the repeated verb",
          "(exists (?customer ?card1 ?card2 ?event1 ?event2) % same KIF as above
                   (and (customer ?customer ?service)
                        (instance ?card1 VisaCard)
		        (instance ?event1 Putting)
		        (agent ?event1 ?customer)
		        (instrument ?event1 ?card1)		   
		        (instance ?event2 Putting)
		        (agent ?event2 ?customer)
		        (instance ?card2 MasterCard)
		        (instrument ?event2 ?card2)
			(before (WhenFn ?event1) (WhenFn ?event2))))").

test_parse(v4, 19.0,"A card is valid or is invalid.",
          "coordination of copula verb phrases can be simplified by omitting the repeated verb",
          "(exists (?card) (or (instance ?card ValidCard) (not (instance ?card ValidCard))))").

% Notes on test 19:
%   1. This translates to a tautology.
%   2. In English this might be taken to mean the same as the ACE statement "Every card is valid or is invalid."
%      but in ACE this just refers to some card when "a card" is used.

test_parse(v4, 20.0,"A card is valid or invalid.",
           % paraphrased as "A card is valid or [is] invalid.",
          "coordination of copula verb phrases can be simplified by omitting the repeated verb",
          "(exists (?card) (or (instance ?card ValidCard) (not (instance ?card ValidCard))))"). % same KIF as above

test_parse(v4, 21.0,"A customer does not insert a card and a PIN.",
          % paraphrased as: "A customer does not insert a card and [does not insert] a PIN.",
          "coordination of copula verb phrases can be simplified by omitting the repeated verb",
          "(exists (?customer ?service)
                   (and (customer ?customer ?service)
                        (not (exists ?event1 ?event2 ?card ?pin)
			     (and
			         (instance ?card BankCard)
			         (pin ?pin ?card)
			         (instance ?event1 Putting)
			         (agent ?event1 ?customer)
			         (instrument ?event1 ?card)
			         (instance ?event2 EnteringAPin)
			         (agent ?event2 ?customer)				   
			         (before ?event1 ?event2)))))").

% Notes on test 21.
%   1. Originally used "code" instead of "PIN" but PIN seems to be what is intended here.
%   2. Translated as opposite of "A customer does insert a card and a PIN."

test_parse(v4, 22.0,"A customer inserts a VisaCard or a MasterCard and a personal code.",
          "in coordination of AND and OR, AND binds stronger than OR",
          "(or
              (exists (?customer ?service ?card ?event)
                      (and 
                           (customer ?customer ?service)
                           (instance ?card VisaCard)
                           (instance ?event Putting)
                           (agent ?event ?customer)				   
                           (instrument ?event ?card)))
              (exists (?customer ?service ?card ?pin ?event1 ?event2)
		        (and
                           (customer ?customer ?service)
		           (instance ?card MasterCard)
		           (instance ?event1 Putting)
			   (agent ?event1 ?customer)				   
		           (instrument ?event1 ?card))
		           (instance ?event2 EneringAPin)
			   (agent ?event2 ?customer)				   
		           (before ?event1 ?event2)))))").

% Notes on test 22.
%   1. Translating "personal code" to "PIN" here.
%   2. Interpreted as "The customer enters a VisaCard or the customer enters a Mastercard and a personal code." p.19.


test_parse(v4, 23.0,"A customer inserts a VisaCard or a MasterCard, and a PIN.",
          "commas can be used to override standard binding order",
          "(and
              (exists (?customer ?service ?card ?code ?event)
                   (and (customer ?customer ?service)
		        (or (instance ?card VisaCard)
			    (instance ?card MasterCard))
		        (instance ?event1 Putting)
		        (agent ?event ?customer)				   
		        (instrument ?event ?card))
              (exists (?customer ?service ?card ?pin ?event)	      
	           (and
		        (customer ?customer ?service)
		        (pin ?pin ?card)

		        (instance ?event2 Putting)
		        (agent ?event2 ?customer)				   
		        (instrument ?event2 ?code)))").      

% Notes on test 23.
%   1. Changed "code" to "PIN" in the test sentence.
%   2. Interpreted as "The customer enters a VisaCard or a Mastercard, and [the customer inserts] a personal code.

% there are two forms of subordination: relative sentences and if-then sentences

% relative sentences start with WHO, WHICH, or THAT

test_parse(v1, 24.0,"A customer who is Swiss inserts a card that he owns.",
          "relative sentence starting with WHO",
          "(exists (?customer ?service ?card ?event)
                   (and (customer ?customer ?service)
		        (member ?customer Swiss)
		        (attribute ?customer Male)
		        (instance ?card BankCard)
		        (possesses ?customer ?card)
		        (instance ?event Putting)
		        (agent ?event ?customer)
		        (instrument ?event ?card)))").

% Notes on test 24.
%   1. Changed "new" to "Swiss" in the test sentence.

% IF-THEN sentences specify conditional or hypothetical situations

test_parse(v3, 25.0,"If a card is valid then a customer inserts it.",
          "an IF-THEN sentence",
      "(implies (and (instance ?card BankCard)
                     (attribute ?card valid))
                (exists (?customer ?service ?event)
		        (and (customer ?customer ?service)
			     (instance ?event Putting)
		             (agent ?event ?customer)
			     (instrument ?event ?card))))").

% Notes on test 25.
%   1. Could wrap the implies in a (forall (?card)...) but that
%   should be unnecessary. We do want to ensure that ?card in the
%   consequent is the same as ?card in the antecedent.	      

% Quantification

test_parse(v3, 26.0,"Every customer inserts a card.",
          "A universal quantifier: all customers insert cards",
          "(implies (customer ?customer ?service)
                    (exists (?card ?event)
		            (and (instance ?card BankCard)
			         (instance ?event Putting)
		                 (agent ?event ?customer)
			         (instrument ?event ?card))))").

% Notes on test 26.
%   1. Interpreted the same as "Every customer owns a card and inserts that card."

test_parse(v3, 27.0,"There is a card that every customer inserts.",
          "to specify that all customers insert the SAME card",
          "(exists (?card)
                   (and (instance ?card BankCard)
		        (forall (?customer)
			        (exists (?event ?service)
				        (and (customer ?customer ?service)
				             (instance ?event Putting)
		                             (agent ?customer Putting)
			                     (instrument ?card Putting))))))").

% Notes on test 27.
%   1. Interpreted the same as "Every customer owns the card X and inserts the card X."
%   by ACE's use of surface order for deciding order of quantifiers, even if the intuitive
%   English reading may be different.

% the following is not allowed:
disallowed(1.0,28.0,"Every card is inserted by a customer.",
     "ACE does not know the passive voice").

% instead use the following sentence to capture the intended meaning:
test_parse(v3, 28.0,"For every card there is a customer who inserts it.",
     "ACE does not know the passive voice",
     "(forall (?card)
              (implies (instance ?card BankCard)
	               (exists (?customer ?service ?event)
		               (and (customer ?customer ?service)
			            (instance ?event Putting)
		                    (agent ?customer Putting)
			            (instrument ?card Putting)))))").

% important notes to help the user understand how ACE works
note(1.0,"The textural occurrence of a quantifier opens its scope that extends to the end of the sentence.").

% Negation

test_parse(v3, 29.0,"A customer does not insert a card.",
          "negation allows us to express something that is not the case",
           "(exists (?customer ?service)
                    (and (customer ?customer ?service)
		         (not (exists (?card ?event)
			              (instance ?card BankCard)
			              (instance ?event Putting)
		                      (agent ?customer Putting)
			              (instrument ?card Putting)))))").

% Notes on test 29.
%   1. Interpreted the same as "There is no card that a customer inserts."
%   see page 32, surface order of quantifiers.

test_parse(v3, 30.0,"A card is not valid.",
          "negation allows us to express something that is not the case",
          "(exists (?card)
                   (and (instance ?card BankCard)
		        (not (instance ?card ValidCard))))").

% Notes on test 30
%   1. Interpreted the same as "There is a card that is not valid."


test_parse(v3, 31.0,"No customer inserts a card.",
          "to negate something for all objects of a certain class use NO",
          "(not (exists (?customer ?service ?card ?event)
                        (and (customer ?customer ?service)
                             (instance ?event Putting)
		             (agent ?customer Putting)
			     (instrument ?card Putting))))").

% Notes on test 31
%   1. Interpreted the same as "There is no customer who inserts a card."


test_parse(v3, 32.0,"There is no customer who inserts a card.",
          "or equivalently, to negate something for all objects of a certain class use ...",
          "(not (exists (?customer ?service ?card ?event)
                        (and (customer ?customer ?service)
                             (instance ?event Putting)
		             (agent ?customer Putting)
			     (instrument ?card Putting))))").

% Notes on test 32
%   1. Same KIF as in test 31.

%%%%%%%%%%%%%%%%%%%
% QUERY SENTENCES %
%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% YES/NO Queries: Most of these are taken from Table 7, page 60.%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_query(v2, 1.0,"A customer inserts a card.","Does a customer insert a card?","Yes.",
          "a yes/no query",
          "(exists (?customer ?service ?card ?event)    % A customer inserts a card.
                   (and (customer ?customer ?service)
		        (instance ?event Putting)
		        (agent ?customer Putting)
		        (instrument ?card Putting)))",
          "(and (customer ?customer ?service)           % Does a customer insert a card?
                (instance ?event Putting)    
                (agent ?customer Putting)
		(instrument ?card Putting))").

test_query(v2, 2.0,"The card X is valid.","Is the card X valid?","Yes.",
          "a yes/no query",
          "(instance CardX ValidCard)",  % The card X is valid. (assertion)
          "(instance CardX ValidCard)"). % Is the card X valid? (query)

% Notes on query test 2:
%     1. The original was "The card is valid." and "Is the card valid?".
%     To avoid questions of anaphoric reference, which would also require
%     the query and sentence to be entered together, I have identified
%     "the" card as a particular card (card X).
%     2. Similarly, for all similar queries below...

test_query(v3, 3.0,"The card X is not valid.","Is the card X valid?","No.",
          "a yes/no query",
          "(not (instance CardX ValidCard))",  % The card X is not valid.
          "(instance CardX ValidCard)").       % Is the card X valid?

test_query(v3, 4.0,"There is no valid VisaCard.","Is there a valid VisaCard?","No.",
          "a yes/no query",
          "(not (exists (?card)                % There is no valid VisaCard.
                        (and (instance ?card VisaCard)
			     (instance ?card ValidCard))))",
          "(and (instance ?card VisaCard)      % Is there a valid VisaCard?
		(instance ?card ValidCard))").

test_query(v2, 5.0,"John inserts a card.", "Does John insert a card?","Yes.",
          "a yes/no query",
          "(exists (?event ?card)                      % John enters a card.
                   (and (instance ?event Putting)
		        (instance ?card BankCard)
		        (agent ?event John)
		        (instrument ?event ?card)))",
          "(and (instance ?event Putting)              % Does John enter a card?
		(instance ?card BankCard)
		(agent ?event John)
		(instrument ?event ?card))").

% Notes on query test 5: It seems that 'enter' as used in the ACE manual has two meanings:
% 1. Enter a card,
% 2. Enter a room
% but as words are supposed to have only one meaning in ACE we have changed 'enter' to 'insert' here.


test_query(v3, 6.0,"The customer does not enter a valid card.","Does the customer enter a valid card?","No.",
          "a yes/no query",
          "(exists (?customer ?service)               % The customer does not enter a valid card.
                   (and (customer ?customer ?service)
                        (not (exists (?card ?event)
			             (and (instance ?card BankCard)
				          (instance ?card ValidCard)
				          (instance ?event Putting)
				          (agent ?event ?customer)
				          (instrument ?event ?card))))))",
         "(and (customer ?customer ?service)           % Does the customer enter a valid card?
               (instance ?card BankCard)
               (instance ?card ValidCard)
               (instance ?event Putting)
               (agent ?event ?customer)
               (instrument ?event ?card))").

% Comments: a multi-sentence example containing two sequential events.
test_query(v4, 7.0,"The customer inserts a card. He types the PIN of the card.",
          "Does the customer insert a card and type the PIN of the card?",
	  "Yes.",
          "a yes/no query",
          "(exists (?customer ?service ?card ?pin ?event1 ?event2)
                   (and (customer ?customer ?service)
		        (instance ?event1 Putting)
		        (agent ?event1 ?customer)
		        (instrument ?event1 ?card)
		        (pin ?pin ?card)
                        (instance ?event2 EnteringAPin)
                        (instance ?pin PIN)
		        (agent ?event2 ?customer)
		        (before (WhenFn ?event1) (WhenFn ?event2))
                        ))",
        "(and (customer ?customer ?service)
	      (instance ?event1 Putting)
	      (agent ?event1 ?customer)
	      (instrument ?event1 ?card)
	      (instance ?event2 EnteringAPin)
	      (instance ?pin PIN)
              (pin ?pin ?card)
	      (agent ?event2 ?customer)
	      (before (WhenFn ?event1) (WhenFn ?event2))
       )").

% Notes on query test 7:
% 1. Changed 'enter' to 'insert' in test sentence for reasons same as in query 5.
% 2. Changed 'code of the card' to 'PIN of the card' as that seems what is intended rather than our use
% of bankcode to mean account code in the financial ontology.


test_query(v4, 8.0,"The customer inserts a VisaCard.",
	  "Does the customer insert a VisaCard or a MasterCard?",
	  "Yes.",
          "a yes/no query",
          "(exists (?customer ?card ?event)         % The customer enters a VisaCard.
                   (and (customer ?customer ?service)
                        (instance ?card VisaCard)
                        (instance ?event Putting)
			(agent ?event ?customer)
			(instrument ?event ?card)))",
          "(and (customer ?customer ?service)           % Does the customer enter a VisaCard or a MasterCard?
                (or (instance ?card VisaCard)
		    (instance ?card MasterCard))
                (instance ?event Putting)
		(agent ?event ?customer)
		(instrument ?event ?card))").
                        
% Notes on query test 8:
% 1. Changed 'enter' to 'insert' in test sentence for reasons same as in query 5.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5%
% WH-QUERIES: Most of these are from Table 8, page 60.%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% QUERY SUBJECT

test_query(v2, 9.0,"John inserts a card.", "Who inserts a card?", "John.",
          "a WH query with WHO, query subject",
          "(exists (?event ?card)                  % John enters a card.
                   (and (instance ?event Putting)
			(instance ?card BankCard)
			(agent ?event John)
			(instrument ?event ?card)))",
          "(exists (?event ?card)                  % Who inserts a card?
                   (and (instance ?event Putting)
			(instance ?card BankCard)
			(agent ?event ?WHO)        % ?WHO is the only free variable.
			(instrument ?event ?card)))").

% Notes on query test 9:
% 1. Changed 'enter' to 'insert' in test sentence for reasons same as in query 5.

test_query(v2, 10.0,"The card X is valid.","What is valid?","The card.",
          "a WH query with WHAT, query subject",
          "(instance CardX ValidCard)",                % The card X is valid.
          "(instance ?WHAT ValidCard)").               % What is valid?


% QUERY DIRECT OBJECT

test_query(v2, 11.0,"John inserts a card.", "What does John insert?", "A card.",
          "a WH query with WHAT, query direct object",
				% John inserts a card.
          "(exists (?event1 ?card)               
                   (and (instance ?card BankCard)
			(instance ?event1 Putting)
			(agent ?event1 John)
			(instrument ?event1 ?card)))",
				% What does John insert?
          "(and (instance ?event1 Putting)
		(agent ?event1 John)
		(instrument ?event1 ?WHAT))").    

% Notes on query test 11:
% 1. Changed 'enter' to 'insert' in test sentence for reasons same as in query 5.


test_query(v2, 12.0,"The manager George meets the customer Fred.", "Who does the manager George meet?", "The customer Fred.",
          "a WH query with WHO, query direct object",
				% The manager George meets the customer Fred.
           "(exists (?event)                            
                    (and
		        (instance George Manager)
			(customer Fred ?service)
			(instance ?event Meeting)
			(agent ?event George)
			(patient ?event Fred)))",
				% WHO does the manager meet?
           "(exists (?event)   
                    (and
		        (instance George Manager)
			(instance ?event Meeting)
			(agent ?event George)
			(patient ?event ?WHO)       
			))").

% Notes on query test 12:
% 1. There is a problem with the orignal formulation using "checks" and "check" instead of "meet". "Meet"
% replaces "check" in the test sentence. Otherwise, "checks" appears to be used here differently than before.
% It appears to be used as in 'checks out', or 'checks a form', or 'authorizes the customer', as opposed to checking
% a personal code.
% 2. Also changed 'employee' to 'manager' as I could find a definition for that but not employee.
% 3. Finally, changed "the manager" to "the manager George" and "the customer" to "the customer Fred" to avoid
% anaphoric reference problems.



test_query(v2, 13.0,"John gives a card to the customer Fred.", "What does John give the customer Fred?","A card.",
          "a WH query with WHAT, query direct object",
          "(exists (?card ?giving ?service)
                   (and (instance ?card BankCard)
                        (customer Fred ?service)
                        (instance ?giving Giving)        % Giving is in the SUO
                        (agent ?giving John)             % who does the giving
                        (patient ?giving ?card)          % what is given
			(destination ?giving Fred)       % destination is a case role in SUO covering 'recepient'
			))",
          "(exists (?giving)
                   (and (customer Fred ?service)
                        (instance ?giving Giving)        % Giving is in the SUO
                        (agent ?giving John)             % who does the giving
                        (patient ?giving ?WHAT)          % WHAT is given?
			(destination ?giving Fred)       % destination is a case role in SUO covering 'recepient'
                        ))").

% Notes on query test 13:
% 1. Changed "the customer" to "the customer Fred" to avoid anaphoric reference problems.


% QUERY NOUN MODIFIER

test_query(v3, 14.0,"The green card is not valid.", "Which card is not valid?", "The green card.",
	"a WH query with WHICH, query noun modifier",
         "(exists (?card)                                % The green card is not valid.
                  (and (instance ?card BankCard)
                       (attribute ?card Green)
                       (not (instance ?card ValidCard))))",
         "(and (instance ?card BankCard)                  % Which card is not valid?
	       (attribute ?card ?WHICH)                   % WHICH attribute identifies it?
	       (not (instance ?card ValidCard)))").



test_query(v2, 15.0,"A Swiss customer inserts a VisaCard.", "Which customer inserts a VisaCard?", "A Swiss customer.",
	"a WH query with WHICH, query noun modifier",
        "(exists (?customer ?card ?event ?service)        % A Swiss customer inserts a VisaCard.
                 (and (customer ?customer ?service)
                      (member ?customer Swiss)
                      (instance ?card VisaCard)
                      (instance ?event Putting)
                      (agent ?event ?customer)
                      (instrument ?event ?card)))",
         "(exists (?customer ?card ?event ?service)      % Which customer inserts a VisaCard?
                 (and (customer ?customer ?service)
                      (attribute ?customer ?WHICH)       % WHICH customer, i.e., what identifying attribute?
                      (instance ?card VisaCard)
                      (instance ?event Putting)
                      (agent ?event ?customer)
                      (instrument ?event ?card)))").

% Notes on query test 15:
% 1. Changed 'enter' to 'insert' in test sentence for reasons same as in query 5.
% 2. Note that will have to check for other ways of expressing identifying characteristics, too, such as
% being a member of a collection (in this case, Swiss).

test_query(v3, 16.0,"John enters a card that is not valid.", "Which card does John enter?", "A card that is not valid.",
	"a WH query with WHICH, query noun modifier",
         "(exists (?card ?event)                      % John enters a card that is not valid.
                  (and (instance ?card BankCard)
                       (instance ?event Entering)
                       (not (instance ?card ValidCard))
                       (agent ?event John)
                       (instrument ?event ?card)))",
         "(exists (?card ?event)                      % Which card does John enter?
                  (and (instance ?card BankCard)
                       (instance ?event Entering)
                       (attribute ?card ?WHICH)          % WHICH, what are the distinguishing attributes?
                       (agent ?event John)
                       (instrument ?event ?card)))").

% Notes on query test 16:
% 1. Translated 'John' to a constant. We could use ?agent instead and add (name ?agent 'John').

test_query(v2, 17.0,"The customer Fred presses the key A.", "Which key does the customer Fred press?", "The key A.",
	"a WH query with WHICH, query noun modifier",
        "(exists (?service ?event)              % The customer presses key 'A'.
                 (and (customer Fred ?service)
                      (instance ?event Poking)
                      (instance KeyA Key)
                      (agent ?event Fred)
		      (patient ?event KeyA)
		 ))",
        "(exists (?service ?event)              % Which key does the customer press?
                 (and (customer Fred ?service)
                      (instance ?event Poking)
                      (instance ?WHICH Key)
                      (agent ?event Fred)
                      (patient ?event ?WHICH)))").

% Notes on query test 17
% 1. Translated 'presses' to 'pokes'. Could also map to 'touches'.
% 2. Changed 'the customer' to 'the customer Fred'.



% QUERY PREPOSITIONAL OBJECT AND INDIRECT OBJECT

test_query(v2, 18.0,"John gives a card to the customer Fred.", "Who does John give a card to?", "To the customer Fred.",
	"a WH query with WHO, query prepositional object",
        "(exists (?card ?service ?event)                       % John gives a card to the customer Fred.
                 (and (instance ?card BankCard)
		      (customer Fred ?service)
		      (instance ?event Giving)
		      (agent ?event John)
		      (patient ?event ?card)
		      (destination ?event Fred))",
        "(exists (?card ?event)                                % Who does John give the card to?
                 (and (instance ?card BankCard)
		      (instance ?event Giving)
		      (agent ?event John)
		      (patient ?event ?card)
		      (destination ?event ?WHO))").

% Notes on query test 18
% 1. Changed 'the customer' to 'the customer Fred' for the usual reasons, to avoid anaphoric references and
% the need for multi-sentence input of the assertion and the query together.		 

test_query(v2, 19.0,"John gives a card to the customer Fred.", "To whom does John give a card?", "To the customer Fred.",
	"a WH query with WHO, query prepositional object",
        "(exists (?card ?service ?event)                       % John gives a card to the customer Fred.
                 (and (instance ?card BankCard)
		      (customer Fred ?service)
		      (instance ?event Giving)
		      (agent ?event John)
		      (patient ?event ?card)
		      (destination ?event Fred))",
        "(exists (?card ?event)                                % To whom does John give the card to?
                 (and (instance ?card BankCard)
		      (instance ?event Giving)
		      (agent ?event John)
		      (patient ?event ?card)
		      (destination ?event ?WHOM))").

% Notes on query test 19.
% 1. Essentially the same KIF as query test 18 as they are just syntactic variants.


% QUERY COMPLEMENTS OF PREPOSITIONS

% What does John work with?
test_query(v2, 20.0,"John works with a blue personal computer.", "What does John work with?", "With a blue personal computer.",
	"a WH query with WHAT, query complements of prepositions",
        "(exists (?computer)
                 (and (uses John ?computer)
		      (instance ?computer PersonalComputer)
		      (attribute ?computer Blue)))",
        "(uses John ?WHAT)").

% Notes on query test 20.
% 1. I am using 'uses' as the meaning of 'works' here as 'uses' is in the SUO but not 'works'.
% 2. Defined PersonalComputer as a Machine.
% 3. Added 'personal' in the test sentence to specify that this is a PC.
% 4. Replaced 'old' in test sentence with 'blue' as 'old' is harder to define.		 

test_query(v2, 21.0,"John works with an old computer.", "With what does John work?", "With an old computer.",
	"a WH query with WHAT, query complements of prepositions",
        "(exists (?computer)
                 (and (uses John ?computer)
		      (instance ?computer PersonalComputer)
		      (attribute ?computer Blue)))",
        "(uses John ?WHAT)").

% Notes on query test 21
% 1. Same KIF as query test 20 as these are just syntactic variants.

test_query(v2, 22.0,"John talks with a customer", "Who does John talk with?", "With a customer.",
	"a WH query with WHO, query complements of prepositions",
        "(exists (?event ?customer ?service)           % John talks with a customer.
                 (and (customer ?customer ?service)
                      (instance ?event Talking)
                      (agent ?event John)
                      (patient ?event ?customer)))",
        "(exists (?event)                              % Who does John talk with?
                 (and (instance ?event Talking)
                      (agent ?event John)
                      (patient ?event ?WHO)))").


test_query(v2, 23.0,"John talks with a customer", "With whom does John talk?", "With a customer.",
	"a WH query with WHO, query complements of prepositions",
        "(exists (?event ?customer ?service)           % John talks with a customer.
                 (and (customer ?customer ?service)
                      (instance ?event Talking)
                      (agent ?event John)
                      (patient ?event ?customer)))",
        "(exists (?event)                              % Who does John talk with?
                 (and (instance ?event Talking)
                      (agent ?event John)
                      (patient ?event ?WHO)))").

% Notes on query test 23
% 1. Same KIF as query test 22 as they are just syntactic variants.
      

% QUERY SUBJECT AND COMPLEMENTS

test_query(v2, 24.0,"John inserts a card.", "Who inserts what?", "John inserts a card.",
	  "a WH query with WHO, query subject and complements",
          "(exists (?event ?card)                  % John inserts a card.
                   (and (instance ?card BankCard)
                        (instance ?event Putting)
                        (agent ?event John)
                        (instrument ?event ?card)))", 
          "(exists (?event ?card)                  % Who enters what?
                   (and (instance ?event Putting)
                        (agent ?event ?WHO)
                        (instrument ?event ?WHAT)))").
     
% Notes on query test 24
% 1. Changed 'enter' to 'insert' in test sentence for reasons same as in query 5.

      

test_query(v2, 25.0,"A customer inserts a VisaCard.", "Who inserts what?", "A customer inserts a VisaCard.",
	  "a WH query with WHO, query subject and complements",
          "(exists (?customer ?service ?event ?card)   % A customer inserts a card.
                   (and (customer ?customer ?service)
                        (instance ?card BankCard)
                        (instance ?event Putting)
                        (agent ?event ?customer)
                        (instrument ?event ?card)))",
          "(exists (?event ?card)                      % Who enters what?
                   (and (instance ?event Putting)
                        (agent ?event ?WHO)
                        (instrument ?event ?WHAT)))").
      
% Notes on query test 25
% 1. Changed 'enter' to 'insert' in test sentence for reasons same as in query 5.


test_query(v2, 26.0,"John gives a book to Mary.", "Who gives what to whom?", "John gives a book to Mary.",
	  "a WH query with WHO and WHAT, query subject and complements",
          "(exists (?event ?book)                        % John gives a book to Mary
                   (and 
                       (instance ?book Book)                   
                       (instance ?event Giving)
                       (agent ?event John)
                       (patient ?event ?book)
                       (destination ?event Mary)))",
          "(exists (?event ?book)                        % Who gives what to whom?
                   (and 
                       (instance ?event Giving)
                       (agent ?event ?WHO)
                       (patient ?event ?WHAT)
                       (destination ?event ?WHOM)))").

% QUERY OF-NOUN PHRASE

test_query(v2, 27.0,"The blue card of John is valid.", "Whose card is valid?", "The blue card of John.",
	  "a WH query with WHOSE, query of-noun phrase",
          "(exists (?card)                          % The blue card of John is valid.
                   (instance ?card BankCard)
                   (attribute ?card Blue)
                   (possesses John ?card)
                   (instance ?card ValidCard))",
          "(exists (?card)                          % WHOSE card is valid?
                   (instance ?card BankCard)
                   (possesses ?WHOSE ?card)
                   (instance ?card ValidCard))").

test_query(v2, 28.0,"The ATM of the Swiss bank is green.", "The ATM of what is green?", "The ATM of the Swiss bank.",
	  "a WH query with WHOSE, query of-noun phrase",
          "(exists (?atm ?bank)
                   (and (possesses ?bank ?atm)
		        (member ?bank Swiss)
		        (attribute ?atm Green)))",
          "(exists (?atm)
                   (and (possesses ?WHAT ?atm)
		        (attribute ?atm Green)))").
          
% Notes on query test 28
% 1. Changed 'The code of the green card is valid' to 'The ATM of the Swiss bank is green' since we use
%    code to refer to an account number and valid to refer to valid cards.



% QUERY COMPLEMENTS OF COPULA

test_query(v3, 29.0,"The card is not valid.","What is the card?","Not valid.","query complement of copula",
           "(exists (?card)                                     % The card is not valid.
                    (and (instance ?card BankCard)
		         (not (instance ?card ValidCard))))",
           "(exists (?card)                                     % The card is not valid.
                    (and (instance ?card BankCard)
		         (instance ?card ?WHAT))))").           % What is the card?
       

test_query(v2, 30.0,"John is a customer.","What is John?","A customer.","query complement of copula",
           "(exists (?service)                    % John is a customer
                    (customer John ?service)",
           "(instance John ?WHAT)").              % What is John?

% Notes on query test 30
% 

test_query(v2,
      31.0,"John is the manager of the bank.","What is John?","John is a manager. He works for a bank.","query complement of copula",
           "(exists (?bank)
                    (and (instance ?bank Bank)               % John is a manager of the bank.
                         (instance John Manager)
                         (employs ?bank John)))",
           "(instance John ?WHAT)").                         % What is John?

% Notes on query test 31
% 1. Changed response from "The manager of the bank." to "John is a manager. He works for a bank."
% as this would be what could be generated easily from first pulling the type information about
% John (manager) and then adding in additional predicate information (the bank employs him).

% QUERY VERB PHRASE MODIFIERS

% LOCATION

test_query(v2, 32.0,"John works underground.", "Where does John work?", "John works underground.",
	"query verb phrase modifiers, location",
        "(exists (?POS ?ORG ?LOC)
                 (and
		      (occupiesPosition John ?POS ?ORG)
                      (instance ?LOC Underground)
                      (or
                         (located ?ORG ?LOC)
                         (exists (?SUB)
                                 (and
                                     (subOrganizations ?SUB ?ORG)
		                     (member John ?SUB)
				     (located ?SUB ?LOC))))))",
        "(exists (?POS ?ORG ?LOC)
                 (and
		      (occupiesPosition John ?POS ?ORG)
                      (instance ?LOC Underground)
                      (or
                         (located ?ORG ?LOC)
                         (exists (?SUB)
                                 (and
                                     (subOrganizations ?SUB ?ORG)
		                     (member John ?SUB)
				     (located ?SUB ?WHERE))))))").


% Notes on query test 32
% 1. Interpreted "works" here as meaning "working for some company" as opposed to doing some
%    other kind of work, e.g., for a hobby, or for his wife.
% 2. We define Underground as a subclass of geographic area.		 
% 3. My first translation:
%    (exists (?org)                             
%             (and (employs ?org John)
%                   (located John Underground)))
% implies that John is continuously underground.

test_query(v2, 33.0,"The card is in the slot.", "Where is the card?", "The card is in the slot.",
	"query verb phrase modifiers, location",
        "(exists (?card ?slot)                      % The card is in the slot.
                 (and (instance ?card BankCard)
		      (instance ?slot ATMSlot)
		      (located ?card ?slot)))",
        "(and (instance ?card BankCard)             % Where is the card?
              (located ?card ?WHERE))").

% ORIGIN

test_query(v2, 34.0,"The train arrives from London.", "Where does the train arrive from?", "From London.",
	"query verb phrase modifiers, origin",
        "(exists (?train ?event ?dest)                    % The train arrives from London.
                 (and (instance ?train Train)
                      (instance ?event Transportation)
                      (located ?train ?dest)
                      (patient ?event ?train)
                      (destination ?event ?dest)
                      (origin ?event London)))",
        "(exists (?train ?event ?dest)                    % Where does the train arrive from?
                 (and (instance ?train Train)
                      (instance ?event Transportation)
                      (located ?train ?dest)
                      (patient ?event ?train)
                      (destination ?event ?dest)
                      (origin ?event ?WHERE)))").

% Notes on query test 34
% 1. Define London as an instance of geographic area.
% 2. Define Train as a TransportationDevice.


test_query(v2, 35.0,"The train arrives from London.", "From where does the train arrive?", "From London.",
	"query verb phrase modifiers, origin",
        "(exists (?train ?event ?dest)                    % The train arrives from London.
                 (and (instance ?train Train)
                      (instance ?event Transportation)
                      (located ?train ?dest)
                      (patient ?event ?train)
                      (destination ?event ?dest)
                      (origin ?event London)))",
        "(exists (?train ?event ?dest)                    % Where does the train arrive from?
                 (and (instance ?train Train)
                      (instance ?event Transportation)
                      (located ?train ?dest)
                      (patient ?event ?train)
                      (destination ?event ?dest)
                      (origin ?event ?WHERE)))").

% Notes on query test 35
% 1. Same KIF as query test 34, just a syntactic variant.

% DIRECTION

test_query(v2, 36.0,"John inserts the card into the slot.","Where does John insert the card into?","Into the slot.",
	"query verb phrase modifiers, direction",
        "(exists (?card ?slot ?event)                      % John inserts the card into the slot.
                 (and (instance ?card BankCard)
		      (instance ?slot ATMSlot)
		      (instance ?event Putting)
		      (agent ?event John)
		      (patient ?event ?card)
		      (destination ?event ?slot)))",
         "(exists (?card ?event)                           % Where does John insert the card into?
		  (and (instance ?card BankCard)
		       (instance ?event Putting)
		       (agent ?event John)		       
		       (patient ?event ?card)
                       (destination ?event ?WHERE)))").

test_query(v2, 37.0,"The machine moves the card X to the slot Y.", "Where does the machine move the card X to?", "To the slot Y.",
	"query verb phrase modifiers, direction",
        "(exists (?machine ?event)                       % The machine moves forward.
                 (and (instance ?machine Machine)
		      (instance ?event Motion)
		      (agent ?event ?machine)
                      (patient ?event CardX)
		      (destination ?event SlotY)))",
        "(exists (?machine ?event)                       % Where does the machine move to?
                 (and (instance ?machine Machine)
		      (instance ?event Motion)
		      (agent ?event ?machine)
                      (patient ?event CardX)
		      (destination ?event ?WHERE)))").       
		  
% Notes on query test 37

% 1. Original text was "The machine moves forward" but "forward" depends on the context
% and observer.


% TIME

test_query(v2, 38.0,"The train arrives now.", "When does the train arrive?", "Now.",
	"query verb phrase modifiers, time",
        "(exists (?train ?event)                    % The train arrives now.
                 (and (instance ?train Train)
                      (instance ?event Transportation)
                      (instrument ?event ?train)
		      (equal Now (EndFn (WhenFn ?event)))))",
        "(exists (?train ?event)                    % When does the train arrive?
                 (and (instance ?train Train)
                      (instance ?event Transportation)
                      (instrument ?event ?train)
		      (equal ?WHEN (EndFn (WhenFn ?event)))))").

% Notes on query test 38
% 1. Now is defined as a TimePoint in the auxiliary KIF for these ACE tests. That does not
% capture the context-dependent sense of Now, but will do.
% 2. The train is the instrument, not the agent, since the agent must be autonomous.

test_query(v2, 39.0,"The train arrives in the morning.", "When does the train arrive?", "In the morning.",
	"query verb phrase modifiers, time",
      "(exists (?train ?event)                  % The train arrives in the morning.
               (instance ?train Train)
               (instance ?event Motion)
               (instrument ?event ?train)
               (time ?event Morning))",
      "(exists (?train ?event)                  % When does the train arrive?
               (instance ?train Train)
               (instance ?event Motion)
               (instrument ?event ?train)
               (time ?event ?WHEN))").
       

% Notes on query test 39
% 1. The train is the instrument, not the agent, since the agent must be autonomous.


% START

test_query(v2, 40.0,"The employee works from morning until afternoon.","Since when does the employee work?","From morning.",
	"query verb phrase modifiers, start",
      "(exists (?employee ?state)
               (and (instance ?state OccupationalProcess)
	            (agent ?state ?employee)
                    (temporalPart (BeginFn (WhenFn ?state)) ?morning)  % starts sometime in the morning
	            (instance ?morning Morning)
	            (temporalPart (EndFn (WhenFn ?state)) ?afternoon)  % ends sometime in the afternoon
	            (instance ?afternoon Afternoon)))",
      "(exists (?employee ?state)
               (and (instance ?state OccupationalProcess)
	            (agent ?state ?employee)
                    (equal ?WHEN (BeginFn (WhenFn ?state)))))").

% Notes on query test 40
% 1. Works is a process or state that persists between two time points. The first, start point,
% is in the morning. The second, end point, is in the afternoon.
% 2. The question asks when the beginning of the work state is.       

% END

test_query(v2, 41.0,"The shop is open until midnight.","Until when is the shop open?","Until midnight.",
	"query verb phrase modifiers, end",
        "(exists (?shop)
                 (and (instance ?shop Corporation)
		      (<=> (holdsDuring ?time (attribute ?shop OpenService))
                           (before (EndFn ?time) Midnight))))",
        "(and (instance ?shop Corporation)
              (exists (?time1 ?time2)
	              (and (holdsDuring ?time (attribute ?shop OpenService))
                           (before (EndFn ?time) ?UNTIL_WHEN))))").

% Notes on query test 41
% 1. Using Corporation for 'Shop'
% 2. The financial ontology term for open for business is OpenService
% 3. In English, the statement is if the shop is open then midnight
% is the end of the time interval.
% 4. In English, the query is to find times when the shop closes (e.g.,
% 5 P.M., Midnight, etc.


% DURATION

test_query(v2, 42.0,"The machine works permanently.","How long does the machine work?","Permanently.",
	  "query verb phrase modifiers, duration",
          "(forall (?time)
                   (=> (instance ?time TimePosition)
                       (holdsDuring ?time
		           (exists (?state ?machine)
		                   (and (instance ?machine Machine)
			                (instance ?state Process)
			                (instrument ?state ?machine))))))",
          "(exists (?machine ?state ?time1 ?time2)
                   (and (holdsDuring ?time1 ?state)
	                (not (holdsDuring ?time2 ?state))
                        (instance ?machine Machine)
                        (instance ?state Process)
		        (instrument ?state ?machine)
			(before ?time1 ?time2)
		        (equal ?HowLong (TimeInterval (StartFn ?time1) (EndFn ?time2)))))").

% Notes on query test 42
% 1. Interpreted as the machine is working all the time.
% 2. The question is asked as when does the machine start and when does the machine stop
% working, and how long is that interval? Unfortunately, we will probably need some lemmas
% to show that a machine working all the time is working 24 hours a day to come up with
% the answer "24 hours" or "all the time".

test_query(v2, 43.0,"The train stops for one hour.","How long does the train stop?", "For one hour.",
	  "query verb phrase modifiers, duration",
          "(exists (?train ?stop ?pos)
                   (and
                      (instance ?train Train)
                      (holdsDuring ?pos (not (exists ?motion)))
		      (instance ?motion Motion)
		      (patient ?motion ?train)
                      (instance ?pos TimeInterval)
                      (duration ?pos (MeasureFn 1 HourDuration))))",
          "(exists (?train ?stop ?pos)
                   (and
                      (instance ?train Train)
                      (holdsDuring ?pos (not (exists ?motion)))
		      (instance ?motion Motion)
		      (patient ?motion ?train)
                      (instance ?pos TimeInterval)
                      (duration ?pos ?HowLong)))").

% Notes on query test 43:
% 1. Do we also have to say the train is in motion when not stopped?

% FREQUENCY

test_query(v2, 44.0,"The milkman comes daily.","How often does the milkman come?","Daily.",
	  "query verb phrase modifiers, frequency",
          "(exists (?milkman ?delivery)
                   (and (instance ?milkman Milkman)
                        (frequency ?delivery (MeasureFn 1 DayDuration))
                        (equal ?delivery
			     [(KappaFn ?process) (and (instance ?process Putting)
                                                      (instance ?milk Milk)
						      (patient ?process ?milk)]))))",
			
          "(exists (?milkman ?delivery)
                   (and (instance ?milkman Milkman)
		        (equal ?delivery
			     [(KappaFn ?process) (and (instance ?process Putting)
                                                      (instance ?milk Milk)
						      (patient ?process ?milk)]))
                        (frequency ?delivery ?HowOften)))").

% Notes on query test 44:
% 1. The KappaFn is like a Lambda in LISP in the sense that it defines a
% a class of processes characterized as in the expression.       

% INSTRUMENT

test_query(v2, 45.0,"John opens the door with a key.", "With what does John open the door?", "With a key.",
	  "query verb phrase modifiers, instrument",
          "(exists (?event ?door ?key)
                   (and (instance ?event Opening)
                        (instance ?door Door)
		        (instance ?key Key)
		        (agent ?event John)
		        (instrument ?event ?key)
		        (patient ?event ?door)))",
          "(exists (?event ?door ?key)
                   (and (instance ?event Opening)
                        (instance ?door Door)
		        (instance ?key Key)
		        (agent ?event John)
		        (instrument ?event ?What)
		        (patient ?event ?door)))").       

% Notes on query test 45:
% 1. 'Door' and 'Key' are defined as Device in the accompanying KIF
% for these questions.       

% MANNER

test_query(v2, 46.0,"John inserts the card correctly.", "How does John insert the card?", "Correctly.",
	  "query verb phrase modifiers, manner",
          "(exists (?event ?card)
                   (and (instance ?card BankCard)
		        (instance ?event Putting)
		        (agent ?event John)
		        (patient ?event ?card)
		        (manner ?event Correctly)))",
          "(exists (?event ?card)
                   (and (instance ?card BankCard)
		        (instance ?event Putting)
		        (agent ?event John)
		        (patient ?event ?card)
		        (manner ?event ?HOW)))").       

% Notes on query test 46:
% 1. 'Correctly' is defined as a SubjectiveAssessmentProperty in the KIF for these tests.


% COMITATIVE

test_query(v2, 47.0,"The employee enters the room with a customer.", "With whom does the employee enter the room?",
	  "With a customer.",
	  "query verb phrase modifiers, comitative",
          "(exists (?employee ?organization ?event ?room ?customer ?service)
                   (and (employs ?organization ?employee)
                        (customer ?customer ?service)
		        (instance ?event Motion)
		        (instance ?room Room)
                        (destination ?event ?room)
		        (agent ?event ?employee)
		        (agent ?event ?customer)))",
          "(exists (?employee ?organization ?event ?room ?customer ?service)
                   (and (employs ?employee ?organization)
		        (customer ?customer ?service)
		        (instance ?event Motion)
		        (instance ?room Room)
		        (agent ?event ?employee)
		        (agent ?event ?WHOM)").		   

% Notes on query test 47:
% 1. Surprisingly, Entering or Enters is not in the SUMO.
% 2. Translated into motion into a room with two agents: the employee and the customer.
% 3. Motion could be broken down into leaving, transit, and entering so this translation
% is an example where the KIF translation loses some specificity in the translation process.
% Other examples add specificity when normative assumptions are made. E.g., if 'see' in
% 'The manager sees the customer.' was translated to 'meet' that would be using a normative
% assumption that the 'meets' sense of 'see' is the one to use in this domain rather than the
% 'perceive' sense. Either could apply: 'The manager sees the customer in the conference room.'
% and 'The manager sees the customer through the glass window.' illustrate the two alternative
% senses.			

%%%%%%%%%%%%%%%
% PARAPHRASES %
%%%%%%%%%%%%%%%

% ANAPHORS

test_paraphrase(v4, 1.0,"The machine checks the card's personal code. It is correct for the card.",
	       "The machine checks the card's personal code. [The personal code] is correct for the card.",
               "a personal pronoun always refers to the most recent accessible noun phrase w same number and gender",
               "(exists (?machine ?pin ?card)
	                (and (instance ?machine Machine)
			     (pin ?pin ?card)
			     (instance ?card BankCard)
			     (correctFor ?pin ?card)))").
	   

% Notes on paraphrase 1.0:
% 1. The original sentence was "The machine checks the personal code. It is valid.",
% which was changed to "The machine checks the card's personal code. It is correct for the card."
% 2. Note that at times "valid" is used with the card code (e.g., parse 2, 3, 11, 19, 20, 25, 30; queries
% 2, 3, 4, 6, 10, 14, 16, 27 and 29) and at times with the personal code (e.g., paraphrase 1) so we need
% two kinds of "valid", one check for a valid bank card (i.e., not
% mutilated or expired) and one check for the matching PIN (i.e., the right personal code for this
% card).
			
% Add correctFor to the financial ontology??

test_paraphrase(v4, 2.0,"A customer XYZ inserts a blue card B. B is valid.",
	       "A customer XYZ inserts a blue card B. [The blue card B] is valid.",
               "a dynamic name used alone refers to the most recent accessible noun phrase in which it occurs",
	       "(exists (?service ?event)
	                (and (customer CustomerXYZ ?service)
			     (instance CardB BankCard)
			     (attribute CardB Blue)
			     (instance CardB ValidCard)
			     (instance ?event Putting)
			     (agent ?event CustomerXYZ)
			     (patient ?event CardB)))").


% Notes on paraphrase 2.0
% 1. The original sentence was "A customer XYZ inserts a blue card B. B is valid.",
% which was changed to "A customer XYZ inserts a blue card B. B is valid.". It is correct for the card."
% 2. Note that at times "enter" is used sometimes with a card  (e.g., 
% ) and sometimes with the personal code (e.g., paraphrase 2) so we need
% two kinds of "enter", one for a bank card and one for a PIN. For the first we use "insert" and
% the second we use "EnteringAPIN".			     

% DISTRIBUTION PRINCIPLE

test_paraphrase(v4, 3.0,"The card is red and green.",
	       "The card is red and [ is ] green.",
               "verbs are distributed over coordinators",
	       "(exists (?card)
	                (and (instance ?card BankCard)
			     (attribute ?card Red)
			     (attribute ?card Green)))").

% Notes on paraphrase 3.0
% 1. Interpreted as meaning the card has both red and green colors.


test_paraphrase(v4, 4.0,"The customer does not enter a card and a code.",
               "The customer does not enter a card and [ does not enter ] a code.",
               "negated verbs are distributed over coordinators",
	       "(exists (?customer ?service)
	                (and (customer ?customer ?service)
			     (not (exists (?event ?card)
				          (and (instance ?card BankCard)
					       (instance ?event Putting)
					       (agent ?event ?customer)
					       (patient ?event ?card))))
			     (not (exists (?event ?code)
				          (and (instance ?code BankCode)
					       (instance ?event Putting)
					       (agent ?event ?customer)
					       )))))").

% Notes on paraphrase 4.0
% 1. This is problematic since 'enter' must apply to both a card and a code, but
% it should mean 'insert' for the card and 'type' for the code, in which case one
% verb maps to two meanings or ACE has to disambiguate the senses of enter. Perhaps
% it does this according to the kind of argument??
% 2. Note that De Morgan's laws are violated by ACE's Principle of Distribution in these
% cases: Rather than translate 'The customer does not enter a card and a code.' to
% 'The customer does not enter a card or does not enter a code.' (as would be logically
% correct applying De Morgan's laws) ACE translates this sentence to 'The customer does
% not enter a card and does not enter a code (Principle of Distribution). Another example
% of how ACE and English can differ.


% MINIMAL ATTACHMENT OF PREPOSITIONAL PHRASES

test_paraphrase(v4, 5.0,"The customer enters a card with a personal code.",
               "The customer { enters a card with a personal code } .",
               "all prep phrases used as modifiers, except of-phrases, relate to the closest preceding verb phrase",
	       "(exists (?customer ?service ?card ?pin ?event)
	                (and (customer ?customer ?service)
			     (instance ?card BankCard)
			     (pin ?pin ?card)
			     (instance ?event Putting)
			     (agent ?event ?customer)
			     (patient ?event ?card)))").

% Notes on paraphrase 5.0
% Changed "with a code" to "with a personal code" to specify the code is a PIN and not a bank account code.


test_paraphrase(v4, 6.0,"The customer enters a card and a code in the morning.",
               "The customer enters a card and { [enters] a code in the morning }.",
               "the principle of minimal attachment applies after the distribution principle").

% Notes on paraphrase 6.0, same problems as paraphrase 4.0:
% ?? This is problematic since 'enter' must apply to both a card and a code, but
% it should mean 'insert' for the card and 'type' for the code, in which case one
% verb maps to two meanings or ACE has to disambiguate the senses of enter. Perhaps
% it does this according to the kind of argument??

% RIGHT ASSOCIATION

test_paraphrase(v4, 7.0,"The customer enters the code of a card that is invalid and that is expired.",
               "The customer enters the code of { a card that is invalid and that is expired } .",
               "a rel pro not preceded by a coordinator relates to the rightmost noun immed preceding the rel pro").

% Do we need to add "expired" to the financial ontology??

% VERB PHRASE COORDINATION AND RELATIVE SENTENCES

test_paraphrase(v4, 8.0,"The customer enters a card that is valid and has a code.",
               "The customer { enters a card that is valid } and { has a code }.",
               "a coord verb phrase belongs to the main clause, not to the relative sentence",
	       "(exists (?customer ?service ?card ?event)
	                (agent ?event ?customer)
	                (patient ?event ?card)	  
	                (instance ?card BankCard)
	                (cardCode ?code ?card)
	                (instance ?card ValidCard))").

% Notes on paraphrase 8.0
% 1. Changed "code" to "card code" to specify that code here refers to the account
% number and not the PIN.	   

test_paraphrase(v4, 9.0,"The customer enters a card that is valid and that has a code.",
               "The customer enters { a card that is valid and that has a code }.",
               "the repeated rel pronouns refer to the same object as the initial rel pronoun.",
	       "(exists (?customer ?service ?card ?event)
	                (agent ?event ?customer)
	                (patient ?event ?card)	  
	                (instance ?card BankCard)
	                (cardCode ?code ?card)
	                (instance ?card ValidCard))").	   


% QUANTIFIER SCOPING, BINDING HIERARCHY, AND INTERACTION OF PRINCIPLES

% Quantifier Scoping

test_interpret(v3, 1.0,"A device controls every pump.",                 % this sentence
                   "There is a device that controls every pump.",   % is interpreted the same as this one
		   "surface order determines quantifier scope, use 'for every' and 'there is a' to move quantifiers",
                   "For every pump there is a device that controls the pump.", % NOT this one!
                   "(exists (?device ?process)
	                    (instance ?device Device)
	                    (forall (?pump)
			            (instance ?pump Pump)
			            (instance ?process Process)
			            (instrument ?process ?device)
			            (patient ?process ?pump)))").

% Notes on interpretation 1.0
% 1. Cannot use 'Guiding' for "controls" as the 'device' is not an autonomous agent.


test_interpret(v3, 2.0,"John assigns a personal code to every customer.",                 % this sentence
                   "There is a personal code that John assigns to every customer.",   % is interpreted like this
		   "the normal English interpretation differs from ACE's: in ACE every customer gets the same code",
                   "For every customer there is a personal code that John assigns to the customer.", % NOT this one!
                   "(exists (?code ?card)
	                    (and (cardCode ?code ?card)
			         (forall (?customer ?service)
				         (and (customer ?customer ?service)
                                              (exists (?event)
                                                      (and 
					                  (instance ?event Giving)
					                  (agent ?event John)
                                                          (patient ?event ?code)
					                  (destination ?event ?customer)))))))").

% Notes on interpretation 2.0,
% 1. Interpreted "assigns" as an event of Giving.

test_interpret(v3, 3.0,"SimpleMat checks the code of every customer.",                    % this sentence
                   "There is a code that SimpleMat checks for every customer.",       % is interpreted like this
		   "the normal English interpretation differs from ACE's: in ACE every customer gets the same code",
                   "For every customer SimpleMat checks the code of the customer.", % NOT this one!
                   "(exists (?code ?card)
	                    (and (cardCode ?code ?card)
			         (forall (?customer ?service)
				         (and (customer ?customer ?service)
                                              (possesses ?customer ?code)
                                              (exists (?event)
                                                      (and 
					                  (instance ?event VerifyingCardCode)
					                  (agent ?event SimpleMat)
                                                          (patient ?event ?code)))))))").

% Notes on interpretation 3.0,
% 1. Interpreted "code" as BankCode and not personal code.

test_interpret(v3, 4.0,"A program calculates every personal code.",                    % this sentence
                   "There is a program that calculates every personal code.",      % is interpreted like this
		   "again the scope in normal English is ambiguous so you may need to reformulate as below",
                   "For every personal code there is a program that calculates the personal code.", % NOT this one!
                   "(exists (?program)
	                    (and (instance ?program ComputerProgram)
			         (forall (?pin ?card)
				         (and (pin ?pin ?card)
                                              (instance ?card BankCard)
					      (cardCode ?code ?card)
					      (exists (?event)
					              (and (instance ?event Encoding)
						           (agent ?event ?program)
						           (instrument ?event ?code)
						           (result ?event ?pin)))))))").

% Notes on interpretation 4.0,
% 1. Interpreted "calculates" as an event of encoding.
% 2. The input of the encoding is the bank code (case role instrument as it is unchanged) and
% the output (case role result) is the PIN.


test_interpret(v3, 5.0,"Every customer who has a VisaCard uses a teller machine.",     % this sentence
                   "For every customer who has a VisaCard there is a teller machine that the customer uses.",
		   "again the scope in normal English is ambiguous so you may need to reformulate as below",
                   "There is a teller machine that every customer who has a VisaCard uses.", % NOT this one!
	           "(forall (?customer ?service ?card)
	                    (and (customer ?customer ?service)
			         (implies (and (possesses ?customer ?card)
					       (instance ?card VisaCard))
				          (exists (?event ?ATM)
					          (and (instance ?ATM ATMMachine)
                                                       (instance ?event Putting)
						       (agent ?event ?customer)
						       (patient ?event ?card)
						       (destination ?ATM))))))").

% Notes on interpretation 5.0,
% 1. Interpreted "teller machine" as ATM.

% Binding Hierarchy

test_interpret(v4, 6.0,"The customer enters a VisaCard or a MasterCard and a personal code.", 
                   "The customer enters a VisaCard or the customer enters a Mastercard and a personal code.",
		   "as in logic AND binds more tightly than OR so add commas to get the meaning intended if needed",
                   "The customer enters a VisaCard or a MasterCard, and a personal code." % NOT this one! N.B. Use of comma!
	   ).


% Notes on interpretation 6.0, same problems as paraphrase 4.0:
% ?? This is problematic since 'enter' must apply to both a card and a code, but
% it should mean 'insert' for the card and 'type' for the code, in which case one
% verb maps to two meanings or ACE has to disambiguate the senses of enter. Perhaps
% it does this according to the kind of argument??
