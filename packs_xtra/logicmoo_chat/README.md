# logicmoo_chat

This is an early release Leveraging of the Self-dialogue Corpus containing 24,165 conversations, or 3,653,313 words, across 23 topics.

It contains a very simple proceedure writen in prolog that emulates a NN using a language model.

## PROBLEM   
Language Models can engage in freeform dialog and generate palatable word salad better than any other systems.  They do well holding a topic of conversation on its own, but has difficulty with letting users create the flow.  Language Models might be sophisticated enough to pick up context, sentiment, or emotion and use them effectively and not just leveraging the “Eliza Effect.”

Perhaps the problem is due to Language Models spending too much parameter space into acquiring syntactic correctness.  Most people are waiting for larger versions of Language Models to come out as they assume they likely runs out of parameter space before actual semantics are learned.  (Ignore for now OpenAI’s argument that many of the “subtleties of the human” have been embedding itself in our syntax). 
 
## IDEA  
The Logicmoo system removes all of the syntactic requirements from Language Models.  This allows faster bootstrap training times by feeding they an entirely Logic Form (LF) . Without these constraints Language Models will have the best chance at successfully showing its abilities.   Logicmoo ensures all parameter space is dedicated to semantics and logic. 

Our LF happens in two general forms:
* DRS (Discourse Representation Structures) These include questions, answers, offers, acceptances, declinations, requests, permissions and promises.
* LPS (Logic-based production system) to interact non-trivially with the world and have accomplishment specifications concerning what the dialog accomplishes in the world. 

These are translated into communication inputs and outputs in an I-O language called TknLF (Tokenized Logical Form).  The “subject matter” of TknLF is defined in terms of the order of sequence.   Sentences of TknLF of correctness can be generated automatically from Language Models.    
English from the user is converted to DRS/LPS Logical forms and then into TknLF understood by Language Models-J.   Language Models-J outputs the TknLF which is converted back into DRS/LPS which then becomes English.  

This document is intentionally terse sparing much details but gives at least a surface level easy to explain.


### Statistics


| Category | Count     |
---------- | -----------
| Topics   | 23        |
| Conversations | 24,165 |
| Words    | 3,653,313 |
| Turns    | 141,945   |
| Unique users | 2,717 |
| Conversations per user | ~9 |
| Unique tokens | 117,068 |

Topics include movies, music, sports, and subtopics within these.

