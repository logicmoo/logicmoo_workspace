# logicmoo_chat

This is an early release Leveraging of the Self-dialogue Corpus containing 24,165 conversations, or 3,653,313 words, across 23 topics.

It contains a very simple proceedure writen in prolog that emulates a language model.
(see https://gitlab.logicmoo.org/gitlab/logicmoo/logicmoo_workspace/-/blob/master/packs_xtra/logicmoo_chat/prolog/pllm/)

## PROBLEM   
GPT can engage in freeform dialog and generate palatable word salad better than any other systems.  They do well holding a topic of conversation on its own, but has difficulty with letting users create the flow.  Some people would belive that GPT might be sophisticated enough to pick up context, sentiment, or emotion and use them effectively though some speculate that GPT is merely leveraging the [“Eliza Effect”](https://en.wikipedia.org/wiki/ELIZA_effect#Overview).

Some people are waiting for larger versions of GPT to come out as they assume it is likely running out of parameter space in order to acquire syntactic correctness .. maybe before before any actual semantics are learned.
 
## IDEA
The Logicmoo system removes all of the syntactic requirements.  (Ignore for now OpenAI’s argument that GPT needed the syntax as many of the “subtleties of the human” have been embedding itself in our syntax).  We allow faster bootstrap training times by feeding an entirely [Logical Form](https://plato.stanford.edu/entries/logical-form/) (LF) .  Logicmoo ensures all parameter space is dedicated to deep logical form.

Our LF happens in two general forms:
* DRS [(Discourse Representation Structures)](https://plato.stanford.edu/entries/discourse-representation-theory/#DRSLanSynSemAcc) These include questions, answers, offers, acceptances, declinations, requests, permissions and promises.
* LPS [(Logic-based production system)](https://arxiv.org/pdf/1601.00529) to interact non-trivially with the world and have accomplishment specifications concerning what the dialog accomplishes in the world. 

These are translated into communication inputs and outputs in a language called TknLF a ([Tokenized Logical Form](https://logicmoo.org/xwiki/bin/view/Main/Psychology/Mentalese489)).  The “subject matter” of TknLF is defined in terms of the order of sequence.   Sentences of TknLF of correctness can be generated automatically from [PLLM (Prolog Language Models)](https://logicmoo.org:2082/gitlab/logicmoo/logicmoo_workspace/-/tree/master/packs_xtra/logicmoo_chat/prolog/pllm) (maybe even GPT).    

English from the user is converted to DRS/LPS Logical forms and then into TknLF understood by PLLM.   PLLM outputs the TknLF which is converted back into DRS/LPS which then becomes English.  

This document is intentionally terse sparing much details but gives at least a surface level easy to explain.


### Statistics on current Training set


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


