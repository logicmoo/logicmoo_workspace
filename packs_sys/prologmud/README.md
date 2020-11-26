# Installation using SWI-Prolog 7.1 or later:

``` prolog
?- pack_install(prologmud).
?- pack_install(prologmud_samples).
?- pack_install(logtalk).

%% to run
?- consult(library(prologmud_sample_games/run_mud_server)).
```

# A Prolog based MUD
Like "Hello world" that must be implemented in every language, a playable MUD server should also exist!  (MUD servers examples are: LambdaMOO, DikuMUD, PerlMUD, CircleMUD.. etc)   

However, for the Prolog language this is even more vital. Here in 2014, no one has created one?! 

After doing the Nani-World tutorial at the AMZI-Prolog website students are excited by how simple it was.  And for very little extra programming the program become better and better.  They might go on to imagining if it was not simply a tutorial but a real game.  Played with many people characters at once. That is what this project is about among other things.   See what is meant by this at 

* http://www.amzi.com/articles/prolog_fun.htm
* http://www.amzi.com/AdventureInProlog/a1start.php

# What is it really?

It turns out that being a MUD keeps a sane description of the minimized version of the code.   But really it is an application framework (much the way Tomcat is a web application hosting framework).
Some games (PrologMUD is no different) end up becoming miniature operating systems with 1000s of moving pieces and the type of problems/solutions being worked on in the layers of the game involve very different methodologies; Take a game like Doom II with Pathfinding, physics, image asset delivery to networked clients, mini-C interpreter for AI and rockets powered weapons, 3D rendering, Client networking to stay in sync, AAS (Area Awareness System) etc etc etc.   PrologMUD has all of these (except 2D rendering in place of 3D rendering) plus natural language understanding, theorem prover, behaviour planner, neural simulator, STRIPS, CYC, CG-KIF and a crap-load of other acronyms. 

10% code is Process management and client networking
10% is the Object Orientation smoothing layer (isa/genls hierarchy)



## What does playing it look like?
A text adventure game from the 1980s

## Why is it over 400,000 lines of code? 
First off, the author only had to write 100k lines to get it "bootstrapped" 
The rest is data and modules created over the last 30 years by research projects in the areas of artificial intelligence.   http://www.cyc.com http://www.larkc.eu  etc
Initial version was only 1000 lines of code.  

## Why is it in Prolog and not LISP or C++?
Prolog is required programming language at all 4 year universities (though most students only get 2 weeks to learn it! .. in which even then it is marginalized by the professor.. since really 2 weeks isn't long enough)
PrologMUD would been OK in LISP but propositional resolution is a type of calculation PrologMUD does most often and in LISP that ends up 10x slower when compared to a Prolog-in-C native unifier.


* DOCUMENTATION https://drive.google.com/#folders/0B0QA19UX0ehlV1ZEaXEzc3hjTWM
* **WEB TELNET** http://prologmoo.com/hmud/
* TELNET telnet://mud.prologmoo.com:4000
* WIKI https://github.com/TeamSPoon/PrologMUD/wiki
* YOUTUBE https://www.youtube.com/watch?v=XUNwYHBWJwQ
* SOURCE CODE https://github.com/TeamSPoon/PrologMUD
* SPARQL http://mud.prologmoo.com:3020/
* MUD HTTP http://mud.prologmoo.com:3020/mud/
* MESSAGE BOARD THREAD - http://www.intfiction.org/forum/viewtopic.php?f=38&t=13717&sid=e1fe6aa4ebb6fda6f2a502a3c26d495d
* HOMEPAGE http://www.prologmoo.com/
* LIVECODING http://livecoding.tv/logicmoo/

* Added Cutted Forward Chaining =!=> as a signal to stop processing rules on first success

## What to Expect during Installation

``` prolog
?- pack_install(prologmud).
% Contacting server at http://www.swi-prolog.org/pack/query ... ok
Install prologmud@1.1.117 from GIT at https://github.com/TeamSPoon/prologmud.git Y/n?

Create directory for packages
   (1) * /home/testprologmud/lib/swipl/pack
   (2)   Cancel

Your choice?
% Cloning into '/home/testprologmud/lib/swipl/pack/prologmud'...
% Contacting server at http://www.swi-prolog.org/pack/query ... ok
Warning: Package depends on the following:
Warning:   "logicmoo_base", provided by logicmoo_base@1.1.117 from https://github.com/TeamSPoon/logicmoo_base.git
Warning:     "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:       "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:         "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:     "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:     "eggdrop", provided by eggdrop@1.1.117 from https://github.com/TeamSPoon/eggdrop.git
Warning:       "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "must_trace", provided by must_trace@1.1.117 from https://github.com/TeamSPoon/must_trace.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "predicate_streams", provided by predicate_streams@1.1.117 from https://github.com/TeamSPoon/predicate_streams.git
Warning:     "file_scope", provided by file_scope@1.1.117 from https://github.com/TeamSPoon/file_scope.git
Warning:       "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "must_trace", provided by must_trace@1.1.117 from https://github.com/TeamSPoon/must_trace.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "instant_prolog_docs", provided by instant_prolog_docs@1.1.117 from https://github.com/TeamSPoon/instant_prolog_docs.git
Warning:       "must_trace", provided by must_trace@1.1.117 from https://github.com/TeamSPoon/must_trace.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "loop_check", provided by loop_check@1.1.117 from https://github.com/TeamSPoon/loop_check.git
Warning:       "with_thread_local", provided by with_thread_local@1.1.117 from https://github.com/TeamSPoon/with_thread_local.git
Warning:         "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:     "multimodal_dcg", provided by multimodal_dcg@1.1.117 from https://github.com/TeamSPoon/multimodal_dcg.git
Warning:       "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "must_trace", provided by must_trace@1.1.117 from https://github.com/TeamSPoon/must_trace.git
Warning:       "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:         "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:           "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:       "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "pfc", provided by pfc@1.1.117 from https://github.com/TeamSPoon/pfc.git
Warning:       "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:       "file_scope", provided by file_scope@1.1.117 from https://github.com/TeamSPoon/file_scope.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:         "must_trace", provided by must_trace@1.1.117 from https://github.com/TeamSPoon/must_trace.git
Warning:           "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:             "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:               "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:           "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "hook_hybrid", provided by hook_hybrid@1.1.117 from https://github.com/TeamSPoon/hook_hybrid.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:         "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:         "file_scope", provided by file_scope@1.1.117 from https://github.com/TeamSPoon/file_scope.git
Warning:           "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:           "must_trace", provided by must_trace@1.1.117 from https://github.com/TeamSPoon/must_trace.git
Warning:             "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:               "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:                 "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:             "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:         "loop_check", provided by loop_check@1.1.117 from https://github.com/TeamSPoon/loop_check.git
Warning:           "with_thread_local", provided by with_thread_local@1.1.117 from https://github.com/TeamSPoon/with_thread_local.git
Warning:             "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:         "must_trace", provided by must_trace@1.1.117 from https://github.com/TeamSPoon/must_trace.git
Warning:           "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:             "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:               "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:           "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:         "subclause_expansion", provided by subclause_expansion@1.1.117 from https://github.com/TeamSPoon/subclause_expansion.git
Warning:           "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:           "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "loop_check", provided by loop_check@1.1.117 from https://github.com/TeamSPoon/loop_check.git
Warning:         "with_thread_local", provided by with_thread_local@1.1.117 from https://github.com/TeamSPoon/with_thread_local.git
Warning:           "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:       "must_trace", provided by must_trace@1.1.117 from https://github.com/TeamSPoon/must_trace.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "no_repeats", provided by no_repeats@1.1.117 from https://github.com/TeamSPoon/no_repeats.git
Warning:       "s_expression", provided by s_expression@1.1.117 from https://github.com/TeamSPoon/s_expression.git
Warning:         "with_open_options", provided by with_open_options@1.1.117 from https://github.com/TeamSPoon/with_open_options.git
Warning:       "with_thread_local", provided by with_thread_local@1.1.117 from https://github.com/TeamSPoon/with_thread_local.git
Warning:         "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:       "xlisting", provided by xlisting@1.1.117 from https://github.com/TeamSPoon/xlisting.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:     "prologmud", provided by prologmud@1.1.117 from https://github.com/TeamSPoon/prologmud.git
Warning:     "s_expression", provided by s_expression@1.1.117 from https://github.com/TeamSPoon/s_expression.git
Warning:       "with_open_options", provided by with_open_options@1.1.117 from https://github.com/TeamSPoon/with_open_options.git
Warning:     "subclause_expansion", provided by subclause_expansion@1.1.117 from https://github.com/TeamSPoon/subclause_expansion.git
Warning:       "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:       "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "with_thread_local", provided by with_thread_local@1.1.117 from https://github.com/TeamSPoon/with_thread_local.git
Warning:       "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:     "xlisting", provided by xlisting@1.1.117 from https://github.com/TeamSPoon/xlisting.git
Warning:       "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:         "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:           "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:     "xlisting_web", provided by xlisting_web@1.1.117 from https://github.com/TeamSPoon/xlisting_web.git
Warning:       "xlisting", provided by xlisting@1.1.117 from https://github.com/TeamSPoon/xlisting.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:   "pfc", provided by pfc@1.1.117 from https://github.com/TeamSPoon/pfc.git
Warning:     "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:     "file_scope", provided by file_scope@1.1.117 from https://github.com/TeamSPoon/file_scope.git
Warning:       "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "must_trace", provided by must_trace@1.1.117 from https://github.com/TeamSPoon/must_trace.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "hook_hybrid", provided by hook_hybrid@1.1.117 from https://github.com/TeamSPoon/hook_hybrid.git
Warning:       "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:         "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:           "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:       "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:       "file_scope", provided by file_scope@1.1.117 from https://github.com/TeamSPoon/file_scope.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:         "must_trace", provided by must_trace@1.1.117 from https://github.com/TeamSPoon/must_trace.git
Warning:           "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:             "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:               "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:           "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "loop_check", provided by loop_check@1.1.117 from https://github.com/TeamSPoon/loop_check.git
Warning:         "with_thread_local", provided by with_thread_local@1.1.117 from https://github.com/TeamSPoon/with_thread_local.git
Warning:           "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:       "must_trace", provided by must_trace@1.1.117 from https://github.com/TeamSPoon/must_trace.git
Warning:         "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:           "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:             "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:       "subclause_expansion", provided by subclause_expansion@1.1.117 from https://github.com/TeamSPoon/subclause_expansion.git
Warning:         "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:         "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "loop_check", provided by loop_check@1.1.117 from https://github.com/TeamSPoon/loop_check.git
Warning:       "with_thread_local", provided by with_thread_local@1.1.117 from https://github.com/TeamSPoon/with_thread_local.git
Warning:         "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:     "must_trace", provided by must_trace@1.1.117 from https://github.com/TeamSPoon/must_trace.git
Warning:       "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:         "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:           "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git
Warning:       "logicmoo_utils", provided by logicmoo_utils@1.1.117 from https://github.com/TeamSPoon/logicmoo_utils.git
Warning:     "no_repeats", provided by no_repeats@1.1.117 from https://github.com/TeamSPoon/no_repeats.git
Warning:     "s_expression", provided by s_expression@1.1.117 from https://github.com/TeamSPoon/s_expression.git
Warning:       "with_open_options", provided by with_open_options@1.1.117 from https://github.com/TeamSPoon/with_open_options.git
Warning:     "with_thread_local", provided by with_thread_local@1.1.117 from https://github.com/TeamSPoon/with_thread_local.git
Warning:       "each_call_cleanup", provided by each_call_cleanup@1.1.117 from https://github.com/TeamSPoon/each_call_cleanup.git
Warning:     "xlisting", provided by xlisting@1.1.117 from https://github.com/TeamSPoon/xlisting.git
Warning:       "clause_attvars", provided by clause_attvars@1.1.117 from https://github.com/TeamSPoon/clause_attvars.git
Warning:         "dictoo", provided by dictoo@1.1.117 from https://github.com/TeamSPoon/dictoo.git
Warning:           "gvar_syntax", provided by gvar_syntax@1.1.117 from https://github.com/TeamSPoon/gvar_syntax.git

What do you wish to do
   (1) * Install proposed dependencies
   (2)   Only install requested package
   (3)   Cancel

Your choice?
% "prologmud.git" was downloaded 1 times
% Cloning into '/home/testprologmud/lib/swipl/pack/logicmoo_base'...
i logicmoo_base@1.1.117     - LogicMOO - Extends Prolog Programming to support Dynamic Epistemic Logic (DEL) with Constraints
% Cloning into '/home/testprologmud/lib/swipl/pack/clause_attvars'...
i clause_attvars@1.1.117    - An alternate interface to the clause database to allow attributed variables to be asserted
% Cloning into '/home/testprologmud/lib/swipl/pack/dictoo'...
i dictoo@1.1.117            - Dict-like OO Syntax
% Cloning into '/home/testprologmud/lib/swipl/pack/gvar_syntax'...
i gvar_syntax@1.1.117       - Global Variable Syntax
% Cloning into '/home/testprologmud/lib/swipl/pack/each_call_cleanup'...
i each_call_cleanup@1.1.117 - Each Call Redo Setup and Cleanup
% Updating index for library /home/testprologmud/lib/swipl/pack/each_call_cleanup/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/eggdrop'...
i eggdrop@1.1.117           - Hook up to an existing IRC Client called an Eggdrop
% Cloning into '/home/testprologmud/lib/swipl/pack/logicmoo_utils'...
i logicmoo_utils@1.1.117    - Common predicates used by external Logicmoo Utils and Base
% Updating index for library /home/testprologmud/lib/swipl/pack/logicmoo_utils/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/must_trace'...
i must_trace@1.1.117        - Trace with your eyeballs instead of your fingers
% Updating index for library /home/testprologmud/lib/swipl/pack/must_trace/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/predicate_streams'...
i predicate_streams@1.1.117 - Implement your own Abstract Predicate Streams
% Updating index for library /home/testprologmud/lib/swipl/pack/predicate_streams/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/file_scope'...
i file_scope@1.1.117        - File local scoped efects
% Updating index for library /home/testprologmud/lib/swipl/pack/file_scope/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/instant_prolog_docs'...
i instant_prolog_docs@1.1.117 - Magically document prolog source files based on predicate and variable naming conventions
% Updating index for library /home/testprologmud/lib/swipl/pack/instant_prolog_docs/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/loop_check'...
i loop_check@1.1.117        - New simple loop checking
% Updating index for library /home/testprologmud/lib/swipl/pack/loop_check/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/with_thread_local'...
i with_thread_local@1.1.117 - Call a Goal with local assertions
% Updating index for library /home/testprologmud/lib/swipl/pack/with_thread_local/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/multimodal_dcg'...
i multimodal_dcg@1.1.117    - Reduce floundering of DCGs by constraining and narrowing search
% Cloning into '/home/testprologmud/lib/swipl/pack/pfc'...
i pfc@1.1.117               - Pfc -- a package for forward chaining in Prolog
% Cloning into '/home/testprologmud/lib/swipl/pack/hook_hybrid'...
i hook_hybrid@1.1.117       - Hook assert retract call of *specific* predicates
% Updating index for library /home/testprologmud/lib/swipl/pack/hook_hybrid/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/subclause_expansion'...
i subclause_expansion@1.1.117 - More detailed versions of term/goal expansion hooks
% Cloning into '/home/testprologmud/lib/swipl/pack/no_repeats'...
i no_repeats@1.1.117        - New ways to avoid duplicate solutions
% Updating index for library /home/testprologmud/lib/swipl/pack/no_repeats/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/s_expression'...
i s_expression@1.1.117      - Utilities for Handling of S-Expression Lisp/Scheme-Like forms and parsing of KIF, GDL, PDDL, CLIF
% Updating index for library /home/testprologmud/lib/swipl/pack/s_expression/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/with_open_options'...
i with_open_options@1.1.117 - Utilities to open various objects for read/write
% Updating index for library /home/testprologmud/lib/swipl/pack/with_open_options/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/xlisting'...
i xlisting@1.1.117          - Selective Interactive Non-Deterministic Tracing
% Updating index for library /home/testprologmud/lib/swipl/pack/xlisting/prolog/
% Cloning into '/home/testprologmud/lib/swipl/pack/prologmud'...
i prologmud@1.1.117         - Online text adventure game - MUD Server
% Cloning into '/home/testprologmud/lib/swipl/pack/xlisting_web'...
i xlisting_web@1.1.117      - Manipulate and browse prolog runtime over www
Package:                prologmud
Title:                  Online text adventure game - MUD Server
Installed version:      1.1.117
Author:                 Douglas R. Miles <logicmoo@gmail.com>, Douglas Miles <http://www.linkedin.com/in/logicmoo>
Maintainer:             TeamSPoon <https://github.com/TeamSPoon/>
Packager:               TeamSPoon <https://github.com/TeamSPoon/>
Home page:              https://github.com/TeamSPoon/prologmud.git
Download URL:           https://github.com/TeamSPoon/prologmud/release/*.zip
Requires:               logicmoo_base, pfc
Activate pack "prologmud" Y/n?
true.

?- pack_install(prologmud_samples).
% Contacting server at http://www.swi-prolog.org/pack/query ... ok
Install prologmud_samples@1.1.117 from GIT at https://github.com/TeamSPoon/prologmud_samples.git Y/n?
% Cloning into '/home/testprologmud/lib/swipl/pack/prologmud_samples'...
% Contacting server at http://www.swi-prolog.org/pack/query ... ok
% "prologmud_samples.git" was downloaded 1 times
Package:                prologmud_samples
Title:                  Online text adventure game - Sample
Installed version:      1.1.117
Author:                 Douglas R. Miles <logicmoo@gmail.com>, Douglas Miles <http://www.linkedin.com/in/logicmoo>
Maintainer:             TeamSPoon <https://github.com/TeamSPoon/>
Packager:               TeamSPoon <https://github.com/TeamSPoon/>
Home page:              https://github.com/TeamSPoon/prologmud_samples.git
Download URL:           https://github.com/TeamSPoon/prologmud_samples/release/*.zip
Requires:               prologmud
Activate pack "prologmud_samples" Y/n?
true.

?- pack_install(logtalk).
% Contacting server at http://www.swi-prolog.org/pack/query ... ok
Install logtalk@3.10.3 from http://logtalk.org/files/swi-prolog/packs/logtalk-3.10.3.tgz Y/n?
% Contacting server at http://www.swi-prolog.org/pack/query ... ok
% "logtalk-3.10.3.tgz" was downloaded 3 times
Package:                logtalk
Title:                  Logtalk - Object-Oriented Logic Programming Language
Installed version:      3.10.3
Author:                 Paulo Moura <pmoura@logtalk.org>
Maintainer:             Paulo Moura <pmoura@logtalk.org>
Packager:               Paulo Moura <pmoura@logtalk.org>
Home page:              http://logtalk.org/
Download URL:           http://logtalk.org/files/swi-prolog/packs/logtalk-3.10.3.tgz
Install "logtalk-3.10.3.tgz" (4,134,728 bytes) Y/n?
true.


?- consult(library(prologmud_sample_games/run_mud_server)).
```
