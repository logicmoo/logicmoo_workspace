# LPS README #
## Introduction ##
### License and Copyright (preliminary) ###
All files in this repository are copyright Robert Kowalski and Fariba Sadri, except for files with specific copyright notices.
### What is this repository for? ###
To support the development of LPS, evolving the Wei implementation and complementing it with related tools and examples

* To edit this and other future wiki documents: [learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### Who do I talk to? ###
* Repo owner or admin: mc@interprolog.com, jacinto.davila@gmail.com
* Bugs and engine support: mc@interprolog.com

## How do I get set up? ##
### With Prolog Studio ###
* Pull or download this whole repository into a new folder "LPS"
* Follow instructions at top of [InterProlog Studio](http://interprolog.com/wiki/index.php?title=Studio_Download_and_installation), including a recent XSB Prolog install

### Without Prolog Studio ###
* Pull or download this whole repository into a new folder "LPS"
* Install a recent (>= April 4 2016) XSB Prolog, such as the one in the [Prolog Studio site](http://interprolog.com/wiki/index.php?title=Studio_Download_and_installation#Installing_XSB_Prolog) for Windows; or pull the latest XSB from the [Sourceforge SVN site](https://sourceforge.net/p/xsb/src/HEAD/tree/trunk/XSB/) and build it yourself - NOT the official release as of today.
* Launch xsb and consult 'LPS_dir/Wei-Engine/interpreter.P'

## Known Bugs
### Missing candidate actions
No matter the strategy (goal_strat(breadth) or not), the following generates a1,a1,... when it should generate only a1+a2 (simultaneous):

	reactive_rule( [happens(cond3,_,_)],
	 [happens(a1,T1,T2),happens(a2,T1,T2)] ).

## How to contribute ##
* Before you commit any changes to the LPS engine or tools, make sure you execute all tests with 'test_examples'.
* Please comment your code, and add appropriate copyright/license header

### Writing tests ###
If you add a file to the examples directory and execute it with make_test, e.g. go(MyFile,[make_test]), a test results file MyFile.lpst will be generated. If the program behaves correctly with the current version of LPS, commit this file too. When you later execute go(MyFile,[run_test]), the program behavior will be compared with the previous test results; ditto when you run all tests, with 'test_examples'.

## Road map ##
### More examples ###
All examples should act as tests too. 
Examples TBD:

* A refactored/cleaned up version of the Survival Melee game (although this depends partly on LPS syntax changes)
* LPS "subsuming" Abductive Logic Programming
* LPS "dispensing" with modal logics, cf. Bob's "Obligations" paper
* Workflow, e.g. document routing?
* Economics (Jacinto?)
* Smart house and its energy game (Bob's idea)
* Lamport's clock
* Kid oriented stuff
* Include and extend these with a cookbook or better - "How to do it with LPS"

### Syntax ###
#### Internal (Wei) syntax ####
* not for timeless predicates
* allow (also) fluent lists in postconditions
* if-then-else (c->a;b) - meaning (c,a;not(c),b) - and disjunctions (a;b):
	* in composite event bodies
	* in intensional fluent bodies
	* in reactive rule antecedents
* ? Consider a non list in a l_timeless body to be Prolog code to be called directly ?
* ? Other logical constructs ?

#### Experimental "papers syntax" ####
Currently prototyped in utils/psyntax.P

* Reflect additions to internal syntax
* Better representation of time, possibly assuming implicit time sequence in body literals

#### Other higher level syntaxes TBD ####
* Teleoreactive, used in Kit's report (?); perhaps adapt the Java preprocessor to internet (Wei) syntax
* Object-oriented?, survival game case in point
* Restricted Natural Language, tying into the linguistic nature of fluents, events, etc.

### Engine per se ###
* SWI Prolog version, sharing as much code as possible with XSB Prolog's
* deal with timeless predicate "floundering" (inadmissible calls, e.g. length(Var1,Var2)): runtime error probaby; subgoal delaying too?
* We have an interpreter; how will the compiler be?

### Tools ###
The engine must NOT depend on these to function with canonica functionality.

#### Studio editor ####
* finer (subterm) error pinpointing
* Smart navigation, dealing with LPS richer causality, not strictly top-bottom as Prolog's

#### Studio timeline ####
The changes will probably wait for move to SWISH:

* display causes for actions; requires further tracing info from the engine

#### Other visualizers ####
* Game engines, 3D and 2D
* Interactive executor/debugger, step by step, forward/back etc.

#### Explanator ####
TBD

### Deployment issues ###
* After all this matures further... where will LPS agents/programs deploy into? Servers? Mobile implementations e.g. via JI Prolog ? ...
* As we gain experience with games etc., the "environment" (cf. LPS literature) will provide events (e.g. user click, bullet collision, remote request) and execute actions (e.g. move object, send message in some protocol); how will this be specified? Might a LPS program have an "interface" section distinct from the main part?
