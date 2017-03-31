
# Elements for the LPS Road Map #
*March 2017*


### More examples needed ###
All examples should act as tests too, as soon as they work, by running them with option make_test. 
Examples TBD:

* A refactored/cleaned up version of the Survival Melee game (although this depends partly on LPS syntax changes)
* LPS "intersecting" Abductive Logic Programming
* LPS "dispensing" with modal logics, cf. Bob's "Obligations" paper
	* Use SBVR [examples](http://www.kdmanalytics.com/sbvr/
	* Cf. Bob's May 21, 2016 email
* Workflow, e.g. document routing?
sbvr_intro_2.html) ?
* Economics and the Rational Choice Theory, for instance [the case of physician-induced demand](http://webdelprofesor.ula.ve/ingenieria/jacinto/publica/2014/phdposters_template-Pedro%20Cadenas.pdf)
* Smart house and its energy game (Bob's idea)
* Lamport's clock
* Kid oriented stuff
* Include and extend these into a cookbook or better - "How to do it with LPS"

### Syntax improvements ###
* allow (also) fluent lists in postconditions
* if-then-else (c->a;b) - meaning (c,a;not(c),b) - and disjunctions (a;b):
	* in composite event bodies
	* in intensional fluent bodies
	* in reactive rule antecedents
* ? Other logical constructs ?

#### Other higher level syntaxes TBD ####
* Teleoreactive, used in Kit's report (?); perhaps adapt the Java preprocessor to internet (Wei) syntax
* Object-oriented?, survival game case in point
* Restricted Natural Language, tying into the linguistic nature of fluents, events, etc.


### Engine per se ###
* deal with timeless predicate "floundering" (inadmissible calls, e.g. length(Var1,Var2)): runtime error probaby; subgoal delaying too?
* We have an interpreter; how will the compiler be?

### Tools / utils directory ###
The engine must always NOT depend on these to function with canonical functionality.


#### Timeline ####
* display causes for actions; requires further tracing info from the engine

#### Other visualizers ####
* Game engines, 3D and 2D
* Interactive executor/debugger, step by step, forward/back etc.

#### Explanator ####
TBD

### Deployment issues ###
* After all this matures further... where will LPS agents/programs deploy into? Servers? Mobile implementations e.g. via JI Prolog ? ...
* As we gain experience with games etc., the "environment" (cf. LPS literature) will provide events (e.g. user click, bullet collision, remote request) and execute actions (e.g. move object, send message in some protocol); how will this be specified? Might a LPS program have an "interface" section distinct from the main part?