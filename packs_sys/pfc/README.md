# Pfc -- forward chaining in Prolog

Installation using SWI-Prolog 7.1 or later:

    `?- pack_install('https://github.com/logicmoo/pfc.git').`



This is a modification of Tim Finin's PFC.

Notable changes are:

 * Forward chaining `=>` is renamed to `==>` to avoid conflict with extensive downstream use of =>/2 to mean logical implication.
 * Bidirectional Forward chaining `<=>` renamed to `<==>` to avoid conflict with logical equivance `<=>`
 * Memoized backchain `<=` is renamed to `<-` to avoid conflict with extensive downstream use of <=/2 to mean reverse implication..  Historically '<-' had meant what is now know as ":-" being unused it was borrowed as it means "Backchaining"
 * Added Macro Transform =@=> so instead of asserting the Anteceedant to assert the Consequent

 @TODO - MANY MORE CHANGES TO WRITE - for now back to coding...
 @TODO = Added Cutted Forward Chaining =!=> as a signal to stop processing rules on first success

    
# Original README

The Pfc system is a package that provides a forward reasoning capability to be used together with conventional Prolog programs.  The Pfc inference rules are Prolog terms which are asserted as clauses into the regular Prolog database.  When new facts or forward reasoning rules are added to the Prolog database (via a special predicate add/1, forward reasoning is triggered and additional facts that can be deduced via the application of the forward chaining rules are also added to the database.  A simple justification-based truth-maintenance system is provided as well as simple predicates to explore the resulting proof trees.

It was originally written circa 1988 at the [Unisys Paoli Research Center](https://en.wikipedia.org/wiki/Paoli_Research_Center).  For more information, see

* Tim Finin,Rich Fritzson and Dave Matuszek, [Adding Forward Chaining and Truth Maintenance to Prolog](http://ebiq.org/p/682), IEEE Conf. on Artificial Intelligence Applications, pp. 123-130, Miami, March 1989.

* Tim Finin, [Pfc User Manual](https://github.com/finin/pfc/blob/master/man/pfc.pdf), Technical Report, COmputer Science and Electrical Engineering, University of Maryland, Baltimore COunty, August 1999.

or contact Tim Finin, finin@umbc.edu

If you use Pfc in your research, please cite the 1989 IEEE CAIA paper.



# Some TODOs

Document this pack!
Write tests
Untangle the 'pack' install deps
Still in progress (Moving predicates over here from logicmoo_base)


[BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
Douglas Miles <logicmoo@gmail.com> and logicmoo
All rights reserved.

# Not _obligated_ to maintain a git fork just to contribute

Dislike having tons of forks that are several commits behind the main git repo?

Be old school - Please ask to be added to logicmoo and Contribute directly !
Still, we wont stop you from doing it the Fork+PullRequest method





