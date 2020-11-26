Consider these horne clause rules:
```

hungery(X):- has_stomach(X),has(X,Y),isa(Y,stomache),isa(Y,emptything).

has_stomach(X):-has(X,Y),isa(Y,stomache).

food(Y):-isa(Y,food).

really_wants(X,Y):-likes(X,Y),near(X,Y),hungery(X),food(Y).


isa(Y,emptything):-isa(Y,container),not(contains(Y,_)).


contains(Y,someContainedThingOf(Y)):- isa(Y,container), not(isa(Y,emptything))

```

Some redundant, All basically legal.  Easy to read? maybe.

I'd like the game to incorporate such rules.

My problem, I have written an implementation of such a working database in prolog. 

It is several 1000s lines of code.   To get a feel for the codebase here is about 1/30th of it..  http://logicmoo.cvs.sourceforge.net/viewvc/logicmoo/openmodaliy/iso-prolog/logicmoo_cmp_compiler.pl?hideattic=0&revision=1.1&view=markup  


I no longer wish to maintain it.   I'd like to simply drop in a system that will replace the one I created that is maintained by more than just me.


I am goingm to try though to salvage the good parts...


The main core 30k lines of prolog code.. (at least 10k i wrote myself) 

In order for me to manage that much code i created a specialized pluggable and debugging framework.

Automated test suite for it.

For example runs and leaves html documents behind.



Also some code like must/1 and debug logging messaging I needing to upgrade to more modern ways.

IndiGOLOG/FLUX creates a SitCalc DB system..  It showed me some tricks I can do to avoid Skolems.   


However Skolems are normally only a trick to handle PNF ->DNF-> Horne Clause conversions? Yes, but there is another extremely usefull extenstential notation that mimics Skolems that I don't think should be lost.  


Take these clause:

loves(motherOf(X),X):-true.

mother(motherOf(X),X):-true.



motherOf/1 is a Skolem! 

It would have mimiced the skolem created by: 

all(X,exists(Y,mother(X,Y) & loves(Y,X)).


1st problem - skolemization works and is too valuable to avoid.. YET it makes you feel like you need to implement your own verson of '='/2."


2nd problem - using prolog to "generate" instances of testable entities so you can submit them into predicates antecedant (Body literals) to make a soemthing true (a Head literal) makes that search to  numberOfInstaces(SomeClass)^ 3


