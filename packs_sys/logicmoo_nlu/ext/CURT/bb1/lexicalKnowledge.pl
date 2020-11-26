/*************************************************************************

    File: lexicalKnowledge.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(lexicalKnowledge,[lexicalKnowledge/3]).

/*========================================================================
   Axioms for Lexical Knowledge: Nouns
========================================================================*/

lexicalKnowledge(event,1,Axiom):-
   Axiom = all(A,imp(event(A),thing(A))).

lexicalKnowledge(entity,1,Axiom):- 
   Axiom = all(A,imp(entity(A),thing(A))).

lexicalKnowledge(object,1,Axiom):- 
   Axiom = all(A,imp(object(A),entity(A))).

lexicalKnowledge(organism,1,Axiom):- 
   Axiom = all(A,imp(organism(A),entity(A))).

lexicalKnowledge(food,1,Axiom):- 
   Axiom = all(A,imp(food(A),object(A))).

lexicalKnowledge(artifact,1,Axiom):- 
   Axiom = all(A,imp(artifact(A),object(A))).

lexicalKnowledge(building,1,Axiom):- 
   Axiom = all(A,imp(building(A),artifact(A))).

lexicalKnowledge(instrument,1,Axiom):- 
   Axiom = all(A,imp(instrument(A),artifact(A))).

lexicalKnowledge(animal,1,Axiom):- 
   Axiom = all(A,imp(animal(A),organism(A))).

lexicalKnowledge(person,1,Axiom):- 
   Axiom = all(A,imp(person(A),organism(A))).

lexicalKnowledge(plant,1,Axiom):- 
   Axiom = all(A,imp(plant(A),organism(A))).

lexicalKnowledge(man,1,Axiom):- 
   Axiom = all(A,imp(man(A),person(A))).

lexicalKnowledge(woman,1,Axiom):- 
   Axiom = all(A,imp(woman(A),person(A))).

lexicalKnowledge(beverage,1,Axiom):- 
   Axiom = all(A,imp(beverage(A),food(A))).

lexicalKnowledge(foodstuff,1,Axiom):- 
   Axiom = all(A,imp(foodstuff(A),food(A))).

lexicalKnowledge(container,1,Axiom):- 
   Axiom = all(A,imp(container(A),instrument(A))).

lexicalKnowledge(device,1,Axiom):- 
   Axiom = all(A,imp(device(A),instrument(A))).

lexicalKnowledge(cup,1,Axiom):- 
   Axiom = all(A,imp(cup(A),container(A))).

lexicalKnowledge(glass,1,Axiom):- 
   Axiom = all(A,imp(glass(A),container(A))).

lexicalKnowledge(burger,1,Axiom):- 
   Axiom = all(A,imp(burger(A),foodstuff(A))).

lexicalKnowledge(qpwc,1,Axiom):- 
   Axiom = all(A,imp(qpwc(A),foodstuff(A))).

lexicalKnowledge(boxer,1,Axiom):- 
   Axiom = all(A,imp(boxer(A),person(A))).

lexicalKnowledge(boss,1,Axiom):- 
   Axiom = all(A,imp(boss(A),person(A))).

lexicalKnowledge(criminal,1,Axiom):- 
   Axiom = all(A,imp(criminal(A),person(A))).

lexicalKnowledge(customer,1,Axiom):- 
   Axiom = all(A,imp(customer(A),person(A))).

lexicalKnowledge(owner,1,Axiom):- 
   Axiom = all(A,imp(owner(A),person(A))).

lexicalKnowledge(robber,1,Axiom):- 
   Axiom = all(A,imp(robber(A),person(A))).

lexicalKnowledge(vehicle,1,Axiom):- 
   Axiom = all(A,imp(vehicle(A),instrument(A))).

lexicalKnowledge(car,1,Axiom):- 
   Axiom = all(A,imp(car(A),vehicle(A))).

lexicalKnowledge(chainsaw,1,Axiom):- 
   Axiom = all(A,imp(chainsaw(A),device(A))).

lexicalKnowledge(drug,1,Axiom):- 
   Axiom = all(A,imp(drug(A),artifact(A))).

lexicalKnowledge(episode,1,Axiom):- 
   Axiom = all(A,imp(episode(A),event(A))).

lexicalKnowledge(footmassage,1,Axiom):- 
   Axiom = all(A,imp(footmassage(A),event(A))).

lexicalKnowledge(fdshake,1,Axiom):- 
   Axiom = all(A,imp(fdshake(A),beverage(A))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(weapon(A),instrument(A))).

lexicalKnowledge(gun,1,Axiom):- 
   Axiom = all(A,imp(gun(A),weapon(A))).

lexicalKnowledge(hammer,1,Axiom):- 
   Axiom = all(A,imp(hammer(A),device(A))).

lexicalKnowledge(hashbar,1,Axiom):- 
   Axiom = all(A,imp(hashbar(A),building(A))).

lexicalKnowledge(restaurant,1,Axiom):- 
   Axiom = all(A,imp(restaurant(A),building(A))).

lexicalKnowledge(husband,1,Axiom):- 
   Axiom = all(A,imp(husband(A),man(A))).

lexicalKnowledge(joke,1,Axiom):- 
   Axiom = all(A,imp(joke(A),event(A))).

lexicalKnowledge(needle,1,Axiom):- 
   Axiom = all(A,imp(needle(A),device(A))).

lexicalKnowledge(piercing,1,Axiom):- 
   Axiom = all(A,imp(piercing(A),artifact(A))).

lexicalKnowledge(radio,1,Axiom):- 
   Axiom = all(A,imp(radio(A),instrument(A))).

lexicalKnowledge(suitcase,1,Axiom):- 
   Axiom = all(A,imp(suitcase(A),container(A))).

lexicalKnowledge(shotgun,1,Axiom):- 
   Axiom = all(A,imp(shotgun(A),gun(A))).

lexicalKnowledge(sword,1,Axiom):- 
   Axiom = all(A,imp(sword(A),weapon(A))).

lexicalKnowledge(wife,1,Axiom):- 
   Axiom = all(A,imp(wife(A),woman(A))).

lexicalKnowledge(entity,1,Axiom):- 
   Axiom = all(A,imp(entity(A),not(event(A)))).

lexicalKnowledge(organism,1,Axiom):- 
   Axiom = all(A,imp(organism(A),not(object(A)))).

lexicalKnowledge(artifact,1,Axiom):- 
   Axiom = all(A,imp(artifact(A),not(food(A)))).

lexicalKnowledge(person,1,Axiom):- 
   Axiom = all(A,imp(person(A),not(animal(A)))).

lexicalKnowledge(plant,1,Axiom):- 
   Axiom = all(A,imp(plant(A),not(animal(A)))).

lexicalKnowledge(plant,1,Axiom):- 
   Axiom = all(A,imp(plant(A),not(person(A)))).

lexicalKnowledge(instrument,1,Axiom):- 
   Axiom = all(A,imp(instrument(A),not(building(A)))).

lexicalKnowledge(drug,1,Axiom):- 
   Axiom = all(A,imp(drug(A),not(building(A)))).

lexicalKnowledge(piercing,1,Axiom):- 
   Axiom = all(A,imp(piercing(A),not(building(A)))).

lexicalKnowledge(drug,1,Axiom):- 
   Axiom = all(A,imp(drug(A),not(instrument(A)))).

lexicalKnowledge(piercing,1,Axiom):- 
   Axiom = all(A,imp(piercing(A),not(instrument(A)))).

lexicalKnowledge(piercing,1,Axiom):- 
   Axiom = all(A,imp(piercing(A),not(drug(A)))).

lexicalKnowledge(woman,1,Axiom):- 
   Axiom = all(A,imp(woman(A),not(man(A)))).

lexicalKnowledge(device,1,Axiom):- 
   Axiom = all(A,imp(device(A),not(radio(A)))).

lexicalKnowledge(container,1,Axiom):- 
   Axiom = all(A,imp(container(A),not(radio(A)))).

lexicalKnowledge(vehicle,1,Axiom):- 
   Axiom = all(A,imp(vehicle(A),not(radio(A)))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(weapon(A),not(radio(A)))).

lexicalKnowledge(container,1,Axiom):- 
   Axiom = all(A,imp(container(A),not(device(A)))).

lexicalKnowledge(vehicle,1,Axiom):- 
   Axiom = all(A,imp(vehicle(A),not(device(A)))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(weapon(A),not(device(A)))).

lexicalKnowledge(vehicle,1,Axiom):- 
   Axiom = all(A,imp(vehicle(A),not(container(A)))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(weapon(A),not(container(A)))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(weapon(A),not(vehicle(A)))).

lexicalKnowledge(beverage,1,Axiom):- 
   Axiom = all(A,imp(beverage(A),not(foodstuff(A)))).

lexicalKnowledge(footmassage,1,Axiom):- 
   Axiom = all(A,imp(footmassage(A),not(episode(A)))).

lexicalKnowledge(joke,1,Axiom):- 
   Axiom = all(A,imp(joke(A),not(episode(A)))).

lexicalKnowledge(joke,1,Axiom):- 
   Axiom = all(A,imp(joke(A),not(footmassage(A)))).

lexicalKnowledge(cup,1,Axiom):- 
   Axiom = all(A,imp(cup(A),not(glass(A)))).

lexicalKnowledge(suitcase,1,Axiom):- 
   Axiom = all(A,imp(suitcase(A),not(glass(A)))).

lexicalKnowledge(suitcase,1,Axiom):- 
   Axiom = all(A,imp(suitcase(A),not(cup(A)))).

lexicalKnowledge(chainsaw,1,Axiom):- 
   Axiom = all(A,imp(chainsaw(A),not(hammer(A)))).

lexicalKnowledge(needle,1,Axiom):- 
   Axiom = all(A,imp(needle(A),not(hammer(A)))).

lexicalKnowledge(needle,1,Axiom):- 
   Axiom = all(A,imp(needle(A),not(chainsaw(A)))).

lexicalKnowledge(unmarried,1,Axiom):- 
   Axiom = all(A,imp(unmarried(A),not(married(A)))).

lexicalKnowledge(burger,1,Axiom):- 
   Axiom = all(A,imp(burger(A),not(qpwc(A)))).

lexicalKnowledge(restaurant,1,Axiom):- 
   Axiom = all(A,imp(restaurant(A),not(hashbar(A)))).

lexicalKnowledge(gun,1,Axiom):- 
   Axiom = all(A,imp(gun(A),not(sword(A)))).

lexicalKnowledge(wife,1,Axiom):- 
   Axiom = all(A,imp(wife(A),married(A))).


/*========================================================================
   Axioms for Lexical Knowledge: Proper Names
========================================================================*/

lexicalKnowledge(mia,0,Axiom):- 
   Axiom = all(A,imp(eq(A,mia),woman(A))).

lexicalKnowledge(vincent,0,Axiom):- 
   Axiom = all(A,imp(eq(A,vincent),man(A))).


/*========================================================================
   Axioms for Lexical Knowledge: Adjectives
========================================================================*/

lexicalKnowledge(red,1,Axiom):- 
   Axiom = all(A,imp(red(A),not(blue(A)))).

lexicalKnowledge(big,1,Axiom):- 
   Axiom = all(A,imp(big(A),not(small(A)))).

lexicalKnowledge(sad,1,Axiom):- 
   Axiom = all(A,imp(sad(A),not(happy(A)))).


/*========================================================================
   Axioms for Lexical Knowledge: Intransitive Verbs
========================================================================*/

lexicalKnowledge(collapse,1,Axiom):-
   Axiom = all(X,imp(collapse(X),or(person(X),building(X)))).

lexicalKnowledge(dance,1,Axiom):- 
   Axiom = all(X,imp(dance(X),person(X))).

lexicalKnowledge(die,1,Axiom):- 
   Axiom = all(X,imp(die(X),organism(X))).

lexicalKnowledge(growl,1,Axiom):- 
   Axiom = all(X,imp(growl(X),or(animal(X),person(X)))).


/*========================================================================
   Axioms for Lexical Knowledge: Transitive Verbs
========================================================================*/

lexicalKnowledge(clean,2,Axiom):- 
   Axiom = all(X,all(Y,imp(clean(X,Y),and(person(X),artifact(Y))))).

lexicalKnowledge(drink,2,Axiom):-    
   Axiom = all(X,all(Y,imp(drink(X,Y),and(person(X),beverage(Y))))).

lexicalKnowledge(eat,2,Axiom):- 
   Axiom = all(X,all(Y,imp(eat(X,Y),
               and(or(person(X),animal(X)),
                   and(edible(Y),food(Y)))))).
