# Genealogist Part 3: Supporting update

In this part of our tutorial we are going to add update capabilities to our application. It is in general not wise to allow a client to use assert/1 and retract/1 directly on the application's database. It is better to wrap assert/1 and retractall/1 in predicates that are then exported by (one of) the application modules. This is exactly what we are doing below.
  ~~~~
  :- module(genealogist, 
	[  ancestor_descendant/2,
	   siblings/2,
	   parent_child/2,
	   father_child/2,
	   mother_child/2,
	   assert_father_child/2,
	   assert_mother_child/2,	   
	   retract_father_child/2,
	   retract_mother_child/2
	]).


ancestor_descendant(X, Y) :- parent_child(X, Y).
ancestor_descendant(X, Z) :- parent_child(X, Y), ancestor_descendant(Y, Z).

siblings(X, Y) :- parent_child(Z, X), parent_child(Z, Y), X @< Y.

parent_child(X, Y) :- mother_child(X, Y).
parent_child(X, Y) :- father_child(X, Y).



:- dynamic mother_child/2, father_child/2.

assert_mother_child(Mother, Child) :-
	assert(mother_child(Mother, Child)).

assert_father_child(Father, Child) :-
	assert(father_child(Father, Child)).
	
retract_mother_child(Mother, Child) :-
	retractall(mother_child(Mother, Child)).

retract_father_child(Father, Child) :-
	retractall(father_child(Father, Child)).
  ~~~~

Note that we have deleted the facts for mother_child/2 and father_child/2 from the file. The clients will now have to assert such facts before any genealogist queries can succeed. Like this for example:

  ~~~~
  ?- use_module(library(pengines)).
true.

?- pengine_rpc('http://localhost:3030', assert_mother_child(trude, sally), [
       application(genealogist)
   ]).
true.

?- pengine_rpc('http://localhost:3030', ancestor_decendant(X, Y), [
       application(genealogist)
   ]).
X = trude,
Y = sally ;
false.

?-
  ~~~~

In [part 4 of this tutorial](http://www.swi-prolog.org/pengines/GenealogistMoreGUI.html) we are going to revise our HTML in order to build the necessary GUI parts for supporting updating. In fact, we are are going to revise the whole Genealogist application quite substantially, by for example splitting it up into separate files for HTML, CSS and JavaScript, in the way that is usually recommended for doing web development.