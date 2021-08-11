# Genealogist Part 1: Your first "real" Pengines application

A _genealogist_ is a person who traces or studies the descent of persons or families. Since a logic program capable of reasoning about family relations happens to be the "Hello World" of Prolog, we thought it would be appropriate to stick to this domain when building our first Pengines application. A typical example of such a program would be a file with the following content:

  ~~~~
ancestor_descendant(X, Y) :- parent_child(X, Y).
ancestor_descendant(X, Z) :- parent_child(X, Y), ancestor_descendant(Y, Z).

siblings(X, Y) :- parent_child(Z, X), parent_child(Z, Y), X @< Y.

parent_child(X, Y) :- mother_child(X, Y).
parent_child(X, Y) :- father_child(X, Y).

mother_child(trude, sally).

father_child(tom, sally).
father_child(tom, erica).
father_child(mike, tom).
  ~~~~

The first thing we need to do is to make this into an SWI-Prolog _module_. We do this by sticking a [module declaration](http://www.swi-prolog.org/pldoc/man?predicate=module/2) on top of it, like so:

  ~~~~
:- module(genealogist, 
	[  ancestor_descendant/2,
           siblings/2,
	   parent_child/2,
	   father_child/2,
	   mother_child/2
	]).

ancestor_descendant(X, Y) :- parent_child(X, Y).
ancestor_descendant(X, Z) :- parent_child(X, Y), ancestor_descendant(Y, Z).

siblings(X, Y) :- parent_child(Z, X), parent_child(Z, Y), X @< Y.

parent_child(X, Y) :- mother_child(X, Y).
parent_child(X, Y) :- father_child(X, Y).

mother_child(trude, sally).

father_child(tom, sally).
father_child(tom, erica).
father_child(mike, tom).
  ~~~~

We then save our file as =|genealogist.pl|=, create a new directory =genealogist= under =|./apps|=, and place the module file in it. Our directory structure under =|./apps|= should now be the following:

  ~~~~
  apps/
     genealogist/
        genealogist.pl
     swish\
     scratchpad\
  ~~~~

But we are not done yet. We must also insert the following declarations in the file =|./load.pl|=:
  ~~~~
:- pengine_application(genealogist).
:- use_module(genealogist:'/apps/genealogist/genealogist.pl').
  ~~~~
[Note: Where we put such declarations may change in a future version of Pengines.]

The first declaration merely names your application. (There is no need to give the application the same name as the directory, but it makes some sense to do so.) The second declaration imports the predicates exported by the module =genealogist= into the application module (which happens to have the same name here too, but nor is this required).

At last, we may (re)start our Pengines server and test our application. We can for example use an ordinary SWI-Prolog shell, perhaps running on another machine:

  ~~~~
?- use_module(library(pengines)).
true.

?- pengine_rpc('http://localhost:3030', ancestor_descendant(X, Y), [
         application(genealogist)
     ]).
X = trude,
Y = sally ;
X = tom,
Y = sally ;
X = tom,
Y = erica ;
X = mike,
Y = tom ;
X = mike,
Y = sally ;
X = mike,
Y = erica ;
false.

?- 
  ~~~~
In [Part 2 of this tutorial](http://www.swi-prolog.org/pengines/GenealogistGUI.txt) we are going to add a simple web-based GUI to our application.