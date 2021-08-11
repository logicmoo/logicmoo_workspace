---+ How to deal with the Prolog dynamic database?

The short answer is: _|if you can avoid using it, consider this first.|_

---++ Why is the dynamic database evil?

The database is a non-logical extension to Prolog. The core philosophy
of Prolog is that is searches for a set of variable bindings that makes a
query true. It does this using depth-first search, binding variables to
values. If an inconsistent variable binding is detected, the system
backtracks: it finds the last choice it took, reverts all actions done
since and continues with the next alternative. This is the core power
of Prolog and gives it its logical basis.

Even if you cannot be bothered by logic, consider what this does for
debugging: you can optimistically step over (skip) goals. If the answer
is wrong, you hit *retry*, which takes the execution back to the start
and you inspect the goal at a deeper level. In other words, the
backtracking machinery provides you with a time-machine that allows you
to go back in history.

Changes to the dynamic database are not reverted on backtracking,
destroying all these nice goodies. Second, dynamic predicates are like
destructible *|global variables|* and therefore subject to all the
common problems with such objects: name-space pollution, state-full
computation, initialization and cleanup requirements and lack of
_|thread safety|_.


---++ I need to preserve data over backtracking

Sometimes, results are computed and stored in the database to preserve
them over backtracking.  E.g.,

  ==
	...,
	assertz(result(X)),
	...,
	fail


	...,
	result(X),
  ==

There are clean skeletons that deal with most of such cases. First,
collecting all alternatives can be done using the all-solution
predicates findall/3, bagof/3 or setof/3.  For example:

  ==
  young_persons(YoungPersons) :-
	findall(Person,
		(  person(Person),
		   age(Person, Age),
		   Age < 18
		),
		YoungPersons).
  ==

A second skeleton deals with aggregation of results obtained from
backtracking.  For example, finding the average age.  Typically,
one can use library(aggregate) for such problems:

  ==
  average_age(Average) :-
	aggregate(r(count,sum(Age)),
		  (   person(Person),
		      age(Person, Age)
		  ),
		  r(Count, Sum)),
	Count > 0,
	Average is Sum/Count.
  ==


---++ I want to use dynamic predicates anyway!

Ok.  You need to take care of some things.

---+++ Initialization

If you want a dynamic database person(Name), your first problem is that
it is initially empty and thus a call

  ==
	...
	person(X),
  ==

yields the error below, which stops the computation

  ==
  ERROR: Undefined procedure: person/1
  ==

A classical solution to this is to use the flag *unknown*, setting this
to =fail=.  *|Never use the unknown flag.|*  It is a fossil that better
remains buried, in particular for SWI-Prolog, where a large part of the
development and library predicates are normally loaded _|on demand|_.

The correct solution is to use dynamic/1, as illustrated below. This
directive tells the compiler that the predicate may have no clauses and
also informs it that clauses may be added at runtime.

  ==
  :- dynamic
	person/1.
  ==

---+++ Cleanup

If the dynamic database is used to store temporary results of a
computation, you typically want them cleaned before you start. Here,
*|never use abolish/1|*. Following the ISO standard, abolish/1 not only
deletes all clauses, but also the (dynamic) predicate properties.
Instead, use retractall/1:

  ==
  cleanup_db :-
	retractall(person(_)).
  ==

Cleanup may be done before the goal using it is started, but of course
the disadvantage is that all useless results left at the end remain in
the database. You can also do it at the end, but then you have to be
careful that you actually reach the end. You may fail reaching the end
due to unexpected failure or an error!  One way to avoid that is using
call_cleanup/2 as below.  The call_cleanup/2 predicate guarantees that the
2nd argument is called, no matter _how_ the first argument terminates.

  ==
  go :-
	call_cleanup(go_with_clean_db, cleanup_db).
  ==

---++ Take home

  1. Do not use dynamic predicates.
  2. If you need to preserve answers over backtracking, use
     findall/3, bagof/3, setof/3 or library(aggregate).
  3. If you really need dynamic predicates, use dynamic/1 to
     declare them, cleanup using retractall/1, and consider using
     call_cleanup/2 to ensure the database is cleaned.

@see multifile/1 if database predicates are loaded from a file.
