# Dealing with dynamic predicates

A dynamic predicate is introduced using dynamic/1, after which
clauses may be added using assertz/1.  Persistent dynamic predicates
are realized using library(persistency).


```
:- dynamic p/1.
```

```
?- assertz(p(a)).
?- p(X).
X = a.
?- retractall(p(_)).
?- p(X).
false.
```

@author Jan Wielemaker
