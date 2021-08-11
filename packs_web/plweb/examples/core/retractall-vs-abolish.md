# retractall/1 or abolish/1?

The retractall/1 predicate removes all clauses whose _head_ matches the
argument from a dynamic predicate. The abolish/1 predicate takes a
_predicate indicator_ to remove __all clauses__ and __all properties__.

```
:- dynamic p/1.

```

```
?- assertz(p(a)).
?- assertz(p(b)).
?- retractall(p(b)).
?- p(a).
true.
?- p(b).
false.
?- retractall(p(_)).
?- p(X).
false.
```

```
?- assertz(p(a)).
?- assertz(p(b)).
?- abolish(p/1).
?- p(X).
ERROR: Unknown procedure: p/1 (DWIM could not correct goal)
```
