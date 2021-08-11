# Test for atoms

```
?- maplist(atom, [a, b, c]).
true.

?- maplist(atom, [a, 2, c]).
false.
```
