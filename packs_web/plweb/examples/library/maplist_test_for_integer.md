# Test for integers in a range

```
?- maplist(between(1, 3), [1, 2, 3]).
true.

?- maplist(between(1, 3), [1, 2, 99]).
false.
```
