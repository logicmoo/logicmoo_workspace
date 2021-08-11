```
?- append([a,b], [c], X).
X = [a,b,c].

?- append(X, [Last], [a,b,c]).
X = [a,b],
Last = c.

?- append([a,b], More, List).
List = [a,b|More].
```
