```
?- selectchk(10,[1,2,3],R).
false.

?- selectchk(2,[1,2,3],R).
R = [1, 3].

?- selectchk(2,[1,2,3,2,4],R).
R = [1, 3, 2, 4].

?- selectchk(2,[1,2,3,2,4,2],R).
R = [1, 3, 2, 4, 2].
```
