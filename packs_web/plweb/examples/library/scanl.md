```
?- scanl(plus, [1,2,3,4,5], 0, Sums).
Sums = [0, 1, 3, 6, 10, 15].
```

When considering the lists _columns_ of a table, scanl/5 combines the accumulator with each _row_ to produce the next value in the result list.  We illustrate this using plus/4 which adds the accumulator with the values from the row to produce the next accumulator.  We use trace/2 to illustrate how the values are combined.

```
plus(A,B,C,Result) :-
    Result is A+B+C.
```

```
?- trace(plus/4, exit).
%         plus/4: [exit]
?- scanl(plus, [1,2,3,4,5], [10,20,30,40,50], 0, Sums).
 T Exit: plus(1, 10, 0, 11)
 T Exit: plus(2, 20, 11, 33)
 T Exit: plus(3, 30, 33, 66)
 T Exit: plus(4, 40, 66, 110)
 T Exit: plus(5, 50, 110, 165)
Sums = [0, 11, 33, 66, 110, 165].
```
