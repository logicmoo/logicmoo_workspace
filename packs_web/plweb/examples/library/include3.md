# Use include/3 to filter odd numbers

```
is_odd(I) :-
    0 =\= I mod 2.
```

```
?- numlist(1, 6, List),
   include(is_odd, List, Odd).
List = [1, 2, 3, 4, 5, 6],
Odd = [1, 3, 5].
```
