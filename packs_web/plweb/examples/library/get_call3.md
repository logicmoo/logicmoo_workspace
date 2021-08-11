# Enumerate the content of a table

```
:- table c/2.

c(F,T) :- c(F,I), c(F,T).
c(F,T) :- c(T,F).

c(a,b).
c(b,c).
c(e,f).
```

Now populate the table for `c(a,T)`:

    ?- c(a,T).
    T = a ;
    ...

Now you can enumerate the table's content using get_call/2 and get_returns/3:


```
?- get_call(c(a,T), Trie, Template),
   get_returns(Trie, Template).
T = a ;
...
```

Note that this gives the same results as just calling `c(a,T)`. The table
inspection predicates are mostly used for debugging.
