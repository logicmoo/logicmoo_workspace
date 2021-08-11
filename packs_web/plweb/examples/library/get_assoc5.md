Normally used to update the value associated with a key. For example, if the value is a count that must be incremented, do

```
inc_count(Key, A0, A) :-
     get_assoc(Key, A0, C0, A, C), !,
     C is C0+1.
inc_count(Key, A0, A) :-
     put_assoc(Key, A0, 1, A).
```
