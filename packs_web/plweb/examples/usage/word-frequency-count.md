# Counting word frequency

```
word_frequency_count(Words, Counts) :-
    maplist(downcase_atom, Words, LwrWords),
    msort(LwrWords, Sorted),
    clumped(Sorted, Counts).
```

```
?- word_frequency_count([a,b,'A',c,d,'B',b,e], Counts).
Counts = [a-2, b-3, c-1, d-1, e-1].

```

@see tokenize_atom/2 or split_string/4 may be used to split a text into tokens

