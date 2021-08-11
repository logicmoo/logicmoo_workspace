# Use append/3 for substitution in a list

The append/3 predicate can be used to split and join lists. We can
combine that to realise substituting all appearances of a sublist into
another as illustrated below.

  - `append(This, After, Rest)` creates a _pattern_ from `This`,
    i.e., the list `Rest` starts with `This`.
  - `append(Before, Rest, MyStr)` matches the pattern against the
    input list.  On a match we commit (!).  Now `Before` is the
    input list before the match and `After` is the input list
    after the match.
  - Recursively replace in `After`
  - Join the parts.


```
subst(This, That, MyStr, Result) :-
    append(This, After, Rest),
    append(Before, Rest, MyStr),
    !,
    subst(This, That, After, AfterResult),
    append([Before,That,AfterResult], Result).
subst(_, _, S, S).
```

```
?- portray_text(true).
true.

?- subst(`this`,`that`,`athishellothis`,R).
R = `athathellothat`.
```
