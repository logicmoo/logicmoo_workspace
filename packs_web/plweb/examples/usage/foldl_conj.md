# Convert a list of goals to a conjunction

> Note that the last two arguments [of foldl/4-7] form a typical
> difference pair, so at least when the predicate is just building data
> structure, it is common for the last argument to be the "input" (the
> trivial case of the data structure) and the second-to-last argument to
> be the "output" (the complete data structure).

(From ["An Elementary Prolog Library"](http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm)
by Richard O'Keefe)

```
flip_conj(Goal, (Goal, Conj), Conj).

?- foldl(flip_conj, [p, q, r], Conj, true).
Conj =  (p, q, r, true).
```

@see comma_list/2 for translating between a conjunction and a list.
