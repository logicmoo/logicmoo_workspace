# Search for sub-atoms

To enumerate the 0-based starting indices of a possibly overlapping sub-atom:

```
?- sub_atom(banana, Pos, _, _, ana).
Pos = 1 ;
Pos = 3 ;
false.
```
