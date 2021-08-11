# Convert a string of 0s and 1s to an integer

This shows how to convert almost any "string" of 0s and 1s to an integer.

It takes advantage of the built-in term reading, thus avoiding an explicit
loop and arithmetic.

```
binary_string_to_int(Str, Int) :-
    (   ( string(Str) ; atom(Str) )
    ->  atomics_to_string(["0b", Str], Literal)
    ;   is_list(Str)
    ->  atomics_to_string(["0b"|Str], Literal)
    ;   type_error(string_type, Str)
    ),
    catch(term_string(Int, Literal, []),
	  error(syntax_error(_), _),
	  type_error(string_of_0s_and_1s, Str)).
```

```
?- binary_string_to_int('101', X).
X = 5.

?- binary_string_to_int([0, 0, 1, 0, 1], X).
X = 5.

?- binary_string_to_int(["1", "1", "0"], X).
X = 6.
```
