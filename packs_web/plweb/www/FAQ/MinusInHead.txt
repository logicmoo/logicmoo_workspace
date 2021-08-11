---+ [Warning: Clauses of (-)/2 are not together in the source-file

Novices, especially with a Lisp background assume that
this-is-an-identifier is a valid Prolog identifier. It is not. Using
the display/1 primitive, we can see that:

==
?- display(this-is-an-identifier).
-(-(-(this, is), an), identifier)
==

Of course, it is is prolog term and therefore no syntax error is raised.
Even for the most common operations such as test for equivalence or
`unify' the result is generally as expected. Some go wrong, such as
sorting or breaking the identifier into its characters (atom_chars/2).

Simple programs actually still work (although much slower).  
However, if you mix proper Prolog identifiers with identifiers with 
a minus, things go wrong:

==
bruce-lee.
godzilla.
chuck-norris.
==

Now you get the warning *|Clauses of (-)/2 are not together in the
source-file|* because there are two clauses of the term (-)/2 with one
of godzilla/0 in between. SWI-Prolog (any many others) anticipate you
might be re-using the same predicate name for a different definition and
warns you (if you want a `discontiguous' predicate, use the
discontiguous/1 declaration).

---++ What to do?

In Prolog, multi-word identifiers are written as this_is_an_identifier
or thisIsAnIdentifier. As we have variables and constants, it is good
and quite widely accepted practice to write

    * this_is_an_atom
    * ThisIsAVariable 
