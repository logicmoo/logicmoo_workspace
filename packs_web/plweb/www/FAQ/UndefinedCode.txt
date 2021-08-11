---+ ERROR: Undefined procedure: Name/Arity

There are three common things people use that produce this error while
they do not realise they made a mistake.

---++ No facts have yet been asserted, but they will be

Sometimes you want to change a predicate at runtime using assert/1 and
retract/1, but you call the predicate before the asserting the first
clause. Prolog doesn't know about your intend and reports an error. It
is good practice to define every predicate you want to manipulate as
dynamic:

==
:- dynamic
	is_a/2,
	person/1.
==

---++ You removed all clauses of a dynamic predicate using abolish/1

abolish/1 forgets everything Prolog knows about the predicate, including
the fact that was dynamic. All clauses should be removed using
retractall/1.  See also DynamicCode.txt.

---++ You enter your program at the prompt

SWI-Prolog (in fact almost any Prolog system) interprets terms typed to
the ?- prompt as queries and wants to prove (i.e., run) them. The
preferred way is to use an editor to create a file and load this into Prolog
by putting it between square brackets (see also LoadProgram.txt).

==
?- [myfile].
==

If you insist on typing your program at the prompt, consult =user= using
the sequence below. Instead of the literal end_of_file term, you can
also type the end-of-file character of your system. This is often
Control-D.  Note that in `user consult' mode, the prompt is changed
from *|?-|* into *||:|*.

==
?- [user].
|: likes(mary, john).
|: end_of_file.
% user compiled 0.00 sec, 344 bytes 
==
