---+ ERROR: Undefined procedure: (:-)/1 | (:-)/2 | (?-)/1

These messages are caused by common misunderstanding of the Prolog interactive toplevel.  Both (:-)/1 and (?-)/1 are used to specify _queries_.  If some
text reads

  ==
  ?- prove(X).
  ==

then the =|?-|= is the _prompt_ and should not be typed.  Repeating the
prompt results in:

  ==
  ?- ?- prove(X).
  ERROR: Undefined procedure: (?-)/1
  ERROR:   ?- is the Prolog prompt
  ERROR:   See FAQ at http://www.swi-prolog.org/FAQ/ToplevelMode.txt
  ==

Terms that you enter at the *toplevel* are processes as _queries_, while terms
that appear in a *file* that is loaded into Prolog is processed as a set of
_rules_ and _facts_.  If a text reads as below, this is a rule. 

  ==
  carnivore(X) :- animal(X), eats_meat(X).
  ==

Trying to enter this at the toplevel results in the error below.  Why?
Because a rule is a term :-(Head, Body), and because the toplevel interprets
terms as _queries_.  There is no _predicate_ with the name :- and two
arguments.

  ==
  ?- carnivore(X) :- animal(X), eats_meat(X).
  ERROR: Undefined procedure: (:-)/2
  ERROR:   Rules must be loaded from a file
  ERROR:   See FAQ at http://www.swi-prolog.org/FAQ/ToplevelMode.txt
  ==

Isn't this stupid?  Well, no.  Suppose we have a term =|eats_meat(rataplan)|=.
If this appears in a file, it states the _fact_ that =rataplan= eats meat.  If
it appears at the toplevel, it asks Prolog to try proving whether rataplan
eats meat.

If a text reads

  ==
  :- use_module(library(clpfd)).
  ==

This is a _directive_.  Directives are similar to queries, but instead of
asking the toplevel to do something, they ask the _compiler_ to do something.
Like rules and facts, such terms belong in files.

---++ Can I add rules and facts from the toplevel?

Yes.  There are two ways.  One is to consult =user=, as illustrated below.
Where is says =|<EOF>|=, you must type the character for end-of-file, which
is Control-D on most systems.

==
?- [user].
|: carnivore(X) :- animal(X), eats_meat(X).
|: <EOF> % user://1 compiled 0.00 sec, 880 bytes
true.
==

The second way is by using assert/1.  Note that these two mechanisms do not
mix for the same predicate.  See DynamicCode.txt.

==
?- assert((carnivore(X) :- animal(X), eats_meat(X))).
==

We advice to use an editor to make a file with rules and load this using
the command below.  Prolog files use the extension =|.pl|=, which you do
not have to specify for loading the file.  You may use any other extension,
but this requires you to add the extension and quotes to turn this into
valid Prolog syntax.  For example: =|?- ['myfile.txt'].|=.

==
?- [myfile].
==

Note that you can easily update your rules by editing the file and typing
the command below.  The make/0 predicate reloads all files that have been
modified, _replacing_ old rules loaded from that file with the current
content of the file.

==
?- make.
==

---++ Can I put queries in files?

Yes.  As we have seen above, there are _directives_ for the compiler that
are written as =|:- Term.|=.  Here, Term must be recognized as a valid
directive by the compiler (SWI-Prolog doesn't enforce this, but some other
Prolog implementations do).  You can specify arbitrary Prolog code that
is executed _after_ loading the file using e.g.,

==
:- initialization
   format('Hello world!~n').
==
