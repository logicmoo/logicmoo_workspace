---+ Warning: (File:Line): Singleton variables: [...]

This is a warning to help you with two common mistakes:

    * Spelling mistakes in variables
    * Forget to use/bind a variable 

It indicates that there is one or more variable in the clause that
appears only once. This is never necessary as the first appearance of a
variable always succeeds with a successful binding. If this binding is
not used anywhere, nothing happens. You may compare it to gcc's warning
``statement has no effect''.

But, what else do I place there? Prolog has the anonymous variable named
=|_|= for this purpose. This variable has `no name', unifies to anything
without any effect. If =|_|= appears multiple times in the same term,
they refer to _distinct_ variables.

But, how do I document what I ignore? Prolog systems won't complain on
variables that start with an underscore. Thus, the variable =|_Country|=
won't be reported if it is singleton. Note however that where two
appearances of =|_|= are distinct variables, two appearances of
=|_Country|= are not: they are the same variable.

But, the program I received has tons. What now? For this emergency there
is the directive style_check/1. The code below compiles silently.

==
:- style_check(-singleton).

better('SWI-Prolog', AnyOtherProlog?).
==

Note: changes to the style_check/1 options are reverted at the end of
the file the directive appears in. See also [[Syntax
Notes][</pldoc/man?section=syntax>]] in the reference manual.

