# SWI-Prolog future directions

The introduction of new features and new incompatibilities with the ISO
standard in SWI-Prolog version 7 has raised considerable concerns about
the future of SWI-Prolog. This page addresses some of these issues.  The
page is organised as questions and answers.

  - [What guides the development of SWI-Prolog?](<#guides>)
  - [Did SWI-Prolog give up on ISO compliance?](<#iso>)
  - [What about vendor lock in?](<#lockin>)
  - [How about SWI-Prolog and education?](<#edu>)


## What guides the development of SWI-Prolog? {#guides}

With SWI-Prolog, we want to provide a language that satisfies the needs
of academic and industrial *application programmers*. SWI-Prolog is a
dialect of Prolog because we believe that the logic foundation provides
a good basis for many relatively simple reasoning tasks seen in
applications. Its reflective capabilities as well as its `program is
data' view makes it an ideal platform for _domain specific languages_
(DSLs) or _micro languages_, which allows for concise description of
application knowledge and separation of this knowledge from how it is
applied. And, of course, Prolog provides a safe environment, free of
crashes and memory leaks.

SWI-Prolog does _not_ want to be an implementation that solves the
N-queens problems elegantly and in splendid isolation. Instead, we want
it to be a system that can operate as a component in modern IT
architectures. That is why it concentrates on multi threading, network
communication (sockets, HTTP, TIPC) and exchange of commonly used
structured documents (XML, JSON, RDF, CSV, etc.). That is also why it
supports relevant data types: Unicode text, big integers, rational
numbers and strings, as well as extended runtime detection of Prolog
data types that allows for a natural representation of dynamic data
while being able to distinguish a string from a list and the empty list
from an identifier without additional type information in the form of
declarations or wrapping data into terms.

With SWI-Prolog we want to maintain a _living language_. That means that
we will try to make the language evolve with insights and trends in the
IT world. Together with our aim to support _application_ programming,
this leads to the following priorities:

  $ Robustness and scalability :
  These should be obvious.  The first aim here is to ensure that
  properly debugged programs can run 24x7 reliably and without
  memory leaks.  This is more or less satisfied.  The second is
  to ensure that broken programs and development interaction
  (debugging, reloading, etc.) does not crash the system.  There
  is still work to be done here.

  $ (Backward) compatibility :
  We try to make as few as possible changes that break backward
  compatibility and stay as close as possible to the ISO standard
  (more about this below) and other Prolog systems (notably YAP).
  If incompatible changes are needed to enhance the compatibility
  or accommodate new extensions deemed necessary, we try to do this
  in such a way that (1) upgrading is relatively easy and (2) it
  is not hard to program in such a way that the code still runs
  on older versions or other Prolog implementations.


## Did SWI-Prolog give up on ISO compliance? {#iso}

Not (much) more than it used to. If you are looking for a Prolog system
that restricts you to the ISO standard, SWI-Prolog should not be the
first thing to look at. *ISO compliant programs* _|that do not explore
corner cases such as relying on specific behaviour on basically invalid
programs (e.g., expecting length(42,X) to fail silently)|_ *should run
fine*.

The ISO standard has done a great job in synchronising and cleaning the
syntax and core semantics of the language. However, the standardised
core is too small to accommodate real applications, the process to
enlarge is too slow (while some vendors do not even want to enlarge
it) and there are no mechanisms that allow us to make even tiny
incompatible changes needed to accommodate new features in a clean way.
Especially this last restriction turns it into a practically dead
language.

Below are a number of specific issues with the ISO standard that we
experience as counterproductive.


### Definition of error handling

ISO Prolog defines the behaviour of all built-in predicates both when
operating on arguments within the meaningful domain for the predicate
and when faced with illegal input, such as passing a non-list to the
first argument of length/2. It often even defines the precise error that
must be raised if the call is wrong for _multiple reasons_.  We consider
this counterproductive for the following reasons:

  - It puts an enormous stress on the ISO process itself.  For example,
    defining that length(List, Len) is _true when Len is the number of
    elements in the list List_ is easy.  Defining that this predicate
    is non-deterministic if List is a _partial list_ and Len is unbound
    is necessary.  Defining what happens on length(List, -1) is also
    necessary, because this this can be a goal resulting from a sensible
    program.

    However, it makes little sense to define what happens on length(42,
    Len). The ISO committee decided this must fail because it wanted
    implementations to allow rewriting e.g., length(List,5) into `List =
    [_,_,_,_,_]` and `42 = [_,_,_,_,_]` fails silently.  These debates
    are involved, trying to balance between usefulness of an exception,
    performance costs and implementation effort to do the required
    checking, possibilities for rewriting (as with length/2),
    consistency, etc.  There is no single truth here.

  - Defining the precise error or failure for basically invalid goals
    makes it hard for systems to comply because it assumes a certain
    implementation technique and order of execution.  For example,
    consider meta-calling, where ISO dictates that `call((fail,1))`
    must raise a type_error.  Here, SWI-Prolog complies
    because call/1 compiles the argument before execution.  Systems
    that prefer an opportunistic approach however will execute fail/0
    and never try to execute the invalid `1`.

    The precise error and failure conditions make it
    hard to perform program rewrites that now needs to maintain the
    exact behaviour on invalid input.  It also does not _allow_ for
    introducing exceptions in places where failure was prescribed
    because the costs of generating an exception was deemed to be
    too high by the ISO committee. It even disallows systems to reject
    goals like `Len is A+B, length(Len, List)` based on static analysis.

In the long run, we expect that SWI-Prolog will become a partially typed
language. Type systems have to choose between decidability and
expressiveness. Here, we plan to go for expressiveness by defining a
type, mode and determinism annotation that can capture the richness of
practical Prolog programming as we see it now. Based on that, we expect
analysis tools that proof _errors_ rather than _correctness_.  Some of
this is likely to be based on the [Ciao](http://ciao-lang.org/)
assertion language.


## Arithmetic

Being a logic based and dynamically typed language, Prolog should offer
_precise_ arithmetic results whenever possible. It should have unbound
integers and rational numbers at its core. Unbound integers are not
prescribed by the ISO standard and rational numbers are not even
mentioned. Arithmetic is defined almost as the C language defines it,
except that all overflow and evaluation errors must be mapped to
exceptions (a good idea). Typed languages have no choice but defining
that a specific operation returns a value of a specific type. Untyped
languages however can define that `X**Y` evaluates to an integer if this
represents the exact result and a rational number or floating point
number otherwise. ISO decided for two exponentiation operators (`**` and
`^`), where `^` evaluates to to an integer and raises an exception if
the result is not integral. We find this confusing. If you want `**` to
do floating point arithmetic, you can cast one of the arguments to
escape from the world of integers:

  ==
  ?- Exp is 2**float(2).
  Exp = 4.0
  ==

In the long run we expect a tighter integration of rational numbers.
This will involve integral division to be mapped to rationals and might
involve syntactical extensions to accommodate rationals.


## Representing text

ISO Prolog provides no sensible way to represent a string of characters.
In general, such data cannot be represented using atoms because systems
pose limits on the length of atoms, the characters that can be inside
atoms or the number of atoms or do not provide atom garbage collection.
There are two list representations for strings, one as a list of
_character codes_ and one as a list of _characters_ (atoms containing
exactly one character). Both representations are expensive and neither
can be distinguished at runtime from either a list of integers or a list
of atoms or the empty list. Without runtime type information on strings,
debugging becomes hard (should the debugger print `"ab"` as is or as
`[97,98]`?) and dynamic data structures cannot be created. The two
string representations suggest a choice, but in reality this choice
needs to be made for the whole application and is therefore not a real
choice.

The SWI-Prolog [extensions](</pldoc/man?section=extensions>) fix some of
these problems by reviving a string type as there was in the BSI Prolog
standard and which survived in several implementations (e.g., ECLiPSe,
Amzi!). We expect that YAP will follow. The primitives will be
synchronised with ECLiPSe.

In the long run, we might do something about the _chars and _codes
predicates, possibly by introducing a new data type _char_.


## Syntax

The ISO Prolog syntax has several flaws.

  $ Quoted atoms and strings :
  Here, we see several issues.  Long strings with good looking
  source layout is not supported because there is no syntax do
  concatenate strings (as in C, where "hello " "world" is identical to
  "hello world") and there is no escape sequence that ignores
  a newline and leading white space.  In addition, there is the
  idiosyncratic `\XXX\` notation for numeric character codes embedded
  in quoted atoms and strings, which is not compatible with older
  practice in Prolog, not with anything else and means nothing because
  it does not define how XXX must be interpreted.

  Unicode is there now long enough for SWI-Prolog to support the
  widely accepted `\uXXXX` and `\UXXXXXXXX`.  In addition, SWI-Prolog
  supports [quasi quotations](</pldoc/man?section=quasiquotations>),
  which can support pretty looking long strings as well as safely
  interpolate Prolog variables into source code fragments of
  external languages.

  $ Lack of support for commonly seen syntactical primitives :
  Expressions such as =|array[index]|=, `X.member`, `function()` or
  =|function(Arg) { Body }|= cannot be expressed in ISO Prolog.  This
  results in needlessly verbose and unnatural notations (see e.g.,
  library(record)) and makes it hard to define DSLs with a natural
  syntax.


## Should SWI-Prolog still be called Prolog? {#naming}

When introducing the [extensions](</pldoc/man?section=extensions>) in
version 7, several people have claimed that SWI-Prolog should choose a
new name that does not refer to Prolog, such as
[Picat](http://www.picat-lang.org/) or
[Mercury](http://mercurylang.org/) did. Both languages share concepts
with Prolog, but both differ so much that it is practically impossible
to run programs unmodified on both a Prolog processor and either Picat
or Mercury.

This is quite different for SWI-Prolog. Most `reasonable' programs that
satisfy the ISO standard or where designed for (especially YAP or
SICStus Prolog) run unmodified on SWI-Prolog or can be changed easily
to run on multiple systems.


## What about vendor lock in? {#lockin}

With SWI-Prolog we wish to maintain as good as possible compatibility
with ISO and other Prolog implementations in the `close family' (notably
YAP and SICStus and at somewhat larger distance Ciao, ECLiPSe and XSB),
while adding extensions to the system that supports our
[guiding principles](<#guides>)). In practice, this means that an
application programmer who experiences problems running the same source
on another Prolog system while this is not a priori impossible (for
example because of completely different feature sets, such as the
(un)availability of attributed variables) and there is no sensible
work-around will be taken seriously.

YAP and SWI-Prolog have a similar drive, where YAP concentrates on
performance and SWI-Prolog concentrates on development and stability.
YAP uses many of SWI-Prolog's packages and generally copies features
that are required to support these packages.

Within the ISO core, it is fairly cheap to switch or maintain a portable
application to just about any Prolog. If ISO doesn't satisfy your
requirements and you want to be able to switch, you should carefully
examine the language features you need and which systems are capable to
support these.

And, of course, SWI-Prolog is open source, so you are free to fork it
under the conditions of the license.


## How about SWI-Prolog and education? {#edu}

SWI-Prolog has always been used extensively in education.  The changes
introduced in version 7 do not make it significantly less suitable for
this purpose.  There are two issues that might require some attention.

  $ Double quoted strings :
  Courses that depend on the mapping of double quoted strings to
  lists of character codes must either switch the double_quotes
  flag, run using =|--traditional|== or use back quoted strings
  as illustrated in the calls below:

    ==
    ?- phrase("hello", "hello world", R).
    false.

    ?- phrase(`hello`, `hello world`, R).
    R = [32, 119, 111, 114, 108, 100].

    ?- phrase("hello", `hello world`, R).
    R = [32, 119, 111, 114, 108, 100].
    ==

  $ Lists can no longer be displayed using `.(A, .(B, []))` :
  The list functor was changed to `'[|]'`.  This can be made visible using e.g. =../2,
  but no longer using display/1 or write_canonical/1 which
  always use the list syntax.

    ==
    ?- [H|T] =.. L.
    L = ['[|]', H, T].
    ==

In the long run we would like to establish comprehensive tutorial
material for SWI-Prolog's extensions.


#### Acknowledgements

I would like to thank all people who constructively helped shaping
SWI-Prolog's recent extensions and expressed their concerns about
the directions taken.


@see The [SWI-Prolog extensions](</pldoc/man?section=extensions>)
