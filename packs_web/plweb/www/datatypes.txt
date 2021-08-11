# SWI-Prolog datatypes

This document lists the SWI-Prolog   datatypes  and relevant information
such as their limits. It is mostly   intended  for programs that want to
examine and/or exchange arbitrary SWI-Prolog data.

## Classical Prolog types

This section discusses the types that are found in any Prolog system.

### Variable

Prolog variables are _logical_ variables. Variables  do not have a fixed
identity, such as a name or  a   permanent  address.  In particular, the
printed `name' of a variable  may   *change  due to garbage collection*.
This is illustrated in the session below:

  ==
  ?- A = x(X), writeln(X), garbage_collect, writeln(X).
  _G1600
  _G1
  A = x(X).
  ==

### Integer

Prolog integers are by default _unbounded_.   Large integers live on the
_global_ (or _term_) stack and thus the   maximum integer is dictated by
the available stack space. If SWI-Prolog was build without large integer
support, they are  represented  as  64-bit   integers,  also  on  32-bit
hardware. From a Prolog point of  view,   all  integers are uniform. The
foreign language interface provides functions   to  exchange integers in
various formats.

### Floats

Prolog floats are represented as C doubles. In practice, this means they
are 64-bit IEEE doubles on all relevant platforms we are aware of.

### Atoms

Prolog atoms are unique handles to a string of Unicode characters. There
is no length limit and all Unicode   code points, including zero (0) are
allowed. The maximum number of  atoms  that   can  be  accommodated in a
single Prolog process is 2^25 (33554432)   on  32-bits machines (2^57 on
64-bit machines).

*Windows*: Unicode atoms are stored using the C-type `wchar_t`, which is
16 bit on Windows (32-bit on  most   other  platforms).  This means that
Unicode  strings  can  only  hold  characters  in  the  range  0..65535.
SWI-Prolog is not aware of  UTF-16   _surrogate  pairs_. Therefore, both
members of the pair count as distinct characters to atom_codes/2, etc.

### Compound terms

Compound terms consist of a name  (atom),   and  _N_  arguments, each of
which may be an arbitrary Prolog data   item. In standard Prolog, _N_ is
at least one. In SWI-Prolog, _N_ can be zero. The number of arguments is
limited to 2^32,  which  implies  limited   by  memory  only  on  32-bit
hardware, but limited on 64-bit hardware. Note   that  this limit can be
lifted fairly easily if the need arises.


## SWI-Prolog extensions

SWI-Prolog provides the following extensions   and  modifications to the
standard Prolog datatypes:


### Blobs

Blobs are a super-type of atoms. Two   blob  types implement normal text
atoms: =text= (ISO-Latin-1 atoms) and   =ucs_text=  (Unicode atoms). The
type test atom/1 succeeds only on these   two.  The other blob types are
typically used as handles.  Currently, the following blob types exist:

    $ stream :
    Used to represent handles to streams
    $ clause :
    Used to represent handles to clauses (asserta/2, etc.)
    $ record :
    Used to represent handles to database records (recorda/3, etc.).
    $ reserved_symbol :
    Reserved symbols.  Currently only [].

Future versions are likely to introduce more blobs, notably to reference
global objects such as threads or   mutexes. Note that foreign libraries
may define additional blob types,  so  code   that  wish  to  be able to
process arbitrary Prolog terms  should  not   make  assumptions  on  the
defined blob types.

### Strings

As atoms, strings are sequences of Unicode   code  points. They have the
same limits as atoms, except for the  maximum length. As strings live on
the global stack, their lenght is limited  by the available stack space.
In addition, the  size  of  a  string   in  memory  is  limited  to 2^25
(33554432) on 32-bits machines or  2^57   on  64-bit machines. Note that
ISO-Latin-1 strings require one byte per   character and unicode strings
require 4 (2 on Windows, see _Atoms_ above) bytes per character.

### Nil ([], empty list)

In SWI-Prolog version 7, `[]` *is not an  atom*. It is a unique constant
that is only equivalent  to  itself.   The  foreign  interface  provides
dedicated  functions  to  deal  with    the   empty  list  (PL_is_nil(),
PL_unify_nil(), etc.)

### List cons-cell

In  SWI-Prolog  version   7,   the   list    _cons-cell_   is   a   term
`'[|]'(Head,Tail)` rather than `.(Head,Tail).`

### Dictionaries

SWI-Prolog version 7 provides the _dict_   type.  See [Dicts: structures
with                                                               named
arguments](<http://www.swi-prolog.org/pldoc/man?section=dicts>)      for
details. Dicts are not yet supported   by the foreign library interface.
Dicts are currently a _subtype_ of  _compound_, which implies that e.g.,
compound/1, =../2, etc.,  work  on  them.   User  code  should  not make
assumptions on this. Notably code must test   for  dicts and process the
dict with dict predicates, before testing for compounds.
