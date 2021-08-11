---+ Representation and printing of floating point numbers

SWI-Prolog internally represents floats using the C-language type
double. On most today systems this implies using a 64-bit IEEE
representation. All floating point math functions are based on the C
math-library.

The write/1 predicate and friends use the =dtoa= library by _|David M.
Gay|_ that prints floating point numbers with the minimal number of
digits such that read/1 reads back the same value.  If you want floats
printed with a specific number of digits, use format/2:

==
?- A is pi, format('Pi = ~5f~n', [A]).
Pi = 3.14159
A = 3.141592653589793.
==

Floating point numbers are not exact. If you want exact arithmetic,
please check out SWI-Prolog's support for rational numbers in the
[[manual][</pldoc/man?section=arith>]].

Quoting Richard O'Keefe:

SWI Prolog uses exactly the same floating point arithmetic
as C.  This is normally provided by your hardware, and
can be expected to conform to the IEEE 754 standard.  This
is BINARY FLOATING-POINT arithmetic, not arithmetic on the
mathematical real numbers.  If you think you have found a
mistake in Prolog's arithmetic, you are almost certainly
wrong, and should make sure that you *understand why, for
example*,

    0.3*3 is not exactly equal to 0.9,

*before you report the "bug"*.

@see [What Every Computer Scientist Should Know About Floating-Point Arithmetic](http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
