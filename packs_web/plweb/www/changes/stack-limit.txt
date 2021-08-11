# Stack limit handling

Older versions of SWI-Prolog had individual limits for all three (local,
global and trail) stacks that were controlled using set_prolog_stack/2
as well as the command line options `-L`, `-G` and `-T`.

In recent version the stack limit is controlled by a single limit per
thread that is controlled by the Prolog flag `stack_limit` and the
command line argument =|--stack_limit=size|=, where `size` is a number
that is optionally postfixed with one of `BKMG` for _bytes_, _Kbytes_
_Mbytes_ or _Gbytes_.  Note that digit grouping may be used to write
large integers.  For example, setting the limit to 10Gb may use

    set_prolog_flag(stack_limit, 10 000 000 000).

Calls to set_prolog_stack/2 that set the limit are translated to setting
the Prolog flag and printing a _deprecated_ warning.

Note that using a flag implies that new threads inherit the limit from
the thread that creates the new thread. In old versions new threads
always started with the default limits or the limits provided at the
command line.
