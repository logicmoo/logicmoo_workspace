---+ How do I enlarge the stacks?

Old versions of SWI-Prolog had quite limited default stack-sizes. As of
version 5.10, there are no resources involved with unused stack-space
and the limits on 32-bit platforms are by default the maximum that can
be handled on these platforms: 128Mb. On 64-bit platforms the default is
256Mb, which typically means you have roughly the same limit as on
32-bit systems. However, on 64-bit platforms you can extend the limits.

---++ But, my program is too big. What now?

Prune choicepoints. Deterministic programs use way less memory on all
the stacks. Use the SWI-Prolog [[source-level debugger][../gtrace.txt]]
to find choicepoints.

---++ But I really have a lot of choicepoints and data

SWI-Prolog can handle that on 64-bit systems. As of version 5.10, the
limits can be modified at runtime using set_prolog_stack/2. The code
might looks like this:

  ==
  :- set_prolog_stack(global, limit(100 000 000 000)).
  :- set_prolog_stack(trail,  limit(20 000 000 000)).
  :- set_prolog_stack(local,  limit(2 000 000 000)).
  ==

This has the same effect as calling Prolog using the [command line
options](http://www.swi-prolog.org/pldoc/man?section=stacksizes)
below, except that the command line options set the initial stack
_limit_ for all threads, while set_prolog_stack/2 only affects the
calling thread.

  ==
  % swipl -G100g -T20g -L2g
  ==
