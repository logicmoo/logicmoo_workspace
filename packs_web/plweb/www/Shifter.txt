---+ Development: Stack shifter

The =shift= branch of the =|pl-devel|= GIT repository contains the new
stack shifter. It was developed to reduce address-space and memory
requirements, notably for multi-threaded applications.

---++ Background

Since long, SWI-Prolog stack management was based on _|virtual memory|_
management: SWI-Prolog claims address-space upto the limit for each
stack and assigns physical memory as required. In the old days, with one
thread and an address-space that is much larger than the affortable
physical memory, this was fine. In todays 32-bit systems however,
address-space is more valuable than physical memory.  On 64-bit systems
address-space is generally larger than physical memory. Still, using
many threads, each of which potentially uses a lot of stacks (but most
do not) easily exhausts address space well before physical memory
becomes an issue.

SWI-Prolog 5.9.x (re-)introduces relocatable stacks (aka
_|stack-shifting|_): stacks are simply allocated and re-allocated using
the C-library malloc() and realloc() calls. There is a price. Using the
sparse virtual memory management, garbage collection was `requested'
long before running out of memory and executed at the first possible
`safe' location (typically the call-port).  The amount of stack needed
to get from one safe point to the next is hard to predict:  consider
arithmetic producing very large integers or unification of large terms
that require long wakeup-lists (coroutining) and/or many `trail' events.
With limited stacks, the previous schema fails.  The new implementation
deals with this in two ways:

  * Allow GC/shift in many more places.   Just about anywhere in the
    virtual machine and any C-function that accesses Prolog through
    the formal interface can invoke a GC/shift.

  * The few places where this is not feasible, a low-level routine
    is called that may return `out-of-stack'.  If so, the system
    backtracks, calls GC/shift and retries.

---++ Benefits

When stable, the new schema will enhance portability (just relying on
GC/shift rather than much harder to port virtual address-space
management), reduce both physical memory and address-space requirements
and -in particular-, provide much better support for applications that
wish to use many _threads_.


---++ Help debugging

One of the problems is that a lot of the code used to manage direct
pointers and was not designed to deal with GC/shift that changes these
pointers. Most of this is already fixed, but extensive testing is hard.
The places where these problems trigger depend on 32/64 bit systems and
the exact sequence of events.

To make quick progress, I'm particularly interested in problems on
64-bit platforms and more specifically in GCC-based systems (i.e.
64-bit Linux :-)  If you are interested, please edit pl/src/Makefile
and set

    ==
    COFLAGS=-gdwarf-2 -g3 -DSECURE_GC -fno-strict-aliasing
    ==

Run SWI-Prolog under GDB, using this =|.gdbinit|=

==
# GDB setup file for debugging SWI-Prolog

# Break on fatal errors and intended traps

set breakpoint pending on
break trap_gdb
break sysError
break fatalError
set breakpoint pending off

# Pass `normal signals'

handle SIGPIPE noprint nostop pass
handle SIGUSR1 noprint nostop pass
handle SIGUSR2 noprint nostop pass
set print thread-events off

# Run under the efence memory debugger

define ef
  set environment LD_PRELOAD=libefence.so.0.0
end
==

If the system crashes, trapped in an =abort= call, =trap_gdb= or a fatal
signal, examine the stack. You are looking for calls from
PL_next_solution() or one of the foreign-language implemented predicates
(often called pl_*) that lead to the crash. In particular, functions
processing direct pointers (type =Word=) that call GC/shift, often
indirectly.

Sometimes, the crash is caused by an earlier GC/shift that corrupted
the data.  You can get info in this using (1 means the latest GC/shift;
2 the one before, upto 10).

==
(gdb) call print_backtrace(1)
==

These stack-traces are addresses only, but gdb can list the code
the belongs to it using the command =|(gdb) list *<address>|=

If this means anything to you, you are possible able to indicate where
the problem is.  If not, sending the program and instructions on how
to reproduce helps a lot.
