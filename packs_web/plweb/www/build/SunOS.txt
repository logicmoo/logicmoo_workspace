# Building SWI-Prolog on SunOS/SPARC (Solaris)

These are additional remarks to [the general build instructions](unix.txt) for SunOS. Test environment:

  | SWI-Prolog version | V6.3.18-26-ge58219b |
  | OS                 | SunOS 5.11 sparc    |
  | GCC                | 4.8.0               |

## Special settings

edit =build= (copied from =build.templ= and edit the following variables to the values given below:

  ==
  MAKE=gmake
  export CMFLAGS="-D_POSIX_PTHREAD_SEMANTICS"
  ==

## Notes

  - =|-D_POSIX_PTHREAD_SEMANTICS|= is need for getpwnam_r(), etc, which have 
    different calling conventions on SunOS without this flag.
  - Compiled with gcc 4.8.0, SWI-Prolog passes all tests. 
    *gcc 4.5.2 miscompiles* SWI-Prolog when using optimization.
    It compiles the system fine when adding the following to the =build= script.

    ==
    export COFLAGS=-O0
    ==

## Thanks

Petra Kaempfer from Humboldt University, Berlin for providing access to their machines.