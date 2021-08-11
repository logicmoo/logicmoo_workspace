# SWI-Prolog signal handling (Unix-like platforms)

SWI-Prolog requires one signal on Unix like systems (basically any
supported platform except MS-Windows).  It processes the signal
listed below.  The signal number of these signals is fixed, but it
would be quite trivial to allow changing these.

  $ SIGUSR2 :
  This signal is used by thread_signal/2 and related functionality
  to make blocking system calls return with *EINTR*, so the Prolog
  wrapper can check for pending signals and restart the blocking
  goal if all signals have been processed without error.

In addition, the common error signals such as SIGSEGV, SIGBUS, SIGFPE,
etc. are by default caught.  Upon receiving one of these signals, the
system tries to print useful information for debugging (depending on
the platform), performs its normal shutdown cleanup and stop.  Handling
these signals can be stopped using the =|--no-signals|= commandline
flags.

Finally, signal handlers may be installed using on_signal/3.

## Java JVM issues

As of SWI-Prolog 7.4 there is no conflict between SWI-Prolog and JVM
signal handling.

### Older versions of SWI-Prolog and JVM

Upto early versions of SWI-Prolog 7.3.x, SWI-Prolog processed *SIGUSR1*,
which conflicts with Java JVM signal handling (described by Oracle in
[this
document](http://www.oracle.com/technetwork/java/javase/signals-139944.html))
.

It seems that JVM and SWI-Prolog can cooperate by setting the
environment variable =_JAVA_SR_SIGNUM= to e.g. 20:

  ==
  _JAVA_SR_SIGNUM=20 swipl
  ?- use_module(library(jpl)).
  ==

See =man 7 signal=, the above Oracle documentation and documentation of
required foreign libraries to select a suitable signal to use for
=_JAVA_SR_SIGNUM=

*Acknowledgements*

The JVM issues were brought to my attention by Honnix Liang and Leonid
Mokrushin.
