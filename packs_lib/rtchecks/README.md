rtchecks
========
Run-Time Checking of Assertions for SWI-Prolog

Installation
------------
To install the Run-Time checker of assertions, just follow the next sequence of
commands in your SWI-Prolog shell:

```bash
  $ swipl
  
  ?- pack_install('https://github.com/edisonm/rtchecks.git').
  true.
```

Based on the run-time checker of Ciao Prolog.

The semantic of run-time checks is explained in the paper, although is out of
date:

http://clip.dia.fi.upm.es/papers/assert-lang-disciplbook_bitmap.pdf

Overview
------------

This package provides a complete implementation of run-time checks of predicate
assertions. The program is instrumented to check such assertions at run time, or
during debugging, and any unsatisfied property is reported.

There are two main applications of run-time checks:

* __To improve debugging of certain predicates__, specifying some expected
  behavior that is checked at run-time with the assertions.

* __To avoid manual implementation of checks__ that should be done in some
  predicates, leaving the code clean and understandable.

The run-time checks can be configured using prolog flags.  Below we list the
valid prolog flags with its values and a brief explanation of the meaning:

* __rtchecks_level__

  * __exports__: Only use rtchecks for external calls of the
                         exported predicates.
  * __inner__  : Use also rtchecks for internal calls. Default.

* __rtchecks_status__

  Contains a list of the assertion statuses to be checked, could be a list of
  [true, false, check, debug, static]. Default value is [check, static].

* __rtchecks_static__

  Contains a list of the assertion statuses to be instrumented statically, could
  be a list of [true, false, check, debug, static]. Default value is [static].

* __rtchecks_entry__
  * __no__     : Disable rtchecks for entry assertions.
  * __yes__    : Enable  rtchecks for entry assertions. Default.

* __rtchecks_exit__
  * __no__     : Disable rtchecks for exit assertions.
  * __yes__    : Enable  rtchecks for exit assertions. Default.

* __rtchecks_abort_on_error__

  Controls if run time checks must abort the execution of a program
  (by raising an exception), or if the execution of the program have
  to continue.

  Note that this option only affect the default handler and the
  predicate @pred{call_rtc/1}, so if you use your own handler it will
  not have effect.

   * __yes__ : Raising a run time error will abort the program.
   * __no__  : Raising a run time error will not stop the execution,
                      but a message will be shown. Default.
