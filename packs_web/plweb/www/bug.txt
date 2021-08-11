# What is a bug?

That is not so easy to define, but roughly these are bugs:

  * Inconsistencies between standards (both ISO and de-facto) and
    system behaviour.
  * Inconsistency between documentation and system behaviour (which
    might be a bug in the documentation).
  * Crashes.  Any reproducible crash is worth reporting.

*Non-experienced users are often surprised by how Prolog behaves.  In the
vast majority of cases this is not a bug. Use the
[forum/mailing list](https://swi-prolog.discourse.group) or
[stackoverflow](http://stackoverflow.com/questions/tagged/prolog) to
find answers.*

## What do I include in the bug-report?

Typically, a report must include the parts below to be meaningful.  This
is just a guideline, but if any of these parts are missing it is very likely that your report will not be answered at all or by additional questions.

  1. The version of SWI-Prolog you use and the operating system.
  2. A clear description on how to reproduce the issue.  Typically,
     this consists of:
     - A _complete_ program.  Please do not send merely a fragment
       because
         1. It costs us a lot of time to complete and
         2. we typically complete it differently and the problem doesn't reproduce.
       Small programs are better, but it isn't always worthwhile to spend
       effort in creating a small program.  If the program is big, do not
       include it in the first message, but merely describe that it is
       available and what it would take to obtain and run it.
     - The behavior you observe.  If there are warnings or error messages,
       include at least the first couple of them _literally_.
     - The behavior you expected.

## For Debian-specific problems

There is [[a Debian bug
tracker][http://bugs.debian.org/cgi-bin/pkgreport.cgi?pkg=swi-prolog]]
which informs the Debian maintainer.

## Where do I send my bug report

Please report it using [[our issue
tracker][https://github.com/SWI-Prolog/swipl-devel/issues]].
Notice that each package has a separate issues tracker, e.g., the
[[issue tracker of the HTTP
package][https://github.com/SWI-Prolog/packages-http/issues]], or
the [[issue tracker of the SSL
package][https://github.com/SWI-Prolog/packages-ssl/issues]].

Alternatively you can add a post to the
[forum](https://swi-prolog.discourse.group), preferably using the [bug
tag](https://swi-prolog.discourse.group/tags/bug).

Finally, for bug reports containing information you do not want to
become public, bugs can be sent by
[E-mail](<mailto:bugs@swi-prolog.org>)

@see [Raise an issue at GitHub](https://github.com/SWI-Prolog/issues/issues)
@see [Submit a patch](</howto/SubmitPatch.html>)
@see ["How to Report Bugs Effectively"](http://www.chiark.greenend.org.uk/~sgtatham/bugs.html) _by Simon Tatham, professional and free-software programmer_


