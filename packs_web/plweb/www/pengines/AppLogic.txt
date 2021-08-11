# Adding application logic to a Pengine server

Any SWI-Prolog web server can be pengine enabled by loading
library(pengines) and configuring the access rights. Clients can then
create pengines that execute code in the module =pengine_sandbox=. By
default, they can only execute white-listed code as defined by the
library(sandbox), i.e., mostly Prolog code that has no side effects.

Typically, a server serves a particular application.  This application
comes with a particular API. A nice and clean example is the RDF library
as realised by library(semweb/rdf_db).  If the pengine server wishes to
make this library available to pengines, it must import this library
into the module =pengine_sandbox= using the following query:

  ==
  :- use_module(pengine_sandbox:library(semweb/rdf_db)).
  ==

This declaration allows pengines to use the side effect free RDF query
predicates, in particular rdf/3.

An application that wishes to provide similar functionality should take
the following steps:

  1. Define a module that exports the desired API.  Remember that the
     client can easily add predicates that combine API calls and massage
     data into a format suitable for the application, so you don't have
     to think about these application oriented queries. Instead, make
     your queries general and purely declarative!

  2. Import this library into =pengine_sandbox= as illustrated above.

That is all, *except that library(sandbox) may not like your code*. What
now?  There are several reasons and work-arounds.

  $ The sandbox library does not understand my code :
  This shows up as _instantiation errors_ and implies that the
  sandboxing libraries finds meta-calls but cannot determine what
  will be called.  There are two remedies:

    - Make the source easier to track. Avoid some meta-calling by
      passing terms that indicate what should happen and a static
      predicate that interprets these terms.  This is often a good
      idea if the intend is to meta-call to a well defined set of
      predicates.

    - Isolate the most deeply nested bit of code that is safe,
      *regardless how it is called* and declare this to be safe
      using sandbox:safe_primitive/1 or, if it is a meta-predicate,
      using sandbox:safe_meta/2.

  $ The API calls foreign code :
  Library(sandbox) cannot prove anything about foreign code.  Make sure
  it is written properly and declare it using sandbox:safe_primitive/1.

  $ The API has side effects, but that it what it should do :
  Make sure that the API correctly validates arguments and really
  cannot do anything unintended and declare it using
  sandbox:safe_primitive/1.
