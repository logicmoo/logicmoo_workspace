# Can I replace a LAMP stack with SWI-Prolog?

Yes, you can, and you'll be happier for it.

[LAMP](http://en.wikipedia.org/wiki/LAMP_%28software_bundle%29) is short
for the following open source components to realise a web server:

  - Linux
  - Apache
  - MySQL
  - PHP

In this picture, [Linux](http://www.linux.org/) provides the OS,
[Apache](http://httpd.apache.org/) the web server,
[MySQL](http://www.mysql.com/) the database and [PHP](http://php.net/)
server-side scripting facilities. In fact, most of these components can
be replaced. One can replace Linux with almost any other OS, Apache with
[Nginx](http://nginx.com/), MySQL with
[PostgreSQL](http://www.postgresql.org/). PHP is similar to ASP.

There are also larger replacements of this stack, such as
[Tomcat](http://tomcat.apache.org/), which replaces both Apache and PHP.
Similar architectures are available for Python
([django](https://www.djangoproject.com/)), Ruby ([Ruby on
Rails](http://rubyonrails.org/)) etc. As we will see below, this is the
picture into which SWI-Prolog fits.

## The SWI-Prolog web framework

The SWI-Prolog web framework (obviously) does not replace the OS. It
does replace Apache, PHP and to some extent MySQL. We could refer to the
stack as _LP_ (Linux Prolog). Below, we point at the various libraries
that make up the stack and relate them to the LAMP components they
replace.

### Replacing Apache

The core web services are provided by the following libraries

  - library(http/thread_httpd)      -- Accept HTTP requests
  - library(http/http_authenticate) -- Basic HTTP authentication
  - library(http/http_openid)       -- OpenID based authentication
  - library(http/http_session)      -- Session management
  - library(http/http_ssl_plugin)   -- Provide SSL functionality (HTTPS)
  - library(http/http_cors)         -- CORS management
  - library(http/http_dispatch)	    -- Bind HTTP locations to actions
  - library(http/http_files)	    -- Serve plain files from the OS
  - library(http/http_parameters)   -- Read GET and POST form data

For development purposes, there are the following support libraries

  - library(http/http_log)	    -- Log traffic
  - library(http/http_error)        -- Present Prolog errors in the browser

### Replacing PHP

The previous section provides a basic web server that can serve static
pages from the filesystem and optionally does authentication, session
management, logging and error handling, i.e., Apache 0.1. The
library(http/http_dispatch) binds HTTP locations to Prolog predicates
that need to write a document to the `current_output` stream using the
CGI conventions, i.e., write the header followed by two blank lines and
the content. There are two high level libraries for generating dynamic
pages. None of them relates directly to PHP, but we consider that [a
good
thing](http://www.codinghorror.com/blog/2012/06/the-php-singularity.html).

  - library(http/html_write)        -- Our flagship for server site generated pages
  - library(http/http_pwp)	    -- Embed Prolog queries in xhtml pages

Anne Ogborn has written a good tutorial on how to [use the SWI-Prolog web framework](https://github.com/Anniepoo/swiplwebtut/blob/master/web.adoc)

#### Supporting AJAX

AJAX programming, i.e., initiating HTTP requests from JavaScript to
update pages locally without reloading the whole page, is supported by:

  - library(http/http_json)	    -- Read/write JSON documents
  - library(pengines)	            -- Use Prolog as a query language [[new.gif]]

## Replacing MySQL

There are several replacements for MySQL, depending on

  1. Persistence requirements
  2. Sharing requirements
  3. Size of the data

The alternatives are

  1. *Using the session (Prolog) database* This database associates
     Prolog terms with the current session.  This is generally suitable
     for keeping track of the status of the dialogue with the user.
     Restarting the server loses the data.
  2. *Using the dynamic Prolog database*
     Any Prolog programmer should know this.  Data stored in dynamic
     predicates is accessible across sessions.  The data can be
     highly dynamic and fast, but is lost on a server restart.
  3. *library(persistency) adds persistence to the Prolog database*
     Extends the dynamic database with wrappers to manipulate
     the dynamic predicates.  These wrappers manage a _journal_,
     which is restored after a server restart.  There is no overhead
     for read access.  The overhead for write access can be controlled
     by selecting the journal flushing regime.
  4. *library(semweb/rdf_db) provides (optionally persistent) RDF storage*
     Using the RDF store is similar to using the dynamic database or
     library(persistency), but enforces the use of the widely
     recognised RDF data model.  For example, you can make
     your data accessible through [SPARQL](http://www.w3.org/TR/sparql11-query/)
     using [ClioPatria](http://cliopatria.www.swi-prolog/)
  5. *library(odbc) provides a connection to RDMS systems*
     This is straightforward and avoids the famous [Object-relational
     impedance mismatch](http://en.wikipedia.org/wiki/Object-relational_impedance_mismatch)

# Developing and deploying your service

Services can be developed simply by running your application on your
laptop or desktop and directing your browser to http://localhost:<port>
as long as you use relative HTTP locations, preferably computed by
http_link_to_id/3 which computes an HREF based on the referenced
predicate on the local server and parameters. When ready for testing,
you can run the web server in a terminal and use an Apache proxy to make
it publically accessible from port 80. If problems occur, you can insert
debugging statements, trace the program, fix it and reload the source
without shutting down the server.  If all runs cleanly, there is this
library and accompanying (Debian) Linux init script for production:

  - library(http/http_unix_daemon) -- Run server as Unix daemon process

# Federating your services

The above assumes a single process server. What if one needs more power
or wishes to split responsibilities over multiple servers? Obviously,
you can use an external load balancing system to distribute the traffic
over multiple Prolog servers.  SWI-Prolog's session management is ready
to deal with the Apache server load balancing facility to keep sessions
on the same server.  What if you want the servers to communicate?

  - Use library(odbc) to share a common database
  - Use [TIPC](</pldoc/package/tipc.html>) to realise a cluster
  - Use library(pengines) to distribute queries

# Advantages of the _LP_ framework

Why would one choose the SWI-Prolog web programming framework over
LAMP, Tomcat, Django, Ruby-on-rails, etc.?

  1. Avoid the [Object-relational impedance
     mismatch](http://en.wikipedia.org/wiki/Object-relational_impedance_mismatch).
     This applies notably when accessing RDMS or RDF storage systems,
     but also dynamic Prolog predicates are way more easily queried than
     data stored in imperative languages. See also the [ClioPatria
     whitepaper](http://cliopatria.swi-prolog.org/help/whitepaper.txt).
     The library(pengines) provides any application with a clean
     modular API that is more elegant than SQL or SPARQL in a few lines
     of code.

  2. Stop _string programming_. [STRINGS ARE
     WRONG](http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm#strs).
     Virtually all processing in the _LP_ framework is based on proper
     parsing, term manipulation (Prolog's speciality) and serialization.
     [Quasi
     quotation](http://www.swi-prolog.org/pldoc/man?section=quasiquotations)
     support allows for _safe_ interpolation into e.g., HTML and
     JavaScript fragments embedded in the Prolog source code.

  3. Many applications need rules.  Prolog is very well suited for
     such tasks. A few rules are way easier to maintain than nested
     if-then-else statements.  External rule formalisms (e.g., RIF,
     SWRL) are easily compiled to Prolog.

  4. Quite a few problems are nicely expressed as constraint problems
     and no language mixes with CLP as easily as Prolog.  See
     library(clpfd) and library(chr).

  5. DCGs may not have proven to be the ideal for full blown parsing of
     NLP, but it is still a well integrated framework that can, due to
     its non-determinism, handle grammars elegantly.  If this doesn't
     suffice, there are various more powerful frameworks that integrate
     cleanly into Prolog.

  6. Prolog allows for hot-swapping code while data stored in dynamic
     predicates is retained.

  7. Programming in Prolog is fun!


# Disadvantages of the _LP_ framework

It is difficult to see principal technical disadvantages in the _LP_
framework. The main weakness of Prolog are algorithms that require
destructive assignment, intensive array processing or bare-metal
performance for e.g., processing pixels. Such requirements are rare in
the context of web programming. If necessary, such requirements can be
fulfilled using C/C++ plugins.

There is a weakness in scalability and robustness. Both are mostly
related to a small user basis and thus limited testing. Notably dealing
with unexpected high loads and DDoS attacks is not ironed out. This can
be remedied by using a proxy server that implements state-of-the-art
DDoS defence techniques, load regulation and load balancing. Slightly
more fundamental is that each request is processed by a Prolog thread.
This is also the case in Apache, but server architectures that aim at
really high traffic such as nginx process many requests using I/O
multiplexing.

Another problem is the (un)availability of server-side plugins to access
external APIs.  Some of this can be remedied by using JavaScript based
replacement technology on the client.  In other cases, our only counter
is to implement these plugins and share them, for example as an
[add-on](http://www.swi-prolog.org/pack/list).

Our final problem is the lack of masses of Prolog programmers one can
easily hire. Running critical Prolog applications is possible if you
invest time to become part of the Prolog community. You will get answers
to your questions and you will be able to hire someone to solve a
problem for you.


@see [Creating Web Applications in
SWI-Prolog](https://github.com/Anniepoo/swiplwebtut/blob/master/web.adoc) by
Anne Ogborn.
@see [How to create a web service
easily?](http://www.swi-prolog.org/howto/http/).
