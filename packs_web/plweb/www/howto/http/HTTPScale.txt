---+ Making your web-server scalable

Using SWI-Prolog 5.9.x, you can create web-servers that handle
significant numbers of concurrent clients. Older versions of SWI-Prolog
have severe limits on the number of _threads_ and scale much worse.

The concurrency model of the server is organised as follows.

    1. A single thread, called http@<port> accepts new connections.
    Once a connection is accepted, it is forwarded to a _|worker-pool|_.
    The size of this pool is controlled by http_workers/2 or using the
    workers(Count) argument of http_server/2:

	==
		...,
		http_server(http_dispatch,
			    [ workers(10)
			    ]),
	==

    2. A worker that receives a connection from the accept-thread reads
    and parses the HTTP request-header.  It is given a limited time to
    receive the header, so malicious clients that stop after sending
    an incomplete header are disconnected.  Next, it typically calls
    http_dispatch/1 to process the request.  Without any further
    precautions, the worker processes the entire request before it
    returns to the acceptor-queue for a new request.

---++ Allowing for a dynamic set of workers

In addition to the above model, a worker can _spawn_ a new thread to
complete the request while the worker returns to the acceptor-queue.
This can be achieved in two ways: by `hand' using http_spawn/2 or
centralised using the option spawn(+Spec) in the option-list of
http_handler/3. This mechanism can create threads in two ways: unbounded
or bounded by a dynamic pool. Unbounded creation carries the risk of
running out of resources.  Pool-bounded creation is therefore often the
way to go.

Pools are created using thread_pool_create/3. A pool has a name, maximum
number of workers and some additional parameters.  Using multiple pools,
we can create separate bounds for different tasks of the server, such as
CPU intensive tasks or tasks sending large files.  You do not want too
many concurrent users with CPU intensive jobs, while you may want many
concurrent users that merely download large files (an operation that can
take long if the connection is slow).  Here is a simple example with two
pools:

==
:- use_module(library(thread_pool)).

:- thread_pool_create(compute,  10, []).
:- thread_pool_create(media,   100, []).

:- http_handler('/solve', solve,       [spawn(compute)]).
:- http_handler('/files', serve_files, [spawn(media)]).
==

Attached, you find scale.pl.  The  file   is  commented  and handles the
following topics:

    * Using a thread-pool; use more default workers.
    * Set overall default options for handlers of a (sub-)hierarchy of
      server-locations
    * Create hyperlinks to call predicates
    * Fetching integer parameters using http_parameters/3 and providing
      a default.
    * Show how to create a web-page where intermediate results are
      printed as they become available.

If you run this demo, it is advised to run the thread-monitor.  This can
be started from the plwin.exe menu or by typing:

==
?- prolog_ide(thread_monitor).
==

*NOTE* thread-specific CPU-time statistics are _not_ provided on MacOS.
If you know a way to get this figure from the OS, please let us know.

@see Source: scale.pl
