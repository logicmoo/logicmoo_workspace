
LOGICMOO's AtomSpace Blackboard Server (LABS)
===========================

opencog | singnet
------- | -------
xxxxxxx  | xxxxxxx

The LOGICMOO's AtomSpace Blackboard Server (LABS) is a CogServer Emulator that exposes a network scheme/prolog command-line the lets you run Logicmoo tools from inside the [OpenCog framework](https://opencog.org).  This provides a simple, straight-forward backend for networking together multiple AtomSpaces with Logicmoo so that they can share data. There is no particular limit, other than that of bandwidth, response-time, etc. 

 In ASCII-art:

```
 +-----------+
 |           |  <---internet-->  *LABS*
 |  Server A |                      ^  ^
 |           |        +-------------+  |
 +-----------+        v                v
                 +----------+   +-----------+
                 |          |   |           |
                 | Server B |   |  Server C |
                 |          |   |           |
                 +----------+   +-----------+
```

Prerequisites
-------------
To run the logicmoo_cogserver, you need to install the SWI-Prolog first.

```bash
apt install swi-prolog-nox
```

Optional
-------------
Logicmoo's Common Logic Interchange Format logicmoo_clif

```prolog
?- pack_install('https://github.com/logicmoo/logicmoo_clif.git').
true.
```


# Installation

Using SWI-Prolog 7.1 or later:

```prolog
?- pack_install('https://github.com/logicmoo/logicmoo_cogserver.git').
true.
```


Using
-----
There are multiple ways to start the logicmoo_cogserver:

* From bash, just start the process:
```bash
$ ./prolog/logicmoo_cogserver.pl`
```
 or
* From prolog, load and start the process:
```prolog
?- use_module(library(logicmoo_cogserver)).
true.

?- start_cogserver.
%~ 21001="CogServerShell".
true.

``` 

Once started, one can obtain a shell by saying `rlwrap telnet localhost
12101`, and then `pl` or `scm` to obtain a prolog or scheme shell.  This
can be done as many times as desired; all shells share the same
AtomSpace, and the system is fully multi-threaded/thread-safe.

The `rlwrap` utility simply adds arrow-key support, so that up-arrow
provides a command history, and left-right arrow allows in-place editing.
Note that `telnet` does not provide any password protection!  It is
fully networked, so you can telnet from other hosts. The default port
number `21001` can be changed; see the documentation.

- LOGICMOO Support Channel (Discord Invite Link)  https://discord.gg/JREW7F2
- LOGICMOO Telegram at [https://t.me/LogicMoo](https://t.me/LogicMoo)


## known issues

- Not all of `scm` shell is supported for now. There are plans to extend this support to other offered shells in the future.
- The logicmoo agent/logic system can be very hard to install and is optional from here
- Document this pack!
- Untangle the 'pack' install deps (Moving predicates over here from logicmoo_base)


## licensing information

This package, like the most of OpenCog packages, is licensed under [AGPL-3.0 license](LICENSE).

## The API

There are fifteen functions for working with storage or a remote AtomSpace. These include opening and closing a connecting, sending and receiving individual Atoms, sending receiving them in bulk, and performing precise, selective queries to get only those Atoms you want.

The names given below are the  [scheme](https://wiki.opencog.org/w/Scheme "Scheme")  names for these functions; the equivalent C++ and  [Prolog](https://wiki.opencog.org/w/prolog "prolog")  names are almost the same, using an underscore instead of a dash.

The methods are:

-   `cog-open`  -- Open a connection to the remote server or storage.
-   `cog-close`  -- Close the connection to the remote server/storage.
-   `fetch-atom`  -- fetch all of the  [Values](https://wiki.opencog.org/w/Value "Value")  on an  [Atom](https://wiki.opencog.org/w/Atom "Atom")  from the remote location.
-   `fetch-value`  -- fetch the single named Value on the given Atom. Handy if the Atom has lots of values on it, and you don't want to waste time getting all the others.
-   `store-atom`  -- store all of the Values on the given Atom. That is, send all of the Values to the remote location.
-   `store-value`  -- store just the single named Value on the given Atom. Handy if you don't want to change the other Values that the remote end is holding.
-   `cog-delete!`  -- Delete the Atom in the current AtomSpace, and also the attached storage. The Atom will  _not_  be deleted, if it has a non-empty incoming set.
-   `cog-delete-recursive!`  -- Delete the Atom in the current AtomSpace, and also the attached storage. If the Atom has a non-empty incoming set, then those Atoms will be deleted as well.
-   `load-atomspace`  -- get every Atom (and the attached Values) from the remote storage, and place them into the current AtomSpace.
-   `load-atoms-of-type`  -- get every Atom that is of the given type. This loads the Values on those Atoms as well.
-   `store-atomspace`  -- dump the entire contents of the current AtomSpace to the remote server/storage.
-   `fetch-incoming-set`  -- get all of the Atoms that are in the incoming set of the indicated Atom.
-   `fetch-incoming-by-type`  -- get all of the Atoms that are in the incoming set, and are also of the given type. Very useful when an Atom has a very large incoming set, and you want only a specific subset of it.
-   `fetch-query`  -- run the indicated query on the remote server/storage, and get all of the results of that query. You can use  [BindLink](https://wiki.opencog.org/w/BindLink "BindLink"),  [GetLink](https://wiki.opencog.org/w/GetLink "GetLink"),  [QueryLink](https://wiki.opencog.org/w/QueryLink "QueryLink"),  [MeetLink](https://wiki.opencog.org/w/MeetLink "MeetLink")  and  [JoinLink](https://wiki.opencog.org/w/JoinLink "JoinLink")  to formulate queries. Note that the query has a built-in caching mechanism, so that calling  `fetch-query`  a second time may return the same cached query results. That cache can be explicitly cleared, so that a second call re-runs the query. This is handy when queries are large and complex and consume lots of CPU time. There is an experimental time-stamp mechanism on the cache.
-   `barrier`  -- Force all prior network or storage operations to complete (on this particular StorageNode) before continuing with later operations.

Detailed and precise documentation can be displayed at the scheme and Prolog command prompts. For example, typing  `,describe fetch-atom`  at the guile prompt will provide the man page for that function (note the comma in front of the "describe". You can shorten it to  `,d`). Something similar gets you the  [Prolog](https://wiki.opencog.org/w/prolog "prolog")  documentation.

All of the above functions are provisional: they have been working and supported for quite a long time, but they should probably be replaced by pure  [Atomese](https://wiki.opencog.org/w/Atomese "Atomese"). That is, there should probably be a  `FetchAtomLink`  so that running  `([cog-execute!](https://wiki.opencog.org/w/Cog-execute! "Cog-execute!")  (FetchAtomLink ...))`  does exactly the same thing as calling  `fetch-atom`. This would be super-easy to do; it hasn't been done, because no one has asked for it yet.

The above API is implemented here:  [opencog/persist/api](https://github.com/opencog/atomspace/tree/master/opencog/persist/api).

## Creating new implementations


The  **CogServer**  is a network server for the  [AtomSpace](https://wiki.opencog.org/w/AtomSpace "AtomSpace"). It provides three different services, tied fairly closely together. These are a telnet server to an OpenCog command line, a telnet server to  [scheme](https://wiki.opencog.org/w/Scheme "Scheme")  and  [Prolog](https://wiki.opencog.org/w/prolog "prolog")  command lines, (which can be used at the same time, for the same AtomSpace!) and a high-performance  [Atomese](https://wiki.opencog.org/w/Atomese "Atomese")  network server. It is straight-forward to extend the CogServer with network services of this kind.

Most of this wiki page will be devoted to describing the high-performance network server. It is fairly important to understand, as it provides a basic building block for building a  [distributed, decentralized AtomSpace](https://wiki.opencog.org/w/Distributed_AtomSpace "Distributed AtomSpace"). It provides the basic peer-to-peer networking layer, by means of the  [CogStorageNode](https://wiki.opencog.org/w/CogStorageNode "CogStorageNode").

Some comments about network serving in general, including other systems, such as RESTful API's, are given at the bottom of this page.

## Contents

-   [1  Overview](https://wiki.opencog.org/w/CogServer#Overview)
-   [2  Implementation](https://wiki.opencog.org/w/CogServer#Implementation)
-   [3  Using the CogServer](https://wiki.opencog.org/w/CogServer#Using_the_CogServer)
-   [4  High-performance Atomese serving](https://wiki.opencog.org/w/CogServer#High-performance_Atomese_serving)
    -   [4.1  Atomese and S-Expressions](https://wiki.opencog.org/w/CogServer#Atomese_and_S-Expressions)
    -   [4.2  The sexpr shell](https://wiki.opencog.org/w/CogServer#The_sexpr_shell)
    -   [4.3  The sexpr commands](https://wiki.opencog.org/w/CogServer#The_sexpr_commands)
    -   [4.4  sexpr usage examples](https://wiki.opencog.org/w/CogServer#sexpr_usage_examples)
    -   [4.5  The sexpr implementation](https://wiki.opencog.org/w/CogServer#The_sexpr_implementation)
-   [5  Other systems](https://wiki.opencog.org/w/CogServer#Other_systems)
    -   [5.1  Network serving ruminations](https://wiki.opencog.org/w/CogServer#Network_serving_ruminations)

## Overview

The components are:

-   A telnet server providing access to a command-line shell. A plugin system allows new commands to be added in a pluggable manner. This shell is mostly unused at this time; it does offer a command-line interface to the  [AgentServer](https://wiki.opencog.org/w/AgentServer "AgentServer").

-   A telnet server providing multi-user access to a  [scheme](https://wiki.opencog.org/w/Scheme "Scheme")  REPL shell, and a  [Prolog](https://wiki.opencog.org/w/prolog "prolog")  REPL shell. (REPL stands for "Read Evaluate Print Loop"). These shells are very useful for starting, stopping and managing long-running AtomSpace jobs. For example, one terminal can be used to start a big job, while a second terminal can be used to monitor its progress. Both the scheme and the Prolog shells can be used  _at the same time_, so that, for example, an  [Atom](https://wiki.opencog.org/w/Atom "Atom")  created with prolog is instantly visible to scheme, and vice-versa. This is particularly useful when interfacing to  [ROS (Robot Operating System)](http://www.ros.org/), which is primarily developed in Prolog. The scheme shell is particularly efficient for moving Atoms across the network.

-   A high-speed  [Atomese](https://wiki.opencog.org/w/Atomese "Atomese")  server. This is a small subset of the scheme shell, but very highly tuned for maximum performance for moving around Atoms on the network. We believe that it is (very nearly) impossible to beat the performance of this component, using any other technology, including ZeroMQ, RESTful API's, Protocol Buffers, JSON servers, and so on. The reason for this is that Atomese is easy to decode, as it has a regular structure: it can be done with string compares. Running Atomese is also extremely fast: it runs at the speed of C++ constructors. There is very little overhead, almost nothing to trim.

The high-speed Atomese server has been used to implement the  [CogStorageNode](https://wiki.opencog.org/w/CogStorageNode "CogStorageNode"), which uses the CogServer to move Atoms around on the network, point-to-point, from within the AtomSpace. A conventional Atomspace job can use the CogStorageNode to easily fetch, send and query Atoms (the full query mechanism--  [BindLink](https://wiki.opencog.org/w/BindLink "BindLink")  and friends, are supported. Basically, you can run BindLink over the network.) The protocol for accomplishing this is documented below.

The  [CogStorageNode](https://wiki.opencog.org/w/CogStorageNode "CogStorageNode")  can in turn be used as a building block to build a  [distributed, decentralized AtomSpace](https://wiki.opencog.org/w/Distributed_AtomSpace "Distributed AtomSpace"). It provides the basic peer-to-peer communications layer.

## Implementation

The source code for the CogServer can be found in the git repo here:  [https://github.com/opencog/cogserver](https://github.com/opencog/cogserver)

Additional shells (i.e. shells for things other than Prolog or scheme) can be implemented by modelling existing code in this directory:  [https://github.com/opencog/cogserver/opencog/cogserver/shell](https://github.com/opencog/cogserver/opencog/cogserver/shell)

## Using the CogServer

Please refer to the  [OpenCog shell](https://wiki.opencog.org/w/OpenCog_shell "OpenCog shell")  page for general info about how to start the CogServer, and how to connect to the telnet shell, and thence the Prolog or scheme shells. The same menu also provides the  sexpr  shell, descried below.

## Atomese serving

This section reviews the so-called  sexpr  shell: this shell handles a small handful of special commands that generally take  [Atomese](https://wiki.opencog.org/w/Atomese "Atomese")  s-expressions as arguments. A few also take  #t  and  #f  as booleans, emulating the scheme API for these same functions.

#### Atomese and S-Expressions

[Atomese](https://wiki.opencog.org/w/Atomese "Atomese")  is a simple language for writing down  [Atoms](https://wiki.opencog.org/w/Atom "Atom")  as  [s-expressions](https://en.wikipedia.org/wiki/S-expression). An s-expression is achieved with nested parenthesis. Most examples on this wiki are written this way: for example

([EvaluationLink](https://wiki.opencog.org/w/EvaluationLink "EvaluationLink") ([PredicateNode](https://wiki.opencog.org/w/PredicateNode "PredicateNode") "enjoys")
     ([ListLink](https://wiki.opencog.org/w/ListLink "ListLink") ([Concept](https://wiki.opencog.org/w/Concept "Concept") "Linas") (Concept "being outdoors")))

Such expressions can be parsed easily enough by performing string searches. Parsing such expressions is not quite as fast as some binary format, but they are eminently human-readable, making them quite nice for general code development, ad hoc usage, debugging. They are easy to print. Most importantly, they can be processed on the server side with relative ease. They can certainly be process much faster than what the guile shell could ever go, or what an equivalent Prolog shell could do.

#### The  sexpr  shell

The  sexpr  can be accessed via telnet, for a manual session, or directly via TCP/IP sockets, for programmatic use. In both cases, the access method is exactly the same: connect to the desired hostname (tcpip address) and port number 17001. (This port number is dynamically configurable, at the time that the CogServer is started.) Enter the  sexpr  shell by sending the ascii (utf-8) string "sexpr\n" (without the quotes; the \n is the newline character.) After sending this string, the socket will be in  sexpr  mode. Nothing will be sent back, unless one of the  sexpr  commands generates output. The  sexpr  can be gracefully exited by sending a control-D (hex 0x04) or a single '.' on a line by itself (that is, a period (0x2E) followed by a newline (0x0A).

#### The  sexpr  commands

The following commands are available. They behave more-or-less exactly the same as their ordinary scheme counterparts. Therefore, using the  ,describe  command at the guile prompt will provide documentation.

-   cog-atomspace-clear
-   cog-execute-cache!
-   cog-extract!
-   cog-extract-recursive!
-   cog-get-atoms
-   cog-incoming-by-type
-   cog-incoming-set
-   cog-keys->alist
-   cog-link
-   cog-node
-   cog-set-value!
-   cog-set-values!
-   cog-set-tv!
-   cog-value

Again, many of these commands are relatively silent; they will not return any bytes, if they don't need to. Syntax errors can be discovered by doing  tail -f /tmp/cogserver.log.

At this time, these are the only supported commends.

#### sexpr  usage examples

To transmit a single Atom to the cogserver, use the  cog-set-tv!  command. For example:

`(cog-set-tv! (ConceptNode "foo") (SimpleTruthValue 0.3 0.6))`

To list the Atoms in the AtomSpace:

`(cog-get-atoms 'Atom #t)`

To list only the ConceptNodes:

`(cog-get-atoms 'ConceptNode #t)`

More complex data transfers can be accomplished by combining these primitives.

Please note that the  cog-execute-cache!  command is rather dangerous: it will execute any executable Atom. The result of execution is cached at the provided key; this result can be fetched by using  cog-value  with the same atom and key.

#### The  sexpr  implementation

The implementation of these commands is  _not_  in the CogServer, but in the base Atomsapce git repo. The actual decoding can be found here:  [opencog/persist/sexpr/Commands.cc](https://github.com/opencog/atomspace/tree/master/opencog/persist/sexpr/Commands.cc). That is, the  sexpr  shell is just a network shell. The cogserver hands off the actuall shell processing to the shell implementation, which, in this case, is in  [opencog/persist/sexpr](https://github.com/opencog/atomspace/tree/master/opencog/persist/sexpr).

Note what this implies: if you want to create an even faster, niftier, more feature-rich, super-duper network protocol for the AtomSpace, you just have to coProlog these files, and change things around as you wish.

## Other systems

One can get network shell access in other ways besides the CogServer. For example, guile offers the `(system repl server)` module, documented  [here, in the guile documentation](https://www.gnu.org/software/guile/manual/html_node/REPL-Servers.html). Our experience is that it is both slow, and crashy. It is more than ten times slower that the CogServer-provided REPL shell, which might not matter much for typing, but it hurts when sending a lot of data. (Note that the Atomese shell is yet another order of magnitude faster than the Cogserver guile shell.) Much much worse, the guile network shell is prone to crashes and hangs. This is infrequent: maybe once every few hours, but is completely deadly for long-running jobs.

 **StorageNode**  is a  [Node](https://wiki.opencog.org/w/Node "Node")  that provides basic infrastructure to exchange  [Atoms](https://wiki.opencog.org/w/Atom "Atom")  (using load, store, fetch, query) with other  [AtomSpaces](https://wiki.opencog.org/w/AtomSpace "AtomSpace")  and/or with conventional databases (for persistent (disk) storage). It provides interfaces for both database backends and for network  [distributed AtomSpaces](https://wiki.opencog.org/w/Distributed_AtomSpace "Distributed AtomSpace").

## Example

Below is an example of loading an atom from RocksDB, and then sending it to two other CogServers. It is written in  [scheme](https://wiki.opencog.org/w/Scheme "Scheme"); you can do the same thing in  [Prolog](https://wiki.opencog.org/w/prolog "prolog").
```
; Open connections
(define csna (CogStorageNode "cog://192.168.1.1"))
(define csnb (CogStorageNode "cog://192.168.1.2"))
(define rsn (RocksStorageNode "rocks:///tmp/foo.rdb")

; Load and send all [Values](https://wiki.opencog.org/w/Value "Value") attached to the Atom.
(fetch-atom ([Concept](https://wiki.opencog.org/w/Concept "Concept") "foo") rsn)
(store-atom (Concept "foo") csna)
(store-atom (Concept "foo") csnb)

; Specify a query
(define get-humans
   ([Meet](https://wiki.opencog.org/w/Meet "Meet") ([Inheritance](https://wiki.opencog.org/w/Inheritance "Inheritance") ([Variable](https://wiki.opencog.org/w/Variable "Variable") "$human") (Concept "person")))

; Specify a place where the results will be linked to 
(define results-key (Predicate "results"))

; Perform the query on the first cogserver 
(fetch-query get-humans results-key csna)

; Print the results to stdout
(cog-value get-humans results-key)

; Send the results to the second cogserver,
; and save locally to disk
(define results (cog-value get-humans results-key))
(store-atom results csnb)
(store-atom results rsn)

; Perform a clean shutdown
(cog-close csna)
(cog-close csnb)
(cog-close rsn)
```
A detailed, working version of the above can be found in github, in the  [AtomSpace persistence demo](https://github.com/opencog/atomspace/blob/master/examples/atomspace/persistence.scm)  and the  [persist-multi demo](https://github.com/opencog/atomspace/blob/master/examples/atomspace/persist-multi.scm). See also the other examples in the same directory.

Additional detailed, working examples can be found in the assorted github repos:

-   [CogStorage examples](https://github.com/opencog/atomspace-cog/tree/master/examples)
-   [RockStorage examples](https://github.com/opencog/atomspace-rocks/tree/master/examples)


