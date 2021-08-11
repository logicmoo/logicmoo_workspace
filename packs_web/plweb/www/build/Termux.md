SWI-Prolog on Android Termux
============================

As of version 7.7.23 we have now SWI-Prolog running on Android under
[termux](https://termux.com/). With the exception of JPL it is passing
all tests, including saved states.

SWI-Prolog is available as a standard package and can be installed
using

    pkg install swi-prolog

After installation you can run it by typing this in the termux terminal:

```
$ swipl
Welcome to SWI-Prolog (threaded, 32 bits, version 7.7.23)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- write('Hello, swipl on Android!').
Hello, swipl on Android!
true.

```

Building it yourself
-----------------------

SWI-Prolog for android is built with the docker file used by `termux` to build packages.

The forked [termux-packages
repository](https://github.com/erlanger/termux-packages/tree/swi-prolog/packages/swi-prolog)
contains the scripts to build the SWI-Prolog `deb` files for Android
termux, as well as ossp-uuid.

The `run-docker` script below downloads and runs the docker container,
which in turn builds SWI-Prolog and all its dependencies. You need
docker installed and running on your build host.

To build the deb files yourself, do the following:

```sh
git clone -b swi-prolog https://github.com/erlanger/termux-packages
cd termux-packages
./scripts/run-docker.sh ./build-package.sh -a arm -f swi-prolog
```

You should now have the  `.deb` files in the `debs` directory.

Unsupported packages
---------------------

bdb, odbc, space


Test results
------------

All tests passing.

```
Welcome to Termux!

Wiki:            https://wiki.termux.com
Community forum: https://termux.com/community
IRC channel:     #termux on freenode
Gitter chat:     https://gitter.im/termux/termux
Mailing list:    termux+subscribe@groups.io

Search packages:   pkg search <query>
Install a package: pkg install <package>
Upgrade packages:  pkg upgrade
Learn more:        pkg help
$ swipl
Welcome to SWI-Prolog (threaded, 32 bits, version 7.7.23)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- test_installation.
% SWI-Prolog test suite.
% To run all tests run ?- test.
%
Running test set "syntax" ............................... done.
Running test set "write_test" .............. done.
Running test set "format_test" ......... done.
Running test set "unify" .. done.
Running test set "occurs_check" ...... done.
Running test set "unifiable" ... done.
Running test set "arithmetic" ...................................... done.
Running test set "arithmetic_functions"  done.
Running test set "floattest" ........ done.
Running test set "gmp" .............................................................
 done.
Running test set "chars" .. done.
Running test set "wchars" .. done.
Running test set "depth_limit" ..... done.
Running test set "type_test" .... done.
Running test set "meta" ............... done.
Running test set "avar" ........................... done.
Running test set "gvar" ..... done.
Running test set "copy_term" .............. done.
Running test set "term_hash" ........ done.
Running test set "cyclic" .......... done.
Running test set "cleanup" ............. done.
Running test set "term" ........... done.
Running test set "list" ........... done.
Running test set "sets" ......... done.
Running test set "atom_handling" ........................ done.
Running test set "string_handling" ..... done.
Running test set "proc" ........ done.
Running test set "cl" ........... done.
Running test set "record" ....... done.
Running test set "compiler" ....... done.
Running test set "flag" . done.
Running test set "update" ... done.
Running test set "gc" ........ done.
Running test set "control" ......... done.
Running test set "exception" ........ done.
Running test set "term_atom" .. done.
Running test set "os" .. done.
Running test set "io" .. done.
Running test set "popen" ... done.
Running test set "timeout" . done.
Running test set "file" ............. done.
Running test set "unicode_file" .... done.
Running test set "seek" . done.
Running test set "load_program" . done.
Running test set "ctype" ...... done.
Running test set "thread" ....... done.
Running test set "mutex" ... done.
Running scripts from unprotected .............................. done
Running scripts from core ..........................................................
....................................................................................
....................................................................................
....................................................................................
....................................................................................
....................................................................................
....................................................................................
....................................................................................
....................................................................................
....................................................................................
....................................................................................
....................................................................................
................................... done
Running scripts from attvar .. done
Running scripts from library .......................................................
....................................................................................
....................................................................................
..................... done
Running scripts from charset . done
Running scripts from eclipse
% Running ECLiPSe tests from file /data/data/com.termux/files/usr/lib/swipl/test/Tes
ts/eclipse/string_tests.tst
....................................................................................
...................................................................................
% Finished tests from file /data/data/com.termux/files/usr/lib/swipl/test/Tests/ecli
pse/string_tests.tst
% 167 tests found.
% 167 tests succeeded.
. done
Running scripts from clp . done
Running scripts from GC ......................... done
Running scripts from thread ........................................................
............................ done
Running scripts from save .... done
Testing package chr:chr ................ Passed 4.59 sec.
Testing package clib:cgi ............... Passed 0.55 sec.
Testing package clib:crypt ............. Passed 0.55 sec.
Testing package clib:memfile ........... Passed 0.59 sec.
Testing package clib:process ........... Passed 0.91 sec.
Testing package clib:readutil .......... Passed 0.46 sec.
Testing package clib:socket ............ Passed 5.68 sec.
Testing package clib:stream ............ Passed 0.50 sec.
Testing package clib:time .............. Passed 1.32 sec.
Testing package clib:uri ............... Passed 0.59 sec.
Testing package http:cgi_stream ........ Passed 1.71 sec.
Testing package http:http .............. Passed 3.99 sec.
Testing package http:json .............. Passed 2.01 sec.
Testing package http:multipart ......... Passed 1.95 sec.
Testing package http:proxy ............. Passed 2.18 sec.
Testing package http:websocket ......... Passed 1.99 sec.
Testing package pengines:pengines ...... Passed 6.77 sec.
Testing package pengines:term_html ..... Passed 1.06 sec.
Testing package protobufs:protobufs .... Passed 0.31 sec.
Testing package RDF:rdf ................ Passed 1.11 sec.
Testing package RDF:write .............. Passed 1.32 sec.
Testing package semweb:con ............. Passed 1.22 sec.
Testing package semweb:litmap .......... Passed 1.68 sec.
Testing package semweb:load ............ Passed 2.49 sec.
Testing package semweb:ntriples ........ Passed 2.22 sec.
Testing package semweb:rdf11 ........... Passed 1.76 sec.
Testing package semweb:rdf_db .......... Passed 1.59 sec.
Testing package semweb:subprop ......... Passed 9.29 sec.
Testing package semweb:turtle2 ......... Passed 2.57 sec.
Testing package semweb:turtle .......... Passed 12.53 sec.
Testing package sgml:sgml .............. Passed 0.58 sec.
Testing package sgml:sgml_write ........ Passed 0.70 sec.
Testing package sgml:xsd ............... Passed 0.66 sec.
Testing package zlib:zlib .............. Passed 1.94 sec.
Testing package pcre:pcre .............. Passed 0.57 sec.
Testing package yaml:yaml .............. Passed 0.71 sec.
Testing package ssl:ssl ................ Passed 7.60 sec.
All tests passed
true.

?-
```
