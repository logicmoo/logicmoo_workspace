---+ Is there a true C++ binding?

Actually, there are two. One is the original, written by me (Jan
Wielemaker), which is part of the standard SWI-Prolog packages. It
consists of the file SWI-cpp.h and is documented in:

    * http://www.swi-prolog.org/pldoc/package/pl2cpp.html

Now, I'm not much of a C++ programmer and the above interface is pretty
simple. It mostly deals with automatic type conversion, mapping of
exceptions (both ways) and scoping of Prolog artifacts such as query
objects.

Volker Wysk has created [another
interface](http://www.volker-wysk.de/swiprolog-c++/index.html) that is
based on how an experienced C++ programmers looks at Prolog.
Unfortunately, this interface is not maintained any more and requires
updating for both the current C++ standards and recent SWI-Prolog
versions. This pointer is maintained on this page to provide a starting
point for anyone looking for a more advanced C++ interface.

Both interfaces are _header only_, which means that there is no
accompanying library. Both interfaces can be freely mixed with each
other and with calls to the plain C API documented in the SWI-Prolog
reference manual. Have a look here if you are missing something as the
C-interface provides more than the two C++ layers on top of it.
