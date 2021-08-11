---+ The rational behind XPCE

XPCE is a toolkit for the development of Graphical User Interfaces
(GUIs). XPCE aims at the rapid development of fast and well-structured
GUIs that are portable accross UNIX/X11 and Windows
(Windows-NT/2000/XP/Vista). It achieves these goals by providing a high
level of abstraction for specifying the GUI, a smooth integration with
Prolog and a set of development tools for browsing the class- and
object-world, with constant access to the documentation and your
source-code.

XPCE provides for two complementary ways of programming. The built-in
and library classes may simply be used from your Prolog code, or new
XPCE classes can be defined in Prolog using natural Prolog syntax. The
first is often used to `do something small', while the latter provides
excellent structuring for large graphical applications.


---++ Prolog and graphics

Prolog is normally associated with AI, natural language processing,
databases and similar tasks. Interactive programs may have subtasks for
which Prolog is `the tool-to-use'. Such applications are often
implemented in other languages and Prolog is used as an embedded engine
to deal with these tasks.

Prolog is very suitable for building graphical applications. Its
powerful meta-programming capabilities allow for clear and concise
declarations of properties and dependencies in the application. Its
interactive development environment allows for fast prototyping while
the application is running and its automatic data and memory management
makes development fast and reliable.

The two most important drawbacks of Prolog, speed and relatively large
footprint and startup time of very small applications do not matter for
interactive applications!


---++ Alternatives for Graphics in Prolog

A couple of approaches have been taken to arrive at graphical
applications that include Prolog. We will review them below.

    $ Embedding :
    Since quite some time, Prolog implementations provide powerful
    interfaces to C and mechanisms to embed the Prolog system in an
    application written in another language. The advantage of this
    approach is freedom of choice for your GUI platform. Disadvantage is
    that debugging and developing the hybrid environment is generally
    cumbersome, and you lose many the advantages of Prolog as a GUI
    language outlined above. 

    $ Using a Prolog with built-in graphics :
    Various Prolog implementations that originate from the PC or Mac
    world provide graphical primitives. They are generally non-portable. 

    $ Window-system API :
    In this approach, the C-API from the window-system is provided from
    Prolog with minimal changes. ProXt, distributed with Quintus Prolog,
    is an example of this approach. Advantages are that C-programmers
    with experience with the API have no trouble using it. The resulting
    GUI can be fully compliant to the platform standards. Disadvantages
    are the generally low-level of abstraction, while Prolog is not the
    most suitable language to add the very CPU-intensive graphics
    manipulations required by some applications. APIs designed for C
    generally do not provide sufficient data integrity to allow for
    complicated modifications at runtime or handling `retry' or `abort'
    from Prolog. Finally, the result is not portable. 

    $ External GUI languages :
    Another common approach is to use an external GUI language such as
    Tcl/Tk, and connect the interface to Prolog using pipes. Advantages
    and disadvantages are similar to `Embedding'. Intensive
    communication between the GUI and Prolog application can cause poor
    performance. 

    $ XPCE :
    The dynamic object-system of XPCE provides the ideal platform. It
    guarantees an acceptable level of integrity at the data level to
    allow for interactive development many Prolog users like so much. It
    provides object-oriented programming using a natural syntax from
    Prolog, which is very suitable for implementing large GUIs. The
    tight integration ensures good performance and finally, abstraction
    over the native Window-system API provides applications that are
    fully portable between UNIX and PCs. 

