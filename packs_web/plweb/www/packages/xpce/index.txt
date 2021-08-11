---+ XPCE: the SWI-Prolog native GUI library

---++ What is XPCE?

XPCE is a toolkit for developing graphical applications in Prolog and
other interactive and dynamically typed languages. XPCE follows a rather
unique approach to developing GUI applications, which we will try to
summarise using the points below.

    $ Add object layer to Prolog :
    XPCE's kernel is a object-oriented engine that allows for the
    definition of methods in multiple languages. The built-in graphics
    are defined in C for speed as well as to define the
    platform-independent layer. Applications, as well as some
    application-oriented libraries are defined as XPCE-classes with
    their methods defined in Prolog.

    Prolog-defined methods can receive arguments in native Prolog data,
    native Prolog data may be associated with XPCE instance-variables
    and XPCE errors are (selectively) mapped to Prolog exceptions. These
    features make XPCE a natural extension to your Prolog program.

    $ High level of abstraction :
    XPCE's graphical layer provides a high abstraction level, hiding
    details on event-handling, redraw-management and layout management
    from the application programmer, while still providing access to the
    primitives to deal with exceptional cases.

    $ Exploit rapid Prolog development cycle :

    Your XPCE classes are defined in Prolog and the methods run
    naturally in Prolog. This implies you can easily cross the border
    between your application and the GUI-code inside the tracer. It also
    implies you can modify source-code and recompile while your
    application is running.

    $ Platform independent programs :
    XPCE/Prolog code is fully platform-independent, making it feasible
    to develop on your platform of choice and deliver on the platform of
    choice of your users. As SWI-Prolog saved-states are
    machine-independent, applications can be delivered as a
    saved-state. Such states can be executed transparently using the
    development-environment to facilitate debugging or the runtime
    emulator for better speed and space-efficiency.

---++ Links about motivation and impressions

    * [[Why using XPCE for graphics in Prolog?][why.txt]]
    * [[Why no GUI-Builder?][noguibuilder.txt]]
    * [[Some code fragments to get an impression][examples.txt]]
    * [[Some screen dumps of applications][screens.txt]]
    * [[The design of the XPCE/Prolog interface][<pub:wlpe-02.pdf>]]
    (Publication in Workshop on Logic Programming Environments, 2002)

---++ Documentation

For starters as well as for more experienced users who want to know how
particular tasks are tackled using XPCE/Prolog, there is the [[XPCE
UserGuide][</download/xpce/doc/userguide/userguide.pdf>]]. The manual is
also available a
[[HTML-tar-archive][</download/xpce/doc/userguide/userguide.html.tgz>]]
and can be [[viewed online][<UserGuide/>]].

The reference documentation is available using a hypertext system
defined in XPCE/Prolog. This tool exploits the XPCE-class descriptions
as well as associated hypertext cards to provide various viewpoints and
search mechanisms for browsing the reference material. The manual tools
are started using the Prolog command manpce/0:

==
?- manpce.
==

Finally, the development tools and libraries form a rich set of
examples. Just browse through them and then use the Visual Hierarchy
Tool to locate the relevant source-code.

On Unix installations, the manpages xpce.1 and xpce-client.1 provide
documentation on the command-line options of these commands.
