---+ Graphical applications

SWI-Prolog itself has no graphical capabilities and will not get them; this is to
ensure that the system can easily be embedded in any type of application. We
use SWI-Prolog primarily to develop interactive graphical applications.
What are the alternatives?

 
    $ Direct access to graphical API :
    Traditionally, people in the Prolog community have written Prolog
    wrappers around the native (C) API of their system. Xwip is an
    important example of this class of GUI approach for Prolog. The problem
    is that most of these APIs are fairly low-level and a lot of work
    is required to get the data types of the API properly and naturally
    represented in Prolog. 
 
    $ External GUI language :
    A better and more popular approach is to use an external language
    designed for GUI development. Popular candidates are Tk/Tcl,
    [[JPL/Java][</packages/jpl/>]], Visual Basic and Delphi. Prolog is
    connected to these systems using pipes, embedding or other suitable
    communication mechanisms. 
 
    $ [[Using XPCE][</packages/xpce/>]] [[star.gif]] :
    XPCE has been developed for GUI development in Prolog from the
    start. XPCE has a dynamically typed object-oriented kernel. Methods
    can be defined in any language. XPCE predefines a large number of
    classes, aiming at data representation as well as graphics. The
    graphical library allows for abstract description of interface
    components and is portable to Win32 and Unix/X11.

Using XPCE, interactive Prolog applications can be written completely in
Prolog. XPCE can be used for (space-)efficient storage of objects and
object-oriented structuring of your application. As of XPCE-5, native
Prolog data can be passed and stored with XPCE/Prolog defined classes. 
