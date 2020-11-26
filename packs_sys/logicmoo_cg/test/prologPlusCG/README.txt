README for Prolog+CG
Ulrik Petersen
Last update: June 22, 2005


What is this?
=============

Prolog+CG is a Java implementation of Prolog, with extensions for
supporting conceptual graphs and Object Orientation.


Website
=======

http://prologpluscg.sourceforge.net/

That is also where you can get the sourcecode for Prolog+CG, among
other goodies.


A better version is available
=============================

Prof. Adil Kabbaj, the original author of Prolog+CG, has developing a
newer, better version Prolog+CG.  It has been as part of the Amine
platform:

http://amine-platform.sourceforge.net/


How do I install it?
====================

First, you need Java 1.4 or 1.5.  If you don't have it, download it
from

http://java.com

or 

http://java.sun.com

You need either the Java Runtime Environment (JRE) or the Java SDK.
It should be version 1.4.2 or later.

Second, just unzip the file anywhere you your harddrive.  Then look in
the next section for how to run it.

Be sure to preserve the directory structure, which most modern ZIP
programs do by default.

You can move the program directory around, so long as you move all
files at once.  There are no Windows Registry settings -- this is a
cross-platform Java program.


How do I get it to run?
=======================

You must run java like this:

java -classpath classes PrologPlusCG/PrologPlusCG

You need Java 1.4.2 or later.

Make sure the current working directory is the one where this
README.txt file is found.


Windows users
-------------

For Windows users, there is a sample start.bat file which contains the
above command.  You must edit it and insert the path to your java.exe
file.

Thus if java.exe is in C:\j2sdk\bin you must edit the start.bat file
to say:

C:\j2sdk\bin\java.exe -classpath classes PrologPlusCG/PrologPlusCG

Note that you cannot use the java.exe file that comes with Internet
Explorer.  This is typically located in C:\Windows or
C:\Windows\System or similar.  Do not use this java.exe file!

If there is a space in the path, you must enclose the path + program
in "double quotes".  For example:

"C:\Program Files\JRE\1.4.2\bin\java.exe" -classpath classes PrologPlusCG/PrologPlusCG


Licenses
========

Prolog+CG is made available under the licenses described in COPYING.
Please read that file.


Documentation
=============

This distribution
-----------------

- NEWS.txt: Release notes.
- AUTHORS.txt: Who wrote what, and how to contact the maintainer.
- COPYING.txt: Copyright and what license restrictions apply (not
               many!)
- README.txt: This file


Main website
------------

The main Prolog+CG website is here:

http://prologpluscg.sourceforge.net/


Manual
------

The manual is included with the program, and will be displayed if you
select the "Help" menu and select "PROLOG+CG user's manual".  It is
located in:

manual/index.html

It is also available as PDF:

manual/manual.pdf

The manual is also available from the main website.

http://prologpluscg.sourceforge.net/manual/


Tutorial
--------

A tutorial on Prolog+CG, including a basic introduction to Prolog, is
available here:

http://www.huminf.aau.dk/cg/

Use Module II.


Building from source
====================

The Java compiler is invoked like this, in the root directory of the
distribution:

javac -source 1.4 -target 1.4 -d classes -classpath classes -sourcepath src src/PrologPlusCG/PrologPlusCG.java

Note that the directory "classes" must exist.  See the javac
documentation for further pointers to what all the switches mean.

Then you must copy the src/PrologPlusCG/gui/ImgPrlg.png file to
classes/PrologPlusCG/gui/ for this and the rest of the images to be
displayed.


