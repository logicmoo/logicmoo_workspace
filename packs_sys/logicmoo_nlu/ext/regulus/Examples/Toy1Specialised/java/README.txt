Checking out the java example from CVS:

* If you have not already downloaded the REGULUS project, you can check
out the entire directory, including the Toy1 java, using the following
commands:

  $ cvs -d:pserver:anonymous@regulus.cvs.sourceforge.net:/cvsroot/regulus login
  $ cvs -z3 -d:pserver:anonymous@regulus.cvs.sourceforge.net:/cvsroot/regulus co -P regulus

* If you have already a previous version:

  $ cvs -d:pserver:anonymous@regulus.cvs.sourceforge.net:/cvsroot/regulus update


    Note: In Makefile.in, ensure that the Java_Home variable points
    to your JDK installation and both classpath variables point to the
    $REGULUS/Examples/Toy*/java, where Toy* is the example application
    directory that you are currently in (e.g. Toy1Specialised).

    You need to have the JDK to compile the application.  JRE is not sufficient.  

Running Toy1app.java

1.  Start up the NUANCE license server and recserver as instructed on
    pp. 27, 50.

2.  In a terminal window (cygwin/unix/linux)
    $ cd $REGULUS/Examples/Toy1/scripts

3.  Compile the application:
    $ make toy1

4.  Run the application:
    $ make run_toy1


Variations on Step 4:

To run the application with debugging output specific to the application:
   $ make run_toy1_debug

To run the application with debugging output from java runtime:
   $ make run_toy1_verbose




Test Suite of Sentences (must be tested in this order):

Command:				Expected Answer:

Turn on the light(s).			Sorry, that's ambiguous.
Turn off the light.			Sorry that doesn't make sense.

Turn on the fan.			The fan in the kitchen is on.
Is the fan on?				The fan in the kitchen is on.
Turn off the fan.			The fan in the kitchen is off.

Turn on the light in the kitchen.	The light in the kitchen is on.
Is the light on?			The light in the kitchen is on.
Turn on the light in the kitchen.	Sorry, that doesn't make sense.
Is the light in the kitchen on?		The light in the kitchen is on.

Turn on the fan in the kitchen.		The fan in the kitchen is on.
Turn on the fan in the kitchen.		Sorry, that doesn't make sense.
Is the fan in the kitchen on?		The fan in the kitchen is on.

Turn on the light in the living room.	The light in the living room is on
Turn on the light in the living room.	Sorry, that doesn't make sense.
Is the light in the living room on?	The light in the living room is on

Is the light off?			No.
Is the light on?			Sorry, that's ambiguous.
Is the light in the kitchen off?	No.
Is the light in the living room off?	No.
Is the fan in the kitchen off?		No.
Is the fan in the living room on?	No.
Is the fan in the living room off?	No.

Turn off the light in the kitchen.	The light in the kitchen is off.
Turn off the light.			The light in the living room is off.
Turn on the light in the living room.	The light in the living room is on.
Turn on the light.			The light in the kitchen is on.
Turn off the light.			Sorry, that's ambiguous.

Turn off the light in the living room.	The light in the living room is off.
Turn off the light.			The light in the kitchen is off.
Turn on the light in the kitchen.	The light in the kitchen is on.
Turn on the light.			The light in the living room is on.

Dim the light in the kitchen.		The light in the kitchen is dimmed.
Dim the light in the kitchen.		Sorry, that doesn't make sense.



