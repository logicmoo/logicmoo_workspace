%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  FILE      	: readme.txt
%  DATE	     	: January 2007
%  AUTHOR 		: Sebastian Sardina & Stavros Vassos 
%  EMAIL        : {ssardina,stavros}@cs.toronto.edu
%  WWW    		: www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  DESCRIPTION	: top-level readme.txt file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This is the root of the IndiGolog system. There are a few things you should
know before running IndiGolog:

(0) Before you can run anything, you will need to read "install.txt".

(1) The files of the implementation are scattered among many different
directories. Here is the structure starting from the initial directory:

Doc/            Documentation and manuals
Env/            Code related to the handling of the external environments.
                (e.g., simulation, LEGO environment, ER1 environment, etc.)
Eval/           Temporal projectors or evaluation procedures
Interpreters/   Main interpreter files and any transition system available.
lib/            Compatibility and tool libraries, global definitions.
Examples/       Domain applications (e.g., elevator controller, Wumpus World)

(2) Most of the code was designed and tested with the SWI Prolog system
(http://www.swi-prolog.org/). Nevertheless, the whole architecture should also
run with ECLIPSE Prolog (http://eclipse.crosscoreop.com/) which sometimes is
needed when the application uses constraint programming.

(3) Each example application has its own sub-directory inside the Examples
top-level directory. By convention, an application is loaded by consulting a
file with a name of the form "main_xxx.pl" where xxx denotes the specific
Prolog platform. For example, main_swi.pl is the file to load if using SWI
Prolog and main_ecl.pl is the file to load if using ECLIPSE Prolog instead.

(4) There are three example applications that work as simulations. You should
try to run these before trying IndiGolog on a real platform (such as on a real
robot, on the Internet, etc.).

   - Examples/Elevator-Vanilla.  This is the simplest application and it uses
     the vanilla IndiGolog interpreter (indigolog-vanilla.pl). It does not
     require any advanced features (like TCP/IP, Tcl/Tk, Java, or threads).

   - Examples/ElevatorSim-BAT. This is the same example as the vanilla one,
     but with a more sophisticated implementation.  There is now a special
     simulation environment that opens a new terminal window and a Tcl/Tk
     interface where the user can enter exogenous actions asynchronously.

   - Examples/ElevatorWumpus-KBAT. This is the Wumpus World implementation.
     It uses a simulated world and results are displayed in a Java window.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: readme.txt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%