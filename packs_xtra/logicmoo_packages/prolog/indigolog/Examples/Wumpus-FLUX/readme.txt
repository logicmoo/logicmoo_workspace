
All code from FLUX was obtained freely from http://www.fluxagent.org/
New predicates were added to run FLUX interpreter on top of the whole
IndiGolog arquitecture.

This implementation requires ECLIPSE Prolog, which can be obtained for free from
the following address:

http://eclipse.crosscoreop.com/eclipse/


Last tested successfully with the following version of ECLIPSE:

Version 5.10 #44, Sun Jan 14 02:06 2007

================================================================================

Mini getting-started guide to get the Wumpus Example in FLUX running:

1. Make sure the installation of IndiGolog is correct 
	(read install.txt at the root directory of IndiGolog)
	- Because this application runs on ECLIPSE Prolog, make sure
	the environment variable ECLIPSELIBRARYPATH points to
	PATH_INDIGOLOG/lib.
	- Even if the main application runs in ECLIPSE, SWI-Prolog is still
	required to run the Wumpus Simulator (the extra xterm that appears and
	communicates with the Wumpus Applet).

2. Install a Java Runtime Environment (JRE) to your system.
   Make sure that 'java' invokes the Java application launcher.
   Also, note that JRE1.5 from SUN is tested to work, while the GNU Java 
   Compiler (GJC) is reported to fail when executing the script at step 5.

3. Go to directory <mypath>/indigolog-linux-0.5a/Examples/Wumpus-FLUX/WumpusApplet
   and execute script startWumpusGUI.

4. Go to directory <mypath>/indigolog/Examples/Wumpus-FLUX and start ECLIPSE Prolog

5. Consult main_ecl.pl and execute goal "main". Several controllers will be offered.

6. Watch the IndiGolog Agent act in the Wumpus World applet.

7. To run it once again, execute goal "main" again.

