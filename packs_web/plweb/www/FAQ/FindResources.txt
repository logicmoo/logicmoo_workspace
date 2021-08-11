---+ [FATAL ERROR: Could not find system resources]

This message is associated with two situations

    * Broken installations
    * Prolog is embedded in a C/C++/Java/... application

On calling PL_initialise(), SWI-Prolog searches for a saved-state
holding the Prolog predicates that make up the system (or your
application). If it fails, it will print the message above and exit.

First of all it will try to identify the running executable. If
successful (which requires passing argv[0] from main() on Unix and
nothing special on Windows) and the executable contains a state at the
end, it is loaded.

Second, it tries to locate the SWI-Prolog home directory. For this, it
first checks the environment variable SWI_HOME_DIR. If it exists and
points to something that looks like the home-directory it uses this
directory. If not, it assumes this executable is installed in the bin
directory of the installation (Unix: bin/_architecture_). On Windows, it
also looks for the folder from which =libswipl.dll= was loaded, again
expecting this to be the =bin= directory of the Prolog home. Finally, it
will use the compiled-in installation directory. It expects the system
state in the file boot32.prc (or boot64.prc) in the home directory.

This poses problems for embedded applications that do not have a
saved-state associated (see the =swipl-ld= program), are not installed
in the same directory as the SWI-Prolog executable and SWI-Prolog is
installed as binary package (the compiled-in default is wrong).

---+ solution

On *Windows*, it suffices to leave =libswipl.dll= in the installation
tree (i.e., *do not copy it elsewhere*) and add the =bin= directory of
the installation tree to =|%PATH%|=.

A cross-platform and robust solution is to use putenv() to put an
appropriate path into the environment before calling PL_initialise().

==
	...;
	putenv("SWI_HOME_DIR=C:\\Program Files\\swipl");
	if ( PL_initialise(argc, argv) )
	  PL_halt(1);
	...
==

In the final version of your application you link the saved-state to the
executable (using =swipl-ld= or =cat= (Unix)) and comment the putenv()
call.

---++ Finding the home directory

You can find SWI-Prolog's notion of home from within Prolog using

  ```
  ?- current_prolog_flag(home, Home).
  Home = '...'
  ```

Or from the shell (for scripting/configuration) using the command below,
which dumps a shell script that sets variables that describe the Prolog
configuration.

  ```
  swipl --dump-runtime-variables
  ```
