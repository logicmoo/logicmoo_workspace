---+ Installation issues with the Visual C runtime library

SWI-Prolog depends on the MSVC runtime library, currently VC90.CRT.
This library is not shipped with the installer.  By default, the
installer tries to detect whether the runtime is available and
installs it from Microsoft if it cannot be detected.  There are
two possible issues with this:

  * On some systems the automatic install fails, e.g., because
  the machine is not attached to the network, uses a proxy, etc.
  In that case, install the runtime `somehow' by hand and retry
  installing SWI-Prolog.

  * On some systems, notably those where Windows is not installed
  in the default =|C:\windows|= folder, detection fails.  Make sure
  that the runtime is installed and uncheck the tickbox *|Install VC runtimes|*
  in the installer.