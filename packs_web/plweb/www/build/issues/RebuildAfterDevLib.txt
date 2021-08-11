# Fix incomplete installation due to missing dependencies

Sometimes SWI-Prolog lacks features or packages because the required
dependencies were not available while compiling the system from source.
Installed features and packages can be checked using
check_installation/0 (requires SWI-Prolog 7.1.10 or later). Issues found
are reported with a link to a wiki page describing the consequences and
often point to a missing development library. Fixing this takes two
steps.

  1. Find and install the library for your OS.

     $ Linux :
     Check out info on [your distribution](</build/LinuxDistro.txt>) or
     use the package manager to find and install the library.  Make sure
     to install the *development version* of the library. On Debian/apt
     based systems, development versions typically have the suffix
     =|-dev|=. On Redhat/rpm based systems the suffix is often
     =|-devel|=.

     $ MacOS :
     Use Macports or Brew to find the library and install it.

     $ Windows :
     See =README.mingw= for downloading and installing dependencies.

  2. Redo the build.  If you have not much experience, the best option is simply
     to redo the whole [build](</build/unix.html>).

     ==
     % make distclean
     % ./build
     ==

     Examine the log info in =configure.out= and =make.out=. If you do
     not understand why a library is claimed missing, examine
     =config.log=.  Both the kernel and each package have a =config.log=
     file.
