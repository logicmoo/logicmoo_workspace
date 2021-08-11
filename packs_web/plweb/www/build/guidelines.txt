# Guidelines for Linux package managers

Over time, each distribution has established   one  or more packages for
SWI-Prolog independently based on their own   conventions.  As a result,
nobody knows what SWI-Prolog is called and which packages to install. We
are guilty of this as well by using the short `pl` as the default system
name for a long time.

## Names of the executables

We wish to use the name *swipl*   on all installations. The system comes
with a helper program named *|swipl-ld|*.

## Directory hierarchy

The default installation of SWI-Prolog creates the following items:

  $ <PREFIX>/lib/swipl-<version> :
  This is a completely self-contained hierarchy that supports multiple
  architectures.  Please do _not_  split this.  Almost all directories
  are searched relative to this directory or documented as located
  relative to this directory.  Note that the `doc` directory is used
  by the help and documentation system.

  $ <PREFIX>/bin/swipl, swipl-ld :
  These binaries are relative symbolic links into the tree above.  Do
  _not_  rename these as the link information is used to find the above
  hierarchy.  This keeps the package relocatable, and this very same mechanism
  can be used to create relocatable applications from SWI-Prolog.

  $ <PREFIX>/man/man1/swipl.1, swipl-ld.1 :
  The standard Unix manpages.  These files are localized based on the
  installation directories.

## Making multiple packages

We would like to see that the   default  SWI-Prolog package installs the
same development environment on every  system.   The  default  top level
configure and make build all the packages we consider essential or small
enough to include anyway. Debian likes small packages and we came to the
following division:

  $ swi-prolog-nox :
  Contains the core compiler and all standard libraries that do not
  introduce major new dependencies.
  $ swi-prolog-x :
  Contains XPCE, the graphical front-end
  $ swi-prolog-odbc :
  Contains the ODBC interface
  $ swi-prolog-java :
  Contains the Java interface (JPL).
  $ swi-prolog-doc :
  Contains all documentation

The package `swi-prolog` depends on `swi-prolog-nox` and `swi-prolog-x`,
providing  a  good  default  system  that    does   not  introduce  many
dependencies on a standard workstation environment. See also Debian.txt.

## Building

Building follows standard CMake procedures.  The   system  can  be built
using _Profile Guided Optimization_  (PGO),  which   makes  it  about 7%
faster. To do so

  1. Configure cmake using the Ninja build tool (-G Ninja)
  2. Run ../script/pgo-compile.sh
  3. Run Ninja

## Patches

If you need patches that are not really specific to your distro, such as
incomplete cleanup in make distclean, missing DESTROOT or portability issues,
please send them to <mailto:bugs@swi-prolog.org>.
