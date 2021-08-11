---+ Status of packs and TODO

---++ Status of the package manager

The package manager is highqly experimental. It clearly needs many
additions as discussed below in the TODO section. We do assume that the
basic structure is sufficiently simple and flexible that this can be
maintained.

---++ TODO list of the package manager

---+++ Support git packages

Distributing packages via [[GIT][http://git-scm.com/]] is, at least in
theory, a much better alternative than using archive files. Git provides
controlled version management, distribution, verification using SHA1
hashes and digital signatures. The current code contains partial support
for installing from GIT repositories.  What needs to be done is:

  - Sync with central package server when installing or upgrading using git
  - Support to upgrade git packages
  - Verify signatures

---+++ Support foreign packages

Currently, packages installation only supports pure Prolog packages. The
core system already adds <lib>/<arch> to the =foreign= search path if
this directory appears in the pack. Future versions will allow for
building foreign packages. The idea is to support several different
build scenarios. The build tools will be called from Prolog, providing
information about the current Prolog version as environment variables.

  $ Using plain make :
  If there is a Makefile, run make on that Makefile.  The following
  targets will be defined:

    - =all= (default) for building the package
    - =check= to run the test suite
    - =install= for installing the library in bin/<arch>
    - =clean= to clean the build environment

  $ Preparing using configure :
  If =configure= is found, run this first.

Variables:

  | SWIPL	| SWI-Prolog executable (absolute path)		|
  | SWIHOME	| SWI-Prolog home directory			|
  | SWIVERSION	| Numeric SWI-Prolog version (e.g., 060114)	|
  | SWILIB	| SWI-Prolog Library to link to			|
  | SWICFLAGS	| Flags for compiling objects			|
  | SWILDFLAGS	| Flags to for linking a shared object/DLL	|


---+++ Support testing

If a directory =test= is in the package, load the files in this
directory one-by-one.  Each file must be named test_*.pl, be a module
file and export a predicate with the same (base)name as the file and
no arguments.  This predicate is called by the test suite.

---+++ More advanced version management

  - Deal with conflicts
  - Support version numbers in provides and requires statements
  - Deal with upgrading multiple packages

---+++ Code analysis

Perform code analysis to verify packages and package dependencies. Code
analysis can also access conflicts, security riscs and conformance to
standards.  Some of this is already present in the ClioPatria package
manager.

---+++ Documentation integration

---+++ Social tagging

Provide user-rating of packages, allow for comments, etc.
