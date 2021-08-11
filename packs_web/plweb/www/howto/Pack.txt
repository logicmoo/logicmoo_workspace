---+ Creating and submitting extension packages for SWI-Prolog

---++ What is a pack?

A pack is an archive (=|.zip|= or =|.tgz|=) file that (minimally) contains,
two items:

  $ A subdirectory =prolog= :
  If the pack is installed, this directory is added to the Prolog
  library.  This directory contains Prolog files using the extension
  =|.pl|=.

  $ A file =|pack.pl|= :
  This file provides [[meta-data][PackInfo.txt]] for the pack.

Creating a pack that uses C or C++ code is [[described here][ForeignPack.txt]]

---++ Creating a pack

A pack is created by creating a directory with the name of the pack
and filling it with the content described above. Next, create an
archive, which is either a _|gzipped tar|_ file or a _zip_ file from the
file that must be named <pack>-<version>.tgz or <pack>-<version>.zip,
where version is a list of digits, separated by dots (.).  For example:

  ==
  % tar zcvf mypack-1.0.tgz mypack
  ==

or

  ==
  % zip -r mypack-1.0.zip mypack
  ==

---++ Test the pack

The pack may be installed using pack_install/1 as illustrated below.

  ==
  ?- pack_install('mypack-1.0.tgz').
  ==

---++ Make the pack available

To make the pack available, it must be downloadable from a publicly
available HTTP server. To support package upgrading, the HTTP server
must have enabled fetching an index of the directory. I.e., if the pack
is located at http://www.example.com/swi-prolog/pack/mypack-1.0.tgz,
fetching http://www.example.com/swi-prolog/pack/ must return an HTML
document with links to available package files.

After uploading the package to e.g.,
http://www.example.com/swi-prolog/pack/mypack-1.0.tgz, it is made
available to other users simply by installing it yourself:

  ==
  ?- pack_install('http://www.example.com/swi-prolog/pack/mypack-1.0.tgz').
  ==

After this, other people can install your package simply using

  ==
  ?- pack_install(mypack).
  ==

---+++ Using GitHub

Packages can be downloaded from [GitHub](http://github.com) in two
ways:

  1. Using the GIT protocol.  In this case, the URL looks like below.
     Be sure to add the =.git= extension.  This is not needed for
     GIT, but it tells the SWI-Prolog package manager that this is
     a git repository rather than a file.

     ==
     https://github.com/<owner>/<pack>.git
     ==

  2. Using [GitHub](http://github.com) _releases_.  These are created
     by adding and pushing a _release tag_ or using the GitHub website.
     The resulting archive can be accessed at the URL below.  Please
     use the =.zip= because the package manager does *not* understand
     the double extension =.tar.gz=.  The _tag_ must be dotted version
     number, optionally preceded by a =v= or =V=.

     ==
     https://github.com/<owner>/<pack>/archive/<tag>.zip
     ==

The advantage of using GitHub releases is that you decide when a new
version is ready for public use.  Using GitHub releases *requires
SWI-Prolog 7.1.22 or later*.  Automatic update checking can be enabled
by setting the =download= attribute of the pack to:

  ==
  https://github.com/<owner>/<pack>/releases/*.zip
  ==


---++ Dos and Don'ts

To make packages work smoothly, package submitters need to take care
of some rules:

  - Use a meaningful name for your package that is not likely to
    conflict.  Check http://www.swi-prolog.org/pack/list to verify
    there is no name conflict.

  - All files in the =prolog= directory *must* be Prolog *module* files.
    Use names for the module files that are not likely to conflict with
    others.

  - Use consistent version numbers (e.g. 0.1, 0.2 ..., 1.0).  Versions
    are compared by turning the version id into a list of integers that
    is compared using compare/3.  Make sure that the version in
    =|pack.pl|= matches the version encoded in the archive name.

  - If you messed up, fix it and *always increment the version number*.
    Uploading a new file with the same name/version is interpreted as a
    possible security breach by the pack system.

@see PackInfo.txt for a description of =|pack.pl|=
@see ForeignPack.txt for creating packs with C or C++ code
@see [[list of submitted packages][</pack/list>]]
@see library(prolog_pack) for predicates to query, install and remove packs
@see [[Status and TODO][PackTodo.txt]]
@see http://rlaanemets.com/post/show/prolog-pack-development-experience for some experience and best practices
