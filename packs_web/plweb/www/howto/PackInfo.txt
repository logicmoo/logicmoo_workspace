---+ The contents of pack.pl

A pack must hold a file named =|pack.pl|= in the toplevel directory.
Note that the toplevel directory of the archive can be =|.|=, <pack> or
<pack>-<version>.  The file =|pack.pl|= contains _ground_ Prolog terms.
The terms below are defined.  Of these, only =name= and =version=
are obligatory.

  * name(Pack)
  Name of the pack.  This must be provided.  Must consists of ASCII
  characters that can always appear in filenames.  Due to case (in)
  sensitivity characters, use lowercase letters.  Use =|_|= as
  word separator.

  * title(Title)
  Free style description.  Please limit to ASCII to avoid problems
  with character encodings.

  * version(Version)
  Current version of the pack.  This is an atom consisting of integers
  separated by dots (.).  E.g., '1.1.2'.  This must match the version
  in the file name of the pack archive (e.g., =|mypack-1.1.2.tgz|=).

  * author(Name, Email)
  Provides name and email address of the author.  If there are multiple
  authors, include multiple of these terms.

  * maintainer(Name, Email)
  Current maintainer of the package.  May appear multiple times.

  * packager(Name, Email)
  The person that created the package.

  * home(URL)
  Home page URL providing more information about the package.

  * download(URLPattern)
  Principal download URL for the package.  Must be an HTTP URL.  May
  contain a file wildcard pattern to match multiple versions of the
  package.  Patterns are used by pack_upgrade/1 to query available
  package versions.

  * provides(Token)
  Package provides _Token_. A token is an atom, describing some property
  of the package. A package always provides its own package name. A pack
  may provided multiple tokens by repeating the provides(Token) term.

  * requires(Token)
  Package requires the presence of another package that is named Token
  or provides Token.

  * conflicts(Token)
  Package cannot be used together with Token.  Currently unused.

  * replaces(Pack)
  Package replaces Pack.  This implies that installing this package
  removes Pack.  May be specified multiple times.

Here is a simple example

  ==
  name(demo).
  title('Demo package').
  version('1.2.1').
  author('Jan Wielemaker', 'J.Wielemaker@vu.nl').
  ==

@see Pack.txt
