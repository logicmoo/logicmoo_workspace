---+ How do I load a file from the library?

In addition to accepting a plain atom representing a filename, the
loading predicates such as consult/1, use_module/1, etc. accept terms of
the form

    SearchPath(FileName)

One of the defined search paths is =library=, searching the standard
Prolog library. So, to load the library lists, use

==
:- use_module(library(lists)).
==

If you are using PceEmacs, library(lists) should colour blue, indicating
the file can be found. A right-click allows opening the file in the
editor.

See also absolute_file_name/3 and file_search_path/2 in the manual.
