---+ Upgrading applications from 5.6.64 to 5.8.0

Most of it should be pretty compatible. The main differences that are
likely to affect people moving from 5.6.64 are:

    * 5.8.0 properly implements :- meta_predicate/1.  Code relying on
    the old incomplete emulation must be updated.  Code using
    :- module_transparent/1 still works, but both for better
    compatibility with the rest of the Prolog world and for better
    support from the environment, it is adviced to update code.

    * The initialization/1 directive is now ISO compliant, which means
    it is executed *after* loading the file in which it appears. This
    may cause problems with code doing

	==
	:- initalization(load_foreign_library(mylib)).
	==

     Such code should use

	==
	:- use_foreign_library(mylib).
	==

    * System libraries libraries no longer (auto-) import from 'user',
    but from `system'.

    * Operators follow the auto-import module relations, which means
    that system libraries only depend on operators declared by themselves
    or in the module `system'.

    * Preparing for 5.9.x, many foreign functions that used to be `void'
    now return an int (TRUE/FALSE).  5.8.x does nothing with this (they
    return TRUE or longjmp if an error happens).  In 5.9.x they will
    return FALSE if an error happens.
