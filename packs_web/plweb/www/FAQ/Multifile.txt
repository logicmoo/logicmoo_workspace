---+ Warning: (file:line) Redefined static procedure: name/arity

This error typically appears if a predicate, say p/1, is defined in
multiple source-files that are not module files (see module/2).  There
are two situations:

  1. This is a mistake; the two versions of p/1 should have been
  different predicates.  Now, there are two ways out:

    - Rename the predicate in one of the two files.
    - Start using [[modules][</pldoc/man?section=modules>]].

  2. This was intended: you want different clauses of the p/1 predicate
  in different clauses.  There are again two ways out:

    - Use the multifile/1 directive.  Add the directive to _each_ file,
      as illustrated below:

      file 1:

	==
	:- multifile p/1.
	p(a).
	p(b).
	==

      file 2:

	==
	:- multifile p/1.
	p(1).
	p(2).
	==

    - Use two different names in file 1 and file 2 and a disjunction to
      query both versions:

	==
	:- [file1].	% loads p_1/1
	:- [file2].	% loads p_2/1

	p(X) :- p_1(X).
	p(X) :- p_2(X).
	==

  The advantage of the multifile/1 based approach is that it is simple
  and all lookup is fully indexed. The price is that you have little
  control over the order of the clauses. If you modify file 1 and reload
  it, its clauses will now be _after_ those from file 2.  Make sure that
  the order does not matter!  The second approach maintains the order of
  answers. This approach is typically preferred if the facts from file 1
  and file 2 are conceptually different, e.g., the first defines people
  and the second organizations.

