THIS is a collection of scripts that overwrites PDDL 3.0 files to prolog friendly syntax.
   PDDL         Prolog 
(on ?x ?y)    on(?x, ?y)

Syntax suggar: op(300, fy, ?).

For whole example check parserDomain.pl or  parserProblem.pl for usage and example output.

Validator:
  Test_validate_input.pl is a small script to test parsers on input files that are stored in ipc2008-no-cybersec. It is a collection of problemset files from International Planning Competition 2008.
  Usage:
  ?-[test_validate_input].
  ?-test.
