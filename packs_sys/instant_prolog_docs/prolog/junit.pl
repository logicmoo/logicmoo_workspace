:- module(
  junit, [
    run_junit_tests/0,
    run_junit_tests/1,
    run_tests_and_halt/0,
    run_tests_and_halt/1
  ]).

:- use_module(library(test_cover)).
:- reexport(library(statistics), [profile/1]).


%  main test runner
run_junit_tests :-
  run_junit_tests(all).


run_junit_tests(Spec) :-
  \+ is_list(Spec),
  Spec \= all,
  !,
  run_junit_tests([Spec]).

run_junit_tests(Spec) :-
  set_prolog_flag(verbose, normal),
  patch_show_coverage,
  nb_setval(seen, 0),
  nb_setval(covered, 0),
  (
    Spec \= all
  ->
    maplist(get_pl_module, Spec, Modules)
  ;
    Modules=[]
  ),
  with_output_to(
    string(Coverage),
    (
      (
        Spec == all
      ->
        (
          flag(slow_test, true, true)
        ->
          show_coverage((run_tests, generate_doc))
        ;
          show_coverage(run_tests)
        )
      ;
        show_coverage(run_tests(Spec), Modules)
      )
    ->
      true
    ;
      % we do not want to fail even if run_tests fails
      true
    )
  ),
  split_string(Coverage, "\n", "\r", CovLines),
  forall(
    (
      member(Line, CovLines),
      split_string(Line, "\t ", "\t ", [_File, Clauses, Percent, _Fail]),
      % number of clauses is formated with ~D, i.e. comma for thousands
      split_string(Clauses, ",", "", LClauses),
      atomics_to_string(LClauses, ClausesNoComma),
      number_string(NClauses, ClausesNoComma),
      number_string(NPercent, Percent)
    ),
    (
      Covered is round(NPercent*NClauses/100),
      nb_getval(seen, Seen),
      nb_getval(covered, Cover),
      NSeen is Seen + NClauses,
      NCover is Cover + Covered,
      nb_setval(seen, NSeen),
      nb_setval(covered, NCover)
    )
  ),
  write(Coverage),
  nb_getval(seen, Seen),
  nb_getval(covered, Cover),
  Covered is Cover*100/Seen,
  format('TOTAL coverage~t ~D~64| ~t~1f~72|~n', [Seen, Covered]),
  open('junit.xml', write, _, [alias(junit)]),
  format(
    junit,
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<testsuites>\n", []
  ),
  forall(
    plunit:current_test_set(Unit),
    (
      format(junit, "  <testsuite name=\"~w\">\n", [Unit]),
      output_unit_results(Unit),
      format(junit, "  </testsuite>\n", [])
    )
  ),
  format(junit, "</testsuites>\n", []),
  close(junit),
  % Now we fail if all did not go right
  plunit:check_for_test_errors.


get_pl_module(Spec, Module) :-
  atom_concat('plunit_', Spec, TestModule),
  module_property(TestModule, file(TestFile)),
  atom_concat(PlFile, 't', TestFile),
  module_property(Module, file(PlFile)).


patch_show_coverage :-
  % old swi-prolog test_coverage.pl has one less argument,
  % FIXME if old enough it is not pachable
  file_search_path(swi, SWI),
  set_prolog_flag(access_level, system),
  (
    current_predicate(prolog_cover:show_coverage/2)
  ->
    dynamic(prolog_cover:file_coverage/4),
    prolog_cover:asserta(
      (prolog_cover:file_coverage(File, _, _, _) :- atom_concat(SWI, _, File),!)
    ),
    prolog_cover:asserta(
      (prolog_cover:file_coverage(File, _, _, _) :- atom_concat(_, '.plt', File),!)
    )
  ;
    dynamic(show_coverage/2),
    assertz(show_coverage(A, _) :- show_coverage(A)),
    (
      catch(
        (
          dynamic(prolog_cover:file_coverage/3),
          prolog_cover:asserta(
            (prolog_cover:file_coverage(File, _, _) :- atom_concat(SWI, _, File),!)
          ),
          prolog_cover:asserta(
            (prolog_cover:file_coverage(File, _, _) :- atom_concat(_, '.plt', File),!)
          )
        ),
        error(permission_error(_, _, _), _),
        true
      )
    )
  ).


run_tests_and_halt :-
  version(V1),
  current_prolog_flag(version, V2),
  format("Biocham v~w running on SWI-Prolog ~w~n", [V1, V2]),
  run_tests_and_halt(all).


run_tests_and_halt(Spec) :-
  call_cleanup(
    (
      run_junit_tests(Spec),
      halt(0)
    ),
    halt(1)
  ).


%  scans plunit dynamic predicates and outputs corresponding info to XML
output_unit_results(Unit) :-
  output_passed_results(Unit),
  output_failed_results(Unit).


%  outputs a successful testcase with its time for each plunit:passed/5 entry
output_passed_results(Unit) :-
  forall(
    plunit:passed(Unit, Name, _Line, _Det, Time),
    format(junit, "    <testcase name=\"~w\" time=\"~w\" />\n", [Name, Time])
  ).


%  outputs a failure inside a testcase for each plunit:failed/4 entry
output_failed_results(Unit) :-
  forall(
    plunit:failed(Unit, Name, _Line, Error),
    (
      format(junit, "    <testcase name=\"~w\">\n", [Name]),
      format(junit, "      <failure message=\"~w\" />\n", [Error]),
      format(junit, "    </testcase>\n", [])
    )
  ).
