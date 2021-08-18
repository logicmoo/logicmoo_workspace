/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
:- if((prolog_load_context(source,File),prolog_load_context(file,File));current_prolog_flag(xref,true)).
:- module(pfc_test,[mpred_test/1]).
:- endif.                             

:- system:use_module(library(prolog_stack)).
:- system:use_module(library(listing)).
:- system:use_module(library(lists)).
:- system:use_module(library(must_trace)).

:- use_module(library(prolog_stack)).
:- use_module(library(listing)).
:- use_module(library(lists)).
:- use_module(library(must_trace)).

%:- dumpST.

test_red_lined(Failed):- notrace((
  format('~N',[]),
  quietly_ex((doall((between(1,3,_),
  ansifmt(red,"%%%%%%%%%%%%%%%%%%%%%%%%%%% find ~q in srcs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",[Failed]),
  ansifmt(yellow,"%%%%%%%%%%%%%%%%%%%%%%%%%%% find test_red_lined in srcs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"))))))).

% mpred_test/1,mpred_test_fok/1, mpred_test(+),mpred_test_fok(+),

%% mpred_test(+P) is semidet.
%
% PFC Test.
%

%mpred_test(G):- notrace(mpred_test0(G)) -> true ; with_no_breaks(with_mpred_trace_exec(must(mpred_test_fok(G)))),!.
mpred_test(G):- notrace(mpred_test_fok(G)).

mpred_test0(G):- notrace((var(G),dmsg_pretty(var_mpred_test(G)))),!,trace_or_throw(var_mpred_test(G)).
%mpred_test((G1;G2)):- !,call_u(G1);mpred_test(G2).
mpred_test0(_):- notrace((compiling; current_prolog_flag(xref,true))),!.
mpred_test0(G):- notrace(mpred_is_silent),!, with_no_mpred_trace_exec(must(mpred_test_fok(G))),!.
mpred_test0(G):- notrace((dmsg_pretty(:-mpred_test(G)),fail)).
mpred_test0(G):- notrace((current_prolog_flag(runtime_debug,D),D<1)),!,with_no_mpred_trace_exec(must((G))),!.


:- if(false).
mpred_test(MPRED):- must(mpred_to_pfc(MPRED,PFC)),!,(show_call(umt(PFC))*->true;(call_u(PFC)*->mpred_why2(MPRED);test_red_lined(mpred_test(MPRED)),!,fail)).
%mpred_test(MPRED):- must(mpred_to_pfc(MPRED,PFC)),!,(show_call(call_u(PFC))*->true;(call(PFC)*->mpred_why2(MPRED);test_red_lined(mpred_test(MPRED)),!,fail)).
mpred_why2(MPRED):- must(mpred_to_pfc(MPRED,PFC)),!,(show_call(mpred_why(PFC))*->true;(test_red_lined(mpred_why(MPRED)),!,fail)).
:- endif.


% mpred_test_fok(G):- source_file(_,_),!,mpred_test_fok_0(G),!.
:- meta_predicate(mpred_test_fok(:)).
mpred_test_fok(G):- must((generate_test_name(G, Testcase), mpred_test_fok(Testcase, G))).

:- thread_local(t_l:mpred_current_testcase/1).

:- abolish(j_u:junit_prop/3).
:- dynamic(j_u:junit_prop/3).

mpred_test_fok(Testcase, G):- 
  add_test_info(testsuite,testcase,Testcase),
  locally(t_l:mpred_current_testcase(Testcase), mpred_test_fok_2(Testcase, G)).

mpred_test_fok_2(Testcase, G):- 
 must_det_l((
  add_test_info(Testcase,goal,G),
  ignore((source_location(S,L),atom(S),add_test_info(Testcase,src,S:L),
   sformat(URI,'~w#L~w',[S,L]),
   replace_in_string(
    [ "/opt/logicmoo_workspace"="https://logicmoo.org:2082/gitlab/logicmoo/logicmoo_workspace/-/blob/master"],
    URI,URL),
   add_test_info(Testcase,url,URL))),
  mpred_test_fok_4(G, TestResult, Elapsed),
  add_test_info(Testcase,time,Elapsed),
  TestResult=..[Type|Info],add_test_info(Testcase,Type,Info),
  add_test_info(Testcase,result,Type))).

mpred_test_fok_4(\+ G, TestResult, Elapsed):- !,
must_det_l((
  get_time(Start),
  catch(( ( \+ call_u(G) ) -> TestResult = passed; TestResult = failure),E, TestResult=error(E)),
  get_time(End),
  Elapsed is End - Start,
  (TestResult == failure -> Retry = G ;  Retry = ( \+ G)),
  save_info_to(info,why_was_true(Retry)))).
mpred_test_fok_4(G, TestResult, Elapsed):- !,
must_det_l((
  get_time(Start),
  catch(( (  call_u(G) ) -> TestResult = passed; TestResult = failure),E, TestResult=error(E)),
  get_time(End),
  Elapsed is End - Start,
  (TestResult == failure -> Retry = ( \+ G ) ;  Retry = G),
  save_info_to(info,why_was_true(Retry)))).


generate_test_name(baseKB:G,Testcase):- nonvar(G), !, generate_test_name(G,Testcase).
generate_test_name(\+ G, Name):- nonvar(G), !, generate_test_name(G,Name1), sformat(Name,'\naf ~w',[Name1]).
generate_test_name(call_u(G), Name):- nonvar(G), !, generate_test_name(G,Name).
generate_test_name(G,Name):- callable(G), (source_location(_,L); (_='',L='')), sformat(Name1,'~q. @ ~w',[G,L]),!, generate_test_name(Name1,Name).
generate_test_name(G,G):-!.
generate_test_name(G,Name):- string(G),!, 
  replace_in_string([
   '('='_',
   ')'='',
   ','='_',
   '__'='_'],G,Name).
                    
:- module_transparent(pfc_feature/1).
:- dynamic(pfc_feature/1).
:- export(pfc_feature/1).
pfc_feature(test_a_feature).

:- module_transparent(pfc_test_feature/2).
:- export(pfc_test_feature/2).

pfc_test_feature(Feature,Test):- pfc_feature(Feature)*-> mpred_test(Test) ; true.

:- system:import(pfc_feature/1).
:- system:export(pfc_feature/1).
:- system:import(pfc_test_feature/2).
:- system:export(pfc_test_feature/2).

:- system:import(pfc_feature/1).
:- system:export(pfc_feature/1).
:- baseKB:import(pfc_test_feature/2).
:- baseKB:export(pfc_test_feature/2).


warn_fail_TODO(G):- dmsg_pretty(:-warn_fail_TODO(G)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DUMPST ON WARNINGS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% none = dont act as installed
% ignore = ignore warnings but dumpst+break on errors
% dumpst = dumpst on warnings +break on errors
% break = break on warnings and errors
:- create_prolog_flag(logicmoo_message_hook,none,[keep(true),type(term)]).


skip_warning(informational).
skip_warning(information).
skip_warning(debug).

skip_warning(discontiguous).
skip_warning(query).
skip_warning(banner).
skip_warning(silent).
skip_warning(debug_no_topic).
skip_warning(break).
skip_warning(io_warning).
skip_warning(interrupt).
skip_warning(statistics).
% skip_warning(check).
skip_warning(compiler_warnings).
skip_warning(T):- \+ compound(T),!,fail.
skip_warning(_:T):- !, compound(T),functor(T,F,_),skip_warning(F).
skip_warning(T):-compound(T),functor(T,F,_),skip_warning(F).

add_test_info(Type,Info):- ignore((t_l:mpred_current_testcase(Testcase), add_test_info(Testcase,Type,Info))).

save_info_to(TestResult,Goal):- with_output_to(string(S),ignore(Goal)), add_test_info(TestResult,S).

add_test_info(Testcase,Type,Info):- j_u:junit_prop(Testcase,Type,InfoM),Info=@=InfoM,!.
add_test_info(Testcase,Type,_):- retract(j_u:junit_prop(Testcase,Type,[])),fail.
add_test_info(Testcase,Type,Info):- assertz(j_u:junit_prop(Testcase,Type,Info)).


inform_message_hook(T1,T2,_):- (skip_warning(T1);skip_warning(T2);(\+ thread_self_main)),!.
inform_message_hook(_,_,_):- \+ current_predicate(dumpST/0),!.
inform_message_hook(compiler_warnings(_,[always(true,var,_),always(false,integer,_),
   always(false,integer,_),always(true,var,_),always(false,integer,_),always(false,integer,_)]),warning,[]):- !.
inform_message_hook(import_private(_,_),_,_).
inform_message_hook(check(undefined(_, _)),_,_).
inform_message_hook(ignored_weak_import(header_sane,_),_,_).
inform_message_hook(error(existence_error(procedure,'$toplevel':_),_),error,_).
% inform_message_hook(_,warning,_).


inform_message_hook(T,Type,Warn):- atom(Type),
  memberchk(Type,[error,warning]),!,
  once((dmsg_pretty(message_hook_type(Type)),dmsg_pretty(message_hook(T,Type,Warn)),  
  ignore((source_location(File,Line),dmsg_pretty(source_location(File,Line)))),
  add_test_info(Type,{type:Type,info:T,data:Warn,src:(File:Line)}),
  nop(dumpST),
  nop(dmsg_pretty(message_hook(File:Line:T,Type,Warn))))),   
  fail.
inform_message_hook(T,Type,Warn):-
  ignore(source_location(File,Line)),
  once((nl,dmsg_pretty(message_hook(T,Type,Warn)),nl,
  add_test_info(Type,{type:Type,info:T,data:Warn,src:(File:Line)}),
  dumpST,nl,dmsg_pretty(message_hook(File:Line:T,Type,Warn)),nl)),
  fail.

inform_message_hook(T,Type,Warn):- dmsg_pretty(message_hook(T,Type,Warn)),dumpST,dmsg_pretty(message_hook(T,Type,Warn)),!,fail.
inform_message_hook(_,error,_):- current_prolog_flag(runtime_debug, N),N>2,break.
inform_message_hook(_,warning,_):- current_prolog_flag(runtime_debug, N),N>2,break.

list_test_results:- !.
list_test_results:-
  format('\n<!--  \n'),
  % listing(j_u:junit_prop/3), 
  show_junit_results,
  format(' -->\n').

show_junit_results:- 
  outer_junit(format('<?xml version="1.0" encoding="utf-8"?>\n<testsuites>\n',[])),
  findall(File,j_u:junit_prop(testsuite,file,File),L),list_to_set(L,S),
  maplist(save_testsuite,S),
  outer_junit(format('</testsuites>\n',[])).

outer_junit(G):- nop(G).

:- multifile prolog:message//1, user:message_hook/3.

halt_junit:- j_u:junit_prop(system,shown_testing_complete,true),!.
halt_junit:- asserta(j_u:junit_prop(system,shown_testing_complete,true)), list_test_results,
  save_results_single.


:- at_halt(halt_junit).


system:test_repl:-  assertz(system:junit_prop(need_retake,warn,need_retake)).
system:test_completed:- halt_junit,test_completed_exit_maybe(7).
system:test_retake:- halt_junit,test_completed_exit_maybe(3).


test_completed_exit(N):- dmsg_pretty(test_completed_exit(N)),fail.
test_completed_exit(7):- halt(7). % Passed
test_completed_exit(4):- halt(4). % Aborted by User
test_completed_exit(5):- halt(5). % Aborted by System

test_completed_exit(N):- getenv(keep_going,'-k'),!, halt(N).
test_completed_exit(N):- (debugging-> true ; halt(N)).

test_completed_exit_maybe(_):- j_u:junit_prop(_,error,_),test_completed_exit(9).
test_completed_exit_maybe(_):- j_u:junit_prop(_,warning,_),test_completed_exit(3).
test_completed_exit_maybe(_):- j_u:junit_prop(_,warn,_),test_completed_exit(3).
test_completed_exit_maybe(N):- test_completed_exit(N).


/* 
<?xml version="1.0" encoding="UTF-8"?>
<!-- a description of the JUnit XML format and how Jenkins parses it. See also junit.xsd -->

<!-- if only a single testsuite element is present, the testsuites
     element can be omitted. All attributes are optional. -->
<testsuites disabled="#" <!-- total number of disabled tests from all testsuites. -->
            errors="#"   <!-- total number of tests with error result from all testsuites. -->
            failures="#" <!-- total number of failed tests from all testsuites. -->
            name=""
            tests="#"    <!-- total number of successful tests from all testsuites. -->
            time="Total"     <!-- time in seconds to execute all test suites. -->
        >

  <!-- testsuite can appear multiple times, if contained in a testsuites element.
       It can also be the root element. -->
  <testsuite name=""      <!-- Full (class) name of the test for non-aggregated testsuite documents.
                               Class name without the package for aggregated testsuites documents. Required -->
         tests="#"     <!-- The total number of tests in the suite, required. -->
         disabled="#"  <!-- the total number of disabled tests in the suite. optional -->
             errors="#"    <!-- The total number of tests in the suite that errored. An errored test is one that had an unanticipated problem,
                               for example an unchecked throwable; or a problem with the implementation of the test. optional -->
             failures=""  <!-- The total number of tests in the suite that failed. A failure is a test which the code has explicitly failed
                               by using the mechanisms for that purpose. e.g., via an assertEquals. optional -->
             hostname=""  <!-- Host on which the tests were executed. 'localhost' should be used if the hostname cannot be determined. optional -->
         id=""        <!-- Starts at 0 for the first testsuite and is incremented by 1 for each following testsuite -->
         package=""   <!-- Derived from testsuite/@name in the non-aggregated documents. optional -->
         skipped=""   <!-- The total number of skipped tests. optional -->
         time=""      <!-- Time taken (in seconds) to execute the tests in the suite. optional -->
         timestamp="" <!-- when the test was executed in ISO 8601 format (2014-01-21T16:17:18). Timezone may not be specified. optional -->
         >

    <!-- Properties (e.g., environment settings) set during test
     execution. The properties element can appear 0 or once. -->
    <properties>
      <!-- property can appear multiple times. The name and value attributres are required. -->
      <property name="" value=""/>
    </properties>

    <!-- testcase can appear multiple times, see /testsuites/testsuite@tests -->
    <testcase name=""       <!-- Name of the test method, required. -->
          assertions="" <!-- number of assertions in the test case. optional -->
          classname=""  <!-- Full class name for the class the test method is in. required -->
          status=""
          time=""       <!-- Time taken (in seconds) to execute the test. optional -->
          >

      <!-- If the test was not executed or failed, you can specify one
           the skipped, error or failure elements. -->

      <!-- skipped can appear 0 or once. optional -->
      <skipped/>

      <!-- Indicates that the test errored. An errored test is one
           that had an unanticipated problem. For example an unchecked
           throwable or a problem with the implementation of the
           test. Contains as a text node relevant data for the error,
           for example a stack trace. optional -->
      <error message="" <!-- The error message. e.g., if a java exception is thrown, the return value of getMessage() -->
         type=""    <!-- The type of error that occured. e.g., if a java execption is thrown the full class name of the exception. -->
         ></error>

      <!-- Indicates that the test failed. A failure is a test which
       the code has explicitly failed by using the mechanisms for
       that purpose. For example via an assertEquals. Contains as
       a text node relevant data for the failure, e.g., a stack
       trace. optional -->
      <failure message="" <!-- The message specified in the assert. -->
           type=""    <!-- The type of the assert. -->
           ></failure>

      <!-- Data that was written to standard out while the test was executed. optional -->
      <system-out></system-out>

      <!-- Data that was written to standard error while the test was executed. optional -->
      <system-err></system-err>
    </testcase>

    <!-- Data that was written to standard out while the test suite was executed. optional -->
    <system-out></system-out>
    <!-- Data that was written to standard error while the test suite was executed. optional -->
    <system-err></system-err>
  </testsuite>
</testsuites>
  */
save_results:-
 j_u:junit_prop(testsuite,file,Name), 
 with_output_to(string(Text),show_junit_results),
 output_to_junit_file(Name,Text).

save_results_single:-
  % $TESTING_TEMP
  tell('/tmp/logicmoo_testing/junit_single.xml'),
  show_junit_results,
  told.


output_to_junit_file(Name,Text):- 
  atom_concat(Name,'_junit.xml',SaveFile), 
    open(SaveFile, write, _, [alias(junit)]),
      format(junit,'~w',Text), 
      close(junit).

save_testsuite(File):- 
   outer_junit(format("  <testsuite name=\"~w\">\n", [File])),
   findall(Name,j_u:junit_prop(testsuite,testcase,Name),L),list_to_set(L,S),
    maplist(save_testcase_result(File),S),
   outer_junit(format("  </testsuite>\n", [])).
  %plunit:check_for_test_errors.

good_type(passed).
nongood_type(warn).
nongood_type(error).
nongood_type(warning).
nongood_type(failure).
info_type(T):- \+ good_type(T), \+ nongood_type(T).

save_testcase_result(Suite,Testcase):- 
 escape_attribute(Testcase,ETestcase),
 ignore((
 format('
     <testcase name=~q ', [ETestcase]),
 replace_in_string([".pfc"="",'/opt/logicmoo_workspace/packs_'='','/'='.',
  '.t.'='.','sys.pfc.'='','.pfc'='.'],Suite,Package),
  format('package="~w" ', [Package]),
  format('classname="~w" ', [Package]),
 ignore((j_u:junit_prop(Testcase,time,Time),format('time="~20f"', [Time]))),
 format('>\n\n', []),
 ignore((write_test_test_info(Testcase))),
 format("\n    </testcase>\n", []))),!.


testcase_props(Testcase):-
 format("\n    <system-err><![CDATA[\n", []),
 forall(j_u:junit_prop(Testcase,Type,Warn), write_testcase_prop(Type,Warn)),
 format("\n    ]]></system-err>\n", []).

write_testcase_prop(_Type,[]):-!.
write_testcase_prop(info,S):- !, nop(format('~N~w~n',[S])).
write_testcase_prop(Type,Warn):- format('~N\t~w \t= ~q.~n',[Type,Warn]).

:- use_module(library(sgml)).
escape_attribute(I,O):-xml_quote_attribute(I,O).

write_test_test_info(Testcase):- j_u:junit_prop(Testcase,result,failure),!,
  with_output_to(string(NonGood), forall((j_u:junit_prop(Testcase,Type,Warn), nongood_type(Type)), format('~N~w = ~q.~n',[Type,Warn]))),
  escape_attribute(NonGood,ENonGood),
  format("      <failure message=~q />\n", [ENonGood]),
  testcase_props(Testcase).

write_test_test_info(Testcase):- \+ j_u:junit_prop(Testcase,result,passed),!,
  with_output_to(string(NonGood), forall((j_u:junit_prop(Testcase,Type,Warn), nongood_type(Type)), format('~N~w = ~q.~n',[Type,Warn]))),
  escape_attribute(NonGood,ENonGood),
  format("      <error message=~q />\n", [ENonGood]),
  testcase_props(Testcase).  

write_test_test_info(Testcase):- testcase_props(Testcase).


set_file_abox_module(User):- '$set_typein_module'(User), '$set_source_module'(User),
  set_fileAssertMt(User).

set_file_abox_module_wa(User):- set_file_abox_module(User),set_defaultAssertMt(User).

:- multifile prolog:message//1, user:message_hook/3.
% message_hook_handle(import_private(pfc_lib,_:_/_),warning,_):- source_location(_,_),!.
message_hook_handle(io_warning(_,'Illegal UTF-8 start'),warning,_):- source_location(_,_),!.
message_hook_handle(undefined_export(jpl, _), error, _):- source_location(_,_),!.
message_hook_handle(_, error, _):- source_location(File,4235),atom_concat(_,'/jpl.pl',File),!.
message_hook_handle(message_lines(_),error,['~w'-[_]]). 
message_hook_handle(error(resource_error(portray_nesting),_),
   error, ['Not enough resources: ~w'-[portray_nesting], nl,
      'In:', nl, '~|~t[~D]~6+ '-[9], '~q'-[_], nl, '~|~t[~D]~6+ '-[7], 
        _-[], nl, nl, 'Note: some frames are missing due to last-call optimization.'-[], nl, 
        'Re-run your program in debug mode (:- debug.) to get more detail.'-[]]).
message_hook_handle(T,Type,Warn):- 
  ((current_prolog_flag(runtime_debug, N),N>2) -> true ; source_location(_,_)),
  memberchk(Type,[error,warning]),once(inform_message_hook(T,Type,Warn)),fail.

:- if(current_predicate(fixup_exports/0)).
:- fixup_exports.
:- endif.

user:message_hook(T,Type,Warn):- 
   Type \== silent, Type \== debug, Type \== informational,
   current_prolog_flag(logicmoo_message_hook,Was),Was\==none,
   once(message_hook_handle(T,Type,Warn)),!.


