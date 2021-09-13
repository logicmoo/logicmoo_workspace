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

%quietly_must_ex(G):- !, must_or_rtrace(G).
:- meta_predicate(quietly_must_ex(:)).
quietly_must_ex(G):- tracing -> (notrace,call_cleanup(must_or_rtrace(G),trace)); quietly_must(G).
:- module_transparent(quietly_must_ex/1).

:- meta_predicate(must_ex(:)).
must_ex(G):- !, must_or_rtrace(G).
:- module_transparent(must_ex/1).
%must_ex(G):- !, must(G).
%must_ex(G):- !, (catch(G,Error,(wdmsg(error_must_ex(G,Error)),fail))*->true;(wdmsg(must_ex(G)),if_interactive((ignore(rtrace(G)),wdmsg(must_ex(G)), break)))).
%must_ex(G):- (catch(quietly(G),Error,(wdmsg(error_must_ex(G,Error)),fail))*->true;(wdmsg(must_ex(G)),if_interactive((ignore(rtrace(G)),wdmsg(must_ex(G)), break)))).

%:- dumpST.

test_red_lined(Failed):- notrace((
  format('~N'),
  quietly((doall((between(1,3,_),
  ansifmt(red,"%%%%%%%%%%%%%%%%%%%%%%%%%%% find ~q in srcs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",[Failed]),
  ansifmt(yellow,"%%%%%%%%%%%%%%%%%%%%%%%%%%% find test_red_lined in srcs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"))))))).

% mpred_test/1,mpred_test/1, mpred_test(+),mpred_test(+),

%% mpred_test(+P) is semidet.
%
% PFC Test.
%

:- meta_predicate(mpred_test(:)).
:- module_transparent(mpred_test/1).
:- if(false).
%mpred_test(G):- notrace(mpred_test0(G)) -> true ; with_no_breaks(with_mpred_trace_exec(must(mpred_test(G)))),!.
%mpred_test(_):- notrace((compiling; current_prolog_flag(xref,true))),!.
mpred_test(MPRED):- must(mpred_to_pfc(MPRED,PFC)),!,(show_call(umt(PFC))*->true;(call_u(PFC)*->mpred_why2(MPRED);test_red_lined(mpred_test(MPRED)),!,fail)).
%mpred_test(MPRED):- must(mpred_to_pfc(MPRED,PFC)),!,(show_call(call_u(PFC))*->true;(call(PFC)*->mpred_why2(MPRED);test_red_lined(mpred_test(MPRED)),!,fail)).
% % mpred_why2(MPRED):- must(mpred_to_pfc(MPRED,PFC)),!,(show_call(mpred_why(PFC))*->true;(test_red_lined(mpred_why(MPRED)),!,fail)).
:- endif.
mpred_test(G):- mpred_test(_Testcase, G).

:- meta_predicate(mpred_test_fok(:)).
:- module_transparent(mpred_test_fok/1).
mpred_test_fok(G):- !, call(G).
mpred_test_fok(G):- mpred_test_fok(_Testcase, G).
:- meta_predicate(mpred_test_mok(:)).
:- module_transparent(mpred_test_mok/1).
mpred_test_mok(G):- !, call(G).
mpred_test_mok(G):- mpred_test_fok(_Testcase, G).

negate_call(\+ G, G).
negate_call(M:G,M:NG):- !, negate_call(G, NG).
negate_call(G, \+ G).

:- thread_local(t_l:mpred_current_testcase/1).
:- dynamic(j_u:junit_prop/3).

mpred_test(_,_):- notrace((compiling; current_prolog_flag(xref,true))),!.
mpred_test(Testcase, G):- ignore(mpred_test_fok(Testcase, G)).


mpred_test_fok(Testcase, G):-   
  junit_incr(tests), 
  junit_incr(test_number), 
  ignore((var(Testcase),generate_test_name(G, Testcase))),
  add_test_info(testsuite,testcase,Testcase),
  locally(t_l:mpred_current_testcase(Testcase), 
  (must_det_l((
    wdmsg(mpred_test(Testcase, G)),
    add_test_info(Testcase,goal,G),
    ignore((source_location(S,L),atom(S),add_test_info(Testcase,src,S:L),
    sformat(URI,'~w#L~w',[S,L]),
    replace_in_string( [ "/opt/logicmoo_workspace"
        ="https://logicmoo.org:2082/gitlab/logicmoo/logicmoo_workspace/-/blob/master"],
        URI,URL),
    add_test_info(Testcase,url,URL))),    
    get_time(Start))),
    Answers = nb(0),
    catch( ( call_u_hook(G) *-> TestResult = passed; TestResult = failure), E, TestResult=error(E)),
    ignore((Answers = nb(0),
      must_det_l((get_time(End),
      Elapsed is End - Start,
      add_test_info(Testcase,time,Elapsed),
      process_test_result(TestResult, G),    
      TestResult=..[Type|Info],add_test_info(Testcase,Type,Info),
      add_test_info(Testcase,result,Type),
      ignore((getenv('TEE_FILE',Tee),
      must_det_l((
        read_file_to_string(Tee,Str,[]),
        add_test_info(Testcase,out,Str),
        save_single_testcase(Testcase),
        nop(kill_junit_tee))))))))),
    (TestResult=error(E)-> throw(E) ; true),
    nb_setarg(1,Answers,1))),
    Type == passed.

kill_junit_tee:- 
  ignore((getenv('TEE_FILE',Tee),
          sformat(Exec,'cat /dev/null > ~w',[Tee]),
          shell(Exec))).

process_test_result(TestResult, G):- TestResult == passed, !, save_info_to(TestResult, why_was_true(G)).
process_test_result(TestResult, G):- TestResult \== failure,junit_incr(errors), !, save_info_to(TestResult, catch(rtrace(call_u_hook(G)), E, writeln(E))).
process_test_result(TestResult, G):- !, 
  junit_incr(failures),
  negate_call(G, Retry),
  save_info_to(TestResult, 
    (why_was_true(Retry),
     rtrace(G))).



junit_incr(Count):- flag(Count,T,T+1).
call_u_hook(\+ G):- !, \+ call_u_hook(G).
call_u_hook(M:( \+ G)):- !, \+ call_u_hook(M:G).
call_u_hook(G):- current_predicate(call_u/1),!,catch_timeout(call(call,call_u,G)).
call_u_hook(G):- catch_timeout(G).

mpred_why_hook(P):- current_predicate(call_u/1),!,catch_timeout(call(call,mpred_why,P)).

:- export(why_was_true/1).
why_was_true((A,B)):- !,why_was_true(A),why_was_true(B).
why_was_true(P):- % predicate_property(P,dynamic),
                  catch_timeout(mpred_why_hook(P)),!.
why_was_true(P):- dmsg_pretty(justfied_true(P)),!.

catch_timeout(P):- tracing,!,call(P).
catch_timeout(P):- catch(call_with_time_limit(30,w_o_c(P)),E,wdmsg(P->E)).

%generate_test_name(G,Name):- getenv('JUNIT_CLASSNAME',Class), gtn_no_pack(G,NPack),sformat(Name,'~w ~w',[Class, NPack]),!.
generate_test_name(G,Name):- source_context_name(SCName), gtn_no_pack(G,GName),
  (atom_length(GName,0)-> SCName = Name ; sformat(Name,'~w__~w',[SCName,GName])).

gtn_no_pack(G,''):- \+ callable(G), !.
gtn_no_pack(baseKB:G,Testcase):- nonvar(G), !, gtn_no_pack(G,Testcase).
gtn_no_pack(M: G, Name):- nonvar(G), !, gtn_no_pack(G,Name1), sformat(Name,'~w_in_~w',[Name1, M]).
gtn_no_pack(\+ G, Name):- nonvar(G), !, gtn_no_pack(G,Name1), sformat(Name,'naf_~w',[Name1]).
gtn_no_pack(G,Name):- atom(G), sformat(Name1,'~w',[G]), !, shorten_and_clean_name(Name1,Name).
gtn_no_pack(G,Name):- \+ compound(G), sformat(Name1,'~w',[G]), !, shorten_and_clean_name(Name1,Name).
gtn_no_pack(G,Name):- arg(_,G,A), compound(A), \+ is_list(A), !, gtn_no_pack(A,Name).
gtn_no_pack(G,Name):- is_list(G), member(E,G),!,gtn_no_pack(E,Name).
gtn_no_pack(G,Name):- arg(_,G,A), integer(A), !, functor(G,F,_),sformat(Name,'~w_~w',[F,A]).
gtn_no_pack(G,Name):- arg(_,G,A), atom(A), !, gtn_no_pack(A,Name).
gtn_no_pack(G,Name):- compound_name_arity(G,F,A),sformat(Name,'~w_~w',[F,A]).
/*
gtn_no_pack(G,Name):- \+ compound(G), !,
  sformat(Name1,'~w',[G]),
  shorten_and_clean_name(Name1,Name2),
  replace_in_string(['_c32_'='_','__'='_'],Name2,Name).
gtn_no_pack(G,Name):- is_list(G),!,maplist(gtn_no_pack,G,NameL), atomic_list_concat(NameL,'_',Name).
gtn_no_pack(G,Name):- compound_name_arguments(G,F,A), gtn_no_pack([F|A],Name).
*/
  

source_context_name(SCName):- 
  (source_location(_,L); (_='',L=0)), flag(test_number,X,X), 
  sformat(Name,'Test_~4d_Line_~4d',[X,L]),
  replace_in_string(['_0.'='_'],Name,SCName). 
                    
:- module_transparent(pfc_feature/1).
:- dynamic(pfc_feature/1).
:- export(pfc_feature/1).
pfc_feature(test_a_feature).

:- module_transparent(pfc_test_feature/2).
:- export(pfc_test_feature/2).

pfc_test_feature(Feature,Test):- pfc_feature(Feature)*-> mpred_test(Test) ; junit_incr(skipped).

:- system:import(pfc_feature/1).
:- system:export(pfc_feature/1).
:- system:import(pfc_test_feature/2).
:- system:export(pfc_test_feature/2).

:- baseKB:import(pfc_feature/1).
:- baseKB:export(pfc_feature/1).
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

system:is_junit_test:- getenv('JUNIT_PACKAGE',_).

skip_warning(T):- \+ callable(T),!,fail.
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
skip_warning(editline).
% skip_warning(check).
skip_warning(compiler_warnings).
skip_warning(T):- \+ compound(T),!,fail.
%skip_warning(M:T):- !, skip_warning(M),skip_warning(T).
skip_warning(C):- compound_name_arguments(C,N,A),member(E,[N|A]),skip_warning(E).


with_output_to_tracing(Where,Goal):- \+ tracing,!,with_output_to(Where,Goal).
with_output_to_tracing(_Where,Goal):- call(Goal).

save_info_to(TestResult,Goal):- 
 with_output_to_tracing(string(S),
  (fmt(TestResult=info(Goal)),
   ignore(Goal))), write(S),
  add_test_info(TestResult,S).

here_dumpST:- !.
here_dumpST:- dumpST.

add_test_info(Type,Info):- ignore(((get_current_testcase(Testcase), add_test_info(Testcase,Type,Info)))).

get_current_testcase(Testcase):- t_l:mpred_current_testcase(Testcase),!.

get_current_testcase(Testcase):- getenv('FileTestCase',Testcase), add_test_info(testsuite,testcase,Testcase),!.
get_current_testcase(Testcase):- "suiteTestcase"=Testcase, add_test_info(testsuite,testcase,Testcase),!.
% get_current_testcase(Testcase):- j_u:junit_prop(testsuite,file,Testcase).

add_test_info(Testcase,Type,Info):- j_u:junit_prop(Testcase,Type,InfoM),Info=@=InfoM,!.
add_test_info(Testcase,Type,_):- retract(j_u:junit_prop(Testcase,Type,[])),fail.
add_test_info(Testcase,Type,Info):- assertz(j_u:junit_prop(Testcase,Type,Info)).


inform_message_hook(T1,T2,_):- (skip_warning(T1);skip_warning(T2);(\+ thread_self_main)),!.
inform_message_hook(_,_,_):- \+ current_predicate(dumpST/0),!.

inform_message_hook(compiler_warnings(_,[always(true,var,_),always(false,integer,_),
   always(false,integer,_),always(true,var,_),always(false,integer,_),always(false,integer,_)]),warning,[]):- !.

% warning, "/opt/logicmoo_workspace/lib/swipl/xpce/prolog/boot/pce_editor.pl:136: Initialization goal failed")

inform_message_hook(error(existence_error(procedure,'$toplevel':_),_),error,_).
% inform_message_hook(_,warning,_).

inform_message_hook(T,Type,Term):- atom(Type),
  memberchk(Type,[error,warning]),!, 
  once((dmsg_pretty(message_hook_type(Type)),dmsg_pretty(message_hook(T,Type,Term)),  
  ignore((source_location(File,Line),dmsg_pretty(source_location(File,Line)))),
  with_output_to(string(Text),
   ignore((set_stream(current_output,tty(true)),
    % format('~q~n',message{type:Type,info:T,src:(File:Line)}),
     inform_message_to_string(Term,Str),write(Str)))),
  add_test_info(Type,Text),
  write(Text),
  nop(dumpST),
  nop(dmsg_pretty(message_hook(File:Line:T,Type,Term))))),   
  fail.
inform_message_hook(T,Type,Term):-
  ignore(source_location(File,Line)),
  once((nl,dmsg_pretty(message_hook(T,Type,Term)),nl,
  add_test_info(Type,{type:Type,info:T,data:Term,src:(File:Line)}),
  here_dumpST, nl,dmsg_pretty(message_hook(File:Line:T,Type,Term)),nl)),
  fail.

inform_message_hook(T,Type,Term):- dmsg_pretty(message_hook(T,Type,Term)),here_dumpST,dmsg_pretty(message_hook(T,Type,Term)),!,fail.
inform_message_hook(_,error,_):- current_prolog_flag(runtime_debug, N),N>2,break.
inform_message_hook(_,warning,_):- current_prolog_flag(runtime_debug, N),N>2,break.

inform_message_to_string(Term,Str):- catch(message_to_string(Term,Str),_,fail),string(Str),\+ atom_contains(Str,"Unknown message"),!.
inform_message_to_string(Term,Str):-
    catch('$messages':actions_to_format(Term, Fmt, Args),_,fail),
    catch(format(string(Str), Fmt, Args),_,fail),!.
inform_message_to_string(Term,Str):- format(string(Str), '~q', [Term]),!.

%list_test_results:- !.
list_test_results:-
  writeln('\n<!-- '),
  % listing(j_u:junit_prop/3), 
  show_all_junit_suites,
  writeln(' -->').

show_all_junit_suites:- 
  %listing(j_u:junit_prop/3),
  outer_junit(writeln('<?xml version="1.0" encoding="utf-8"?>\n<testsuites>')),
  findall(File,j_u:junit_prop(testsuite,file,File),L),list_to_set(L,S),
  maplist(show_junit_suite,S),
  outer_junit(writeln('</testsuites>')).

outer_junit(G):- nop(G).


system:halt_junit:- j_u:junit_prop(system,halted_junit,true),!.
system:halt_junit:- asserta(j_u:junit_prop(system,halted_junit,true)),!,
  % list_test_results,
  save_junit_results.

junit_term_expansion(I,_):-
 notrace((
    I==end_of_file,   
    current_prolog_flag(test_src,Src),
    source_location(Src,_),
    current_prolog_flag(test_completed,Goal))),
    ignore(Goal),fail.
junit_term_expansion(_ , _ ):- prolog_load_context(file,SF), \+ j_u:junit_prop(testsuite,file,SF),!,fail.
junit_term_expansion(Var , _ ):- var(Var),!,fail.
junit_term_expansion( (end_of_file), [] ):- !, system:halt_junit, fail.
junit_term_expansion(M:I,M:O):- !, junit_term_expansion(I,O).
junit_term_expansion((:- I),O):- !, junit_dirrective_expansion(I,M), (is_list(M) -> O=M ; O=(:-M)).

junit_dirrective_expansion(I,O):- junit_expansion(junit_dirrective_exp,I,O).

junit_dirrective_exp( I , O ) :- junit_goal_exp(I,O) -> I\=@=O. 
junit_dirrective_exp( listing(X), dmsg(skipped(listing(X))) ):- keep_going. 
junit_dirrective_exp( \+ X, mpred_test( \+ X ) ).
%junit_dirrective_exp( X, X  ):- predicate_property(X,static).
%junit_dirrective_exp( X, X  ):- predicate_property(X,built_in).
%junit_dirrective_exp( X, mpred_test( X ) ).
junit_dirrective_exp( X, X  ):- !.

junit_expansion(_,Var , Var ):- var(Var),!.
junit_expansion(P,(A,B),(AO,BO)):- !,junit_expansion(P,A,AO),junit_expansion(P,B,BO).
junit_expansion(P,(A;B),(AO;BO)):- !,junit_expansion(P,A,AO),junit_expansion(P,B,BO).
junit_expansion(P,M:I,M:O):- !, junit_expansion(P,I,O).
junit_expansion(P,I,O):-call(P,I,O).

junit_goal_expansion(I,O):- junit_expansion(junit_goal_exp,I,O).
junit_goal_exp( Break, dmsg(skipped(blocks_on_input,Break))):- blocks_on_input(Break), keep_going. 
junit_goal_exp( Messy, dmsg(skipped(messy_on_output,Messy))):- messy_on_output(Messy), keep_going. 
% junit_goal_exp( must(A),mpred_test(A)) :- is_junit_test.



messy_on_output( cls ).
messy_on_output( listing ).
messy_on_output( xlisting(_) ).

blocks_on_input( trace ).
blocks_on_input( break ).
blocks_on_input( prolog ).



test_completed_exit(N):- dmsg_pretty(test_completed_exit(N)),fail.
test_completed_exit(7):- halt(7). % Passed
test_completed_exit(4):- halt(4). % Aborted by User
test_completed_exit(5):- halt(5). % Aborted by System

test_completed_exit(_):- once((listing(j_u:junit_prop(_,warn,_)),
                               listing(j_u:junit_prop(_,warning,_)),
                               listing(j_u:junit_prop(_,error,_)))),fail.

test_completed_exit(N):- keep_going,!, halt(N).
test_completed_exit(N):- (current_prolog_flag(debug,true)-> true ; halt(N)).

test_completed_exit_maybe(_):- j_u:junit_prop(_,error,_), test_completed_exit(9).
test_completed_exit_maybe(_):- j_u:junit_prop(_,warning,_),test_completed_exit(3).
test_completed_exit_maybe(_):- j_u:junit_prop(_,warn,_),test_completed_exit(3).
test_completed_exit_maybe(N):- test_completed_exit(N).


system:test_repl:-  assertz(system:junit_prop(need_retake,warn,need_retake)).
system:test_completed:- system:halt_junit,test_completed_exit_maybe(7).
system:test_retake:- system:halt_junit,test_completed_exit_maybe(3).

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
                               ShortClass name without the package for aggregated testsuites documents. Required -->
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
save_junit_results:-
 forall(j_u:junit_prop(testsuite,file,File), 
    (with_output_to(string(Text),show_junit_suite_xml(File)),
     save_to_junit_file(File,Text))).
save_junit_results:- wdmsg(unused(save_junit_results)).

show_junit_suite_xml(File):- 
  writeln('<?xml version="1.0" encoding="utf-8"?>'),
  writeln('<testsuites>'),
  maplist(show_junit_suite,File),
  writeln('</testsuites>'),!.
  

junit_count(tests).
junit_count(errors).
junit_count(skipped).
%junit_count(disabled).
junit_count(failures).


clear_suite_attribs:- forall(junit_count(F),flag(F,_,0)),
  retractall(j_u:junit_prop(testsuite,start,_)),
  get_time(Start),asserta(j_u:junit_prop(testsuite,start,Start)).

get_suite_attribs(SuiteAttribs):-    
  with_output_to(string(SuiteAttribs),
(( ignore((getenv('JUNIT_PACKAGE',Package), format(' package="~w"', [Package]))),
   ignore((j_u:junit_prop(testsuite,start,Start),get_time(End),Elapsed is End - Start,format(' time="~3f"',[Elapsed]))),
   forall((junit_count(F),flag(F,C,C)),format(' ~w="~w"',[F,C]))))).

show_junit_suite(File):- 
   (getenv('JUNIT_SUITE',SuiteName);SuiteName=File),!,
  get_suite_attribs(SuiteAttribs),
  format("  <testsuite name=\"~w\" ~w>\n", [SuiteName, SuiteAttribs]),
   findall(Name,j_u:junit_prop(testsuite,testcase,Name),L),list_to_set(L,S),
    maplist(show_junit_testcase(File),S),
   writeln("  </testsuite>"),
   clear_suite_attribs.

save_single_testcase(Name):- 
 with_output_to(string(Text),
  (writeln('<?xml version="1.0" encoding="utf-8"?>'),
   j_u:junit_prop(testsuite,file,File),
   writeln("  <testsuites>"),
   (getenv('JUNIT_SUITE',SuiteName);SuiteName=File),!,
  get_suite_attribs(SuiteAttribs),
  format("  <testsuite name=\"~w\" ~w>\n", [SuiteName, SuiteAttribs]),
   show_junit_testcase(File,Name),
   writeln("  </testsuite>"),
   writeln(" </testsuites>"))),
 shorten_and_clean_name(File,SFile),
 shorten_and_clean_name(Name,SName),
 atomic_list_concat([SFile,'-',SName],RSName),
 save_to_junit_file(RSName,Text).

classname_to_package(CN,P,C):- atomic_list_concat(List,'.',CN), append(Left,[C],List),atomic_list_concat(Left,'.',P).

%shorten_and_clean_name(Name,RSName):- atomic_list_concat([L,_|_],'.',Name),!,shorten_and_clean_name(L,RSName).
%shorten_and_clean_name(Name,RSName):- atomic_list_concat(List,'/',Name),append(_,[N1,N2,N3,N4],List),
%  atomic_list_concat(['prolog.',test_,N1,'.',N2,'.',N3,'.',N4],'',RSName).
shorten_and_clean_name(Name,RSName):- 
  ensure_compute_file_link(Name,Name0),
  replace_in_string(
  ['https://logicmoo.org:2082/gitlab/logicmoo/'="",
   '-/blob/'='',
   '/'='_',
   '_master_packs_'='_'],Name0,Name1),
  p_n_atom_filter_var_chars(Name1,Name2),
  replace_in_string(['__'='_'],Name2,Name3),
  last_n_chars(Name3,RSName),!.

%last_n_chars(SName,RSName):- sub_atom(SName,0,20,0,RSName),!.
last_n_chars(SName,SName).


clean_away_ansi(DirtyText,CleanText):- atom_codes(DirtyText,Codes),clean_ansi_codes(Codes,CodesC),sformat(CleanText,'~s',[CodesC]),!.
clean_away_ansi(DirtyText,DirtyText).

  is_control_code(10):-!, fail.  is_control_code(13):-!, fail.
  is_control_code(C):- C < 32.  is_control_code(C):- \+ char_type(C,print),!.
  is_control_code(C):- C>128.
  
  clean_ansi_codes([],[]).
  clean_ansi_codes([27,_|Codes],CodesC):- !, clean_ansi_codes(Codes,CodesC).
  clean_ansi_codes([C|Codes],CodesC):- is_control_code(C),!, clean_ansi_codes(Codes,CodesC).
  clean_ansi_codes([C|Codes],[C|CodesC]):- clean_ansi_codes(Codes,CodesC).

:- dynamic(j_u:last_saved_junit/1).

save_to_junit_file_text(Full,Text):- j_u:last_saved_junit(Full),!,
    flag(Full,X,X+1),
    atomic_list_concat([Full,'_',X,'-junit.xml'],FullF),
    format('~N% saving_junit: ~w~n',[FullF]),
  setup_call_cleanup(open(FullF, write, Out),writeln(Out,Text), close(Out)),!.
save_to_junit_file_text(Full,Text):- 
    asserta(j_u:last_saved_junit(Full)),
    atomic_list_concat([Full,'-junit.xml'],FullF),
    format('~N% saving_junit: ~w~n',[FullF]),    
  setup_call_cleanup(open(FullF, write, Out),writeln(Out,Text), close(Out)),!.

save_to_junit_file(Name,DirtyText):-
 clean_away_ansi(DirtyText,Text),
  getenv('TEST_STEM_PATH',Dir),!,
  must_det_l(( 
  atomic_list_concat([Dir,'-',Name],Full),
  write_testcase_env(Name),
  save_to_junit_file_text(Full,Text),
  clear_suite_attribs)).

save_to_junit_file(Name,DirtyText):- 
  clean_away_ansi(DirtyText,Text),
   format('<Test name="~w">',[Name]),
   writeln(Text),!,
   format('<!-- ~w--></Test>',[Name]).

save_junit_results_single:-
  % $TESTING_TEMP
  getenv('TESTING_TEMP',Dir),
  directory_file_path(Dir,'junit_single.ansi',Full),!,
  tell(Full),
  show_all_junit_suites,
  told, clear_suite_attribs.
save_junit_results_single.


good_type(passed).
nongood_type(warn).
nongood_type(error).
nongood_type(warning).
nongood_type(failure).
info_type(T):- \+ good_type(T), \+ nongood_type(T).

suite_to_package(Suite,Package):- shorten_and_clean_name(Suite,Suite0),
  atomic_list_concat(Split,'/logicmoo_workspace/',Suite0),last(Split,Right),
  replace_in_string([".pfc"="",".pl"="",'/'='.'],Right,Package),!.

show_junit_testcase(Suite,Testcase):- 
 j_u:junit_prop(Testcase,goal,Goal),
 (getenv('JUNIT_CLASSNAME',Classname)-> true ; suite_to_package(Suite,Classname)),
 %(getenv('JUNIT_PACKAGE',Package) -> true ; classname_to_package(Classname,Package,_ShortClass)),
 %ignore((getenv('JUNIT_SHORTCLASS',ShortClass))),
 %ignore((getenv('JUNIT_SUITE',JUNIT_SUITE))),
 %(nonvar(ShortClass)-> true; atom_concat(Package,ShortClass,Classname)),
 sformat(DisplayName,'~w@~w: ~p',[Classname,Testcase,Goal]),
 escape_attribute(DisplayName,EDisplayName),
 ignore((
 format('\n     <testcase name=~q ', [EDisplayName]),
  % format('package="~w" ', [Package]),
  format('classname="~w" ', [Classname]),
 ignore((j_u:junit_prop(Testcase,time,Time),format('time="~3f"', [Time]))),
 writeln('>'),
 ignore((write_testcase_info(Testcase))),
 writeln("\n    </testcase>"))),!.

write_testcase_env(Name):-
  write_testcase_prop(name,Name),
  forall(junit_env_var(N),ignore((getenv(N,V),write_testcase_prop(N,V)))),!.

junit_env_var('JUNIT_CLASSNAME').
junit_env_var('JUNIT_PACKAGE').
junit_env_var('JUNIT_SHORTCLASS').
junit_env_var('JUNIT_SUITE').
junit_env_var('JUNIT_CMD').

write_testcase_std_info(Testcase):-
 writeln("\n    <system-err><![CDATA["),
 write_testcase_env(Testcase),
 ignore((j_u:junit_prop(Testcase,out,Str),format('~w',[Str]))),
  forall(j_u:junit_prop(Testcase,Type,Term), write_testcase_prop(Type,Term)),
 writeln("\n    ]]></system-err>").

write_testcase_prop(_Type,[]):-!.
write_testcase_prop(info,S):- !, format('~N~w~n',[S]).
write_testcase_prop(out,_).
write_testcase_prop(url,Term):- !, format('~N\t~w \t= <pre>~w</pre>~n',[url,Term]).
write_testcase_prop(Type,Term):- string(Term),!,format('~N\t~w \t=~w~n',[Type,Term]).
write_testcase_prop(Type,Term):- format('~N\t~w \t= ~q.~n',[Type,Term]).

:- use_module(library(sgml)).
escape_attribute(I,O):-xml_quote_attribute(I,O).


get_nongood_strings(Testcase,NonGood):- 
  with_output_to(string(NonGood), 
    forall((j_u:junit_prop(Testcase,Type,Term), nongood_type(Type)), 
      format('~N~w = ~q.~n',[Type,Term]))).

write_testcase_info(Testcase):- j_u:junit_prop(Testcase,result,failure),!,
  get_nongood_strings(Testcase,NonGood),
  write_message_ele('failure',NonGood),
  write_testcase_std_info(Testcase),!.

write_testcase_info(Testcase):- \+ j_u:junit_prop(Testcase,result,passed),!,
  get_nongood_strings(Testcase,NonGood),
  write_message_ele('error',NonGood),
  write_testcase_std_info(Testcase),!.

write_testcase_info(Testcase):- write_testcase_std_info(Testcase),!.

write_message_ele(Ele,NonGood):-
  text_to_string(NonGood,SNonGood), 
  escape_attribute(SNonGood,ENonGood),
  format("      <~w message=\"~w\" />\n", [Ele,ENonGood]).


:- multifile prolog:message//1, user:message_hook/3.
% message_hook_handle(import_private(pfc_lib,_:_/_),warning,_):- source_location(_,_),!.

message_hook_dontcare(import_private(_,_),_,_).
message_hook_dontcare(check(undefined(_, _)),_,_).
message_hook_dontcare(ignored_weak_import(header_sane,_),_,_).
message_hook_dontcare(io_warning(_,'Illegal UTF-8 start'),warning,_):- source_location(_,_),!.
message_hook_dontcare(undefined_export(jpl, _), error, _):- source_location(_,_),!.
message_hook_dontcare(_, error, _):- source_location(File,4235),atom_concat(_,'/jpl.pl',File),!.


message_hook_handle(Term, Kind, Lines):- message_hook_dontcare(Term, Kind, Lines),!.
message_hook_handle(message_lines(_),error,['~w'-[_]]). 
message_hook_handle(error(resource_error(portray_nesting),_),
   error, ['Not enough resources: ~w'-[portray_nesting], nl,
      'In:', nl, '~|~t[~D]~6+ '-[9], '~q'-[_], nl, '~|~t[~D]~6+ '-[7], 
        _-[], nl, nl, 'Note: some frames are missing due to last-call optimization.'-[], nl, 
        'Re-run your program in debug mode (:- debug.) to get more detail.'-[]]).
message_hook_handle(T,Type,Term):- 
  ((current_prolog_flag(runtime_debug, N),N>2) -> true ; source_location(_,_)),
  memberchk(Type,[error,warning]),once(inform_message_hook(T,Type,Term)),fail.

:- if( \+ current_prolog_flag(test_completed,_)).
:- if(set_prolog_flag(test_completed,test_completed)). :- endif.
:- endif.
% :- if((current_prolog_flag(test_completed,TC),writeln(test_completed=TC))). :-endif.

:- if(current_predicate(fixup_exports/0)).
:- fixup_exports.
:- endif.

:- system:import(junit_term_expansion/2).
:- system:import(junit_goal_expansion/2).

:- multifile prolog:message//1, user:message_hook/3.
:- dynamic prolog:message//1, user:message_hook/3.
:- module_transparent prolog:message//1, user:message_hook/3.

user:message_hook(T,Type,Term):- 
   notrace((
   Type \== silent, Type \== debug, Type \== informational,
   current_prolog_flag(logicmoo_message_hook,Was),Was\==none,Was\==false)),
   once(message_hook_handle(T,Type,Term)),!.

system:term_expansion(I,P,O,PO):- notrace((nonvar(P),is_junit_test, junit_term_expansion(I,O))),P=PO.
system:goal_expansion(I,P,O,PO):- notrace((nonvar(P),is_junit_test, junit_goal_expansion(I,O))),P=PO.
  
