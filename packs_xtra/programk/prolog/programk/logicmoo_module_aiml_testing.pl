% ===================================================================
% File 'logicmoo_module_aiml_main.pl'
% Purpose: To load and test the AIML interpretor (sanity checks)
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml_main.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

:-ensure_loaded(logicmoo_module_aiml_toplevel).

:-dynamic(recordedTime/2).           
timeRecorded(Call):-timeRecorded(Call,Time),asserta(recordedTime(Time,Call)),listing(recordedTime/2),!.
timeRecorded(Call,Time):- statistics(cputime,Start),context_module(M),prolog_statistics:time(M:Call),statistics(cputime,End),Time is End - Start.
% :-catch(noguitracer,E,writeq(E)),nl.

save:-tell(aimlCate),
   aimlCateSig(CateSig),
   listing(CateSig),
   listing(dict),
   told,
   predicate_property(CateSig,number_of_clauses(N)),
   predicate_property(dict(_,_,_),number_of_clauses(ND)),
   debugFmt([aimlCate=N,dict=ND]),!.

dt:- withAttributes(Ctx,[graph='ChomskyAIML'],load_aiml_files(Ctx,'chomskyAIML/*.aiml')).

do:-load_aiml_files,alicebot.


hasLibrarySupport :- absolute_file_name(library('programk/logicmoo_module_aiml_toplevel.pl'),File),exists_file(File).
throwNoLib:-  absolute_file_name('.',Here),throw(error(existence_error(url, Here), context(_, status(404, Here)))).

% up one if we are in same directory
 up_one_if :- absolute_file_name('./logicmoo_module_aiml_toplevel.pl',File),(exists_file(File)->cd('../');true).

%:- leash(-all).

%:- trace.

%:- up_one_if.


% if not has library suport, add this directory as a library directory
addSupportHere:-
  absolute_file_name('logicmoo_module_aiml_toplevel.pl',File),
  (exists_file(File)-> ((
  absolute_file_name('.',Here),
  asserta(library_directory(Here))));true).

%:-not(hasLibrarySupport)->addSupportHere;true.

%:-hasLibrarySupport->true;throwNoLib.

% goal is to remove this next line and have it work!
%%:-ensure_loaded(library('autolog/autolog.pl')).
%:-ensure_loaded(library('programk/logicmoo_module_aiml_toplevel.pl')).


%:-ensure_loaded('bootstrap.aiml.pl').

% :- tell(listing1),listing,told.

dtt:- timeRecorded(dt),statistics,alicebot.

dttt:-timeRecorded(consult(aimlCate_checkpoint)),alicebot.

:-catch(noguitracer,_,true).
:-traceAll.

% :-asserta((portray_text:do_portray_text(X) :- writeq(p(X)))).

% :-list_undefined.

% :-debug.

%:-dttt.
%:-do.
%:-load_aiml_files.
%:-debug,run_chat_tests.
%:-main_loop.
% :-'trace'(findall/3,[-all]).

stdCatchAll:-
 % Catch all
   assert_cate_in_load(aimlCate(*,*,*,*,*,*,*,*,*,*,
      [element(srai,[],['STDCATCHALL',star(pattern,[],[])])],
       element(category,[],[element(pattern,[],[*]),
        element(template,[],[element(srai,[],['STDCATCHALL',element(star,[],[])])])]),
    'c:/development/opensim4opencog/bin/cynd/programk/test_suite/customtagtest.aiml':737-20056,aruleStd2)),
 % Complain
  assert_cate_in_load(aimlCate(*,*,*,*,*,['STDCATCHALL',*],*,*,*,*,
    ['ERROR',understanding,:,star(pattern,[],[])],
     element(category,[],[element(pattern,[],['STDCATCHALL *']),
      element(template,[],['ERROR understanding:',element(star,[],[])])]),
   'c:/development/opensim4opencog/bin/cynd/programk/test_suite/customtagtest.aiml':44-3205,aruleStd3)).

unusedCates:-assert_cate_in_load(aimlCate(*,*,*,*,*,[34],*,*,*,*,element(template,[],[element(srai,[],[1])]),foo3,'c:/development/opensim4opencog/bin/cynd/programk/test_suite/customtagtest.aiml':44-3205)),
 assert_cate_in_load(aimlCate(*,*,*,*,*,['34'],*,*,*,*,element(template,[],[element(srai,[],[2])]),foo3,'c:/development/opensim4opencog/bin/cynd/programk/test_suite/customtagtest.aiml':44-3205)),
 assert_cate_in_load(aimlCate(*,*,*,*,*,[35],*,*,*,*,element(template,[],[element(srai,[],[3])]),foo3,'c:/development/opensim4opencog/bin/cynd/programk/test_suite/customtagtest.aiml':44-3205)),
 assert_cate_in_load(aimlCate(*,*,*,*,*,['35'],*,*,*,*,element(template,[],[element(srai,[],[4])]),foo3,'c:/development/opensim4opencog/bin/cynd/programk/test_suite/customtagtest.aiml':44-3205)),
 assert_cate_in_load(aimlCate(*,*,*,*,*,[37],*,*,*,*,element(template,[],[element(srai,[],['6'])]),foo3,'c:/development/opensim4opencog/bin/cynd/programk/test_suite/customtagtest.aiml':44-3205)),
 assert_cate_in_load(aimlCate(*,*,*,*,*,['38'],*,*,*,*,element(template,[],[element(srai,[],['7'])]),foo3,'c:/development/opensim4opencog/bin/cynd/programk/test_suite/customtagtest.aiml':44-3205)),
 assert_cate_in_load(aimlCate(*,*,*,*,*,[39],*,*,*,*,element(template,[],[element(srai,[],['8'])]),foo3,'c:/development/opensim4opencog/bin/cynd/programk/test_suite/customtagtest.aiml':44-3205)),
 assert_cate_in_load(aimlCate(*,*,*,*,*,['40'],*,*,*,*,element(template,[],[element(srai,[],['9'])]),foo3,'c:/development/opensim4opencog/bin/cynd/programk/test_suite/customtagtest.aiml':44-3205)).

%%chomskyAIML:-catch(consult(chomskyAIML),_,fail),!.
chomskyAIML:-once(load_aiml_files(('chomskyAIML/*.aiml'))).

test_suite_files:-once(load_aiml_files(('test_suite/*.aiml'))).

loadBasicDictionaries:-once(load_aiml_files(('test_suite/ProgramD/predicates.xml'))),fail.
loadBasicDictionaries:-once(load_aiml_files(('test_suite/ProgramD/properties.xml'))),fail.
loadBasicDictionaries:-once(load_aiml_files(('test_suite/ProgramD/substitutions.xml'))),fail.
loadBasicDictionaries.

run_chat_tests_here(Ctx):-     
   timeRecorded(test_suite_files),
   timeRecorded(test_call(alicebotCTX(Ctx,'qt'))),
   timeRecorded(test_call(alicebotCTX(Ctx,'qt1'))),!.

run2(Ctx):-
   %%test_call(alicebotCTX(Ctx,'Hi')),
   test_call(alicebotCTX(Ctx,'What is your name')),
   test_call(alicebotCTX(Ctx,'What is your thing')),
   test_call(alicebotCTX(Ctx,'My name is Fred.')),
   test_call(alicebotCTX(Ctx,'what is my name?')).

blackjack_test_load:-  test_call(alicebot('@load blackjack.aiml')).
blackjack_test:-
   test_call(alicebotCTX(Ctx,'blackjack')),
   test_call(alicebotCTX(Ctx,'d')),
   test_call(alicebotCTX(Ctx,'3')),!.

annie:-withNamedContext(toplevel,Ctx),timeRecorded(run_chat_tests_here(Ctx)),unify_listing(unitTestResult(unit_failed,_)).

%%:-timeRecorded(test_suite_files).

%%:-timeRecorded(blackjack_test_load).
/*
:-timeRecorded(load_aiml_files('chomskyAIML/update007.aiml')).
:-timeRecorded(blackjack_test_load).
:-timeRecorded(blackjack_test).
%:-timeRecorded(chomskyAIML).
:-timeRecorded(load_aiml_files('chomskyAIML/update013.aiml')).
:-timeRecorded(load_aiml_files('chomskyAIML/update012.aiml')).
*/



%%:-unify_listing(unitTestResult(unit_passed,_)).

%%:-timeRecorded(ppfs('../aiml/chomskyAIML/*.aiml')).


%%:-timeRecorded(ppfs('../aiml/std_alice/*.aiml')).

%%:-timeRecorded(load_aiml_files('chomskyAIML/*.aiml')).
%%:-timeRecorded(alicebot).

:-stdCatchAll.
:-alicebot('<category><pattern>*</pattern><that>what was it</that><template><think><set name="it"><star/></set></think></template></category>').

:-alicebot('<category><pattern>pppp</pattern><template>555555</template><that>*</that></category>').
:-alicebot('<category><pattern>suggest a topic</pattern><template><srai>random topic</srai></template><that>*</that></category>').
stdalice:-timeRecorded(load_aiml_files('std_alice/*.aiml')),!. %%timeRecorded(load_aiml_files('std_alice/hide/*.aiml')).

%%:-initialization((stdalice,statistics)).

saveAIMLCore :- tell('aimlCore.pl'),listing(aimlCate),listing(argNFound),listing(dict),told.
saveAIMLCore2 :- tell('aimlCore2.pl'),listing(aimlCate),listing(argNFound),listing(dict),told.

%%:-initialization(timeRecorded(alicebot)).

blastAll:-aimlCateSig(Sig),retractall(Sig),fail.
blastAll:-retractall(argNFound(_,_,_,_)),fail.
blastAll.

%% :-sdtCatchAll.

:-timeRecorded(annie).
:-unify_listing(unitTestResult(unit_passed,_)).
:-unify_listing(unitTestResult(unit_failed,_)).


end_of_file.

:-timeRecorded(alicebot).

%%:-timeRecorded(load_aiml_files('special/*.aiml')).


stdalice: 

517.27 seconds cpu time for 813,888,978 inferences
87,573 atoms, 14,811 functors, 6,105 predicates, 118 modules, 2,906,150 VM-codes

                       Limit    Allocated       In use
Heap         :                              36,362,224 Bytes
Local  stack : 1,073,741,824    2,093,056        8,272 Bytes
Global stack : 1,073,741,824 1,073,737,720 1,042,914,328 Bytes
Trail  stack : 1,073,741,824  268,433,408        3,448 Bytes




519.78 seconds cpu time for 813,933,333 inferences
84,379 atoms, 14,811 functors, 6,105 predicates, 118 modules, 2,357,066 VM-codes

                       Limit    Allocated       In use
Heap         :                              30,136,344 Bytes
Local  stack : 1,073,741,824    2,093,056        8,272 Bytes
Global stack : 1,073,741,824 1,073,737,720 1,042,914,328 Bytes
Trail  stack : 1,073,741,824  268,433,408        3,448 Bytes

63 atom garbage collections gained 344,622 atoms in 34.75 seconds.
Stack shifts: 5 local, 21 global, 18 trail.
1 threads, 0 finished threads used 0.00 seconds.



'PP'=number_of_clauses(20230)]


?521.59 seconds cpu time for 813,992,073 inferences
83,598 atoms, 14,812 functors, 6,106 predicates, 118 modules, 657,131 VM-codes

   Limit    Allocated       In use
Heap         :                              14,549,344 Bytes
Global stack : 1,073,741,824 1,073,737,720 1,042,973,384 Bytes
Trail  stack : 1,073,741,824  268,433,408        3,448 Bytes

65 atom garbage collections gained 365,703 atoms in 36.31 seconds.
Stack shifts: 5 local, 21 global, 18 trail.
1 threads, 0 finished threads used 0.00 seconds.



18 ?- findall(Arg,(aimlCateSig(Call),Call,arg(4,Call,Arg),Arg\=(*)),List),length(List,Len),sort(List,Sort),length(Sort,SLen).
List = [i(have(got(teeth))), are(you(married)), yes(it(is(cool))), that(is(all(i(hear(from(...)))))), the(movies(are(the(only(...))))), are(you(interested(in(...)))), i(know(what(...))), maybe(you(...)), sex(...)|...],
Len = 28080,
Sort = ['39', [], absolutely, adios, again, ah, all, and, ayuh|...],
SLen = 7910.


?-  aimlCateSig(Call),Call,arg(4,Call,Arg),Arg\=(*),aimlCateSig(Call2),arg(6,Call2,Arg),Call2.

 aimlCateArg(that,Aiml2,Arg2),Arg2\=(*),fromIndexableSArg(Arg2,Sarg),


