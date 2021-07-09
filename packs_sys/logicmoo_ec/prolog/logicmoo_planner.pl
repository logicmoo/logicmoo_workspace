:- module(logicmoo_planner,[load_planner_api/0]).


:- style_check(-singleton).

:- use_module(library(prolog_pack)).

:- if( \+ prolog_pack:current_pack(logicmoo_ec)).
:- add_absolute_search_folder(pack,'../../').
:- attach_packs.
:- initialization(attach_packs).
:- endif.

%:-  add_absolute_search_folder(ext,('../ext/')).
:-  add_absolute_search_folder(ext,library('../ext/')).
%:-  add_absolute_search_folder(pack,library('../prolog/../..')).

:- if( \+  user:file_search_path(pddl,_) ).

:-  add_absolute_search_folder(pddl,library('../test/pddl_tests/')),
    add_absolute_search_folder(pddl,library('../test/pddl_reader/')),
    add_absolute_search_folder(pddl,library('../test/uw-yale-pddl/strict-domains/')),
    add_absolute_search_folder(pddl,library('../test/uw-yale-pddl/domains/')),
    !.
         
:- endif.


:- module_transparent(export_transparent/1).
export_transparent(P):-
  export(P),
  module_transparent(P).

% [Required] Load the Logicmoo Library Utils
% :- ensure_loaded(library(logicmoo_hyhtn)).
% 
% load_planner_api:- ensure_loaded(library(rsasak_forward_wa_star_h_add)).
%:- initialization(load_planner, program).
load_planner_api.

test_all:-test_all(7).

test_all(N):- must_filematch_list(pddl('orig_pddl_parser/test/?*?/domain*.pddl'),RList),RList\=[],!,reverse(RList,List),
  forall(member(E,List),once(test_domain(E,N))).

test_all(N):- 
  must_filematch(pddl('orig_pddl_parser/test/?*?/domain*.pddl'),_),!,
  (forall(must_filematch(pddl('orig_pddl_parser/test/?*?/domain*.pddl'),E),
                                                 once(test_domain(E,N)))).

:- thread_local(t_l:loading_files/0).

test_domain(DP):- t_l:loading_files,!,must(test_domain(DP)).
test_domain(DP):- test_domain(DP,12).

min_sas(A,B,A):-A =< B,!.
min_sas(_,A,A).

test_domain(DP,Num):- \+ atom(DP),forall((filematch_smart(DP,FOUND),exists_file(FOUND)),test_domain(FOUND,Num)),!.
test_domain(DP,Num):- \+ exists_file(DP),!, forall(must_filematch(DP,MATCH),(exists_file(MATCH),test_domain(MATCH,Num))).
test_domain(DP,Num):-
   format('~q.~n',[test_domain(DP)]),
  directory_file_path(D,_,DP),directory_files(D,RList),reverse(RList,ListR),
   sort(ListR,ListS),length(ListR,PosNum),min_sas(PosNum,Num,MinNum),length(List,MinNum),append(List,_,ListS),!,
   test_domain_num(DP,List,Num).

test_domain_num(DP,List,Num):-
  directory_file_path(D,_,DP),
  forall(member(T,List),ignore((directory_file_path(D,T,TP),exists_file(TP),not(same_file(DP,TP)),
  planner_solve_files(DP,TP)))).


                                            

% planner_solve_files(+DomainFile, +ProblemFile)
%
%   Reads files and set timelimit for planner
%
planner_solve_files(DomainFile, ProblemFile):- 
 forall(must(must_filematch(DomainFile,DomainFile0)),
   forall(must(must_filematch(ProblemFile,ProblemFile0)),
     (time(show_call(planner_solve_abs_file(DomainFile0, ProblemFile0)))))),!.


% Reads files and set timelimit for planner
planner_solve_abs_file(DomainFile, ProblemFile):- 
       update_changed_files,
       directory_file_path(_,File,ProblemFile),
       wdmsg(planner_solve_files(DomainFile, ProblemFile)),
       planner_solve_abs_file(DomainFile, ProblemFile, File),!.

planner_solve_abs_file(DomainFile, ProblemFile, File):- slow_on(File),!,wdmsg(slow_on(DomainFile, ProblemFile)).
planner_solve_abs_file(DomainFile, ProblemFile, File):-
                nop(time((test_solve_files(DomainFile, ProblemFile)))),
                (time(show_call(logicmoo_ocl_and_pddl:solve_files_w_ocl(DomainFile, ProblemFile)))),
                nop(time(show_call(solve_files_w_lps(DomainFile, ProblemFile)))),
                !.

test_solve_files(D,P):- call(call,solve_files,D,P).

test_dir_sas(DirIn):-forall(must_filematch(DirIn,DirInM),test_dir_m(DirInM)).
test_dir_m(DIR):-
  working_directory(WAS,WAS),
     call_cleanup(( 
        cd(DIR),
        wdmsg(test_dir_files_sas(DIR)),
	%write('Testing ':DIR), nl,
	test_dir_files_sas(DIR)),
        cd(WAS)).

test_dir_files_sas(PDDLDir,D,P):- filematch_smart(PDDLDir,Dir), 
        directory_file_path(Dir,D,DF),
        directory_file_path(Dir,P,PF),
        nop((test_parse_file(DF),test_parse_file(PF))),
        planner_solve_files(DF,PF),!.


test_dir_files_sas(Dir):-   
	test_dir_files_sas(Dir,'p01-domain.pddl','p01.pddl'),
	test_dir_files_sas(Dir,'p02-domain.pddl','p02.pddl'),
	test_dir_files_sas(Dir,'p03-domain.pddl','p03.pddl'),
	test_dir_files_sas(Dir,'p04-domain.pddl','p04.pddl'),
	test_dir_files_sas(Dir,'p05-domain.pddl','p05.pddl'),
	test_dir_files_sas(Dir,'p06-domain.pddl','p06.pddl'),
	test_dir_files_sas(Dir,'p07-domain.pddl','p07.pddl'),
	test_dir_files_sas(Dir,'p08-domain.pddl','p08.pddl'),
	test_dir_files_sas(Dir,'p09-domain.pddl','p09.pddl'),
	test_dir_files_sas(Dir,'p10-domain.pddl','p10.pddl'),
	test_dir_files_sas(Dir,'p11-domain.pddl','p11.pddl'),
	test_dir_files_sas(Dir,'p12-domain.pddl','p12.pddl'),
	test_dir_files_sas(Dir,'p13-domain.pddl','p13.pddl'),
	test_dir_files_sas(Dir,'p14-domain.pddl','p14.pddl'),
	test_dir_files_sas(Dir,'p15-domain.pddl','p15.pddl'),
	test_dir_files_sas(Dir,'p16-domain.pddl','p16.pddl'),
	test_dir_files_sas(Dir,'p17-domain.pddl','p17.pddl'),
	test_dir_files_sas(Dir,'p18-domain.pddl','p18.pddl'),
	test_dir_files_sas(Dir,'p19-domain.pddl','p19.pddl'),
	test_dir_files_sas(Dir,'p20-domain.pddl','p20.pddl'),
	test_dir_files_sas(Dir,'p21-domain.pddl','p21.pddl'),
	test_dir_files_sas(Dir,'p22-domain.pddl','p22.pddl'),
	test_dir_files_sas(Dir,'p23-domain.pddl','p23.pddl'),
	test_dir_files_sas(Dir,'p24-domain.pddl','p24.pddl'),
	test_dir_files_sas(Dir,'p25-domain.pddl','p25.pddl'),
	test_dir_files_sas(Dir,'p26-domain.pddl','p26.pddl'),
	test_dir_files_sas(Dir,'p27-domain.pddl','p27.pddl'),
	test_dir_files_sas(Dir,'p28-domain.pddl','p28.pddl'),
	test_dir_files_sas(Dir,'p29-domain.pddl','p29.pddl'),
	test_dir_files_sas(Dir,'p30-domain.pddl','p30.pddl').

twhy
  :- show_call(record_time(forall(between(1,1000000,_),forall(get_action_bb(_),true)),_Time1)),
   show_call(record_time(forall(between(1,1000000,_),forall(actn(_,_),true)),_Time2)).

slow_on('blocks-07-0.pddl').
slow_on('blocks-08-0.pddl').
slow_on('blocks-09-0.pddl').
slow_on('hanoi7.pddl').
slow_on('hanoi8.pddl').

test_blocks:- time(notrace(test_blocks0)).

test_blocks0:- planner_solve_files(pddl('orig_pddl_parser/test/blocks/domain-blocks.pddl'), 
   pddl('orig_pddl_parser/test/blocks/blocks-03-0.pddl')), fail.
test_blocks0:- fail, must_filematch_list(pddl('orig_pddl_parser/test/blocks/domain*.pddl'),RList),reverse(RList,List),
        forall(member(E,List),once(test_domain(E))),fail.
test_blocks0:- must_filematch_list(pddl('orig_pddl_parser/test/?*?/domain*.pddl'),RList),reverse(RList,List),
        forall(member(E,List),once(test_domain(E))),fail.
test_blocks0:- !.

:- dynamic(pddl_test_unit/1).
:- multifile(pddl_test_unit/1).
:- discontiguous(pddl_test_unit/1).
pddl_test_unit(blocks) :- test_blocks.

:- fixup_exports.

pddl_test_unit(test_sas):- 
      test_dir_sas('ipc2008-no-cybersec/seq-sat/elevators-strips/'),!, % NO FIRST ANSWER
      !.

pddl_test_unit(test_sas_sanity):- 
      test_dir_sas('ipc2008-no-cybersec/seq-opt/openstacks-strips/'), %PASSES BUT RUNS SLOW
       test_dir_sas('ipc2008-no-cybersec/seq-opt/transport-strips/'), %PASSES BUT RUNS
      test_dir_sas('ipc2008-no-cybersec/netben-opt/elevators-strips/'), % FAIL ALL
      !.


pddl_test_unit(test_rest):-	
	test_dir_sas('ipc2008-no-cybersec/seq-opt/parcprinter-strips/'),
	test_dir_sas('ipc2008-no-cybersec/seq-opt/pegsol-strips/'),
	test_dir_sas('ipc2008-no-cybersec/seq-opt/scanalyzer-strips/'),
	test_dir_sas('ipc2008-no-cybersec/seq-opt/sokoban-strips/'),  % NO FIRST ANSWER
       
	test_dir_sas('ipc2008-no-cybersec/seq-opt/woodworking-strips/'),
	
        
        expand_file_name('ipc2008-no-cybersec/?*?/*/',O),
        forall(member(E,O),test_dir_sas(E)).


pddl_test_unit(frolog) :- test_dir_files_sas('frolog','p02-domain.pddl','p02.pddl'),
    test_dir_files_sas('frolog','tPddlAgent01-domain.pddl','tPddlAgent01.pddl'),
    !. % test_dir_files_sas('frolog','tPddlAgent02-domain.pddl','tPddlAgent02.pddl').

pddl_test_unit(mystery) :- planner_solve_files(pddl('benchmarks/mystery/domain.pddl'),pddl('benchmarks/mystery/prob01.pddl')).
pddl_test_unit(driverlog) :- test_domain(pddl('benchmarks/driverlog/domain.pddl'),4).
pddl_test_unit(woac) :- planner_solve_files(pddl('hsp2_1_0_pddl/parcprinter-strips/p01-domain-woac.pddl'),
               pddl('hsp2_1_0_pddl/parcprinter-strips/p01-woac.pddl')).

pddl_test_unit(chameleonWorld) :- test_domain(pddl('../domains_ocl/chameleonWorld/domain*')).

/*
:- if(current_predicate(pce_show_profile/0)).
:- pce_show_profile.
:- endif.
*/

% :- twhy.

% BAD 
pddl_test_unit(elearning) :- test_domain(pddl('elearning/domain.pddl')).

pddl_test_unit(sat11) :- planner_solve_files(pddl('benchmarks/nomystery-sat11-strips/domain.pddl'),pddl('benchmarks/nomystery-sat11-strips/p01.pddl')).
pddl_test_unit(sat11_2) :- test_domain(pddl('benchmarks/nomystery-sat11-strips/domain.pddl')).
pddl_test_unit(toasterWorldv2) :- test_domain(pddl('../domains_ocl/toasterWorldv2/domain*')).
pddl_test_unit(rover) :- forall(must_filematch(pddl('rover/?*?/?*domain*.*'),E),once(test_domain(E))).
pddl_test_unit(hsp_pddl) :- forall(must_filematch(pddl('hsp-planners-master/?*?/pddl/?*?/?*domain*.*'),E),once(test_domain(E))).
pddl_test_unit(hsp_examples) :- forall(must_filematch(pddl('hsp-planners-master/?*?/examples/?*?/?*domain*.*'),E),once(test_domain(E))).
pddl_test_unit(primaryobjects_strips) :- forall(must_filematch(pddl('primaryobjects_strips/?*?/?*domain*.*'),E),once(test_domain(E))).
pddl_test_unit(monkey) :- planner_solve_files(pddl('hakank-pddl/monkey-domain.pddl'),pddl('hakank-pddl/monkey-prob01.pddl')).

:- fixup_exports.

:-thread_local(t_l:hyhtn_solve/1).
% t_l:other_planner(hyhtn_solve).


:- flag(time_used,_,0).
:- flag(time_used_other,_,0).

:- if(gethostname(c3po);gethostname(ubuntu);gethostname(titan)).
% :- debug,(must(test_blocks)).
% :- test_all(5). % should be 7
% :- test_all(7).
:- endif.
%:- show_call(flag(time_used_other,W,W)).
%:- show_call(flag(time_used,W,W)).

% :- xlisting(logicmoo_planner).
% :- xlisting(test).

:- fixup_exports.




