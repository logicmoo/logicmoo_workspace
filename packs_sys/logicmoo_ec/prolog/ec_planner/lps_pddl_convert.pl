
%:- prolog_load_context(file, File), throw(wrong_file(File)).
% =========================================
% Goal/Plan translating
% =========================================
:- module(lps_pddl_convert,[%load_e/1, needs_proccess/3,process_ec/2,fix_time_args/3,fix_goal/3, 
  %brk_on_bind/1,assert_axiom_2/2,
   assert_tl_pddl/1,
   test_lps_pddl_convert/1,   
   test_lps_pddl_convert/0,   
   lps_pddl_convert/2,
   lps_pddl_convert/1]).
                     

:- use_module(library(logicmoo_common)).

/*export_transparent(P):-
  export(P),
  module_transparent(P).

   A big problem with distruted wi
   Imagine for the moment the current weights are ideally fitted for a current task
   Any new capablity (fitting itself towards a new task) will need to not change those weigths too drastically. 
   IOW we have to throttle how much it is allowed to fit towards certain improvements.  
   The consequence being that the type of overhaul it would required for any extreme forward push in skills would break the system
*/
%:- use_module(library(logicmoo_lps)).
:- use_module(library(ec_planner/ec_lps_convert)).
:- use_module(library(wam_cl/sreader)).
:- use_module(library(hyhtn_pddl/rsasak_pddl_parser)).
% system:pddl_current_domain(X):- wdmsg(pddl_current_domain(X)),fail.
%:- user:use_module(library('pddl_planner/pddl_planner_dmiles')).
%:- use_module(library(pddl_planner/pddl_reader)).

:- use_module(library(lps_corner)).

:- set_prolog_flag(lps_translation_only_HIDE,false).
:- set_prolog_flag(lps_translation_only,false).


assert_tl_pddl('$COMMENT'(Cmt,_,_)):- !,
  print_tree_cmt('PDDL COMMENT',blue,Cmt).
assert_tl_pddl(Stuff):- 
 must_or_rtrace_l(lps_pddl_convert:(
  map_nonvars(p,[In,Out]>>((atom(In), \+ atom_contains(In,"/"), downcase_atom(In,Out))),Stuff,DCStuff0),
  with_kif_ok(to_untyped(DCStuff0,DCStuff1)),
  deconflict_pddl(DCStuff1,DCStuff),  
  assert_pddl([],DCStuff))),
  call(print_tree_cmt('Translated',green,DCStuff)),!.

deconflict_pddl(I,I):- \+ sub_atomz(I,at),!.
deconflict_pddl(I,R):- sub_term(L,I), is_list(L), L = [at|More],last(More,Last),atom(Last), atom_concat('at_',Last,NewAt),
  subst(I,at,NewAt,R),!.
deconflict_pddl(I,I):- !.



skipped_pddl_file('4').
skipped_pddl_file('5').
skipped_pddl_file('6').
skipped_pddl_file('7').
skipped_pddl_file('8').
skipped_pddl_file('/prob01').
skipped_pddl_file('/prob02').
skipped_pddl_file('/prob03').
skipped_pddl_file('/prob04').
%skipped_pddl_file('/prob05').
skipped_pddl_file('/prob06').
skipped_pddl_file('/prob07').
skipped_pddl_file('/prob08').
skipped_pddl_file('/prob09').

skipped_pddl_file('/prob1').
skipped_pddl_file(File):- tmp_pddl:took_test(File,_,_).
skipped_pddl_file(File):- atom(File), exists_file(File), size_file(File,Size), Size > 35535.
:- dynamic(tmp_pddl:took_test/3).
:- volatile(tmp_pddl:took_test/3).

assert_overwrite(M:Pred):- duplicate_term(Pred,Dupe),functor(Dupe,_,A),nb_setarg(A,Dupe,_),
 retractall(M:Dupe),
 asserta(M:Pred).

include_e_lps_pddl_file_now(Type,MFile):- strip_module(MFile,M,File), include_e_lps_pddl_file_now(Type,M,File).
include_e_lps_pddl_file_now(Type,M,File):- absolute_file_name(File,AbsFile),File\==AbsFile,exists_file(AbsFile), !,include_e_lps_pddl_file_now(Type,M,AbsFile).

%include_e_lps_pddl_file_now(_Type,_Ctx,File):- 
%   with_lisp_translation(File,pprint_ecp(yellow)),!.

include_e_lps_pddl_file_now(_Type,_Ctx,File):- skipped_pddl_file(File),!.
include_e_lps_pddl_file_now(_Type,_Ctx,File):- skipped_pddl_file(S), atom_contains(File,S),!.

include_e_lps_pddl_file_now(Type,Ctx,File):-
  assert_overwrite(tmp_pddl:took_test(File,state,started)),
  with_all_rest_info(writeln),
  include_e_lps_pddl_file_no_output(Type,Ctx,File),
  do_stored_pddl_stuff.
  
include_e_lps_pddl_file_now_output(Type,Ctx,File):-
  guess_output_name(Type,File,OutputFile),
  file_directory_name(OutputFile,Dir),
  make_directory_path(Dir),
  setup_call_cleanup(
    open(OutputFile,write,OS),
    include_e_lps_pddl_file_no_output(Type,Ctx,File),
    close(OS)),!.
   

include_e_lps_pddl_file_no_output(_Type,_Ctx,File):-   
    with_lisp_translation(File,assert_tl_pddl).
%include_e_lps_pddl_file_no_output(_Type,_Ctx,File):-   
%    with_lisp_translation(File,store_pddl_stuff).

:- dynamic(tmp_pddl:stored_pddl_stuff/1).

store_pddl_stuff(Stuff):- 
  assertz(tmp_pddl:stored_pddl_stuff(Stuff)),!.

do_stored_pddl_stuff:- 
 Pred1= assert_tl_pddl,
 forall(clause(tmp_pddl:stored_pddl_stuff(O2),_,Ref),
  (zalwayz(ignore(call(Pred1,O2))),erase(Ref))),!.


guess_output_name(Type,File,OutputFile):-
  file_directory_name(File,Dir),
  file_base_name(Dir,Dir0),
  file_base_name(File,ShortName),
  atomic_list_concat(['/tmp/lmws/',Dir0,'-',ShortName,'.',Type],OutputFile),!.
  
  
load_e_lps_pddl_file(Type,File):- %update_changed_files,  
  retractall(etmp:pddl_option(load(_), _)), 
  include_e_lps_pddl_file(Type,File).
%load_e_lps_pddl_file(Type,File):- retractall(etmp:pddl_option(load(_), _)), include_e_lps_pddl_file(Type,File).


include_e_lps_pddl_file(Type,File):- is_list(File), !, maplist(include_e_lps_pddl_file(Type),File).
include_e_lps_pddl_file(Type,File):- wdmsg(include_e_lps_pddl_file(Type,File)),fail.
include_e_lps_pddl_file(Type,File):- needs_resolve_local_files(File,Resolved),!,include_e_lps_pddl_file(Type,Resolved).
include_e_lps_pddl_file(Type,File):- absolute_file_name(File,DB), exists_file(DB),!, 
  was_s_l(File,1),  
  strip_module(_,M,_), prolog_statistics:time(M:include_e_lps_pddl_file_now(Type,File)),!.
include_e_lps_pddl_file(Type,File):- throw(with_abs_paths(include_e_lps_pddl_file(Type),File)).

lps_pddl_convert(File):- compound(File),pre_pddl_tests,fail.
lps_pddl_convert(File):- update_changed_files, lps_pddl_convert(lps, File).
lps_pddl_convert(Proc1,File):- load_e_lps_pddl_file(Proc1,File).

solve_files_w_lps(DomainFile, ProblemFile):- 
  lps_pddl_convert(ProblemFile),!,
%  parseProblem(ProblemFile,PStuff),%break,
  %pprint_ecp(blue,PStuff),!, break,
  %parseDomain(DomainFile,Stuff), pprint_ecp(yellow,Stuff),!, % break,
  lps_pddl_convert(DomainFile),
  !.

sanity_breaks:- 
 pre_pddl_tests,
 l_open_input('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/pddl_tests/benchmarks/airport/p29-airport4halfMUC-p8.pddl',Stream),
 set_stream(Stream,buffer_size(2560000)),
  with_lisp_translation(Stream,writeln).

pre_pddl_tests:- retractall(tmp_pddl:took_test(_,_,_)),!.

sanity_rtrace:- rtrace(lps_pddl_convert(pddl('benchmarks/airport/p29-airport4halfMUC-p8.pddl'))).

test_lps_pddl_convert:-  
 pre_pddl_tests,
  lps_pddl_convert(pddl('orig_pddl_parser/test/blocks/domain-blocks.pddl')),
  lps_pddl_convert(pddl('orig_pddl_parser/test/blocks/blocks-03-0.pddl')),
 !.

test_lps_pddl_convert(S):- nonvar(S), listify(S,List),
  ((select(L,List,Rest),(number(L);var(L)))->true;L=_),
  findall(Test-Body, (clause(test_lps_pddl_convert(Decl),Body), ec:match_test(Rest,Decl)), Candidates),
  forall((nth0(L,Candidates,Test-Body),
  pprint_ecp_cmt(yellow,(test_lps_pddl_convert(Test):-Body))),
  once(call(Body))).
  
 
test_lps_pddl_convert([convert]):- 
 pre_pddl_tests,
 lps_pddl_convert(pddl('assembly/domain*1.pddl')),
 lps_pddl_convert(pddl('assembly/prob20.pddl')),
 !.

test_lps_pddl_convert([convert]):- 
 pre_pddl_tests,
lps_pddl_convert(ext('flp/worlds/flp/flp.d.pddl')),
lps_pddl_convert(ext('flp/worlds/flp/flp.p.pddl')),
 !.

test_lps_pddl_convert([convert]):- 
 pre_pddl_tests,
 lps_pddl_convert(pddl('truckworld/domain*1.pddl')),
 lps_pddl_convert(pddl('truckworld/pro*03.pddl')),
 !.



:- add_history((cls, test_lps_pddl_convert)).

test_lps_pddl_convert([convert]):- 
  pre_pddl_tests,
  lps_pddl_convert(pddl('benchmarks/elevators-opt11-strips/domain.pddl')), 
  lps_pddl_convert(pddl('benchmarks/nomystery-opt11-strips/domain.pddl')),
  lps_pddl_convert(pddl('transplan/domain.pddl')),
  lps_pddl_convert(pddl('briefcase/domain01.pddl')),
  lps_pddl_convert(pddl('ZenoTravel/zeon_p14a_dp.pddl')),
  lps_pddl_convert(pddl('benchmarks/airport/p29-airport4halfMUC-p8.pddl')),
  lps_pddl_convert(pddl('transplan/domain.pddl')),
  lps_pddl_convert(pddl('elearning/domain.pddl')),
  !.


test_lps_pddl_convert([convert]):- 
  test_lps_pddl_convert(0),
   lps_pddl_convert(pddl('../uw-yale-pddl/*/*/*.pddl')),
   lps_pddl_convert(pddl('*/*.pddl')),
   !.


test_lps_pddl_convert([convert]):- 
 test_lps_pddl_convert(1),
 lps_pddl_convert(pddl('*/*/*/*.pddl')),
 lps_pddl_convert(pddl('*/*/*.pddl')),
 !.

:- ensure_loaded(library(logicmoo/util_structs)).
:- ensure_loaded(library(statistics)).
%:- ensure_loaded(library(logicmoo_util_bb_env)).

test_lps_pddl_convert([solve]):- !,
   planner_solve_files(pddl('orig_pddl_parser/test/blocks/domain-blocks.pddl'), 
      pddl('orig_pddl_parser/test/blocks/blocks-03-0.pddl')),!.
    



into_pterm( Ctx,I,O):- must_or_rtrace_l((with_kif_ok(to_untyped(I,M)),our_sterm2pterm(Ctx,M,O))).

get_svars(P,Vars):- findall(VAR, (sub_term(VAR,P),compound(VAR),VAR='$VAR'(_)), List), list_to_set(List,Vars).



no_cmpd_change(NC):- \+ compound(NC),!,fail.
no_cmpd_change(isa(_,_)).
no_cmpd_change(typed(_,_)).


%is_pddl_special(_,'object').
%is_pddl_special(_,'exists').
%is_pddl_special(_,'forall').
%is_pddl_special(_,'all').

is_pddl_special(_,'imply').
is_pddl_special(typed,'either').
is_pddl_special(at,'start').
is_pddl_special(at,'end').
is_pddl_special([axiom,'durative-action'],'always').
is_pddl_special([axiom,'durative-action'],'sometime').
is_pddl_special([axiom,'durative-action'],'over').
is_pddl_special([axiom,'durative-action'],'at').

is_pddl_special(_,'within').
is_pddl_special(_,'at-most-once').
is_pddl_special(_,'sometime-after').
is_pddl_special(_,'sometime-before').
is_pddl_special(_,'always-within').
is_pddl_special(_,'hold-during').
is_pddl_special(_,'hold-after').
is_pddl_special(_,'total-time').
is_pddl_special(_,'is-violated').


%is_pddl_special(effect,'total-cost').
is_pddl_special([axiom,'durative-action'],'minimize').
is_pddl_special([axiom,'durative-action'],'maximize').
is_pddl_special([axiom,'durative-action'],when).
is_pddl_special([axiom,'durative-action'],while).
is_pddl_special([axiom,'durative-action'],'increase').
is_pddl_special([axiom,'durative-action'],'decrease').
is_pddl_special([axiom,'durative-action'],'assign').
is_pddl_special(effect,'scale-up').
is_pddl_special(effect,'scale-down').


pddl_replace(_Ctx,PRED,_):- \+ atom(PRED), !, fail.
pddl_replace(_,'#f',pddl_f).
pddl_replace(_,'#t',pddl_t).
pddl_replace(_,'-',pddl_minus).
pddl_replace(_,',','&').
pddl_replace(_,'and','&').
pddl_replace(Ctx,PRED,NEWPRED):- is_pddl_special(OkWhen,PRED),is_ok_when(OkWhen,Ctx),atom_concat('pddl-',PRED,NEWPRED).

is_ok_when(V1,V2):- (var(V1); var(V2)),!.
is_ok_when(V1,V2):- sub_atomz(V1,K1),sub_atomz(V2,K2), same_keys(K1,K2),!.

sub_atomz(A,_):- var(A),!,fail.
sub_atomz(A,B):- (\+ compound(A) -> ! ; true), A=B.
sub_atomz(A,B):- arg(_,A,C),sub_atomz(C,B).

our_sterm2pterm(Why,I,O):- listify(Why,Ctx),!, our_sterm2pterm_0(Ctx,I,O).

our_sterm2pterm_0( Ctx,In,Out):- nonvar(Out),!,our_sterm2pterm_0( Ctx,In,OutM),!,must(Out=OutM).
our_sterm2pterm_0(_Ctx,VAR,VAR):- var(VAR),!.

% our_sterm2pterm_0( Ctx,[Compound],Res):- !, our_sterm2pterm_0( Ctx,Compound,Res).
%our_sterm2pterm_0( Ctx,[A|List],Res):- atom(A),is_list(List),!, Res =.. [A|List].
%our_sterm2pterm_0( Ctx,[A|List],Res):- compound(A),is_list(List),!,append_termlist(A,List,Res),!.
%our_sterm2pterm_0( Ctx,List,Res):- Res =..[t|List].
% our_sterm2pterm_0( Ctx,Decl,t(Decl)).
our_sterm2pterm_0(_Ctx,M,'$VAR'(Name)):- with_kif_ok(svar(M,Name)),!.


our_sterm2pterm_0(_Ctx,Decl,Res):- atomic_or_var(Decl),!, Res = Decl. % Was Res = t(Decl)
our_sterm2pterm_0(_Ctx,NC,NC):- no_cmpd_change(NC),!.
our_sterm2pterm_0( Ctx,if(P),O):- !, our_sterm2pterm_0( Ctx,P,O).
our_sterm2pterm_0( Ctx,P,O):- \+ is_list(P), !, P=..[F|ARGS], our_sterm2pterm_0(Ctx,[F|ARGS],O).
our_sterm2pterm_0(_Ctx,KVList,T):- if_into_typed_params(KVList,T).
our_sterm2pterm_0(Ctx,[F|ARGS],O):- \+ member(F,Ctx), !, our_sterm2pterm_0([F|Ctx],[F|ARGS],O).
%our_sterm2pterm_0( Ctx,QDown,'?'(UP)):- \+ is_list(QDown),svar_fixvarname(QDown,UP),!.
%our_sterm2pterm_0(domain,[S],S):-atomic_or_var(S),!. % ,atom_concat(':',_,S),!.
our_sterm2pterm_0( Ctx,[LIST],O):- is_list(LIST), !, our_sterm2pterm_0( Ctx, LIST, O),!.
our_sterm2pterm_0( Ctx,[PRED|LIST],OUT):- atom(PRED),pddl_replace(Ctx,PRED,NEWPRED),!,our_sterm2pterm_0( Ctx,[NEWPRED|LIST],OUT).
our_sterm2pterm_0(_Ctx,[S],S):-atomic_or_var(S), !. % ,atom_concat(':',_,S),!.

our_sterm2pterm_0( Ctx,[all,Decl,H],all(Params,Res)):-!, must_into_typed_params(Decl,Params), our_sterm2pterm_0( Ctx,H,Res).
our_sterm2pterm_0( Ctx,[S,Vars,SLIST],POUT):-atom(S),is_quantifier_type(S,SQ),into_typed_params(Vars,PVars),our_sterm2pterm_0( Ctx,SLIST,TERM),POUT=..[SQ,PVars,TERM].
our_sterm2pterm_0( Ctx,[S,Vars|SLIST],POUT):-atom(S),is_quantifier_type(S,SQ),into_typed_params(Vars,PVars),our_sterm2pterm_list( Ctx,SLIST,TERM),POUT=..[SQ,PVars,TERM].
our_sterm2pterm_0( Ctx,[exists,Decl,H],exists(Params,Res)):-!, must_into_typed_params(Decl,Params), our_sterm2pterm_0( Ctx,H,Res).

our_sterm2pterm_0( Ctx,[(';')|X],Res):- !, our_sterm2pterm_0( Ctx,[or|X],Res).
our_sterm2pterm_0( Ctx,['&',X],Res):- !, our_sterm2pterm_0( Ctx,X,Res).
our_sterm2pterm_0( Ctx,['&',X|L],and(Res,LRes)):- our_sterm2pterm_0( Ctx,X,Res), our_sterm2pterm_0( Ctx,L,[and|LRes]).
our_sterm2pterm_0( Ctx,[or,X],Res):- !, our_sterm2pterm_0( Ctx,X,Res).
our_sterm2pterm_0( Ctx,[or,X|L],or(Res,LRes)):- our_sterm2pterm_0( Ctx,X,Res), our_sterm2pterm_0( Ctx,L,[or|LRes]).
our_sterm2pterm_0( Ctx,[not,X],not(Res)):- !, our_sterm2pterm_0( Ctx,X,Res).
our_sterm2pterm_0( Ctx,[S|SLIST],PTERM):-atom(S), atom_concat(':',_,S),
            our_sterm2pterm_list(Ctx,SLIST,PLIST),           
            PTERM=..[S,PLIST].
our_sterm2pterm_0( Ctx,[S|SLIST],PTERM):-atom(S), \+ svar(S,_),!,            
            our_sterm2pterm_list(Ctx,SLIST,PLIST),           
            PTERM=..[S|PLIST].
our_sterm2pterm_0( Ctx,SLIST,PLIST):- is_list(SLIST),!,our_sterm2pterm_list(Ctx,SLIST,PLIST).
our_sterm2pterm_0(_Ctx,VAR,VAR):-!.

our_sterm2pterm_list(_Ctx,[],[]).
our_sterm2pterm_list(_Ctx,KVList,T):- is_type_param_list(KVList), !, KVList=T.
our_sterm2pterm_list( Ctx,[Item|List],[H|T]):- our_sterm2pterm( Ctx,Item,H),our_sterm2pterm_list(Ctx,List,T).

into_plus_minus(Conds,Pos,Neg):- 
   into_pterm( domain,Conds,PostC),
   into_enables(PostC,Pos),
   into_disables(PostC,Neg).
  



is_empty(Rule):-  (Rule == [] ; Rule == and ; Rule == true; Rule == or ; Rule == (&) ), !.
is_empty([Rule]):- nonvar(Rule), is_empty(Rule).

our_sterm2pterm(I,O):- our_sterm2pterm(domain,I,O).
add_conjuncts(Empty,Types,TypesO):- is_empty(Empty),!,our_sterm2pterm(Types,TypesO).
add_conjuncts(Types,Empty,TypesO):- is_empty(Empty),!,our_sterm2pterm(Types,TypesO).
add_conjuncts(Pre,Types,Out):- ands_to_list(Pre,L1), ands_to_list(Types,L2), append(L1,L2,L3),our_sterm2pterm([&|L3],Out).

ands_to_list(Empty,[]):- is_empty(Empty),!.
ands_to_list([Conj],ListO):- nonvar(Conj),ands_to_list(Conj,ListO),!.
ands_to_list(Conj,ListO):- conj_args(Conj,List),maplist(ands_to_list,List,ListOfLists),append(ListOfLists,ListO),!.
ands_to_list([X|List],[X|List]):-!.
ands_to_list(C,[C]):-!.

% assert_1pddl([constant|Ctx],C):- compound(C),
/*
and_to_comma(Rule, true):- is_empty(Rule).
and_to_comma(Rule0,Rule):- atomic_or_var(Rule0),!, Rule0=Rule.
and_to_comma(Rule0,Rule):- into_pterm(and_to_comma,Rule0,Rule1),and_to_comma_0(Rule1,Rule).

and_to_comma_0(Rule0,Rule):- atomic_or_var(Rule0), !, Rule=Rule0.
and_to_comma_0(and(A,B),(AA,BB)):- !, and_to_comma_0(A,AA),and_to_comma_0(B,BB).
and_to_comma_0(and(A),AA):- !,and_to_comma_0(A,AA).
and_to_comma_0(ANDA,(AA,BB)):- compound(ANDA), ANDA=..[and,A|As],!,compound_name_arguments(B,and,As),and_to_comma_0(A,AA),and_to_comma_0(B,BB).
and_to_comma_0(A,AA):-
   % pddl_to_lps(top, A0, A),
   compound_name_arguments(A,F,As),
   maplist(and_to_comma_0,As,AAs),
   compound_name_arguments(AA,F,AAs).
*/
is_pddl_amethod(Atom):- \+ atom(Atom), !, fail.
is_pddl_amethod(process).
is_pddl_amethod(event).
is_pddl_amethod(action).
is_pddl_amethod(task).
is_pddl_amethod(method).
is_pddl_amethod(KW):- atom_concat('durative-',M,KW),!,is_pddl_amethod(M).
is_pddl_amethod(KW):- un_kw_directive(KW,M),!,is_pddl_amethod(M).
%assert_pddl(Ctx,_,include(F)):- include_e_lps_pddl_file_now(Type,Ctx:F).
%assert_pddl(Ctx,_,load(F)):- include_e_lps_pddl_file_now(Type,Ctx:F). 
%assert_pddl(Ctx,_,include(F)):- !, with_e_file(assert_pddl(Ctx),current_output, [pddl(F)]). 
%assert_pddl(Ctx,_,load(X)):- nop(assert_pddl(Ctx,include(X))),!.

%assert_pddl(Ctx,Form):- 
assert_pddl(Ctx,Form):- \+ compound_gt(Form,0),!,assert_1pddl(Ctx,Form).
assert_pddl(Ctx,t(Type,Inst)):- atom(Type), M=..[Type,Inst],!,assert_pddl(Ctx,M),!.
%assert_pddl(Ctx,Form):- already_lps(Form),!,assert_1pddl(Ctx,Form).
assert_pddl(Ctx,Form):- \+ is_list(Form),!,must_or_rtrace_l(assert_1pddl(Ctx,Form)).

assert_pddl(Ctx,Form):- 
  Form = [ define, Decl|Rest],
  into_pterm( define,Decl,Named),
  assert_pddl([Named|Ctx],Rest),!.

assert_pddl(Ctx,Form):- Form = [ Method, Decl|Rest], is_pddl_amethod(Method),
  into_pterm( named_method,Decl,Named),
  assert_pddl([Method|Ctx],[Named|Rest]),!.

assert_pddl(Ctx,[[KW,Data]|Rest]):-
  kw_directive(KW,NewType),
  kw_soon(Rest),
  assert_pddl([NewType|Ctx],Data),
  assert_pddl(Ctx,Rest),!.

assert_pddl(Ctx,[[KW|Data]|Rest]):- Data\==[],
  kw_directive(KW,NewType),
  kw_soon(Rest),
  assert_pddl([NewType|Ctx],Data),
  assert_pddl(Ctx,Rest),!.
/*
assert_pddl(Ctx,[KW,Data|Rest]):- Data\==[],
  kw_directive(KW,NewType),
  kw_soon(Rest),
  assert_pddl([NewType|Ctx],Data),
  assert_pddl(Ctx,Rest),!.
*/

%assert_pddl([init|Ctx],Data):-  map_pddl_list(assert_pddl([s(initially)|Ctx]),Data).
assert_pddl(Ctx,Data):- \+ is_list(Data),!, assert_pddl(Ctx,[Data]).

assert_pddl([AtomS|Ctx],Data):- atom(AtomS),item_list(Atom,AtomS),!, map_pddl_list(assert_1pddl([Atom|Ctx]),Data).
assert_pddl([One,Ctx],SData):- atom(One),!, assert_1pddl([One,Ctx],SData).
assert_pddl([s(Pred)|Ctx],SData):- our_sterm2pterm(SData,Data), !,assert_1pddl([Pred|Ctx],Data).
assert_pddl(Ctx,[ KW, Decl|Rest]):-  un_kw_directive(KW,Atom), assert_pddl(Ctx,[ Atom, Decl|Rest]), !.
assert_pddl(Ctx,Form):- pprint_ecp_cmt(pink,assert_pddl(Ctx,Form)),fail.
assert_pddl(Ctx,Form):- assert_1pddl(Ctx,Form), !.


not_item_list(vars).
not_item_list(functions).

item_list(_,NIL):- not_item_list(NIL),!,fail.
item_list(event,events).
item_list(fluent,fluents).
item_list(initially,init).
item_list(predicate,predicates).
item_list(invariant,timeless).
item_list(subtype,types).
item_list(Action,Actions):- atom_concat(Action,"s",Actions), \+ atom_concat(_,"s",Action).
% item_list(Action,Actions):- arg_info(domain,Action,arginfo),atom_concat(Action,"s",Actions).


assert_pddl_pairs(_,[]).
assert_pddl_pairs(Ctx,[[N,V]|Form]):- assert_1pddl_pair([N|Ctx],V),assert_pddl_pairs(Ctx,Form).
assert_pddl_pairs(Ctx,[[N|V]|Form]):- assert_1pddl_pair([N|Ctx],V),assert_pddl_pairs(Ctx,Form).
assert_pddl_pairs(Ctx,[N,V|Form]):- assert_1pddl_pair([N|Ctx],V),assert_pddl_pairs(Ctx,Form).

% assert_1pddl_pair(NameCtx,Value):- must_or_rtrace(assert_1pddl(NameCtx,Value)),!.
assert_2pddl_pair(NameCtx,Value):- must_or_rtrace_l(assert_1pddl(NameCtx,Value)),!.

  
kw_soon(Rest):- 
  (Rest ==[] ; 
  (Rest = [KW2|_],kw_directive(KW2,_)); 
  (Rest = [[KW2|_]|_],kw_directive(KW2,_))).

kw_directive(KW,NewType):- atom(KW), atom_concat(':',Stuff,KW), downcase_atom(Stuff,NewType),!.

un_kw_directive(KW,NewType):- kw_directive(KW,NewType),!.
un_kw_directive(KW,NewType):- atom(KW), downcase_atom(KW,NewType),!, KW \==NewType.
 


pddl_type_of(X,Y):- into_typed(X,typed(Y,_)),!.
pddl_type_of(F,F).

pddl_value_of(X,Y):- into_typed(X,typed(_,Y)).
pddl_value_of(F,F).

as_isa_list([],[]).
as_isa_list([G|L],[isa(V,T)|LL]):- into_typed(G,typed(T,V)), as_isa_list(L,LL).
as_isa_list([_|L],LL):- as_isa_list(L,LL).

same_keys(X,Y):- must((unkw_s(X,X1),unkw_s(Y,Y1))), !,  Y1==X1.

into_kwu(N,KW):- freeze(KW,freeze(N,same_keys(KW,N))).
%select_within(N,V,Form,NForm):- nonvar(N), into_kwu(N,KW),!, select_within(KW,V,Form,NForm).
select_within(N,V,Form,NForm):- select([NN,V],Form,NForm),atom(NN),same_keys(NN,N),!.
select_within(N,V,Form,NForm):- select([NN|V],Form,NForm),atom(NN),same_keys(NN,N),!.
select_within(N,V,Form,NForm):- append(Left,[NN,V|Right],Form),atom(NN),same_keys(NN,N),append(Left,Right,NForm),!.
select_within(N,V,Form,NForm):- append(NForm,[NN|V],Form),atom(NN),same_keys(NN,N),!.

pddl_param_type(Ctx,_):-var(Ctx),!,fail.
pddl_param_type(':vars',typed).
pddl_param_type(':parameters',typed).
pddl_param_type(':objects',typed).
pddl_param_type(':constants',typed).
pddl_param_type(':domain-variables',typed).
%pddl_param_type(':condition',effect).
%pddl_param_type(':method',effect).
%pddl_param_type(':durative-action',effect),
%pddl_param_type(Ctx,CtxO):-nonvar(Ctx),Ctx=CtxO.

maybe_convert(_Ctx,N,V0,V,_Else):- 
  pddl_param_type(N,typed),must_into_typed_params(V0,V),!.

maybe_convert(Ctx,N,V0,V,Else):- 
  include(nonvar,[N,Else,V0|Ctx],ACtx), 
  our_sterm2pterm(ACtx,V0,V),!,
  ((V\==[], V0==V) -> debugging_trans(pprint_ecp_cmt(blue, maybe_convert(ACtx,V0,V))) ; true), !.
/*maybe_convert(Ctx,N,V0,V,Else):- 
  member(Why,[N,Else,V0]),
  nonvar(Why),pddl_param_type(Why,What),type_how(Ctx,What,Call),!,call(Call,V0,V),!.
maybe_convert(_Ctx,_,V,V,_):-!.
at_effect(I,O):- our_sterm2pterm(effect,I,O),!.
at_effect(I,I):-!. 

dont_convert(_Effect,I,I).

type_how(_,typed,must_into_typed_params).
type_how(Ctx,Effect,our_sterm2pterm([Ctx,Effect])).
type_how(_,X,dont_convert(X)).
*/

unkw_s(N,KW):- \+ atom(N),!,KW=N.
unkw_s(N,KW):- atom_concat(':',M,N), !, unkw_s(M,KW).
unkw_s(N,KW):- downcase_atom(N,KW).

get_1pair_value(ACtx,(N1;N2),V,Form,NForm,Else):- !, 
  (get_1pair_value(ACtx, N1,V,Form,NForm,Else) -> V\==Else) -> true ; get_1pair_value(ACtx, N2,V,Form,NForm,Else).

get_1pair_value(Ctx, N,V,Form,NForm,Else):- select_within(N,V0,Form,NForm),maybe_convert(Ctx,N,V0,V,Else),!.
get_1pair_value(Ctx, N,V,Form, Form,Else):- V0 = Else,!,maybe_convert(Ctx,N,V0,V,Else).

get_pair_values(_ACtx,[],Form,Form):-!.
get_pair_values(ACtx,ndv(N,E,V),Form,FormOut):- !,must_or_rtrace_l(get_1pair_value(ACtx,N,V,Form, FormOut, E)),!.
get_pair_values(ACtx,prop(N,E),Form,FormOut):- !, get_1pair_value(ACtx,N,V,Form, FormMID, E),!, (V==E -> FormOut=FormMID ; FormOut=[N,V|FormMID]).
get_pair_values(ACtx,[Op|Rest],Form,FormOut):-
  get_pair_values(ACtx,Op,Form,FormM),!,
  get_pair_values(ACtx,Rest,FormM,FormOut).

is_pddl_prop_holder(Term):- \+ atom(Term), !, fail.
is_pddl_prop_holder(length).

assert_2pddl(Lps):- assert_1pddl(db,Lps).

never:- set_prolog_flag(debugger_write_options,
  [quoted(true), max_depth(100), spacing(next_argument)]).
% assert_1pddl([_Ctx],Form):- Form ==[], 

into_pos_negs(NC,[],[]):- is_empty(NC),!.
into_pos_negs(NC,[NC],[]):- atomic_or_var(NC), !.
into_pos_negs(Conj,Pos,Neg):- conj_args(Conj,List),!,into_pos_negs(List,Pos,Neg).
into_pos_negs(Neg,Pos,Negs):- is_neg(Neg,Inv),!,into_pos_negs(Inv,Negs,Pos).
into_pos_negs(lps_at(not(X),T),Pos,Negs):- !, into_pos_negs(lps_at(X,T),Negs,Pos).
into_pos_negs('pddl-over'(all,not(Conj)),Pos,Neg):- !, into_pos_negs(from(Conj,to('$VAR'('T0'),'$VAR'('T9'))),Neg,Pos).
into_pos_negs('pddl-over'(all,Conj),Pos,Neg):- !, into_pos_negs(from(Conj,to('$VAR'('T0'),'$VAR'('T9'))),Pos,Neg).
%into_pos_negs('pddl-over'(all,Conj),Pos,Neg):- !, into_pos_negs('lps_at'(Conj,'$VAR'('T9')),Pos,Neg).
into_pos_negs('pddl-at'(start,Conj),Pos,Neg):- !, into_pos_negs('lps_at'(Conj,'$VAR'('T0')),Pos,Neg).
%into_pos_negs('pddl-at'(start,Conj),Pos,Neg):- !, into_pos_negs(Conj,Pos,Neg).
into_pos_negs('pddl-at'(end,Conj),Pos,Neg):- !, into_pos_negs(Conj,Pos,Neg).
%into_pos_negs('pddl-at'(end,Conj),Pos,Neg):- into_pos_negs('lps_at'(Conj,'$VAR'('T9')),Pos,Neg).
into_pos_negs([A|B],Pos,Neg):- 
  into_pos_negs(A,Pos1,Neg1),
  into_pos_negs(B,Pos2,Neg2),
  append(Pos1,Pos2,Pos),
  append(Neg1,Neg2,Neg).
into_pos_negs(Pos,[Pos],[]).

is_neg(X,Inv):- compound(X),compound_name_arguments(X,F,[Inv]), is_neg_oper(F).
negate_inv(not(X),X).
negate_inv(lps_at(not(X),T),lps_at(X,T)).
negate_inv(lps_at(X,T),lps_at(not(X),T)).
negate_inv(X,not(X)).
is_neg_oper('\\+').
is_neg_oper('not').

conj_args(Var,_):- var(Var),!,fail.
conj_args(Var,[]):- conj_oper(Var),!.
conj_args([F|AB],AB):- conj_oper(F).
conj_args(Conj,AB):- compound_gt(Conj,0), Conj=..[F|AB], conj_oper(F).
conj_oper(NA):- \+ atom(NA), !, fail.
conj_oper(and).
conj_oper(&).
conj_oper(',').

no_t_needed(Var):- var(Var),!,fail.
no_t_needed(&).
no_t_needed(isa(_,_)).
no_t_needed(typed(_,_)).
no_t_needed(lps_at(_,_)).
no_t_needed(from(_,_)).

is_meta_connective(F):- conj_oper(F),!.
is_meta_connective(F):- current_op(EP,_,(=)), current_op(P,_,F), P > EP. % @TODO be less tricky

add_at_time_arg(_T, DoesntT, DoesntT):- no_t_needed(DoesntT),!.
add_at_time_arg(T1,Stuff, lps_at(Stuff,T1)):- \+ compound(Stuff),!.
add_at_time_arg(_T, lps_at(Stuff,T1), lps_at(Stuff,T1)):- !.
add_at_time_arg(T1, Meta, MetaT):- 
  compound_name_arguments(Meta,F,Args), is_meta_connective(F),
  maplist(add_at_time_arg(T1),Args,TArgs),
  compound_name_arguments(MetaT,F,TArgs),!.
add_at_time_arg(T1,Stuff, lps_at(Stuff,T1)).



condition_to_pre(Cond,Pre):- into_pos_negs(Cond,Pos,Neg),
 maplist(negate_inv,Neg,SNeg),
 append(Pos,SNeg,PosNeg),
 Pre=..[&|PosNeg].

make_enabler(Action,PreWithTypes,Enabler):- 
  get_svars(Action+PreWithTypes,PVars), functor(Action,F,_),Enabler =.. [if_possible,F|PVars].

pprint_pddl([A,Ctx],PDDL):- pprint_sexp(green,[Ctx,[A|PDDL]]),!.
pprint_pddl(_Ctx,PDDL):- pprint_sexp(green,PDDL).

pprint_sexp(Color, S):- print_tree_cmt('Translating', Color, S),!.
pprint_sexp(Color, S):- dvars_to_dqvars(S,SQ), with_output_to(string(Str),f_print(SQ,_)),
  pprint_ecp_cmt(Color, Str),!.

dvars_to_dqvars(Term,QTerm):- 
  must_det_l((   
  term_variables(Term,Vars),must_maplist(get_q_vname,Vars,Names),
  copy_term(Term:Vars,CTerm:CVars),
  must_maplist(=,Names,CVars),
  nop(writeq(Term:Vars-->CTerm:CVars)))),!,
  dvars_to_dqvars_p2(CTerm,QTerm),!.

dvars_to_dqvars_p2(Term,CTerm):- 
  once(map_nonvars(p,lps_pddl_convert:dvar_to_qvar,Term,CTerm)),!.

dvar_to_qvar(C,QName):- compound(C), C='$VAR'(V),into_qname(V,QName).

get_q_vname(CVar, QName):- 
 source_variables_l(NamedVars),
 ignore((member(Name=Var,NamedVars), Var == CVar, into_qname(Name,QName))),!.

into_qname(Name,QName):- upcase_atom(Name,UP),atom_concat('?',UP,QName).

assert_1pddl([Length|Ctx], [Domain|Props]):-  is_pddl_prop_holder(Length),!,maplist(assert_1pddl([Length|Ctx]), [Domain|Props]).
assert_1pddl([functions|Ctx],[[Name|Params],'-',Result|More]):-
  must(atom(Name)),
  must_or_rtrace_l((
    must_into_typed_params(Params,RParams),
    maplist(pddl_type_of,RParams,TParams),!,
    compound_name_arguments_maybe_zero(Lps,Name,TParams),
  assert_lps_pl(Ctx,function(Lps,Result)))),
  assert_1pddl([functions|Ctx],More).

assert_1pddl([functions|Ctx],[[Name|Params]|More]):-
  must(atom(Name)),
  must_or_rtrace_l((
    must_into_typed_params(Params,RParams),
    maplist(pddl_type_of,RParams,TParams),!,
    compound_name_arguments_maybe_zero(Lps,Name,TParams),
  assert_lps_pl(Ctx,function(Lps,'any')))),
  assert_1pddl([functions|Ctx],More).
assert_1pddl(Ctx,PDDL):-
  pprint_pddl(Ctx,PDDL),
  assert_2pddl(Ctx,PDDL), !.

	
assert_2pddl([AM|Ctx],[Name|Form]):- is_pddl_amethod(AM),
 ACtx = [axiom|Ctx],
 must_or_rtrace_l((get_pair_values(ACtx,[
     ndv(':vars',[],Vars),
     ndv(':parameters',[],Params),
     prop(':expansion',[]),
     prop(':name',[]),
     prop(':only-in-expansions',[]),
     prop(':duration',1),
     prop(':tasks',[]),     
     ndv((':condition'),['and'],Cond),
     ndv((':precondition'),['and'],PreC),
     ndv(':effect',['and'],Effect0)]
                                   ,Form,ExtraInfo),
   (is_empty(PreC) -> condition_to_pre(Cond,Pre); condition_to_pre(PreC,Pre)),
   condition_to_pre(Effect0,Effect),
   our_sterm2pterm(Form,PForm),   
   get_svars(PForm,SVars),   
   (is_list(Params)-> maplist(pddl_value_of,Params,VParams);VParams=[Params]),
   as_isa_list(Params,PreConds0),
   as_isa_list(Vars,PreConds1),
   append([['&'],PreConds0,PreConds1],Types),
   % must(into_plus_minus(Pre,PrePos,PreNeg)),
   %into_plus_minus(Effect,Enables,Disables),  
   atomic_list_concat(['enabled_',AM,'_',Name],FAM),
   compound_name_arguments_maybe_zero(Enabler,FAM,SVars),
   assert_lps_pl(Ctx,fluents(Enabler)),
   compound_name_arguments_maybe_zero(Action,Name,VParams),
   assert_lps_pl(Ctx,actions(Action)),
   assert_lps_meta(Ctx,kind_oper(Enabler,AM,Action)),
   T1 = '$VAR'('T0'),
   copy_term(Pre,CopyPre),
   add_at_time_arg(T1,Pre,PreT),
   Oopsy = true, % (pprint_ecp_cmt(red,[Name|Form]),break),
   (Pre\==CopyPre -> Oopsy ; true),
   ((Pre == &) -> Oopsy ; true),
   add_conjuncts(Types,PreT,PreWithTypes),
   assert_lps_pl(Ctx,if(lps_at(Enabler,T1),PreWithTypes)),   
   into_pos_negs(Effect,Pos,Neg),
   forall(member(Ef,Pos), assert_lps_pl(Ctx,if( initiates(Action,Ef),Enabler))),
   forall(member(Ef,Neg), assert_lps_pl(Ctx,if(terminates(Action,Ef),Enabler))),
   nop(assert_lps_meta(Ctx,if_oper_effect(Enabler,Pre,Action,Effect))),
   nop(assert_pddl_pairs([Enabler|Ctx],ExtraInfo)))).

assert_2pddl(Last,[]):- last(Last,domain(_)), !.

assert_2pddl([predicate|Ctx],[Name|Params]):- 
  must(atom(Name)),
  must_or_rtrace_l((
    must_into_typed_params(Params,RParams),
    maplist(pddl_type_of,RParams,TParams),!,
    compound_name_arguments_maybe_zero(Lps,Name,TParams),
  assert_lps_pl(Ctx,fluents(Lps)))).

assert_2pddl([invariant|Ctx],[Name|Params]):- 
  must(atom(Name)),
  must_or_rtrace_l((
    must_into_typed_params(Params,RParams),
    maplist(pddl_type_of,RParams,TParams),!,
    compound_name_arguments_maybe_zero(Lps,Name,TParams),
  assert_lps_pl(Ctx,timeless(Lps)))).




 
assert_2pddl([derived|Ctx],[[Name|Params],PreConds]):- 
 must_or_rtrace_l((
  must_into_typed_params(Params,Typed),as_isa_list(Typed,Types),
  maplist(arg(2),Typed,ParamVars),
  compound_name_arguments_maybe_zero(RHS,Name,ParamVars),
  add_conjuncts([and|Types],PreConds,LHS),
  assert_lps_pl(Ctx,if(RHS,LHS)))).

assert_2pddl([axiom|Ctx],Form):- 
 ACtx = [axiom|Ctx],
 must_or_rtrace_l((
  get_1pair_value(ACtx,':vars',Vars,Form,Form0,[]),
  get_1pair_value(ACtx,':context',Pre,Form0,Form1,['and']),
  get_1pair_value(ACtx,':implies',PostCond,Form1,LeftOver,'$error'))),  
 must_or_rtrace_l((
  must(PostCond \== '$error'),
  must_into_typed_params(Vars,Value1b),as_isa_list(Value1b,Isas),
  add_conjuncts(Isas,Pre,Precond),
  assert_lps_pl(Ctx,if(PostCond,Precond)),
  assert_pddl_pairs([implication|Ctx],LeftOver))).

 
/*
assert_2pddl([KW,action(N,RParams)|Ctx],PreConds):- kw_directive(KW,Directive),
   maplist(pddl_value_of,RParams,VParams),
   assert_1pddl([action_types(N,TParams)|Ctx],[]),
   maplist(pddl_type_of,RParams,TParams),
   into_pterm( Ctx,PreConds,Conds),
   assert_1pddl([Directive,action(N,VParams)|Ctx],Conds),!.
*/

 %assert_1pddl([type,domain(midominio)],typed(object,boolean)).

 %  assert_lps_pl(type(domain(midominio),typed(object,boolean))).

assert_2pddl([requirement,Ctx],Inst):- 
  assert_lps_pl(Ctx,requirement(Inst)),!.

assert_2pddl([goal,Ctx],Inst):- 
   our_sterm2pterm(Inst,PTerm),
   listify(PTerm,LTerm),
  assert_lps_pl(Ctx,ec:demo_test(Ctx, lps_demo, LTerm)),!.
 
assert_2pddl([FormatType,Ctx],typed(Type,Inst)):- 
  %constant,object,type
  (atom(FormatType) -> LPS =.. [FormatType,Inst,Type]; LPS = is_typed(FormatType,Inst,Type)),
  assert_lps_pl(Ctx,LPS),!.

assert_2pddl([Atom|Ctx], KVList):- atom(Atom), our_sterm2pterm(KVList,Params), !,
    LPS =.. [Atom,Params],
    assert_lps_pl(Ctx,LPS),!.
assert_2pddl(Ctx,Form):- pprint_ecp_cmt(white,assert_1pddl(Ctx,Form)),fail.

assert_2pddl(Ctx,Form):- Ctx=[Front|Rest],is_list(Rest),into_context(Rest,RContext),
   (atom(Front) -> NewForm=Front;append_termlist(Front,[],NewForm)),
   append_term_pddl(NewForm,Form,Data),!,
   assert_lps_pl(RContext,Data).


assert_2pddl(Ctx,Form):- assert_lps_pl(Ctx,Form),!.

typify(any, Item,Typed):- \+ atomic_or_var(Item), into_typed_name(Item,Typed), !.
typify(Type,Item,typed(TType,Item)):- either_type(Type,TType).

either_type([either|Types],eitherOf(Types)).
either_type(Type,Type).

is_type_param_list([_|VList]):- member('-',VList),!.
is_type_param_list(KVList):- member(typed(_,_),KVList),!.

into_typed_params([], []):-!.
into_typed_params(List, _) :- \+ ground(List), throw(\+ ground(List)).
into_typed_params(List, AllParms) :- append(Items,['-',Type|Right],List),maplist(typify(Type),Items,Params),!,into_typed_params(Right, Rest), append(Params, Rest, AllParms).
into_typed_params(Items,Params):- maplist(typify(any),Items,Params).

into_typed(G,H):- into_typed_name(G,M),maybe_into_named_var(M,H).

maybe_into_named_var(M,V):- with_kif_ok(svar(M,Name)),!,V='$VAR'(Name),!.
maybe_into_named_var(M,V):- M=V.

into_typed_name(H,          typed(any,H)) :- atomic_or_var(H),!.
into_typed_name(  isa(I,T), typed(T,I)).
into_typed_name(typed(T,I), typed(T,I)).
into_typed_name('-'(T,I),   typed(T,I)).
into_typed_name(G,          typed(T,I)):- compound(G),compound_name_arguments(G,T,[I]),!.
into_typed_name(H,          typed(any,H1)):- our_sterm2pterm(H,H1). 

if_into_typed_params(KVList,Params):- is_type_param_list(KVList), must_into_typed_params(KVList,Params).
must_into_typed_params(KVList,Params):- must_or_rtrace(into_typed_params(KVList,Params)).

map_pddl_list(_Pred,[]).
map_pddl_list(Pred1,KVList):- if_into_typed_params(KVList,Params),!, maplist(Pred1,Params).
map_pddl_list(Pred1,[Item1|List]):- call(Pred1,Item1),map_pddl_list(Pred1,List).


append_term_pddl(X,Y,Z):- compound_gt(X,0),X=..[KW|ARGS],kw_directive(KW,NewType),X2=..[NewType|ARGS],!,append_term_pddl(X2,Y,Z).
append_term_pddl(X,Y,Z):- 
  append_term(X,Y,Z).


assert_lps_pl(_Ctx,Lps):-
  assert_lps_core(Lps).

assert_lps_meta(_Ctx,Lps):- debugging_trans(pprint_ecp_cmt(green,Lps)).

debugging_trans(_).

into_context(X,X):- atomic_or_var(X).
into_context([X],Y):- !, into_context(X,Y),!.
into_context(List,Y):- is_list(List),reverse(List,RList), Y=..[ctx|RList],!.
into_context(Y,Y):- is_context_arg(Y),!.
into_context(Y,ctx(Y)).

is_context_arg(Y):-  compound_gt(Y,0),functor(Y,ctx,_).


final_to_lps(lps_at,at).
final_to_lps(invariant,timeless).
final_to_lps(or,';').
final_to_lps(any,object).
final_to_lps(object(X,Y),isa(X,Y)).
final_to_lps(lps_at(X,Y),at(X,Y)).
final_to_lps(constant(X,Y),isa(X,Y)).
final_to_lps(typed(Y,X),isa(X,YY)):- once(map_nonvars(a,final_to_lps,Y,YY)).
final_to_lps(subtype(X),subtype(X,any)).

unlistify_arg(C,C):- atomic_or_var(C),!.
unlistify_arg([C],O):- !, unlistify_arg(C,O).
unlistify_arg([],[]):- !.
unlistify_arg(Empty,true):- is_empty(Empty),!.
unlistify_arg(C,O):- ands_to_list(C,List), list_to_conjuncts(List,O),!.
% unlistify_arg([C|AND],CAND):- into_pterm(unlistify_arg,[C|AND],CAND),!.
%unlistify_arg(C,C).
dash_to_underscore((ISA,Data),DData):- compound(ISA),ISA=..[isa,V,T],var(V),T==object,dash_to_underscore(Data,DData).
dash_to_underscore(Data,DData):- atom(Data), \+ atom_contains(Data,"/"), atom_subst(Data,'-','_',DData).



assert_lps_core(Lps):- once(map_nonvars(p,lps_pddl_convert:dash_to_underscore,Lps,LpsM)), Lps\=@=LpsM, !, assert_lps_core(LpsM).
assert_lps_core(Lps):- once(map_nonvars(p,lps_pddl_convert:unlistify_arg,Lps,LpsM)), Lps\=@=LpsM, !, assert_lps_core(LpsM).
%assert_lps_core(Lps):- once(map_nonvars(p,remove_context_arg,Lps,LpsM)), Lps\=@=LpsM, !, assert_lps_core(LpsM).
assert_lps_core(Lps):- once(map_nonvars(a,lps_pddl_convert:final_to_lps,Lps,LpsM)), Lps\=@=LpsM, !, assert_lps_core(LpsM).

% if(initiates pddl_when(autstate_1_2, prev_autstate_1_2)if enabled_action(refuel, X0, X1, X2, X3).

%assert_lps_core(Lps):- pprint_ecp(cyan,(Lps)),fail.
assert_lps_core(if(initiates(Action,not(Effect)),Prev)):- !,
  assert_lps_core(if(terminates(Action,Effect),Prev)).
assert_lps_core(if(initiates(Action,pddl_when(Cond,Effect)),Prev)):- !,
 assert_lps_core(if(initiates(Action,Effect),(Prev,Cond))).

assert_lps_core(if(initiates(Action,(Effect1,Effect2)),Prev)):- !,
 assert_lps_core(if(initiates(Action,Effect1),Prev)),
 assert_lps_core(if(initiates(Action,Effect2),Prev)).


assert_lps_core(if(terminates(Action,pddl_when(Cond,Effect)),Prev)):- !,
  assert_lps_core(if(terminates(Action,Effect),(Prev,Cond))).

assert_lps_core(Lps):-
  lps_xform(Lps,Prolog),!,
  ((Lps\==Prolog
   ->
   must_or_rtrace_l((
    ec_lps_convert:print_lps_syntax(yellow,Lps),
    debugging_trans(pprint_ecp_cmt(red,Prolog)),
    debugging_trans((pprint_ecp_cmt(white,"% =================================")))))
  ;   print_lps_syntax(red,Prolog))).

lps_xform(Lps,Prolog):- fail, 
 Ctx = db,
 locally(current_prolog_flag(lps_translation_only_HIDE,true),
   locally(t_l:is_lps_program_module(Ctx),
    notrace(lps_term_expander:lps_f_term_expansion_now(Ctx,Lps,Prolog)))),!.
lps_xform(Lps,Lps).


/*

:- module(lps_term_expander, [lps_term_expander/3,lps_f_term_expansion_now/3, lps_f_term_expansion/3]).

:- nodebug(lps(term_expand)).

:- module_transparent(lps_term_expander/3).

:- use_module(('../utils/psyntax.P'),[ 
	lps2p_file/2, lps2p/3, syntax2p_file/4, syntax2p/4, syntax2p_literal/7, golps/2, golps/1, dumpjs/2, dumpjs/1,
	file_generator_name/2, may_clear_hints/0,term_colours/2,timeless_ref/1, set_top_term/1, dumploaded/2
	]).

get_source_location(File,Line):- source_location(File,Line),!.
get_source_location(File,Line):-
	prolog_load_context(source,File), 
	prolog_load_context(term_position,TP), stream_position_data(line_position,TP,Line),!.

add_source_location(_Module,ExpandedTerm,Output):-  nonvar(ExpandedTerm), 
  \+ ((ExpandedTerm=(Compound:_), compound(Compound))),
  get_source_location(File,Line),
  Output = ('$source_location'(File, Line):ExpandedTerm), !.
add_source_location(_Module,Output,Output).

% on SWISH we'll avoid the file to file translation, by converting on a term by term basis, assuming the transform to be 1-1 (except for nlp)
% we assume the LPS transform to preserve Prolog 
lps_f_term_expansion(Module,NiceTerm,Output) :-         
	% somehow the source location is not being kept, causing later failure of clause_info/5 :-(
	% atom_prefix(File,'pengine://'), % process only SWISH windows
	lps_f_term_expansion_now(Module,NiceTerm,ExpandedTerm),!,
	add_source_location(Module,ExpandedTerm,Output),
	maybe_inform(Module,NiceTerm,ExpandedTerm).

lps_f_term_expansion_now(_Module,NiceTerm,ExpandedTerm):- 
	notrace(catch(call(call,lps_nlp_translate(NiceTerm,ExpandedTerm)),_,fail)), !. % hook for LogicalContracts extension
lps_f_term_expansion_now(_Module,NiceTerm,ExpandedTerm) :- 
	may_clear_hints, set_top_term(NiceTerm),!,
	% current_syntax(lps2p,true), In the future we may want to support other syntax conversions
	% variable names probably not available here, but we don't care about lpsp2p syntax anymore:
	% somehow this fails to... some terms;-) prolog_load_context(file,File), mylog(normal-File),
	syntax2p(NiceTerm,[],lps2p,ExpandedTerm). 


:- volatile(tmp:module_dialect_lps/4).
:- thread_local(tmp:module_dialect_lps/4).

is_lps_module_and_stream_ok(Module):- dialect_input_stream(In), tmp:module_dialect_lps(In,_,Module,_),!.
is_lps_module_and_stream_ok(Module):- debugging(lps(term_expand)), 
     use_module(library(listing)),
     dialect_input_stream(In),
     debug(lps(term_expand),'~p',(tryed(tmp:module_dialect_lps(In,_,Module,_)))),
     listing(tmp:module_dialect_lps/4),!,fail.
%is_lps_module_and_stream_ok(Module):- tmp:module_dialect_lps(_,_,Module,_), !.

using_lps(Module):- 
  \+ current_prolog_flag(emulated_dialect, swi),
  is_lps_module_and_stream_ok(Module),!.


lps_term_expander(Module,NiceTerm,ExpandedTerm):- 
  using_lps(Module),
  % context_module(user), % LPS programs are in the user module
  lps_f_term_expansion(Module,NiceTerm,ExpandedTerm),!,
  prolog_load_context(variable_names, Vars),
  maybe_save_varname_info(NiceTerm,Vars,module(Module)),
  maybe_save_varname_info(ExpandedTerm,Vars,module(Module)),!.

maybe_save_varname_info(ExpandedTerm,Vars,Why):-
  expand_to_hb(ExpandedTerm,H,B),
  ignore((Vars\==[], assertz(varname_cache:varname_info(H,B,Vars,Why)))),!.
  
  
  
maybe_inform(_Module,NiceTerm,ExpandedTerm):-   
  ignore(((debugging(lps(term_expand)),
          NiceTerm\=@=ExpandedTerm,flush_output(user_error),
          format(user_error,'~N~n% LPS:  ~p.~n% Into: ~p.~n',[NiceTerm,ExpandedTerm]),
          flush_output(user_error)))).


*/


























:- use_module(library(lps_syntax)).



% [waiter,agent,food,time]
% HoldsAt(BeWaiter1(waiter),time) ->
% Initiates(Order(agent,waiter,food),
%           BeWaiter2(waiter),
%           time).


pddl_to_lps(_Top, X, X):- atomic_or_var(X),!.
pddl_to_lps(_Top, X, X):- functor(X,_,1), arg(1,X,Var), is_ftVar(Var),!.
pddl_to_lps(_Top,lps_at(X,Y),loc_at(X,Y)).
pddl_to_lps(Top,Prop,O):- 
  Prop =.. [ThereExists,Vars,Term0], 
  is_quantifier_type(ThereExists,Exists),
  is_list(Vars), forall(member(E,Vars),ground(E)),
  QProp =.. [Exists,Vars,Term0],
  insert_vars(QProp, Vars, Term, _Has),
  pddl_to_lps(Top, Term,O),!.

pddl_to_lps(_Top,metreqs(X),X).
pddl_to_lps(_Top,'->'(at(F1,T1),initiates(E,F2,T2)),Becomes):- T1==T2,  
   Becomes = (F1->initiates(E,F2)).
pddl_to_lps(_Top,'->'(at(F1,T1),terminates(E,F2,T2)),Becomes):- T1==T2,  
  Becomes = (F1->terminates(E,F2)).
pddl_to_lps(_Top,'->'(holds_at(F1,T1),initiates(E,F2,T2)),Becomes):- T1==T2,  
   Becomes = (F1->initiates(E,F2)).
pddl_to_lps(_Top,'->'(holds_at(F1,T1),terminates(E,F2,T2)),Becomes):- T1==T2,  
  Becomes = (F1->terminates(E,F2)).

pddl_to_lps(Top,neg(X),Rest):- pddl_to_lps(Top,not(X),Rest).
pddl_to_lps(_Top,holds_at(Fluent, Time),initially(Fluent)):- Time==start, !.
pddl_to_lps(_Top,holds_at(Fluent, Time),initially(Fluent)):- Time==0, !.
%pddl_to_lps(_Top,holds_at(Fluent, Time),at(Fluent, Time)):- !.
pddl_to_lps([],happens_at(Event,Time),(observe Event at Time)):- !.
pddl_to_lps(_Top,happens(Event,Time),(Event at Time)):- !.
% observe(from(muestra_del_general('+86-555000001'),to(2,3)))
pddl_to_lps(  [],initiates_at(Event,Fluent,Time),initiates(Event,Fluent)):- is_ftVar(Time), !.
pddl_to_lps(  [],terminates_at(Event,Fluent,Time),terminates(Event,Fluent)):- is_ftVar(Time), !.

pddl_to_lps(_Top,initiates_at(Event,Fluent,Time),(Event initiates Fluent at Time)):- !.
pddl_to_lps(_Top,terminates_at(Event,Fluent,Time),(Event terminates Fluent at Time)):- !.

pddl_to_lps(_Top, not(exists(_,X)), not(X)):-!.
%pddl_to_lps(_Top, not(initially(X)),(initially not X)):-!.
%pddl_to_lps(_Top, not(holds_at(X,T)),holds_at(not(X),T)).

pddl_to_lps(_Top, holds_at(Fluent, From, To),holds(Fluent, From, To)):- !.



%pddl_to_lps(_Top,happensAt(Event,Time),at(observe(Event),Time)):- !.
pddl_to_lps(_Top,Form,LpsO):- Form=..[EFP,X], argtype_pred4lps_pddl(EFP,_), protify(EFP,X,Lps),!,flatten([Lps],LpsO).
pddl_to_lps(_Top,X=Y,Lps):- callable(X),append_term_pddl(X,Y,Lps).
pddl_to_lps(Top,','(X1,X2),(Lps1,Lps2)):- pddl_to_lps(Top,X1,Lps1),pddl_to_lps(Top,X2,Lps2).
pddl_to_lps(Top,'<->'(X1,X2),[Lps1,Lps2]):- simply_atomic_or_conj(X1),simply_atomic_or_conj(X2), pddl_to_lps(Top,'->'(X1,X2),Lps1),pddl_to_lps(Top,'->'(X2,X1),Lps2).
pddl_to_lps(_Top,'->'(X1,X2),(X2 if X1)):- simply_atomic_or_conj(X1),simply_atomic_or_conj(X2),!.
pddl_to_lps(_Top,X1,X1):- simply_atomic(X1),!.
pddl_to_lps(_Top,X1,false(Lps)):- \+ (X1 = false(_)), into_false_conj(X1,Lps),Lps\=not(_),!.
pddl_to_lps(_Top,X,X):-!.


assert_tl_pddl_try_harder_now((X2 if X1),(if X1 then X2)):- simply_atomic_or_conj(X1), simply_atomic_or_conj(X2).


assert_tl_pddl_try_harder1(Prolog):-  assert_tl_pddl_try_harder_now(Prolog,Again),
  lps_xform(db,Again,PrologAgain),Again\==PrologAgain,!, 
   ec_lps_convert:print_lps_syntax(yellow,Again),
   pprint_ecp_cmt(cyan,PrologAgain),
   pprint_ecp_cmt(white,"% ================================="),
   !.
%assert_tl_pddl_try_harder(Prolog):- on_x_fail(assert_tl_pddl_try_harder1(Prolog)),!.
assert_tl_pddl_try_harder(Prolog):- pprint_ecp(red,Prolog),!.

argtype_pred4lps_pddl(event,events).
argtype_pred4lps_pddl(fluent,fluents).
argtype_pred4lps_pddl(action,actions).
argtype_pred4lps_pddl(predicate,predicates).
argtype_pred4lps_pddl(invariant,timeless).
argtype_pred4lps_pddl(function,functions).
argtype_pred4lps_pddl(Action,Actions):- arg_info(domain,Action,arginfo),atom_concat(Action,"s",Actions).


:- fixup_exports.




%:- break.
/*

into_enables([and|Args],Enables):- !, maplist(into_enables,Args,EnablesL),append(EnablesL,Enables).
into_enables(Effect,Enables):- compound(Effect), compound_name_arguments_maybe_zero(Effect,and,Args),maplist(into_enables,Args,EnablesL),append(EnablesL,Enables).
into_enables(not(_),[]).
into_enables(E,[E]).

into_disables(not(E),Es):- into_enables(E,Es).
into_disables([not,E],Es):- into_enables(E,Es).
into_disables([and|Args],Enables):- !, maplist(into_disables,Args,EnablesL),append(EnablesL,Enables).
into_disables(Effect,Enables):- compound(Effect), compound_name_arguments_maybe_zero(Effect,and,Args),maplist(into_disables,Args,EnablesL),append(EnablesL,Enables).
into_disables(_,[]).*/

