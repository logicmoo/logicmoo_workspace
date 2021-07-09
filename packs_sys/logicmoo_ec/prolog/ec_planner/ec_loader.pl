% =========================================
% Goal/Plan translating
% =========================================
:- module(ec_loader,[load_e/1, fix_time_args/3,fix_goal/3, brk_on_bind/1,assert_axiom_2/2, is_e_toplevel/0,
  needs_proccess/3,process_ec/2]).

:- use_module(library(logicmoo_common)).

only_dec_pl(Goal):- into_dec_pl->call(Goal);true.
only_lps(Goal):- into_lps->call(Goal);true.
:- dynamic(translating_inline/0).
:- dynamic(translating_files/0).
:- thread_local(t_l:is_ec_cvt/1).
into_lps :- fail, (t_l:is_ec_cvt(lps) ; true),!.
into_dec_pl :- \+ into_lps. % t_l:is_ec_cvt(lps).
no_canon :- true. 
reduce_holds_at:- fail.
prefer_lps(X,X):- fail, holds_not_holds_at.
holds_not_holds_at :- true.

major_debug(Goal):- nop(Goal).

assert_to_lps(Stuff):- ignore(ec_lps_convert:assert_lps(Stuff)).


pprint_ecp_pl(Type,Form):- \+ into_lps, !, pprint_ecp(Type,Form),!.
pprint_ecp_pl(Type,Form):- major_debug(pprint_ecp_cmt(Type,Form)),!.

:- use_module(library(logicmoo_common)).

is_e_toplevel :- prolog_load_context(source,File),prolog_load_context(file,File).

:- if(\+ current_prolog_flag(lm_no_autoload,_)).
:- set_prolog_flag(lm_no_autoload,false).
:- wdmsg("WARNING: PFC_AUTOLOAD").
:- endif.

:- if(\+ current_prolog_flag(lm_pfc_lean,_)).
:- set_prolog_flag(lm_pfc_lean,false).
:- wdmsg("WARNING: PFC_NOT_LEAN").
:- endif.

:- use_module(library(logicmoo_utils_all)).

:- use_module(library(pfc_lib)).
%:- include(library(pfc)).
:- baseKB:export(baseKB:'$spft'/4).
:- system:import(baseKB:'$spft'/4).

%:- reexport(library('ec_planner/ec_planner_dmiles')).
:- reexport(ec_reader).

export_transparent(X):- export(X), module_transparent(X).

:- export_transparent(e_reader_teste/0).
e_reader_teste:- with_e_sample_tests(load_e),e_reader_teste2.

:- export_transparent(e_reader_testec/0).
e_reader_testec:- with_e_sample_tests(cvt_e_pl).

:- export_transparent(e_reader_testec_lps/0).
e_reader_testec_lps:- with_e_sample_tests(translate_e_to_pfc).

:- export_transparent(load_e/1).
% load_e(F):- load_e_from_translation(F).
load_e(F):- into_dec_pl, !, load_e_cond(F, always),!.
load_e(F):- load_e_into_memory(F).

load_e_from_translation(F):- 
  translate_e_to_pfc(F),
  calc_pel_filename(F,_,PL),
  consult(PL),!.

foundations_file('foundations/EC.e').
foundations_file('foundations/Root.e').
:- export_transparent(load_e_cond/2).

resolve_local_files_quietly(S0,S1):- quietly(resolve_local_files(S0,S1)).

quietly_needs_resolve_local_files(S0,S1):- quietly(needs_resolve_local_files(S0,S1)).

load_e_cond(F,Cond):- into_dec_pl, cond_load_e(Cond,F).
load_e_cond(S0,How):-
 must((
  ((resolve_local_files_quietly(S0,SS), 
  SS\==[])
   -> must_maplist(load_e_cond(How), SS)
   ; pprint_ecp_cmt(red, load(How,S0))))),!.

current_loading_proc(Proc1):- nb_current('$loading_proc',Proc1),!.
current_loading_proc(Proc1):- t_l:is_ec_cvt(FileType),filetype_to_proc(FileType,Proc1).
current_loading_proc(Proc1):- into_dec_pl, Proc1 = assert_ele_cond_load_e,!.
current_loading_proc(Proc1):- Proc1 = assert_e, !.

% filetype_to_proc(FileType,Proc1).
filetype_to_proc(pl,assert_ele_cond_load_e).
filetype_to_proc(pel,pprint_ecp(blue)).
filetype_to_proc(lps,assert_ep4lps(db)).

:- export_transparent(cond_load_e/2).

assert_e(X):- ec_lps_convert:assert_ep4lps(db,X).
% assert_e(X):- assert_ele(X).

cond_load_e(Cond,EndsWith):- is_list(EndsWith),!,must_maplist(cond_load_e(Cond),EndsWith).
cond_load_e(changed,F):- etmp:ec_option(load(F), loaded),!.
cond_load_e(changed,EndsWith):- atom(EndsWith),foundations_file(Already),atom_concat(_,Already,EndsWith),!.
cond_load_e(Cond,F):- etmp:ec_option(load(F), loading), Cond\==recursed, !.
cond_load_e(Cond,F):- quietly_needs_resolve_local_files(F, L), !, must_maplist(cond_load_e(Cond), L).  
cond_load_e(_,F):- is_filename(F), \+ atom_concat(_,'.e',F), !.
cond_load_e(Cond,F):- quietly_needs_resolve_local_files(F, L), !, must_maplist(cond_load_e(Cond), L).  
%cond_load_e(include,_):- nb_current('$load_cond', cvt_e_pl),!. 
%cond_load_e(include,_):- nb_current('$load_cond', translate_e_to_filetype( PlExt)),!. 
cond_load_e(Cond,F):- Req = [pl],
   \+ nb_current('$output_lang',Req), !,
   locally(b_setval('$output_lang',Req), cond_load_e(Cond,F)).
cond_load_e(Cond, S0):- resolve_local_files_quietly(S0,SS), SS==[], !, pprint_ecp_cmt(red, load(Cond,S0)),!.
cond_load_e(Cond,F):-
   pprint_ecp_cmt(green, loading(Cond, F)),
   current_prolog_flag(occurs_check,Was),
   (nb_current('$load_cond', OldCond);OldCond=[]),
   setup_call_cleanup(
     % Setup
     (set_ec_option(load(F), loading),
      set_ec_option(load_cond, Cond),
      set_prolog_flag(occurs_check,error),
      nb_setval('$load_cond', Cond)),
     % Call
     (current_loading_proc(Proc1),with_e_file(Proc1, current_output, F)),
     % Cleanup
     (set_ec_option(load(F), unknown),
      set_prolog_flag(occurs_check,Was),
      nb_setval('$load_cond', OldCond))),
   set_ec_option(load(F), loaded).


load_e_into_memory(F):- 
  with_e_file(assert_e,current_output,F).


:- export_transparent(translate_e_to_pfc/1).
translate_e_to_pfc(F):- translate_e_to_filetype(pfc,F),!.

:- export_transparent(translate_e_to_filetype/2).
translate_e_to_filetype(FileType,F):- quietly_needs_resolve_local_files(F, L), !, must_maplist(translate_e_to_filetype(FileType), L),!. 
translate_e_to_filetype(FileType,F):- calc_filename_ext(FileType,F,E,PL), translate_e_to_filetype( FileType,E,PL),!.

filetype_to_dialect(pl,pfc).
filetype_to_dialect(pfc,pfc).
filetype_to_dialect(pel,ecalc).
filetype_to_dialect(lps,lps).
filetype_to_dialect(FileType,Dialect):- fail, FileType = Dialect,!.

:- thread_local(t_l:is_ec_cvt/1).

:- export_transparent(translate_e_to_filetype/3).
translate_e_to_filetype( FileType,F,OutputName):- absolute_file_name(F,FA),F\==FA,!,translate_e_to_filetype(FileType,FA,OutputName).
translate_e_to_filetype(_FileType,_,OutputName):- fail, \+ should_update(OutputName),!. 
translate_e_to_filetype(MFileType,F,MOutputName):- 
 strip_module(MOutputName,Mod,OutputName), MOutputName\==OutputName,!,
 strip_module(MFileType,_,FileType),
 translate_e_to_filetype_4(FileType, Mod, F,OutputName),!.

translate_e_to_filetype(MFileType,F,OutputName):- 
 strip_module(MFileType,Mod,FileType),
 translate_e_to_filetype_4(FileType, Mod, F,OutputName),!.

translate_e_to_filetype_4(FileType, Mod, F,OutputName):- 
  (\+ atom(FileType) ;  \+ filetype_to_dialect(FileType,_)),
  Proc1 = FileType,!,
  locally(b_setval('$loading_proc',Proc1),
    translate_e_to_filetype_5(FileType, Mod, F,OutputName)).
translate_e_to_filetype_4(FileType, Mod, F,OutputName):- 
  translate_e_to_filetype_5(FileType, Mod, F,OutputName),!.
 
translate_e_to_filetype_5(FileType, Mod, F, OutputName):- 
 setup_call_cleanup(flag('$ec_translate_depth', Was, Was),
   ((ignore((Was==0 -> retractall(etmp:ec_option(load(_), _)))),
   locally(t_l:is_ec_cvt(FileType),
     (current_loading_proc(Proc1),with_e_file(Mod:Proc1, OutputName, F))))),
   flag('$ec_translate_depth', _, Was)).

calc_filename_ext(MEx,F,FE,PL):- strip_module(MEx,_,Ext), show_call((calc_filename_ext_1(Ext,F,FE,PL))), !.
calc_filename_ext_1(pel,F,FE,PL):- calc_pel_filename(F,FE,PL),!.
calc_filename_ext_1(Ext,F,FE,PL):- compound(Ext), compound_name_arity(Ext,Out,_),calc_filename_ext_1(Out,F,FE,PL).
calc_filename_ext_1(Ext,F,FE,PL):- atom_concat(Was,Ext,F),PL=F,!,calc_filename_ext_1(Ext,Was,FE,_).
%calc_filename_ext_1(Ext,F,FE,PL):- =(Was,F),FE=F, calc_filename_ext_1(Ext,Was,_,PL), exists_file(PL),!.
%calc_filename_ext_1(Ext,F,FE,PL):- atom_concat(Was,'.e',F),FE=F,!,calc_filename_ext_1(Ext,Was,_,PL).
calc_filename_ext_1(Ext,F,FE,OutputName):- FE=F, calc_where_to(outdir('.', Ext), F, OutputName).


calc_pel_filename(F,FE,F):- atom_concat(Was,'.pel',F),!,atom_concat(Was,'.e',FE).
calc_pel_filename(F,FE,F):- atom_concat(Was,'.e.pl',F),!,atom_concat(Was,'.e',FE).
calc_pel_filename(F,FE,F):- atom_concat(Was,'.pl',F),!,atom_concat(Was,'.e',FE).
calc_pel_filename(F,F,PL):- atom_concat(Was,'.e',F), atom_concat(Was,'.pel',PL),exists_file(PL).
calc_pel_filename(F,F,PL):- atom_concat(Was,'.e',F), atom_concat(Was,'.e.pl',PL),exists_file(PL).
calc_pel_filename(F,F,PL):- atom_concat(Was,'.e',F), atom_concat(Was,'.pl',PL),exists_file(PL).

:- export_transparent(load_e_pl/1).
load_e_pl(F):- quietly_needs_resolve_local_files(F, L), !, must_maplist(load_e_pl, L),!. 
load_e_pl(F):- to_e_pl(F,E,PL),load_e_pl(E,PL),!.

:- export_transparent(cvt_e_pl/1).
cvt_e_pl(F):- quietly_needs_resolve_local_files(F, L), !, must_maplist(cvt_e_pl, L),!. 
cvt_e_pl(F):- to_e_pl(F,E,PL), cvt_e_pl(E,PL),!.
                        
to_e_pl(F,FE,PL):- calc_filename_ext(pel,F,FE,PL),!.

:- export_transparent(load_e_pl/2).
load_e_pl(ME,PL):- strip_module(ME,M,E),cvt_e_pl(E,PL),M:user:consult(PL),!.

get_date_atom(Atom):- 
   get_time(Now),stamp_date_time(Now, Date, 'UTC'), 
   format_time(atom(Atom),'%a, %d %b %Y %T GMT', Date, posix),!.

cvt_e_pl(F0,OutputName):- dumpST,
 wdmsg(call(trans_e)), break,
 absolute_file_name(F0,F),
 ( 
 (( ( fail, \+ should_update(OutputName))) -> true ;
  setup_call_cleanup(open(OutputName, write, Outs),
  (must_maplist(format(Outs,'~N~q.~n'),
    [( :- include(library('ec_planner/ec_test_incl'))),
     ( :- expects_dialect(pfc))]),
       get_date_atom(Atom),format(Outs,'% ~w',[Atom]),
       nb_setval('$ec_output_stream',Outs),
  with_output_to(Outs, cond_load_e(load_e_pl,F))),
  (close(Outs),nb_setval('$ec_output_stream',[]))))), %trace, %consult(OutputName),
  !.

/*
 
 state constraints:  

      /**/holds(Fluent)
          implies/equivalent_to
      /**/holds(Fluent)


 effect axioms:  

      happens_at(Event)
           terminates_at/initiates_at/releases_at
      /**/holds(Fluent)
       

 trigger axioms: 
  
      /**/holds(Fluent)/happens_at(Event)
           triggers
      happens_at(Event)


 precondition axioms:  
  
      happens_at(Event)
           implies
      /**/holds(Fluent)

 

happens_at(doorLock(Agent,Door),Time) -> 
  /**/holds(awake(Agent),Time) &
  /**/holds(doorUnlocked(Door),Time) &
  /**/holds(nearPortal(Agent,Door),Time)




   
axiom(
 terminates_at(doorLock(Agent,Door),
            doorUnlocked(Door),Time), [])

becomes 
...

axiom(
 terminates_at(doorLock(Agent,Door),
            doorUnlocked(Door),Time),
   [ /**/holds(awake(Agent),Time) &
     /**/holds(doorUnlocked(Door),Time) &
     /**/holds(nearPortal(Agent,Door),Time)]).
 
*/
functors_are(F,E):- \+ is_list(E), conjuncts_to_list(E, L), !, functors_are(F, L).
functors_are(\+ F,L):-  nonvar(F), !, forall(member(E,L), \+ functor_is(F, E)).
functors_are((F1,F2),L):- !,  
  partition(functors_are(F1),L,[_|_],RestOf),
  functors_are(F2,RestOf).
%functors_are((F1;F2),L):-  !, nonvar(F1), (functors_are(F1,L);functors_are(F2,L)).
functors_are(F,L):- must_maplist(functor_is(F),L).


functor_is(F, not(E)):- !, compound(E), functor_is(F, E).
functor_is(F, exists(_, E)):- !, compound(E), functor_is(F, E).
functor_is(F,(F1;F2)):- !, functor_is(F,F1), functor_is(F,F2).
functor_is(F,(F1,F2)):- !, functor_is(F,F1), functor_is(F,F2).
functor_is(\+ F,P):- !, nonvar(F), !,  \+ functor_is(F,P).
functor_is((F1;F2),P):- !, nonvar(F1), (functor_is(F1,P);functor_is(F2,P)).
functor_is(F,P):- compound(P), compound_name_arity(P,F,_).

:- export_transparent(set_mpred_props/2).
set_mpred_props(MF,E):- strip_module(MF,M,P),MF==P,!,must(set_mpred_props(M:P,E)).
set_mpred_props(M:F/A,E):- !, (is_ftVar(A)-> true ; ain(mpred_prop(M,F,A,E))).
set_mpred_props(M:P,E):- \+ compound(P),!,set_mpred_props(M:P/_,E).
set_mpred_props(M:P,E):- compound_name_arity(P,F,A),set_mpred_props(M:F/A,E),
   add_mat(P),!.

add_mat(P):- compound_name_arity(P,_,0),!.
add_mat(P):-
   only_dec_pl(assert_ready(red, ('==>'(meta_argtypes(P))))),
   ain(meta_argtypes(P)).




is_4th_order_f0(axiom).
is_4th_order_f0(F):- upcase_atom(F,UD),upcase_atom(F,UD).

get_arg_type(argIsa(W),1,W).
get_arg_type(F, Nth,Type):- arg_info(_,F,VTypes), compound(VTypes), arg(Nth,VTypes,Type), !.
get_arg_type(F,_,axiom_head):- is_4th_order_f0(F),!.

coerce_arg_to_type(Time, axiom_head, H, HH):- !, show_fix_axiom_head(Time, H, HH).

coerce_arg_to_type(_, axiom, HB, HB):- compound_gt(HB, 0), HB=..[RelType|_], fixed_already(RelType), !.
coerce_arg_to_type(Time, axiom, HB, axiom(AxHB,[])):-  coerce_arg_to_type(Time, axiom_head, HB, AxHB), !.
coerce_arg_to_type(Time, axiom, H, HH):- fix_assert(Time, H, HH).
coerce_arg_to_type(_Time, fluent, H, H):-
   assert_ele(fluent(H)).


fixed_already(mpred_prop).
fixed_already(meta_argtypes).
fixed_already(predicate).
fixed_already(event).
fixed_already(axiom).
fixed_already(ignore).
fixed_already(load).
fixed_already(ec_current_domain_db1).
fixed_already(ec_current_domain_db2).
fixed_already(include).
fixed_already(set_ec_option).
fixed_already(F):- arg_info(domain,F,_).
fixed_already(F):- arg_info(predicate,F,_).

fix_assert(_, X, Y):- prefer_lps(X,Y), !.
fix_assert(_T, :-(B), :-(B)):- !. 
fix_assert(_T, '==>'(B), '==>'(B)):- !. 
fix_assert(Time, HT:-B, HTO:-B):- !, 
  fix_assert(Time, HT, HTO).
fix_assert(Time, HT, HTO):- 
 fix_argtypes(Time, 1, [argIsa(axiom)], HT, HTM),!,
 fix_assert_pass2(Time, HTM, HTO).

fix_assert_pass2(_, HB, HB):- HB=..[RelType|_], fixed_already(RelType), !.
fix_assert_pass2(Time, G, axiom(GG, [])):- must(show_fix_axiom_head(Time, G, GG)),!.


fix_argtypes(Time, NthArg, [F|_], HT, HTO):-
    get_arg_type(F,NthArg,Type), 
    coerce_arg_to_type(Time,Type,HT,HTO),!.
fix_argtypes(_, _NthArg, _Type, HB, HB):- \+ compound_gt(HB, 0), !.
fix_argtypes(Time, _NthArg, Type, HT, HTO):-
 compound_name_arguments(HT, F, L), 
 fix_argtypes(Time, 1, [F|Type], L, LL),
 compound_name_arguments(HTO, F, LL).

fix_numbered_argtypes(Time, NthArg, FType, [H|T], [HH|TT]):- !,
  fix_argtypes(Time, NthArg, FType, H, HH),
  NthArgPlus1 is NthArg + 1,
  fix_numbered_argtypes(Time, NthArgPlus1, FType, T, TT).
fix_numbered_argtypes(_Time, _NthArg, _FType, [], []).

:- export_transparent(fix_axiom_head/3).

fix_axiom_head(_, X, Y):- prefer_lps(X,Y), !.
fix_axiom_head(_, X, Y):-  (\+ callable(X);\+ compound(X)), !, X=Y.
fix_axiom_head(T, [G1|G2], [GG1|GG2]):- !, fix_axiom_head(T, G1, GG1),fix_axiom_head(T, G2, GG2). 
fix_axiom_head(T, (G1,G2), (GG1,GG2)):- !, fix_axiom_head(T, G1, GG1),fix_axiom_head(T, G2, GG2). 
fix_axiom_head(T, (G1;G2), (GG1;GG2)):- !, fix_axiom_head(T, G1, GG1),fix_axiom_head(T, G2, GG2). 
fix_axiom_head(T, (G1:-B), (GG1:-B)):- !, fix_axiom_head(T, G1, GG1),!.


fix_axiom_head(T, exists(X,G), (ex(X),GG)):- no_canon, !, fix_axiom_head(T, G, GG).

fix_axiom_head(T, exists(X,G), exists(X,GG)):-!, fix_axiom_head(T, G, GG).

fix_axiom_head(T, /**/not(II),O):- compound(II), II= /**/not(I), !, fix_axiom_head(T, I,O),!.
fix_axiom_head(T, /**/not(I),O):- !, fix_axiom_head(T, I,M), correct_holds(/**/not, not(M), O). 
fix_axiom_head(T, not(I),O):- !, fix_axiom_head(T, I,M), correct_holds(/**/not, not(M), O).
fix_axiom_head(T, G, GG):- must_or_dumpst(cvt0_full(T,G,Y)), (G==Y -> fail; fix_axiom_head(T,Y,GG)),!.

fix_axiom_head(T, (G1G2), (GG1GG2)):- compound_name_arguments(G1G2,F,[G1,G2]),
   member(F,['->','<-','<->',';',',']), !,
   fix_axiom_head(T, G1, GG1),fix_axiom_head(T, G2, GG2),
   compound_name_arguments(GG1GG2,F,[GG1,GG2]).

% EC to LPS

fix_axiom_head(T, /**/holds(G,T1), G):- reduce_holds_at, into_lps, same_times(T1,T), functor_skel(G,P), syntx_term_check(fluent(P)),!.
fix_axiom_head(T, /**/holds(Event,T1), G):- reduce_holds_at, into_lps, nonvar(Event), same_times(T1,T), !, fix_axiom_head(T1, Event, G).
fix_axiom_head(T, happens_at(Event,T1), G):- into_lps, nonvar(Event), same_times(T1,T), !, fix_axiom_head(T1, Event, G).
fix_axiom_head(_, G, G):- into_lps, functor_skel(G,P), syntx_term_check(fluent(P)),!.
fix_axiom_head(_, G, G):- into_lps, functor_skel(G,P), syntx_term_check(predicate(P)),!.

% EC to DEC_PL
fix_axiom_head(_, G, G):- into_dec_pl, safe_functor(G,F,A), already_good(F,A),!.
fix_axiom_head(T, G, /**/holds(G,T)):- into_dec_pl, functor_skel(G,P), syntx_term_check(fluent(P)),!.
fix_axiom_head(T, G, happens_at(G,T)):- into_dec_pl,  functor_skel(G,P), \+ syntx_term_check(predicate(P)), (syntx_term_check(action(P);event(P))),!.
fix_axiom_head(T, G, /**/holds(G,T)):- into_dec_pl, functor_skel(G,P), syntx_term_check(fluent(P)),!.

fix_axiom_head(_, G, G):- G\=not(_), functor_skel(G,P), syntx_term_check(predicate(P)),!.

fix_axiom_head(T, P, PP):-  
   P =..[F|Args],functor(P,F,A), arg_info(AxH,F,Arity),
   functor(Arity,_,N),  correct_ax_args(T,F,A,Args,AxH,Arity,N,PP),!. 
fix_axiom_head(T, P, Out):- predicate_property(P,foreign),!,if_debugging(ec,((call(dumpST), wdmsg(fix_axiom_head(T, call(P))),break))),!,Out=call(P).

fix_axiom_head(T, G, GG):- into_dec_pl, !, if_debugging(ec, ((call(dumpST), wdmsg(fix_axiom_head(T, G)), break))), 
  GG = /**/holds_at_oops(G, T).

% LPS Only
fix_axiom_head(_, P, P):-  functor(P,F,_),fixed_already(F),!.
fix_axiom_head(_, P, P):-  functor(P,F,_),atom_concat(F0,'s',F),fixed_already(F0),!.

fix_axiom_head(T, G, GG):- if_debugging(ec, ((call(dumpST), wdmsg(fix_axiom_head(T, G)), break))),  \+ into_lps,
  GG = holds_at_oops(G, T).
fix_axiom_head(_T,G,G):-!.

same_times(T1,T):- T1==T,!.

%maybe_show_diff(T0, F, O):- copy_term_nat(T0+F+O,T+HT+HTTermO),
maybe_show_diff(T, HT, HTTermO):-
  ignore((HT\==HTTermO, HT \= not(/**/holds(_,_)), 
    % pprint_ecp_cmt(blue,(axiom_head(T) -> HT)),
    major_debug(pprint_ecp_cmt(blue,(fix_axiom_head(T) -> [HT ,(->), HTTermO]))))),!.

:- export(show_fix_axiom_head/3).
show_fix_axiom_head(T, HT, HTTermO):- 
    fix_axiom_head(T, HT, HTTermO),!,
    ignore(maybe_show_diff(T, HT, HTTermO)).
    
show_fix_axiom_head(T, HT, HTTermO):- dumpST, 
 compound_name_arguments(HT, F, L),
 upcase_atom(F,U),downcase_atom(F,U),
 must_maplist(show_fix_axiom_head(T),L,LL),
 compound_name_arguments(HTTerm, F, LL),
 show_fix_axiom_head(T, HTTerm, HTTermO), !.

show_fix_axiom_head(T, HT, HTTermO):- trace, rtrace(fix_axiom_head(T, HT, HTTermO)),!.

:- dynamic(ec_tmp:do_next_axiom_uses/1).

call_ready_body(Type,Body):- 
   setup_call_cleanup(
    pprint_ecp(Type,':-'(if(is_e_toplevel))),
    ((notrace((echo_format('~N'))),
    pprint_ecp(Type,':-'(Body)),
    notrace((echo_format('~N'))),
    ignore(must((Body,!))),
    notrace((echo_format('~N'))))),
    pprint_ecp(Type,':-'(endif))).

assert_ready(P):- assert_ready(pl,P).

assert_ready(Type,(:-Body)):- 
   call_ready_body(Type,Body),!.

assert_ready(Type,'==>'(next_axiom_uses(Value))):- 
     pprint_ecp_pl(Type,'next_axiom_uses'(Value)),
     assert(ec_tmp:do_next_axiom_uses(Value)),!.

assert_ready(Type,'==>'(Value)):- 
  only_dec_pl( pprint_ecp_pl(Type,'==>'(Value))),
   mpred_fwc('==>'(Value)),
   Value = ValueO, % fix_assert(_Time,Value,ValueO),
   into_current_domain_db(ValueO),!.

assert_ready(Type,axiom(H,B)):- select(I,B,NB),compound(I),I=ignore(Was==Into),
   subst(axiom(H,NB),Was,Into,axiom(HH,BB)),!,
   %append(BB,[ignore(ignore(Was==Into))],BBB),
   BB=BBB,
   assert_ready(Type,axiom(HH,BBB)), !.


assert_ready(Type,axiom(H,B)):- B ==[],compound(H),functor(H,F,_),verbatum_functor(F),!,
  assert_ready(Type,H).

assert_ready(Type,Value):- 
  assert_ready_now(Type,Value).

assert_ready_now(Type,Value):- 
   only_dec_pl(pprint_ecp_pl(Type,Type=Value)),
   notrace((echo_format('~N'))),
   mpred_fwc(Value),
   fix_assert(_Time,Value,ValueO),
   into_current_domain_db(ValueO),!.

into_current_domain_db('==>'(Value)):-!,into_current_domain_db((Value)).
into_current_domain_db(ec_current_domain_db1(Value)):- !, into_current_domain_db((Value)).
into_current_domain_db(ec_current_domain_db2(Value,T)):- is_ftVar(T),!,into_current_domain_db(Value).
into_current_domain_db(ec_current_domain_db2(Value,T)):- !, assertz_if_new_domain_db(Value,T).
into_current_domain_db(Value):- get_varname_list(Vs),=(Value-Vs,ValueO-VsO),
   locally(b_setval('$variables',VsO),assertz_if_new_domain_db(ValueO,_)).


assertz_if_new_domain_db((H:-B),T):- !, assertz_if_new_msg((user:ec_current_domain_db2(H,T):-B)).
assertz_if_new_domain_db(ValueO,_):- ValueO =@= axiom(/**/holds(/**/not(raining), _), []),!,barf.
assertz_if_new_domain_db(ValueO,T):- assertz_if_new_msg(user:ec_current_domain_db2(ValueO,T)).

assertz_if_new_msg(Stuff):- clause_asserted(Stuff),!, major_debug(wdmsg(already(Stuff))), !, only_lps(assert_to_lps(Stuff)),!.
assertz_if_new_msg(Stuff):- assertz_if_new(Stuff), only_lps(assert_to_lps(Stuff)),!.

some_renames(O,O):- \+ compound(O),!.
some_renames(HB,O):- notrace((sub_term(Sub,HB),compound(Sub),Sub=before(X,Y))),!,
  subst(HB,Sub,b(X,Y),HTM),some_renames(HTM,O).
some_renames(HB,O):- notrace((sub_term(Sub,HB),compound(Sub),compound_name_arity(Sub,Name,0),!,
  subst(HB,Sub,Name,HTM),some_renames(HTM,O))).
some_renames(O,O).

:- export_transparent(assert_ele_cond_load_e/1).
assert_ele_cond_load_e(EOF) :- must(brk_on_bind(EOF)), must(assert_ele(EOF)),!.

predform_to_functionform(PF,equals(Fn,LastArg)):- notrace((PF=..[F|Args],append(FnArgs,[LastArg],Args),predname_to_fnname(F,FnF),Fn=..[FnF|FnArgs])).
functionform_to_predform(equals(Fn,LastArg),PF):- notrace((Fn=..[FnF|FnArgs],append(FnArgs,[LastArg],Args),fnname_to_predname(FnF,F),PF=..[F|Args])).

predname_to_fnname(Pred,Fun):- notrace(clause_b(functional_predicate(Fun,Pred))),!.
predname_to_fnname(Pred,Fun):- atom(Pred),atom_concat(Pred,'Of',Fun),call_u(resultIsa(Templ,_)),functor(Templ,F,_),Fun==F,!.
predname_to_fnname(Pred,Fun):- atom(Pred),atom_concat(Pred,'Fn',Fun),!.
predname_to_fnname(Pred,Fun):- atom(Pred),atom_concat(Fun,'Pred',Pred),!.

fnname_to_predname(Fun,Pred):- clause_b(functional_predicate(Fun,Pred)).
fnname_to_predname(Fun,Pred):- atom(Fun),atom_concat(Pred,'Fn',Fun),!.
fnname_to_predname(Fun,Pred):- atom(Fun),atom_concat(Pred,'Of',Fun),!.
fnname_to_predname(Fun,Pred):- atom(Fun),atom_concat(Fun,'Pred',Pred),!.

:- export_transparent(assert_ele/1).
assert_ele(EOF) :- notrace((EOF == end_of_file)),!.
assert_ele(I):- notrace(\+ callable(I)),!,assert_ele(uncallable(I)).
assert_ele(SS):- notrace(is_list(SS)),!,must_maplist(assert_ele,SS).
assert_ele(Cvt1):-  (some_renames(Cvt1,Cvt2) -> Cvt1\=@=Cvt2), !, assert_ele(Cvt2).


assert_ele(_):- notrace((echo_format('~N'), fail)).
assert_ele(translate(Event, Outfile)):- !, mention_s_l, echo_format('% translate: ~w  File: ~w ~n',[Event, Outfile]).
%assert_ele('==>'(S0)):- !, assert_ready( '==>'(S0)).
assert_ele(:- S0):- !, assert_ready( (:-(S0))).

assert_ele(axiom(H,B)):- !, assert_ready(ec_current_domain_db1(axiom(H,B))).
assert_ele(include(S0)):- !, assert_ready( :-(load_e_cond(S0,include))).
assert_ele(load(S0)):- !, assert_ready( :-(load_e_cond(S0,changed))).
assert_ele(load(Cond, S0)):- !, assert_ready( :-(load_e_cond(S0,Cond))).
assert_ele(ec_current_domain_db1(P)):- !, assert_ready( ec_current_domain_db1(P)).

assert_ele(HB):- \+ compound_gt(HB, 0), !, assert_axiom(HB, []).

assert_ele(HB):- HB=..[=, Function, Value],
  %get_functor(RelSpec,F), get_mpred_prop(Function,function),
  %must(compound(Function)),
  %append_term(Function,Value,Predicate), !,
  assert_ele(equals(Function,Value)).

assert_ele(HB):- HB=..[function, RelSpec, RetType], 
  %append_term(RelSpec,RetType,PredSpec),
  %assert_ele(functional_predicate(PredSpec)),
  must_det_l((
   assert_ele(function(RelSpec)),

   functionform_to_predform(equals(RelSpec,RetType),PredSpec),
   assert_ele(predicate(PredSpec)),
   get_functor(RelSpec,F),
   get_functor(PredSpec,P),
   assert_ele(functional_predicate(F,P)),
   assert_ele(function_argtypes(P,RelSpec,RetType)),
   assert_ele('==>'(resultIsa(F, RetType))))).

assert_ele(HB):- HB=..[RelType,RelSpec],arg_info(domain,RelType,arginfo), !, 
  functor_skel(RelSpec,P),!, 
  RelTypeOpen=..[RelType,P],
  only_dec_pl(assert_ready(blue, RelTypeOpen)),
  assert_ready(blue, HB),
  assert_ready(red, ('==>'(mpred_prop(RelSpec, RelType)))),
  must(set_mpred_props(RelSpec,RelType)).

assert_ele(HB):- functor(HB,F, L), arg_info(predicate,F,Args),Args=..[v|ArgL], length(ArgL,L), !, assert_ready(yellow, '==>'(HB)).
assert_ele(subsort(F, W)):- !, must_maplist(assert_ready(yellow),[sort(F),sort(W),subsort(F, W)]).
assert_ele(option(X,Y)):- set_ec_option(X,Y), must_maplist(assert_ready(yellow),[:- set_ec_option(X,Y)]).
assert_ele(xor(XORS)):- conjuncts_to_list(XORS,List),  !, assert_ready(red, '==>'xor(List)).
assert_ele(t(F, W)):- !, must_maplist(assert_ready(yellow),['==>'(sort(F)), '==>'(t(F, W))]).

% assert_ele(not(/**/holds(H,T))):- assert_ele(/**/holds(/**/not(H),T)).
assert_ele(Cvt1):- \+ into_lps,  (cvt0(_T, Cvt1,Cvt2) -> Cvt1\=@=Cvt2), !, assert_ele(Cvt2).

  %locally(b_setval('$output_lang',[ec]), assert_ele(P)).
assert_ele('<->'(H,B)):- 
  pprint_ecp_cmt(green, '<->'(H,B)), !,
  only_dec_pl((atoms_of(H,HH), atoms_of(B,BB),pprint_ecp_cmt(yellow, '<->'(HH,BB)))), !,
  assert_ele('->'(H,B)),
  assert_ele('->'(B,H)).


% assert_ele(initially(F)):- !, assert_axiom(initially(F),[]).
assert_ele(directive(F)):- !, assert_ele(next_axiom_uses(F)).

/*
assert_ele('->'(H,B)):- nonvar(B),
  B=(BP,(B1;B2)),!,
  assert_ele('->'(H,(BP,B1))), 
  must(assert_ele('->'(H,(BP,B2)))),!.

assert_ele('->'(B,H)):- nonvar(B),
  B=(BP,(B1;B2)),!,
  assert_ele('->'((BP,B1),H)),assert_ele('->'((BP,B2),H)).

assert_ele('->'(B,H)):- nonvar(B),B=((B1;B2)),!,assert_ele('->'(B1,H)),assert_ele('->'(B2,H)).
*/

assert_ele('<-'(H,B)):- conjuncts_to_list(B,BL), !, must(assert_axiom(H,BL)).
assert_ele((H :- B)):- !,  assert_ready( (H :- B)).
% assert_ele((H :- B)):- (H=not(_);is_axiom_head(H)), !, conjuncts_to_list(B,BL), assert_axiom(H,BL).
assert_ele(axiom(H,B)):- echo_format('~N'), !,
  correct_axiom_time_args(t,H,B,HH,BB),
  assert_ready( axiom(HH,BB)).

assert_ele(HB):- correct_holds(outward,HB, HBC), HB\=@=HBC, !, assert_ele(HBC).

% , assert_ready(e, HB),
%assert_ele(SS):- syntx_term_check(SS),!.

assert_ele('->'(Body,AxHead)):- AxHead=..[Effect|_],
  member(Effect,[initiates_at,terminates_at,releases_at]),
  conjuncts_to_list_body(Body, Conds), 
  assert_axiom(AxHead, Conds).

assert_ele(AxHead):- AxHead=..[Effect|_],
  member(Effect,[initiates_at,terminates_at,releases_at]),
  assert_axiom(AxHead, []).

/*
assert_ele(if(happens_at(A,T),Holds)):- 
  assert_m_axiom(if(not(Holds),not(happens_at(A,T)))).
*/
assert_ele(happens_at(A,T)):- number(T), !, assert_ele(happens_at(A,t+T)).

assert_ele('->'(B,H)):-  fail, term_variables(B+H,Vars), Vars=[_,_|_], 
   atoms_of('->'(B,H),Atoms), \+ member(allDifferent,Atoms),!,
   assert_m_axiom('->'(allDifferent(Vars),'->'(B,H))).

assert_ele('->'(B,H)):- no_canon, !, assert_m_axiom('->'(B,H)).
% assert_ele('->'(B,H)):-  conjuncts_to_list_body(B,BL), !, assert_axiom(H,BL).

assert_ele(H):- compound_name_arity(H, F, 2),
  needs_cononicalization(F),
  e_to_pel(H,P), !, assert_m_axiom(P).

assert_ele(not(H)):-  !,  assert_m_axiom(not(H)).

assert_ele('==>'(SS)):- echo_format('~N'), !,
  assert_ready(red, '==>'(SS)).

assert_ele(axiom(H)):- !, assert_ele(axiom(H,[])).

assert_ele(P):-  functor(P,F,_),fixed_already(F),!,assert_ready( ec_current_domain_db1(P)).

% assert_ele(SS):- fix_time_args(T, SS),
assert_ele(SS):- echo_format('~N'), 
  wdmsg(warn(assert_ele(SS))),
  assert_axiom(SS,[]),!.
  %assert_ready(red, SS).

correct_axiom_time_args(_Stem,H,B,HH,BB):- into_lps,!, H=HH,B=BB,!.
correct_axiom_time_args( Stem,H,B,HH,BB):- 
  visit_time_args(Stem,[],H,HH,Mid),
  visit_time_args(Stem,Mid,B,BBs,Out),
  append(BBs,Out,BB),!.

/*
ForAll([event,animal,time],
 HoldsAt(DoneBy(event,animal),time) <->
 (Happens(event,time) &
  ((Exists([gate], event=Close(animal,gate))) |
   (Exists([animal1].  event=GetOff(animal,animal1)))|
   (Exists([animal1].  event=Mount(animal,animal1)))|
   (Exists([position].  event=Move(animal,position)))|
   (Exists([gate].  event=Open(animal,gate))) |
   (Exists([human1].  event=ThrowOff(animal,human1)))))).
*/

barf:- dumpST,wdmsg(i_BaRfzzzzzzzzzzzzzzzzzzzzzzzzzzz), break.
use_inititally:- true.

cvt0_full(T,G,GG):- must(cvt0(T,G,Y)), !, (G==Y -> G=GG ; cvt0(T,Y,GG)),!.

is_start_t(Zero):- Zero == 0,!.
is_start_t(Zero):- Zero == start,!.

start_plus(Zero,start):- is_start_t(Zero).
start_plus(Zero,start+Zero):- number(Zero),!.


cvt0(_, X, Y):-  (\+ callable(X);\+ compound(X)), !, X=Y.
cvt0(_, X\=Y, diff(X,Y)) :- !.
cvt0(T, equals(X,Y), O):- is_ftVar(X), \+ is_ftVar(Y),!,cvt0(T, equals(Y,X), O),!.
cvt0(T, equals(X,Y), O):- \+compound(X), compound(Y),!,cvt0(T, equals(Y,X), O),!.
cvt0(_, equals(X,Y), O):-compound(X), functionform_to_predform(equals(X,Y),O),!.
cvt0(_, X=Y, Equals):- !,as_equals(X,Y,Equals).
cvt0(T0, /**/holds(NotH,T),O):- compound(NotH),not(H)= NotH, !, cvt0(T0, /**/holds(/**/not(H),T), O).
%cvt0(T, P, call(P)):- predicate_property(P,foreign),!.
%cvt0(_, not(exists(_Vars, /**/holds(P, Time))),not(/**/holds(/**/not(P), Time))).
cvt0(T, initially(N), Out):- \+ use_inititally, cvt0(T, /**/holds(N, 0), Out).
cvt0(_, /**/holds(N,Zero),initially(N)):- use_inititally, is_start_t(Zero),!.
cvt0(_, /**/holds(N,Zero),/**/holds(N,Expr)):- \+ into_lps, start_plus(Zero,Expr),!.
cvt0(_, happens_at(N,Zero),happens_at(N,Expr)):-  \+ into_lps, start_plus(Zero,Expr),!.
%cvt0(T, /**/holds(N,AT),'<-'(/**/holds(N,TN),(start(T),toffset(T,AT,TN)))):- number(AT),!,atom_concat(t,AT,TN).

cvt0(_, before(X,Y),b(X,Y)).
%cvt0(_, '<-'(X,Y),'<-'(X,Y)).
cvt0(_T, option(X,Y),option(X,Y)):-!.
cvt0(T, X\=Y, O):- must(cvt0(T, not(X=Y), O)).

cvt0(_, =(X,Y),P):- !, as_equals(X,Y,P).
cvt0(_, equals(X,Y),P):- !, as_equals(X,Y,P).
cvt0(T, axiom_uses(V),axiom_uses(V,T)):- !.


cvt0(T, /**/not(II),O):- compound(II), II= /**/not(I), !, cvt0(T, I,O),!.
cvt0(T, /**/not(I),O):- !, cvt0(T, I,M), correct_holds(/**/not, not(M), O). 
cvt0(T, not(I),O):- !, cvt0(T, I,M), correct_holds(/**/not, not(M), O). 
cvt0(T, happens_at(F, T1, T2), O):- T1==T2, cvt0(T, happens_at(F, T1), O).
% cvt0(_T, happens_at(F, T1), happens_at(F, T1)):- !.
cvt0(_, ec_current_domain_db1(X), ec_current_domain_db1(X)).
cvt0(T, ec_current_domain_db2(X,Y), ec_current_domain_db2(X,Y)):- ignore(Y=T).

cvt0(T, [G1|G2], [GG1|GG2]):- !, cvt0(T, G1, GG1),cvt0(T, G2, GG2). 
cvt0(T, (G1,G2), (GG1,GG2)):- !, cvt0(T, G1, GG1),cvt0(T, G2, GG2). 
cvt0(T, (G1;G2), (GG1;GG2)):- !, cvt0(T, G1, GG1),cvt0(T, G2, GG2). 

/*

cvt0(_, happens_at(G, T), happens_at(G, T)):-!.
cvt0(_, happens_at(G, T, T2), happens_at(G, T, T2)):-!.
cvt0(_, /**/holds(G, T), /**/holds(G, T)):-!.
cvt0(_, /**/holds(G, T, T2), /**/holds(G, T, T2)):-!.
*/
cvt0(T, G, Gs):-  \+ into_lps, fix_goal_add_on_arg( T, G, Gs, _TExtra),!.
%cvt0(T,(G1,G2),GGs):- !, cvt0(T,G1,GG1),cvt0(T,G2,GG2).
%cvt0(T, P, P).
%cvt0(T,X,Y):- trace, ec_to_ax(T, X,Y).
cvt0(T, P, PP):-  \+ into_lps,
   P =..[F|Args],functor(P,F,A), arg_info(AxH,F,Arity),
   functor(Arity,_,N), 
   correct_ax_args(T,F,A,Args,AxH,Arity,N,PP),!. 
% cvt0(_T, exists(X,Y), exists(X,Y)):-!.
cvt0(T, HT, HTTerm):-  
 compound_name_arguments(HT, F, L),
 % upcase_atom(F,U),downcase_atom(F,U),
 must_maplist(cvt0(T),L,LL),
 ( LL\==L -> compound_name_arguments(HTTerm, F, LL) ; HT = HTTerm ), !.
cvt0(_, G, G).

must_or_dumpst(G):- call(G),!.
must_or_dumpst(G):- ignore(rtrace(G)),dumpST,break.

needs_cononicalization(_):- into_lps, no_canon, !, fail.
needs_cononicalization(',').
needs_cononicalization(';').
needs_cononicalization('exists').
needs_cononicalization('all').
needs_cononicalization('if').
needs_cononicalization('iff').
needs_cononicalization('equiv').
needs_cononicalization('implies').
needs_cononicalization('->').
needs_cononicalization('<->').
needs_cononicalization('and').
needs_cononicalization('xor').
needs_cononicalization('or').
needs_cononicalization('&').
needs_cononicalization('|').
needs_cononicalization('dia').
needs_cononicalization('box').
needs_cononicalization('cir').
needs_cononicalization(X):-  fix_predname(X, Y),!, X\==Y, needs_cononicalization(Y).




negations_inward_to_list(C,L):- negations_inward(C,I),conjuncts_to_list(I, L).
%  must(convert_to_axiom(lsvm(L,F,Vs,M),HB,NEWHB)),
%  do_process_ec(assert_ele,M, NEWHB),
conjuncts_to_list_body(Body, Conds):- 
  conjuncts_to_list(Body, CondsL),
  must_maplist(negations_inward_to_list,CondsL,CondsLI),
  append(CondsLI,Conds).
  

skipped_head(H):- \+ compound(H),!,fail.
skipped_head(diff(_,_)).
skipped_head(equals(_,_)).
skipped_head(equals(_,_)).
skipped_head(not(H)):- !, skipped_negated_head(H).
skipped_negated_head(H):- \+ compound(H),!,fail.
skipped_negated_head(allDifferent(_)).
skipped_negated_head(equals(_,_)).
skipped_negated_head(diff(_,_)).
skipped_negated_head(axiom_uses(_,_)).
skipped_negated_head(some(_,_)).
skipped_negated_head(comparison(_,_,_)).

assert_ele_clauses(X,L,L):- is_list(L), !, 
 length(L,N), 
 ((N > 19, false )
  -> 
   (assert_ready(magenta,
         todo_later1(N,X)),
     must_maplist(pprint_ecp_cmt(blue),L),
      sleep(1.0))
   ; must_maplist(assert_ele_clauses(X,L),L)).


assert_ele_clauses(_X,_L,(H:-B)):- skipped_head(H), !,
   nop((pprint_ecp_cmt(yellow,skipped_head(H):-B))),!.

assert_ele_clauses(_X,_L,(H:-B)):- !,
  only_dec_pl(pprint_ecp_cmt(red,(H:-B))),
  conjuncts_to_list_body(B, BL),
  assert_axiom(H , BL).
assert_ele_clauses(_X,_L,H):-  
  assert_axiom(H , []). 

%assert_m_axiom(Ax):- (e_to_pel(Ax,X)->Ax\==X),!,assert_m_axiom(Ax).
assert_m_axiom(Ax):- 
  retract(ec_tmp:do_next_axiom_uses(Value)),!,
  assert_m_axiom(axiom_uses(Value)->Ax).
assert_m_axiom('<->'(A,B)):- !, assert_m_axiom(A->B), assert_m_axiom(B->A). 
assert_m_axiom('<-'(A,B)):- !, assert_m_axiom(A->B). 

assert_m_axiom(X):- \+ no_canon,
  major_debug(pprint_ecp_cmt(green, clausify_pnf=X)),
  with_output_to(string(_), clausify_pnf(X,Conds)), 
  conjuncts_to_list(Conds,CondsL),
  assert_ele_clauses(X,CondsL,CondsL).

%assert_m_axiom(A->B):- into_lps, !, assert_lps(A->B),!. 

assert_m_axiom(A->B):- !, assert_axiom(B,[A]),!. 
assert_m_axiom(H):- assert_axiom(H, []),!.

:- export_transparent(assert_axiom/2).


assert_axiom(Conds, []):- is_list(Conds),!, must_maplist(assert_ele,Conds).
assert_axiom(AxHead, append3(L1,L2,LL)):- 
   conjuncts_to_list_body(L1,LL1),conjuncts_to_list_body(L2,LL2),
   append([LL1,LL2,LL],L12),!,
   assert_axiom(AxHead, L12).
assert_axiom(AxHead, B) :- \+ is_list(B), !,
  conjuncts_to_list_body(B,Bs),
  assert_axiom(AxHead, Bs).

assert_axiom(AxHead, B):- 
  retract(ec_tmp:do_next_axiom_uses(Value)),!,
  assert_axiom(AxHead, [axiom_uses(Value)|B]).


/*
assert_axiom_unused(happens_at(A,T), []):- number(T), !,
   assert_axiom(happens_at(A,T), [is_time(T)]).

assert_axiom_unused(happens_at(A,T), []):- !,
   assert_axiom(happens_at(A,T), [is_time(T)]).  
*/

assert_axiom(Conds, [happens_at(A,T)]):-
   conjuncts_to_list_body(Conds, B ), 
   functors_are(\+ happens_at, B), !,
   %trace,
   debug_var(when,T),
   assert_axiom(requires(A,T),/**/holds(metreqs(A),T)),
   assert_axiom(/**/holds(metreqs(A),T),B).

/*
assert_axiom_unused(Conds, [happens_at(A,T)]):-
   conjuncts_to_list(Conds, B ), 
   functors_are(\+ happens_at, B), !,
   assert_axiom(requires(A,T),Conds),!.
*/

assert_axiom(AxHead, Some):- is_list(Some), select(E,Some,Rest), compound(E), E = (A,B), !,
   conjuncts_to_list_body((A,B), Conds),
   append(Conds,Rest,BReast),
   assert_axiom(AxHead, BReast).

assert_axiom(AxHead, B):-
  semi_legit_time(AxHead,TimeBase),
  (is_ftVar(TimeBase) -> must(TimeBase = T) ; T = _),
  fix_goal(T,B,Bs), B\=@=Bs, !, 
  assert_axiom(AxHead, Bs).

assert_axiom(AxHead, B):-
  AxHead=..[Effect,Event,Fluent,T],
  callable(Event),
  member(Effect,[initiates_at,terminates_at,releases_at]),
  assert_effect(Effect,Event,Fluent, T, B), !.

assert_axiom(H,B) :- 
  ignore((semi_legit_time(H+B,TimeBase),is_ftVar(TimeBase));semi_legit_time(H+B,TimeBase)),
  (is_ftVar(TimeBase) -> must(TimeBase = Time) ; Time = _ ),

  must_maplist(show_fix_axiom_head(Time),[H|B],[HH|BB]),

  correct_axiom_time_args_other(Time,HH,BB,HHH,BBB),
  with_no_brk_on_bind(quietly(notrace(only_dec_pl(report_time_values(Time,HH,BB,HHH,BBB))))),
  must(assert_axiom_2(HHH,BBB)),!.


report_time_values(Time,HH,BB,HHH,BBB):-
  get_time_values(axiom(HH,BB),TimeVs1),
  get_time_values(axiom(HHH,BBB),TimeVs2),
  %ignore(TimeVs2=TimeVs1),
  pprint_ecp_cmt(green,BBB->ta(Time,tvs1=TimeVs1,tvs2=TimeVs2,HHH)),!.

hide_compound(ignore(_)).
msub_term(X, X).
msub_term(X, Term) :-
    compound(Term),
    \+ hide_compound(Term),
    arg(_, Term, Arg),
    msub_term(X, Arg).

correct_axiom_time_args_other(_Time,HH,BB,HHH,BBB):- into_lps,!, HH=HHH,BB=BBB,!.
correct_axiom_time_args_other(_Time,HH,BB,HHH,BBB):- correct_axiom_time_args(t,HH,BB,HHH,BBB), (HH\==HHH;BB\==BBB),!.
correct_axiom_time_args_other(Time,HH,BB,HHH,BBB):- atom(Time),subst(HH+BB,Time,NewTime,H+B),
  correct_axiom_time_args_other(NewTime,H,B,HHH,BBB),!.
correct_axiom_time_args_other(Time,HH,BB,HHH,[ignore(Sub==NewTime),b(Lo,Hi)|BBB]):- 
   msub_term(Sub,axiom(HH,BB)),compound(Sub),functor(Sub,F,2),member(F,[(+),(-)]),
   sub_term(SV,Sub),SV==Time,!,
   % gensym(newTime,NewTime), 
   subst(HH+BB,Sub,NewTime,H+B),
   ((F == +) -> Lo+Hi=Time+NewTime ; Lo+Hi=NewTime+Time),correct_axiom_time_args_other(NewTime,H,B,HHH,BBB),!.
correct_axiom_time_args_other(_Time,HH,BB,HHH,BBB):- correct_axiom_time_args(t,HH,BB,HHH,BBB),!.

is_symetric_pred(equals).
is_symetric_pred(diff).

fix_symetrics(_,O,O):- \+ compound(O),!.
fix_symetrics(_, allDifferent(List), allDifferent(List2)):- sort(List,List2),!.
fix_symetrics(H, BXY, BYX):- compound_name_arguments(BXY, F, [X,Y]), 
  is_symetric_pred(F), \+ compound(X),
  contains_var(Y,H), \+ contains_var(X,H), !, 
  compound_name_arguments(BYX, F, [Y,X]).

fix_symetrics(H, BT, BTTerm):-  
 compound_name_arguments(BT, F, L),
 must_maplist(fix_symetrics(H),L,LL),
 compound_name_arguments(BTTerm, F, LL), !.

/*
assert_axiom(AxHead, Some):- select(E,Some,Rest), E = (A;B), !, nnf(A,NotA), nnf(B,NotB),
   assert_axiom(AxHead, append3(A,NotB,Rest)),
   assert_axiom(AxHead, append3(B,NotA,Rest)).
*/
assert_axiom_2(AxHead, B) :- \+ is_list(B), !,
  conjuncts_to_list_body(B,Bs),
  assert_axiom_2(AxHead, Bs).

assert_axiom_2(AxHead, B) :- 
  must_maplist(fix_symetrics(AxHead), B,Bs)-> B\== Bs,
  assert_axiom_2(AxHead, Bs).

assert_axiom_2(AxHead, Some):- use_proxy_kr, Some=[_,_|_],  member(E,Some), E = (_;_), 
   term_variables(E,Vars),
   gensym(disj_,Ref),
   P =.. [Ref|Vars],
   subst(Some,E,P,NewSome), !,
   assert_ele('->'(E,P)),
   assert_axiom_2(AxHead, NewSome).

assert_axiom_2(AxHead, Some):- use_proxy_kr, select(E,Some,Rest), E = (_;_), Rest\==[], !,
   term_variables(E,Vars),
   gensym(disj_,Ref),
   P =.. [Ref|Vars],
   assert_ele('->'(E,P)),
   assert_axiom_2(AxHead, [P|Rest]).

assert_axiom_2(AxHead, Some):- breakup_ors, select(E,Some,Rest), E = (A;B), !,
   assert_axiom_2(AxHead, [A|Rest]),
   assert_axiom_2(AxHead, [B|Rest]).

/*
assert_axiom((A1,A2), B):- 
  assert_axiom(A1, [possible(A2)|B]),
  assert_axiom(A2, [possible(A1)|B]).
assert_axiom(A1;A2, B):- 
  assert_axiom(A1, [not(A2)|B]),
  assert_axiom(A2, [not(A1)|B]).
*/

assert_axiom_2(H,B):- 
  compound(H), compound_name_arity(H, F, 2), needs_cononicalization(F), !,
  list_to_conjuncts(B, BB),
  assert_m_axiom('->'(BB,H)).

assert_axiom_2(H,B):-
  assert_ready(axiom(H,B)).

% Normals
assert_effect(Effect,(A1,A2),Fluent,T,B):- !,
   assert_effect(Effect,A1,Fluent,T,[possible(A2)|B]),
   assert_effect(Effect,A2,Fluent,T,[possible(A1)|B]).
assert_effect(Effect,(A1;A2),Fluent,T,B):- !,
   assert_effect(Effect,A1,Fluent,T,[possible(not(A2))|B]),
   assert_effect(Effect,A2,Fluent,T,[possible(not(A2))|B]).

breakup_ors:- fail.
use_proxy_kr:- fail.

:- use_module(ec_nnf).


:- export_transparent(rect/0).    

% recompiles and restart tests everytime source is updated
rect:- once(ect), % first run
   repeat,
   wait_for_input([current_input],Was,0.5), 
   make:modified_file(_Any),
   once(ect), 
   Was == [current_input]. 

rect2:- 
   once(ect), % first 
   wait_for_input([current_input],Was,0.5), 
   ( \+ make:modified_file(_Any) -> rect2; 
   ( Was \== [current_input] -> rect2; true)). 
  
                         

% event calc tests
:- export_transparent(ect/0).
ect:- call(call,ect1).

:- export_transparent(ect1/0).
ect1:- 
   cls, update_changed_files,  Out = translate_e_to_pfc,
   call(Out, ['../*/*/*/*.e','../*/*/*.e','../*/*.e']),
   list_undefined,
   list_void_declarations,  
  !.

:- export_transparent(ect2/0).
ect2:- 
   cls, update_changed_files,  Out = translate_e_to_pfc,
   call(Out, 'examples/FrankEtAl2003/Story1.e'),
   % call(Out, 'ectest/ec_reader_test_ecnet.e'),
   call(Out, 'ecnet/GSpace.e'),
   call(Out, 'ecnet/Diving.e'),
   call(Out, 'ecnet/RTSpace.e'),
   call(Out, 'ecnet/SpeechAct.e'),
   call(Out, 'ecnet/Kidnapping.e'),
   call(Out,'examples/Mueller2006/Exercises/MixingPaints.e'),
   call(Out,'examples/Mueller2006/Chapter11/HungryCat.e'),
   call(Out, 'examples/AkmanEtAl2004/ZooWorld.e'),   
   % call(Out,'examples/Mueller2006/Chapter11/*.e'),

   list_undefined,
   list_void_declarations,  
  !.

:- export_transparent(ect1_cvt_e_pl/0).
ect1_cvt_e_pl:- 
   cls, update_changed_files,  Out = cvt_e_pl,
   call(Out, ['../*/*/*/*.e','../*/*/*.e','../*/*.e']),
   list_undefined,
   list_void_declarations,  
  !.

:- export_transparent(ect2_cvt_e_pl/0).
ect2_cvt_e_pl:- 
   cls, update_changed_files,  Out = cvt_e_pl,
   call(Out, 'examples/FrankEtAl2003/Story1.e'),
   % call(Out, 'ectest/ec_reader_test_ecnet.e'),
   call(Out, 'ecnet/GSpace.e'),
   call(Out, 'ecnet/Diving.e'),
   call(Out, 'ecnet/RTSpace.e'),
   call(Out, 'ecnet/SpeechAct.e'),
   call(Out, 'ecnet/Kidnapping.e'),
   call(Out,'examples/Mueller2006/Exercises/MixingPaints.e'),
   call(Out,'examples/Mueller2006/Chapter11/HungryCat.e'),
   call(Out, 'examples/AkmanEtAl2004/ZooWorld.e'),   
   % call(Out,'examples/Mueller2006/Chapter11/*.e'),

   list_undefined,
   list_void_declarations,  
  !.



fix_goal_add_on_arg(T, G, G0, [b(T,T2),b(T2,end)]):- G =.. [F,A],  already_good(F,2), G0 =.. [F,A,T].%, next_t(T,T2).
fix_goal_add_on_arg(T, G, G0, [b(T,T2),b(T2,end)]):- G =.. [F,A,B], already_good(F,3), \+ already_good(F,2), G0 =.. [F,A,B,T]. %, next_t(T,T2).


:- export_transparent(fix_goal/3).


to_axiom_head(T,G,GG) :-  fix_axiom_head(T,G,GG),!.
to_axiom_head(T,G,GG) :-  trace,fix_axiom_head(T,G,GG),!.


fix_goal(_, X, Y):- prefer_lps(X,Y), !.
fix_goal(_, Nil,[]):- Nil==[],!.
fix_goal(T,[G|Gs],GGs):- !, fix_goal(T,G,G0),fix_goal(T,Gs,Gs0),append(G0,Gs0,GGs),!.
fix_goal(T,(G,Gs),GGs):- !, fix_goal(T,G,G0),fix_goal(T,Gs,Gs0),append(G0,Gs0,GGs),!.
fix_goal(T,{Gs},{GGs}):- !, fix_goal(T,Gs,GGs).
fix_goal(T, G, GGs):- into_lps, (fix_axiom_head(T,G,GG)-> G\==GG), !, fix_goal(T, GG, GGs).
fix_goal(T, G, GGs):- fix_axiom_head(T,G,GG),!, listify(GG,GGs).
fix_goal(T, G, [Gs| TExtra]):-  \+ into_lps, fix_goal_add_on_arg( T, G, Gs, TExtra),!.
fix_goal(T, G, GGs):- to_axiom_head(T,G,GG),!, listify(GG,GGs).
fix_goal(_, /**/holds(G, T3), [/**/holds(G, T3)]):- into_dec_pl,!.
fix_goal(T, G, [/**/holds(G, T)]):- into_dec_pl,!.


ec_to_ax(_, X, Y):- prefer_lps(X,Y), !.
ec_to_ax(_, X,Y):-  (\+ callable(X) ; \+ compound(X)), !, X=Y.
%ec_to_ax(T, (Pre -> iff(HB,BH)), HBO):- ec_to_ax(T, (iff((Pre,HB),(Pre,BH))), HBO).
%ec_to_ax(T, or(Pre , '->'(HB,BH)), HBO):- ec_to_ax(T, '->'(or(Pre , HB),BH), HBO).
%ec_to_ax(T, (H<-B),O):- !, into_axiom(T,H,B,O).
ec_to_ax(T, '->'(B,H),O):- !, into_axiom(T,H,B,O).
ec_to_ax(T, '<->'(HB1,HB2),[A,B]):- !, ec_to_ax(T, '->'(HB1,HB2),A),ec_to_ax(T, '->'(HB2,HB1),B).
ec_to_ax(T, axiom(H,B),O):- into_axiom(T,H,B,O), !.
ec_to_ax(_, axiom(H,B), axiom(H,B)):- !.
ec_to_ax(T,X,O):- fix_axiom_head(T,X,Y), !, (X==Y -> X=O ; ec_to_ax(T,Y,O)),!.
ec_to_ax(_, X,X).

to_axiom_body(T,G,GGs) :-  fix_goal(T,G,GGs).

%into_axiom(T,H,happens_at(E,T),OUT):- into_axiom(T,not(happens_at(E,T)),not(H),OUT).
into_axiom(T,H,B,'->'(ABNonList,AH)):- to_axiom_head(T1,H,AH),
  to_axiom_body(T2,B,AB),!,ignore(T=T1),ignore(T2=T1),
  list_to_conjuncts(AB, ABNonList),!.





%as_equals(X,Y,=(X,Y)):-!.
%as_equals(X,Y,Equals):- compound(X),append_term(X,Y, Equals),!.
as_equals(X,Y,equals(X,Y)).


syntx_term_check(G):- var(G),!,fail.
syntx_term_check(G):- is_ftVar(G),!,fail.
syntx_term_check((G1;G2)):- !, syntx_term_check(G1); syntx_term_check(G2).
syntx_term_check(G):- predicate_property(G,clause_count(_)), clause(G,_).
syntx_term_check(G):- clause(user:ec_current_domain_db2(G, _),_).
syntx_term_check(G):- into_lps, G=..[F,A], ec_lps_convert:argtype_pred(F,FS),GS=..[FS,[A]],syntx_term_check(GS).


functor_skel(G,P):- compound(G), compound_name_arity(G,F,A), compound_name_arity(P,F,A),!.
functor_skel(G,P):- atom(G),P=G.

between_r(H,L,N):- nonvar(N),!,between(L,H,N).
between_r(H,L,N):- Hm1 is H - L, !, between(L,H,NN), N is NN + Hm1.

can_be_time_arg(Var):- is_ftVar(Var),!.
can_be_time_arg(_+_):-!.
can_be_time_arg(_-_):-!.
can_be_time_arg(A):- atom(A).

semi_legit_time(V,_):- \+ compound_gt(V,0), !, fail.
semi_legit_time(Holds,T1):- sub_term(Holds1,Holds),compound_gt(Holds1,0),
   functor(Holds1,F,_),
   time_arg(F,N), arg(N,Holds1,T1),can_be_time_arg(T1).
semi_legit_time(Holds,T1):- sub_term(Holds1,Holds),compound_gt(Holds1,0),
   functor(Holds1,F,_),
   time_arg(F,N), arg(N,Holds1,T1),\+ can_be_time_arg(T1).

/*
semi_legit_time(Holds1,T1):- 
   functor(Holds1,_,A), 
   member(P1,[number,string,atom]),
   (arg(A,Holds1,T1);arg(_,Holds1,T1)), 
   T1\==[], call(P1,T1).
*/

:- export_transparent(compare_on_time_arg/3).
compare_on_time_arg(Result,Holds1,Holds2):- 
   (((semi_legit_time(Holds1,T1),semi_legit_time(Holds2,T2),
      compare(Result,T1,T2), Result\== (=))) 
     -> true;
        compare_on_time_arg(Result,Holds1,Holds2)).

time_arg(b, N):- between(1,2,N).
time_arg(beq, N):- between(1,2,N).
time_arg(/**/holds, 2).
time_arg(is_time, 1).
time_arg(happens_at, N):- between_r(3,2,N), N\=1.
time_arg(clipped, N):- between_r(3,1,N), N\=2.
time_arg(declipped, N):- between_r(3,1,N), N\=2.
time_arg(F, N):- arg_info(axiom_head,F,V),compound(V),arg(N,V,time).

get_time_values(G,Set):- (setof(ST,semi_legit_time(G,ST),List),list_to_set(List,Set))->true;Set=[].
:- export_transparent(fix_time_args/3).
fix_time_args(T,G,Gss):- \+ is_list(G), conjuncts_to_list_body(G, Gs), !,fix_time_args(T,Gs,Gss)   .
fix_time_args(T,[G|Gs],Gss):- 
  semi_legit_time([G|Gs],ST),
  fix_time_args1(ST,[G|Gs],Gs0),
  fix_time_args2(T,Gs0,Gss).

%fix_time_args2(_,Gs,Gs):-!.
fix_time_args2(_,Gs,Gss):-
  list_to_set([b(start,t),b(t,end)|Gs],Gss),!.

visit_time_args(_,   In,G,G,In):- \+ compound(G),!.
visit_time_args(Stem,In,[G|Gs],[GO|GsO],Out):- !, 
    visit_time_args(Stem,In,G,GO,Mid),
    visit_time_args(Stem,Mid,Gs,GsO,Out).
visit_time_args(Stem,In,/**/holds(A,T1),/**/holds(A,T1R),Out):- !,
   correct_time_arg(Stem,In,T1,T1R,Out).
visit_time_args(Stem,In,happens_at(A,T1,T2),happens_at(A,T1R,T2R),Out):- !,
   correct_time_arg(Stem,In,T1,T1R,B0),
   correct_time_arg(Stem,B0,T2,T2R,Out).
visit_time_args(Stem,In,happens_at(A,T1),happens_at(A,T1R),Out):- !,
   correct_time_arg(Stem,In,T1,T1R,Out).
visit_time_args(Stem,In,b(T1,T2),b(T1R,T2R),Out):- !,
   correct_time_arg(Stem,In,T1,T1R,B0),
   correct_time_arg(Stem,B0,T2,T2R,Out).
visit_time_args(Stem,In,not(G),not(GG),Out):- !, visit_time_args(Stem,In,G,GG,Out).
visit_time_args(Stem,In,beq(T1,T2),beq(T1R,T2R),Out):- !,
   correct_time_arg(Stem,In,T1,T1R,B0),
   correct_time_arg(Stem,B0,T2,T2R,Out).
visit_time_args(Stem,In,clipped(T1,A,T2),clipped(T1R,A,T2R),Out):- !,
   correct_time_arg(Stem,In,T1,T1R,B0),
   correct_time_arg(Stem,B0,T2,T2R,Out).
visit_time_args(Stem,In,declipped(T1,A,T2),declipped(T1R,A,T2R),Out):- !,
   correct_time_arg(Stem,In,T1,T1R,B0),
   correct_time_arg(Stem,B0,T2,T2R,Out).
visit_time_args(Stem,In, HT, HTO,Out):- compound_name_arguments(HT, F, L), !,
  visit_time_f_args(Stem,In,F,1,L, LL,Out),
  compound_name_arguments(HTO, F, LL).

visit_time_f_args(_Stem,InOut,_, _, [], [],InOut):-!.
visit_time_f_args(Stem,In,F, N, [T2|L], [T2R|LL],Out):- time_arg(F,N),!,
   correct_time_arg(Stem,In, T2,T2R,MID),N2 is N+1,
   visit_time_f_args(Stem,MID,F, N2, L, LL,Out).
visit_time_f_args(Stem,In,F, N, [HT|L], [HTO|LL], Out):- N2 is N+1,
   visit_time_args(Stem,In, HT, HTO, MID),
   visit_time_f_args(Stem,MID,F, N2, L, LL,Out).


correct_time_arg(_Stem,In, TN, TN, In):- is_ftVar(TN), !.
correct_time_arg(_Stem,In, TN, TN, In):- TN==start, !.
correct_time_arg(_Stem,In, TN, TN, In):- atom(TN), !.
correct_time_arg(_Stem,In, AM1,T, In):- compound(AM1),(AM1 = (AfterT- N)),compound(AfterT),after(T)=AfterT, N==1, is_ftVar(T), !.
correct_time_arg(Stem, In, TN, TpN, Out):- number(TN), !, correct_time_arg(Stem,In, Stem+TN, TpN, Out).
correct_time_arg(_Stem,In, v, _, In):- !.
correct_time_arg(_Stem,In, TN, TpN, In):- lookup_time_val(TN,TpN,In),!.
correct_time_arg(Stem,In, TN, TpN, [ignore(TN==TpN)|Out]):-  number(TN), !, correct_time_arg(Stem,In, Stem+TN, TpN, Out).
correct_time_arg(Stem,In, T-N, TpN, Out):- number(N), N<0, NN is abs(N),!,correct_time_arg(Stem,In, T+NN, TpN, Out).
correct_time_arg(Stem,In, T+N, TpN, Out):- number(N), N<0, NN is abs(N),!,correct_time_arg(Stem,In, T-NN, TpN, Out).
correct_time_arg(Stem,In, Now+N, T, [ignore(Now+N==T)|Out]):- concat_time_arg_syms, number(N), N>1, NN is N-1, correct_time_arg(_Stem,In, Now+1, Tm2, Mid),
  correct_time_arg(Stem, Mid, Tm2+NN, T, Out).
correct_time_arg(Stem,In, Now-N, T, [ignore(Now-N==T)|Out]):- concat_time_arg_syms, number(N), N<1, NN is N+1, correct_time_arg(_Stem,In, Now-1, Tm2, Mid),
  correct_time_arg(Stem, Mid, Tm2-NN, T, Out).
correct_time_arg(_Stem,In, T+N, T, In):- N==0,!.
correct_time_arg(_Stem,In, T-N, T, In):- N==0,!.
correct_time_arg(_Stem,In, T-N, TN, Out):- !, t_plus_or_minus_1(In, T-N, TN, Out).
correct_time_arg(_Stem,In, T+N, TN, Out):- !, t_plus_or_minus_1(In, T+N, TN, Out).
correct_time_arg(_Stem,In, TN, TN, In).

concat_time_arg_syms:- fail.

lookup_time_val(TN,TpN,In):- copy_term(TN,TNS),member(ignore(TNS==TpN),In),TNS=@=TN,!.

t_plus_or_minus_1(In, TN, TpN, In):- lookup_time_val(TN,TpN,In),!.
t_plus_or_minus_1(In, T-N, TN, In):- N==1, memberchk(b(TN,T),In),!.
t_plus_or_minus_1(In, T+N, TN, In):- N==1, memberchk(b(T,TN),In),!.
t_plus_or_minus_1(In, T-N, TN, [b(TN,T),ignore(T-N==TN)|In]):- N==1,!.
t_plus_or_minus_1(In, T+N, TN, [b(T,TN),ignore(T+N==TN)|In]):- N==1,!.
t_plus_or_minus_1(In, T+N, TN, [b(T,TN),toffset(T,N,TN),ignore(T+N==TN)|In]):- is_ftVar(N),!.
t_plus_or_minus_1(In, T-N, TN, [b(TN,T),toffset(TN,N,T),ignore(T-N==TN)|In]):- is_ftVar(N),!.
t_plus_or_minus_1(In, T+N, TN, [b(T,TN),toffset(T,N,TN),ignore(T+N==TN)|In]):- number(N),!,debug_var([T,N],TN).
t_plus_or_minus_1(In, T-N, TN, [b(TN,T),toffset(TN,N,T),ignore(T-N==TN)|In]):- number(N),!,debug_var([T,minus,N],TN).
%t_plus_or_minus_1(In, T-N, TN, [b(TN,T),ignore(T-N==TN)|In]):- N==1, must((next_t(TN,T))),!.
%t_plus_or_minus_1(In, T+N, TN, [b(T,TN),ignore(T+N==TN)|In]):- N==1, must((next_t(T,TN))),!.
t_plus_or_minus_1(In, T-N, TN, [b(TN,T),ignore(T-N==TN)|In]):- ((ground(T+N), atomic_list_concat([T,N],minus,TN))),!.
t_plus_or_minus_1(In, T+N, TN, [b(T,TN),ignore(T+N==TN)|In]):- ((ground(T+N), atom_concat(T,N,TN))),!.
t_plus_or_minus_1(In, T-N, TN, [b(TN,T),ignore(T-N==TN)|In]):- gensym(t_less,TN).
t_plus_or_minus_1(In, T+N, TN, [b(T,TN),ignore(T+N==TN)|In]):- gensym(t_more,TN).
/*
next_t(T,Var):- is_ftVar(T),is_ftVar(Var),!.
next_t(start,t).
next_t(T,Var):- is_ftVar(T),!,Var=after(T).
next_t(t,now).
next_t(now,aft).
next_t(aft,Aft_1):- is_ftVar(Aft_1),!,gensym(aft_,Aft_1).
next_t(aft,completeFn(_)).
*/

fix_time_args1(T,G,Gs):- 
  visit_time_args(T,[],G,Gs,_Mid).



already_good(comparison, 3).
already_good(some, 2).
already_good(some, 1).
already_good(call, 1).
already_good(requires, 2).
already_good(equals, 2).
already_good(axiom_uses, 2).
already_good(is, 2).
already_good(diff, 2).
already_good(dif, 2).
already_good(allDifferent, 1).
already_good(terms_or_rels,3).
already_good(ignore, 1).
already_good(F,A):- into_dec_pl, already_good_dec_pl(F,A).

already_good_dec_pl(holds, 2):- holds_not_holds_at,!.
already_good_dec_pl(happens_at, 2).
already_good_dec_pl(happens_at, 3).
already_good_dec_pl(/**/holds_at, 2).
already_good_dec_pl(/**/holds_at, 3).
already_good_dec_pl(b, 2).
already_good_dec_pl(toffset, 3).
already_good_dec_pl(F, 1):- arg_info(domain,F,arginfo).
already_good_dec_pl(F, N):- arg_info(axiom_head,F,Args),compound(Args),functor(Args,v,N).
already_good_dec_pl(F, 1):- fixed_already(F).
already_good_dec_pl(F,A):- functor(P,F,A),syntx_term_check(predicate(PP)),compound(PP),PP=P.
already_good_dec_pl(F,A):- functor(P,F,A),syntx_term_check(predicate(PP)),compound(PP),PP=P.
already_good_dec_pl(Add_at,N):- atom(Add_at), \+ atom_concat(_,'_at',Add_at),  atom_concat(Add_at,'_at',With_at), already_good_dec_pl(With_at,N).



%predicate, option range load fluent event noninertial xor completion


%special_directive('!').
%special_directive('/**/not').

% builtin_pred(releasedAt).

%xfr_body(true,[]).



:- export_transparent(get_linfo/1).

get_linfo(lsvm(L,F,Vs,M)):- 
  quietly((must(ec_reader:s_l(F,L)),!,
  '$current_source_module'(M),
  nb_current('$variable_names',Vs))).



:- thread_local(t_l:no_brk_on_bind/0).                    

brk_on:attr_unify_hook(Var,Vars):- brk_on_unify_hook(Var,Vars).
brk_on:attribute_goals(Var,[brk_on(Vars,Var)|L],L):- get_attr(Var,brk_on,Vars).

brk_on_unify_hook(_,_):- t_l:no_brk_on_bind,!.
brk_on_unify_hook(Var,Vars):- 
 (is_ftVar(Var), \+ ( member(E,Vars), E==Var)) -> true ;
    if_debugging(ec,((dumpST,wdmsg(brk_on(Var)),break))).

brk_on_bind(_):-!.
brk_on_bind(HB):- term_variables(HB,Vars),must_maplist(brk_on(Vars),Vars).
brk_on(Vars,X):- ord_del_element(Vars,X,Rest),put_attr(X,brk_on,Rest).
with_no_brk_on_bind(Goal):- locally(t_l:no_brk_on_bind, Goal).



needs_process_axiom(C):- \+ compound(C), !, fail.
needs_process_axiom(axiom(_,_)).
needs_process_axiom(axiom(_)).
needs_process_axiom(predicate(_)).
needs_process_axiom(event(_)).
needs_process_axiom(P):- compound_name_arity(P,F,A),needs_process_axiom_fa(F,A).

needs_process_axiom_fa(iff,2).
%needs_process_axiom_fa('<-',2).
needs_process_axiom_fa('->',2).
needs_process_axiom_fa(F,A):- arg_info(_,F,Args), (Args==arginfo-> true; functor(Args,_,A)).


:- export_transparent(needs_proccess/3).
needs_proccess(_File, PA,_):- \+ compound(PA),!,fail.
needs_proccess( File, M:H, How):- !, M\==system, nonvar(H),!,needs_proccess(File, H,How).
needs_proccess(_File, PA, process_ec):- needs_process_axiom(PA),!.
needs_proccess( File, (axiom(_,_) :- _),process_ec):- \+ skipped_ec_file(File), !.
needs_proccess( File, (H :- _),How):- \+ skipped_ec_file(File), !, nonvar(H),!,needs_proccess(File, H,How).

:- export_transparent(process_ec/1).

process_ec( HB ):- notrace(must(get_linfo(T))), process_ec( T, HB ).
%:- export_transparent(process_ec/2).
%process_ec( _, HB , T):- !,process_ec( T, HB ).


:- export_transparent(process_ec/2).

process_ec( _, HB ):- brk_on_bind(HB), assert_ele(HB),!.
process_ec( lsvm(L,S,Vs,M), HB ):- fail,  
  must(convert_to_axiom(lsvm(L,S,Vs,M),HB,NEWHB)),
  do_process_ec(assertz,M, NEWHB).

merge_into_body(X,_Y,Z):- Z = X.

:- export_transparent(do_process_ec/3).
do_process_ec(_Why, M, NonCallable) :- break, assertion((current_module(M),callable(NonCallable))), fail.
do_process_ec(Why, M, NEWHB):- is_list(NEWHB), !, must_maplist(do_process_ec(Why, M), NEWHB).
do_process_ec(_Why, M, (:- GOAL)):- !, must(M:GOAL).
do_process_ec(_Why, M, (?- GOAL)):- !, (M:forall(GOAL, true)).
% How to? M:assertz('$source_location'(S, L):NEWHB),
%do_process_ec(Why, M, NEWHB):- wdmsg(do_process_ec(Why, M, NEWHB)),fail.
do_process_ec(Why, M, NEWHB):- M:call(Why, NEWHB).

e_reader_teste2:- 
     convert_to_axiom(fff,
  ((/**/holds(beWaiter3(waiterOf(Restaurant)), Time),
    exists([Agent], /**/holds(knowOrder(waiterOf(Restaurant), Agent, Food), Time))) ->
       ( happens_at(order(waiterOf(Restaurant),
                      cookOf(Restaurant),
                      Food),
                Time))),O),
     assert_ready(e,O).

:- export_transparent(convert_to_axiom/3).
convert_to_axiom(T,A,A):- into_lps, throw(convert_to_axiom(T,A,A)),!.
convert_to_axiom(T, M:H, [M:HH]):- !, convert_to_axiom(T, H, HH).
convert_to_axiom(T, (H:-B),[(HH:- B)]):- !, convert_to_axiom(T, H,HH).
convert_to_axiom(_, predicate(H), predicate(H)):- !.
convert_to_axiom(_, t(C, E), List):- !, to_fact_head([sort(C),t(C, E)],List).

convert_to_axiom(L,[H|T],ABC):- % trace, 
   once((convert_to_axiom(L,H,A), convert_to_axiom(L,T,B),append(A,B,AB))), 
   AB\=@= [H|T],
   convert_to_axiom(L,AB,ABC).

convert_to_axiom(T, (Pre -> '<->'(HB,BH)), HBO):-
  convert_to_axiom(T, ('<->'((Pre,HB),(Pre,BH))), HBO),!.

convert_to_axiom(T, '<->'(HB,BH), HBOO):-
  convert_to_axiom(T, '->'(HB,BH), HBO1),
  convert_to_axiom(T, '->'(BH,HB), HBO2),
   % convert_to_axiom(T, '<-'(HB,BH), HBO2),
  flatten([HBO1,HBO2],HBO),
  convert_to_axiom1(T,HBO,HBOO),!.


convert_to_axiom(T, exists(Vars,BH), HBO):- convert_exists(exists(Vars,BH), Conj),!,
  convert_to_axiom(T, Conj , HBO).
%convert_to_axiom(T, exists(Vars,Info), HBO):- !, convert_to_axiom(T, Info, HB), 
%  merge_into_body(HB,some(Vars),HBO).

%  compound_name_arity(P,F,A),compound_name_arity(PP,F,A).


convert_to_axiom(T, X, O):- nop(debug_var('AxTime',Time)), ec_to_ax(Time, X,Y), 
  (is_list(Y)->convert_to_axiom1(T, Y, O); (X\=Y -> convert_to_axiom(T, Y, O);convert_to_axiom1(T, Y, O))), !.
convert_to_axiom(T, Y, O):- convert_to_axiom1(T, Y, O).

% convert_to_axiom1(T, '->'(E,B), HBO2):- convert_to_axiom(T, precond(B,E), HBO2).
to_fact_head(H,List):- H=List.


convert_to_axiom1(_, EOF, []) :- EOF = end_of_file,!.
convert_to_axiom1(T, P, O):- is_axiom_head(P),!, convert_to_axiom1(T, axiom(P), O).
convert_to_axiom1(T, axiom(P), O):- convert_to_axiom1(T, axiom(P ,[]), O).
convert_to_axiom1(_LSV, axiom(X,Y), [axiom(X,Y)]).
convert_to_axiom1(LSV, Pred, [ec_current_domain_db2(Pred,LSV)]).

convert_exists( exists(Vars,B -> H), (B -> Conj)):- conjoin(H,some(Vars),Conj), !.
convert_exists( exists(Vars, H), HBO):- conjoin(H,some(Vars),Conj), !,  Conj = HBO.

is_axiom_head(P):- compound_name_arity(P,F,_), arg_info(axiom_head,F,_),!.
is_axiom_head(P):- functor_skel(P, G), syntx_term_check(predicate(G)),!.


arg_info(domain,event,arginfo).
arg_info(domain,fluent,arginfo).
arg_info(domain,action,arginfo).
arg_info(domain,predicate,arginfo).
arg_info(domain,function,arginfo).
arg_info(domain,functional_predicate,v(pred,function)).

arg_info(domain,reified_sort,arginfo).

arg_info(predicate,noninertial,v(pred)).
arg_info(predicate,completion,v(pred)).
arg_info(predicate,next_axiom_uses,v(pred)).
arg_info(predicate,sort,v(sort)).
arg_info(predicate,subsort,v(sort,sort)).
arg_info(predicate,range,v(atomic,int,int)).
arg_info(predicate,t,v(sort,term)).
%arg_info(domain,axiom,v(axiom,list)).

%arg_info(axiom_head,b,v(time,time)).
arg_info(axiom_head,requires,v(event,time)).
arg_info(axiom_head,happens_at,v(event,time)).
arg_info(axiom_head,happens_at,v(event,time,time)).

arg_info(axiom_head,holds_at,v(fluent,time)).
arg_info(axiom_head,holds_at,v(fluent,time,time)).
arg_info(axiom_head,initially,v(fluent)).
arg_info(axiom_head,initiates_at,v(event,fluent,time)).
arg_info(axiom_head,terminates_at,v(event,fluent,time)).
arg_info(axiom_head,releases_at,v(event,fluent,time)). 
arg_info(axiom_head,released_at,v(fluent,time)). 

arg_info(axiom_head,clipped,v(time,fluent,time)).
arg_info(axiom_head,declipped,v(time,fluent,time)).
arg_info(axiom_head,trajectory,v(fluent,time,fluent,offset)).
arg_info(axiom_head,anti_trajectory,v(fluent,time,fluent,offset)).
arg_info(axiom_head,Add_at,VV):- atom(Add_at), \+ atom_concat(_,'_at',Add_at),  atom_concat(Add_at,'_at',With_at), arg_info(axiom_head,With_at,VV).
%arg_info(axiom_head,releasesAt,v(fluent,time)).
arg_info_arity(Type,F,A):- arg_info(Type,F,Info), (atom(Info) -> A=1 ; functor(Info,_,A)).

:- export(lock_ec_pred/2).
lock_ec_pred(_,_):- !.
lock_ec_pred(F,A):- current_predicate(system:F/A),!,dmsg(warn(current_predicate(system:F/A))),!.
lock_ec_pred(F,A):- 
  module_transparent(system:F/A),
  functor(P,F,A),
  assert((system:P:- local_database(P))),
  compile_predicates([system:P]),
  export(system:F/A),
  user:import(system:F/A),
  ec:import(system:F/A),
  ec_loader:import(system:F/A),
  ec_reader:import(system:F/A),
  user:export(system:F/A),
  lock_predicate(system:F/A),
  listing(F/A).


:- lock_ec_pred(axiom,2).
:- lock_ec_pred(function,2).
:- lock_ec_pred(action,1).
:- lock_ec_pred(event,1).
:- lock_ec_pred(predicate,1).
:- lock_ec_pred(fluent,1).

:- forall((arg_info_arity(_,F,A),F\==t), lock_ec_pred(F,A)).


correct_ax_args(T,F,A,Args,axiom_head,_Arity,N, PP):-  N is A +1 ,!, append(Args,[T],NewArgs), PP =.. [F|NewArgs].
correct_ax_args(_T,F,A,Args,axiom_head,_Arity,N,PP):- A=N, PP =.. [F|Args].
correct_ax_args(_T,F,1,Args,domain,arginfo,0,PP):- PP =.. [F|Args].
correct_ax_args(_T,F,2,[P,R],domain,arginfo,0,PP):- append_term(P,R,AB),PP =.. [F,AB].
%orrect_ax_args(_4456,function,2,[side1(portal),location],domain,arginfo,0,_4470))

%correct_ax_args(_T,initiates_at,2,[go(_1694),at(_1694)],axiom_head,3,v(event,fluent,time),_1684)
%failed_must(correct_ax_args(_T,initiates_at,2,[go(_1694),at(_1694)],axiom_head,3,v(event,fluent,time),_1684))


skipped_ec_file(File):- is_ftVar(File),fail.

:- export_transparent(hook_ec_axioms/2).

:- (prolog:(import(hook_ec_axioms/2))).

hook_ec_axioms(What, File):- is_ftVar(File), !, current_input(Input), hook_ec_axioms(What, Input).
hook_ec_axioms(What, file(_File,AbsFile)):- !, hook_ec_axioms(What, file(AbsFile)).
hook_ec_axioms(What, file(AbsFile)):- !, hook_ec_axioms(What, AbsFile).
hook_ec_axioms(What, File):- fail, 
    prolog_load_context(module, M),
    dmsg(hook_ec_axioms(M, What, File)),fail.
hook_ec_axioms(_What, _File):- !.
/*
hook_ec_axioms(_What, File):- atom(File), exists_file(File),
    forall(((clause(axiom(_, _), _Body, Ref), File==S)),
       erase(Ref)),!.
*/
:- export(falling_edges/6).

falling_edges(V2,Stem_plus_, T_minus_1_Start, [Haps|List], Edges, Out):-
  make_falling_edges(V2,Stem_plus_, T_minus_1_Start, [], [Haps|List], Edges, Out).
  

make_falling_edges(V2,Stem_plus_, T_minus_1_Start, SoFar, [Haps|List], Edges, Out):-
  make_falling_edges(V2,Stem_plus_, T_minus_1_Start, SoFar, Haps, Edges1, Out1),
  make_falling_edges(V2,Stem_plus_, T_minus_1_Start, Out1, List, Edges2, Out),
  append(Edges1,Edges2,Edges).

make_falling_edges(V2,Stem_plus_, T_minus_1_Start, SoFar, happens_at(Event,Time), Edges, Out):-
  nop(vars(V2,Stem_plus_, T_minus_1_Start, SoFar, Edges, Out)),
  Edges = [/**/holds(has_occured(Event),Time)],
  Out = SoFar,!.
make_falling_edges(V2,Stem_plus_, T_minus_1_Start, SoFar, Was, Edges, Out):- 
  nop(vars(V2,Stem_plus_, T_minus_1_Start, SoFar, Was, Edges, Out)),
  Edges = [Was], Out = SoFar,!.


%:- use_module(library(ec_planner/code_icl/'icl_int.tex')).
%:- reexport(library(ec_planner/code_icl/'icl_int.tex')).

:- fixup_exports.

:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).
:- export_transparent(user:message_hook/3).
user:message_hook(load_file(start(_Level, File)),_,_):- hook_ec_axioms(load,File),fail.
user:message_hook(include_file(start(_Level, File)),_,_):- hook_ec_axioms(include,File),fail.
:- multifile(prolog:make_hook/2).
:- export_transparent(prolog:make_hook/2).
prolog:make_hook(before, Files):-  must_maplist(hook_ec_axioms(make(before)),Files), fail.

:- multifile prolog:message//1.
prolog:message(welcome) -->  {hook_ec_axioms(welcome, welcome),fail}.



:- multifile(user:term_expansion/4).
:- dynamic(user:term_expansion/4).
:- export_transparent(user:term_expansion/4).
:- user:import(ec_loader:needs_proccess/3).
:- user:import(ec_loader:process_ec/2).
:- set_prolog_flag(ec_loader,false).
user:term_expansion(In,P,Out,PO):-  \+ into_lps,
  notrace((nonvar(P),compound(In), In \= (:- _), 
  ( \+ current_prolog_flag(ec_loader,false) ),
  set_prolog_flag(ec_loader,true),
  source_location(File,_))), 
  notrace(needs_proccess(File, In, Type)),PO=P,
  (Out = ( :- locally(current_prolog_flag(ec_loader,false),call(Type, In) ))).

  


% :- dynamic(axiom/2).
/*
axiom(initiates_at(wake_up(X), awake(X), _T), []).
axiom(initiates_at(open(_, Y), opened(Y), _T), []).
axiom(terminates_at(fall_asleep(X), awake(X), _T), []). 
axiom(initially(/**/not(awake(N))), [N=nathan]). 
axiom(initially(/**/not(opened(cont1))), []). 


%:- dynamic(event/1).
event(wake_up(_X)).
event(fall_asleep(_X)).
event(open(_X, _Y)).
*/
%:- dynamic(predicate/1).
%predicate(dummy).
% :- ec:abdemo([/**/holds(awake(nathan), t)], R), writeq(R).
/*
  R = [[happens_at(wake_up(nathan), t1, t1)], [before(t1, t)]]

                                            abdemo([/**/holds(awake(nathan), t), /**/holds(opened(foo), t)], R)
*/

%:- ec:abdemo([/**/holds(awake(nathan), t), before(t, t2), /**/holds(/**/not(awake(nathan)), t2)], R),writeq(R).


