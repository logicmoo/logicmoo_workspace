/*   
%  LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles
%
%   File   : pfccompile.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%   Purpose: compile system file for Pfc


*/
:- if(('$current_source_module'(SM),'context_module'(M),'$current_typein_module'(CM),asserta(baseKB:'using_pfc'(M,CM,SM,pfc_lib)))).
:- endif.

:- if((prolog_load_context(source,File),prolog_load_context(file,File))).
:- module(pfc_lib,[]).
:- endif.
%:- set_prolog_flag(gc,false).
:- set_prolog_flag(pfc_version,2.0).
:- set_prolog_flag(retry_undefined, kb_shared).

kb_global_w(M:F/A):- 
   M:multifile(M:F/A),
   M:module_transparent(M:F/A),
   M:dynamic(M:F/A),
   M:export(M:F/A),
   do_import(system,M,F,A),
   do_import(user,M,F,A),
   do_import(pfc_lib,M,F,A),
   do_import(header_sane,M,F,A),
   M:kb_global(M:F/A),
   system:import(M:F/A).


:- user:use_module(library(file_scope)).
:- set_prolog_flag_until_eof(access_level,system).

:- user:use_module(library(attvar_reader)).
:- user:use_module(library(logimcoo/each_call)).
:- user:use_module(library(must_trace)).
:- user:use_module(library(virtualize_source)).
:- user:use_module(library(hook_hybrid)).
:- user:use_module(library(no_repeats)).
:- user:use_module(library(logicmoo_util_strings)).
:- user:use_module(library(loop_check)).
:- user:use_module(library(attvar_serializer)).

:- dynamic(rdf_rewrite:(~)/1).
:- kb_global_w(rdf_rewrite:arity/2).
:- kb_global_w(baseKB:genlMt/2).
:- kb_global_w(baseKB:mpred_prop/4).
:- kb_global_w(baseKB:mtHybrid/1).
:- kb_global_w(baseKB:mtProlog/1).
:- kb_global_w(baseKB:tCol/1).
:- kb_global_w(baseKB:mpred_database_term/3).
:- kb_global_w(baseKB:mtNoPrologCode/1).
:- kb_global_w(baseKB:ftText/1).
:- kb_global_w(baseKB:mtNotInherits/1).
:- kb_global_w(baseKB:mtInherits/1).
:- kb_global_w(baseKB:rtArgsVerbatum/1).





:- kb_shared(baseKB:never_assert_u/1).
:- kb_shared(baseKB:never_assert_u/2).
:- kb_shared(baseKB:never_retract_u/1).
:- kb_shared(baseKB:never_retract_u/2).
:- kb_shared(baseKB:mpred_prop/4).
:- kb_shared(baseKB:do_and_undo/2).
:- kb_shared(baseKB:spft/4).
:- kb_shared(baseKB:bkct/2).
:- kb_shared(baseKB:hs/1).
:- kb_shared(baseKB:negt/3).
:- kb_shared(baseKB:pk/3).
:- kb_shared(baseKB:pozt/2).
:- kb_shared(baseKB:que/2).
:- kb_shared(baseKB:pm/1).
:- kb_shared(baseKB:spft/4).
:- kb_shared(baseKB:tms/1).



%:- listing(arity/2).
%:- listing(baseKB:_).

:- set_prolog_flag_until_eof(debug,true).

:- if(\+ current_prolog_flag(lm_pfc_lean,_)).
:- set_prolog_flag(lm_pfc_lean,true).
:- endif.


kb_shared_base(M:FA):-!,kb_shared(M:FA).
kb_shared_base(FA):-kb_local(baseKB:FA).
kb_local_base(M:FA):-!,kb_local(M:FA).
kb_local_base(FA):-kb_local(baseKB:FA).
kb_global_base(M:FA):-!,kb_global(M:FA).
kb_global_base(FA):- kb_local(baseKB:FA).



% :- kb_global_base(baseKB:genlMt/2).

:- kb_shared(baseKB:mpred_prop/4).

:- baseKB:forall(between(1,11,A),kb_local(t/A)).
:- baseKB:forall(between(5,7,A),kb_local(mpred_f/A)).

% :- kb_shared_base(baseKB:admittedArgument/3).
%:- set_prolog_flag(runtime_speed,0). % 0 = dont care
:- set_prolog_flag(runtime_speed, 1). % 1 = default
:- set_prolog_flag(runtime_debug, 1). % 2 = important but dont sacrifice other features for it
:- set_prolog_flag(runtime_safety, 3).  % 3 = very important
:- set_prolog_flag(unsafe_speedups, false).
:- set_prolog_flag(pfc_booted,false).


:- use_module(library(prolog_pack)).
pfc_rescan_autoload_pack_packages_part_1:- dmsg("SCAN AUTOLOADING PACKAGES..."),
 forall('$pack':pack(Pack, _),
  forall(((pack_property(Pack, directory(PackDir)),prolog_pack:pack_info_term(PackDir,autoload(true)))),
  (access_file(PackDir,write) -> prolog_pack:post_install_autoload(PackDir, [autoload(true)]) ; dmsg(cannot_access_file(PackDir,write))))),
 dmsg(".. SCAN AUTOLOADING COMPLETE"),!.
:- current_prolog_flag(lm_no_autoload,true) -> true; pfc_rescan_autoload_pack_packages_part_1.


:- meta_predicate pack_autoload_packages(0).
pack_autoload_packages(NeedExistingIndex):- 
 forall(user:expand_file_search_path(library(''),Dir),
  ignore(( (\+ NeedExistingIndex ; absolute_file_name('INDEX',_Absolute,[relative_to(Dir),access(read),file_type(prolog),file_errors(fail)]))->
   maybe_index_autoload_dir(Dir)))),
 reload_library_index.


maybe_index_autoload_dir(PackDir):- \+ access_file(PackDir,write),!,dmsg(cannot_write_autoload_dir(PackDir)).
maybe_index_autoload_dir(PackDir):- user:library_directory(PackDir), make_library_index(PackDir, ['*.pl']),dmsg(update_library_index(PackDir)).
maybe_index_autoload_dir(PackDir):- fail,
  prolog_pack:pack_info_term(PackDir,autoload(true)),
  prolog_pack:post_install_autoload(PackDir, [autoload(true)]) ,
  dmsg(post_install_autoload(PackDir,write)).  
maybe_index_autoload_dir(PackDir):- asserta(user:library_directory(PackDir)), make_library_index(PackDir, ['*.pl']), dmsg(created_library_index_for(PackDir)).

pfc_rescan_autoload_pack_packages_part_2 :- pack_autoload_packages(true).

:- current_prolog_flag(lm_no_autoload,true) -> true; pfc_rescan_autoload_pack_packages_part_2.



input_from_file:- prolog_load_context(stream,Stream),current_input(Stream).

:- module_transparent(intern_predicate/1).
:- module_transparent(intern_predicate/2).
intern_predicate(MFA):- '$current_typein_module'(To),intern_predicate(To,MFA).
intern_predicate(To,F/A):- !, '$current_source_module'(M),intern_predicate(To,M:F/A).
intern_predicate(To,From:F/A):-!,
  From:module_transparent(From:F/A),
  From:export(From:F/A),To:export(From:F/A),To:export(From:F/A),
  From:compile_predicates([F/A]),system:lock_predicate(From:F/A),
  export(From:F/A),export(From:F/A),
  pfc:export(From:F/A),pfc:export(From:F/A),
  user:export(From:F/A),user:export(From:F/A),
  baseKB:export(From:F/A),baseKB:export(From:F/A),
  system:export(From:F/A),system:export(From:F/A),!.

scan_missed_source:-!.
scan_missed_source:-
  prolog_load_context(file,File),scan_missed_source(File),
  prolog_load_context(source,SFile),!,
  (SFile==File-> true; scan_missed_source(SFile)).

scan_missed_source(SFile):-prolog_load_context(module,M),
   forall(source_file(Pred,SFile),scan_missed_source(M,Pred,SFile)).

scan_missed_source(M,Pred,SFile):- \+ M:clause(Pred,_,_),!,nop(dmsg(scan_missed_source(M,Pred,SFile))).
scan_missed_source(M,Pred,SFile):- doall((M:clause(Pred,_,Ref),
  (clause_property(Ref,file(SFile)) -> visit_pfc_file_ref(M,Ref) ; visit_pfc_non_file_ref(M,Ref)))).

visit_pfc_file_ref(M,Ref):- system:clause(H,B,Ref),dmsg(visit_pfc_file_ref(M,H,B)).
visit_pfc_non_file_ref(M,Ref):- system:clause(H,B,Ref),dmsg(visit_pfc_non_file_ref(M,H,B)).



:- intern_predicate(system,intern_predicate/1).

:- intern_predicate(system,intern_predicate/2).

'?='(ConsqIn):- fully_expand(ConsqIn,Consq),call_u(Consq),forall(mpred_why(MZ,Consq,Ante),wdmsg(Ante)).
'?=>'(AnteIn):- fully_expand(AnteIn,Ante),call_u(Ante),forall(mpred_why(MZ,Consq,Ante),wdmsg(Consq)).

:- lock_predicate(pfc:'?='/1).
:- lock_predicate(pfc:'?=>'/1).

:- thread_local(t_l:disable_px).

:- include('pfc2.0/mpred_header.pi').

/*
:- nop(kb_shared((
   bkct/2, %basePFC
   hs/1, %basePFC
   negt/3, %basePFC
   pk/3, %basePFC
   pozt/2, %basePFC
   que/1, %basePFC
   pm/1, %basePFC
   spft/4, %basePFC
   tms/1 %basePFC
   ))).
:- nop(kb_shared( ('~') /1)).
*/


:- if( \+ current_predicate(each_call_cleanup/3)).
:- user:use_module(library(each_call_cleanup)).
:- endif.

/*
% Make YALL require ">>" syntax (the problem was it autoloads when its sees PFC code containing "/" and gripes all the time)

disable_yall:- multifile(yall:lambda_functor/1),
   dynamic(yall:lambda_functor/1),
   with_no_mpred_expansions(use_module(yall:library(yall),[])),
   retractall(yall:lambda_functor('/')).

:- disable_yall.
*/

%:- set_prolog_flag_until_eof(access_level,system).

/*
% baseKB:startup_option(datalog,sanity). %  Run datalog sanity tests while starting
% baseKB:startup_option(clif,sanity). %  Run datalog sanity tests while starting
:- set_prolog_flag(fileerrors,false).
:- set_prolog_flag(gc,false).
:- set_prolog_flag(gc,true).
:- set_prolog_flag(optimise,false).
:- set_prolog_flag(last_call_optimisation,false).
:- set_prolog_flag(debug,true).
:- debug.
*/
%:- guitracer.
%:- set_prolog_flag(access_level,system).

% :- set_prolog_flag(logicmoo_autoload,false).
% :- set_prolog_flag(logicmoo_autoload,true).

% must be xref-ing or logicmoo_autoload or used as include file
:- set_prolog_flag(logicmoo_include,lmbase:skip_module_decl).
% lmbase:skip_module_decl:- source_location(F,L),wdmsg(lmbase:skip_module_decl(F:L)),!,fail.
lmbase:skip_module_decl:- prolog_load_context(file,F), prolog_load_context(source,S),S\=F,!.
lmbase:skip_module_decl:-!,fail.
lmbase:skip_module_decl:-
   (current_prolog_flag(xref,true)-> false ;
    (current_prolog_flag(logicmoo_autoload,true)-> false ;
      ((prolog_load_context(file,F),  prolog_load_context(source,F))
             -> throw(error(format(":- include(~w).",[F]),reexport(F))) ; true))). 

%%% TODO one day :- set_prolog_flag(logicmoo_include,fail).


baseKB:mpred_skipped_module(eggdrop).
:- forall(current_module(CM),assert(baseKB:mpred_skipped_module(CM))).
:- retractall(baseKB:mpred_skipped_module(pfc_lib)).

% ================================================
% DBASE_T System
% ================================================    

:- multifile(baseKB:safe_wrap/4).
:- dynamic(baseKB:safe_wrap/4).

:- if((current_prolog_flag(runtime_debug,D),D>1)).
:- dmsg("Ensuring PFC Loaded").
:- endif.

:- use_module(library(subclause_expansion)).
%:- ensure_loaded(library('pfc2.0/mpred_core.pl')).
%:- system:reexport(library('pfc2.0/mpred_at_box.pl')).

:- user:use_module(library('file_scope')).
% :- set_how_virtualize_file(bodies).
:- module_transparent(baseKB:prologBuiltin/1).
:- multifile baseKB:prologBuiltin/1.
:- discontiguous baseKB:prologBuiltin/1.
:- dynamic baseKB:prologBuiltin/1.


%:- break.
%system:ensure_abox(M):- wdmsg(was(ensure_abox(M))).

:- multifile(lmcache:mpred_directive_value/3).
:- volatile(lmcache:mpred_directive_value/3).
:- dynamic(lmcache:mpred_directive_value/3).
/*
:- multifile(mpred_database_term/3).
:- volatile(mpred_database_term/3).
:- dynamic(mpred_database_term/3).
*/

:- reexport(('../pfc2.2/src/pfcsyntax.pl')).
:- reexport(('../pfc2.2/src/pfccore')).
:- reexport(('../pfc2.2/src/pfcsupport')).
:- reexport(('../pfc2.2/src/pfcdb')).
:- reexport(('../pfc2.2/src/pfcdebug')).
:- reexport(('../pfc2.2/src/pfcjust')).
:- reexport(('../pfc2.2/src/pfcwhy')).
:- reexport(library(pfc_ex)).

%:- autoload([verbose(false)]).



%baseKB:sanity_check:- findall(U,(current_module(U),default_module(U,baseKB)),L),must(L==[baseKB]).
baseKB:sanity_check:- doall((current_module(M),setof(U,(current_module(U),default_module(U,M),U\==M),L),
     wdmsg(imports_eache :- (L,[sees(M)])))).
baseKB:sanity_check:- doall((current_module(M),setof(U,(current_module(U),default_module(M,U),U\==M),L),wdmsg(imports(M):-L))).
baseKB:sanity_check:- doall((baseKB:mtProlog(M),
    setof(U,(current_module(U),default_module(M,U),U\==M),L),wdmsg(imports(M):-L))).


%:- rtrace((mpred_at_box:defaultAssertMt(G40331),rtrace(set_prolog_flag(G40331:unknown,warning)))).
%:- dbreak.
:- must(set_prolog_flag(abox:unknown,error)).
%:- locally_tl(side_effect_ok,doall(call_no_cuts(module_local_init(abox,baseKB)))).
% :- forall(baseKB:sanity_check,true).


:- if( \+ prolog_load_context(reload,true)).
:-module_transparent(hook_database:ain/1).
:-module_transparent(hook_database:aina/1).
:-module_transparent(hook_database:ainz/1).
:-multifile(hook_database:ain/1).
:-multifile(hook_database:aina/1).
:-multifile(hook_database:ainz/1).
:-dynamic(hook_database:ain/1).
:-dynamic(hook_database:aina/1).
:-dynamic(hook_database:ainz/1).
:-module_transparent(mpred_core:mpred_ain/1).
:-module_transparent(mpred_core:mpred_aina/1).
:-module_transparent(mpred_core:mpred_ainz/1).
:-multifile(mpred_core:mpred_ain/1).
:-multifile(mpred_core:mpred_aina/1).
:-multifile(mpred_core:mpred_ainz/1).
:-dynamic(mpred_core:mpred_ain/1).
:-dynamic(mpred_core:mpred_aina/1).
:-dynamic(mpred_core:mpred_ainz/1).
:-hook_database:export(mpred_core:mpred_ain/1).
:-hook_database:export(mpred_core:mpred_aina/1).
:-hook_database:export(mpred_core:mpred_ainz/1).

:-asserta_new((hook_database:ainz(G):- !, mpred_ainz(G))).
:-asserta_new((hook_database:ain(M:G):- !, M:mpred_ain(M:G))).
:-asserta_new((hook_database:aina(G):- !, mpred_aina(G))).
:- endif.

% Load boot base file
user:lmbf:- 
 locally( set_prolog_flag(mpred_te,true),
  locally( set_prolog_flag(subclause_expansion,true),
   locally(set_prolog_flag(pfc_booted,false),
     with_umt(baseKB,
  prolog_statistics:time((reexport(baseKB:library(logicmoo/pfc/'system_base.pfc')))))))),
  set_prolog_flag(pfc_booted,true).

/*
:- set_prolog_flag(unknown,error).
:- set_prolog_flag(user:unknown,error).
:- set_prolog_flag(lmcode:unknown,error).
:- set_prolog_flag(baseKB:unknown,error).
*/
:- sanity(current_prolog_flag(unknown,error)).
:- sanity(current_prolog_flag(user:unknown,error)).

in_goal_expansion:- prolog_current_frame(F),
   prolog_frame_attribute(F,parent_goal,expand_goal(_,_,_,_)).

in_clause_expand(I):-  nb_current('$goal_term',Was),same_terms(I, Was),!,fail.
in_clause_expand(I):-  
   (nb_current_or_nil('$source_term',TermWas),\+ same_terms(TermWas, I)),
   (nb_current_or_nil('$term',STermWas),\+ same_terms(STermWas, I)),!,
   fail.
in_clause_expand(_).


% SHOULD NOT NEED THIS 
%          maybe_should_rename(M,O):-current_prolog_flag(do_renames,term_expansion),if_defined(do_renames(M,O)),!.
maybe_should_rename(O,O).


% check_how_virtualize_file(heads,File):- prolog_load_context(file,File),t_l:current_lang(pfc).
% check_how_virtualize_file(heads,File):- prolog_load_context(source,File),t_l:current_lang(pfc),source_location(SFile,_W), \+ check_how_virtualize_file(false,SFile),!.

% file late late joiners
:- if( \+ prolog_load_context(reload,true)).
:- source_location(File, _)-> during_boot(((set_how_virtualize_file(false,File)))).
:- doall((module_property(M,file(File)),module_property(M,class(CT)),memberchk(CT,[library,system]),(set_how_virtualize_file(false,File)))).
%:- doall((source_file(File),(set_how_virtualize_file(false,File)))).
%base_kb_dynamic(F,A):- ain(mpred_prop(M,F,A,prologHybrid)),kb_shared(F/A).
%:- doall((virtualize_ereq(F,A),base_kb_dynamic(F,A))).
:- endif.

:- discontiguous(baseKB:'$pldoc'/4).


in_dialect_pfc:- is_pfc_file. % \+ current_prolog_flag(dialect_pfc,cwc),!.

%is_pfc_module(SM):- clause_b(using_pfc(SM,_, SM, pfc_toplevel)),!.
%is_pfc_module(SM):- clause_b(using_pfc(SM,_, SM, pfc_mod)),!,baseKB:mtCanAssert(SM).
is_pfc_module(SM):- clause_b(mtHybrid(SM)).

% First checks to confirm there is nothing inhibiting
must_not_be_pfc_file:- is_pfc_file0, rtrace(is_pfc_file0),trace,!,fail.
must_not_be_pfc_file:- !.

is_pfc_file:- current_prolog_flag(never_pfc,true),!,must_not_be_pfc_file,!,fail.
is_pfc_file:- quietly(is_pfc_file0),!.

is_pfc_file0:- source_location(File,_W),!,is_pfc_file(File),!.
is_pfc_file0:- prolog_load_context(module, M),is_pfc_module(M),!.
%is_pfc_file0:- source_context_module(M),is_pfc_module(M).

:- meta_predicate is_pfc_file(:).
is_pfc_file(M:File):- is_pfc_file(M,File).
is_pfc_file(_,File):- atom_concat(_,'.pfc.pl',File);atom_concat(_,'.clif',File);atom_concat(_,'.plmoo',File);atom_concat(_,'.pfc',File),!.
is_pfc_file(_,File):- call(call,lmcache:mpred_directive_value(File, language, Lang)),!,(Lang==pfc;Lang==clif;Lang==fwd).
is_pfc_file(_,File):- check_how_virtualize_file(false,File),!,fail.
is_pfc_file(_,File):- check_how_virtualize_file(heads,File),!.
is_pfc_file(M,Other):- prolog_load_context(source, File),Other\==File,!,is_pfc_file(M,File).
%is_pfc_file(M,_):- prolog_load_context(module, SM), SM\==M,!, is_pfc_module(SM).
%is_pfc_file(M,_):- is_pfc_module(M).

:- fixup_exports.

sub_atom(F,C):- sub_atom(F,_,_,_,C).

only_expand(':-'(I), ':-'(M)):- !,in_dialect_pfc,fully_expand('==>'(I),M),!.
only_expand(I,OO):- fail, quietly(must_pfc(I,M)),  
  % current_why(S),!,
  S= mfl4(VarNameZ,Module, File, Line),source_location(File,Line),prolog_load_context(module,Module),
  conjuncts_to_list(M,O), !, %  [I]\=@=O,
  make_load_list(O,S,OO).

make_load_list([C|O],S,[baseKB:spft(MZ,C,S,ax), :- mpred_enqueue_w_mode(S,direct,C)|OO]):- clause_asserted(C),!, make_load_list(O,S,OO).
make_load_list([C|O],S,[C, baseKB:spft(MZ,C,S,ax), :- mpred_enqueue_w_mode(S,direct,C)|OO]):-  is_loadin(C),!,make_load_list(O,S,OO).
make_load_list(_,_,[]):-!.  

is_loadin(C):- strip_module(C,M,CC),is_loadin(M,CC).
is_loadin(_,CC):- must_pfc_p(CC),!.
is_loadin(_,(_:-_)):-!.
is_loadin(M,CC):- functor(CC,F,A),show_call(kb_local(M:F/A)),break.


must_pfc(IM,_):- is_never_pfc(IM),!,fail.
%must_pfc(IM,'==>'(IM)):- (in_dialect_pfc;must_pfc_p(IM)),!.
must_pfc(IM,SM:'==>'(IM)):- (in_dialect_pfc;must_pfc_p(IM)),!,source_module(SM),!.

must_pfc_exp(IM,MO):- in_dialect_pfc,fully_expand(IM,MO),!.
must_pfc_exp(IM,MO):- must_pfc_p(IM),!,fully_expand(IM,MO),!.

must_pfc_p('-->'(_,_)):-!,fail.
must_pfc_p(':-'(_,(CWC,_))):- atom(CWC),arg(_,v(bwc,fwc,awc,zwc),CWC),!.
must_pfc_p(':-'(_,(CWC,_))):- atom(CWC),arg(_,v(cwc),CWC),!,is_pfc_file.
must_pfc_p(':-'(Head,_)):- !, must_pfc_p(Head),!.
must_pfc_p('==>'(_,_)).
must_pfc_p('==>'(_)).
must_pfc_p('<==>'(_,_)).
must_pfc_p('<=='(_,_)).
must_pfc_p('<-'(_,_)).
must_pfc_p('<--'(_,_)).
must_pfc_p('->'(_,_)).
must_pfc_p('~'(_)).
must_pfc_p('--->'(_,_)).
% must_pfc_p('=>'(_,_)).
must_pfc_p(_:P):- !, must_pfc_p(P),!.
must_pfc_p(FAB):-functor(FAB,F,A),must_pfc_fa(F,A),!.

must_pfc_fa(prologHybrid,_).
must_pfc_fa(F,A):- mpred_database_term(F,A,_),!.
must_pfc_fa(F,A):- clause_b(mpred_prop(M,F,A,_)),!, \+ clause_b(mpred_prop(M,F,A,prologBuiltin)).
must_pfc_fa(F,2):- sub_atom(F,'='),(atom_concat(_,'>',F);atom_concat('<',_,F)).


:- module_transparent(base_clause_expansion/2).

% module prefixed clauses for sure should be non pfc?
is_never_pfc(Var):- \+ callable(Var),!.
is_never_pfc(_):- prolog_load_context(file,F),\+ prolog_load_context(source,F),atom_concat(_,'.pl',F),\+ atom_concat(_,'pfc.pl',F).
is_never_pfc(goal_expansion(_,_,_,_)).
is_never_pfc(':-'(_)).
is_never_pfc('?-'(_)).
is_never_pfc('-->'(_,_)):-!.
is_never_pfc(attr_unify_hook(_,_)):-!.

% TODO Maybe find a better spot?  see t/sanity_base/hard_mt_04a.pfc
is_never_pfc(M:C):- \+ is_never_pfc(C), \+ current_module(M),
   is_pfc_file,
   fileAssertMt(CMt),
   CMt:clause_b(mtHybrid(CMt)),
   CMt:ensure_abox(M),
   CMt:ain(genlMt(CMt,M)),!,fail.

is_never_pfc(':-'(C,_)):- !,is_never_pfc(C).
is_never_pfc(M:P):- functor(P,F,A),clause_b(mpred_prop(M,F,A,prologBuiltin)),!.
is_never_pfc(_:C):- is_never_pfc(C).


% base_clause_expansion(Var,Var):- current_prolog_flag(mpred_te,false),!.
base_clause_expansion(Var,Var):-var(Var),!.
base_clause_expansion( :- module(W,List), [:- writetln(module(W,List)), :- set_fileAssertMt(W)]):- is_pfc_file,!.
base_clause_expansion('?=>'(I), ':-'(O)):- !, sanity(nonvar(I)), fully_expand('==>'(I),O),!. % @TODO NOT NEEDED REALY UNLESS DO mpred_expansion:reexport(library('pfc2.0/mpred_expansion.pl')),
base_clause_expansion(:-(I),:-(I)):- !.
base_clause_expansion(IM,':-'(ain(==>(IM)))):- \+ compound(IM),(sub_atom(IM,';');sub_atom(IM,'(')),!.
% NEXT LINE REDUNDANT base_clause_expansion(IM,IM):- \+ callable(IM),!.
base_clause_expansion(NeverPFC, EverPFC):- is_never_pfc(NeverPFC),!,NeverPFC=EverPFC.

% base_clause_expansion(In,Out):- only_expand(In,Out),!.
base_clause_expansion(IN, ':-'(ain(ASSERT))):- must_pfc(IN,ASSERT).
base_clause_expansion(ASSERT, ':-'(ain(ASSERT))):- is_pfc_file.

/*


% Checks if **should** be doing base_expansion or not      
:- module_transparent(base_clause_expansion_fa/4).
base_clause_expansion_fa(_,_,F,A):- clause_b(mpred_prop(M,F,A,prologBuiltin)),!,fail.
base_clause_expansion_fa(I,O,F,A):- (needs_pfc(F,A) -> true ; base_kb_dynamic(F,A)),
  base_clause_expansion('==>'(I),O).

:- module_transparent(needs_pfc/2).
needs_pfc(F,_):- (clause_b(functorIsMacro(F));clause_b(functorDeclares(F))).
needs_pfc(F,A):- base_kb_dynamic(F,A).
needs_pfc(F,A):- clause_b(mpred_prop(M,F,_,prologHybrid)), \+ clause_b(mpred_prop(M,F,A,prologBuiltin)).

maybe_builtin(M : _ :-_):- atom(M),!.
maybe_builtin(M : _ ):- atom(M),!.
maybe_builtin(I) :- nonvar(I),get_unnegated_functor(I,F,A),
   \+ (clause_b(functorIsMacro(F));clause_b(functorDeclares(F));clause_b(mpred_prop(M,F,A,prologHybrid))),
   ain(prologBui sltin(F/A)).

*/

:- sanity((clause(check_how_virtualize_file(false,_),B),compound(B))).

:- if(false).
%:- autoload([verbose(false)]).
:- statistics.
:- endif.

% :- ain(arity(functorDeclares, 1)).
% Load boot base file
%:- dynamic(isa/2).

%is_lm_mod(M):-atom_concat('logicmoo_i_',_,M).
%is_lm_mod(M):-atom_concat('common_logic_',_,M).
%is_lm_mod(M):-atom_concat('mpred_',_,M).
%is_lm_mod(M):-atom_concat('baseK',_,M).
is_lm_mod(M):-atom_concat('mud_',_,M).
make_exported(op(X,Y,Z),:-op(X,Y,Z)).
make_exported(Pred,:-export(Pred)).

term_expansion_UNUSED(:-module(M,List),Pos,ExportList,Pos):- nonvar(Pos),
  ((prolog_load_context(file,File),\+ prolog_load_context(source,File));is_lm_mod(M)),
   maplist(make_exported,List,ExportList).

%:- thread_local t_l:side_effect_ok/0.



:- module_transparent(pfc_clause_expansion/2).
pfc_clause_expansion(I,O):- nonvar(I),I\==end_of_file,
  base_clause_expansion(I,M),!,I\=@=M,
   ((
      maybe_should_rename(M,MO), 
      ignore(( \+ pfc_lib:same_expandsion(I,MO), dmsg(pfc_clause_expansion(I)-->MO))),
      maybe_directive_to_clauses(MO,O),
      ignore(( O\==MO , (dmsg(directive_to_clauses(I)-->O)))))),!.

%maybe_directive_to_clauses(:- ain(A),Clauses):- loader_side_effect_capture_only(ain(A),Clauses).
%maybe_directive_to_clauses(:- ain(A),Clauses):- loader_side_effect_capture_only(ain(A),Clauses).
maybe_directive_to_clauses(O,O):-!.

same_expandsion(I,O):-var(I),!,I==O.
same_expandsion(I,O):-var(O),!,I==O.
same_expandsion(_:I,MO):-!,same_expandsion(I,MO).
same_expandsion(MO,_:I):-!,same_expandsion(I,MO).
same_expandsion('==>'(I),MO):-!,same_expandsion(I,MO).
same_expandsion(I,'==>'(MO)):-!,same_expandsion(I,MO).
same_expandsion(I,[MO|_]):-!,same_expandsion(I,MO).
same_expandsion(I, (:-ain(MO))):-!,same_expandsion(I,MO).
same_expandsion(I, (:-mpred_ain(MO))):-!,same_expandsion(I,MO).
same_expandsion(I,O):-I==O.

%   File   : pfc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: consult system file for ensure

mpred_Version(2.2).

mpred_File('pfcsyntax').	% operator declarations.
mpred_File('pfccore').	% core of Pfc.
mpred_File('pfcsupport').	% support maintenance
mpred_File('pfcdb').	% predicates to manipulate database.
mpred_File('pfcdebug').	% debugging aids (e.g. tracing).
mpred_File('pfcjust').	% predicates to manipulate justifications.
mpred_File('pfcwhy').	% interactive exploration of justifications.

pfcLoad :- mpred_File(F), ensure_loaded(F), fail.
pfcLoad.

pfcFcompile :- mpred_File(F), fcompile(F), fail.
pfcFcompile.

:- pfcLoad.

%   File   : pfccompile.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%   Purpose: compile system file for Pfc


:- compile(pfcsyntax).
:- compile(pfccore).
:- compile(pfcdb).
:- compile(pfcjust).
:- compile(pfcwhy).
:- compile(pfcdebug).
%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

:- module(pfcsyntax, [
    op(500,fx,'~'),
    op(1050,xfx,('==>')),
    op(1050,xfx,'<==>'),
    op(1050,xfx,('==>')),
    op(1050,xfx,'==>'),
    op(1050,xfx,('<-')),
    op(1050,xfx,('<-')),
    op(1100,fx,('==>')),
    op(1150,xfx,('::::'))]).
:- use_module(library(pfc_pack_xform)).

:- op(500,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).

/*
:- multifile('term_expansion'/2).

term_expansion((P==>Q),(:- add((P==>Q)))).
%term_expansion((P==>Q),(:- add(('<-'(Q,P))))).  % speed-up attempt
term_expansion(('<-'(P,Q)),(:- add(('<-'(P,Q))))).
term_expansion((P<==>Q),(:- add((P<==>Q)))).
term_expansion((RuleName :::: Rule),(:- add((RuleName :::: Rule)))).
term_expansion((==>P),(:- add(P))).
*/


%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.


:- module(pfccore, []).

:- use_module(library(pfc_pack_xform)).

:- use_module(library(lists)).


:- dynamic ('<-')/2.
:- dynamic ('==>')/2.
:- dynamic ('::::')/2.
%:- dynamic '<==>'/2.
:- dynamic 'pt'/2.
:- dynamic 'nt'/3.
:- dynamic 'bt'/2.
:- dynamic fcUndoMethod/2.
:- dynamic fcAction/2.
:- dynamic fcTmsMode/1.
:- dynamic pfcQueue/1.
:- dynamic pfcDatabase/1.
:- dynamic pfcHaltSignal/1.
:- dynamic pfcDebugging/0.
:- dynamic pfcSelect/1.
:- dynamic pfcSearch/1.
:- dynamic pfcCurrentDb/1.

%%% initialization of global assertons 

%% pfcDefault/1 initialized a global assertion.
%%  pfcDefault(P,Q) - if there is any fact unifying with P, then do 
%%  nothing, else assert Q.

:- export(pfcDefault/2).
:- module_transparent(pfcDefault/2).
pfcDefault(GeneralTerm,Default) :-
  umt((clause(GeneralTerm,true) -> true ; assert(Default))).

%% fcTmsMode is one of {none,local,cycles} and controles the tms alg.
:- initialization(baseKB:pfcDefault(fcTmsMode(_), fcTmsMode(cycles))).

% Pfc Search strategy. pfcSearch(X) where X is one of {direct,depth,breadth}
:- initialization(baseKB:pfcDefault(pfcSearch(_), pfcSearch(direct))).


% 

%% add/2 and post/2 are the main ways to assert new clauses into the
%% database and have forward reasoning done.

%% add(P,S) asserts P into the dataBase with support from S.

add(M:P) :- b_setval(defaultQueryMt,M),b_setval(defaultAssertMt,M),!,
  pfcCurrentUserSupport(UU),
  M:add(P,UU).
add(P) :-  pfcCurrentUserSupport(UU),add(P,UU).

add((==>(P)),S) :- add(P,S).

add(P,S) :- 
  post(P,S),
  pfcRun.

%add(_,_).
%add(P,S) :- pfcWarn("add(~p,~p) failed",[P,S]).


% post(+Ps,+S) tries to add a fact or set of fact to the database.  For
% each fact (or the singelton) post1 is called. It always succeeds.

post([H|T],S) :-
  !,
  post1(H,S),
  post(T,S).
post([],_) :- !.
post(P,S) :- post1(P,S).


% post1(+P,+S) tries to add a fact to the database, and, if it succeeded,
% adds an entry to the pfc queue for subsequent forward chaining.
% It always succeeds.

post1(M:'==>'(P),S) :-!, post1_(M:P,S).
post1('==>'(P),S) :-!, post1_(P,S).
post1(P,S) :- post1_(P,S),!.

post1_(PIn,S) :- 
  defaultAssertMt(M),
  %% db 
  (pfcAddDbToHead(PIn,P) -> true ; P = PIn),
  % old vesrion 
  nop(pfcRemoveOldVersion(P)),
  pfcAddSupport(P,S),
  pfcUnique(P),
  assert(P),
  pfcTraceAdd(P,S),
  !,
  pfcEnqueue(M,P,S),
  !.

post1_(_,_).
%%post1_(P,S) :-  pfcWarn("add(~p,~p) failed",[P,S]).

%%  pfcAddDbToHead(+P,-NewP) is semidet.
% talkes a fact P or a conditioned fact
% (P:-C) and adds the Db context.
%

pfcAddDbToHead(P,NewP) :-
  umt(pfcCurrentDb(Db)),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   otherwise      -> NewP = (P :- Db)).


%% pfcUnique(X) is det.
% 
% is true if there is no assertion X in the prolog db.
%

pfcUnique((Head:-Tail)) :- 
  !, 
  \+ clause(Head,Tail).
pfcUnique(P) :-
  !,
  \+ clause(P,true).



%% pfcEnqueue(P,Q) is det.
% 
% Enqueu according to settings
%
/*
pfcEnqueue(P,S):- strip_module(P,M,PP),
 % defaultAssertMt(M),
  pfcEnqueue(M,PP,S).
*/
get_pfcSearch(Mode):- umt(pfcSearch(Mode0)),!,Mode0=Mode.
get_pfcSearch(direct).
pfcEnqueue(M,P,S) :-
  must(get_pfcSearch(Mode)) 
    -> (Mode=direct  -> fc(P) ;
	Mode=depth   -> pfcAsserta(pfcQueue(M,P),S) ;
	Mode=breadth -> pfcAssert(pfcQueue(M,P),S) ;
	nop(otherwise)         -> pfcWarn("Unrecognized pfcSearch mode: ~p", Mode))
     ; pfcWarn("No pfcSearch mode").


%% pfcRemoveOldVersion(+Rule) is det.
%
% if there is a rule of the form Identifier ::: Rule then delete it.

pfcRemoveOldVersion((Identifier::::Body)) :-
  % this should never happen.
  (var(Identifier)
  ->
  pfcWarn("variable used as an  rule name in ~p :::: ~p",
          [Identifier,Body]);
  umt((pfcRemoveOldVersion0(Identifier::::Body)))).

  
pfcRemoveOldVersion0((Identifier::::Body)) :-
  nonvar(Identifier),
  clause((Identifier::::OldBody),_),
  \+(Body=OldBody),
  pfc_withdraw((Identifier::::OldBody)),
  !.
pfcRemoveOldVersion0(_).



% 

% pfcRun compute the deductive closure of the current database. 
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the pfcQueue mechanism.

pfcRun :-
  (\+ get_pfcSearch(direct)),
  pfcStep,
  pfcRun.
pfcRun.


% pfcStep removes one entry from the pfcQueue and reasons from it.


pfcStep :-  
  % if pfcHaltSignal is true, reset it and fail, thereby stopping inferencing.
  pfcRetract(pfcHaltSignal(Msg)),
  pfc_trace_msg(removing(pfcHaltSignal(Msg))),
  !, 
  fail.

pfcStep :-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P),
  ignore(fc(P)),
  !.

get_next_fact(P) :-
  %identifies the nect fact to fc from and removes it from the queue.
  select_next_fact(P),
  remove_selection(P).

remove_selection(P) :- 
  umt((
    pfcRetract(pfcQueue(MM,P)),sanity((strip_module(P,M,PP),MM=M),
  must(pfc_remove_supports_quietly(pfcQueue(MM,PP)))))),
  !.
remove_selection(P) :-
  brake(format("~Npfc:get_next_fact - selected fact not on Queue: ~p",
               [P])).


% select_next_fact(P) identifies the next fact to reason from.  
% It tries the user defined predicate first and, failing that, 
%  the default mechanism.

select_next_fact(M:P) :- 
  umt((pfcSelect(M,P))),
  !.  
select_next_fact(P) :- 
  defaultpfcSelect(P),
  !.  

% the default selection predicate takes the item at the froint of the queue.
defaultpfcSelect(M:PP) :- umt((pfcQueue(MM,P),sanity((strip_module(P,M,PP),MM=M)))),!.

% pfcHalt stops the forward chaining.
pfcHalt :-  pfcHalt("",[]).

pfcHalt(Format) :- pfcHalt(Format,[]).

pfcHalt(Format,Args) :- 
  format(Format,Args),
  umt((pfcHaltSignal -> 
       pfcWarn("pfcHalt finds pfcHaltSignal already set")
     ; assert(pfcHaltSignal))).


%%
%%
%% predicates for manipulating triggers
%%

pfcAddTrigger(pt(Trigger,Body),Support) :-
  !,
  deterministically_must(pfc_trace_msg('      Adding positive trigger: ','~p~n',
		[pt(Trigger,Body)])),
  deterministically_must(pfcAssert(pt(Trigger,Body),Support)),
  copy_term(pt(Trigger,Body),Tcopy),
  deterministically_must(pfc(Trigger)),
  deterministically_must(fcEvalLHS(Body,(Trigger,Tcopy))),
  fail.


pfcAddTrigger(nt(Trigger,Test,Body),Support) :-
  !,
  pfc_trace_msg('      Adding negative trigger: ','~p~n       test: ~p~n       body: ~p~n',
		[Trigger,Test,Body]),
  copy_term(Trigger,TriggerCopy),
  pfcAssert(nt(TriggerCopy,Test,Body),Support),
  \+ call(Test),
  fcEvalLHS(Body,((\+Trigger),nt(TriggerCopy,Test,Body))).

pfcAddTrigger(bt(Trigger,Body),Support) :-
  !,
  pfcAssert(bt(Trigger,Body),Support),
  pfcBtPtCombine(Trigger,Body,Support).

pfcAddTrigger(X,_Support) :-
  pfcWarn("Unrecognized trigger to pfcAddtrigger: ~p",[X]).


pfcBtPtCombine(Head,Body,Support) :- 
  %% a backward trigger (bt) was just added with head and Body and support Support
  %% find any pt's with unifying heads and add the instantied bt body.
  pfcGetTriggerQuick(pt(Head,_PtBody)),
  fcEvalLHS(Body,Support),
  fail.
pfcBtPtCombine(_,_,_) :- !.

pfcGetTriggerQuick(Trigger) :-  umt(clause(Trigger,true)).
% pfcGetTriggerQuick(Trigger) :-  umt(Trigger).

%%
%%
%% predicates for manipulating action traces.
%%

pfcAddActionTrace(Action,Support) :- 
  % adds an action trace and it's support.
  umt((pfcAddSupport(pfcAction(Action),Support))).

pfcRemActionTrace(pfcAction(A)) :-
  umt((fcUndoMethod(A,M),
  M)),
  !.


%%
%% predicates to remove pfc facts, triggers, action traces, and queue items
%% from the database.
%%

pfcRetract(X) :- 
  %% retract an arbitrary thing.
  pfcType(X,Type),
  pfcRetractType(Type,X),
  !.

pfcRetractType(fact,X) :-   
  %% db 
  pfcAddDbToHead(X,X2)-> retract(X2) ; retract(X).

pfcRetractType(rule,X) :- 
  %% db  
  pfcAddDbToHead(X,X2) ->  retract(X2) ; retract(X).

pfcRetractType(trigger,X) :- 
  retract(X)
    -> unFc(X)
     ; pfcWarn("Trigger not found to retract: ~p",[X]).

pfcRetractType(action,X) :- pfcRemActionTrace(X).
  

%% pfcAdd(X) adds item X to some database

pfcAdd(X) :-
  % what type of X do we have?
  pfcType(X,Type),
  % call the appropriate predicate.
  pfcAddType(Type,X).

pfcAddType(fact,X) :- 
  pfcUnique(X), 
  assert(X),!.
pfcAddType(rule,X) :- 
  pfcUnique(X), 
  assert(X),!.
pfcAddType(trigger,X) :- 
  assert(X).
pfcAddType(action,_Action) :- !.


  

%% pfc_withdraw(P,S) removes support S from P and checks to see if P is still supported.
%% If it is not, then the fact is retreactred from the database and any support
%% relationships it participated in removed.

pfc_withdraw(P) :- 
  pfcCurrentUserSupport(UU),
  % iterate down the list of facts to be pfc_withdraw'ed.
  (is_list(P)->
  pfc_withdraw_list(P,UU);
    % pfc_withdraw/1 is the user's interface - it withdraws user support for P.
  pfc_withdraw(P,UU)).
  
  
pfc_withdraw_list(P) :- 
  pfcCurrentUserSupport(UU),
  pfc_withdraw_list(P,UU).

pfc_withdraw_list([H|T],UU) :-
  % pfc_withdraw each element in the list.
  pfc_withdraw(H,UU),
  pfc_withdraw_list(T,UU).

pfc_withdraw(P,S) :-
  % pfcDebug(format("~Nremoving support ~p from ~p",[S,P])),
  (pfc_trace_msg('    Removing support: ','~p~n',[S]),
     pfc_trace_msg('     Which was: ','~p~n',[P])),
  
  ((pfcRemSupport(P,S)
     -> removeIfUnsupported(P)
      ; pfcWarn("pfc_withdraw/2 Could not find support ~p to remove from fact ~p",
                [S,P]))).

%%
%% pfc_remove2 is like pfc_withdraw, but if P is still in the DB after removing the
%% user's support, it is retracted by more forceful means (e.g. remove).
%%

pfc_remove2(P) :-  freeze(UU,pfcCurrentUserSupport(UU)),
  % pfc_remove2/1 is the user's interface - it withdraws user support for P.
  pfc_remove2(P,UU).

pfc_remove2(P,S) :-
  pfc_withdraw(P,S),
  pfc(P)
     -> remove(P) 
      ; true.

%%
%% remove(+F) retracts fact F from the DB and removes any dependent facts */
%%

remove(F) :- 
  pfcRemoveSupports(F),
  fcUndo(F).


% removes any remaining supports for fact F, complaining as it goes.

pfcRemoveSupports(F) :- 
  pfcRemSupport(F,S),
  pfcWarn("~p was still supported by ~p",[F,S]),
  fail.
pfcRemoveSupports(_).

pfc_remove_supports_quietly(F) :- 
  pfcRemSupport(F,_),
  fail.
pfc_remove_supports_quietly(_).

% fcUndo(X) undoes X.


fcUndo(pfcAction(A)) :-  
  % undo an action by finding a method and successfully executing it.
  !,
  pfcRemActionTrace(pfcAction(A)).

fcUndo(pt(Head,Body)) :-  
  % undo a positive trigger.
  %
  !,
  (retract(pt(Head,Body))
    -> unFc(pt(Head,Body))
     ; pfcWarn("Trigger not found to retract: ~p",[pt(Head,Body)])).

fcUndo(nt(Head,Condition,Body)) :-  
  % undo a negative trigger.
  !,
  (retract(nt(Head,Condition,Body))
    -> unFc(nt(Head,Condition,Body))
     ; pfcWarn("Trigger not found to retract: ~p",[nt(Head,Condition,Body)])).

fcUndo(Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  retract(Fact),
  pfcTraceRem(Fact),
  unFc1(Fact).
  

%% unFc(P) is det.
%
% unFc(P) "un-forward-chains" from fact f.  That is, fact F has just
% been removed from the database, so remove all support relations it
% participates in and check the things that they support to see if they
% should stayu in the database or should also be removed.


unFc(F) :- 
  pfcRetractSupportRelations(F),
  unFc1(F).

unFc1(F) :-
  pfcUnFcCheckTriggers(F),
  % is this really the right place for pfcRun<?
  pfcRun.

%pfcUnFcCheckTriggers(F):- umt((pfcUnFcCheckTriggers(F))).
pfcUnFcCheckTriggers(F) :-
  pfcType(F,fact),
  copy_term(F,Fcopy),
  umt(nt(Fcopy,Condition,Action)),
  (\+ umt(Condition)),
  fcEvalLHS(Action,((\+F),nt(F,Condition,Action))),
  fail.
pfcUnFcCheckTriggers(_).

pfcRetractSupportRelations(Fact) :-
  pfcType(Fact,Type),
  (Type=trigger -> pfcRemSupport(P,(_,Fact))),
  removeIfUnsupported(P),
  fail.
pfcRetractSupportRelations(Fact) :-
  % pfcType(Fact,Type),
  pfcRemSupport(P,(Fact,_)),
  removeIfUnsupported(P),
  fail.
pfcRetractSupportRelations(_).



%% removeIfUnsupported(+P) checks to see if P is supported and removes
%% it from the DB if it is not.

removeIfUnsupported(P) :- 
   fcSupported(P) -> true ;  fcUndo(P).


%% fcSupported(+P) succeeds if P is "supported". What this means
%% depends on the TMS mode selected.

fcSupported(P) :- 
  must(umt(fcTmsMode(Mode));Mode=cycles),
  pfcSupported(Mode,P).

pfcSupported(local,P) :- !, pfcGetSupport(P,_).
pfcSupported(cycles,P) :-  !, wellFounded(P).
pfcSupported(_,_P) :- true.


%%
%% a fact is well founded if it is supported by the user
%% or by a set of facts and a rules, all of which are well founded.
%%

wellFounded(Fact) :- wf(Fact,[]).

wf(F,_) :-
  % supported by user (axiom) or an "absent" fact (assumption).
  (axiom(F) ; assumption(F)),
  !.

wf(F,Descendants) :-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supports(F,Supporters),
  % all of whose members are well founded.
  wflist(Supporters,[F|Descendants]),
  !.

%% wflist(L) simply maps wf over the list.

wflist([],_).
wflist([X|Rest],L) :-
  wf(X,L),
  wflist(Rest,L).



% supports(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [user].

supports(F,[Fact|MoreFacts]) :-
  pfcGetSupport(F,(Fact,Trigger)),
  triggerSupports(Trigger,MoreFacts).

triggerSupports(U,[]) :- pfcCurrentUserSupport((_,U)), !.
triggerSupports(Trigger,[Fact|MoreFacts]) :-
  pfcGetSupport(Trigger,(Fact,AnotherTrigger)),
  triggerSupports(AnotherTrigger,MoreFacts).


%%
%%
%% fc(X) forward chains from a fact or a list of facts X.
%%


fc([H|T]) :- !, fc1(H), fc(T).
fc([]) :- !.
fc(P) :- fc1(P).

% fc1(+P) forward chains for a single fact.

fc1(Fact) :-
  fc_rule_check(Fact),
  copy_term(Fact,F),
  % check positive triggers
  fcpt(Fact,F),
  % check negative triggers
  fcnt(Fact,F).


%%
%% fc_rule_check(P) does some special, built in forward chaining if P is 
%% a rule.
%% 

fc_rule_check((P==>Q)) :-  
  !,  
  processRule(P,Q,(P==>Q)).
fc_rule_check((Name::::P==>Q)) :- 
  !,  
  processRule(P,Q,(Name::::P==>Q)).
fc_rule_check((P<==>Q)) :- 
  !, 
  processRule(P,Q,(P<==>Q)), 
  processRule(Q,P,(P<==>Q)).
fc_rule_check((Name::::P<==>Q)) :- 
  !, 
  processRule(P,Q,((Name::::P<==>Q))), 
  processRule(Q,P,((Name::::P<==>Q))).

fc_rule_check(('<-'(P,Q))) :-
  !,
  pfcDefineBcRule(P,Q,('<-'(P,Q))).

fc_rule_check(_).


fcpt(Fact,F) :- 
  pfcGetTriggerQuick(pt(F,Body)),
  pfc_trace_msg('      Found positive trigger: ','~p~n       body: ~p~n',
		[F,Body]),
  fcEvalLHS(Body,(Fact,pt(F,Body))),
  fail.

%fcpt(Fact,F) :- 
%  pfcGetTriggerQuick(pt(presently(F),Body)),
%  fcEvalLHS(Body,(presently(Fact),pt(presently(F),Body))),
%  fail.

fcpt(_,_).

fcnt(_Fact,F) :-
  support3(nt(F,Condition,Body),X,_),
  call(Condition),
  pfc_withdraw(X,(_,nt(F,Condition,Body))),
  fail.
fcnt(_,_).


%%
%% pfcDefineBcRule(+Head,+Body,+ParentRule) - defines a backeard
%% chaining rule and adds the corresponding bt triggers to the database.
%%

pfcDefineBcRule(Head,_Body,ParentRule) :-
  (\+ pfcAtom(Head)),
  pfcWarn("Malformed backward chaining rule.  ~p not atomic.",[Head]),
  pfcWarn("rule: ~p",[ParentRule]),
  !,
  fail.

pfcDefineBcRule(Head,Body,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  pfcCurrentUserSupport((_,U)),
  buildRhs(Head,Rhs),
  forall(pfc_nf(Body,Lhs),
          ignore((buildTrigger(Lhs,rhs(Rhs),Trigger),
           add(bt(Head,Trigger),(ParentRuleCopy,U))))).
 


%%
%%
%% eval something on the LHS of a rule.
%%

 
fcEvalLHS((Test->Body),Support) :-  
  !, 
  (call(Test) -> fcEvalLHS(Body,Support)),
  !.

fcEvalLHS(rhs(X),Support) :-
  !,
  pfc_eval_rhs(X,Support),
  !.

fcEvalLHS(X,Support) :-
  pfcType(X,trigger),
  !,
  pfcAddTrigger(X,Support),
  !.

%fcEvalLHS(snip(X),Support) :- 
%  snip(Support),
%  fcEvalLHS(X,Support).

fcEvalLHS(X,_) :-
  pfcWarn("Unrecognized item found in trigger body, namely ~p.",[X]).


%%
%% eval something on the RHS of a rule.
%%

pfc_eval_rhs([],_) :- !.
pfc_eval_rhs([Head|Tail],Support) :- 
  pfc_eval_rhs1(Head,Support),
  pfc_eval_rhs(Tail,Support).


pfc_eval_rhs1({Action},Support) :-
 % evaluable Prolog code.
 !,
 fcEvalAction(Action,Support).

pfc_eval_rhs1(P,_Support) :-
 % predicate to remove.
 pfcNegatedLiteral(P),
 !,
 pfc_withdraw(P).

pfc_eval_rhs1([X|Xrest],Support) :-
 % embedded sublist.
 !,
 pfc_eval_rhs([X|Xrest],Support).

pfc_eval_rhs1(Assertion,Support) :-
 % an assertion to be added.
 post1(Assertion,Support).


pfc_eval_rhs1(X,_) :-
  pfcWarn("Malformed rhs of a rule: ~p",[X]).


%%
%% evaluate an action found on the rhs of a rule.
%%

fcEvalAction(Action,Support) :-
  umt(Action), 
  (undoable(Action) 
     -> pfcAddActionTrace(Action,Support) 
      ; true).


%%
%% 
%%

trigger_trigger(Trigger,Body,_Support) :-
 trigger_trigger1(Trigger,Body).
trigger_trigger(_,_,_).


%trigger_trigger1(presently(Trigger),Body) :-
%  !,
%  copy_term(Trigger,TriggerCopy),
%  pfc(Trigger),
%  fcEvalLHS(Body,(presently(Trigger),pt(presently(TriggerCopy),Body))),
%  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  pfc(Trigger),
  fcEvalLHS(Body,(Trigger,pt(TriggerCopy,Body))),
  fail.



%% pfc(F) is nondet.
%
% pfc(F) is true iff F is a fact available for forward chaining.
% Note that this has the side effect of catching unsupported facts and
% assigning them support from God.
%

pfc(P) :-
  % trigger any bc rules.
  pfcGetTriggerQuick(bt(P,Trigger)),
  pfcGetSupport(bt(P,Trigger),S),
  fcEvalLHS(Trigger,S),
  fail.

pfc(F) :- ground(F),!,pfc0(F),!.
pfc(F) :- pfc0(F).
pfc(F) :-
  %- this is probably not advisable due to extreme inefficiency.
  var(F)    ->  pfcFact(F) ;
  otherwise ->  clause(F,Condition),call(Condition).

%- pfc(F) :- 
%-  %% we really need to check for system predicates as well.
%-  % current_predicate(_,F) -> call(F).
%-  clause(F,Condition),call(Condition).


pfc0(F) :- !,
  %- this is probably not advisable due to extreme inefficiency.
  (var(F)    ->  pfcFact(F) ;
  otherwise -> findall(F-C,clause(F,C),List),member(F-C,List),umt(C)).



% an action is undoable if there exists a method for undoing it.
undoable(A) :- umt(fcUndoMethod(A,_)).



%%
%%
%% defining fc rules 
%%

%% pfc_nf(+In,-Out) maps the LHR of a pfc rule In to one normal form 
%% Out.  It also does certain optimizations.  Backtracking into this
%% predicate will produce additional clauses.


pfc_nf(LHS,List) :-
  mpred_nf1(LHS,List2),
  pfc_nf_negations(List2,List).


%% mpred_nf1(+In,-Out) maps the LHR of a pfc rule In to one normal form
%% Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

mpred_nf1(P,[P]) :- is_ftVar(P), !.


mpred_nf1(P/Cond,[(\+P)/Cond]):- mpred_negated_literal(P), !, dmsg(warn(mpred_nf1(P/Cond,[(\+P)/Cond]))).

mpred_nf1(P/Cond,[P/Cond]):- var(P),!.
mpred_nf1(P/Cond,[P/Cond]):- ((mpred_db_type(P,trigger);mpred_literal_nonvar(P))), !.


% these next two rules are here for upward compatibility and will go 
% away eventually when the P/Condition form is no longer used anywhere.

mpred_nf1(P/Cond,[(\+P)/Cond]) :- pfcNegatedLiteral(P), !.

mpred_nf1(P/Cond,[P/Cond]) :-  pfcAtom(P), !.

%% handle a negated form

mpred_nf1(NegTerm,NF) :-
  pfc_negation(NegTerm,Term),
  !,
  pfc_nf1_negation(Term,NF).

%% disjunction.

mpred_nf1((P;Q),NF) :- 
  !,
  (mpred_nf1(P,NF) ;   mpred_nf1(Q,NF)).


%% conjunction.

mpred_nf1((P,Q),NF) :-
  !,
  mpred_nf1(P,NF1),
  mpred_nf1(Q,NF2),
  append(NF1,NF2,NF).


% prolog_clause mpred_nf1
mpred_nf1((H :- B)  , [(H :- B)]):-  
  mpred_positive_literal(H),!.


%% handle a random atom.

mpred_nf1(P,[P]) :- 
  pfcAtom(P), 
  !.

%%% shouln't we have something to catch the rest as errors?
mpred_nf1(Term,[Term]) :-
  pfcWarn("pfc_nf doesn't know how to normalize ~p",[Term]).


%% pfc_nf1_negation(P,NF) is true if NF is the normal form of \+P.
pfc_nf1_negation((P/Cond),[(\+(P))/Cond]) :- !.

pfc_nf1_negation((P;Q),NF) :-
  !,
  pfc_nf1_negation(P,NFp),
  pfc_nf1_negation(Q,NFq),
  append(NFp,NFq,NF).

pfc_nf1_negation((P,Q),NF) :- 
  % this code is not correct! twf.
  !,
  pfc_nf1_negation(P,NF) 
  ;
  (mpred_nf1(P,Pnf),
   pfc_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

pfc_nf1_negation(P,[\+P]).


%% pfc_nf_negations(List2,List) sweeps through List2 to produce List,
%% changing ~{...} to {\+...}
%%% ? is this still needed? twf 3/16/90

pfc_nf_negations(X,X) :- !.  % I think not! twf 3/27/90

pfc_nf_negations([],[]).

pfc_nf_negations([H1|T1],[H2|T2]) :-
  pfc_nf_negation(H1,H2),
  pfc_nf_negations(T1,T2).

pfc_nf_negation(Form,{\+ X}) :- 
  nonvar(Form),
  Form=(~({X})),
  !.
pfc_nf_negation(X,X).




     %% mpred_unnegate(+N, ?P) is semidet.
     %
     %  is true if N is a negated term and P is the term
     %  with the negation operator stripped.  (not Logical ~ negation however)
     %
     mpred_unnegate(P,_):- is_ftVar(P),!,fail.
     mpred_unnegate((\+(P)),P).
     mpred_unnegate((-P),P).



     %% mpred_negated_literal(+P) is semidet.
     %
     % PFC Negated Literal.
     %
     mpred_negated_literal(P):-
       mpred_unnegate(P,Q),
       mpred_positive_literal(Q).

     orig_2_0_mpred_literal(X):- is_ftVar(X),!.
     orig_2_0_mpred_literal(X):- mpred_negated_literal(X),!.
     orig_2_0_mpred_literal(X):- mpred_positive_literal(X),!.

     mpred_is_trigger(X):-   mpred_db_type(X,trigger).

     mpred_positive_fact(X):-  mpred_positive_literal(X), X \= ~(_), mpred_db_type(X,fact(_FT)), \+ mpred_db_type(X,trigger).

     mpred_positive_literal(X):-
       is_ftNonvar(X),
       \+ mpred_db_type(X,rule(_RT)),
       get_functor(X,F,_),
       \+ mpred_neg_connective(F),
       !.


     mpred_connective(Var):-var(Var),!,fail.
     mpred_connective(';').
     mpred_connective(',').
     mpred_connective('/').
     mpred_connective('{}').
     mpred_connective('|').
     mpred_connective(('==>')).
     mpred_connective(('<-')).
     mpred_connective('==>').
     mpred_connective('-').
     % mpred_connective('~').
     mpred_connective(('\\+')).


     mpred_neg_connective('-').
     % mpred_neg_connective('~').
     mpred_neg_connective('\\+').

     is_simple_lhs(ActN):- is_ftVar(ActN),!,fail.
     is_simple_lhs( \+ _ ):-!,fail.
     is_simple_lhs( ~ _ ):-!,fail.
     is_simple_lhs( _  / _ ):-!,fail.
     is_simple_lhs((Lhs1,Lhs2)):- !,is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
     is_simple_lhs((Lhs1;Lhs2)):- !,is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
     is_simple_lhs(ActN):- is_active_lhs(ActN),!,fail.
     is_simple_lhs((Lhs1/Lhs2)):- !,fail, is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
     is_simple_lhs(_).


     is_active_lhs(ActN):- var(ActN),!,fail.
     is_active_lhs(!).
     is_active_lhs(cut_c).
     is_active_lhs(actn(_Act)).
     is_active_lhs('{}'(_Act)).
     is_active_lhs((Lhs1/Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).
     is_active_lhs((Lhs1,Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).
     is_active_lhs((Lhs1;Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).


     add_lhs_cond(Lhs1/Cond,Lhs2,Lhs1/(Cond,Lhs2)):-!.
     add_lhs_cond(Lhs1,Lhs2,Lhs1/Lhs2).


     %% constrain_meta(+Lhs, ?Guard) is semidet.
     %
     % Creates a somewhat sane Guard.
     %
     % To turn this feature off...
     % ?- set_prolog_flag(constrain_meta,false).  
     %
     %
     constrain_meta(_,_):- current_prolog_flag(constrain_meta,false),!,fail.
     % FACT
     constrain_meta(P,mpred_positive_fact(P)):- is_ftVar(P),!.
     % NEG chaining
     constrain_meta(~ P, CP):- !,  constrain_meta(P,CP).
     constrain_meta(\+ P, CP):- !,  constrain_meta(P,CP).
     % FWD chaining
     constrain_meta((_==>Q),nonvar(Q)):- !, is_ftVar(Q).
     % EQV chaining
     constrain_meta((P<==>Q),(nonvar(Q);nonvar(P))):- (is_ftVar(Q);is_ftVar(P)),!.
     % BWD chaining
     constrain_meta((Q <- _),mpred_literal(Q)):- is_ftVar(Q),!.
     constrain_meta((Q <- _),CQ):- !, constrain_meta(Q,CQ).
     % CWC chaining
     constrain_meta((Q :- _),mpred_literal(Q)):- is_ftVar(Q),!.
     constrain_meta((Q :- _),CQ):- !, constrain_meta(Q,CQ).



%% mpred_db_type(+VALUE1, ?Type) is semidet.
%
% PFC Database Type.
%
%  simple typeing for Pfc objects
%
mpred_db_type(Var,Type):- var(Var),!, Type=fact(_FT).
mpred_db_type(_:X,Type):- !, mpred_db_type(X,Type).
mpred_db_type(~_,Type):- !, Type=fact(_FT).
mpred_db_type(('==>'(_,_)),Type):- !, Type=rule(fwd).
mpred_db_type(('==>'(_,_)),Type):- !, Type=rule(==>).
mpred_db_type(('<-'(_,_)),Type):- !, Type=rule(bwc).
mpred_db_type((':-'(_,_)),Type):- !, Type=rule(cwc).
mpred_db_type(pt(_,_,_),Type):- !, Type=trigger.
mpred_db_type(pt(_,_),Type):- !, Type=trigger.
mpred_db_type(nt(_,_,_),Type):- !,  Type=trigger.
mpred_db_type(bt(_,_),Type):- !,  Type=trigger.
mpred_db_type(actn(_),Type):- !, Type=action.
mpred_db_type((('::::'(_,X))),Type):- !, mpred_db_type(X,Type).
mpred_db_type(_,fact(_FT)):-
  %  if it''s not one of the above, it must_ex be a fact!
  !.



%%
%% buildRhs(+Conjunction,-Rhs)
%%

buildRhs(X,[X]) :- 
  var(X),
  !.

buildRhs((A,B),[A2|Rest]) :- 
  !, 
  pfcCompileRhsTerm(A,A2),
  buildRhs(B,Rest).

buildRhs(X,[X2]) :-
   pfcCompileRhsTerm(X,X2).

pfcCompileRhsTerm((P/C),((P:-C))) :- !.

pfcCompileRhsTerm(P,P).


%% pfc_negation(N,P) is true if N is a negated term and P is the term
%% with the negation operator stripped.

pfc_negation(P,_):- is_ftVar(P),!,fail.
pfc_negation((~P),P).
pfc_negation((-P),P).
pfc_negation((\+(P)),P).

pfcNegatedLiteral(P) :- 
  callable(P),
  pfc_negation(P,Q),
  pfcPositiveAtom(Q).

pfcAtom(X) :- pfcNegatedLiteral(X).
pfcAtom(X) :- pfcPositiveAtom(X).

pfcPositiveAtom(X) :-  
  callable(X),
  functor(X,F,_), 
  \+ pfcConnective(F).

pfcConnective(';').
pfcConnective(',').
pfcConnective('/').
pfcConnective('|').
pfcConnective(('==>')).
pfcConnective(('<-')).
pfcConnective('<==>').

pfcConnective(('==>')).
pfcConnective(('<-=')).
pfcConnective('==>').

pfcConnective('-').
%pfcConnective('~').
pfcConnective(( \+ )).

processRule(Lhs,Rhs,ParentRule) :-
 pfcCurrentUserSupport((_,U)),
  copy_term(ParentRule,ParentRuleCopy),
  buildRhs(Rhs,Rhs2),
  forall(pfc_nf(Lhs,Lhs2), 
          ignore(buildRule(Lhs2,rhs(Rhs2),(ParentRuleCopy,U)))).

buildRule(Lhs,Rhs,Support) :-
  buildTrigger(Lhs,Rhs,Trigger),
  fcEvalLHS(Trigger,Support).

buildTrigger([],Consequent,Consequent).

buildTrigger([V|Triggers],Consequent,pt(V,X)) :-
  var(V),
  !, 
  buildTrigger(Triggers,Consequent,X).

buildTrigger([(T1/Test)|Triggers],Consequent,nt(T2,Test2,X)) :-
  pfc_negation(T1,T2),
  !, 
  buildNtTest(T2,Test,Test2),
  buildTrigger(Triggers,Consequent,X).

buildTrigger([(T1)|Triggers],Consequent,nt(T2,Test,X)) :-
  pfc_negation(T1,T2),
  !,
  buildNtTest(T2,true,Test),
  buildTrigger(Triggers,Consequent,X).

buildTrigger([{Test}|Triggers],Consequent,(Test->X)) :-
  !,
  buildTrigger(Triggers,Consequent,X).

buildTrigger([T/Test|Triggers],Consequent,pt(T,X)) :-
  !, 
  buildTest(Test,Test2),
  buildTrigger([{Test2}|Triggers],Consequent,X).


%buildTrigger([snip|Triggers],Consequent,snip(X)) :-
%  !,
%  buildTrigger(Triggers,Consequent,X).

buildTrigger([T|Triggers],Consequent,pt(T,X)) :-
  !, 
  buildTrigger(Triggers,Consequent,X).

%%
%% buildNtTest(+,+,-).
%%
%% builds the test used in a negative trigger (nt/3).  This test is a
%% conjunction of the check than no matching facts are in the db and any
%% additional test specified in the rule attached to this ~ term.
%%

buildNtTest(T,Testin,Testout) :-
  buildTest(Testin,Testmid),
  pfcConjoin((pfc(T)),Testmid,Testout).

  
% this just strips away any currly brackets.

buildTest({Test},Test) :- !.
buildTest(Test,Test).

%%



%% simple typeing for pfc objects

pfcType(('==>'(_,_)),Type) :- !, Type=rule.
pfcType(('<==>'(_,_)),Type) :- !, Type=rule.
pfcType(('<-'(_,_)),Type) :- !, Type=rule.
pfcType(pt(_,_,_),Type) :- !, Type=trigger.
pfcType(pt(_,_),Type) :- !, Type=trigger.
pfcType(nt(_,_,_),Type) :- !,  Type=trigger.
pfcType(bt(_,_),Type) :- !,  Type=trigger.
pfcType(pfcAction(_),Type) :- !, Type=action.
pfcType((('::::'(_,X))),Type) :- !, pfcType(X,Type).
pfcType(_,fact) :-
  %% if it's not one of the above, it must be a fact!
  !.

pfcAssert(P,Support) :- 
  (pfc_clause(P) ; assert(P)),
  !,
  pfcAddSupport(P,Support).

pfcAsserta(P,Support) :-
  (pfc_clause(P) ; asserta(P)),
  !,
  pfcAddSupport(P,Support).

pfcAssertz(P,Support) :-
  (pfc_clause(P) ; assertz(P)),
  !,
  pfcAddSupport(P,Support).

pfc_clause((Head :- Body)) :-
  !,
  copy_term(Head,Head_copy),
  copy_term(Body,Body_copy),
  clause(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

pfc_clause(Head) :-
  % find a unit clause identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  clause(Head_copy,true),
  variant(Head,Head_copy).

pfcForEach(Binder,Body) :- umt(( Binder,pfcdo(Body))),fail.
pfcForEach(_,_).

% pfcdo(X) executes X once and always succeeds.
pfcdo(X) :- umt((X)),!.
pfcdo(_).


%% pfcUnion(L1,L2,L3) - true if set L3 is the result of appending sets
%% L1 and L2 where sets are represented as simple lists.

pfcUnion([],L,L).
pfcUnion([Head|Tail],L,Tail2) :-  
  memberchk(Head,L),
  !,
  pfcUnion(Tail,L,Tail2).
pfcUnion([Head|Tail],L,[Head|Tail2]) :-  
  pfcUnion(Tail,L,Tail2).


%% pfcConjoin(+Conjunct1,+Conjunct2,?Conjunction).
%% arg3 is a simplified expression representing the conjunction of
%% args 1 and 2.

pfcConjoin(C1,C2,C12):- 
  C1 == true -> C12 = C2;
  C2 == true -> C12 = C1;
  otherwise -> C12 = (C1,C2).

/*
pfcConjoin(true,X,X) :- !.
pfcConjoin(X,true,X) :- !.
pfcConjoin(C1,C2,(C1,C2)).
*/

:- fixup_exports.


%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
%%	restore, reset, etc.0

:- module(pfcdb, []).
:- use_module(library(pfc_pack_xform)).

% pfcDatabaseTerm(P/A) is true iff P/A is something that pfc adds to
% the database and should not be present in an empty pfc database

pfcDatabaseTerm(support1/3):- ifNotDMiles(true,fail).
pfcDatabaseTerm(support2/3):- ifNotDMiles(true,fail).
pfcDatabaseTerm(support3/3):- ifNotDMiles(true,fail).
pfcDatabaseTerm(spft/3):- ifNotDMiles(fail,true).

pfcDatabaseTerm(pt/2).
pfcDatabaseTerm(bt/2).
pfcDatabaseTerm(nt/3).
pfcDatabaseTerm('==>'/2).
pfcDatabaseTerm('<==>'/2).
pfcDatabaseTerm('<-'/2).
pfcDatabaseTerm(pfcQueue/2).

% removes all forward chaining rules and justifications from db.

pfcReset :-
  pfcGetSupport(P,(F,Trigger)),
  pfcAddDbToHead(P,PDb),
  pfcRetractOrWarn(PDb),
  pfcRetractOrWarn(support1(P,F,Trigger)),
  pfcRetractOrWarn(support2(F,Trigger,P)),
  pfcRetractOrWarn(support3(Trigger,P,F)),
  fail.
pfcReset :-
  pfcDatabaseItem(T),
  pfcError("Pfc database not empty after pfcReset, e.g., ~p.~n",[T]).
pfcReset.

% true if there is some pfc crud still in the database.
pfcDatabaseItem(Term) :-
  pfcDatabaseTerm(P/A),
  functor(Term,P,A),
  clause(Term,_).

pfcRetractOrWarn(X) :-  retract(X), !.
pfcRetractOrWarn(X) :- 
  pfcWarn("Couldn't retract ~p.",[X]).


:- fixup_exports.

%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugginh 
%   for Pfc.

:- module(pfcdebug, []).
:- use_module(library(pfc_pack_xform)).

:- dynamic pfcTraced/1.
:- dynamic pfcSpied/2.
:- dynamic pfcTraceExecution/0.
:- dynamic   pfcWarnings/1.


:- initialization(pfcDefault(pfcWarnings(_), pfcWarnings(true))).

%% predicates to examine the state of pfc

pfcQueue :- umt(( listing(pfcQueue/2))).

pfcPrintDB :-
  pfcPrintFacts,
  pfcPrintRules,
  pfcPrintTriggers,
  pfcPrintSupports.

%% pfcPrintFacts ...

pfcPrintFacts :- pfcPrintFacts(_,true).

pfcPrintFacts(Pattern) :- pfcPrintFacts(Pattern,true).

pfcPrintFacts(P,C) :-
  pfcFacts(P,C,L),
  pfcClassifyFacts(L,User,Pfc,_Rule),
  ansi_format([underline],"~N~nUser added facts: ",[]),
  pfcPrintitems(User),
  ansi_format([underline],"~N~nPfc added facts: ",[]),
  pfcPrintitems(Pfc).



pfcPrintitems(List):- \+ \+ umt((pfcPrintitems0(List))).
%- printitems0 clobbers it's arguments - beware!
pfcPrintitems0([]).
pfcPrintitems0([H|T]) :-
  numbervars(H,0,_),
  ansi_format([bold],"~N  ~p",[H]),
  pfcPrintitems0(T).

pfcClassifyFacts([],[],[],[]).

pfcClassifyFacts([H|T],User,Pfc,[H|Rule]) :-
  pfcType(H,rule),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],[H|User],Pfc,Rule) :-
  pfcGetSupport(H,UU),
  get_first_real_user_reason(H,UU),
  is_axiom_support(UU),
  !,
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcClassifyFacts([H|T],User,[H|Pfc],Rule) :-
  pfcClassifyFacts(T,User,Pfc,Rule).

pfcPrintRules :-
  bagof((P==>Q),clause((P==>Q),true),R1),
  pfcPrintitems(R1),
  bagof((P<==>Q),clause((P<==>Q),true),R2),
  pfcPrintitems(R2),
  bagof((P<-Q),clause((P<-Q),true),R3),
  pfcPrintitems(R3).

pfcPrintTriggers :-
  ansi_format([underline],"~NPositive triggers...~n",[]),
  bagof(pt(T,B),pfcGetTrigger(pt(T,B)),Pts),
  pfcPrintitems(Pts),
  ansi_format([underline],"~NNegative triggers...~n",[]),
  bagof(nt(A,B,C),pfcGetTrigger(nt(A,B,C)),Nts),
  pfcPrintitems(Nts),
  ansi_format([underline],"~NGoal triggers...~n",[]),
  bagof(bt(A,B),pfcGetTrigger(bt(A,B)),Bts),
  pfcPrintitems(Bts).

pfcPrintSupports :- 
  % temporary hack.
  setof((S > P), pfcGetSupport(P,S),L),
  pfcPrintitems(L).

%% pfcFact(P) is true if fact P was asserted into the database via add.

pfcFact(P) :- pfcFact(P,true).

%% pfcFact(P,C) is true if fact P was asserted into the database via
%% add and contdition C is satisfied.  For example, we might do:
%% 
%%  pfcFact(X,pfcUserFact(X))
%%

pfcFact(P,C) :- 
  pfcGetSupport(P,_),
  pfcType(P,fact),
  call(C).

%% pfcFacts(-ListofPfcFacts) returns a list of facts added.

pfcFacts(L) :- pfcFacts(_,true,L).

pfcFacts(P,L) :- pfcFacts(P,true,L).

%% pfcFacts(Pattern,Condition,-ListofPfcFacts) returns a list of facts added.

pfcFacts(P,C,L) :- setof(P,pfcFact(P,C),L).

brake(X) :-  pfc(X), break.

%%
%%
%% predicates providing a simple tracing facility
%%

pfcTraceAdd(P) :- 
  % this is here for upward compat. - should go away eventually.
  pfcTraceAdd(P,(o,o)).

pfcTraceAdd(pt(_,_),_) :-
  % hack for now - never trace triggers.
  !.
pfcTraceAdd(nt(_,_,_),_) :-
  % hack for now - never trace triggers.
  !.

pfcTraceAdd(P,S) :-
   pfcTraceAddPrint(P,S),
   pfcTraceBreak(P,S).
   

pfcTraceAddPrint(P,S) :-
  umt(pfcTraced(P)),
  !,
  copy_term(P,Pcopy),
  numbervars(Pcopy,0,_),
  (pfcCurrentUserSupport(S)
       -> ansi_format([fg(green)],"~NAdding (u) ~p~n",[Pcopy])
        ; ansi_format([fg(green)],"~NAdding ~p~n",[Pcopy])).

pfcTraceAddPrint(_,_).


pfcTraceBreak(P,_S) :-
  umt(pfcSpied(P,add)) -> 
   (copy_term(P,Pcopy),
    numbervars(Pcopy,0,_),
    ansi_format([fg(yellow)],"~N~nBreaking on add(~p)~n",[Pcopy]),
    break)
   ; true.

pfcTraceRem(pt(_,_)) :-
  % hack for now - never trace triggers.
  !.
pfcTraceRem(nt(_,_,_)) :-
  % hack for now - never trace triggers.
  !.

pfcTraceRem(P) :-
  (umt(pfcTraced(P))
     -> ansi_format([fg(cyan)],'~NRemoving ~p.',[P])
      ; true),
  (umt(pfcSpied(P,rem))
   -> (ansi_format([fg(yellow)],"~NBreaking on rem(~p)",[P]),
       break)
   ; true).


pfcTrace :- pfcTrace(_).

pfcTrace(Form) :-
  assert(pfcTraced(Form)).

pfcTrace(Form,Condition) :- 
  assert((pfcTraced(Form) :- umt(Condition))).

pfcSpy(Form) :- pfcSpy(Form,[add,rem],true).

pfcSpy(Form,Modes) :- pfcSpy(Form,Modes,true).

pfcSpy(Form,[add,rem],Condition) :-
  !,
  pfcSpy1(Form,add,Condition),
  pfcSpy1(Form,rem,Condition).

pfcSpy(Form,Mode,Condition) :-
  pfcSpy1(Form,Mode,Condition).

pfcSpy1(Form,Mode,Condition) :-
  assert((pfcSpied(Form,Mode) :- umt(Condition))).

pfcNospy :- pfcNospy(_,_,_).

pfcNospy(Form) :- pfcNospy(Form,_,_).

pfcNospy(Form,Mode,Condition) :- 
  clause(pfcSpied(Form,Mode), umt(Condition), Ref),
  erase(Ref),
  fail.
pfcNospy(_,_,_).

pfcNoTrace :- pfcUntrace.
pfcUntrace :- pfcUntrace(_).
pfcUntrace(Form) :- retractall(pfcTraced(Form)).

% needed:  pfcTraceRule(Name)  ...


pfc_trace_msg(Msg) :- pfc_trace_msg('TRACE:','~N~p~N', Msg),!.
% if the correct flag is set, trace exection of Pfc
pfc_trace_msg(Msg,Args) :- pfc_trace_msg('       TRACE:',Msg,Args).
pfc_trace_msg(PreMsg,Msg,Args) :-
    umt(pfcTraceExecution),
    !,
    ansi_format([fg(green)], '~N~n', []),!,
    ansi_format([fg(green)], PreMsg, []),!,
    ansi_format([fg(yellow)], Msg, Args),!.
pfc_trace_msg(_PreMsg,_Msg,_Args).


mpred_notrace_exec:- pfcNoWatch.

mpred_trace_exec:- pfcWatch.

pfcWatch :- assert(pfcTraceExecution).

pfcNoWatch :-  retractall(pfcTraceExecution).

pfcError(Msg) :-  pfcError(Msg,[]).

pfcError(Msg,Args) :- 
  ansi_format([fg(red)],"~N~nERROR/Pfc: ",[]),
  ansi_format([fg(red),bold],Msg,Args),
  ansi_format([underline],"~N",[]).


%%
%% These control whether or not warnings are printed at all.
%%   pfcWarn.
%%   nopfcWarn.
%%
%% These print a warning message if the flag pfcWarnings is set.
%%   pfcWarn(+Message)
%%   pfcWarn(+Message,+ListOfArguments)
%%

pfcWarn :- 
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(true)).

nopfcWarn :-
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(false)).
 
pfcWarn(Msg) :-  pfcWarn('~p',[Msg]).

pfcWarn(Msg,Args) :- 
  umt(pfcWarnings(true)),
  !,
  ansi_format([fg(red)],"~N~nWARNING/Pfc: ",[]),
  ansi_format([fg(yellow)],Msg,Args),
  ansi_format([underline],"~N",[]).
pfcWarn(_,_).

%%
%% pfcWarnings/0 sets flag to cause pfc warning messages to print.
%% pfcNoWarnings/0 sets flag to cause pfc warning messages not to print.
%%

pfcWarnings :- 
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(true)).

pfcNoWarnings :- 
  retractall(pfcWarnings(_)).

:- fixup_exports.
%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%= *** predicates for exploring supports of a fact *****

:- module(pfcjust, []).
:- use_module(library(pfc_pack_xform)).

:- use_module(library(lists)).

justification(F,J) :- supports(F,J).

justifications(F,Js) :- bagof(J,justification(F,J),Js).



%% base(P,L) - is true iff L is a list of "base" facts which, taken
%% together, allows us to deduce P.  A base fact is an axiom (a fact 
%% added by the user or a raw Prolog fact (i.e. one w/o any support))
%% or an assumption.

base(F,[F]) :- (axiom(F) ; assumption(F)),!.

base(F,L) :-
  % i.e. (reduce 'append (map 'base (justification f)))
  justification(F,Js),
  bases(Js,L).


%% bases(L1,L2) is true if list L2 represents the union of all of the 
%% facts on which some conclusion in list L1 is based.

bases([],[]).
bases([X|Rest],L) :-
  base(X,Bx),
  bases(Rest,Br),
  pfcUnion(Bx,Br,L).
	
%- axiom(F) :- 
%-  pfcGetSupport(F,(user,user)); 
%-  pfcGetSupport(F,(god,god)).

axiom(F) :- 
 umt(((pfcGetSupport(F,UU),
   \+ \+ is_axiom_support(UU)))).

pfcCurrentUserSupport(UU):- get_source_ref(UU).
%pfcCurrentUserSupport((user,user)).

%is_axiom_support(UU):- pfcCurrentUserSupport(UU).
is_axiom_support((_,AX)):- atomic(AX).


%% an assumption is a failed goal, i.e. were assuming that our failure to 
%% prove P is a proof of not(P)

assumption(P) :- pfc_negation(P,_).
   
%% assumptions(X,As) if As is a set of assumptions which underly X.

assumptions(X,[X]) :- assumption(X).
assumptions(X,[]) :- axiom(X).
assumptions(X,L) :-
  justification(X,Js),
  assumptions1(Js,L).

assumptions1([],[]).
assumptions1([X|Rest],L) :-
  assumptions(X,Bx),
  assumptions1(Rest,Br),
  pfcUnion(Bx,Br,L).  


%% pfcProofTree(P,T) the proof tree for P is T where a proof tree is
%% of the form
%%
%%     [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%%          ^                         and has the form of
%%          [J11, J12,... J1n]      a list of proof trees.


%% mpred_child(+P,?Q) is nondet.
%
% mpred_child(P,Q) is true iff P is an immediate justifier for Q.
%

mpred_child(P,Q) :-
  pfcGetSupport(Q,(P,_)).

mpred_child(P,Q) :-
  pfcGetSupport(Q,(_,Trig)),
  pfcType(Trig,trigger),
  mpred_child(P,Trig).

mpred_children(P,L) :- bagof(C,mpred_child(P,C),L).

% pfcDescendant(P,Q) is true iff P is a justifier for Q.

pfcDescendant(P,Q) :- 
   pfcDescendant1(P,Q,[]).

pfcDescendant1(P,Q,Seen) :-
  mpred_child(X,Q),
  (\+ member(X,Seen)),
  (P=X ; pfcDescendant1(P,X,[X|Seen])).
  
pfcDescendants(P,L) :- 
  bagof(Q,pfcDescendant1(P,Q,[]),L).

%%
%%
%% predicates for manipulating support relationships
%%
:- module(pfcsupport,[ifNotDMiles/1,ifNotDMiles/2]).


:- use_module(library(pfc_pack_xform)).

:- if(false).
% NON-DMILES
:- dynamic support1/3.
:- dynamic support2/3.
:- dynamic support3/3.
ifNotDMiles(G):- G.
ifNotDMiles(G,_):- G.

:- else.

% DMILES
:- dynamic spft/3.
support1(P,Fact,Trigger):-umt(spft(P,Fact,Trigger)).
support2(Fact,Trigger,P):-umt(spft(P,Fact,Trigger)).
support3(Trigger,P,Fact):-umt(spft(P,Fact,Trigger)).

ifNotDMiles(_).
ifNotDMiles(_,G):- G.

:- endif.


%% pfcAddSupport(+Fact,+Support)

pfcAddSupport(P,(Fact,Trigger)) :- umt(clause_asserted(spft(P,Fact,Trigger))),!,dmsg(tWICE_pfcAddSupport(P,(Fact,Trigger))),!.
pfcAddSupport(P,(Fact,Trigger)) :-
  show_call(assert_u(spft(P,Fact,Trigger))),!,
  nop(ifNotDMiles(assert(support2(Fact,Trigger,P)))),
  nop(ifNotDMiles(assert(support3(Trigger,P,Fact)))),!.

pfcGetSupport(P,(F,T)):- !, umt((spft(P,F,T))).

pfcGetSupport(P,FT):- umt((pfcGetSupport0(P,FT))).

pfcGetSupport(P,(Fact,Trigger)) :-
   nonvar(P)         -> support1(P,Fact,Trigger) 
   ; nonvar(Fact)    -> support2(Fact,Trigger,P) 
   ; nonvar(Trigger) -> support3(Trigger,P,Fact) 
   ; otherwise       -> support1(P,Fact,Trigger).


% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(P),
  !,
  pfcRetractOrWarn(spft(P,Fact,Trigger)),
  ifNotDMiles(pfcRetractOrWarn(support2(Fact,Trigger,P))),
  ifNotDMiles(pfcRetractOrWarn(support3(Trigger,P,Fact))).


pfcRemSupport(P,(Fact,Trigger)) :-
  nonvar(Fact),
  !,
  ifNotDMiles(pfcRetractOrWarn(support2(Fact,Trigger,P)),support2(Fact,Trigger,P)),
  pfcRetractOrWarn(spft(P,Fact,Trigger)),
  ifNotDMiles(pfcRetractOrWarn(support3(Trigger,P,Fact))).

pfcRemSupport(P,(Fact,Trigger)) :-
  ifNotDMiles(pfcRetractOrWarn(support3(Trigger,P,Fact)),support3(Trigger,P,Fact)),
  pfcRetractOrWarn(spft(P,Fact,Trigger)),
  ifNotDMiles(pfcRetractOrWarn(support2(Fact,Trigger,P))).


pfc_collect_supports(Tripples) :-
  bagof(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports([]).

pfc_support_relation((P,F,T)) :-
  umt((support1(P,F,T))).

pfc_make_supports((P,S1,S2)) :- 
  pfcAddSupport(P,(S1,S2)),
  (pfcAdd(P); true),
  !.

%% pfcTriggerKey(+Trigger,-Key) 
%%
%% Arg1 is a trigger.  Key is the best term to index it on.

pfcTriggerKey(pt(Key,_),Key).
% unused? pfcTriggerKey(pt(Key,_,_),Key).
pfcTriggerKey(nt(Key,_,_),Key).
pfcTriggerKey(Key,Key).


%%^L
%% Get a key from the trigger that will be used as the first argument of
%% the trigger base clause that stores the trigger.
%%

pfc_trigger_key(X,X) :- var(X), !.
pfc_trigger_key(chart(word(W),_L),W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
pfc_trigger_key(chart(Concept,_L),Concept) :- !.
pfc_trigger_key(X,X).

:- fixup_exports.
%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

end_of_file.

:- module(pfcwhy, []).
:- use_module(library(pfc_pack_xform)).

% ***** predicates for brousing justifications *****

:- use_module(library(lists)).

:- thread_local(t_l:whybuffer/2).

pfcWhy :- 
 umt((
  t_l:whybuffer(P,_),
  mpred_why0(P))).
% see pfc_why
mpred_why(X):- source_file(_,_), % non-interactive
  color_line(green,2),
  forall(no_repeats(P-Js,justifications(P,Js)),
    (color_line(yellow,1),mpred_showJustifications(X,Js))),
  color_line(green,2),!.
  

mpred_why(X):-
  umt((mpred_why0(X))).

mpred_why0(N) :-
  number(N),
  !,
  t_l:whybuffer(P,Js),
  mpred_whyCommand0(N,P,Js).

mpred_why0(P) :-
  justifications(P,Js),
  retractall(t_l:whybuffer(_,_)),
  assert(t_l:whybuffer(P,Js)),
  mpred_whyBrouse(P,Js).

mpred_why1(P) :-
  justifications(P,Js),
  mpred_whyBrouse(P,Js).

mpred_whyBrouse(P,Js) :-
  mpred_showJustifications(P,Js),
  ttyflush,
  read_pending_chars(current_input,_,[]),!,
  ttyflush,
  % pfcAsk(' >> ',Answer),
  % read_pending_chars(current_input,[Answer|_],[]),!,  
  format('~N',[]),write('proof [q/h/u/?.?]: '),get_char(Answer),
  mpred_whyCommand0(Answer,P,Js).

mpred_whyCommand0(q,_,_) :- !.
mpred_whyCommand0(h,_,_) :- 
  !,
  format("~n
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level
",[]).

mpred_whyCommand0(N,_P,Js) :-
  float(N),
  !,
  mpred_selectJustificationNode(Js,N,Node),
  mpred_why1(Node).

mpred_whyCommand0(u,_,_) :-
  % u=up
  !.

mpred_whyCommand0(N,_,_) :-
  integer(N),
  !,
  format("~n~w is a yet unimplemented command.",[N]),
  fail.

mpred_whyCommand0(X,_,_) :-
 format("~n~w is an unrecognized command, enter h. for help.",[X]),
 fail.
  
mpred_showJustifications(P,Js) :-
  format("~nJustifications for ~w:",[P]),
  mpred_showJustification1(Js,1).

mpred_showJustification1([],_).

mpred_showJustification1([J|Js],N) :-
  % show one justification and recurse.
  nl,
  mpred_showJustifications2(J,N,1),
  N2 is N+1,
  mpred_showJustification1(Js,N2).

mpred_showJustifications2([],_,_).

mpred_showJustifications2([C|Rest],JustNo,StepNo) :- 
  copy_term(C,CCopy),
  numbervars(CCopy,0,_),
  format("~n    ~w.~w ~w",[JustNo,StepNo,CCopy]),
  StepNext is 1+StepNo,
  mpred_showJustifications2(Rest,JustNo,StepNext).

pfcAsk(Msg,Ans) :-
  format("~n~w",[Msg]),
  read(Ans).

mpred_selectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth1(StepNo,Justification,Step).
 



:- throw(module(pfc_umt,[umt/1])).
:- module(pfc_umt,[umt/1]).

:- 
    op(1050,xfx,('==>')),
    op(1050,xfx,'<==>'),
    op(1050,xfx,('<-')),
    op(1050,xfx,('<==')),
    op(1100,fx,('==>')).


:- op(1050,xfx,('<-')).
:- op(1199,fx,('==>')).
:- op(1190,xfx,('::::')).
:- op(1180,xfx,('==>')).
:- op(1170,xfx,'<==>').
:- op(1160,xfx,('<-')).
:- op(1150,xfx,'=>').
:- op(1140,xfx,'<=').
:- op(1130,xfx,'<=>').
:- op(1100,fx,('nesc')).
:- op(300,fx,'-').
:- op(300,fx,'~').
:- op(600,yfx,'&'). 
:- op(600,yfx,'v').
:- op(1075,xfx,'<-').
:- op(350,xfx,'xor').

nb_current_no_nil(N,V):- nb_current(N,V),V\==[].

set_fileAssertMt(M):- nb_setval(defaultQueryMt,M),nb_setval(defaultAssertMt,M),nb_setval(fileAssertMt,M),maybe_ensure_abox(M).

% :- use_module(pfcsyntax).

defaultQueryMt(M):- nb_current_no_nil(defaultQueryMt,M)->true;(defaultQueryMt0(M)->nb_setval(defaultQueryMt,M)),!.
defaultQueryMt(M):- M=baseKB.
defaultQueryMt0(M):- nb_current_no_nil(fileAssertMt,M),!.
defaultQueryMt0(M):- nb_current_no_nil(defaultAssertMt,M),!.
defaultQueryMt0(M):- strip_module(module,M,module),M \==user,!.
defaultQueryMt0(M):- prolog_load_context(module,M),M \==user,!.
defaultQueryMt0(M):- '$current_typein_module'(M),M \==user,!.

set_defaultAssertMt(M):- nb_setval(defaultAssertMt,M),maybe_ensure_abox(M).

defaultAssertMt(M):- nb_current_no_nil(defaultAssertMt,M)->true;(defaultAssertMt0(M)->nb_setval(defaultAssertMt,M)),!.
defaultAssertMt(M):- M=baseKB.
defaultAssertMt0(M):- nb_current_no_nil(fileAssertMt,M),!.
defaultAssertMt0(M):- nb_current_no_nil(defaultQueryMt,M),!.
defaultAssertMt0(M):- 'strip_module'(module,M,module),M \==user,!.
defaultAssertMt0(M):- '$current_typein_module'(M),M \==user,!.
defaultAssertMt0(M):- prolog_load_context(module,M), M \==user,!.

fileAssertMt(M):- nb_current_no_nil(fileAssertMt,M)->true;(fileAssertMt0(M)->nb_setval(fileAssertMt,M)),!.
fileAssertMt(M):- M=baseKB.
fileAssertMt0(M):- nb_current_no_nil(defaultAssertMt,M),!.
fileAssertMt0(M):- nb_current_no_nil(defaultQueryMt,M),!.
fileAssertMt0(M):- prolog_load_context(module,M),M \==user,!.
fileAssertMt0(M):- '$current_typein_module'(M),M \==user,!.
fileAssertMt0(M):- 'strip_module'(module,M,module),M \==user,!.

fix_mp(_OpType,MP,M,P):-strip_module(MP,M,P).

get_clause_head(H,H):- \+ compound(H),!.
get_clause_head(P:-_,H):-!,get_clause_head(P,H).
get_clause_head(_:P,H):- get_clause_head(P,H).
get_clause_head(H,H).

get_unnegated_head(P,H):- get_clause_head(P,M),((current_predicate(pfc_negation/2),
  pfc_negation(M,H))->true;M=H).

get_unnegated_functor(P,F,A):-get_unnegated_head(P,H),!,functor(H,F,A).

:- module_transparent( (get_unnegated_head_arg)/3).
get_unnegated_head_arg(N,P,E):-get_unnegated_head(P,H),!,arg(N,H,E).


mpred_trace :- pfcWatch.
mpred_trace_exec :- pfcWatch.


%% erase_w_attvars( +Data0, ?Ref) is semidet.
%
% Erase W Attribute Variables.
%
erase_w_attvars(Data0,Ref):- attempt_side_effect(erase(Ref)),add_side_effect(erase,Data0).

ensure_abox(Mt):- maybe_ensure_abox(Mt).

:- meta_predicate(maybe_ensure_abox(:)).

maybe_ensure_abox(Mt):- 
 strip_module(Mt,I,M), 
 dmsg(maybe_ensure_abox(strip_module(Mt,I,M))),
 must(maybe_ensure_abox(M,M)).

:- multifile(pfc_umt:pfcDatabaseTerm_DYN/1).
:- dynamic(pfc_umt:pfcDatabaseTerm_DYN/1).

%quietly_ex(G):- quietly(G),!.
quietly_ex(G):- quietly(umt((G))),!.

%trace_or_throw_ex(G):- trace_or_throw_ex(G).

trace_or_throw_ex(G):- log_failure,trace_or_throw_ex(G).

%pfc_umt:pfcDatabaseTerm_DYN(FA):- pfcDatabaseTerm(FA).
pfc_umt:pfcDatabaseTerm_DYN(FA):- member(FA,[never_retract_u/2,never_retract_u/1,never_assert_u/2,never_assert_u/1]).
%% check_never_assert(+Pred) is semidet.
%
% Check Never Assert.
%

%check_never_assert(_Pred):-!.

check_never_assert(Pred):- quietly_ex(ignore(( copy_term_and_varnames(Pred,Pred_2),umt(never_assert_u(Pred_2,Why)),variant_u(Pred,Pred_2),trace_or_throw_ex(never_assert_u(Pred,Why))))).

%check_never_assert(Pred):- quietly_ex(ignore(( copy_term_and_varnames(Pred,Pred_2),umt(never_assert_u(Pred_2)),variant_u(Pred,Pred_2),trace_or_throw_ex(never_assert_u(Pred))))).
%check_never_assert(Pred):- quietly_ex((( copy_term_and_varnames(Pred,Pred_2),umt(never_assert_u(Pred_2,Why)), variant_u(Pred,Pred_2),trace_or_throw_ex(never_assert_u(Pred,Why))))),fail.

%% check_never_retract(+Pred) is semidet.
%
% Check Never Retract.
%

%check_never_retract(_Pred):-!.
check_never_retract(Pred):- quietly_ex(ignore(( copy_term_and_varnames(Pred,Pred_2),umt(never_retract_u(Pred_2,Why)),variant_u(Pred,Pred_2),trace_or_throw_ex(never_retract_u(Pred,Why))))).


:- thread_local(t_l:noDBaseMODs/1).
:- thread_local(t_l:side_effect_buffer/3).


kb_shared_local(_,_, ('=>' / _ )):- dumpST,throw_depricated,!.
kb_shared_local(_,_, _: ('=>' / _ )):- dumpST,throw_depricated,!.
kb_shared_local(_,_, ( _ :  '=>' / _ )):- dumpST,throw_depricated,!.
kb_shared_local(M,I,F/A):- I:kb_local(M:F/A),functor(P,F,A),
  (\+ clause(M:P,_) -> true;
  must((
   sanity((
   
   clause(M:P,_,Ref),
   clause_property(Ref,module(CM)),
   nop(listing(M:P)),
   CM==M))))).
 
maybe_ensure_abox(M,I) :-
  ignore(((M==user;M==baseKB)->true;nop(add_import_module(M,pfc_lib,end)))),
  M:import(pfccore:pfcDefault/2),
  I:import(pfccore:pfcDefault/2),
 % pfc_umt:abox_pred_list(PREDS)-> must_maplist(kb_shared_local(M,I),PREDS),
 forall(no_repeats(pfc_umt:pfcDatabaseTerm_DYN(F/A)),show_call(kb_shared_local(M,I,F/A))).



maybe_ensure_abox(M,I) :-
 pfc_umt:abox_pred_list(PREDS),
  M:module_transparent(PREDS),
  M:dynamic(PREDS),
   M:import(pfccore:pfcDefault/2),
   I:import(pfccore:pfcDefault/2),
 (I==M -> true ;
   ((M:export(M:PREDS),I:import(M:PREDS)))).

pfc_umt:abox_pred_list((([
     ('=>')/2,
     ('::::')/2,
%     '<=>'/2,
pfcTraced/1,
pfcSpied/2,
pfcTraceExecution/0,
pfcWarnings/1,
     'pozt'/2,
     'negt'/3,
     'bkct'/2,
t_l:whybuffer/2,
     fcUndoMethod/2,
     spft/4,
     fcAction/2,
     fcTmsMode/1,
     pfcQueue/1,
     pfcCurrentDb/1,     
     pfcHaltSignal/1,
     pfcDebugging/0,
     pfcSelect/2,
     pfcSearch/1]))).


red_line(Failed):- quietly((
  format('~N',[]),
  quietly_ex((doall((between(1,3,_),
  ansifmt(red,"%%%%%%%%%%%%%%%%%%%%%%%%%%% find ~q in srcs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",[Failed]),
  ansifmt(yellow,"%%%%%%%%%%%%%%%%%%%%%%%%%%% find red_line in srcs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"))))))).








%% umt( +H ) is nondet.
%
% Clause Or Call.
%

:- meta_predicate(umt(*)).
umt(M:Goal):- !, umt(M,Goal).
umt(Goal):-  defaultQueryMt(M),umt(M,Goal).

%% umt( +M, +H ) is nondet.
%
% Clause Or Call.
%

:- meta_predicate(umt(+,*)).
umt(M,Goal):-
  (M:(((predicate_property(Goal,built_in)->call(Goal); 
      ((predicate_property(Goal, defined)->call(Goal);
         pfc_call(Goal))))))).
/*
umt(M,Goal):- 
  (M:(((predicate_property(Goal,built_in)->call(Goal); 
    ((predicate_property(Goal,defined)->call(Goal))*->true);pfc_call(Goal))))).
*/
%% clause_or_call( +H, ?B) is semidet.
%
% Clause Or Call.
%
clause_or_call(M:H,B):-is_ftVar(M),!,no_repeats(M:F/A,(f_to_mfa(H,M,F,A))),M:clause_or_call(H,B).
clause_or_call(isa(I,C),true):-!,umt(isa_asserted(I,C)).
clause_or_call(genls(I,C),true):-!,on_x_log_throw(umt(genls(I,C))).
clause_or_call(H,B):- clause(src_edit(_Before,H),B).
clause_or_call(H,B):- predicate_property(H,number_of_clauses(C)),predicate_property(H,number_of_rules(R)),((R*2<C) -> (clause_u(H,B)*->!;fail) ; clause_u(H,B)).
clause_or_call(H,true):- umt(should_call_for_facts(H)),no_repeats(on_x_log_throw(H)).


% as opposed to simply using clause(H,true).

%% should_call_for_facts( +H) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(H):- get_functor(H,F,A),umt(should_call_for_facts(H,F,A)).



%% should_call_for_facts( +VALUE1, ?F, ?VALUE3) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(_,F,_):- a(prologSideEffects,F),!,fail.
should_call_for_facts(H,_,_):- modulize_head(H,HH), \+ predicate_property(HH,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- clause_b(mpred_prop(_M,F,A,pfcRHS)),!,fail.
should_call_for_facts(_,F,A):- clause_b(mpred_prop(_M,F,A,pfcMustFC)),!,fail.
should_call_for_facts(_,F,_):- a(prologDynamic,F),!.
should_call_for_facts(_,F,_):- \+ a(pfcControlled,F),!.




a(C,I):-umt(((current_predicate(C/1),call(C,I)))).



%% is_relative( :TermV) is semidet.
%
% If Is A Relative.
%
is_relative(V):- (\+is_ftCompound(V)),!,fail.
is_relative(update(_)).
is_relative(replace(_)).
is_relative(rel(_)).
is_relative(+(X)):- \+ is_ftVar(X).
is_relative(-(X)):- \+ is_ftVar(X).
is_relative(*(X)):- \+ is_ftVar(X).



% ======================= 
% user''s program''s database
% ======================= 

%% update_single_valued_arg(+Module, +P, ?N) is semidet. 
%
% Update Single Valued Argument.
%
:- module_transparent( (update_single_valued_arg)/3).

update_single_valued_arg(M,M:Pred,N):-!,update_single_valued_arg(M,Pred,N).

update_single_valued_arg(world,P,N):- !, update_single_valued_arg(baseKB,P,N).
update_single_valued_arg(M,P,N):- \+ clause_b(mtHybrid(M)), clause_b(mtHybrid(M2)),!,update_single_valued_arg(M2,P,N).

update_single_valued_arg(M,P,N):- 
  get_unnegated_head_arg(N,P,UPDATE),
  is_relative(UPDATE),!,
  dtrace,
  replace_arg(P,N,OLD,Q),
  must_det_l((clause_u(Q),update_value(OLD,UPDATE,NEW),\+ is_relative(NEW), replace_arg(Q,N,NEW,R))),!,
  update_single_valued_arg(M,R,N).


update_single_valued_arg(M,P,N):- 
 umt((must_det_l((

  umt(mtHybrid(M)),
  mpred_type_args \= M,
  mpred_kb_ops \= M,
  get_unnegated_head_arg(N,P,UPDATE),
  replace_arg(P,N,Q_SLOT,Q),
  var(Q_SLOT),
  same_functors(P,Q),
  % get_source_ref1(U),
  must_det_l((
     % rtrace(attvar_op(assert_if_new,M:spft(MZ,P,U,ax))),
     % (umt(P)->true;(op_dir_mu(assert,assertz,P))),
     op_dir_mu(assert,assertz,M,P),
     doall((
          lookup_u(M:Q,E),
          UPDATE \== Q_SLOT,
          op_dir_mu(retract,erase(E),M,Q),         
          mpred_unfwc1(M:Q))))))))).


% ======================= 
% user''s program''s source reason
% ======================= 


%% get_source_ref( :TermU) is det.
%
% Get Source Ref (Current file or User)
%
:- module_transparent((get_source_ref)/1).
get_source_ref(O):- quietly_ex((current_why(U),(U=(_,_)->O=U;O=(U,ax)))),!.
get_source_ref(O):- quietly_ex((get_source_ref1(U),(U=(_,_)->O=U;O=(U,ax)))),!.

get_source_ref_stack(O):- findall(U,current_why(U),Whys),Whys\==[],!, U=(_,_),(Whys=[U]->O=U;O=(Whys,ax)),!.
get_source_ref_stack(O):- get_source_ref1(U),(U=(_,_)->O=U;O=(U,ax)),!.

get_startup_uu((mfl4(VarNameZ,baseKB, user_input, _), ax)):-true.

is_user_reason((_,U)):-atomic(U).

is_user_fact(P):-get_first_user_reason(P,UU),is_user_reason(UU).

lookup_spft(MZ,P,F,T):- umt(spft(MZ,P,F,T)).

get_first_real_user_reason(P,UU):- nonvar(P), UU=(F,T),
  quietly_ex((  ((((lookup_spft(MZ,P,F,T))),is_user_reason(UU))*-> true;
    ((((lookup_spft(MZ,P,F,T))), \+ is_user_reason(UU))*-> (!,fail) ; fail)))).

get_first_user_reason(P,(F,T)):-
  UU=(F,T),
  ((((lookup_spft(MZ,P,F,T))),is_user_reason(UU))*-> true;
    ((((lookup_spft(MZ,P,F,T))), \+ is_user_reason(UU))*-> (!,fail) ;
       (clause_asserted_u(P),get_source_ref(UU),is_user_reason(UU)))),!.
get_first_user_reason(_,UU):-get_source_ref_stack(UU),is_user_reason(UU),!.
get_first_user_reason(_,UU):- get_source_ref_stack(UU),!.
get_first_user_reason(P,UU):- must(ignore(((get_first_user_reason0(P,UU))))),!.
get_first_user_reason0(_,(M,ax)):-get_source_ref10(M).

%get_first_user_reason(_,UU):- get_source_ref(UU),\+is_user_reason(UU). % ignore(get_source_ref(UU)).

%% get_source_ref1(+Mt) is semidet.
%
% Get Source Ref Secondary Helper.
%
:- module_transparent((get_source_ref1)/1).
:- module_transparent((get_source_ref10)/1).
% get_source_ref1(M):- atom(M),must((get_source_ref10(N),atom(N))),!,M=N.
get_source_ref1(M):- ground(M),!.
get_source_ref1(M):- get_source_ref10(M),!.
get_source_ref1(_).

get_source_ref10(M):- current_why(M), nonvar(M) , M =mfl4(VarNameZ,_,_,_).
get_source_ref10(mfl4(VarNameZ,M,F,L)):- defaultAssertMt(M), source_location(F,L),unifyable_varname(.

get_source_ref10(mfl4(VarNameZ,M,F,L)):- defaultAssertMt(M), current_source_file(F:L),prolog_load_context(variable_names,VarNameZ).
get_source_ref10(mfl4(VarNameZ,M,F,_L)):- defaultAssertMt(M), current_source_file(F).
get_source_ref10(mfl4(VarNameZ,M,_F,_L)):- defaultAssertMt(M).
%get_source_ref10(M):- (defaultAssertMt(M)->true;(atom(M)->(module_property(M,class(_)),!);(var(M),module_property(M,class(_))))).
get_source_ref10(M):- fail,dtrace,
 ((defaultAssertMt(M) -> !;
 (atom(M)->(module_property(M,class(_)),!);
    mpred_error(no_source_ref(M))))).

is_source_ref1(_).


% ======================= 
% user''s program''s database
% ======================= 
%clause_u(X,Y,Z):-umt(clause(X,Y,Z)).
clause_u(A,B,C):- umt(clause_i(A,B,C)).
%clause_u(X,Y):-umt(clause(X,Y)).
clause_u(A,B):- umt(clause_i(A,B)).
clause_u(A):- clause_i(A).
clause_asserted_u(A) :- clause_asserted_i(A).


%% assert_u(:X) is det.
%% asserta_u(:X) is det.
%% assertz_u(:X) is det.
%
% Assert For User Code.
%
assert_u(MH):-  fix_mp(clause(assert,assert_u),MH,M,H),get_unnegated_functor(H,F,A),assert_mu_fa(M,H,F,A).
asserta_u(MH):- fix_mp(clause(assert,asserta_u),MH,M,H),op_dir_mu(assert,asserta,M,H).
assertz_u(MH):- fix_mp(clause(assert,assertz_u),MH,M,H),op_dir_mu(assert,assertz,M,H).

%% asserta_mu(+M, ?X) is det.
%% assertz_mu(+M, ?X) is det.
%% assertz_mu(+M, ?X) is det.
%
% Assert(a/z/?) Module Unit.
%
assert_mu(M,P):- get_unnegated_functor(P,F,A),assert_mu_fa(M,P,F,A).
asserta_mu(M,P):- op_dir_mu(assert,asserta,M,P).
assertz_mu(M,P):- op_dir_mu(assert,assertz,M,P).


% :- kb_shared(baseKB:singleValuedInArg/2).
:- thread_local(t_l:assert_to/1).

%% assert_mu_fa(+Module, +Pred, +Functor, +Arity) is semidet.
%
% Assert For User Code 
% (Functor/Arity) on Module helps resolve specially typed predicates
%
assert_mu_fa(M,M2:Pred,F,A):- M == M2,!, assert_mu(M,Pred,F,A).
assert_mu_fa(M,_:Pred,F,A):- dtrace,sanity(\+ is_ftVar(Pred)),!, assert_mu(M,Pred,F,A).

%assert_mu_fa(M,Pred,F,_):- clause_b(singleValuedInArg(F,SV)),!,must(update_single_valued_arg(M,Pred,SV)),!.
%assert_mu_fa(M,Pred,F,A):- a(prologSingleValued,F),!,must(update_single_valued_arg(M,Pred,A)),!.

assert_mu_fa(M,Pred,F,_):- a(prologOrdered,F),!,op_dir_mu(assert,assertz,M,Pred).
assert_mu_fa(M,Pred,_,_):- t_l:assert_to(Where),!, (Where = a -> op_dir_mu(assert,asserta,M,Pred); op_dir_mu(assert,assertz,M,Pred)).
assert_mu_fa(M,Pred,_,1):- !, op_dir_mu(assert,assertz,M,Pred),!.
assert_mu_fa(M,Pred,_,_):- op_dir_mu(assert,asserta,M,Pred).


%% retract_u(:TermX) is nondet.
%
% Retract For User Code.
%
retract_u(MH):- fix_mp(clause(retract,retracta),MH,M,H),retract_mu(M,H).
retractall_u(MH):- fix_mp(clause(retract,retractall),MH,M,H),retractall_mu(M,H).

retractall_mu(M,PRED):- expand_to_hb(PRED,H,_), (PRED\==H ->retract_all_mu(M,H);retract_all_mu(M,PRED)).

retract_all_mu(M,H):- retract_mu(M,H),fail.
retract_all_mu(_).

%% retract_mu(M, :TermX) is semidet.
%
% Retract For User Code in Module.
%
retract_mu(M,(H:-B)):-!,clause_u(H,B,R),op_dir_mu(retract,erase(R),M,(H:-B)).
retract_mu(M,(H)):- clause_u(H,true,R),op_dir_mu(retract,erase(R),M,H).

%retract_mu(M,~(X)):-!,show_success(why,retract_eq_quitely_f(~(X))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).
%retract_mu(M,(X)):-!,show_success(why,retract_eq_quitely_f((X))),must((expire_tabled_list(~(X)))),must((expire_tabled_list((X)))).


:- baseKB:multifile(baseKB:mtExact/1).
:- baseKB:dynamic(baseKB:mtExact/1).
:- baseKB:export(baseKB:mtExact/1).


% ============================================

%% correct_module( ?M, ?X, ?T) is semidet.
%
% Correct Module.
%
correct_module(M,G,T):-functor(G,F,A),quietly_must(correct_module(M,G,F,A,T)),!.

%% correct_module( ?M, ?Goal, ?F, ?A, ?T) is semidet.
%
% Correct Module.
%
correct_module(abox,G,F,A,T):- !, defaultAssertMt(M),correct_module(M,G,F,A,T).
correct_module(tbox,G,F,A,T):- !, fileAssertMt(M),correct_module(M,G,F,A,T).
correct_module(user,G,F,A,T):- fail,!,defaultAssertMt(M),correct_module(M,G,F,A,T).
correct_module(HintMt,_,_,_,HintMt):-!.  % TODO KEEP IT SIMPLE

correct_module(HintMt,Goal,F,A,OtherMt):-var(Goal),functor(Goal,F,A),!,correct_module(HintMt,Goal,F,A,OtherMt).
correct_module(HintMt,Goal,_,_,OtherMt):- predicate_property(HintMt:Goal,imported_from(OtherMt)).
correct_module(_,Goal,_,_,OtherMt):- predicate_property(Goal,imported_from(OtherMt)).
correct_module(HintMt,_,_,_,HintMt):- a(mtExact,HintMt).
correct_module(HintMt,Goal,_,_,HintMt):- predicate_property(HintMt:Goal,exported).
correct_module(_,Goal,_,_,OtherMt):- var(OtherMt),!, predicate_property(OtherMt:Goal,file(_)).
correct_module(_,Goal,_,_,OtherMt):- a(mtGlobal,OtherMt), predicate_property(OtherMt:Goal,file(_)).
correct_module(MT,_,_,_,MT):-!.

% =============================================================
%                 SIDE EFFECT PROCESSING
%
% TODO ISSUE https://github.com/logicmoo/PrologMUD/issues/7
% =============================================================


%% pfc_nochaining( +Goal) is semidet.
%
% PFC No Chaining.
%
pfc_nochaining(Goal):- locally(t_l:no_attempt_side_effects,call(Goal)).


%% pfc_with_chaining( +Goal) is semidet.
%
% PFC No Chaining.
%
pfc_with_chaining(Goal):- locally(- t_l:no_attempt_side_effects,call(Goal)).



%% op_dir_mu(+Op,+Type,+M, ?X) is det.
%
% Op(Assert/Retract) in Ordered in Module Unit.
%
:- module_transparent(op_dir_mu/4).
op_dir_mu(Op,Type,M,spft(MZ,P,mfl4(VarNameZ,KB,F,L),T)):-M\==KB,!,op_dir_mu(Op,Type,KB,spft(MZ,P,mfl4(VarNameZ,KB,F,L),T)).
op_dir_mu(Op,Type,M,X):- correct_module(M,X,T),T\==M,!,op_dir_mu(Op,Type,T,X).
op_dir_mu(Op,Type,M,M2:Pred):- sanity(M == M2),!, op_dir_mu(Op,Type,M,Pred).
op_dir_mu(Op,Type,M,X):- 
    strip_module(X,_,P), 
    sanity(Op==assert->check_never_assert(M:P);check_never_retract(M:P)), 
    must((expire_tabled_list(M:P),
    must(attvar_op(db_op_call(Op,Type),M:P)))).


:- module_transparent(attvar_op/2).

% % attvar_op(Op,Data):- deserialize_attvars(Data,Data0), attvar_op(Op,Data0).
attvar_op(Op,MData):-
 must_det_l((
   strip_module(Op,_,OpA), sanity( \+ atom(OpA)),
   fix_mp(clause(Op,OpA),MData,M,Data),
   add_side_effect(OpA,M:Data),
   quietly(current_prolog_flag(assert_attvars,true)->deserialize_attvars(Data,Data0);Data=Data0))),!,
   attempt_side_effect_mpa(M,OpA,Data0).


:- thread_local((t_l:use_side_effect_buffer , t_l:verify_side_effect_buffer)).

%% record_se is semidet.
%
% Record Se.
%
record_se:- (t_l:use_side_effect_buffer ; t_l:verify_side_effect_buffer).



%% add_side_effect( +Op, ?Data) is semidet.
%
% Add Side Effect.
%
add_side_effect(_,_):- ( \+  record_se ),!.
add_side_effect(Op,Data0):-get_source_ref1(Why),serialize_attvars(Data0,Data),assert(t_l:side_effect_buffer(Op,Data,Why)).



:- meta_predicate(db_op_call(*,*,?)).
db_op_call(_What,erase(E),_Data):- !, erase(E).
db_op_call(_What,How,Data):- call(How,Data).

% attempt_side_effect_mpa(M,OpA,Data):- record_se,!,add_side_effect(OpA,M:Data).
attempt_side_effect_mpa(M,db_op_call(_,retract_u0),Data0):- \+ lookup_u(M:Data0),!,fail.
attempt_side_effect_mpa(M,OpA,Data0):- \+ record_se, is_side_effect_disabled,!,mpred_warn('no_attempt_side_effects ~p',attempt_side_effect_mpa(M,OpA,Data0)).
% @TODO BROKEN phys ical_side_effect_call(M,assertz_i,Data0):- must((compile_aux_clauses(M:Data0))),!.
attempt_side_effect_mpa(M,OpA,Data0):- show_failure(M:call(M:OpA,M:Data0)).










:- thread_local(t_l:no_attempt_side_effects/0).

%% attempt_side_effect( +PSE) is semidet.
%
% Physical Side Effect.
%
attempt_side_effect(PSE):- to_physical_mpa(PSE,M,P,A),!,attempt_side_effect_mpa(M,P,A).

to_physical_mpa(PSE,M,P,A):- strip_module(PSE,M,PA),to_physical_pa(PA,P,A).
to_physical_pa(PA,P,A):-PA=..[P,A],!. to_physical_pa(PA,call,PA).


/*

  b_setval(th_asserts,[]),
  umt(G),
  b_getval(th_asserts,List).

attempt_side_effect_mpa(C) :- 
   b_getval(th_asserts,List),
   b_setval(th_asserts,[C|List]),!.



*/



%% no_side_effects( +P) is semidet.
%
% No Side Effects.
%
no_side_effects(P):-  (\+ is_side_effect_disabled->true;(get_functor(P,F,_),a(prologSideEffects,F))).



:-thread_local(t_l:side_effect_ok/0).
%% is_side_effect_disabled is semidet.
%
% If Is A Side Effect Disabled.
%
is_side_effect_disabled:- t_l:no_attempt_side_effects,!.
is_side_effect_disabled:- t_l:side_effect_ok,!,fail.
is_side_effect_disabled:- t_l:noDBaseMODs(_),!.



:- fixup_exports.


% prolog:message(ignored_weak_import(Into, From:PI))--> { nonvar(Into),Into \== system,dtrace(dmsg(ignored_weak_import(Into, From:PI))),fail}.
% prolog:message(Into)--> { nonvar(Into),functor_safe(Into,_F,A),A>1,arg(1,Into,N),\+ number(N),dtrace(wdmsg(Into)),fail}.

/*
:- multifile(user:clause_expansion/2).
user:clause_expansion(I,O):- pfc_clause_expansion(I,O).
*/

/*
:- multifile(clause_expansion/2).
clause_expansion(I,O):- pfc_clause_expansion(I,O).
*/


% term_expansion(I,P1,O,P2):- is_pfc_file,mpred_te(term,system,I,P1,O,P2).

module_uses_pfc(SM):- current_predicate(SM:'$uses_pfc_toplevel'/0).

:- multifile(pfc_goal_expansion/4).
:- dynamic(pfc_goal_expansion/4).
:- module_transparent(pfc_goal_expansion/4).
pfc_goal_expansion(I,P,O,PO):- 
 quietly(( \+ source_location(_,_),
     callable(I),          
     var(P), % Not a file goal     
     \+ current_prolog_flag(xref,true), 
     \+ current_prolog_flag(mpred_te,false),
     '$current_typein_module'(CM),
     prolog_load_context(module,SM),
     ((SM \== CM) -> module_uses_pfc(SM); module_uses_pfc(CM)), 
     (I \= (CM : call_u(_))), (I \= call_u(_)))),
     fully_expand(I,M),
     % quietly
     ((
     O=CM:call_u(M),
     PO=P)).


saveBaseKB:- tell(baseKB),listing(baseKB:_),told.

%baseKB:'==>'(Consq) :- sanity( \+ input_from_file), ain('==>'(Consq)),!.
%baseKB:'==>'(Ante,Consq):- sanity( \+ input_from_file), mpred_why_2(Consq,Ante).

:- set_prolog_flag(subclause_expansion,false).

:- fixup_exports.


:- if(exists_source(library(retry_undefined))).

:- use_module(library(retry_undefined)).
:- install_retry_undefined(baseKB,kb_shared).

:- else.

:- endif.

:- multifile(system:clause_expansion/2).
:- module_transparent(system:clause_expansion/2).
:- system:import(pfc_clause_expansion/2).
system:clause_expansion(I,O):- pfc_clause_expansion(I,O).


%:- set_prolog_flag(read_attvars,false).
:- set_prolog_flag(subclause_expansion,true).
:- set_prolog_flag(mpred_te,true).

:- retractall(t_l:disable_px).


:- multifile(system:goal_expansion/4).
:- module_transparent(system:goal_expansion/4).
:- system:import(pfc_goal_expansion/4).
system:goal_expansion(I,P,O,PO):- pfc_goal_expansion(I,P,O,PO).


:- set_prolog_flag(mpred_te,true).
:- set_prolog_flag(retry_undefined, module).


