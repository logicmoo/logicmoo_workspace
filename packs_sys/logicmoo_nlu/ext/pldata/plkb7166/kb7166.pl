

:- module(kb7166,
   [ from_qlfs/0,kb7166_info/0,kb7166_qconsult/0, kb7166_qcompile/0,qconsult/2,lmcache:kb7166_qconsulted/0]).

% :- unload_file(kb7166).

:- multifile(mdep/1).

:- set_module(class(test)).
:- use_module(library(statistics),[time/1]).

do_mdep(FA):-
  '$current_source_module'(M),
  '$current_typein_module'(CM),
  M:multifile(FA),  
  M:dynamic(FA),
  M:export(FA),
  CM:import(M:FA),
  CM:export(CM:FA),
  M:public(FA).




mdep(FA):-do_mdep(FA),!.
from_qlfs:-
    multifile(mdep/1),
    kb7166:['dir.header'],
    kb7166:export(mdep/1),
    user:import(kb7166:mdep/1),
    kb7166:[kb7166_asse00],
    expand_file_name('kb7166_asse*.qlf',QLFS),
    maplist(kb7166:ensure_loaded,QLFS).

needs_mdep(assertion_content_not/3).
needs_mdep(assertion_content_implies/3).

needs_mdep(assertion_abbreviation/1).
needs_mdep(assertion_abnormal/1).
needs_mdep(assertion_asserted/1).
needs_mdep(assertion_asserted_by/2).
needs_mdep(assertion_asserted_when/2).
needs_mdep(assertion_assertion_has_meta_assertion/1).
needs_mdep(assertion_assertive_wff/1).
needs_mdep(assertion_atomic/1).
needs_mdep(assertion_backward/1).
needs_mdep(assertion_backward_rule_required_for/1).
needs_mdep(assertion_bookkeeping/1).
needs_mdep(assertion_code/1).
needs_mdep(assertion_computed_skolem/1).
needs_mdep(assertion_considered_but_not_successful/1).
needs_mdep(assertion_constraint/1).
needs_mdep(assertion_content/3).
needs_mdep(assertion_content/4).
needs_mdep(assertion_content/5).
needs_mdep(assertion_content/6).
needs_mdep(assertion_content/7).
needs_mdep(assertion_content/8).
needs_mdep(assertion_content/9).
needs_mdep(assertion_content/10).
needs_mdep(assertion_content/11).
needs_mdep(assertion_content/12).
needs_mdep(assertion_content/13).
needs_mdep(assertion_contextually_dependent_lexical/1).
needs_mdep(assertion_cva/1).
needs_mdep(assertion_deduced/1).
needs_mdep(assertion_default/1).
needs_mdep(assertion_dependants/1).
needs_mdep(assertion_documentation/1).
needs_mdep(assertion_fact/1).
needs_mdep(assertion_false/1).
needs_mdep(assertion_fast_documentation/1).
needs_mdep(assertion_forward/1).
needs_mdep(assertion_forward_tms/1).
needs_mdep(assertion_gen_template_query_sentence/1).
needs_mdep(assertion_ground/1).
needs_mdep(assertion_gtqs/1).
needs_mdep(assertion_has_dependents/1).
needs_mdep(assertion_has_genstring/1).
needs_mdep(assertion_has_meta/1).
needs_mdep(assertion_has_smeta/1).
needs_mdep(assertion_higher_order/1).
needs_mdep(assertion_implementation/1).
needs_mdep(assertion_indirect_lexical/1).
needs_mdep(assertion_inert/1).
needs_mdep(assertion_inference_relevant/1).
needs_mdep(assertion_lifting/1).
needs_mdep(assertion_lifting/2).
needs_mdep(assertion_lifting_consequent/2).
needs_mdep(assertion_looks_like_kappa_rephrase/1).
needs_mdep(assertion_meta/1).
needs_mdep(assertion_monotonic/1).
needs_mdep(assertion_mt/2).
needs_mdep(assertion_needs_review/1).
needs_mdep(assertion_nl_trie_syntactic/1).
needs_mdep(assertion_non_abducible/1).
needs_mdep(assertion_not_assertible/1).
needs_mdep(assertion_not_exportable/1).
needs_mdep(assertion_not_first_order/1).
needs_mdep(assertion_not_non_tva_gaf/1).
needs_mdep(assertion_not_true/1).
needs_mdep(assertion_pph_required/1).
needs_mdep(assertion_reformulator_relevant/1).
needs_mdep(assertion_relevant/1).
needs_mdep(assertion_rule/1).
needs_mdep(assertion_rule_meta/1).
needs_mdep(assertion_self_expanding/1).
needs_mdep(assertion_self_looping/1).
needs_mdep(assertion_single_literal/1).
needs_mdep(assertion_single_literal_antecedent/1).
needs_mdep(assertion_skolem/1).
needs_mdep(assertion_some_suggested_pragma_sentences_for/1).
needs_mdep(assertion_syntactically_ill_formed/1).
needs_mdep(assertion_syntactically_illformed/2).
needs_mdep(assertion_tva/1).
needs_mdep(assertion_two_literal_antecedent/1).
needs_mdep(assertion_unbound/1).
needs_mdep(assertion_universal_lifting/1).
needs_mdep(assertion_uses_non_extensional_set/1).
needs_mdep(assertion_variable_guard/2).
needs_mdep(assertion_varnames/2).
needs_mdep(assertion_vars/2).
needs_mdep(assertion_wrapper/3).

:- forall(needs_mdep(P),'@'(kb7166:do_mdep(P),kb7166)).

kb7166_info:- forall(needs_mdep(P),kb7166_info(P)).
kb7166_info(M:F/A):- !, kb7166_info(M,F,A).
kb7166_info(F/A):- !, kb7166_info(kb7166,F,A).
kb7166_info(M:P):- !,functor(P,F,A),kb7166_info(M,F,A).
kb7166_info(P):- !,functor(P,F,A),kb7166_info(kb7166,F,A).
kb7166_info(M,F,A):- functor(P,F,A), % findall(PP,(predicate_property(M:P,PP),atom(PP)),O),
  predicate_property(M:P,number_of_clauses(NC)),
  findall(B,predicate_property(M:P, indexed(B)),C),
  format('~N~n~q:~q/~q = ~w ~w~N',[M,F,A,NC,-]),
  (C==[] -> true ;
   foreach(member(E,C),(format('~`=t~68|~n'),maplist(prolog_jiti:print_secondary_index,E)))).


qlf_is_missing_or_old(File):- 
   absolute_file_name(File,Name),
   file_name_extension(Base,_Ext,Name),
   file_name_extension(Base,'qlf',QName),
   (\+ exists_file(QName)-> true
   ;(
    file_name_extension(Base,'pl',PName),
    exists_file(PName),
    time_file(PName,PTime),
    time_file(QName,QTime),
    QTime<PTime)).

qlf_name(File,QName):-
   absolute_file_name(File,Name),
   file_name_extension(Base,_Ext,Name),
   file_name_extension(Base,'qlf',QName).

source_file_of_qlf(QName,PName):- directory_file_path(_,File,QName),qlf_name(File,'pl',PName).

qlf_name(File,NewExt,QName):-
   file_name_extension(Base,_Ext,File),
   file_name_extension(Base,NewExt,QName).

   
qcompile_if_missing(_,File,QName):- \+ qlf_is_missing_or_old(File),!,qlf_name(File,QName).
qcompile_if_missing(M,File,QName):- % source_file_of_qlf(File,PName),!,writeq(source_file_of_qlf(File,PName)),nl,
  M:qcompile(File, [if(not_loaded),derived_from('src~/unseen'),module(M),redefine_module(false)]),nop(qlf_name(File,QName)).

qcompile_if_missing(M,File):- qcompile_if_missing(M,File,_).

qconsult(M,File):- compiling,!, M:load_files(File, [if(not_loaded),register(false),derived_from('src~/unseen'),module(M),redefine_module(false),qcompile(part)]).
qconsult(M,File):- time((writeln(qconsult(M,File)),
   must(qcompile_if_missing(M,File,QName)),
   M:load_files(QName, [if(not_loaded),module(M),register(false),redefine_module(false)]))).

:- dynamic(kb_dir/1).
:- prolog_load_context(directory,From),asserta(kb_dir(From)).

kb7166_qcompile:- kb_dir(From),cd(From),
  KB= kb7166,
 expand_file_name('src~/kb7166_asse*.pl',List),
  maplist( [E] >> (module(KB), '$set_source_module'(KB), qcompile(E,[register(false),
       derived_from('src~/unseen'),module(KB),
       redefine_module(false),qcompile(part)])),List),!.
% kb7166_qcompile:-!.
kb7166_qcompile:- 
   kb_dir(From),
   atom_concat(From,'/src~/kb7166_asse*.pl',Filter),  
   expand_file_name(Filter,List),
   writeq(List),
   maplist(qcompile_if_missing(kb7166),List),!.

:- dynamic(lmcache:kb7166_qconsulted/0).
:- export(lmcache:kb7166_qconsulted/0).

kb7166_qconsult:- lmcache:kb7166_qconsulted,!.
kb7166_qconsult:- 
   asserta(lmcache:kb7166_qconsulted),
   kb_dir(From),atom_concat(From,'/src~/kb7166_asse*.qlf',Filter),
   expand_file_name(Filter,List),
   maplist(qload,List),!.

qload(File):- kb7166:load_files(File, [if(not_loaded),register(false),redefine_module(false),qcompile(part)]).

% loaded later
% :- qconsult(kb7166_pt7_constant_renames).

% needs regen
% :- qconsult(kb7166_pt8_supports.pl).
