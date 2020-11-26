

:- module(kb7166s,
   [ from_qlfs/0,kb7166s_info/0,kb7166s_qconsult/0, kb7166s_qcompile/0,qconsult/2,lmcache:kb7166s_qconsulted/0]).

% :- unload_file(kb7166s).

:- multifile(mdep/1).

:- set_module(class(test)).
:- use_module(library(statistics),[time/1]).
:- use_module(library(check)).

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
    kb7166s:['dir.header'],
    kb7166s:export(mdep/1),
    user:import(kb7166s:mdep/1),
    kb7166s:[kb7166s_assertions_00],
    expand_file_name('supports_each_*.qlf',QLFS),
    maplist(kb7166s:ensure_loaded,QLFS).

needs_mdep(deduction_7166/3).

:- forall(needs_mdep(P),'@'(kb7166s:do_mdep(P),kb7166s)).

kb7166s_info:- forall(needs_mdep(P),kb7166s_info(P)).
kb7166s_info(M:F/A):- !, kb7166s_info(M,F,A).
kb7166s_info(F/A):- !, kb7166s_info(kb7166s,F,A).
kb7166s_info(M:P):- !,functor(P,F,A),kb7166s_info(M,F,A).
kb7166s_info(P):- !,functor(P,F,A),kb7166s_info(kb7166s,F,A).
kb7166s_info(M,F,A):- functor(P,F,A), % findall(PP,(predicate_property(M:P,PP),atom(PP)),O),
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

kb7166s_qcompile:- fail,  kb_dir(From),cd(From), KB= kb7166s, expand_file_name('src~/supports_each_*.pl',List),
  maplist( [E] >> (module(KB), '$set_source_module'(KB), qcompile(E,[register(false),
       derived_from('src~/unseen'),module(KB),
       redefine_module(false),qcompile(part)])),List),!.
% kb7166s_qcompile:-!.
kb7166s_qcompile:- kb_dir(From),atom_concat(From,'/src~/supports_each_*.pl',Filter),  
   expand_file_name(Filter,List),
   writeq(List),
   maplist(qcompile_if_missing(kb7166s),List),!.

:- dynamic(lmcache:kb7166s_qconsulted/0).
:- export(lmcache:kb7166s_qconsulted/0).

kb7166s_qconsult:- lmcache:kb7166s_qconsulted,!.
kb7166s_qconsult:- 
   asserta(lmcache:kb7166s_qconsulted),
   kb_dir(From),atom_concat(From,'/src~/supports_each_*.qlf',Filter),
   expand_file_name(Filter,List),
   maplist(qload,List),!.

qload(File):- kb7166s:load_files(File, [if(not_loaded),register(false),redefine_module(false),qcompile(part)]).

% loaded later
% :- qconsult(kb7166s_pt7_constant_renames).

% needs regen
% :- qconsult(kb7166s_pt8_supports.pl).
