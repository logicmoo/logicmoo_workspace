/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

/*  

%fav(v('20818e16'),[guess_bg,indiv(min(2))]).

%(fav(_,P)/(flatten([P],Flat),member(E,Flat))) ==> fav_trait(E).

*/
:- dynamic(kaggle_arc_json/4).

%:- system:reconsult(library(lists)).
%:- listing(lists:'$seek_list'/4).
%:- break.


% Docker VM shoudl contain a /data directory
mask_to_fullnames(Mask,FullNames):- absolute_file_name(Mask,AbsMask,[relative_to('/data')]), 
  expand_file_name(AbsMask,FullNames),FullNames\==[].
mask_to_fullnames(Mask,FullNames):- AbsMask = Mask, expand_file_name(AbsMask,Nonmask),Nonmask\==[],
  my_maplist(absolute_file_name,Nonmask,FullNames).
mask_to_fullnames(Mask,FullNames):-
  arc_sub_path('.',ARC_DIR), member(Rel,['.','/','/data',ARC_DIR]), absolute_file_name(Mask,AbsMask,[relative_to(Rel)]),
  expand_file_name(AbsMask,FullNames),FullNames\==[].
mask_to_fullnames(Mask,FullNames):-
  arc_sub_path('.',ARC_DIR), 
  member(Rel,[ARC_DIR,'/data','.','/']),
  absolute_file_name(Mask,AbsMask,[relative_to(Rel)]),
  expand_file_name(AbsMask,FullNames),FullNames\==[].

  

:- export(load_json_files/3).
load_json_files(SuiteX,F,Mask):-
  (SuiteX=='';var(SuiteX)),
  dir_to_suitename(Mask,SuiteName),
  SuiteName\==SuiteX,!,
  load_json_files(SuiteName,F,Mask).

load_json_files(SuiteX,F,Mask):- 
  Info = [test_suite=SuiteX,loadmask=Mask],
  asserta_if_new(dir_test_suite_name(SuiteX)),
  %wdmsg(Info),
  locally(t_l:local_test_props(Info), 
    load_json_files(F,Mask)).

/*:- export(load_json_files/2).
load_json_files(F,Mask):- 
  mask_to_fullnames(Mask,FullNames),  FullNames\==[],
  u_dmsg(load_json_files(F,Mask)),
  my_maplist(load_json_files(F),FullNames),!.
*/
no_uscore(UBaseName,BaseName):- 
  atomic_list_concat(List,'_',UBaseName),
  atomic_list_concat(List,'-',BaseName).

dir_to_suitename(Mask,SuiteName):- 
  split_string(Mask, "\s\t\n\\/.", "\s\t\n\\/.", L),
  %wdmsg(dir_to_suitename(Mask->L)),
  my_include(interesting_pathname,L,IP),
  atomic_list_concat(IP,'-',SuiteName).
interesting_pathname(S):- string(S), atom_string(A,S),!,interesting_pathname(A).
interesting_pathname(N):- \+ dumb_pathname(N).
dumb_pathname('*'). dumb_pathname('.').  dumb_pathname('json').
dumb_pathname('corpus'). dumb_pathname('data'). dumb_pathname(''). dumb_pathname('/').
dumb_pathname('Data'). dumb_pathname('*.json').
dumb_pathname('opt'). dumb_pathname('logicmoo_workspace'). dumb_pathname('packs_sys').
dumb_pathname('logicmoo_agi'). dumb_pathname('prolog'). dumb_pathname('kaggle_arc').
/*
expand_to_abs_file_name_list(FName,ABSFullNames):- 
  \+ exists_file(FName), expand_file_name(FName,FullNames),FullNames\=@=[FName],
  my_maplist(afn_maybe,FullNames,ABSFullNames),
  last(ABSFullNames,Exist),
  exists_file(Exist),!.
afn_maybe(A,A).
load_json_files(F, FName):-  pp(load_json_files(F)=FName),fail.
load_json_files(F, FName):- is_list(FName),!,my_maplist(load_json_files(F), FName).
load_json_files(F, FName):- \+ exists_file(FName), !, expand_to_abs_file_name_list(FName,FullNames),load_json_files(F, FullNames).
%load_json_files(F, FName):- exists_file(FName),\+ is_absolute_file_name(FName), absolute_file_name(FName,ABSName),FName\==ABSName,!,load_json_file_abs(F, ABSName).
load_json_files(F, FName):- \+ is_absolute_file_name(FName), absolute_file_name(FName,ABSName),FName\=@=ABSName,!,load_json_file_abs(F, ABSName).
load_json_files(F, FName):- load_json_file_abs(F, FName),!.
*/
%load_json_files(F, FName):-  pp(load_json_files(F)=FName),fail.

:- abolish(is_expanded_file_name/2).
:- dynamic(is_expanded_file_name/2).
expand_json_files(_F,PathMasked):- 
  is_expanded_file_name(PathMasked,FullNames),!,
  length(FullNames,Len),
  nop(wdmsg(already_did(PathMasked=Len))),!.
expand_json_files(F,PathMasked):-
  expand_file_name(PathMasked,FullNames),FullNames\=@=[PathMasked],!,
  assert(is_expanded_file_name(PathMasked,FullNames)),
  my_partition(file_not_dir,FullNames,Files,Dirs),
  length(Files,NFiles),length(Dirs,NDirs),
  if_t(NFiles\==0, wdmsg(files(PathMasked)=NFiles)),
  nop((if_t(NDirs\==0, wdmsg(dirs(PathMasked)=NDirs)))),
  (FullNames==[] -> true ; load_json_files(F, FullNames)),!.

file_not_dir(File):- atom(File), \+ exists_directory(File),exists_file(File).
load_json_files(F, FName):- is_list(FName),!,my_maplist(load_json_files(F), FName).
load_json_files(F, File):- atomic(File), exists_file(File), !, load_json_file(F,File).
load_json_files(F,Dir):- atomic(Dir), exists_directory(Dir),!,
  dir_to_suitename(Dir,SuiteName), 
  directory_file_path(Dir,'*',NewMask),
  load_json_files(SuiteName,F,NewMask).

load_json_files(F,PathMasked):- atomic(PathMasked), atom_contains(PathMasked,'*'), 
  atomic_list_concat([Node|Nodes],'*',PathMasked),directory_file_path(Dir,ShortedFileMask,Node),
  \+ exists_directory(Dir),
  arc_sub_path(Dir,RealDir), exists_directory(RealDir),
  directory_file_path(RealDir,ShortedFileMask,NewShortedFileMask),
  atomic_list_concat([NewShortedFileMask|Nodes],'*',FullNewMask),
  FullNewMask\==PathMasked,!, expand_json_files(F,FullNewMask).

load_json_files(F, PathMasked):- (\+ atomic(PathMasked); \+ exists_file(PathMasked); atom_contains(PathMasked,'*')), !,
  expand_json_files(F,PathMasked).

%load_json_files(F, FName):- exists_file(FName),\+ is_absolute_file_name(FName), absolute_file_name(FName,ABSName),FName\==ABSName,!,load_json_file(F, ABSName).
load_json_files(F, FName):- load_json_file(F, FName),!.


%load_json_file(F, FName):- atomic(FName), \+ is_absolute_file_name(FName), 
%  absolute_file_name(FName,ABSName),FName\=@=ABSName,!,load_json_file(F, ABSName),!.

%load_json_file(F, PathMasked):- (\+ atomic(PathMasked); \+ exists_file(PathMasked); atom_contains(PathMasked,'*')), 
%  expand_json_files(F,PathMasked).


load_json_file(_F,PathMasked):- atomic(PathMasked), \+ atom_concat(_,'.json',PathMasked),!,
  wdmsg(skipping_file(PathMasked)),!.
 
load_json_file(F, FullName):- 
  %pp(load_json_file(F)=FullName),!,
  must_det_ll((
  file_base_name(FullName,FileBaseName),
  file_name_extension(UName,_,FileBaseName),
  no_uscore(UName,Name), 
  Testname=..[F,Name], 
  % dmsg(load_json_files=FullName),

  setup_call_cleanup(open(FullName,read,In), json:json_read(In,Term,[]), close(In)),

  locally(t_l:local_test_props(filename=FullName),
    (load_json_of_file(Testname,file,Term),
     ignore((
      kaggle_arc(Testname,_,_,_), 
      add_test_info(Testname),
      add_testfile_name(Testname,FullName))))))),!.


add_testfile_name(Testname,FullName):- 
  ignore(( 
  directory_file_path(Dir,_,FullName),
  dir_to_suitename(Dir,SuiteName),
  %split_string(FullName, "\\/",'./',L),append(_,[Dir,_],L),
  %atom_string(ADir,Dir),!,
  %ADir\==[],
  add_test_info_prop(Testname,test_suite,SuiteName),
  (SuiteName==solution -> true ; asserta_if_new(dir_test_suite_name(SuiteName))))).


add_test_info(Name):- forall(t_l:local_test_props(Props),add_test_info_props(Name,Props)).


add_test_info_props(Name,RestInfo):- is_list(RestInfo),!, my_maplist(add_test_info_props(Name),RestInfo).
add_test_info_props(Name,F=V):- !, add_test_info_prop(Name,F,V).
add_test_info_props(Name,FV):- compound_name_arguments(FV,F,V),add_test_info_prop(Name,F,V),!.
add_test_info_props(Name,TV):- assert_if_new(some_test_info_prop(Name,TV)),u_dmsg(fallback_some_test_info_prop(Name,TV)).

:- multifile(muarc_tmp:some_test_info_prop/2).
:- dynamic(muarc_tmp:some_test_info_prop/2).

add_test_info_prop(Name,F,V):- var(V),!,u_dmsg(var_add_test_info_prop(Name,F,V)).
add_test_info_prop(Name,F,[]):- NV=..[F,_], \+ \+ muarc_tmp:some_test_info_prop(Name,NV),!.
add_test_info_prop(Name,F,[V]):- !,add_test_info_prop(Name,F,V).

%add_test_info_prop(Testname,test_suite,ADir):- 
% wdmsg(add_test_info_prop(Testname,test_suite,ADir)),fail.
  
add_test_info_prop(Name,F,V):-  
  must_det_ll((
    NV=..[F,[]],ignore(retract(muarc_tmp:some_test_info_prop(Name,NV))), 
   n_v_to_nv(F,[V],TV),
   assert_if_new(muarc_tmp:some_test_info_prop(Name,TV)),
   nop(u_dmsg(muarc_tmp:some_test_info_prop(Name,TV))))).

:- thread_local(t_l:local_test_props/1).


  load_json_predictions(Name,Rest,json(Term)):- !, load_json_predictions(Name,Rest,Term).
  load_json_predictions(Name,Rest,Term):- %u_dmsg(load_json_predictions(Name,Rest,Term)),
    select(output_id=ID,Rest,RestInfo),
    select(prediction_id=AnswerID0,Term,TermInfo0),    
     AnswerID is AnswerID0 + 10,
    select(output=G,TermInfo0,TermInfo1),
    add_test_info_props(Name,TermInfo1),
    add_test_info_props(Name,RestInfo),
    json_to_colors(G,Grid), 
    add_prediction(Name,ID,AnswerID,Grid).

  load_json_predictions(Name,Rest,Term):- u_dmsg(munused_load_json_predictions(Name,Rest,Term)),!.


 add_prediction(Name,ID,AnswerID,_Grid):- assert_if_new(kaggle_arc_answers(Name,ID,AnswerID,_BadGrid)).


  load_json_of_file(_, Atom+_, []):- atom(Atom),!.
  load_json_of_file(Name,Type,json(Term)):-! , load_json_of_file(Name,Type,Term).
  
  load_json_of_file(FName,Type,Term):- select(task_name=Name,Term,Rest),!,
   must_det_ll((FName=..[F|Info], TestID=..[F,Name],         
         add_test_info(TestID), 
         add_test_info_props(TestID,info(Info)),
         load_json_of_file(TestID,Type,Rest))).

  load_json_of_file(Name,Type,[id=_|T]):- !, load_json_of_file(Name,Type,T).
  load_json_of_file(FName,Type,Term):- select(id=Name,Term,Rest), atom(Name), atom_length(Name,Len),Len>1,
    must_det_ll((FName=..[F|Info], TestID=..[F,Name],
        locally(t_l:local_test_props(Info), add_test_info(TestID)),
        locally(t_l:local_test_props(Rest),
          load_json_of_file(TestID,Type,Rest)))).

  %load_json_of_file(FName,Type,Term):- select(id=Name,Term,Rest),functor(FName,F,_), TestID=..[FName,Name],!, load_json_of_file(TestID,Type,Rest).

  load_json_of_file(TestID,_Type,[Test=Out]):- Test==test, !, load_json_of_file(TestID,Test,Out).

  load_json_of_file(Name, _Type,List):- select(predictions=Items,List,Rest),!,
    add_test_info_props(Name,Rest),
    my_maplist(load_json_predictions(Name,Rest),Items).

  load_json_of_file(Name,_,Type=Value):-!, load_json_of_file(Name,Type,Value).
  load_json_of_file(TestID,train,T):-!,load_json_of_file(TestID,trn,T).
  load_json_of_file(TestID,test,T):-!,load_json_of_file(TestID,tst,T).


  
  load_json_of_file(Name,_Type,[train =In, test=Out]):- load_json_of_file(Name,train,In),load_json_of_file(Name,test,Out).
  load_json_of_file(Name,Type,[input=In,output=Out]):- assert_kaggle_arc_json(Name,Type,In,Out),!.
  load_json_of_file(Name,Type,[input=In]):-assert_kaggle_arc_json(Name,Type,In,_Out),!.

  load_json_of_file(Name,A,V):- atom(A),atomic(V),!,add_test_info_prop(Name,A,V).

  load_json_of_file(Name,Type,[H|T]):- atom(Type),is_list(T),!,forall(nth00(N,[H|T],E), 
      load_json_of_file(Name,Type+N,E)).    

  load_json_of_file(Name,A,V):- u_dmsg(miss_load_json_of_file(Name,A,V)),!, add_test_info_prop(Name,A,V).

assert_kaggle_arc_json(Name,Type,In0,Out0):- 
  json_to_colors(In0,In), json_to_colors(Out0,Out),!, 
  assert_kaggle_arc_json_now(Name,Type,In,Out).
  

%assert_kaggle_arc_json_now(Name,Type,In,Out):- kaggle_arc_json(Name,Type,In,OutO),Out=OutO,!.
assert_kaggle_arc_json_now(Name,Type,In,Out):- 
  retractall(kaggle_arc_json(Name,Type,_In,_Out)),
  into_solid_grid(In,InS),
  into_solid_grid(Out,OutS),
  assert_if_new(kaggle_arc_json(Name,Type,InS,OutS)),
  add_test_info(Name).

:- dynamic(kaggle_arc_answers/4).

:- multifile(dir_test_suite_name/1).
:- dynamic(dir_test_suite_name/1).

n_v_to_nv(T,V,TV):- atom(T),TV=..[T,V],!.
n_v_to_nv(T,V,TV):- TV=(T=V),!.

nth00(N,HT,E):- integer(N),!,length(Left,N),append(Left,[E|_Right],HT).
nth00(N,HT,E):- append(Left,[E|_Right],HT), length(Left,N).
nth11(N,HT,E):- integer(N),!, N2 is N-1, nth00(N2,HT,E).
nth11(N,HT,E):- nth00(N2,HT,E),N2 is N+1.

nth00(N,HT,E,Rest):- integer(N),!,length(Left,N),append(Left,[E|Right],HT),append(Left,Right,Rest).
nth00(N,HT,E,Rest):- append(Left,[E|Right],HT), length(Left,N),append(Left,Right,Rest).
nth11(N,HT,E,Rest):- integer(N),!, N2 is N-1, nth00(N2,HT,E,Rest).
nth11(N,HT,E,Rest):- nth00(N2,HT,E,Rest),N2 is N+1.

%ma:attr_unify_hook(

%cell(color,type,objects,

% json_to_colors(Out,Color):- is_grid_color(Out),!,Out=Color.
:- export(json_to_colors/2).
json_to_colors(Out,Color):- is_list(Out),!,my_maplist(json_to_colors,Out,Color).
json_to_colors(Out,Color):- grid_color_code(Out,Color).


:- dynamic(muarc_tmp:arc_code_directory/1).

enum_arc_directory(ARC_DIR):- 
  no_repeats_var(NRDIR),
  enum_arc_directory_1(DIR),exists_directory(DIR),
  once((absolute_file_name(DIR,ABSDIR),NRDIR=ABSDIR,ARC_DIR=ABSDIR)).

enum_arc_directory_1(ARC_DIR):- getenv('ARC_DIR',ARC_DIR), ARC_DIR\==''. 
enum_arc_directory_1(ARC_DIR):- muarc_tmp:arc_code_directory(CODE_DIR),
    absolute_file_name('../shared/',ARC_DIR,[relative_to(CODE_DIR),access(read),file_errors(fail),
      expand(true), file_type(directory),solutions(first)]).
enum_arc_directory_1(ARC_DIR):- member(Rel,['/data','.']), 
  absolute_file_name('.',AbsMask,[relative_to(Rel)]),
  expand_file_name(AbsMask,FullNames),member(ARC_DIR,FullNames).
enum_arc_directory_1(ARC_DIR):- muarc_tmp:arc_code_directory(ARC_DIR).
enum_arc_directory_1(ARC_DIR):- working_directory(ARC_DIR,ARC_DIR).
enum_arc_directory_1('/').

:- multifile (user:file_search_path/2).
user:file_search_path(arc,  AbsolutePath):- arc_sub_path('.',AbsolutePath).

:- prolog_load_context(directory,ARC_DIR), asserta(muarc_tmp:arc_code_directory(ARC_DIR)).

%test_name_ansi_output_file(TestID,File):- absolute_file_name(TestID,File,[access(read),file_errors(fail)]),!.
%test_name_ansi_output_file(TestID,File):- absolute_file_name(TestID,File,[access(create),file_errors(fail)]),!.


absolute_dir_or_file_name(ARC_DIR,Subdir,AbsolutePath):- 
  catch(absolute_file_name(Subdir,AbsolutePath,[relative_to(ARC_DIR),expand(true),
    file_type(directory),solutions(first),file_errors(fail),access(read)]),_,fail),!.
absolute_dir_or_file_name(ARC_DIR,Subdir,AbsolutePath):- 
  absolute_file_name(Subdir,AbsolutePath,[relative_to(ARC_DIR),expand(true),
    file_type(regular),solutions(first),file_errors(fail),access(read)]).

absolute_dir_or_file_name(ARC_DIR,Subdir,AbsolutePath):- 
  catch(absolute_file_name(Subdir,AbsolutePath,[relative_to(ARC_DIR),expand(true),
    file_type(directory),solutions(first),file_errors(fail),access(none)]),_,fail),!.
absolute_dir_or_file_name(ARC_DIR,Subdir,AbsolutePath):- 
  absolute_file_name(Subdir,AbsolutePath,[relative_to(ARC_DIR),expand(true),
    file_type(regular),solutions(first),file_errors(fail),access(none)]).

existing_arc_sub_paths(Subdir,AbsolutePath):- 
  no_repeats_var(NRAbsolutePath),
  enum_arc_directory(ARC_DIR), 
    once(absolute_dir_or_file_name(ARC_DIR,Subdir,AbsolutePathTry)), exists_file_or_dir(AbsolutePathTry),
    NRAbsolutePath = AbsolutePathTry, AbsolutePath=AbsolutePathTry.

arc_sub_path(Subdir,AbsolutePath):-
 no_repeats_var(NRAbsolutePath),
  ((existing_arc_sub_paths(Subdir,AbsolutePathTry)*->true;
  once((enum_arc_directory(ARC_DIR), absolute_dir_or_file_name(ARC_DIR,Subdir,AbsolutePathTry))))),
  NRAbsolutePath = AbsolutePathTry, AbsolutePath=AbsolutePathTry.

:- export(arc_sub_path/2).

load_deval:- fail.

load_json_files1:- load_json_files(train400,t,'../shared/data/training/*.json').
load_json_files1:- load_json_files(eval400,v,'../shared/data/devaluation/*.json').
%:- load_json_files(v,'../shared/data/test_100/*.json').
%:- load_json_files(t,'../shared/data/test_nar_10x10/*.json').
load_json_files1:- load_json_files(t,'../shared/data/1D_testset/').
load_json_files1:- load_json_files(t,'../ARC-Others/dbigham/Data/MyTrainingData/').
%load_json_files1:- load_json_files(t,'../shared/data/ConceptARC/corpus/*/*/').
load_json_files1:- load_json_files(t,'../shared/data/ConceptARC/corpus/').

%load_json_files1:- load_json_files('object_modifications_schema',t,'./object_modifications_schema/tasks/*.json').
%:- load_json_files(v,'../../secret_data/solu**66/*.json').
%:- load_json_files(v,'../../secret_data/evaluation/*.json').
%load_json_files1:- load_deval, load_json_files('secret_data_evaluation',v,'/data/evaluation/*.json').
%load_json_files1:- load_json_files('secret_data_solution',v,'/secret_data/solution/*.json').
% % % load_json_files1:- load_json_files('secret_data_solution',v,'/data/solution/*.json').

%load_json_files2:- load_json_files(michod,t,'./arc-task-generator/conditional_transforms_schema/tasks/0*.json').
load_json_files1:- load_json_files('MiniARC',t,'../ARC-Others/MINI-ARC/data/MiniARC/*.json').

load_json_files:- 
  forall(load_json_files1,true).

%:- load_json_files(v,'../shared/data/test/*.json').
:- export(kaggle_arc/4).

:- thread_local(t_l:encoder/1).

with_raw_encoder(Goal):- with_encoder(raw,Goal).

with_encoder(Enc,Goal):- locally(t_l:encoder(Enc),Goal).

:- dynamic(is_encode_tst/1).

is_encode_tst(A):- simp_encode(A).
is_encode_tst(A):- ensure_test(A), fail.

is_output_simple(TestID):- fail, foreach_test((TestID), 
  \+ \+ (get_raw_input_outputs(TestID,_ExampleNums,_Ins,Outs), simple_reencode(_,Outs,EOuts),Outs\==EOuts)).

is_input_simple(TestID):- fail, foreach_test((TestID), 
  \+ \+ (get_raw_input_outputs(TestID,_ExampleNums,Ins,_Outs),  simple_reencode(_,Ins,EIns),Ins\==EIns)).

is_each_pair_use_simple(_TestID):- fail.

simp_encode(t('5582e5ca')).
simp_encode(t('0d3d703e')).

get_raw_input_outputs(TestID,ExampleNums,Ins,Outs):-
  (var(TestID)-> is_encode_tst(TestID);true),
  findall(ExampleNum,kaggle_arc_raw(TestID,ExampleNum,In,Out),ExampleNums),
  findall(In,kaggle_arc_raw(TestID,ExampleNum,In,Out),Ins),
  findall(Out,kaggle_arc_raw(TestID,ExampleNum,In,Out),Outs).

:- ensure_loaded('../logical_ml/muarc_dmiles').
%kaggle_arc(TestID,ExampleNum,In,Out):- kaggle_arc_raw(TestID,ExampleNum,In,Out).
kaggle_arc(TestID,ExampleNum,In,Out):- kaggle_arc_json(TestID,ExampleNum,In,OutM),
  check_output_grid(TestID,ExampleNum,In,OutM,Out).

check_output_grid(_TestID,_ExampleNum,_In,OutM,Out):- is_grid(OutM),Out=OutM.

/*kaggle_arc(TestID,ExampleNum,In,Out):- var(In),var(Out),!,
  kaggle_arc_raw(TestID,ExampleNum,In0,Out0),
  duplicate_term(In0,In),
  duplicate_term(Out0,Out).
  */
/*  
kaggle_arc(TestID,ExampleNum,In,Out):- nonvar(In),var(Out),!,
  duplicate_term(In,In0),
  kaggle_arc_raw(TestID,ExampleNum,In0,Out0),  
  duplicate_term(Out0,Out).
kaggle_arc(TestID,ExampleNum,In,Out):- var(In),nonvar(Out),!,
  duplicate_term(Out,Out0),
  kaggle_arc_raw(TestID,ExampleNum,In0,Out0),  
  duplicate_term(In0,In).
kaggle_arc(TestID,ExampleNum,In,Out):- nonvar(In),nonvar(Out),!,
  duplicate_term(In,In0),
  duplicate_term(Out,Out0),
  kaggle_arc_raw(TestID,ExampleNum,In0,Out0).
*/

/*
kaggle_arc(TestID,ExampleNum,In,Out):-
  kaggle_arc_raw(TestID,ExampleNum,In0,Out0),
  % in private impl of muarc_dmiles
  maybe_reencode(TestID,ExampleNum,In0,Out0,In1,Out1), In1=In, Out1=Out.
*/
maybe_reencode(TestID,ExampleNum,In0,Out0,In,Out):-
 (t_l:encoder(Enc)->Enc\==raw),!,call(Enc,TestID,ExampleNum,In0,Out0,In,Out).
maybe_reencode(_TName,_ExampleNum,In,Out,In,Out).


kaggle_arc_raw(TestID,ExampleNum,In,Out):- !, kaggle_arc_json(TestID,ExampleNum,In,Out).
%kaggle_arc_raw(TestID,ExampleNum,In,Out):- kaggle_arc0(TestID,ExampleNum,In,Out)*-> true ; kaggle_arc1(TestID,ExampleNum,In,Out).
%kaggle_arc_raw(Name,tst+AnswerID,In,Grid):- kaggle_arc_answers(Name,ID,AnswerID,Grid), kaggle_arc0(Name,tst+ID,In,_Out).

kaggle_arc0(TestID,ExampleNum,In,Out):- kaggle_arc_json(TestID,ExampleNum,In,O), not_disallow_test_out(ExampleNum,O,Out).
kaggle_arc1(TestID,ExampleNum,In,Out):- nonvar(ExampleNum),
  kaggle_arc0(TestID,NewExample,In,Out),!,
  ignore((\+ \+ nb_current(example,ExampleNum),  nb_setval(example,NewExample))).

   
not_disallow_test_out(_,OO,OO):- !.
not_disallow_test_out(_,OO,OO):- allow_peeking,!.

not_disallow_test_out(trn+_,OO,OO):-!.
not_disallow_test_out(tst+_, O,OO):- grid_size(O,H,V),make_grid(H,V,OO),mapgrid(=(black),OO).


tasks_split(ID,String):- split_string(String,",[] \n\r\t\s",",[] \n\r\t\s",L),member(S,L),atom_string(E,S),atom_id_e(E,ID).

icu(evaluation,'00576224',fail).
icu(evaluation,'009d5c81',fail).
icu(evaluation,'00dbd492',fail).
icu(evaluation,'03560426',fail).
icu(evaluation,'05a7bcf2',fail).
icu(evaluation,'0607ce86',fail).
icu(evaluation,'0692e18c',pass).
icu(evaluation,'070dd51e',pass).
icu(evaluation,'08573cc6',fail).
icu(evaluation,'0934a4d8',fail).
icu(evaluation,'09c534e7',fail).
icu(evaluation,'0a1d4ef5',fail).
icu(evaluation,'0a2355a6',fail).
icu(evaluation,'0b17323b',fail).
icu(evaluation,'0bb8deee',pass). % depth 4
icu(evaluation,'0becf7df',fail).
icu(evaluation,'0c786b71',pass).
icu(evaluation,'0c9aba6e',pass).
icu(evaluation,'0d87d2a6',fail).
icu(evaluation,'0e671a1a',fail).
icu(evaluation,'0f63c0b9',pass).
icu(evaluation,'103eff5b',fail).
icu(evaluation,'11e1fe23',fail).
icu(evaluation,'12422b43',fail).
icu(evaluation,'12997ef3',pass).
icu(evaluation,'12eac192',pass).
icu(evaluation,'136b0064',fail).
icu(evaluation,'13713586',fail).
icu(evaluation,'137f0df0',pass).
icu(evaluation,'140c817e',pass).
icu(evaluation,'14754a24',fail).
icu(evaluation,'15113be4',fail).
icu(evaluation,'15663ba9',fail).
icu(evaluation,'15696249',pass). % depth 2
icu(evaluation,'16b78196',fail).
icu(evaluation,'17b80ad2',pass).
icu(evaluation,'17cae0c1',fail).
icu(evaluation,'18419cfa',pass).
icu(evaluation,'184a9768',fail).
icu(evaluation,'195ba7dc',pass).
icu(evaluation,'1990f7a8',fail).
icu(evaluation,'19bb5feb',pass).
icu(evaluation,'1a2e2828',pass).
icu(evaluation,'1a6449f1',pass).
icu(evaluation,'1acc24af',fail).
icu(evaluation,'1c02dbbe',pass).
icu(evaluation,'1c0d0a4b',pass).
icu(evaluation,'1c56ad9f',fail).
icu(evaluation,'1d0a4b61',pass).
icu(evaluation,'1d398264',pass).
icu(evaluation,'1da012fc',fail).
icu(evaluation,'1e81d6f9',pass).
icu(evaluation,'1e97544e',fail).
icu(evaluation,'2037f2c7',fail).
icu(evaluation,'2072aba6',pass).
icu(evaluation,'20818e16',pass).
icu(evaluation,'20981f0e',fail).
icu(evaluation,'212895b5',fail).
icu(evaluation,'21f83797',pass).
icu(evaluation,'22a4bbc2',fail).
icu(evaluation,'25094a63',fail).
icu(evaluation,'2546ccf6',pass). % depth 4
icu(evaluation,'256b0a75',fail).
icu(evaluation,'2685904e',fail).
icu(evaluation,'2697da3f',fail).
icu(evaluation,'2753e76c',pass). % depth 4
icu(evaluation,'27a77e38',pass).
icu(evaluation,'27f8ce4f',pass).
icu(evaluation,'281123b4',pass).
icu(evaluation,'292dd178',fail).
icu(evaluation,'29700607',fail).
icu(evaluation,'2a5f8217',fail).
icu(evaluation,'2b01abd0',fail).
icu(evaluation,'2c0b0aff',pass). % depth 2
icu(evaluation,'2c737e39',fail).
icu(evaluation,'2f0c5170',fail).
icu(evaluation,'310f3251',fail).
icu(evaluation,'3194b014',pass).
icu(evaluation,'319f2597',pass).
icu(evaluation,'31adaf00',fail).
icu(evaluation,'31d5ba1a',pass).
icu(evaluation,'32e9702f',pass).
icu(evaluation,'332efdb3',pass).
icu(evaluation,'3391f8c0',fail).
icu(evaluation,'33b52de3',fail).
icu(evaluation,'3490cc26',fail).
icu(evaluation,'34b99a2b',pass).
icu(evaluation,'351d6448',fail).
icu(evaluation,'358ba94e',pass).
icu(evaluation,'37d3e8b2',fail).
icu(evaluation,'3979b1a8',pass).
icu(evaluation,'3a301edc',fail).
icu(evaluation,'3b4c2228',pass).
icu(evaluation,'3d31c5b3',pass).
icu(evaluation,'3ed85e70',fail).
icu(evaluation,'3ee1011a',fail).
icu(evaluation,'3f23242b',fail). 
icu(evaluation,'40f6cd08',fail).
icu(evaluation,'414297c0',fail).
icu(evaluation,'423a55dc',fail).
icu(evaluation,'42918530',fail).
icu(evaluation,'42a15761',fail).
icu(evaluation,'4364c1c4',fail).
icu(evaluation,'456873bc',fail).
icu(evaluation,'45737921',pass). % depth 2
icu(evaluation,'45bbe264',pass).
icu(evaluation,'477d2879',fail).
icu(evaluation,'47996f11',pass).
icu(evaluation,'48131b3c',pass).
icu(evaluation,'4852f2fa',pass).
icu(evaluation,'48f8583b',pass).
icu(evaluation,'4aab4007',pass).
icu(evaluation,'4acc7107',fail).
icu(evaluation,'4b6b68e5',fail).
icu(evaluation,'4c177718',fail).
icu(evaluation,'4cd1b7b2',pass).
icu(evaluation,'4e45f183',fail).
icu(evaluation,'4e469f39',fail).
icu(evaluation,'4f537728',pass).
icu(evaluation,'4ff4c9da',fail).
icu(evaluation,'505fff84',fail).
icu(evaluation,'506d28a5',pass).
icu(evaluation,'50a16a69',pass).
icu(evaluation,'50aad11f',pass). % depth 4
icu(evaluation,'50f325b5',fail).
icu(evaluation,'516b51b7',pass).
icu(evaluation,'5207a7b5',fail).
icu(evaluation,'5289ad53',fail).
icu(evaluation,'52fd389e',fail).
icu(evaluation,'54db823b',pass).
icu(evaluation,'55059096',fail).
icu(evaluation,'551d5bf1',fail).
icu(evaluation,'55783887',fail).
icu(evaluation,'575b1a71',fail).
icu(evaluation,'5783df64',pass).
icu(evaluation,'5833af48',pass). % depth 4
icu(evaluation,'58743b76',fail).
icu(evaluation,'58e15b12',fail).
icu(evaluation,'59341089',pass).
icu(evaluation,'5a5a2103',pass).
icu(evaluation,'5af49b42',fail).
icu(evaluation,'5b526a93',pass).
icu(evaluation,'5b692c0f',fail).
icu(evaluation,'5b6cbef5',pass).
icu(evaluation,'5d2a5c43',pass).
icu(evaluation,'5ffb2104',pass).
icu(evaluation,'604001fa',fail).
icu(evaluation,'60a26a3e',pass).
icu(evaluation,'60c09cac',pass).
icu(evaluation,'626c0bcc',fail).
icu(evaluation,'62ab2642',pass). % depth 2
icu(evaluation,'62b74c02',pass).
icu(evaluation,'639f5a19',pass).
icu(evaluation,'642248e4',fail).
icu(evaluation,'642d658d',pass).
icu(evaluation,'64a7c07e',fail).
icu(evaluation,'66e6c45b',pass).
icu(evaluation,'66f2d22f',pass).
icu(evaluation,'67636eac',pass).
icu(evaluation,'67b4a34d',fail).
icu(evaluation,'67c52801',fail).
icu(evaluation,'68b67ca3',pass).
icu(evaluation,'692cd3b6',fail).
icu(evaluation,'695367ec',pass).
icu(evaluation,'696d4842',fail).
icu(evaluation,'69889d6e',pass). % depth 4
icu(evaluation,'6a11f6da',pass).
icu(evaluation,'6ad5bdfd',fail).
icu(evaluation,'6df30ad6',pass).
icu(evaluation,'6ea4a07e',pass).
icu(evaluation,'6f473927',pass).
icu(evaluation,'7039b2d7',pass).
icu(evaluation,'705a3229',fail).
icu(evaluation,'712bf12e',fail).
icu(evaluation,'72207abc',fail).
icu(evaluation,'72a961c9',fail).
icu(evaluation,'73182012',pass).
icu(evaluation,'73c3b0d8',fail).
icu(evaluation,'73ccf9c2',fail).
icu(evaluation,'759f3fd3',fail).
icu(evaluation,'762cd429',fail).
icu(evaluation,'770cc55f',pass).
icu(evaluation,'782b5218',pass).
icu(evaluation,'79369cc6',fail).
icu(evaluation,'7953d61e',pass).
icu(evaluation,'79fb03f4',fail).
icu(evaluation,'7bb29440',pass).
icu(evaluation,'7c8af763',fail).
icu(evaluation,'7c9b52a0',pass).
icu(evaluation,'7d18a6fb',fail).
icu(evaluation,'7d1f7ee8',pass).
icu(evaluation,'7d419a02',fail).
icu(evaluation,'7e02026e',fail).
icu(evaluation,'7ee1c6ea',pass). 
icu(evaluation,'817e6c09',fail).
icu(evaluation,'81c0276b',pass).
icu(evaluation,'833dafe3',pass).
icu(evaluation,'845d6e51',fail).
icu(evaluation,'84db8fc4',pass).
icu(evaluation,'84f2aca1',fail).
icu(evaluation,'8597cfd7',pass).
icu(evaluation,'85b81ff1',fail).
icu(evaluation,'85fa5666',fail).
icu(evaluation,'8719f442',fail).
icu(evaluation,'88207623',fail).
icu(evaluation,'891232d6',fail).
icu(evaluation,'896d5239',fail).
icu(evaluation,'8a371977',pass).
icu(evaluation,'8b28cd80',fail).
icu(evaluation,'8ba14f53',pass).
icu(evaluation,'8cb8642d',fail).
icu(evaluation,'8dae5dfc',fail).
icu(evaluation,'8e2edd66',pass).
icu(evaluation,'8ee62060',pass).
icu(evaluation,'8fbca751',fail).
icu(evaluation,'90347967',fail).
icu(evaluation,'903d1b4a',pass).
icu(evaluation,'9110e3c5',pass). % depth 2
icu(evaluation,'917bccba',pass).
icu(evaluation,'929ab4e9',pass).
icu(evaluation,'92e50de0',fail).
icu(evaluation,'9356391f',fail).
icu(evaluation,'93b4f4b3',fail).
icu(evaluation,'93c31fbe',pass).
icu(evaluation,'94133066',pass).
icu(evaluation,'94414823',pass).
icu(evaluation,'94be5b80',fail).
icu(evaluation,'95a58926',pass).
icu(evaluation,'963f59bc',fail).
icu(evaluation,'96a8c0cd',fail).
icu(evaluation,'97239e3d',fail).
icu(evaluation,'9772c176',fail).
icu(evaluation,'981571dc',pass).
icu(evaluation,'992798f6',fail).
icu(evaluation,'99306f82',fail).
icu(evaluation,'9a4bb226',pass).
icu(evaluation,'9b2a60aa',fail).
icu(evaluation,'9b365c51',fail).
icu(evaluation,'9b4c17c4',fail).
icu(evaluation,'9bebae7a',fail).
icu(evaluation,'9c1e755f',pass).
icu(evaluation,'9c56f360',fail).
icu(evaluation,'9caba7c3',fail).
icu(evaluation,'9ddd00f0',fail).
icu(evaluation,'9def23fe',pass).
icu(evaluation,'9f27f097',pass). % depth 4
icu(evaluation,'a04b2602',fail).
icu(evaluation,'a096bf4d',fail).
icu(evaluation,'a3f84088',fail).
icu(evaluation,'a406ac07',pass).
icu(evaluation,'a57f2f04',fail).
icu(evaluation,'a59b95c0',pass).
icu(evaluation,'a680ac02',pass).
icu(evaluation,'a8610ef7',fail).
icu(evaluation,'a934301b',pass). % depth 2
icu(evaluation,'aa18de87',pass).
icu(evaluation,'aa300dc3',fail).
icu(evaluation,'aa4ec2a5',pass).
icu(evaluation,'aab50785',fail).
icu(evaluation,'ac0c5833',fail).
icu(evaluation,'ac2e8ecf',fail).
icu(evaluation,'ac3e2b04',fail).
icu(evaluation,'ac605cbb',fail).
icu(evaluation,'ad7e01d0',pass).
icu(evaluation,'ae58858e',pass).
icu(evaluation,'aee291af',fail).
icu(evaluation,'af22c60d',pass).
icu(evaluation,'af24b4cc',pass).
icu(evaluation,'b0722778',fail).
icu(evaluation,'b0f4d537',fail).
icu(evaluation,'b15fca0b',fail).
icu(evaluation,'b1fc8b8e',pass).
icu(evaluation,'b20f7c8b',fail).
icu(evaluation,'b457fec5',fail).
icu(evaluation,'b4a43f3b',fail).
icu(evaluation,'b7999b51',fail).
icu(evaluation,'b7cb93ac',fail).
icu(evaluation,'b7f8a4d8',pass).
icu(evaluation,'b7fb29bc',fail).
icu(evaluation,'b942fd60',fail).
icu(evaluation,'b9630600',fail).
icu(evaluation,'ba9d41b8',pass).
icu(evaluation,'baf41dbf',fail).
icu(evaluation,'bb52a14b',fail).
icu(evaluation,'bbb1b8b6',pass).
icu(evaluation,'bc4146bd',pass).
icu(evaluation,'bcb3040b',fail).
icu(evaluation,'bd14c3bf',fail).
icu(evaluation,'be03b35f',pass).
icu(evaluation,'bf32578f',pass).
icu(evaluation,'bf699163',fail).
icu(evaluation,'bf89d739',fail).
icu(evaluation,'c074846d',fail).
icu(evaluation,'c1990cce',fail).
icu(evaluation,'c3202e5a',pass).
icu(evaluation,'c35c1b4c',pass).
icu(evaluation,'c48954c1',pass).
icu(evaluation,'c62e2108',fail).
icu(evaluation,'c64f1187',fail).
icu(evaluation,'c658a4bd',fail).
icu(evaluation,'c663677b',pass).
icu(evaluation,'c6e1b8da',fail).
icu(evaluation,'c7d4e6ad',pass).
icu(evaluation,'c87289bb',fail).
icu(evaluation,'c8b7cc0f',pass).
icu(evaluation,'c92b942c',pass).
icu(evaluation,'c97c0139',fail).
icu(evaluation,'ca8de6ea',pass).
icu(evaluation,'ca8f78db',pass).
icu(evaluation,'cad67732',fail).
icu(evaluation,'cb227835',fail).
icu(evaluation,'ccd554ac',pass).
icu(evaluation,'cd3c21df',pass).
icu(evaluation,'ce039d91',pass).
icu(evaluation,'ce8d95cc',pass).
icu(evaluation,'cf133acc',pass).
icu(evaluation,'cfb2ce5a',fail).
icu(evaluation,'d017b73f',fail).
icu(evaluation,'d19f7514',pass).
icu(evaluation,'d282b262',fail).
icu(evaluation,'d2acf2cb',fail).
icu(evaluation,'d304284e',fail).
icu(evaluation,'d37a1ef5',pass).
icu(evaluation,'d47aa2ff',fail).
icu(evaluation,'d492a647',pass).
icu(evaluation,'d4b1c2b1',pass).
icu(evaluation,'d4c90558',fail).
icu(evaluation,'d56f2372',pass).
icu(evaluation,'d5c634a2',fail).
icu(evaluation,'d931c21c',fail).
icu(evaluation,'d94c3b52',fail).
icu(evaluation,'da2b0fe3',pass).
icu(evaluation,'da515329',fail).
icu(evaluation,'dc2aa30b',fail).
icu(evaluation,'dc2e9a9d',fail).
icu(evaluation,'dd2401ed',fail).
icu(evaluation,'de493100',fail).
icu(evaluation,'df8cc377',fail).
icu(evaluation,'e0fb7511',pass).
icu(evaluation,'e133d23d',pass).
icu(evaluation,'e1baa8a4',pass).
icu(evaluation,'e1d2900e',fail).
icu(evaluation,'e2092e0c',fail).
icu(evaluation,'e21a174a',fail).
icu(evaluation,'e345f17b',pass).
icu(evaluation,'e4075551',fail).
icu(evaluation,'e41c6fd3',pass).
icu(evaluation,'e57337a4',pass). % depth 4
icu(evaluation,'e5790162',fail).
icu(evaluation,'e5c44e8f',fail).
icu(evaluation,'e619ca6e',fail).
icu(evaluation,'e633a9e5',pass).
icu(evaluation,'e66aafb8',fail).
icu(evaluation,'e681b708',fail).
icu(evaluation,'e69241bd',pass).
icu(evaluation,'e6de6e8f',fail).
icu(evaluation,'e74e1818',pass).
icu(evaluation,'e760a62e',fail).
icu(evaluation,'e7639916',pass).
icu(evaluation,'e78887d1',fail).
icu(evaluation,'e7a25a18',pass).
icu(evaluation,'e7b06bea',fail).
icu(evaluation,'e7dd8335',pass).
icu(evaluation,'e872b94a',pass).
icu(evaluation,'e88171ec',fail).
icu(evaluation,'e95e3d8e',pass).
icu(evaluation,'e99362f0',pass). % depth 4
icu(evaluation,'e9ac8c9e',fail).
icu(evaluation,'e9b4f6fc',fail).
icu(evaluation,'e9bb6954',fail).
icu(evaluation,'e9c9d9a1',pass).
icu(evaluation,'ea959feb',pass).
icu(evaluation,'ea9794b1',pass).
icu(evaluation,'ecaa0ec1',pass).
icu(evaluation,'ed74f2f2',fail).
icu(evaluation,'ed98d772',pass).
icu(evaluation,'ef26cbf6',pass).
icu(evaluation,'f0afb749',pass).
icu(evaluation,'f0df5ff0',pass).
icu(evaluation,'f21745ec',fail).
icu(evaluation,'f3b10344',fail).
icu(evaluation,'f3cdc58f',fail).
icu(evaluation,'f3e62deb',fail).
icu(evaluation,'f4081712',fail).
icu(evaluation,'f45f5ca7',fail).
icu(evaluation,'f5aa3634',fail).
icu(evaluation,'f5c89df1',fail).
icu(evaluation,'f823c43c',pass).
icu(evaluation,'f83cb3f6',pass).
icu(evaluation,'f8be4b64',fail).
icu(evaluation,'f9a67cb5',fail).
icu(evaluation,'f9d67f8b',fail).
icu(evaluation,'fafd9572',fail).
icu(evaluation,'fb791726',pass).
icu(evaluation,'fc754716',pass).
icu(evaluation,'fd096ab6',fail).
icu(evaluation,'fd4b2b02',fail).
icu(evaluation,'fe9372f3',fail).
icu(evaluation,'fea12743',fail).
icu(evaluation,'ff72ca3e',fail).

icu(training,'007bbfb7',pass).
icu(training,'00d62c1b',pass).
icu(training,'017c7c7b',pass).
icu(training,'025d127b',pass).
icu(training,'045e512c',fail).
icu(training,'0520fde7',pass).
icu(training,'05269061',pass).
icu(training,'05f2a901',fail).
icu(training,'06df4c85',pass).
icu(training,'08ed6ac7',fail).
icu(training,'09629e4f',pass).
icu(training,'0962bcdd',pass).
icu(training,'0a938d79',fail).
icu(training,'0b148d64',pass).
icu(training,'0ca9ddb6',fail).
icu(training,'0d3d703e',fail).
icu(training,'0dfd9992',pass).
icu(training,'0e206a2e',fail).
icu(training,'10fcaaa3',fail).
icu(training,'11852cab',fail).
icu(training,'1190e5a7',pass).
icu(training,'137eaa0f',fail).
icu(training,'150deff5',fail).
icu(training,'178fcbfb',pass).
icu(training,'1a07d186',fail).
icu(training,'1b2d62fb',pass).
icu(training,'1b60fb0c',fail).
icu(training,'1bfc4729',pass).
icu(training,'1c786137',pass).
icu(training,'1caeab9d',fail).
icu(training,'1cf80156',pass).
icu(training,'1e0a9b12',pass).
icu(training,'1e32b0e9',pass).
icu(training,'1f0c79e5',pass).
icu(training,'1f642eb9',fail).
icu(training,'1f85a75f',pass).
icu(training,'1f876c06',pass).
icu(training,'1fad071e',pass).
icu(training,'2013d3e2',pass).
icu(training,'2204b7a8',pass).
icu(training,'22168020',pass).
icu(training,'22233c11',fail).
icu(training,'2281f1f4',pass).
icu(training,'228f6490',pass).
icu(training,'22eb0ac0',pass).
icu(training,'234bbc79',fail).
icu(training,'23581191',pass). % depth 2
icu(training,'239be575',pass).
icu(training,'23b5c85d',pass).
icu(training,'253bf280',pass).
icu(training,'25d487eb',pass).
icu(training,'25d8a9c8',fail).
icu(training,'25ff71a9',pass).
icu(training,'264363fd',fail).
icu(training,'272f95fa',pass).
icu(training,'27a28665',fail).
icu(training,'28bf18c6',pass).
icu(training,'28e73c20',fail).
icu(training,'29623171',pass).
icu(training,'29c11459',fail).
icu(training,'29ec7d0e',pass).
icu(training,'2bcee788',pass). % depth 4
icu(training,'2bee17df',pass).
icu(training,'2c608aff',fail).
icu(training,'2dc579da',pass).
icu(training,'2dd70a9a',fail).
icu(training,'2dee498d',pass).
icu(training,'31aa019c',pass).
icu(training,'321b1fc6',fail).
icu(training,'32597951',pass).
icu(training,'3345333e',fail).
icu(training,'3428a4f5',pass).
icu(training,'3618c87e',pass).
icu(training,'3631a71a',fail).
icu(training,'363442ee',fail).
icu(training,'36d67576',fail).
icu(training,'36fdfd69',fail).
icu(training,'3906de3d',pass).
icu(training,'39a8645d',fail).
icu(training,'39e1d7f9',fail).
icu(training,'3aa6fb7a',pass).
icu(training,'3ac3eb23',fail).
icu(training,'3af2c5a8',pass).
icu(training,'3bd67248',pass).
icu(training,'3bdb4ada',fail).
icu(training,'3befdf3e',fail).
icu(training,'3c9b0459',pass).
icu(training,'3de23699',fail).
icu(training,'3e980e27',fail).
icu(training,'3eda0437',fail).
icu(training,'3f7978a0',fail). 
icu(training,'40853293',pass).
icu(training,'4093f84a',fail).
icu(training,'41e4d17e',fail).
icu(training,'4258a5f9',pass).
icu(training,'4290ef0e',fail).
icu(training,'42a50994',pass).
icu(training,'4347f46a',pass).
icu(training,'444801d8',pass).
icu(training,'445eab21',pass).
icu(training,'447fd412',fail).
icu(training,'44d8ac46',fail).
icu(training,'44f52bb0',pass).
icu(training,'4522001f',fail).
icu(training,'4612dd53',pass). % depth 2
icu(training,'46442a0e',fail).
icu(training,'469497ad',fail).
icu(training,'46f33fce',fail).
icu(training,'47c1f68c',pass).
icu(training,'484b58aa',fail).
icu(training,'48d8fb45',pass).
icu(training,'4938f0c2',fail).
icu(training,'496994bd',pass).
icu(training,'49d1d64f',pass).
icu(training,'4be741c5',pass).
icu(training,'4c4377d9',pass).
icu(training,'4c5c2cf0',fail).
icu(training,'50846271',fail).
icu(training,'508bd3b6',fail).
icu(training,'50cb2852',pass).
icu(training,'5117e062',pass).
icu(training,'5168d44c',fail).
icu(training,'539a4f51',fail).
icu(training,'53b68214',pass).
icu(training,'543a7ed5',pass).
icu(training,'54d82841',pass).
icu(training,'54d9e175',fail).
icu(training,'5521c0d9',pass). % depth 4
icu(training,'5582e5ca',pass).
icu(training,'5614dbcf',pass).
icu(training,'56dc2b01',pass).
icu(training,'56ff96f3',pass).
icu(training,'57aa92db',fail).
icu(training,'5ad4f10b',pass).
icu(training,'5bd6f4ac',pass).
icu(training,'5c0a986e',fail).
icu(training,'5c2c9af4',fail).
icu(training,'5daaa586',fail).
icu(training,'60b61512',fail).
icu(training,'6150a2bd',pass).
icu(training,'623ea044',pass).
icu(training,'62c24649',pass).
icu(training,'63613498',pass).
icu(training,'6430c8c4',pass).
icu(training,'6455b5f5',pass).
icu(training,'662c240a',pass).
icu(training,'67385a82',pass).
icu(training,'673ef223',fail).
icu(training,'6773b310',pass). % depth 4
icu(training,'67a3c6ac',pass).
icu(training,'67a423a3',pass). % depth 2
icu(training,'67e8384a',pass).
icu(training,'681b3aeb',pass).
icu(training,'6855a6e4',pass). % depth 2
icu(training,'68b16354',pass).
icu(training,'694f12f3',pass). % depth 2
icu(training,'6a1e5592',fail).
icu(training,'6aa20dc0',fail).
icu(training,'6b9890af',pass).
icu(training,'6c434453',pass).
icu(training,'6cdd2623',pass).
icu(training,'6cf79266',fail).
icu(training,'6d0160f0',fail).
icu(training,'6d0aefbc',pass).
icu(training,'6d58a25d',pass).
icu(training,'6d75e8bb',pass).
icu(training,'6e02f1e3',fail).
icu(training,'6e19193c',fail).
icu(training,'6e82a1ae',pass).
icu(training,'6ecd11f4',fail).
icu(training,'6f8cd79b',pass).
icu(training,'6fa7a44f',pass).
icu(training,'72322fa7',fail).
icu(training,'72ca375d',fail).
icu(training,'73251a56',pass).
icu(training,'7447852a',fail).
icu(training,'7468f01a',pass).
icu(training,'746b3537',pass).
icu(training,'74dd1130',pass).
icu(training,'75b8110e',pass).
icu(training,'760b3cac',pass). % depth 2
icu(training,'776ffc46',fail).
icu(training,'77fdfe62',pass). % depth 4
icu(training,'780d0b14',fail).
icu(training,'7837ac64',fail).
icu(training,'794b24be',fail).
icu(training,'7b6016b9',pass).
icu(training,'7b7f7511',fail).
icu(training,'7c008303',fail).
icu(training,'7ddcd7ec',pass). % depth 2
icu(training,'7df24a62',fail).
icu(training,'7e0986d6',fail).
icu(training,'7f4411dc',fail).
icu(training,'7fe24cdd',fail).
icu(training,'80af3007',pass).
icu(training,'810b9b61',pass).
icu(training,'82819916',fail).
icu(training,'83302e8f',pass).
icu(training,'834ec97d',pass). % depth 4
icu(training,'8403a5d5',fail).
icu(training,'846bdb03',fail).
icu(training,'855e0971',pass). % depth 4
icu(training,'85c4e7cd',fail).
icu(training,'868de0fa',fail).
icu(training,'8731374e',fail).
icu(training,'88a10436',fail).
icu(training,'88a62173',pass).
icu(training,'890034e9',fail).
icu(training,'8a004b2b',fail).
icu(training,'8be77c9e',pass).
icu(training,'8d5021e8',pass).
icu(training,'8d510a79',pass).
icu(training,'8e1813be',pass).
icu(training,'8e5a5113',pass).
icu(training,'8eb1be9a',pass).
icu(training,'8efcae92',pass).
icu(training,'8f2ea7aa',pass).
icu(training,'90c28cc7',pass).
icu(training,'90f3ed37',fail).
icu(training,'913fb3ed',fail).
icu(training,'91413438',fail).
icu(training,'91714a58',fail).
icu(training,'9172f3a0',pass).
icu(training,'928ad970',pass).
icu(training,'93b581b8',fail).
icu(training,'941d9a10',fail).
icu(training,'94f9d214',pass).
icu(training,'952a094c',pass).
icu(training,'9565186b',pass).
icu(training,'95990924',fail).
icu(training,'963e52fc',pass).
icu(training,'97999447',fail).
icu(training,'97a05b5b',fail).
icu(training,'98cf29f8',pass). % depth 2
icu(training,'995c5fa3',fail).
icu(training,'99b1bc43',pass).
icu(training,'99fa7670',pass).
icu(training,'9aec4887',fail).
icu(training,'9af7a82c',pass). % depth 4
icu(training,'9d9215db',fail).
icu(training,'9dfd6313',pass).
icu(training,'9ecd008a',fail).
icu(training,'9edfc990',fail).
icu(training,'9f236235',pass).
icu(training,'a1570a43',pass). % depth 4
icu(training,'a2fd1cf0',pass).
icu(training,'a3325580',fail).
icu(training,'a3df8b1e',fail).
icu(training,'a416b8f3',pass).
icu(training,'a48eeaf7',fail).
icu(training,'a5313dff',pass).
icu(training,'a5f85a15',fail).
icu(training,'a61ba2ce',fail).
icu(training,'a61f2674',fail).
icu(training,'a64e4611',fail).
icu(training,'a65b410d',pass).
icu(training,'a68b268e',pass).
icu(training,'a699fb00',pass).
icu(training,'a740d043',pass).
icu(training,'a78176bb',pass). % depth 2
icu(training,'a79310a0',pass). % depth 4
icu(training,'a85d4709',fail).
icu(training,'a87f7484',pass).
icu(training,'a8c38be5',fail).
icu(training,'a8d7556c',fail).
icu(training,'a9f96cdd',fail).
icu(training,'aabf363d',pass).
icu(training,'aba27056',fail).
icu(training,'ac0a08a4',pass).
icu(training,'ae3edfdc',pass). % depth 2
icu(training,'ae4f1146',pass).
icu(training,'aedd82e4',fail).
icu(training,'af902bf9',pass).
icu(training,'b0c4d837',pass). % depth 2
icu(training,'b190f7f5',pass). % depth 2
icu(training,'b1948b0a',pass).
icu(training,'b230c067',fail).
icu(training,'b27ca6d3',fail).
icu(training,'b2862040',pass).
icu(training,'b527c5c6',fail).
icu(training,'b548a754',pass).
icu(training,'b60334d2',fail).
icu(training,'b6afb2da',pass).
icu(training,'b7249182',fail).
icu(training,'b775ac94',fail).
icu(training,'b782dc8a',pass).
icu(training,'b8825c91',pass).
icu(training,'b8cdaf2b',pass). % depth 4
icu(training,'b91ae062',pass).
icu(training,'b94a9452',pass).
icu(training,'b9b7f026',fail).
icu(training,'ba26e723',pass).
icu(training,'ba97ae07',pass).
icu(training,'bb43febb',pass).
icu(training,'bbc9ae5d',pass).
icu(training,'bc1d5164',pass).
icu(training,'bd4472b8',fail).
icu(training,'bda2d7a6',fail).
icu(training,'bdad9b1f',fail).
icu(training,'be94b721',pass).
icu(training,'beb8660c',pass). % depth 4| 
icu(training,'c0f76784',fail).
icu(training,'c1d99e64',pass).
icu(training,'c3e719e8',pass).
icu(training,'c3f564a4',pass).
icu(training,'c444b776',fail).
icu(training,'c59eb873',pass).
icu(training,'c8cbb738',fail).
icu(training,'c8f0f002',pass).
icu(training,'c909285e',fail).
icu(training,'c9e6f938',pass).
icu(training,'c9f8e694',pass).
icu(training,'caa06a1f',fail).
icu(training,'cbded52d',fail).
icu(training,'cce03e0d',pass).
icu(training,'cdecee7f',fail).
icu(training,'ce22a75a',pass).
icu(training,'ce4f8723',pass).
icu(training,'ce602527',pass). % depth 2
icu(training,'ce9e57f2',pass).
icu(training,'cf98881b',pass).
icu(training,'d037b0a7',pass).
icu(training,'d06dbe63',fail).
icu(training,'d07ae81c',fail).
icu(training,'d0f5fe59',fail).
icu(training,'d10ecb37',pass).
icu(training,'d13f3404',pass).
icu(training,'d22278a0',fail).
icu(training,'d23f8c26',pass).
icu(training,'d2abd087',fail).
icu(training,'d364b489',fail).
icu(training,'d406998b',pass). % depth 2
icu(training,'d43fd935',fail).
icu(training,'d4469b4b',pass).
icu(training,'d4a91cb9',pass). % depth 4
icu(training,'d4f3cd78',fail).
icu(training,'d511f180',pass).
icu(training,'d5d6de2d',pass).
icu(training,'d631b094',pass).
icu(training,'d687bc17',pass).
icu(training,'d6ad076f',pass).
icu(training,'d89b689b',pass).
icu(training,'d8c310e9',fail).
icu(training,'d90796e8',fail).
icu(training,'d9f24cd1',fail).
icu(training,'d9fac9be',pass).
icu(training,'dae9d2b5',pass).
icu(training,'db3e9e38',pass).
icu(training,'db93a21d',fail).
icu(training,'dbc1a6ce',pass).
icu(training,'dc0a314f',fail).
icu(training,'dc1df850',pass).
icu(training,'dc433765',pass).
icu(training,'ddf7fa4f',pass).
icu(training,'de1cd16c',pass).
icu(training,'ded97339',pass).
icu(training,'e179c5f4',fail).
icu(training,'e21d9049',pass).
icu(training,'e26a3af2',fail).
icu(training,'e3497940',pass).
icu(training,'e40b9e2f',fail).
icu(training,'e48d4e1a',fail).
icu(training,'e5062a87',fail).
icu(training,'e509e548',fail).
icu(training,'e50d258f',fail).
icu(training,'e6721834',fail).
icu(training,'e73095fd',fail).
icu(training,'e76a88a6',fail).
icu(training,'e8593010',pass).
icu(training,'e8dc4411',fail).
icu(training,'e9614598',fail).
icu(training,'e98196ab',pass).
icu(training,'e9afcf9a',pass).
icu(training,'ea32f347',fail).
icu(training,'ea786f4a',pass).
icu(training,'eb281b96',pass).
icu(training,'eb5a1d5d',pass).
icu(training,'ec883f72',fail).
icu(training,'ecdecbb3',fail).
icu(training,'ed36ccf7',pass).
icu(training,'ef135b50',pass).
icu(training,'f15e1fac',fail).
icu(training,'f1cefba8',fail).
icu(training,'f25fbde4',pass).
icu(training,'f25ffba3',pass).
icu(training,'f2829549',pass).
icu(training,'f35d900a',pass). % depth 4
icu(training,'f5b8619d',pass).
icu(training,'f76d97a5',pass).
icu(training,'f8a8fe49',pass).
icu(training,'f8b3ba0a',pass).
icu(training,'f8c80d96',pass).
icu(training,'f8ff0b80',fail).
icu(training,'f9012d9b',fail).
icu(training,'fafffa47',pass).
icu(training,'fcb5c309',pass).
icu(training,'fcc82909',fail).
icu(training,'feca6190',pass).
icu(training,'ff28f65a',fail).
icu(training,'ff805c23',fail).

%Personally Created Training Tasks (16)
suite_tag(dbigham_personal,'
jnohuorzh-easier
ihiz27k2n
jnohuorzh
0uduqqj6f
2wfys5w64
2wfys5w64-relative-right-side
n1hczotml
ifmyulnv8
ifmyulnv8-colorlesspoints
ifmyulnv8-dynamic-colorlesspoints
referenceable-components
178fcbfb-easier
middle
surface-pixel-count
4938f0c2-easy
56dc2b01-easier-2
').

% Core ARC Training Tasks (93)
suite_tag(dbigham_train_core,'

0ca9ddb6
3c9b0459
1caeab9d
b60334d2
25ff71a9
3ac3eb23
e76a88a6
c0f76784
321b1fc6
05f2a901
08ed6ac7
a61f2674
253bf280
25d8a9c8
c8f0f002
31aa019c
363442ee
25d487eb
0962bcdd
0d3d703e
1bfc4729
178fcbfb
1f876c06
22eb0ac0
746b3537
6f8cd79b
72ca375d
a79310a0
40853293
95990924
be94b721
ed36ccf7
a740d043
b9b7f026
c59eb873
d631b094
5614dbcf
694f12f3
8be77c9e
46442a0e
2dee498d
3af2c5a8
d0f5fe59
beb8660c
272f95fa
6773b310
8e5a5113
b91ae062
74dd1130
7468f01a
5117e062
67385a82
ac0a08a4
28bf18c6
496994bd
d2abd087
5582e5ca
0bb8dee
d9f24cd1
0520fde7
94f9d214
f25ffba3
9ecd008a
29ec7d0e
bda2d7a6
d5d6de2d
44f52bb0
3bd67248
3631a71a
b8825c91
d13f3404
feca6190
4938f0c2
8eb1be9a
eb281b96
91413438
a5f85a15
97999447
6d75e8bb
63613498
29c11459
b6afb2da
963e52fc
d364b489
1e0a9b12
7e0986d6
868de0fa
56dc2b01
d6ad076f
f8a8fe49
5168d44c
b7249182
b548a754
').

% Tasks Passing via Generalization
% Training Tasks (88)
% The following ARC training tasks started passing after some different task was implemented.
suite_tag(dbigham_train_pass,'
ea32f347
5521c0d9
6c434453
6e82a1ae
aabf363d
b1948b0a
d511f180
88a10436
d037b0a7
4347f46a
56ff96f3
4be741c5
90c28cc7
a87f7484
e9afcf9a
f8ff0b80
1cf80156
23b5c85d
445eab21
4258a5f9
913fb3ed
a61ba2ce
810b9b61
27a28665
6e02f1e3
b2862040
de1cd16c
9172f3a0
d4469b4b
9af7a82c
42a50994
6150a2bd
50cb2852
b230c067
bb43febb
bdad9b1f
794b24be
85c4e7cd
ba97ae07
ff28f65a
67a3c6ac
68b16354
a8c38be5
62c24649
67e8384a
6d0aefbc
6fa7a44f
7fe24cdd
c9e6f938
88a62173
54d9e175
5bd6f4ac
4c4377d9
8d5021e8
a416b8f3
f25fbde4
9dfd6313
8efcae92
aedd82e4
1b2d62fb
99b1bc43
3428a4f5
44d8ac46
6430c8c4
ce4f8723
f2829549
dae9d2b5
fafffa47
ff805c23
dc0a314f
0dfd9992
484b58aa
c3f564a4
d10ecb37
2dc579da
b8cdaf2b
1190e5a7
3aa6fb7a
60b61512
3618c87e
8d510a79
9565186b
af902bf9
ba26e723
dc1df850
3bdb4ada
dc433765
e50d258f
').

% Evaluation Tasks (56)
% The following ARC evaluation tasks are passing. My intention is to not analyzed or implemented evaluation examples specifically, 
% but I have implemented a few accidentally.
suite_tag(dbigham_eval_pass,'
84f2aca1
66e6c45b
f45f5ca7
fc754716
e21a174a
070dd51e
1a2e2828
64a7c07e
ae58858e
37d3e8b2
4364c1c4
60c09cac
85b81ff1
e41c6fd3
3194b014
7953d61e
833dafe3
b1fc8b8e
bc4146bd
00576224
59341089
c48954c1
0c786b71
ed98d772
3979b1a8
be03b35f
d4b1c2b1
0bb8deee
9110e3c5
9a4bb226
ca8de6ea
cd3c21df
00dbd492
34b99a2b
0c9aba6e
195ba7dc
31d5ba1a
506d28a5
5d2a5c43
66f2d22f
d19f7514
e133d23d
e345f17b
67b4a34d
e66aafb8
f4081712
1d0a4b61
5207a7b5
c663677b
e95e3d8e
73ccf9c2
d56f2372
903d1b4a
981571dc
42a15761
e872b94a
2b01abd0
817e6c09
8ee62060
3ee1011a
7039b2d7
da2b0fe3
e5790162
27a77e38').



