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


mask_to_fullnames(Mask,FullNames):- absolute_file_name(Mask,AbsMask,[relative_to('/data')]), 
  expand_file_name(AbsMask,FullNames),FullNames\==[].
mask_to_fullnames(Mask,FullNames):- AbsMask = Mask, expand_file_name(AbsMask,Nonmask),Nonmask\==[],
  my_maplist(absolute_file_name,Nonmask,FullNames).
mask_to_fullnames(Mask,FullNames):-
  arc_sub_path('.',ARC_DIR), member(Rel,['.','/','/data',ARC_DIR]), absolute_file_name(Mask,AbsMask,[relative_to(Rel)]),
  expand_file_name(AbsMask,FullNames),FullNames\==[].

  

:- export(load_json_files/3).
load_json_files(SuiteX,F,Mask):- 
  asserta_if_new(dir_test_suite_name(SuiteX)),
  locally(t_l:local_test_props([test_suite=SuiteX,loadmask=Mask]),
     load_json_file(F,Mask)).

/*:- export(load_json_files/2).
load_json_files(F,Mask):- 
  mask_to_fullnames(Mask,FullNames),  FullNames\==[],
  u_dmsg(load_json_files(F,Mask)),
  my_maplist(load_json_file(F),FullNames),!.
*/
no_uscore(UBaseName,BaseName):- 
  atomic_list_concat(List,'_',UBaseName),
  atomic_list_concat(List,'-',BaseName).

/*
expand_to_abs_file_name_list(FName,ABSFullNames):- 
  \+ exists_file(FName), expand_file_name(FName,FullNames),FullNames\=@=[FName],
  my_maplist(afn_maybe,FullNames,ABSFullNames),
  last(ABSFullNames,Exist),
  exists_file(Exist),!.
afn_maybe(A,A).
load_json_file(F, FName):-  pp(load_json_file(F)=FName),fail.
load_json_file(F, FName):- is_list(FName),!,my_maplist(load_json_file(F), FName).
load_json_file(F, FName):- \+ exists_file(FName), !, expand_to_abs_file_name_list(FName,FullNames),load_json_file(F, FullNames).
%load_json_file(F, FName):- exists_file(FName),\+ is_absolute_file_name(FName), absolute_file_name(FName,ABSName),FName\==ABSName,!,load_json_file_abs(F, ABSName).
load_json_file(F, FName):- \+ is_absolute_file_name(FName), absolute_file_name(FName,ABSName),FName\=@=ABSName,!,load_json_file_abs(F, ABSName).
load_json_file(F, FName):- load_json_file_abs(F, FName),!.
*/
%load_json_file(F, FName):-  pp(load_json_file(F)=FName),fail.
load_json_file(F, FName):- is_list(FName),!,my_maplist(load_json_file(F), FName).
load_json_file(F, FName):- \+ exists_file(FName), expand_file_name(FName,FullNames),FullNames\=@=[FName],last(FullNames,Exist),exists_file(Exist),!,load_json_file(F, FullNames).
%load_json_file(F, FName):- exists_file(FName),\+ is_absolute_file_name(FName), absolute_file_name(FName,ABSName),FName\==ABSName,!,load_json_file2(F, ABSName).
load_json_file(F, FName):- \+ is_absolute_file_name(FName), absolute_file_name(FName,ABSName),FName\=@=ABSName,!,load_json_file2(F, ABSName),!.
load_json_file(F, FName):- load_json_file2(F, FName),!.

load_json_file2(F, FullName):- 
  %pp(load_json_file2(F)=FullName),!,
  must_det_ll((
  file_base_name(FullName,FileBaseName),
  file_name_extension(UName,_,FileBaseName),
  no_uscore(UName,Name), 
  Testname=..[F,Name], 
  % dmsg(load_json_file=FullName),
  setup_call_cleanup(open(FullName,read,In),
   json:json_read(In,Term,[]),
   close(In)),
  locally(t_l:local_test_props(filename=FullName),
    (load_json_of_file(Testname,file,Term),
     ignore((
      kaggle_arc(Testname,_,_,_), 
      add_test_info(Testname),
      add_testfile_name(Testname,FullName))))))),!.


add_testfile_name(Testname,FullName):- 
  ignore((  
  split_string(FullName, "\\/",'./',L),append(_,[Dir,_],L),
  atom_string(ADir,Dir),!,
  ADir\==[],
  add_test_info_prop(Testname,test_suite,ADir),
  (ADir==solution -> true ; asserta_if_new(dir_test_suite_name(ADir))))).


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
  assert_if_new(kaggle_arc_json(Name,Type,In,Out)),
  add_test_info(Name).


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


:- dynamic(muarc_tmp:arc_directory/1).
muarc_tmp:arc_directory(ARC_DIR):- getenv('ARC_DIR',ARC_DIR), exists_directory(ARC_DIR),!.

:- multifile (user:file_search_path/2).
user:file_search_path(arc,  AbsolutePath):- arc_sub_path('.',AbsolutePath).

:- prolog_load_context(directory,ARC_DIR), asserta(muarc_tmp:arc_directory(ARC_DIR)).

absolute_dir_or_file_name(ARC_DIR,Subdir,AbsolutePath):- 
  catch(absolute_file_name(Subdir,AbsolutePath,[relative_to(ARC_DIR),expand(true),
    file_type(directory),solutions(first),file_errors(error),access(read)]),_,fail),!.
absolute_dir_or_file_name(ARC_DIR,Subdir,AbsolutePath):- 
  absolute_file_name(Subdir,AbsolutePath,[relative_to(ARC_DIR),expand(true),
    file_type(regular),solutions(first),file_errors(error),access(read)]).

arc_sub_path(Subdir,AbsolutePath):- muarc_tmp:arc_directory(ARC_DIR),absolute_dir_or_file_name(ARC_DIR,Subdir,AbsolutePath),!.

:- export(arc_sub_path/2).

load_deval:- fail.

load_json_files1:- load_json_files(train400,t,'./data/training/*.json').
load_json_files1:- load_json_files(eval400,v,'./data/devaluation/*.json').
%:- load_json_files(v,'./data/test_100/*.json').
%:- load_json_files(t,'./data/test_nar_10x10/*.json').
load_json_files1:- load_json_files('1D_testset',t,'./data/1D_testset/*.json').
load_json_files1:- load_json_files('MyTrainingData',t,'./dbigham/Data/MyTrainingData/*.json').
%:- load_json_files(v,'../../secret_data/solu**66/*.json').
%:- load_json_files(v,'../../secret_data/evaluation/*.json').
%load_json_files1:- load_deval, load_json_files('secret_data_evaluation',v,'/data/evaluation/*.json').
%load_json_files1:- load_json_files('secret_data_solution',v,'/secret_data/solution/*.json').
% % % load_json_files1:- load_json_files('secret_data_solution',v,'/data/solution/*.json').

load_json_files2:- load_json_files('MiniARC',t,'./MINI-ARC/data/MiniARC/*.json').

load_json_files:- 
  forall(load_json_files1,true).

%:- load_json_files(v,'./data/test/*.json').
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

:- ensure_loaded('./logical_ml/muarc_dmiles').
%kaggle_arc(TestID,ExampleNum,In,Out):- !, kaggle_arc_raw(TestID,ExampleNum,In,Out).
kaggle_arc(TestID,ExampleNum,In,Out):-
  kaggle_arc_raw(TestID,ExampleNum,In0,Out0),
  % in private impl of muarc_dmiles
  maybe_reencode(TestID,ExampleNum,In0,Out0,In1,Out1), In1=In, Out1=Out.

maybe_reencode(TestID,ExampleNum,In0,Out0,In,Out):-
 (t_l:encoder(Enc)->Enc\==raw),!,call(Enc,TestID,ExampleNum,In0,Out0,In,Out).
maybe_reencode(_TName,_ExampleNum,In,Out,In,Out).


kaggle_arc_raw(TestID,ExampleNum,In,Out):- kaggle_arc0(TestID,ExampleNum,In,Out)*-> true ; kaggle_arc1(TestID,ExampleNum,In,Out).
kaggle_arc_raw(Name,tst+AnswerID,In,Grid):- kaggle_arc_answers(Name,ID,AnswerID,Grid), kaggle_arc0(Name,tst+ID,In,_Out).

kaggle_arc0(TestID,ExampleNum,In,Out):- kaggle_arc_json(TestID,ExampleNum,In,O), disallow_test_out(ExampleNum,O,Out).
kaggle_arc1(TestID,ExampleNum,In,Out):- nonvar(ExampleNum),
  kaggle_arc0(TestID,NewExample,In,Out),!,
  ignore((\+ \+ nb_current(example,ExampleNum),  nb_setval(example,NewExample))).

   


%adisallow_test_out(trn+_,OO,OO):-!.
%disallow_test_out(tst+_, O,OO):- grid_size(O,H,V),make_grid(H,V,OO).
disallow_test_out(_,OO,OO).

tasks_split(ID,String):- split_string(String,",[] \n\r\t\s",",[] \n\r\t\s",L),member(S,L),atom_string(E,S),atom_id_e(E,ID).

icu('007bbfb7', x + 3 ).
icu('00d62c1b', x + 3 ).
icu('017c7c7b', x + 33 ).
icu('025d127b', x + 3 ).
icu('045e512c', -1 ).
icu('0520fde7', 3 ).
icu('05269061', x + 3 ).
icu('05f2a901', x + -1 ).
icu('06df4c85', 3 ).
icu('08ed6ac7', -1 ).
icu('09629e4f', 3 ).
icu('0962bcdd', 3 ).
icu('0a938d79', -1 ).
icu('0b148d64', 3 ).
icu('0ca9ddb6', -1 ).
icu('0d3d703e', -1 ).
icu('0dfd9992', 3 ).
icu('0e206a2e', -1 ).
icu('10fcaaa3', -1 ).
icu('11852cab', -1 ).
icu('1190e5a7', 3 ).
icu('137eaa0f', -1 ).
icu('150deff5', -1 ).
icu('178fcbfb', 3 ).
icu('1a07d186', -1 ).
icu('1b2d62fb', 3 ).
icu('1b60fb0c', -1 ).
icu('1bfc4729', 3 ).
icu('1c786137', 3 ).
icu('1caeab9d', -1 ).
icu('1cf80156', 3 ).
icu('1e0a9b12', 3 ).
icu('1e32b0e9', 3 ).
icu('1f0c79e5', 3 ).
icu('1f642eb9', -1 ).
icu('1f85a75f', 3 ).
icu('1f876c06', 3 ).
icu('1fad071e', 3 ).
icu('2013d3e2', 3 ).
icu('2204b7a8', 3 ).
icu('22168020', 3 ).
icu('22233c11', -1 ).
icu('2281f1f4', 3 ).
icu('228f6490', 3 ).
icu('22eb0ac0', 3 ).
icu('234bbc79', -1 ).
icu('23581191', 23 ).
icu('239be575', 3 ).
icu('23b5c85d', 3 ).
icu('253bf280', 3 ).
icu('25d487eb', 3 ).
icu('25d8a9c8', -1 ).
icu('25ff71a9', 3 ).
icu('264363fd', -1 ).
icu('272f95fa', 3 ).
icu('27a28665', -1 ).
icu('28bf18c6', 3 ).
icu('28e73c20', -1 ).
icu('29623171', 3 ).
icu('29c11459', -1 ).
icu('29ec7d0e', 3 ).
icu('2bcee788', 4 ).
icu('2bee17df', 3 ).
icu('2c608aff', -1 ).
icu('2dc579da', 3 ).
icu('2dd70a9a', -1 ).
icu('2dee498d', 3 ).
icu('31aa019c', 3 ).
icu('321b1fc6', -1 ).
icu('32597951', 3 ).
icu('3345333e', -1 ).
icu('3428a4f5', 3 ).
icu('3618c87e', 3 ).
icu('3631a71a', -1 ).
icu('363442ee', -1 ).
icu('36d67576', -1 ).
icu('36fdfd69', -1 ).
icu('3906de3d', 3 ).
icu('39a8645d', -1 ).
icu('39e1d7f9', -1 ).
icu('3aa6fb7a', 3 ).
icu('3ac3eb23', -1 ).
icu('3af2c5a8', 3 ).
icu('3bd67248', 3 ).
icu('3bdb4ada', -1 ).
icu('3befdf3e', -1 ).
icu('3c9b0459', 3 ).
icu('3de23699', -1 ).
icu('3e980e27', -1 ).
icu('3eda0437', -1 ).
icu('3f7978a0', -1 ).
icu('40853293', 3 ).
icu('4093f84a', -1 ).
icu('41e4d17e', -1 ).
icu('4258a5f9', 3 ).
icu('4290ef0e', -1 ).
icu('42a50994', 3 ).
icu('4347f46a', 3 ).
icu('444801d8', 3 ).
icu('445eab21', 3 ).
icu('447fd412', -1 ).
icu('44d8ac46', -1 ).
icu('44f52bb0', 3 ).
icu('4522001f', -1 ).
icu('4612dd53', 23 ).
icu('46442a0e', -1 ).
icu('469497ad', -1 ).
icu('46f33fce', -1 ).
icu('47c1f68c', 3 ).
icu('484b58aa', -1 ).
icu('48d8fb45', 3 ).
icu('4938f0c2', -1 ).
icu('496994bd', 3 ).
icu('49d1d64f', 3 ).
icu('4be741c5', 3 ).
icu('4c4377d9', 3 ).
icu('4c5c2cf0', -1 ).
icu('50846271', -1 ).
icu('508bd3b6', -1 ).
icu('50cb2852', 3 ).
icu('5117e062', 3 ).
icu('5168d44c', -1 ).
icu('539a4f51', -1 ).
icu('53b68214', 3 ).
icu('543a7ed5', 3 ).
icu('54d82841', 3 ).
icu('54d9e175', -1 ).
icu('5521c0d9', 4 ).
icu('5582e5ca', 3 ).
icu('5614dbcf', 3 ).
icu('56dc2b01', 33 ).
icu('56ff96f3', 3 ).
icu('57aa92db', -1 ).
icu('5ad4f10b', 3 ).
icu('5bd6f4ac', 3 ).
icu('5c0a986e', -1 ).
icu('5c2c9af4', -1 ).
icu('5daaa586', -1 ).
icu('60b61512', -1 ).
icu('6150a2bd', 3 ).
icu('623ea044', 3 ).
icu('62c24649', 3 ).
icu('63613498', 3 ).
icu('6430c8c4', 3 ).
icu('6455b5f5', 3 ).
icu('662c240a', 3 ).
icu('67385a82', 3 ).
icu('673ef223', -1 ).
icu('6773b310', 4 ).
icu('67a3c6ac', 3 ).
icu('67a423a3', 23 ).
icu('67e8384a', 3 ).
icu('681b3aeb', 3 ).
icu('6855a6e4', 23 ).
icu('68b16354', 3 ).
icu('694f12f3', 23 ).
icu('6a1e5592', -1 ).
icu('6aa20dc0', -1 ).
icu('6b9890af', 3 ).
icu('6c434453', 3 ).
icu('6cdd2623', 3 ).
icu('6cf79266', -1 ).
icu('6d0160f0', -1 ).
icu('6d0aefbc', 3 ).
icu('6d58a25d', 3 ).
icu('6d75e8bb', 3 ).
icu('6e02f1e3', -1 ).
icu('6e19193c', -1 ).
icu('6e82a1ae', 33 ).
icu('6ecd11f4', -1 ).
icu('6f8cd79b', 3 ).
icu('6fa7a44f', 3 ).
icu('72322fa7', -1 ).
icu('72ca375d', -1 ).
icu('73251a56', 3 ).
icu('7447852a', -1 ).
icu('7468f01a', 3 ).
icu('746b3537', 3 ).
icu('74dd1130', 3 ).
icu('75b8110e', 3 ).
icu('760b3cac', 23 ).
icu('776ffc46', -1 ).
icu('77fdfe62', 4 ).
icu('780d0b14', -1 ).
icu('7837ac64', -1 ).
icu('794b24be', -1 ).
icu('7b6016b9', 3 ).
icu('7b7f7511', -1 ).
icu('7c008303', -1 ).
icu('7ddcd7ec', 23 ).
icu('7df24a62', -1 ).
icu('7e0986d6', -1 ).
icu('7f4411dc', -1 ).
icu('7fe24cdd', -1 ).
icu('80af3007', 3 ).
icu('810b9b61', 3 ).
icu('82819916', -1 ).
icu('83302e8f', 3 ).
icu('834ec97d', 4 ).
icu('8403a5d5', -1 ).
icu('846bdb03', -1 ).
icu('855e0971', 4 ).
icu('85c4e7cd', -1 ).
icu('868de0fa', -1 ).
icu('8731374e', -1 ).
icu('88a10436', -1 ).
icu('88a62173', 3 ).
icu('890034e9', -1 ).
icu('8a004b2b', -1 ).
icu('8be77c9e', 3 ).
icu('8d5021e8', 3 ).
icu('8d510a79', 3 ).
icu('8e1813be', 3 ).
icu('8e5a5113', 3 ).
icu('8eb1be9a', 3 ).
icu('8efcae92', 3 ).
icu('8f2ea7aa', 3 ).
icu('90c28cc7', 3 ).
icu('90f3ed37', -1 ).
icu('913fb3ed', -1 ).
icu('91413438', -1 ).
icu('91714a58', -1 ).
icu('9172f3a0', 3 ).
icu('928ad970', 3 ).
icu('93b581b8', -1 ).
icu('941d9a10', -1 ).
icu('94f9d214', 3 ).
icu('952a094c', 3 ).
icu('9565186b', 3 ).
icu('95990924', -1 ).
icu('963e52fc', 3 ).
icu('97999447', -1 ).
icu('97a05b5b', -1 ).
icu('98cf29f8', 23 ).
icu('995c5fa3', -1 ).
icu('99b1bc43', 3 ).
icu('99fa7670', 3 ).
icu('9aec4887', -1 ).
icu('9af7a82c', 4 ).
icu('9d9215db', -1 ).
icu('9dfd6313', 3 ).
icu('9ecd008a', -1 ).
icu('9edfc990', -1 ).
icu('9f236235', 3 ).
icu('a1570a43', 4 ).
icu('a2fd1cf0', 3 ).
icu('a3325580', -1 ).
icu('a3df8b1e', -1 ).
icu('a416b8f3', 3 ).
icu('a48eeaf7', -1 ).
icu('a5313dff', 3 ).
icu('a5f85a15', -1 ).
icu('a61ba2ce', -1 ).
icu('a61f2674', -1 ).
icu('a64e4611', -1 ).
icu('a65b410d', 3 ).
icu('a68b268e', 3 ).
icu('a699fb00', 3 ).
icu('a740d043', 3 ).
icu('a78176bb', 23 ).
icu('a79310a0', 4 ).
icu('a85d4709', -1 ).
icu('a87f7484', 3 ).
icu('a8c38be5', -1 ).
icu('a8d7556c', -1 ).
icu('a9f96cdd', -1 ).
icu('aabf363d', 3 ).
icu('aba27056', -1 ).
icu('ac0a08a4', 3 ).
icu('ae3edfdc', 23 ).
icu('ae4f1146', 3 ).
icu('aedd82e4', -1 ).
icu('af902bf9', 3 ).
icu('b0c4d837', 23 ).
icu('b190f7f5', 23 ).
icu('b1948b0a', 3 ).
icu('b230c067', -1 ).
icu('b27ca6d3', -1 ).
icu('b2862040', 3 ).
icu('b527c5c6', -1 ).
icu('b548a754', 3 ).
icu('b60334d2', -1 ).
icu('b6afb2da', 3 ).
icu('b7249182', -1 ).
icu('b775ac94', -1 ).
icu('b782dc8a', 3 ).
icu('b8825c91', 3 ).
icu('b8cdaf2b', 4 ).
icu('b91ae062', 3 ).
icu('b94a9452', 3 ).
icu('b9b7f026', -1 ).
icu('ba26e723', 3 ).
icu('ba97ae07', 3 ).
icu('bb43febb', 3 ).
icu('bbc9ae5d', 3 ).
icu('bc1d5164', 3 ).
icu('bd4472b8', -1 ).
icu('bda2d7a6', -1 ).
icu('bdad9b1f', -1 ).
icu('be94b721', 3 ).
icu('beb8660c', 4 ).
icu('c0f76784', -1 ).
icu('c1d99e64', 3 ).
icu('c3e719e8', 3 ).
icu('c3f564a4', 3 ).
icu('c444b776', -1 ).
icu('c59eb873', 3 ).
icu('c8cbb738', -1 ).
icu('c8f0f002', 3 ).
icu('c909285e', x + -1 ).
icu('c9e6f938', 3 ).
icu('c9f8e694', 3 ).
icu('caa06a1f', -1 ).
icu('cbded52d', -1 ).
icu('cce03e0d', 3 ).
icu('cdecee7f', -1 ).
icu('ce22a75a', 3 ).
icu('ce4f8723', x + 3 ).
icu('ce602527', 23 ).
icu('ce9e57f2', 3 ).
icu('cf98881b', 3 ).
icu('d037b0a7', 3 ).
icu('d06dbe63', -1 ).
icu('d07ae81c', -1 ).
icu('d0f5fe59', x + -1 ).
icu('d10ecb37', 3 ).
icu('d13f3404', 3 ).
icu('d22278a0', -1 ).
icu('d23f8c26', 3 ).
icu('d2abd087', -1 ).
icu('d364b489', -1 ).
icu('d406998b', 23 ).
icu('d43fd935', -1 ).
icu('d4469b4b', 3 ).
icu('d4a91cb9', 4 ).
icu('d4f3cd78', -1 ).
icu('d511f180', 3 ).
icu('d5d6de2d', 3 ).
icu('d631b094', 3 ).
icu('d687bc17', 3 ).
icu('d6ad076f', 3 ).
icu('d89b689b', 3 ).
icu('d8c310e9', -1 ).
icu('d90796e8', -1 ).
icu('d9f24cd1', -1 ).
icu('d9fac9be', 3 ).
icu('dae9d2b5', 3 ).
icu('db3e9e38', 3 ).
icu('db93a21d', -1 ).
icu('dbc1a6ce', 3 ).
icu('dc0a314f', -1 ).
icu('dc1df850', 3 ).
icu('dc433765', 3 ).
icu('ddf7fa4f', 33 ).
icu('de1cd16c', 3 ).
icu('ded97339', 3 ).
icu('e179c5f4', -1 ).
icu('e21d9049', 3 ).
icu('e26a3af2', -1 ).
icu('e3497940', 3 ).
icu('e40b9e2f', -1 ).
icu('e48d4e1a', -1 ).
icu('e5062a87', -1 ).
icu('e509e548', -1 ).
icu('e50d258f', -1 ).
icu('e6721834', -1 ).
icu('e73095fd', -1 ).
icu('e76a88a6', -1 ).
icu('e8593010', 3 ).
icu('e8dc4411', -1 ).
icu('e9614598', -1 ).
icu('e98196ab', 3 ).
icu('e9afcf9a', 3 ).
icu('ea32f347', -1 ).
icu('ea786f4a', 3 ).
icu('eb281b96', 3 ).
icu('eb5a1d5d', 3 ).
icu('ec883f72', -1 ).
icu('ecdecbb3', -1 ).
icu('ed36ccf7', 3 ).
icu('ef135b50', 3 ).
icu('f15e1fac', -1 ).
icu('f1cefba8', -1 ).
icu('f25fbde4', 3 ).
icu('f25ffba3', 3 ).
icu('f2829549', 3 ).
icu('f35d900a', 4 ).
icu('f5b8619d', 3 ).
icu('f76d97a5', 3 ).
icu('f8a8fe49', 3 ).
icu('f8b3ba0a', 3 ).
icu('f8c80d96', 3 ).
icu('f8ff0b80', -1 ).
icu('f9012d9b', -1 ).
icu('fafffa47', 3 ).
icu('fcb5c309', 3 ).
icu('fcc82909', -1 ).
icu('feca6190', 3 ).
icu('ff28f65a', -1 ).
icu('ff805c23', -1 ).

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
