/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

arc_test_name(Name):- 
  findall(Name,kaggle_arc(Name,_,_,_),All),
  list_to_set(All,AllS),member(Name,AllS).
test_info(Name,InfoS):- fix_test_name(Name,CName,_),
 findall([Inf],(user:fav(CName,Inf0),repair_info(Inf0,Inf)),Info),
  flatten(Info,InfoF),list_to_set(InfoF,InfoS).

repair_info(Inf0,Inf):- is_list(Inf0),!,maplist(repair_info,Inf0,Inf).
repair_info(Inf,InfO):- compound(Inf),functor(Inf,F,1),!,arg(1,Inf,A),listify(A,ArgsL),InfO=..[F,ArgsL].
repair_info(Inf,InfO):- compound(Inf),!,compound_name_arguments(Inf,F,ArgsL),InfO=..[F,ArgsL].
repair_info(Inf,Inf).

was_fav(X):- nonvar_or_ci(X), clause(fav(XX,_),true),nonvar_or_ci(XX),X==XX.

test_names_by_hard(Name):- test_names_ord_favs(FavList),test_names_ord_hard(NamesByHard),my_append(NamesByHard,FavList,All),
 list_to_set(All,AllS),!,member(Name,AllS).

test_names_by_hard_rev(Name):- test_names_ord_favs(FavList),test_names_ord_hard(NamesByHard),my_append(NamesByHard,FavList,All),
 list_to_set(All,AllS),!,reverse(AllS,AllR),member(Name,AllR).

test_names_by_fav(Name):- test_names_ord_favs(All),member(Name,All).
test_names_by_fav_rev(Name):- test_names_ord_favs(AllR),reverse(AllR,All),member(Name,All).

 /*
test_names_by_hard(Name):- test_names_ord_favs(FavList),test_names_ord_hard(NamesByHard),my_append(FavList,NamesByHard,All),
 list_to_set(All,AllS),!,member(Name,AllS).*/
test_names_ord_favs(FavListS):- findall(Name,fav(Name),FavList),list_to_set(FavList,FavListS).
test_names_ord_hard(NamesByHard):- findall(Hard-Name,(arc_test_name(Name),hardness_of_name(Name,Hard)),All),
  keysort(All,AllK), reverse(AllK,AllR), maplist(arg(2),AllR,NamesByHard),!.

%:- use_module(library(pfc_lib)).

ascending_hard:-
  tell('arc_ascending.pl'),
  forall(test_names_by_hard(Name),
    forall(kaggle_arc(Name,ExampleNum,In,Out),format('~q.~n',[kaggle_arc_ord(Name,ExampleNum,In,Out)]))),
  told,
  reconsult(arc_ascending).

:- style_check(-singleton).
negate_number(N,NN):- NN is - N.
hardness_of_name(Name,Hard):-
 %ExampleNum=tst+_,
 ExampleNum=_,
 findall(_,kaggle_arc(Name,(trn+_),_,_),Trns),
 findall(Hard,
 (kaggle_arc(Name,ExampleNum,In,Out),
  grid_size(In,IH,IV),
  grid_size(Out,OH,OV),

  IArea is IH * IV,
  OArea is OH * OV,
  DArea is abs(OArea-IArea),
  max_min(OArea,IArea,MaxArea,MinArea),

  mass(In,IMass),
  mass(Out,OMass),
  DMass is abs(OMass-IMass),

  unique_color_count(In,ICCount),
  unique_color_count(Out,OCCount),
  DCCount is abs(OCCount-ICCount),

  unique_colors(In,IColors),
  unique_colors(Out,OColors),
  intersection(IColors,OColors,SColors,UIColors,UOColors),
  append([UIColors,SColors,UOColors],UAllColors),sort(UAllColors,AllColors),
  maplist(length,[IColors,OColors,SColors,UIColors,UOColors,AllColors],
              [IColorsL,OColorsL,SColorsL,UIColorsL,UOColorsL,AllColorsL]),
  maplist(negate_number,[IColorsL,OColorsL,SColorsL,UIColorsL,UOColorsL,AllColorsL],
              [IColorsNeg,OColorsNeg,SColorsNeg,UIColorsNeg,UOColorsNeg,AllColorsNeg]),

  IDensity is IMass/IArea,
  ODensity is OMass/OArea,
  DDensity is abs(ODensity-IDensity),

  max_min(OH,OV,OMax,_),
  max_min(IH,IV,IMax,_),
  OHV is OMax*OMax, IHV is IMax*IMax,
  OHV is OMax*OMax, IHV is IMax*IMax,

  max_min(OHV,IHV,Max,Min),
  D is Max-Min,

  Hard = SColorsNeg+UOColorsNeg+UIColorsNeg+DArea+DDensity+Trns

  ),All),
  sort(All,AllK),last(AllK,Hard).


sort_univ(=,A,B):- var(A),var(B), A==B,!.
sort_univ(=,A,B):- compound(A),compound(B), \+ A\=B,!.
sort_univ(R,A,B):- compare(R,A,B).

:- style_check(+singleton).

:- dynamic(fav/2).

macro(one_obj, must(len(objs)=1)).

test_p2(P2):- clsmake,
  append_termlist(P2,[N1,'$VAR'('Result')],N2), 
  time(forall(into_gridoid(N1,G1), 
     forall(call(P2,G1,G2),
       once(ignore((print_side_by_side4(G1,N1,_LW,G2,?-N2),dash_chars)))))).

:- style_check(-singleton).
whole(I,O):- is_group(I),length(I,1),I=O,!.
whole(I,O):- print_grid(I),pt(I),into_obj(I,O).
one_obj(I,I):- is_group(I),length(I,1),!.
one_obj(I,I):- is_group(I),!.
one_obj(I,I).
uncolorize(I,O):- set_all_fg_colors(grey,I,O).
resize_grid(_H,_V,List,_,List):- is_list(List).
resize_grid(H,V,Color,_,NewGrid):- make_grid(H,V,NewGrid),replace_grid_point(1,1,Color,_,NewGrid,Grid),nop(set_bgc(Color)).

resize_grid(H,V,_,NewGrid):- make_grid(H,V,NewGrid).

h_symmetric(Group,TF):- call_bool(h_symmetric(Group),TF).

call_bool(G,TF):- (call(G)*->TF=true;TF=false).
freeze_on([NV],Goal):- !, call(Goal).
freeze_on([],Goal):- !, call(Goal).
freeze_on([NV|Vars],Goal):- nonvar(NV),!,freeze_on(Vars,Goal).
freeze_on([NV|Vars],Goal):- maplist(nonvar,Vars),!,call(Goal).
freeze_on(Vars,Goal):- maplist(freeze_until(Goal,Vars),Vars).
freeze_until(Goal,Vars,Var):- freeze(Var,freeze_on(Vars,Goal)).

i(A,B,C):- individuate(A,B,C),!.

:- dynamic(is_db/2).
db_value(value:Color,_,Color).
db_value(largest:P1,[Obj|_],P1:TF):- nonvar(P1),!,freeze(Obj,call_bool(call(P1,Obj),TF)),!.
db_value(Color,_,Color).


is_eval(P1,Prev,P1A):- nop(is_eval(P1,Prev,P1A)),fail.
db_u(PL1,P1,PL2,P2,In,Out):- is_eval(P1,Prev,P1A),!,db_u([Prev|PL1],P1,P2L,P2,In,Out).
db_u(PL1,P1,PL2,P2,In,Out):- is_eval(P2,Prev,P2A),!,db_u(PL1,P1,[Prev|P2L],P2,In,Out).

%db(P1,P2,In,In):- t_or_t(freeze_for([P2],assert(is_db(TF,P2))),is_db(TF,P2)).
%db(P2,P1,In,In):- nonvar(Color), db_value(P1,In,TF),!,t_or_t(freeze_for([Color],assert(is_db(TF,Color))),is_db(TF,Color)).
db(P1,P2,In,Out):- db_u([],P1,[],P2,In,Out).
db(X,Y,I,I):- pt(db(X,Y)),pt(I).

copy_grid(In,G,G):- In == in,!.
copy_grid(Name,_,G):- get_vm(VM), G=VM.Name .

current_test_name(Name):- nb_current(arc_test_name,Name),!.
current_test_name([]).

/*  

%fav(v('20818e16'),[guess_bg,indiv(min(2))]).

%(fav(_,P)/(flatten([P],Flat),member(E,Flat))) ==> fav_trait(E).

*/
:- dynamic(muarc:kaggle_arc_json/4).


load_json_files(F,Mask):- 
  arc_sub_path('.',ARC_DIR),
  absolute_file_name(Mask,AbsMask,[relative_to(ARC_DIR)]),
  expand_file_name(AbsMask,FullNames),
  maplist(file_base_name,FullNames,BaseNames),
  maplist(file_name_extension,Names,_,BaseNames),
  maplist(load_json_file(F),Names,FullNames),!.

load_json_file(F, BaseName, FullName):- Testname=..[F,BaseName], 
  % dmsg(load_json_file=FullName),
  setup_call_cleanup(open(FullName,read,In),
   json_read(In,Term,[]),
   close(In)),
  load_json_of_file(Testname,file,Term),!.

  load_json_of_file(Name,Type,json(Term)):-! , load_json_of_file(Name,Type,Term).
  load_json_of_file(Name,_,Type=Value):-!, load_json_of_file(Name,Type,Value).
  load_json_of_file(Name,train,T):-!,load_json_of_file(Name,trn,T).
  load_json_of_file(Name,test,T):-!,load_json_of_file(Name,tst,T).
    load_json_of_file(Testname,ExampleNum,[input=In,output=Out]):-
       json_to_colors(In,InColor),
       json_to_colors(Out,OutColor),
       assert_if_new(muarc:kaggle_arc_json(Testname,ExampleNum,InColor,OutColor)),!.
  load_json_of_file(Name,Type,[input=In,output=Out]):-assert_if_new(muarc:kaggle_arc_json(Name,Type,In,Out)),!.
  load_json_of_file(Name,Type,[H|T]):- !, forall(nth0(N,[H|T],E), load_json_of_file(Name,Type+N,E)).
  load_json_of_file(N,T,V):- wdmsg(load_json_of_file(N,T,V)),!.


json_to_colors(Out,Color):- is_grid_color(Out),!,Out=Color.
json_to_colors(Out,Color):- is_list(Out),!,maplist(json_to_colors,Out,Color).
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

:- load_json_files(t,'./data/training/*.json').
:- load_json_files(v,'./data/evaluation/*.json').
%:- load_json_files(v,'./data/test/*.json').
kaggle_arc(TName,ExampleNum,In,Out):- muarc:kaggle_arc_json(TName,ExampleNum,In,Out).
% ExampleNum is tst or trn
/*
kaggle_arc(t(Name), TypeI, In, Out):- 
 nth_fact(kaggle_arc_train(Name, ExampleNum, In, Out), This), once((nth_fact(kaggle_arc_train(Name, ExampleNum, _, _), Start), I is This - Start, TypeI=ExampleNum->I)).
kaggle_arc(v(Name), TypeI, In, Out):- 
 member(ExampleNum, [trn, tst]), nth_fact(kaggle_arc_eval(Name, ExampleNum, In, Out), This), once((nth_fact(kaggle_arc_eval(Name, ExampleNum, _, _), Start), I is This - Start, TypeI=ExampleNum->I)).
*/

fix_test_name(X,X,_):- plain_var(X),!.
fix_test_name(Tried*ExampleNum*_,Fixed,ExampleNum):-!,fix_test_name(Tried,Fixed,_).
fix_test_name(Tried*ExampleNum,Fixed,ExampleNum):-!,fix_test_name(Tried,Fixed,_).
fix_test_name(Tried,Tried,ExampleNum):- \+ \+ kaggle_arc(Tried,_,_,_),!, kaggle_arc(Tried,ExampleNum,_,_),!.
fix_test_name(Tried,Fixed,ExampleNum):- compound(Tried),!,arg(_,Tried,E),nonvar_or_ci(E),fix_test_name(E,Fixed,ExampleNum).
fix_test_name(Tried,t(Tried),_):- kaggle_arc(t(Tried),_,_,_),!.
fix_test_name(X,v(X),_):- kaggle_arc(v(X),_,_,_),!.

print_trainer0:- arc(t('25d487eb')).
print_eval0:- arc(v('009d5c81')).



parc1:- parc1(6300*3). 
parc1(OS):- clsmake,   nb_setval(last_test,[]),
   open(tt,write,O,[encoding(text)]), parc0(OS), with_output_to(O,parc0(OS)), close(O).
parc0(OS):-
 locally(set_prolog_flag(gc,true),forall(parc11(OS,_),true)).


parcCmt(TName):-
  ignore((
  fix_test_name(TName,TestID,_),
  kaggle_arc(TestID,(trn+0),In,Out),
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  IHV = IH*IV, OHV = OH*OV,
  BGColor = '$VAR'('Color'),
  (IHV\==OHV -> CG = resize_grid(OH,OV,BGColor); CG = copy_grid(in)),
  findall(III,gather_more_task_info(TestID,III),InfoUF),
  flatten(InfoUF,InfoF),
  DSL = sol(i(complete),CG,db(objs:h___________symetric,BGColor)),
  predsort(sort_univ,[DSL|InfoF],InfoFS), %44f52bb0
  reverse(InfoFS,InfoSR),
  P = fav(TestID,InfoSR),
  format('~q.~n',[P]))).

cmt_border:- format('~N% '), dash_chars(120,"="), nl.

%print_test(TName):- !, parcCmt(TName).
print_test(TName):- 
    once(fix_test_name(TName,TestID,_)),
    cmt_border,format('% ?- ~q. ~n',[print_test(TName)]),cmt_border,
    parcCmt(TName),nl,
    forall(arg(_,v(trn+_,tst+_),ExampleNum),
     forall(kaggle_arc(TestID,ExampleNum,In,Out),   
     (test_id_border(TestID),
      print_single_test(TestID*ExampleNum)))),
    cmt_border.

test_id_border(TestID):-
    (nb_current(last_test,WasTestID);WasTestID=[]),!,
    ignore((WasTestID\==TestID, nb_setval(last_test,TestID), cmt_border)).

print_single_test(TName):-
  fix_test_name(TName,TestID,ExampleNum),
  kaggle_arc(TestID,ExampleNum,In,Out),
  nb_current(last_test,WasTestID),
  once(in_out_name(ExampleNum,NameIn,NameOut)),
  format('~Ntestcase(~q,"\n~@").~n~n~n',[TestID*ExampleNum,print_side_by_side4(In,NameIn,_LW,Out,NameOut)]),
  ignore((WasTestID\==TestID, write('%= '), parcCmt(TName), nl)).

in_out_name(lrn+_,'Training Input','Training Output').
in_out_name(trn+_,'Training Input','Training Output').
in_out_name(tst+_,'Test Input','Expected Output').
in_out_name(_,'Test Input','Expected Output').

parc11(OS,_):-
  forall(test_names_by_hard(TName),    print_test(TName)).


parc11(OS,TName):- fail,    
  fix_test_name(TName,TestID,ExampleNum),   
  kaggle_arc(TestID,ExampleNum,In,Out),
  maplist(color_sym(OS),In,I),
  grid_size(In,IH,IV),
  grid_size(Out,_OH,OV),
  %V is OV-IV,
  H is IH,
  maplist(color_sym(OS),Out,O),
  format('~Ntestcase(~q,"\n~@").~n',[TestID*ExampleNum,
    print_side_by_side(call((print_grid(IH,IV,I),
      write(' '),forall(between(IV,OV,_),(write('\n '),dash_chars(H,'  '))))),call(print_grid(O)))]).

%color_sym(OS,[(black='°'),(blue='©'),(red='®'),(green=''),(yellow),(silver='O'),(purple),(orange='o'),(cyan= 248	ø ),(brown)]).
color_sym(OS,C,Sym):- is_list(C),maplist(color_sym(OS),C,Sym),!.
color_sym(_,black,' ').
color_sym(OS,C,Sym):- color_sym(OS,4,C,Sym).
color_sym(_,_,C,Sym):- enum_colors(C),color_int(C,I),nth1(I,`ose=xt~+*zk>`,S),name(Sym,[S]).
%color_sym(P*T,_,C,Sym):- enum_colors(C),color_int(C,I),S is P+I*T,name(Sym,[S]).

:- include('kaggle_arc_test_easy.pl').
:- include('kaggle_arc_test_old.pl').

:- fixup_exports.
