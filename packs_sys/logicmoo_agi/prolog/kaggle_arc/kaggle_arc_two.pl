
:- enable_arc_expansion.

make_training(TestID,VMO):- 
 %make_fti(_GH,_GV,TestID,_Grid,_Sofar,_Reserved,_Options,_Points,ArgVM),
 must_det_ll((
    WAZ = _{
      %program:[],
      %pairs:_, %datatree:_, 
      %current:_,
      test_id:TestID},
    make_training_hints(TestID,WAZ,VMO))).
    /*
     test:ID,mappings:_,
     pre_in:_, pre_out:_,
     inC:_InC,outC:_OutC,
     removed:_,added:_, kept:_,   
     grid_in:_,target_grid:_,
   set(VM.mappings) =[map])), !. % pp(VM),nl.
  */



%show_arc_pair_progress(TestID,ExampleNum,In,Out):- show_arc_pair_progress_sol(TestID,ExampleNum,In,Out),!.
train_test:- notrace(get_current_test(TestID)), once(train_test(TestID)).
train_test(TestID):- with_pair_mode(whole_test,train_whole_test(TestID)).
train_whole_test(TestID):- 
  clear_training(TestID),
  compile_and_save_test(TestID),!,
  locally(nb_setval(menu_key,o),ndividuator),
  locally(nb_setval(menu_key,s),ndividuator),!.
  %train_test(TestID,train_using_oo_ii_io).
  %train_test(TestID,train_using_io).
train_test(TestID,P2):-   
  print_testinfo(TestID),
  flag(indiv,_,0),
  %get_training(PrevPairEnv),
  %luser_setval(prev_pairEnv,PrevPairEnv),
  %nb_delete('$training_vm'),
  %get_training(Training),
  %my_time(make_training_hints(TestID,Training,_HIDE_Dictation)),
  %Dictation = Training,
  %set_training(Dictation),
  rb_new(Dictation),
  my_time(call(P2,TestID,Dictation,DictOut)),
  set_training(DictOut),!.


train_using_oo_ii_io(TestID,DictIn,DictOut):- 
  train_using_oo_ii_io(TestID,trn,0,DictIn,DictOut).

train_using_oo_ii_io(TestID,Trn,N1,DictIn,DictOut):-
 (kaggle_arc(TestID,(Trn+N1),In1,Out1), N2 is N1 + 1),

 (kaggle_arc(TestID,(Trn+N2),_In2,Out2)
   -> 
    (with_other_grid(Out2,train_for_objects_from_pair_with_mono(DictIn,TestID,[Trn,'o',N1,'o',N2],Out1,Out2,Dict1)),
     %nop((train_for_objects_from_pair_with_mono(Dict0,TestID,[Trn,'i',N1,'i',N2],In1,In2,Dict1))),
     train_using_oo_ii_io(TestID,Trn,N2,Dict1,DictM))
     ; (DictM = DictIn)),
  !,
  with_other_grid(Out1,train_for_objects_from_pair_with_mono(DictM,TestID,[Trn,'i',N1,'o',N1],In1,Out1,DictOut)),!.

train_using_oo_ii_io(_TestID,_Trn,_N1,DictInOut,DictInOut).

train_only_from_pairs:- notrace(get_current_test(TestID)), train_only_from_pairs(TestID).

train_only_from_pairs(TestID):- clear_training(TestID), train_test(TestID,train_using_io).

train_using_io(TestID,DictIn,DictOut):- train_using_io(TestID,trn,0,DictIn,DictOut),!.
train_using_io(TestID,Trn,N1,DictIn,DictOut):-  
  kaggle_arc(TestID,(Trn+N1),In,Out),!,

  with_test_pairs(TestID,(Trn+N1),In,Out,
  must_det_ll((
  %detect_pair_hints(TestID,(Trn+N1),In,Out),
  pp(train_for_objects_from_1pair(DictIn,TestID,[Trn,'i',N1,'o',N1],In,Out,DictMid)),
  DictMid=DictIn,
  i_pair(complete,In,Out),
  N2 is N1 + 1,
  train_using_io(TestID,Trn,N2,DictMid,DictOut)))).
train_using_io(_TestID,_Trn,_,DictInOut,DictInOut):-!.

%:- thread_local(keep_going/0).

which_io0(i,in). which_io0(o,out).
which_io(I,In):- which_io0(I,In),!.
which_io(In,In):- which_io0(_,In),!.


train_for_objects_from_pair_with_mono(Dict0,TestID,Desc,In,Out,Dict9):- 
 must_det_ll((
  into_monochrome(In,MonoIn0), into_monochrome(Out,MonoOut0),
  ensure_other_grid(MonoIn0,MonoOut0),
  copy_term(MonoIn0,MonoIn),copy_term(MonoOut0,MonoOut),
 Desc = [_Trn,IsIO1,N1,IsIO2,N2], 
 MonoDesc = ['train_mono',IsIO1,N1,IsIO2,N2], 
  ensure_other_grid(In,Out),
  with_other_grid(Out,train_for_objects_from_1pair(Dict0,TestID,Desc,In,Out,Dict1)),!,
  nop(train_for_objects_from_1pair(Dict1,TestID,MonoDesc,MonoIn,MonoOut,Dict9)),!,
   ignore(Dict1=Dict9))),!.

cs(G):- call(G).
%cs(G):- w_section(train_for_objects_from_1pair1,G,debug).

train_for_objects_from_1pair(Dict0,TestID,Desc,InA,OutA,Dict1):-
  locally(set_prolog_flag(gc,true),
    train_for_objects_from_1pair1(Dict0,TestID,Desc,InA,OutA,Dict1)).

train_for_objects_from_1pair1(Dict0,_TestID,Desc,_InA,_OutA,Dict0):- Desc = [_Trn,'o',_N1,'o',_N2], !.


train_for_objects_from_1pair1(Dict0,TestID,Desc,InA,OutA,Dict1):-
 must_det_ll((
   Desc = [Trn,IsIO1,N1,IsIO2,N2], 
   which_io(IsIO1,IO1),
   which_io(IsIO2,IO2),
   atomic_list_concat([IO1,IO2],'_',ModeIn),
   atomic_list_concat([IO2,IO1],'_',ModeOut),
   atom_concat(IO1,N1,ION1),
   atom_concat(IO2,N2,ION2),
   atomic_list_concat([ION1,ION2],'_',ExampleNum),
 pp([train_for_objects_from_1pair1=ExampleNum,left=ION1,right=ION2]),
 
   %must_det_ll((%
   %garbage_collect,
    Dict0=Dict1,
    format('~N dict= '), pp(Dict0),
    
    %get_map_pairs(Dict0,_Type,Pairs),
    %list_to_rbtree_safe(Pairs,InVM),
    into_grid(InA,In), into_grid(OutA,Out),!,
    name_the_pair(TestID,ExampleNum,In,Out,PairName),
    grid_size(In,IH,IV), grid_size(Out,OH,OV),
    ignore((IH+IV \== OH+OV , writeln(io(size2D(IH,IV)->size2D(OH,OV))))),
    
    into_fti(TestID>(Trn+N1)*IO1,ModeIn,In,InVM),!,
    into_fti(TestID>(Trn+N2)*IO2,ModeOut,Out,OutVM),!,

   %InVM.compare=OutVM, 
   set(InVM.target_grid)=Out,
   %OutVM.compare=InVM, 
   set(OutVM.target_grid)=In,
   show_pair_grid(yellow,IH,IV,OH,OV,original(InVM.id),original(OutVM.id),PairName,In,Out),!,  
   individuate_c(InVM),!,
   individuate_c(OutVM),!,

   InC = InVM.objs,
   OutC = OutVM.objs,
  %print_info(InC),
  %print_info(OutC),
  %u_dmsg(InC=OutC),
  
  pred_intersection(overlap_same_obj,InC,OutC,RetainedIn,RetainedOut,Removed,Added),
  /*add_shape_lib(pair,RetainedIn),
  % add_shape_lib(pair,RetainedOut),
  add_shape_lib(removed(PairName),Removed),
  add_shape_lib(added(PairName),Added),*/
  
  dash_chars,dash_chars,dash_chars,dash_chars,
  show_pair_grid(cyan,IH,IV,OH,OV,original(InVM.id),original(OutVM.id),PairName,In,Out),!,
  max_min(IH,OH,IOH,_), max_min(IV,OV,IOV,_),
  luser_setval(no_rdot,true),
  ((Removed==Added, Removed==[]) -> pp(yellow,nothing_removed_added(PairName)) ;
  show_pair_diff_code(IOH,IOV,IOH,IOV,removed(PairName),added(PairName),PairName,Removed,Added)),
  ((RetainedIn==RetainedOut, RetainedIn==[]) -> pp(yellow,nothing_retained(PairName)) ;
  show_pair_diff_code(IH,IV,   OH, OV,retained(ION1),retained(ION2),PairName,RetainedIn,RetainedOut)),
  ((InC==OutC, InC==[]) -> pp(yellow,nothing_individuated(PairName)) ;
  show_pair_diff_code(IH,IV,   OH, OV,individuated1(ION1),individuated1(ION2),PairName,InC,OutC)),!, 
  luser_setval(no_rdot,false),
  % pp(OutC=InC),
  ignore(( learn_rule_o(ModeIn,InVM,OutVM))),  
  ignore(( ModeIn == in_out, Trn == trn, train_io_from_hint(TestID,Trn+N1,InVM))),
  dash_chars,dash_chars,dash_chars,dash_chars,
  print_testinfo(TestID))).

show_pair_diff_code(IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out):-
  show_pair_diff(IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out),
  dash_chars,dash_chars,
  nop(show_pair_code(In,Out)),!.

show_pair_code(In,Out):- 
  pp(purple,show_objs_as_code),
  dash_chars,
  show_objs_as_code(In),
  dash_chars,
  show_objs_as_code(Out),
  dash_chars,dash_chars.

% trials(learn). trials(clue).   
trials(human). 
trials(Sol):-trial_non_human(Sol).
% trials(dsl). trials(runDSL).
trial_non_human(sol).

sols_for(TestID,Trial,TrialSol):- 
 ensure_test(TestID),
 (var(Trial)->trials(Trial);true),
 once((compound_name_arguments(Entry,Trial,[Sol]), 
 test_info(TestID,Sols),member(Entry,Sols))),
  append_trial(Trial,Sol,TrialSol).

append_trial(Trial,Sol,TrialSol):- listify(Sol,SolL),
  ((appended_trial(Trial,TrialAppend), \+ append(_,TrialAppend,SolL)) -> append(SolL,TrialAppend,TrialSol) ;
    TrialSol = SolL).

appended_trial(human,[learn_rule]).



solve_test:- forall(trial_non_human(Trial),solve_test_trial(Trial)).

solve_test_trial(Trial):- mmake, with_test_pairs(TestID,ExampleNum,I,O,solve_test_trial_pair(Trial,TestID,ExampleNum,I,O)).

solve_test_trial_pair(Trial,TestID,ExampleNum,_I,_O):- 
 my_time((my_menu_call((catch(solve_test_trial(Trial,TestID,ExampleNum),E,
   u_dmsg(E=solve_test_trial(Trial,TestID,(ExampleNum)))))))),!.

solve_test_training_too:- 
 solve_test,
 my_menu_call((get_current_test(TestID), catch(solve_test_trial(Trial,TestID,(trn+A)),E,u_dmsg(E=solve_test_trial(Trial,TestID,(trn+A)))))),!.


solve_test(Name):- forall(trial_non_human(Trial),solve_test_trial(Trial,Name)).

solve_test_trial(Trial,Name):- 
  fix_test_name(Name,TestID,ExampleNum),!, 
  solve_test_trial(Trial,TestID,ExampleNum).

solve_test(TestID,ExampleNum):-
  forall(trial_non_human(Trial),solve_test_trial(Trial,TestID,ExampleNum)).

solve_test_trial(Trial,TestID,ExampleNum):-
 forall(kaggle_arc(TestID,ExampleNum,TestIn,ExpectedOut),
   ignore(solve_test_trial(Trial,TestID,ExampleNum,TestIn,ExpectedOut))).

solve_test(TestID,ExampleNum,TestIn,ExpectedOut):-
  forall(trial_non_human(Trial),solve_test_trial(Trial,TestID,ExampleNum,TestIn,ExpectedOut)).
  
solve_test_trial(Trial,TestID,ExampleNum,TestIn,ExpectedOut):-
   must_det_ll((    
    name_the_pair(TestID,ExampleNum,TestIn,ExpectedOut,PairName))),
   must_det_ll((       
    grid_size(TestIn,IH,IV), grid_size(ExpectedOut,OH,OV),
    ignore((IH+IV \== OH+OV , writeln(io(size2D(IH,IV)->size2D(OH,OV))))),
    print_testinfo(TestID))), 
   must_det_ll((
   try_easy_io(TestID>ExampleNum,TestIn,ExpectedOut),
    dash_chars, dash_chars,
    show_pair_grid(green,IH,IV,OH,OV,'Test TestIn','Solution ExpectedOut (Not computed by us)',PairName,TestIn,ExpectedOut),!,  
    get_training(Training))),
    flag(indiv,_,0),    
    into_fti(TestID>ExampleNum*in,in,TestIn,InVM),!,
    set(InVM.objs) = [],
    %set(InVM.lo_points) = [],
    %set(InVM.training) = Training,
    set_training(Training),
    maybe_set_vm(InVM),    
    gset(InVM.target_grid) = ExpectedOut,
    must_det_ll((
    %print(training(Training)),nl,
    %ppt(InVM),
    dash_chars, dash_chars,    
    %print_testinfo(TestID),
    do_sols_for(Trial,"Taking Test",InVM,TestID,ExampleNum))).

    % find indiviuation one each side that creates the equal number of changes

do_solve(InVM,SolutionProgram,GridOut):- run_dsl(InVM,SolutionProgram,InVM,GridOut).  

do_sols_for(Trial,Why,InVM,TestID,ExampleNum) :-
 must_det_ll(( ppt("BEGIN!!!"+Why+TestID>ExampleNum), 
    kaggle_arc_io(TestID,ExampleNum,out,ExpectedOut),
    set_target_grid(ExpectedOut),
    forall(sols_for(TestID,Trial,SolutionProgram),
     ignore(((
      once((pp(cyan,trial=Trial),
       ppt(cyan,run_dsl(TestID>ExampleNum,Trial,SolutionProgram)),!,
       (my_time((
              maybe_set_vm(InVM),
              kaggle_arc_io(TestID,ExampleNum,in,TestIn),
              gset(InVM.grid) = TestIn,
              maybe_set_vm(InVM),
              do_solve(InVM,SolutionProgram,GridOut)))*->!;GridOut=InVM.grid),
       into_pipe(GridOut,Solution)))
       *->    
      ignore((count_difs(ExpectedOut,Solution,Errors),
       print_side_by_side(blue,Solution,"Our Ran Solution",_,ExpectedOut,"Expected Solution"),
          (Errors==0 -> 
             arcdbg_info(green,pass(Why,TestID,ExampleNum,SolutionProgram))
             ; (banner_lines(red), arcdbg(fail(Why,Errors,TestID,ExampleNum,SolutionProgram)),
                test_info(TestID,InfoF),ppnl(fav(TestID>ExampleNum,InfoF)),
                banner_lines(red)))))

     

       ;arcdbg(warn(unrunable(TestID,ExampleNum,SolutionProgram))))))),
    print_side_by_side("our grid", InVM.grid, InVM.objs),!,
    print_list_of("our objs",InVM.objs),
    ppt("END!!!"+Why+TestID+ExampleNum))),!.


:- luser_linkval(test_rules,[rules]).
:- luser_linkval(pair_rules,[rules]).
  

reuse_indivs(IndvA,IndvB,BetterA,BetterB):-
  smallest_first(IndvA,IndvAS),
  smallest_first(IndvB,IndvBS),
  my_append(IndvAS,IndvBS,IndvCC), list_to_set(IndvCC,IndvC),
  smallest_first(IndvC,IndvCS),
  reuse_indivs_cleanup(IndvAS,IndvBS,IndvCS,BetterA,BetterB,_BetterC),!.

reuse_indivs_cleanup(IndvA,IndvB,IndvC,_,_,_):-
  my_maplist(length,[IndvA,IndvB,IndvC],Rest),
  u_dmsg(len=Rest),fail.
reuse_indivs_cleanup(IndvA,IndvB,IndvC,BetterAO,BetterBO,BetterCO):-
  select(A,IndvC,IndvCRest), member(B,IndvCRest),
  select(A,IndvA,IndvARest),
  select(A,IndvB,IndvBRest),
  reuse_a_b(A,B,AA),
  my_append(IndvARest,[AA],BetterA),
  my_append(IndvBRest,[B],BetterB),
  my_append(IndvCRest,[AA],BetterC),
  reuse_indivs_cleanup(BetterA,BetterB,BetterC,BetterAO,BetterBO,BetterCO),!.
reuse_indivs_cleanup(A,B,C,A,B,C).

%same_object(D)
reuse_a_b(A,B,AA):-
  findall(H,compare_objs1(H,A,B),How),
  obj_to_oid(B,BOID),
  obj_to_oid(A,_AOID),
  setq(A,oid(BOID),AA),
  object_glyph(A,GlyphA),
  object_glyph(B,GlyphB),
  ignore((How ==[]-> nop(pp(shared_object(GlyphB->GlyphA))); 
    (pp(same_object(GlyphA,GlyphB,How))))).

:- ensure_loaded('arc-dsl/dsl_solvers.pl').
