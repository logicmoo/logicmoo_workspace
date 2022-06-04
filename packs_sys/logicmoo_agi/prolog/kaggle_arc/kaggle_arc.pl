/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- encoding(iso_latin_1).
:- SL  is 2_147_483_648*8, set_prolog_flag(stack_limit, SL ).
:- set_prolog_flag(encoding,iso_latin_1).
:- set_prolog_flag(color_term,true).
:- set_stream(current_output, tty(true)).
:- stream_property(S,file_no(2)), set_stream(S,tty(true)).
:- stream_property(S,file_no(1)), set_stream(S,tty(true)).
:- dynamic(prolog:'$exported_op'/3).
:- assert((system:'$exported_op'(_,_,_):- fail)).
%:- multifile('$exported_op'/3).
:- system:ensure_loaded(library(logicmoo_common)).
%:- system:ensure_loaded(library(pfc_lib)).
%:- expects_dialect(pfc).
:- dynamic((fav/2,ap/1,apv/2)).
:- dynamic(cmem/3).
:- dynamic(grid_nums/1).
:- dynamic(grid_nums/2).
:- discontiguous grid_color_individualizer/19.
:- (getenv('DISPLAY',_) -> true ; setenv('DISPLAY','10.0.0.122:0.0')).
%:- (getenv('DISPLAY',_) -> guitracer ; true).
:- set_prolog_flag(toplevel_print_anon,true).
%:- set_prolog_flag(toplevel_print_factorized,true).
:- set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(20), attributes(portray)]).

:- multifile(decl_sf/1).
:- discontiguous(decl_sf/1).
:- dynamic(decl_sf/1).
decl_sf(G):- ground(G), !, assertz(decl_sf(G)).
:- multifile(decl_pt/1).
:- discontiguous(decl_pt/1).
:- dynamic(decl_pt/1).
decl_pt(G):- ground(G), !, assertz(decl_pt(G)).

:- ensure_loaded(kaggle_arc_utils).
:- ensure_loaded(kaggle_arc_ui_ansi).
:- ensure_loaded(kaggle_arc_domaintypes).
:- ensure_loaded(kaggle_arc_explaination).
:- ensure_loaded(kaggle_arc_howdiff).
:- ensure_loaded(kaggle_arc_imageproc).
:- ensure_loaded(kaggle_arc_db).
:- ensure_loaded(kaggle_arc_individuation).
:- ensure_loaded(kaggle_arc_interpreter).
:- ensure_loaded(kaggle_arc_test_iface).
:- ensure_loaded(kaggle_arc_object).
:- ensure_loaded(kaggle_arc_learning).
:- ensure_loaded(kaggle_arc_imagens).
:- ensure_loaded(kaggle_arc_recognise).
:- ensure_loaded(kaggle_arc_ui_html).

%c:- forall(clause(fav(A,B),true),add_history1((fav(A,B)))).
:- add_history1(arc).


:- forall((fav(_,P),flatten([P],Flat),member(E,Flat)), assert_if_new(fav_trait(E))).


run_nb(G):- call(G).
%run_nb(G):- setup_call_cleanup(G,true,notrace).

clsmake:- cls,mmake.
arc:- forall(arc1,true).
arc1:- clsmake, test_names_by_hard(X), arc1(X).
arc2:- clsmake, test_names_by_hard_rev(X), arc1(X).
arc3:- clsmake, arc1(v('009d5c81')).
arc4:- arc1(t('25d487eb')).
arc5:- clsmake, arc1(v('1d398264')).

fav:- forall(fav1,true).
fav1:- clsmake, test_names_by_fav(X), arc1(X).
fav2:- clsmake, test_names_by_fav_rev(X), arc1(X).
fav(X):- nonvar_or_ci(X),!, clsmake, arc1(X).
fav(X):- clause(fav(X,_),true).

arc(TestID):- forall(arc1(TestID),true).

arc1(TName):-    
 locally(set_prolog_flag(gc,true),
  (fix_test_name(TName,TestID,ExampleNum),   
  kaggle_arc(TestID,ExampleNum,In,Out),
  run_arc_io(TestID,ExampleNum,In,Out))).

run_arc_io(TestID,ExampleNum,In,Out):-
  time(show_arc_pair_progress(TestID,ExampleNum,In,Out)).


tie_break_individualizer(PairName,ImO,OmI,IMass,OMass,ShapesIO,ShapesIO):-
  grid_size(ImO,IH,IV), grid_size(OmI,OH,OV),
  show_pair_indivs(IH,IV,OH,OV,tie_break,PairName,ImO,OmI),
  ((IMass==0, OMass>0) -> USE = OmI;
   ((OMass==0, IMass>0) -> USE = ImO;
    ((OMass < IMass) -> USE = OmI;  USE = ImO))),
  individuate([],USE,ShapesIO),
  %nb_current(rules,Rules),
  %pt("USING-RULE"=Info+Rules),
  nop(print_grid(USE)).

gr2o(Grid,Obj):- localpoints(Grid,NoisePoints), make_indiv_object(NoisePoints,[object_shape(noise)],Obj),!.
%gr2o(Grid,Obj):- Grid=Obj.
  
show_pair_indivs(IH,IV,OH,OV,Info,PairName,ImO,OmI):-
  %nb_current(rules,Rules),
  %add_rule( Info),
  %add_grule( PairName*in(Info),ImO),
  %add_grule( PairName*out(Info),OmI),
  show_pair(IH,IV,OH,OV,Info,PairName,ImO,OmI).
  

grid_color_individualizer(PairName,In,Out,IH,IV,OH,OV,
    ICs,IPCs,CommonCs,OPCs,OCs,
    ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
    ShapesI,ShapesO):- %fail,
  CommonCsL>0,
  IPCsL>0, OPCsL==0,
  once((
  add_cond(hasCommonColors(pair,CommonCs)),
  add_cond(hasPrivateColor(out,OPCs)),
  add_cond(hasPrivateColor(in,IPCs)),
  do_action(delete_colors(CommonCs,Out,OmI)),
  do_action(delete_colors(CommonCs,In,ImO)),
  show_pair_indivs(IH,IV,OH,OV,grid_color_individualizer,PairName,ImO,OmI),
  mass(ImO,IMass),mass(OmI,OMass),
  %one_is_zero(IMass,OMass),
  individuate([options([full])],ImO,NoiseObject), do_action(add_shape_lib(noise,NoiseObject)),
  individuate([options([by_color(IPCs)])],ImO,NewImO), do_action(add_shape_lib(noise,NewImO)),
  tie_break_individualizer(PairName,ImO,OmI,IMass,OMass,ShapesI,ShapesO))).


grid_color_individualizer(PairName,In,Out,IH,IV,OH,OV,
    ICs,IPCs,CommonCs,OPCs,OCs,
    ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
    ShapesI,ShapesO):- 
  CommonCsL>0,
  one_is_zero(IPCsL,OPCsL),
  add_cond(hasCommonColors(pair,CommonCs)),
  add_cond(hasPrivateColor(out,OPCs)),
  add_cond(hasPrivateColor(in,IPCs)),
  do_action(delete_colors(OPCs,Out,OmI)),
  delete_colors(IPCs,In,ImO),
  % mass(In,InMass),mass(Out,OutMass),
  mass(ImO,IMass),mass(OmI,OMass),
  IMass>0,OMass>0, OPCsL == 0,
  ShapesI = options([solid(squares),defaults]),
  individuate([options([solid(squares)])],ImO,NewImO), do_action(add_shape_lib(pair,NewImO)),
  add_indiv(in,ShapesI),
  add_comparitor(-size),
  show_pair_indivs(IH,IV,OH,OV,'Filter noise',PairName,ImO,OmI),
  add_action(show_pair_indivs),
  individualizer_from_grid(PairName,out,Out,In,ShapesO),!.



one_is_zero(IMass,OMass):- 
  once(IMass>0;OMass>0),once(IMass=:=0;OMass=:=0).

delete_colors([],Out,Out):-!.
delete_colors([C|IPLs],In,Out):- 
 subst_w_attv(In,C,black,Mid),
 delete_colors(IPLs,Mid,Out).


individualizer(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO):-
  (individualizers_from_pair(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO)
    *->true;individualizer_fallback(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO)).

individualizers_from_pair(PairName,In,Out,H,V,H,V,ShapesI,ShapesO):-
  add_note("trying grid minus grid"),
  grid_minus_grid(In,Out,ImO),mass(ImO,IMass),
  grid_minus_grid(Out,In,OmI),mass(OmI,OMass),
  one_is_zero(IMass,OMass),
  tie_break_individualizer(PairName,ImO,OmI,IMass,OMass,ShapesI,ShapesO).

individualizers_from_pair(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO):- 
  add_note("trying unique_colors per grid"),
  unique_colors(In,ICs), unique_colors(Out,OCs),
  intersection(ICs,OCs,CommonCs,IPCs,OPCs),
  maplist(length,[ICs,IPCs,CommonCs,OPCs,OCs],[ICsL,IPCsL,CommonCsL,OPCsL,OCsL]),
  grid_color_individualizer(PairName,In,Out,
    IH,IV,OH,OV,
    ICs,IPCs,CommonCs,OPCs,OCs,
    ICsL,IPCsL,CommonCsL,OPCsL,OCsL,
    ShapesI,ShapesO).

individualizer_fallback(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO):- 
	individualizer_from_grid(PairName,in,In,IH,IV,Out,OH,OV,ShapesI),
	individualizer_from_grid(PairName,out,Out,OH,OV,In,IH,IV,ShapesO),!.
/*
individualizer_fallback(_PairName,In,Out,_IH,_IV,_OH,_OV,ShapesI,ShapesO):- 
  individuate([],Out,UnsharedOut),
  individuate([],In,UnsharedIn),  
  maplist(length,[UnsharedIn,UnsharedOut],[IMass,OMass]),
  ((OMass>IMass) -> individuate(UnsharedIn,Out,ShapesI);
   (IMass>OMass) -> individuate(UnsharedOut,In,ShapesO)),
  add_rule(less_shapes(IMass,OMass)).
*/
/*
  individuate([],Out,UnsharedOut),
  individuate([],In,UnsharedIn),  
  maplist(length,[UnsharedIn,UnsharedOut],[IMass,OMass]),
  ((OMass>IMass) -> individuate(UnsharedIn,Out,ShapesI);
   (IMass>OMass) -> individuate(UnsharedOut,In,ShapesO)),
  add_rule(less_shapes(IMass,OMass)).
*/
individualizer_from_grid(PairName,InOrOut,In,Out,ShapesO):-
  grid_size(In,IH,IV), grid_size(Out,OH,OV),
  individualizer_from_grid(PairName,InOrOut,In,IH,IV,Out,OH,OV,ShapesO).

individualizer_from_grid(_PairName,_InOrOut,_In,IH,IV,_Out,_OH,_OV,ShapesO):-
   (IH<6;IV<6),!,make_indivs_options([retain_grid(full),dots],ShapesO).
/*

individualizer_from_grid(_PairName,_InOrOut,_In,IH,IV,_Out,_OH,_OV,ShapesO):-
  (IH>15,IV>15),!,make_indivs_options([-(=(dots)),fourway,defaults],ShapesO).
  */
individualizer_from_grid(_PairName,_InOrOut,_In,_IH,_IV,_Out,_OH,_OV,[]):-!.
  


print_collapsed(G):-
  wots(_,G). 

:- nb_setval(rules,[rules]).
examine_installed_individualizers_from_pairs(PairName,In,Out,IH,IV,OH,OV):-
  add_note(examine_installed_individualizers_from_pairs),
  individualizer(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO),
  %nb_current(rules, Info),
  %format('~N+Done with Ideas~N'),
  show_idea(PairName,In,Out,IH,IV,OH,OV,ShapesI,ShapesO),
  nb_linkval(rules, [rules]).
  
show_idea(PairName,In,Out,IH,IV,OH,OV,Shapes_I,Shapes_O):- 
  make_indivs_options(Shapes_I,ShapesII),
  make_indivs_options(Shapes_O,ShapesOO),
 
  show_rules,
  show_pair_indivs(IH,IV,OH,OV,shape,PairName,ShapesII,ShapesOO),
  format('~N+individuate(IN)~N'),
  expand_individualizer(ShapesII,ShapesI),individuate(ShapesI,In,UnsharedIn),
  format('~N+individuate(OUT)~N'),
  expand_individualizer(ShapesOO,ShapesO),individuate(ShapesO,Out,UnsharedOut),
  format('~N-individuate~N'),
  show_pair_indivs(IH,IV,OH,OV,unshared,PairName,UnsharedIn,UnsharedOut),
  format('~N-unshared~N'),
  %pt(yellow,in=UnsharedIn),
  pred_intersection(compare_objs1([same]),UnsharedIn,UnsharedOut,CommonCsIn,CommonCsOut,IPCs,OPCs),
  nop((
    format('~N-pred_intersection~N'),
    individuate(UnsharedOut,Out,SharedInR),
    individuate(UnsharedIn,In,SharedOutR),
  show_pair_indivs(IH,IV,OH,OV,shared,PairName,SharedInR,SharedOutR))),
  format('~N-Rule made from~N'),
  show_rules,
  RESS =.. [res,unsharedIn=UnsharedIn,
             %onlyIn= IPCs,commonIn=CommonCsIn,commonOut=CommonCsOut,onlyOut=OPCs, 
             unsharedOut=UnsharedOut],
  tersify(RESS,ShortInfo),         
  format('~N-Stats:~N'),
  pt(yellow,pair=ShortInfo), !.

expand_individualizer(Shapes,Shapes):- \+ is_group(Shapes),!.
expand_individualizer(Shapes,SmallLib):- must_be_free(SmallLib),
  must_det_l((print_collapsed(
  show_workflow(Shapes,
   [ =,"Vanila indivs",
    % searchable,"Searchable indivs", 
       all_rotations, % "All rotations of indivs", 
       add(change_color_blue), "Add blue indivs", 
       % add(change_color), % "Add new colors indivs",    
		 
    %decolorize % decolorized points are not yet printable 
    =],SmallLib)
    ))).

  
show_arc_pair_progress(TestID,ExampleNum,In,Out):-
	name_the_pair(TestID,ExampleNum,In,Out,PairName),
	grid_size(In,IH,IV), grid_size(Out,OH,OV),
	nop(writeln(grid_convert(size(IH,IV)->size(OH,OV)))),
	ignore((more_task_info(TestID,III),pt(III),nl)), 
	show_pair(IH,IV,OH,OV,test,PairName,In,Out),
	/*nb_linkval(rules, [rules]),	
	individualizer_from_grid(PairName,in,In,IH,IV,Out,OH,OV,ShapesI_S),
	individualizer_from_grid(PairName,out,Out,OH,OV,Out,IH,IV,ShapesO_S),
	show_idea("Individuals",PairName,In,Out,IH,IV,OH,OV,ShapesI_S,ShapesO_S),*/
	forall(examine_installed_individualizers_from_pairs(PairName,In,Out,IH,IV,OH,OV),true),
	!.
	 	 
	
/*
  
  nop((reuse_indivs(SharedIn,SharedOut,BetterA,BetterB),
  ( (SharedOut\==BetterB ; SharedIn\== BetterA) ->
    show_pair(IH,IV,OH,OV,better,PairName,BetterA,BetterB);
     writeln('nothing better')))),
*/

reuse_indivs(IndvA,IndvB,BetterA,BetterB):-
  smallest_first(IndvA,IndvAS),
  smallest_first(IndvB,IndvBS),
  append(IndvAS,IndvBS,IndvCC), list_to_set(IndvCC,IndvC),
  smallest_first(IndvC,IndvCS),
  reuse_indivs_cleanup(IndvAS,IndvBS,IndvCS,BetterA,BetterB,_BetterC),!.

reuse_indivs_cleanup(IndvA,IndvB,IndvC,_,_,_):-
  maplist(length,[IndvA,IndvB,IndvC],Rest),
  wdmsg(len=Rest),fail.
reuse_indivs_cleanup(IndvA,IndvB,IndvC,BetterAO,BetterBO,BetterCO):-
  select(A,IndvC,IndvCRest), member(B,IndvCRest),
  select(A,IndvA,IndvARest),
  select(A,IndvB,IndvBRest),
  reuse_a_b(A,B,AA),
  append(IndvARest,[AA],BetterA),
  append(IndvBRest,[B],BetterB),
  append(IndvCRest,[AA],BetterC),
  reuse_indivs_cleanup(BetterA,BetterB,BetterC,BetterAO,BetterBO,BetterCO),!.
reuse_indivs_cleanup(A,B,C,A,B,C).

%same_object(D)
reuse_a_b(A,B,AA):-
  findall(H,compare_objs1(H,A,B),How),
  object_indv_id(B,ID,Iv),
  setq(A,object_indv_id(ID,Iv),AA),
  object_glyph(A,GlyphA),
  object_glyph(B,GlyphB),
  ignore((How ==[]-> nop(pt(shared_object(GlyphB->GlyphA))); 
    (pt(same_object(GlyphA,GlyphB,How))))).


:- learn_shapes.

