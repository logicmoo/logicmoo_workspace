/*
  this is part of (H)MUARC

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


:- dynamic(is_gridname/2).

set_gridname(Grid,Name):- nb_setval(grid_name,Name),
  assertion((ground(Name),nonvar_or_ci(Grid))),
  asserta_new(is_gridname(Grid,Name)).

%get_gridname(Grid,Name):- is_gridname(Grid,Name)*->true; (plain_var(Name)->(nb_current(grid_name,Name),Name\=[],get_gridname(Grid,Name))).
get_gridname(Grid,Name):- is_gridname(Grid,Name).
get_gridname(In,Name*ExampleNum*in):- kaggle_arc(Name,ExampleNum,In,_).
get_gridname(Out,Name*ExampleNum*out):- kaggle_arc(Name,ExampleNum,_,Out).
get_gridname(Grid,Name):- is_grid(Grid),must_be_free(Name),gensym('grid_',Name),asserta(is_gridname(Grid,Name)).

into_gridname(G,TstName):- nonvar_or_ci(G), into_gridnameA(GVar,TstName),G=@=GVar,!.
into_gridname(G,TstName):- makeup_gridname(TstName),
  set_gridname(G,TstName).

makeup_gridname(GridName):- gensym('GridName_',TstName), 
   GridName = TstName*('ExampleNum'+0)*io.

into_gridnameA(G,Name*ExampleNum*in):- kaggle_arc(Name,ExampleNum,G,_).
into_gridnameA(G,Name*ExampleNum*out):- kaggle_arc(Name,ExampleNum,_,G).
into_gridnameA(G,TstName):- is_gridname(G,TstName).
into_gridnameA(G,TstName):- is_shared_saved(TstName,G).
into_gridnameA(G,TstName):- is_unshared_saved(TstName,G).
into_gridnameA(G,TstName*T):- fix_test_name(TstName+T,Name,ExampleNum),kaggle_arc(Name,ExampleNum,G,_).
into_gridnameA(G,TstName):- learned_color_inner_shape(TstName,magenta,BG,G,_),get_bgc(BG).


kaggle_arc_db(Name,Example,Num,out,G):- kaggle_arc(Name,Example+Num,_,G).
kaggle_arc_db(Name,Example,Num,in,G):- kaggle_arc(Name,Example+Num,G,_).

maybe_confirm_sol(Name,ExampleNum,In,Out):- 
   ignore((sols_for(Name,Sol), confirm_sol(Sol,Name,ExampleNum,In,Out))),!.

sols_for(Name,Sol):- test_info(Name,Sols),member(lmDSL(Sol),Sols).

confirm_sol(Prog,Name,ExampleNum,In,Out):- 
   (run_dsl(Prog,In,Grid),into_grid(Grid,GridO))
   *->    
   count_difs(Out,GridO,Errors),
   (Errors==0 -> arcdbg(pass(Name,ExampleNum,Prog));(arcdbg(fail(Errors,Name,ExampleNum,Prog)),
     test_info(Name,InfoF),
     wqnl(fav(Name*ExampleNum,InfoF)),
     red_noise,
     once(print_grid(GridO)),
     red_noise))
   ;arcdbg(warn(unrunable(Name,ExampleNum,Prog))).


describe_feature(Grid,List):- is_list(List),!,maplist(describe_feature(Grid),List).
describe_feature(_,call(Call)):- !, call(Call).
describe_feature(Grid,Pred):- is_pointless(Grid), !, as_debug(9,pt(usupported_call(Pred,Grid))).
describe_feature(Grid,Pred):- call(Pred,Grid,Res)->print_equals(Grid,Pred,Res);print_equals(Pred,f),!.


is_pointless(O):- \+ is_gridoid(O).

combine_grids(_,[G],G):-!.
combine_grids(How,[G1,G2|Gs],GO):- combine_grids(How,[G2|Gs],G1,GO).

combine_grids(_How,[],G,G):-!.
combine_grids(How,[H|T],G,GO):- !,
  %in_cmt((writeln(How),print_grid(H))),
  combine_grids(How,H,G,GM),
  combine_grids(How,T,GM,GO).
combine_grids(overlay,H,G,GM):- globalpoints(H,Points),set_local_points(Points,G,GM),!.
combine_grids(append,H,G,GM):- grid_size(H,W,_),length(Row,W), append(G,[Row|H],GM).
  
debug_indiv:- test_config(nodebug_indiv),!,fail.
debug_indiv:- test_config(debug_indiv),!.
debug_indiv:- test_config(indiv(_)),!.

will_show_grid(Obj,true):- mass(Obj,Mass)-> (Mass>4, vis_hv(Obj,H,V) -> (H>1,V>1)),!.
will_show_grid(_,false).

:- discontiguous debug_indiv/1. 

debug_indiv(Var):- plain_var(Var),pt(debug_indiv(Var)),!.

debug_as_grid(Grid):-
  grid_size(Grid,H,V),
  dash_char(H),
  wqnl(debug_indiv_grid(H,V)),
  print_grid(Grid),
  dash_char(H),!.


debug_indiv(Grid):- is_grid(Grid),!,debug_as_grid(Grid).
debug_indiv(Grid):- maplist(is_cpoint,Grid),!,debug_as_grid(Grid).
debug_indiv(Grid):- maplist(is_point,Grid),!,debug_as_grid(Grid).


debug_indiv(List):- is_list(List),length(List,Len),!,
  dash_char,
  wqnl(objs = Len),
  max_min(Len,40,_,Min),
  forall(between(1,Min,N),(N<40->(nth1(N,List,E),debug_indiv(E));wqnl(total = Len))),
  dash_char,!.


debug_indiv(obj(A)):- \+ is_list(A),!, pt(debug_indiv(obj(A))).

/*
debug_indiv(A):- is_point_obj(A,Color,Point),
  object_indv_id(A,Tst,Id), i_glyph(Id,Sym),
  hv_point(H,V,Point), i_glyph(Id,Sym),
  wqnl([' % Point: ', color_print(Color,Sym), dot, color(Color), fav1(Tst), nth(Id), loc_xy(H,V)]),!. 
*/

debug_indiv(obj(A)):- Obj = obj(A), is_list(A),!,
  ignore((o2g(Obj,GGG), nonvar(GGG),asserta(g2o(GGG,Obj)))),
%debug_indiv(Obj):- Obj = obj(A), is_list(A),  
  once(colors(Obj,[cc(FC,_)|_]);FC=9),
  sort_obj_props(A,AS),
  will_show_grid(Obj,TF),
  remove_too_verbose(AS,TV0), include('\\=='(''),TV0,TV),
  flatten(TV,F),predsort(longer_strings,F,[Caps|_]),
  toPropercase(Caps,PC),
  %i_glyph(Id,Sym), wqnl([writef("%% %Nr%w \t",[PC]), color_print(FC,Sym) | AAAA ]),!. 
  object_glyph(Obj,Glyph),  
  ignore((TF==true,dash_char)),
  wqnl([format("%  ~w:\t",[PC]), color_print(FC,Glyph) | TV ]),!,
  ignore(( TF==true, mass(Obj,Mass),!,Mass>4, vis_hv(Obj,H,V),!,H>1,V>1, localpoints(Obj,Points), print_grid(H,V,Points))),
  ignore(( fail, mass(Obj,Mass),!,Mass>4, vis_hv(Obj,H,V),!,H>1,V>1, show_st_map(Obj))),
  ignore((TF==true,dash_char)),!.

show_st_map(Obj):-
  ignore(( 
  localpoints(Obj,Points),
%  mass(Obj,Mass),!,Mass>4,
%  vis_hv(Obj,H,V),!,H>1,V>1,
  format('~N'),
  solidness(Points,0,inf,Res),
  solidness_no_diag(Points,0,inf,ResND),
  solidness_is_diag(Points,0,inf,ResD),
  print_side_by_side(print_side_by_side(ResND,ResD),Res))).

debug_indiv(obj(A)):- is_list(A),!, 
  dash_char,  
  maplist(debug_indiv(obj(A)),A),
  dash_char,!.

debug_indiv([]):- !.

debug_indiv(diff(_)).
debug_indiv([Other]):-!,debug_indiv(Other).
debug_indiv(P):- is_rule(P,Q),
  dash_char,
  pt(Q),
  dash_char,!.

is_rule(P,_):- \+ compound(P),!,fail.
is_rule(A:-true,A):-!.
is_rule(A:-B,A:-B):-!.


debug_indiv(Other):-
  dash_char,
  functor(Other,F,A),
  wqnl(other = F/A),
  pt(Other),
  dash_char,!.

debug_indiv(Obj,P):- compound(P),!,compound_name_arguments(P,F,A),debug_indiv(Obj,P,F,A).

priority("bckgrnd",0).
priority("point",0).
priority(A,1):- atom_contains(A,")").
priority(_,2).
longer_strings(R,A,B):- string(A),string(B),priority(A,PA),priority(B,PB),atom_length(A,AL),atom_length(B,BL),compare(R,PA+AL+A,PB+BL+B).
longer_strings(R,A,B):- obj_prop_sort_compare(R,A,B).

remove_too_verbose(Var,plain_var(Var)):- plain_var(Var),!.
remove_too_verbose(H,''):- too_verbose(H),!.

remove_too_verbose(dot,"point"):- !.
%remove_too_verbose(line(HV),S):- sformat(S,'~w-Line',[HV]).
%remove_too_verbose(square,S):- sformat(S,'square',[]).
remove_too_verbose(background,S):- sformat(S,'bckgrnd',[]).
remove_too_verbose(object_shape(H),S):- !, remove_too_verbose(H,HH),sformat(S,'~w',[HH]).
remove_too_verbose(colors(H),HH):- !, remove_too_verbose(H,HH).
remove_too_verbose(object_indv_id(_ * _ * X,Y),[layer(XX),nth(Y)]):- =(X,XX).
remove_too_verbose(object_indv_id(_ * X,Y),[layer(XX),nth(Y)]):- =(X,XX).
remove_too_verbose(loc_xy(X,Y),loc_xy(X,Y)).
remove_too_verbose(vis_hv(X,Y),size(X,Y)).
remove_too_verbose(changes([]),'').
remove_too_verbose(rotation(same),'').
remove_too_verbose(L,LL):- is_list(L),!, maplist(remove_too_verbose,L,LL).
remove_too_verbose(H,H).
too_verbose(P):- compound(P),compound_name_arity(P,F,_),!,too_verbose(F).
too_verbose(globalpoints).
too_verbose(shape).
too_verbose(localpoints).
too_verbose(grid).
too_verbose(grid_size).
too_verbose(rotated_grid).


debug_indiv(_,_,X,_):- too_verbose(X),!.
debug_indiv(Obj,_,F,[A]):- maplist(is_cpoint,A),!,
  vis_hv(Obj,H,V), wqnl(F), 
  loc_xy(Obj,OH,OV),
  EH is OH+H-1,
  EV is OV+V-1,
  object_glyph(Obj,Glyph),
  Pad1 is floor(H),  

  wots(S,
    (dash_char(Pad1,' '),write(Id=Glyph),
     print_grid(OH,OV,OH,OV,EH,EV,EH,EV,Obj))),

  nop(wots(S,
    (dash_char(Pad1,' '),write(Id=Glyph),
     print_grid(H,V,A)))),


  Pad is floor(20-V/2),
  max_min(Pad,OH,PadH,_),
  print_w_pad(PadH,S).


debug_indiv(_,P,_,_):- pt(P).


