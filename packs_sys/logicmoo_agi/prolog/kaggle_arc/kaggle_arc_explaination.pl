


:- dynamic(is_gridname/2).

set_gridname(Grid,Name):- nb_setval(grid_name,Name),
  assertion((ground(Name),nonvar(Grid))),
  asserta_new(is_gridname(Grid,Name)).

%get_gridname(Grid,Name):- is_gridname(Grid,Name)*->true; (var(Name)->(nb_current(grid_name,Name),Name\=[],get_gridname(Grid,Name))).
get_gridname(Grid,Name):- is_gridname(Grid,Name).
get_gridname(In,Name*ExampleNum*in):- kaggle_arc(Name,ExampleNum,In,_).
get_gridname(Out,Name*ExampleNum*out):- kaggle_arc(Name,ExampleNum,_,Out).
get_gridname(Grid,Name):- gensym('grid_',Name),asserta(is_gridname(Grid,Name)).

into_gridname(G,TstName):- nonvar(G), into_gridname(GVar,TstName),G=@=GVar,!.
into_gridname(G,Name*ExampleNum*in):- kaggle_arc(Name,ExampleNum,G,_).
into_gridname(G,Name*ExampleNum*out):- kaggle_arc(Name,ExampleNum,_,G).
into_gridname(G,TstName):- is_gridname(G,TstName).
into_gridname(G,TstName):- is_shared_saved(TstName,G).
into_gridname(G,TstName):- is_unshared_saved(TstName,G).
into_gridname(G,TstName*T):- fix_test_name(TstName+T,Name,ExampleNum),kaggle_arc(Name,ExampleNum,G,_).
into_gridname(G,TstName):- learned_color_inner_shape(TstName,magenta,BG,G,_),get_bgc(BG).


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
describe_feature(Grid,Pred):- call(Pred,Grid,Res)->print_equals(Grid,Pred,Res);print_equals(Pred,f),!.


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

print_info(A):- debug_indiv(A).

debug_indiv(Var):- var(Var),pt(debug_indiv(Var)),!.

debug_indiv(Grid):- is_grid(Grid),!,grid_size(Grid,H,V),
  dash_char(H),
  wqnl(debug_indiv_grid(H,V)),
  print_grid(Grid),
  dash_char(H),!.

debug_indiv(obj(A)):- \+ is_list(A),!, pt(debug_indiv(obj(A))).

/*
debug_indiv(A):- is_point_obj(A,Color,Point),
  object_indv_id(A,Tst,Id), i_glyph(Id,Sym),
  hv_point(H,V,Point), i_glyph(Id,Sym),
  wqnl([' % Point: ', color_print(Color,Sym), dot, color(Color), fav1(Tst), nth(Id), offset(H,V)]),!. 
*/
debug_indiv(obj(A)):- Obj = obj(A), is_list(A),!,
%debug_indiv(Obj):- Obj = obj(A), is_list(A),
  object_indv_id(Obj,_,Id),
  i_sym(Id,Sym),
  ignore(colors_count(Obj,[color_count(FC,_)|_])),
  remove_too_verbose(A,AA), 
  flatten([AA],F),
  predsort(longer_strings,F,AAA),
  include('\\=='(''),AAA,[Caps|AAAA]),
  toPropercase(Caps,PC), 
  %i_glyph(Id,Sym), wqnl([writef("%% %Nr%w \t",[PC]), color_print(FC,Sym) | AAAA ]),!. 
  i_glyph(Sym,Glyph), wqnl([format("%  ~w:\t",[PC]), color_print(FC,Glyph) | AAAA ]),!. 

debug_indiv(obj(A)):- is_list(A),!, 
  dash_char,  
  maplist(debug_indiv(obj(A)),A),
  dash_char,!.

debug_indiv(List):- is_list(List),!,length(List,Len), 
  dash_char,
  wqnl(list = Len),
  max_min(Len,40,_,Min),
  forall(between(1,Min,N),(N<40->(nth1(N,List,E),debug_indiv(E));wqnl(total = Len))),
  dash_char,!.

debug_indiv(Other):-
  dash_char,
  functor(Other,F,A),
  wqnl(other = F/A),
  pt(Other),
  dash_char,!.

priority("bckgrnd",0).
priority("point",0).
priority(A,1):- atom_contains(A,")").
priority(_,2).
longer_strings(R,A,B):- string(A),string(B),priority(A,PA),priority(B,PB),atom_length(A,AL),atom_length(B,BL),compare(R,PA+AL+A,PB+BL+B).
longer_strings(R,A,B):- compare(R,A,B).

remove_too_verbose(Var,var(Var)):- var(Var),!.
remove_too_verbose(H,''):- too_verbose(H),!.
remove_too_verbose(dot,"point"):- !.
%remove_too_verbose(line(HV),S):- sformat(S,'~w-Line',[HV]).
%remove_too_verbose(square,S):- sformat(S,'square',[]).
remove_too_verbose(background,S):- sformat(S,'bckgrnd',[]).
remove_too_verbose(object_shape(H),S):- !, remove_too_verbose(H,HH),sformat(S,'~w',[HH]).
remove_too_verbose(colors_count(H),HH):- !, remove_too_verbose(H,HH).
remove_too_verbose(object_indv_id(_ * _ * X,Y),[layer(XX),nth(Y)]):- =(X,XX).
remove_too_verbose(object_indv_id(_ * X,Y),[layer(XX),nth(Y)]):- =(X,XX).
remove_too_verbose(object_offset(X,Y),offset(X,Y)).
remove_too_verbose(object_size(X,Y),size(X,Y)).
remove_too_verbose(point_count(X),points(X)).
remove_too_verbose(L,LL):- is_list(L),!, maplist(remove_too_verbose,L,LL).
remove_too_verbose(H,H).
too_verbose(P):- compound(P),compound_name_arity(P,F,_),!,too_verbose(F).
too_verbose(globalpoints).
too_verbose(localpoints_nc).
too_verbose(localpoints).
too_verbose(grid).
too_verbose(grid).
too_verbose(rotated_grid).

debug_indiv(Obj,P):- compound_name_arguments(P,F,A),debug_indiv(Obj,P,F,A).
debug_indiv(_,_,X,_):- too_verbose(X),!.
debug_indiv(Obj,_,F,[A]):- maplist(is_cpoint,A),!,
  object_size(Obj,H,V), wqnl(F), 
  object_offset(Obj,OH,OV),
  EH is OH+H-1,
  EV is OV+V-1,
  object_indv_id(Obj,_Tst,Id), %i_sym(Id,Sym),
  i_glyph(Id,Glyph),
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


