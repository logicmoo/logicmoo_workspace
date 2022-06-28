/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


:- dynamic(is_grid_id/2).

set_grid_id(Grid,ID):-
  my_assertion((ground(ID),nonvar_or_ci(Grid))),
  my_assertion(\+ is_grid(ID)),
  nb_setval(grid_name,ID),
  ignore(( \+ into_gridnameA(Grid,ID),
  copy_term(Grid,GGrid),numbervars(GGrid,1,_),
  asserta(is_grid_id(GGrid,ID)))).

:- dynamic(is_grid_id/2).

%grid_to_id(Grid,Name):- is_grid_id(Grid,Name)*->true; (plain_var(Name)->(nb_current(grid_name,Name),Name\=[],grid_to_id(Grid,Name))).



kaggle_arc_db(Name,Example,Num,out,G):- kaggle_arc(Name,Example+Num,_,G).
kaggle_arc_db(Name,Example,Num,in,G):- kaggle_arc(Name,Example+Num,G,_).


into_pipe(Grid,Grid):- !. % into_group
into_pipe(Grid,Solution):- into_grid(Grid,Solution).

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
combine_grids(my_append,H,G,GM):- grid_size(H,W,_),length(Row,W), my_append(G,[Row|H],GM).
  
debug_indiv:- test_config(nodebug_indiv),!,fail.
debug_indiv:- test_config(debug_indiv),!.
debug_indiv:- test_config(indiv(_)),!.

will_show_grid(Obj,true):- mass(Obj,Mass)-> (Mass>4, vis_hv(Obj,H,V) -> (H>1,V>1)),!.
will_show_grid(_,false).



  


print_list_of(_,[]):-!.
print_list_of(N,OO):-
 sort(OO,O),
 (N\=[] -> pt(N); true),
  %save_grouped(print_list_of(N),O),
   maplist(print_info,O).

print_info(A):- is_grid(A),print_grid(A).
print_info(A):- is_object(A), ignore(debug_indiv(A)).
print_info(A):- is_group(A),debug_indiv(A).
%print_info(A):- into_obj(A,Obj),print_info(Obj).
print_info([]):-!.
print_info(A):- pt(A).


:- discontiguous debug_indiv/1. 

debug_indiv(Var):- plain_var(Var),pt(debug_indiv(Var)),!.

debug_as_grid(Grid):-
  grid_size(Grid,H,V),
  dash_chars(H),
  wqnl(debug_indiv_grid(H,V)),
  print_grid(Grid),
  dash_chars(H),!.

debug_indiv(_):- is_print_collapsed,!.
debug_indiv(Grid):- is_grid(Grid),!,debug_as_grid(Grid).
debug_indiv(Grid):- maplist(is_cpoint,Grid),!,debug_as_grid(Grid).
debug_indiv(Grid):- maplist(is_point,Grid),!,debug_as_grid(Grid).


debug_indiv(List):- is_list(List),length(List,Len),!,
  dash_chars,
  wqnl(objs = Len),
  max_min(Len,40,_,Min),
  forall(between(1,Min,N),(N<40->(nth1(N,List,E),debug_indiv(E));wqnl(total = Len))),
  dash_chars,!.


debug_indiv(obj(A)):- \+ is_list(A),!, pt(debug_indiv(obj(A))).

/*
debug_indiv(A):- is_point_obj(A,Color,Point),
  object_indv_id(A,Tst,Id), i_glyph(Id,Sym),
  hv_point(H,V,Point), i_glyph(Id,Sym),
  wqnl([' % Point: ', color_print(Color,Sym), dot, color(Color), fav1(Tst), nth(Id), loc_xy(H,V)]),!. 
*/

debug_indiv(obj(A)):- \+ \+ debug_indiv_obj(A).
debug_indiv_obj(A):- Obj = obj(A), is_list(A),!,
 must_det_ll((
  ignore((o2g(Obj,GGG), nonvar(GGG),nb_setval(GGG,Obj), nop( my_asserta_if_new(g2o(GGG,Obj))))),
%debug_indiv(Obj):- Obj = obj(A), is_list(A),  
  once(colors(Obj,[cc(FC,_)|_]);FC=grey),
  sort_obj_props(A,AS),
 % will_show_grid(Obj,TF),
  TF = false,
  remove_too_verbose(AS,TV0), include(not_too_verbose,TV0,TV),
  flatten(TV,F),predsort(longer_strings,F,[Caps|_]),
  toPropercase(Caps,PC),
  %i_glyph(Id,Sym), wqnl([writef("%% %Nr%w \t",[PC]), color_print(FC,Sym) | AAAA ]),!. 
  object_glyph(Obj,Glyph),  
  ignore((TF==true,dash_chars)),
  ignore((is_colorish(FC) -> wqnl([format("%  ~w:\t",[PC]), color_print(FC,Glyph) | TV ]);
                             wqnl([format("%  ~w:\t",[PC]), color_print(Glyph) | TV ]))),
  ignore(( TF==true, mass(Obj,Mass),!,Mass>4, vis_hv(Obj,H,V),!,H>1,V>1, localpoints(Obj,Points), print_grid(H,V,Points))),
  ignore(( fail, mass(Obj,Mass),!,Mass>4, vis_hv(Obj,H,V),!,H>1,V>1, show_st_map(Obj))),
  %pt(A),
  ignore((TF==true,dash_chars)))),!.

not_too_verbose(X):- X\==(''), X\==s('').

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
  dash_chars,  
  maplist(debug_indiv(obj(A)),A),
  dash_chars,!.

debug_indiv([]):- !.

debug_indiv(diff(_)).
debug_indiv([Other]):-!,debug_indiv(Other).
debug_indiv(P):- is_rule(P,Q),
  dash_chars,
  pt(Q),
  dash_chars,!.

is_rule(P,_):- \+ compound(P),!,fail.
is_rule(A:-true,A):-!.
is_rule(A:-B,A:-B):-!.


debug_indiv(Other):-
  dash_chars,
  functor(Other,F,A),
  wqnl(other = F/A),
  pt(Other),
  dash_chars,!.

debug_indiv(Obj,P):- compound(P),!,compound_name_arguments(P,F,A),debug_indiv(Obj,P,F,A).

priority("bckgrnd",0).
priority("point",0).
priority(A,1):- atom_contains(A,")").
priority(_,2).
longer_strings(R,A,B):- string(A),string(B),priority(A,PA),priority(B,PB),atom_length(A,AL),atom_length(B,BL),compare(R,PA+AL+A,PB+BL+B).
longer_strings(R,A,B):- obj_prop_sort_compare(R,A,B).

remove_too_verbose(Var,plain_var(Var)):- plain_var(Var),!.
remove_too_verbose(H,''):- too_verbose(H),!.

% @TODO UNCOMMENT THIS remove_too_verbose(dot,"point"):- !.
%remove_too_verbose(line(HV),S):- sformat(S,'~w-Line',[HV]).
%remove_too_verbose(square,S):- sformat(S,'square',[]).
% @TODO UNCOMMENT THIS remove_too_verbose(background,S):- sformat(S,'bckgrnd',[]).
%remove_too_verbose(iz(H),S):- !, remove_too_verbose(H,HH),sformat(S,'~q',[s(HH)]).

remove_too_verbose(touches(Dir,ID),HH):- integer(ID),int2glyph(ID,Glyph),remove_too_verbose(touches(Dir,Glyph),HH).

remove_too_verbose(colors(H),HH):- !, remove_too_verbose(H,HH).
remove_too_verbose(object_indv_id(_ * _ * X,Y),[layer(XX),nth(Y)]):- =(X,XX).
remove_too_verbose(object_indv_id(_ * X,Y),[layer(XX),nth(Y)]):- =(X,XX).
%remove_too_verbose(loc_xy(X,Y),loc_xy(X,Y)).
%remove_too_verbose(vis_hv(X,Y),size(X,Y)).
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
    (dash_chars(Pad1,' '),write(Id=Glyph),
     print_grid(OH,OV,OH,OV,EH,EV,EH,EV,Obj))),

  nop(wots(S,
    (dash_chars(Pad1,' '),write(Id=Glyph),
     print_grid(H,V,A)))),


  Pad is floor(20-V/2),
  max_min(Pad,OH,PadH,_),
  print_w_pad(PadH,S).


debug_indiv(_,P,_,_):- pt(P).

:- fixup_exports.

