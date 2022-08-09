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
  luser_setval(grid_name,ID),
  ignore(( \+ into_gridnameA(Grid,ID),
  copy_term(Grid,GGrid),numbervars(GGrid,1,_),
  asserta(is_grid_id(GGrid,ID)))).

:- dynamic(is_grid_id/2).

%grid_to_id(Grid,Name):- is_grid_id(Grid,Name)*->true; (plain_var(Name)->(luser_getval(grid_name,Name),Name\=[],grid_to_id(Grid,Name))).



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

will_show_grid(Obj,true):- mass(Obj,Mass)-> (Mass>4, v_hv(Obj,H,V) -> (H>1,V>1)),!.
will_show_grid(_,false).



  

print_list_of(N,O):- print_list_of(print_info,N,O).
print_list_of(_,_,[]):-!.
print_list_of(P1,N,O):-
 (N\=[] -> pt(N); true),
  maybe_cache_glyphs(O),
  %save_grouped(print_list_of(N),O),
  g_out( maplist(P1,O)),!.


maybe_cache_glyphs(O):- ignore((is_group(O),maplist(o2g,O,_))).

print_info(A):- is_grid(A),print_grid(A).
print_info(A):- is_object(A), ignore(debug_indiv(A)).
print_info(A):- is_group(A),maybe_cache_glyphs(A),debug_indiv(A).
%print_info(A):- into_obj(A,Obj),print_info(Obj).
print_info([]):-!.
print_info(A):- pt(A).


debug_as_grid(Grid):-
  grid_size(Grid,H,V),
  dash_chars(H),
  wqnl(debug_indiv_grid(H,V)),
  print_grid(Grid),
  dash_chars(H),!.

:- discontiguous debug_indiv/1. 

debug_indiv(_):- is_print_collapsed,!.
debug_indiv(Var):- plain_var(Var),pt(debug_indiv(Var)),!.
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
  o_i_d(A,Tst,Id), i_glyph(Id,Sym),
  hv_point(H,V,Point), i_glyph(Id,Sym),
  wqnl([' % Point: ', color_print(Color,Sym), dot, color(Color), fav1(Tst), nth(Id), loc(H,V)]),!. 
*/
object_glyph_color(Obj,FC):- once(colors(Obj,[cc(FC0,_)|_]);FC0=fg),
  (FC0==black_n -> FC= wbg ; FC = FC0).

object_s_glyph(Obj,SGlyph):- 
  object_glyph(Obj,Glyph), 
  colors(Obj,Colors),
  maplist(arg(1),Colors,NColors),
  object_glyph_color(Obj,FC),
  wots(SGlyph,
   (
     (( \+ (member(E,NColors),E==FC))->color_print(FC,Glyph);true),
     maplist(print_ncolors(Glyph),NColors))).


prefered(repaired).
prefered(full_grid).
prefered(invisible).
prefered(neededChanged).
prefered(changed).
prefered(nsew).
prefered(colormass).
prefered(alone_dots).
prefered_header(cc(Caps,_),Caps):- freeze(Caps,wbg == Caps).
prefered_header(cc(Caps,_),Caps):- freeze(Caps,black == Caps).
prefered_header(o(Caps,_,_),Caps):- freeze(Caps,i_bg_shapes == Caps).
prefered_header(o(Caps,sf(_),0),Caps):- freeze(Caps,atom(Caps)).
prefered_header(o(Caps,lf(_),0),Caps):- freeze(Caps,atom(Caps)).
prefered_header(birth(Caps),PCaps):-prefered(PCaps),freeze(Caps,Caps == PCaps).
%prefered_header(iz(Caps),PCaps):-prefered(PCaps),freeze(Caps,Caps == PCaps).
prefered_header(Caps,PCaps):-prefered(PCaps),freeze(Caps,Caps == PCaps).
prefered_header(birth(Caps),Caps).
prefered_header(iz(Caps),Caps).
% I didn't really have the programming chops to take his program and give it human level reasoning until about 5 years ago
debug_indiv(obj(A)):- \+ \+ debug_indiv_obj(A).
debug_indiv_obj(A):- Obj = obj(A), is_list(A),!,
 maplist(must_det_ll,[
  ignore((o2g(Obj,GGG), nonvar(GGG),luser_setval(GGG,Obj), nop( my_asserta_if_new(g2o(GGG,Obj))))),
%debug_indiv(Obj):- Obj = obj(A), is_list(A),  
  
  sort_obj_props(A,AS0),
 % will_show_grid(Obj,TF),
  TF = false,
  o_i_d(Obj,_,MyID),
  o2ansi(MyID,MissGlyph),
  object_s_glyph(Obj,SGlyph),
  append(AS0,[nth(MyID),miss(MissGlyph)],AS),  
  remove_too_verbose(MyID,AS,TV0), include(not_too_verbose,TV0,TV),

  %flatten(TV,F),predsort(longer_strings,F,[Caps|_]), 
  sort(AS,ASA),reverse(ASA,ASAR),
  once((prefered_header(P,Caps),member(P,ASAR),atom(Caps))),
  toPropercase(Caps,PC),
  sort_obj_props(TV,TVS),

  ignore((TF==true,dash_chars)),
  ignore(( g_out(style('font-size','75%'),wqnl([format("% ~w:\t\t~w\t",[PC,SGlyph]) | TVS ])))),
  ignore(( TF==true, mass(Obj,Mass),!,Mass>4, v_hv(Obj,H,V),!,H>1,V>1, localpoints(Obj,Points), print_grid(H,V,Points))),
  ignore(( fail, mass(Obj,Mass),!,Mass>4, v_hv(Obj,H,V),!,H>1,V>1, show_st_map(Obj))),
  %pt(A),
  ignore(( TF==true,dash_chars))]),!.

not_too_verbose(X):- X\==(''), X\==s('').

show_st_map(Obj):-
  ignore(( 
  localpoints(Obj,Points),
%  mass(Obj,Mass),!,Mass>4,
%  v_hv(Obj,H,V),!,H>1,V>1,
  format('~N'),
  solidness(Points,0,inf,Res),
  solidness_no_diag(Points,0,inf,ResND),
  solidness_is_diag(Points,0,inf,ResD),
  print_side_by_side(print_side_by_side(ResND,ResD),Res))).

debug_indiv(obj(A)):- \+is_list(A),!, append(A,[],A),debug_indiv(obj(A)).
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


%alt_id(_MyID,ID,Alt):- int2glyph(ID,Alt).
alt_id(MyID,ID,Alt):- Alt is abs(MyID-ID).
remove_too_verbose(_MyID,Var,plain_var(Var)):- plain_var(Var),!.
remove_too_verbose(_MyID,H,''):- too_verbose(H),!.
remove_too_verbose(MyID,List,ListO):- is_list(List),!,maplist(remove_too_verbose(MyID),List,ListO),!.

% @TODO UNCOMMENT THIS remove_too_verbose(MyID,dot,"point"):- !.
%remove_too_verbose(MyID,line(HV),S):- sformat(S,'~w-Line',[HV]).
%remove_too_verbose(MyID,square,S):- sformat(S,'square',[]).
% @TODO UNCOMMENT THIS remove_too_verbose(MyID,background,S):- sformat(S,'bckgrnd',[]).
remove_too_verbose(MyID,iz(H),HH):- remove_too_verbose(MyID,H,HH),!.

remove_too_verbose(_MyID,o_i_d(_ * _ * X,Y),NTH):- NTH=..[X,Y].
remove_too_verbose(_MyID,o_i_d(_ * _+_ * X,Y),NTH):- NTH=..[X,Y].
remove_too_verbose(_MyID,o_i_d(_ * X,Y),NTH):- NTH=..[X,Y].

remove_too_verbose(MyID,link(Touched,ID,Dir),HH):- %number(MyID),
  MyID\==0,integer(ID),alt_id(MyID,ID,Alt),o2ansi(ID,Glyph),
  remove_too_verbose(0,link(Touched,Alt,Dir,Glyph),HH).
remove_too_verbose(MyID,link(Touched,ID),HH):- % number(MyID),
  MyID\==0, integer(ID),alt_id(MyID,ID,Alt),o2ansi(ID,Glyph),
  remove_too_verbose(0,link(Touched,Alt,Glyph),HH).

remove_too_verbose(MyID,TP,HH):- compound(TP),compound_name_arguments(TP,link,[F|A]),atom(F),
   compound_name_arguments(TPP,F,A),!,remove_too_verbose(MyID,TPP,HH).

remove_too_verbose(MyID,colors(H),HH):- !, remove_too_verbose(MyID,H,HH).
%remove_too_verbose(MyID,loc(X,Y),loc(X,Y)).
%remove_too_verbose(MyID,v_hv(X,Y),size(X,Y)).
remove_too_verbose(_MyID,changes([]),'').
remove_too_verbose(_MyID,rotation(same),'').
remove_too_verbose(MyID,L,LL):- is_list(L),!, maplist(remove_too_verbose(MyID),L,LL).
remove_too_verbose(_MyID,H,HH):- compound(H),arg(1,H,L), is_list(L), maybe_four_terse(L,T),H=..[F,L|Args],HH=..[F,T|Args].
remove_too_verbose(_MyID,H,H).

too_verbose(P):- compound(P),compound_name_arity(P,F,_),!,too_verbose(F).
too_verbose(globalpoints).
too_verbose(monochrome).
too_verbose(shape).
too_verbose(localpoints).
too_verbose(grid).
%too_verbose(link).
too_verbose(grid_size).
too_verbose(rotated_grid).

debug_indiv(_,_,X,_):- too_verbose(X),!.
debug_indiv(Obj,_,F,[A]):- maplist(is_cpoint,A),!,
  v_hv(Obj,H,V), wqnl(F), 
  loc(Obj,OH,OV),
  EH is OH+H-1,
  EV is OV+V-1,
  object_glyph(Obj,Glyph),
  Pad1 is floor(H),  

  wots(S,
    (dash_chars(Pad1,' '),write(Id=Glyph),
     print_grid(OH,OV,EH,EV,Obj))),

  nop(wots(S,
    (dash_chars(Pad1,' '),write(Id=Glyph),
     print_grid(H,V,A)))),


  Pad is floor(20-V/2),
  max_min(Pad,OH,PadH,_),
  print_w_pad(PadH,S).


debug_indiv(_,P,_,_):- pt(P).




:- fixup_exports.

