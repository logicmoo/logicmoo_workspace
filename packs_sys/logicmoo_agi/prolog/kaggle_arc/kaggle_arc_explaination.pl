/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).


% make_grid(3,4)

into_pipe(Grid,Grid):- !. % into_group
into_pipe(Grid,Solution):- into_grid(Grid,Solution).

describe_feature(Grid,List):- is_list(List),!,maplist(describe_feature(Grid),List).
describe_feature(_,call(Call)):- !, call(Call).
describe_feature(Grid,Pred):- is_pointless(Grid), !, as_debug(9,pp(usupported_call(Pred,Grid))).
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

will_show_grid(Obj,true):- amass(Obj,Mass)-> (Mass>4, vis2D(Obj,H,V) -> (H>1,V>1)),!.
will_show_grid(_,false).



  

print_list_of(Title,O):- print_list_of(print_info,Title,O).

:- meta_predicate(print_list_of(1,+,+)).
print_list_of(P1,Title,O):- \+ is_list(O),!,print_list_of(P1,Title,[O]).
print_list_of(_,Title,[]):- pp(no_data(Title)),!.
print_list_of(P1,Title,O):- length(O,Len), 
  wots_vs(SS,((Title\=[],Title\="",Title\='') -> pp(Title=print_list_of(Len)); ppt(P1=print_list_of(Len)))),
  print_list_of(P1,SS,Len,O).
:- meta_predicate(print_list_of(1,+,+,+)).

print_list_of(P1,Title,Len,O):- fail, Len = 1,!,
((
  write(Title),
  ignore(maybe_cache_glyphs(O)),
  %save_grouped(print_list_of(Title),O),
  g_out( maplist(ignore_call_p1(P1),O)))),!.

print_list_of(P1,Title,_Len,O):-
 collapsible_section(info,Title,maybe,
((
 ignore(maybe_cache_glyphs(O)),
  %save_grouped(print_list_of(Title),O),
  g_out( maplist(ignore_call_p1(P1),O))))),!.

ignore_call_p1(P1,A):- ignore(call(P1,A)).

maybe_cache_glyphs(O):- ignore((is_group(O),mapgroup(o2g,O,_))).

print_info(R):- is_object_props(R),!,print_info(obj(R)).
print_info(A):- is_grid(A),print_grid(A),!.
print_info(A):- is_object(A), ignore(debug_indiv(A)),!.
print_info(A):- is_group(A),maybe_cache_glyphs(A),debug_indiv(A),!.
%print_info(A):- into_obj(A,Obj),print_info(Obj).
print_info([]):-!.
print_info(A):- pp(A),!.

print_info_1(G):- print_info(G).

print_info_l(GridS):- maplist(print_info_1,GridS).


global_or_object_grid(O1,global+grid,Grid):-  global_grid(O1,Grid),!.
global_or_object_grid(O1,local+grid,Grid):-  object_grid(O1,Grid),!.
global_or_object_grid(O1,global+points,Points):- globalpoints_include_bg(O1,Points).
global_or_object_grid(O1,local+points,Points):- localpoints_include_bg(O1,Points).
global_or_object_grid(O1,local+self,O1).

object_grid_to_str(Obj,Str,Title):- 
 must_det_ll((
  vis2D(Obj,H,V), 
  object_glyph(Obj,Glyph),
  Title = t(LG+Type,loc2D(OH,OV),loc2G(OGH,OGV),center2G(CX,CY),size2D(H,V)),
  loc2D(Obj,OH,OV),
  ignore(loc2G(Obj,OGH,OGV)),
  ignore(center2G(Obj,CX,CY)),
  
  global_or_object_grid(Obj,LG+Type,GridO),
  (is_grid(GridO)-> grid_size(GridO,IH,IV) 
     ; (LG==local -> (IH=H,IV=V) ; grid_size(Obj,IH,IV))),

  make_bg_visible(GridO,GridOO),
  wots(GS,(print_grid(IH,IV,GridOO))),

  replace_in_string(['®'=Glyph,'@'=Glyph],GS,GSS),

  (LG==local
   -> (HH is (OH - 1) * 2, wots(Str,(print_w_pad(HH,GSS))))
    ; (Str=GSS)))).
 


debug_as_grid(Grid):- debug_as_grid('',Grid),!.

:- discontiguous debug_as_grid/2.
debug_as_grid(Why,R):- is_object_props(R),!,debug_as_grid(Why,obj(R)).
debug_as_grid(Why,R):- atom(R), atom_contains(R,'_'), pp_parent([LF|_]), \+ (LF==sf;LF==objFn), 
  resolve_reference(R,Var), R\==Var, \+ plain_var(Var),!, 
  write(' '), writeq(R), write(' /* '), debug_as_grid(Why,Var), write(' */ ').



%debug_as_grid(Why,R):- resolve_reference(R,Var)-> R\==Var, write(' ( '), writeq(R),write(' , '),debug_as_grid(Why,Var),write(' )'),!.
debug_as_grid(Why,Grid):- (is_object(Grid)/*;is_grid(Grid)*/),!,
  must_det_ll((
  vis2D(Grid,H,V),
  object_glyph(Grid,Glyph),  
  Title = debug_as_grid(Why,objFn(Glyph),loc2D(OH,OV),size2D(H,V)),
  if_t((H\==1;V\==1;true),
    must_det_ll((
     loc2D(Grid,OH,OV),     
     nop((shape2D(Grid,SX,SY),max_min(H,SX,IH,_),max_min(V,SY,IV,_))),
     ignore(IV=V),ignore(IH=H),
     localpoints_include_bg(Grid,GridOM),
     points_to_grid(H,V,GridOM,GridO),
     make_bg_visible(GridOM,PrintGrid),    
     copy_term(PrintGrid,PrintGridC),
     into_ngrid(PrintGridC,NGrid),
     %wots(GS,print_grid(IH,IV,Title,PrintGridC)),replace_in_string(['®'=Glyph,'@'=Glyph],GS,GSS),
     %wots(S,print_side_by_side(GSS,print_grid(IH,IV,ngrid,NGrid))),

     wots(S,print_grid(IH,IV,Title,NGrid)), HH is (OH - 1) * 2, print_w_pad(HH,S),

     ignore(( O = GridO, once(grid_to_norm(O,Ops,N)), O\=@=N, print_side_by_side(Ops,O,N),writeln(Ops))),

     true))),
  if_t(is_object(Grid),
    (format('~N~n'),
     locally(nb_setval(debug_as_grid,nil),underline_print(debug_indiv(Grid))))),
     format('~N'),dash_chars(15))),!.

debug_as_grid(  I,   A):- is_1gridoid(A), !, make_bg_visible(A,AA), print_grid(I,AA).
debug_as_grid( '',Grid):- !, pp(Grid).
debug_as_grid(Why,Grid):- pp(debug_as_grid(Why,Grid)).
  

:- discontiguous debug_indiv/1. 

debug_indiv(_):- is_print_collapsed,!.
debug_indiv(R):- is_object_props(R),!,debug_indiv(obj(R)).
debug_indiv(_):- format('~N'),fail.
debug_indiv(Var):- plain_var(Var),pp(debug_indiv(Var)),!.
debug_indiv(Grid):- is_grid(Grid),!,debug_as_grid(is_grid,Grid),!.
debug_indiv(Grid):- is_cpoints_list(Grid),!,debug_as_grid(is_cpoint,Grid).
debug_indiv(Grid):- maplist(is_point,Grid),!,debug_as_grid(is_point,Grid).


debug_indiv(List):- is_list(List),length(List,Len),!,
  dash_chars,
  wqnl(debug_indiv = Len),
  print_list_of(debug_indiv,debug_indiv_list,List),
  %max_min(Len,40,_,Min),
  %forall(between(1,Min,N),(N=<40->(nth1(N,List,E),debug_indiv(E));wqnl(total = 40/Len))),
  dash_chars,!.


/*
debug_indiv(A):- is_point_obj(A,Color,Point),
  obj_to_oid(A,Tst,Id), i_glyph(Id,Sym),
  hv_point(H,V,Point), i_glyph(Id,Sym),
  wqnl([' % Point: ', color_print(Color,Sym), dot, color(Color), fav1(Tst), nth(Id), loc2D(H,V)]),!. 
*/
object_glyph_one_color(Obj,FC):- once((unique_colors(Obj,CL),member(FC0,CL),is_real_color(FC0));FC0=wfg),
  (FC0==black_n -> FC= wbg ; FC = FC0).

to_realer_color(Var,white):- plain_var(Var),!.
to_realer_color(C,C):- is_real_color(C),!.
to_realer_color(C,C):- is_unreal_color(C),!.
to_realer_color(C,N):- into_color_name_always(C,N),!.
object_glyph_colorz(Obj,Colors):- 
  object_glyph_one_color(Obj,FC1),
  unique_colors(Obj,CL),
  findall(FC,(member(FC0,CL),to_realer_color(FC0,FC)),NColors),
  flatten_set([FC1,NColors],Colors).

object_s_glyph_long(PA, CGA):- 
 must_det_ll((
  obj_to_oid(PA,OID),
  object_glyph_colorz(PA,Colors),
  print_colors_on_ss(OID,Colors,CGAO),
  CGA=CGAO)).

object_s_glyph(Obj,SGlyph):-
 must_det_ll((
  object_glyph(Obj,Glyph),
  object_glyph_colorz(Obj,Colors),
  print_colors_on_ss(Glyph,Colors,SGlyph))).
  
print_colors_on_ss(Glyph,[],SSGlyph):- sformat(SSGlyph,'~q',[Glyph]),!.
print_colors_on_ss(Glyph,Colors,SGlyph):- atom_length(Glyph,N), 
  wots(SGlyph,print_colors_on(Colors,N,Glyph)).


print_colors_on([Color],_,Glyph):- color_print(Color,call(writeq(Glyph))),!.
print_colors_on(Colors,L,Glyph):- length(Colors,CL), CL>L,write('\''), user:maplist(print_ncolors(Glyph),Colors), write('\''),!.
print_colors_on(Colors,_,Glyph):- atom_chars(Glyph,Chars),write('\''),print_colors_on_s(Colors,Chars),write('\''),!.
print_colors_on(Colors,Glyph):- write('\''), user:maplist(print_ncolors(Glyph),Colors), write('\'').
print_colors_on_s([],G):-  format('~s',[G]).
print_colors_on_s([C],G):- sformat(GS,'~s',[G]),color_print(C,GS).
print_colors_on_s([C|Color],Glyph):- length([C|Color],CL),length(Glyph,GL),CLL is CL div GL,CLL>1,length(GLL,CLL),append(GLL,More,Glyph),
  sformat(G,'~s',[GLL]),!,color_print(C,G),print_colors_on_s(Color,More).
print_colors_on_s([C|Color],[G|Glyph]):- color_print(C,G),print_colors_on_s(Color,Glyph).


object_s_glyph2(Obj,S):- o2g(Obj,G),colors(Obj,Colors),maplist(arg(1),Colors,NColors),
  wots(S,maplist(user:print_ncolors1(G),NColors)),!.

print_ncolors(G,C):- color_print(C,G).
print_ncolors1(G,C):- sformat(F,'~w',[G]),sub_string(F,0,1,_,SS),color_print(C,SS).

o2c(Obj,Glyph):- color(Obj,Glyph),!.

o2ansi(I,S):- integer(I),int2glyph(I,G),!,o2ansi(G,S). 
o2ansi(G,S):- atom(G),!,g2o(G,O),o2ansi(O,S),!.
o2ansi(G,S):- \+ is_object(G),!,colorize_oterms(G,S).
o2ansi(Obj,S):- object_s_glyph(Obj,S),!.
o2ansi(Obj,S):- object_s_glyph2(Obj,S),!.

colorize_oterms(O,A):- var(O),!,A=O.
colorize_oterms(O,A):- term_contains_ansi(O),!,A=O.
colorize_oterms(O,A):- number(O),!,wots(A,bold_print(write(O))).
colorize_oterms(-O,-A):- !, colorize_oterms(O,A).
colorize_oterms(+O,+A):- !, colorize_oterms(O,A).
colorize_oterms(O,A):- is_list(O),!,maplist(colorize_oterms,O,A).
colorize_oterms(O,A):- is_object(O),O=obj(_),object_s_glyph(O,A),!.
colorize_oterms(O,A):- compound(O),compound_name_arguments(O,F,Args),!,maplist(colorize_oterms,Args,AArgs),compound_name_arguments(A,F,AArgs).
colorize_oterms(O,A):- \+ atom(O),!,A=O.
colorize_oterms(O,A):- is_color(O),!,wots(A,color_print(O,O)).
colorize_oterms(O,A):- member(O,[n,s,e,w,c,ne,se,sw,nw]),!,wots(A,bold_print(write(O))).
colorize_oterms(O,A):- o2ansi(O,A),!.
colorize_oterms(O,O).

prefered(repaired).
prefered(full_grid).
prefered(hidden).
prefered(neededChanged).
prefered(changed).
prefered(nsew).
prefered(colormass).
prefered(alone_dots).
prefered(hv_line(_)).
prefered(dg_line(_)).
prefered_header(cc(Caps,_),Caps):- freeze(Caps,wbg == Caps).
prefered_header(cc(Caps,_),Caps):- get_black(Black),freeze(Caps,Black == Caps).
prefered_header(o(_,_,Caps),Caps):- freeze(Caps,i_bg_shapes == Caps).
prefered_header(o(sf(_),1,Caps),Caps):- freeze(Caps,atom(Caps)).
prefered_header(o(sf(_),last(_),Caps),Caps):- freeze(Caps,atom(Caps)).
prefered_header(birth(Caps),PCaps):-prefered(PCaps),freeze(Caps,(nonvar(Caps),Caps = PCaps)).
%prefered_header(iz(Caps),PCaps):-prefered(PCaps),freeze(Caps,Caps == PCaps).
prefered_header(Caps,PCaps):-prefered(PCaps),freeze(Caps,(nonvar(Caps),Caps = PCaps)).
prefered_header(birth(Caps),Caps).
prefered_header(iz(Caps),Caps).

debug_indiv(OBJ):- compound(OBJ), OBJ=obj(_), debug_indiv_obj_1(OBJ),!.
%debug_indiv(obj(A)):- is_list(A),!, dash_chars,   maplist(debug_indiv_2(obj(A)),A), dash_chars,!.  debug_indiv_2(Obj,P):- compound(P),!,compound_name_arguments(P,F,A),debug_indiv(Obj,P,F,A),!.

  debug_indiv_obj_1(obj(A)):- is_cons(A), \+ is_list(A), append(A,[],A),debug_indiv_obj_1(obj(A)),!.
  debug_indiv_obj_1(obj(A)):- \+ is_list(A),!, pp(debug_indiv(obj(A))).
  debug_indiv_obj_1(A):- nb_current(debug_as_grid,t), \+ \+ locally(nb_setval(debug_as_grid,f),debug_as_grid(A)),!.
  debug_indiv_obj_1(A):- \+ \+ debug_indiv_obj(A).

  


choose_header(ASFROM,Caps):- once((prefered_header(P,Caps),member(P,ASFROM),\+ skip_header(Caps),ground(Caps))),!.
choose_header(ASFROM,Caps):- once((prefered_header(P,CapsO),member(P,ASFROM),term_to_atom(CapsO,Caps))).

skip_header(X):- compound(X).
skip_header(grid_sz(_,_)).
skip_header(sizeX(_)).
skip_header(sizeY(_)).

is_open_list(T):- var(T),!.
is_open_list([_|T]):-!,is_open_list(T).

is_arg_in(Only,P):- compound(P),arg(2,P,Ref),
  once(into_obj(Ref,Obj)), \+ is_not_in(Only,Obj).

object_dglyphH(PA, OUTS):- 
  obj_to_oid(PA,GA),% mass(PA,Mass),
  shape(PA,Shape),pen(PA,Pen),loc2D(PA,X,Y), rotation(PA,ROT),vis2D(PA,XX,YY),
  shape_id(Shape,ShapeID),
  OUT = objFn(GA,[loc2D(X,Y),rotation(ROT),pen(Pen),vis2D(XX,YY),sid(ShapeID)]),
  colorize_oterms(OUT,OT),
  wots(SS,write(OT)),
  OUTS = SS.

object_dglyphH(PA,GA):- object_dglyph(PA,GA).
%object_dglyph(O,G):- object_cglyph(O,G). % want this
object_dglyph(O,D):- object_glyph(O,G), atom_concat(' ',G,D),!.

show_touches(Only,Obj):- must_det_ll((into_obj(Obj,RealObj),show_touches0(Only,RealObj))).
show_touches0(Only,Obj):- is_not_in(Only,Obj),!.
show_touches0(Only,Obj):- Obj = obj(List), 
 must_det_ll((
  object_dglyphH_no_loop(Obj,SGlyph),
  include(is_functor(link),List,TP),
  include(is_arg_in(Only),TP,TPO),!,
  show_touches2(SGlyph,TPO))).
show_touches2(_,[]):-!.
show_touches2(SGlyph,TP):-   
  remove_too_verbose(SGlyph,TP,OO),
  format("~N~n% ~w:\t",[SGlyph]),wqs_l(OO),format('~N').

dbg_show_touches(Only,Obj):- must_det_ll((into_obj(Obj,RealObj),dbg_show_touches0(Only,RealObj))).
dbg_show_touches0(Only,Obj):- must_det_ll(( \+ is_not_in(Only,Obj))),!.
dbg_show_touches0(Only,Obj):- Obj = obj(List), 
 must_det_ll((
  object_dglyphH_no_loop(Obj,SGlyph),
  include(is_functor(link),List,TP),
  include(is_arg_in(Only),TP,TPO),!,
  dbg_show_touches2(SGlyph,TPO))).
dbg_show_touches2(_,[]):-!.
dbg_show_touches2(SGlyph,TP):-   
  remove_too_verbose(SGlyph,TP,OO),
  format("~N~n% ~w:\t",[SGlyph]),wqs_l(OO),format('~N').


object_dglyphH_no_loop(PA, OUTS):- 
 must_det_ll((
  object_s_glyph_long(PA, CGA),
  mass(PA,Mass),
  shape(PA,Shape),pen(PA,Pen),loc2D(PA,X,Y), rotation(PA,ROT),
  shape_id(Shape,ShapeID),  
  OUT = oFn(CGA,Mass,loc2D(X,Y),ROT,pen(Pen),ShapeID),
  wots(SS,wqs_l(OUT)),
  OUTS = SS)).


debug_indiv_obj(Obj):- Obj = obj(A), nonvar(A),!,debug_indiv_obj(A).
debug_indiv_obj(Props):- is_open_list(Props),!,must_det_ll((append(Props,[],CProps),!,debug_indiv_obj(CProps))).
debug_indiv_obj(AS):- 
 must_det_ll((
  Obj = obj(AS),
  %ignore((o2g(Obj,GGG), nonvar(GGG),set_glyph_to_object(GGG,Obj))),
 % will_show_grid(Obj,TF),
  TF = false,
  obj_to_oid(Obj,MyOID),
  %o2ansi(MyOID,MissGlyph),
  object_s_glyph(Obj,SGlyph),
  append(AS,[],Props),  

  remove_too_verbose(MyOID,Props,TV0), include(not_too_verbose,TV0,TV),

  %flatten(TV,F),predsort(longer_strings,F,[Caps|_]), 
  =(TV,ASA),reverse(ASA,ASAR),
  append(ASAR,Props,ASFROM),
  choose_header(ASFROM,Caps),  
  toPropercase(Caps,PC),
  sort(TV,TVS),
  my_partition(is_o3,TVS,TVSO,TVSI),
  predsort(sort_on(arg(2)),TVSO,TVSOR),reverse(TVSOR,TVSOS),
  ignore((TF==true,dash_chars)),
  sformat(SF,"% ~w:\t\t~w\t",[PC,SGlyph]),
  ignore(( g_out_style(style('font-size2D','75%'),(write(SF), wqs(TVSI))))),
  %maplist(write_indented_list('~N    '),wqs(TVSOS),
  nop((format('~N    '),wqs(TVSOS))),
  ignore(( TF==true, amass(Obj,Mass),!,Mass>4, vis2D(Obj,H,V),!,H>1,V>1, localpoints(Obj,Points), print_grid(H,V,Points))),
  ignore(( fail, amass(Obj,Mass),!,Mass>4, vis2D(Obj,H,V),!,H>1,V>1, show_st_map(Obj))),
  %pp(Props),
  ignore(( TF==true,dash_chars)))),!.

write_indented_list(F,WQS):- format(F),wqs(WQS).

not_too_verbose(X):- X\==(''), X\==s('').

is_o3(o(_,_,_)).

show_st_map(Obj):-
  ignore(( 
  localpoints(Obj,Points),
%  amass(Obj,Mass),!,Mass>4,
%  vis2D(Obj,H,V),!,H>1,V>1,
  format('~N'),
  solidness(Points,0,inf,Res),
  solidness_no_diag(Points,0,inf,ResND),
  solidness_is_diag(Points,0,inf,ResD),
  print_side_by_side(print_side_by_side(ResND,ResD),Res))).


debug_indiv([]):- !.

debug_indiv(diff(_)):-!.
debug_indiv([Other]):-debug_indiv(Other),!.
debug_indiv(P):- is_rule(P,Q),
  dash_chars,
  pp(Q),
  dash_chars,!.

is_rule(P,_):- \+ compound(P),!,fail.
is_rule(A:-true,A):-!.
is_rule(A:-B,A:-B):-!.


debug_indiv(Other):-
  dash_chars,
  functor(Other,F,A),
  wqnl(other = F/A),
  pp(Other),
  dash_chars,!.



%alt_id(_MyID,ID,Alt):- int2glyph(ID,Alt).
alt_id(MyOID,ID,Alt):- Alt is abs(MyOID-ID).
remove_too_verbose(_MyID,Var,plain_var(Var)):- plain_var(Var),!.
remove_too_verbose(_MyID,H,''):- too_verbose(H),!.
remove_too_verbose(MyOID,List,ListO):- is_list(List),!,maplist(remove_too_verbose(MyOID),List,ListO),!.

% @TODO UNCOMMENT THIS remove_too_verbose(MyOID,dot,"point"):- !.
%remove_too_verbose(MyOID,line(HV),S):- sformat(S,'~w-Line',[HV]).
%remove_too_verbose(MyOID,square,S):- sformat(S,'square',[]).
% @TODO UNCOMMENT THIS remove_too_verbose(MyOID,background,S):- sformat(S,'bckgrnd',[]).
remove_too_verbose(MyOID,iz(H),HH):- compound(H), remove_too_verbose(MyOID,H,HH),!.

%remove_too_verbose(_MyID,obj_to _oid(_ * _ * X,Y),NTH):- NTH=..[X,Y].
%remove_too_verbose(_MyID,obj_to_oid(_ * _+_ * X,Y),NTH):- NTH=..[X,Y].
%remove_too_verbose(_MyID,obj_to _oid(_ * X,Y),NTH):- NTH=..[X,Y].

remove_too_verbose(MyOID,link(Touched,ID,Dir),HH):- %number(MyOID),
  MyOID\==0,integer(ID),alt_id(MyOID,ID,Alt),o2ansi(ID,Glyph),
  remove_too_verbose(0,link(Touched,Alt,Dir,Glyph),HH).

remove_too_verbose(MyOID,link(Touched,ID),HH):- % number(MyOID),
  MyOID\==0, integer(ID),alt_id(MyOID,ID,Alt),o2ansi(ID,Glyph),
  remove_too_verbose(0,link(Touched,Alt,Glyph),HH).

remove_too_verbose(MyOID,TP,OO):- compound(TP),compound_name_arguments(TP,link,[F|A]),atom(F),
   maplist(colorize_oterms,A,AA),
   compound_name_arguments(TPP,F,AA),!,remove_too_verbose(MyOID,TPP,HH),
   OO= HH,!.

remove_too_verbose(MyOID,colors(H),HH):- !, remove_too_verbose(MyOID,H,HH).
%remove_too_verbose(MyOID,loc2D(X,Y),loc2D(X,Y)).
%remove_too_verbose(MyOID,vis2D(X,Y),size2D(X,Y)).
remove_too_verbose(_MyID,changes([]),'').
remove_too_verbose(_MyID,rotation(sameR),'').
remove_too_verbose(MyOID,L,LL):- is_list(L),!, maplist(remove_too_verbose(MyOID),L,LL).
remove_too_verbose(_MyID,H,HH):- compound(H),arg(1,H,L), is_list(L), maybe_four_terse(L,T),H=..[F,L|Args],HH=..[F,T|Args].
remove_too_verbose(_MyID,H,H).

too_verbose(P):- compound(P),compound_name_arity(P,F,_),!,too_verbose(F).
too_verbose(globalpoints).
too_verbose(monochrome).
too_verbose(shape).
% too_verbose(gid). too_verbose(giz). 
too_verbose(grid_sz). too_verbose(grid_size). 
too_verbose(localpoints).
too_verbose(grid).
too_verbose(norm_grid).
%too_verbose(link).
too_verbose(rotated_grid).
%too_verbose(wide). too_verbose(tall).
too_verbose(locX).
too_verbose(locY).
too_verbose(cenX).
too_verbose(cenY).

debug_indiv(_,_,X,_):- too_verbose(X),!.
debug_indiv(Obj,_,F,[A]):- is_cpoints_list(A),!,
  vis2D(Obj,H,V), wqnl(F), 
  loc2D(Obj,OH,OV),
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


debug_indiv(_,P,_,_):- pp(P).




:- include(kaggle_arc_footer).


