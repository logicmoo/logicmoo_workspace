/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).
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

will_show_grid(Obj,true):- mass(Obj,Mass)-> (Mass>4, vis2D(Obj,H,V) -> (H>1,V>1)),!.
will_show_grid(_,false).



  

print_list_of(Title,O):- print_list_of(print_info,Title,O).

:- meta_predicate(print_list_of(1,+,+)).
print_list_of(P1,Title,O):- \+ non_grid_list(O),!,print_list_of(P1,Title,1,[O]).
print_list_of(P1,Title,O):- length(O,Len), 
  print_list_of(P1,Title,Len,O).

:- meta_predicate(print_list_of(1,+,+,+)).
print_list_of(P1,Title,0,_):- pp(no_data(P1,Title)),!.
print_list_of(P1,Title,Len,O):-
 ignore(maybe_cache_glyphs(O)),
 w_section([Title,print_list_of(Len),P1],
  %save_grouped(print_list_of(Title),O),
  maplist_n(1,ignore_call_p1(P1),O),P1),!.

ignore_call_p1(P1,N,A):- nl_if_needed,write(N),write(': '),ignore(call(P1,A)).

maybe_cache_glyphs(O):- ignore((is_group(O),mapgroup(o2g,O,_))).

print_info(R):- is_object_props(R),!,print_info(obj(R)).
print_info(A):- is_grid(A),print_grid(A),!.
print_info(A):- is_object(A), ignore(debug_as_grid(A)),!.
print_info(A):- is_group(A),maybe_cache_glyphs(A),debug_indiv(A),!.
%print_info(A):- into_obj(A,Obj),print_info(Obj).
print_info([]):-!.
print_info(A):- pp(A),!.

print_info_1(G):- print_info(G).

print_info_l(GridS):- maplist(print_info_1,GridS).


global_or_object_grid(O1,O1):- \+ is_object(O1).
global_or_object_grid(O1,Grid):- global_or_object_grid(O1,_,Grid).
global_or_object_grid(O1,global+grid,Grid):-  global_grid(O1,Grid),!.
global_or_object_grid(O1,local+grid,Grid):-  object_grid(O1,Grid),!.
global_or_object_grid(O1,global+points,Points):- globalpoints_include_bg(O1,Points).
global_or_object_grid(O1,local+points,Points):- localpoints_include_bg(O1,Points).
global_or_object_grid(O1,local+self,O1).

object_or_global_grid(O1,global+grid,Grid):- vis2D(O1,1,1), global_grid(O1,Grid),!.
object_or_global_grid(O1,local+grid,Grid):-  object_grid(O1,Grid),!.
object_or_global_grid(O1,global+grid,Grid):-  global_grid(O1,Grid),!.
object_or_global_grid(O1,local+points,Points):- localpoints_include_bg(O1,Points).
object_or_global_grid(O1,global+points,Points):- globalpoints_include_bg(O1,Points).
object_or_global_grid(O1,local+self,O1).

/*
object_grid_to_str(Obj,Str,Title):- object_grid_to_str(Obj,_GridO,Str,Title).

object_grid_to_str(Obj,GridO,Str,Title):- 
 must_det_ll((
  vis2D(Obj,H,V), 
  object_glyph(Obj,Glyph),
  (var(Title) -> (Title = t(LG+Type,loc2D(OH,OV),loc2G(OGH,OGV),center2G(CX,CY),size2D(H,V))) ; true),
  loc2D(Obj,OH,OV),
  ignore(loc2G(Obj,OGH,OGV)),
  ignore(center2G(Obj,CX,CY)),
  
  global_or_object_grid(Obj,LG+Type,GridO),
  (is_grid(GridO)-> grid_size(GridO,IH,IV) 
     ; (LG==local -> (IH=H,IV=V) ; grid_size(Obj,IH,IV))),

  make_bg_visible(GridO,GridOO),
  wots(GS,(print_grid(IH,IV,GridOO))),

  replace_in_string(['�'=Glyph,'@'=Glyph],GS,GSS),

  (LG==local
   -> (HH is (OH - 1) * 2, wots(Str,(print_w_pad(HH,GSS))))
    ; (Str=GSS)))).
*/

printable_grid(_,_,Grid,GridO,GridO):-localpoints_include_bg(Grid,GridO).
printable_grid(H,V,Grid,GridO,NGrid):-
     localpoints_include_bg(Grid,GridOM),
     points_to_grid(H,V,GridOM,GridO),
     make_bg_visible(GridO,PrintGrid),    
     copy_term(PrintGrid,PrintGridC),
     ( false -> into_ngrid(PrintGridC,NGrid); NGrid = PrintGridC),!.     

debug_as_grid(Grid):- debug_as_grid('',Grid),!.

:- discontiguous debug_as_grid/2.
debug_as_grid(Why,R):- is_object_props(R),!,debug_as_grid(Why,obj(R)).
debug_as_grid(Why,R):- atom(R), atom_contains(R,'_'), pp_parent([LF|_]), \+ (LF==sf;LF==objFn), 
  resolve_reference(R,Var), R\==Var, \+ plain_var(Var),!, 
  write(' '), writeq(R), write(' /* '), debug_as_grid(Why,Var), write(' */ ').

debug_as_grid(Why, Obj):- is_object(Obj),!, show_indiv(Why, Obj).
debug_as_grid(  I,   A):- is_1gridoid(A), !, make_bg_visible(A,AA), print_grid(I,AA).
debug_as_grid( '',Grid):- !, pp(Grid).
debug_as_grid(Why,Grid):- pp(debug_as_grid(Why,Grid)).



%show_indiv(_Why,Obj):- is_bg_object(Obj),!.
arg1(P1,E):- arg(1,E,E1),call(P1,E1),!.

show_indiv(_Why, Obj):- obj(ObjL)=Obj,
  object_color_glyph_short(Obj,SGlyph),
  my_partition(arg1(is_grid),ObjL,_GridArgs,NonGridArgs),
  my_partition(arg1(is_cpoints_list),NonGridArgs,_CArgs,NonCArgs),
  my_partition(arg1(is_points_list),NonCArgs,_PArgs,NonPArgs),
  sformat(S,'Object ~w',[SGlyph]),
  global_grid(Obj,GG),
  norm_grid(Obj,NormGrid),
  norm_ops(Obj,NormOps),
  print_side_by_side(green,GG,SGlyph,_,NormGrid,NormOps),!,
  %print_side_by_side(green,GG,SGlyph,_,NormGrid,NormOps),!,
  pp([S|NonPArgs]).

show_indiv(Why, Obj):-

  format('~N'),dash_chars(45),dash_chars(45),
  must_det_ll((
  vis2D(Obj,H,V),
  DoFF = false,

  findall(SubGroup,is_in_subgroup(Obj,SubGroup),SubGroupS), 
  pp(subGroupS=SubGroupS),

  if_t((H\==1;V\==1;true),
    must_det_ll((     
     %object_or_global_grid(Obj,LG+Type,GridS),      
     global_grid(Obj,GridS),
     object_grid(Obj,Grid),   
     Title = show_indiv(Why,objFn(Glyph),loc2D(OH,OV),center2G(CX,CY),size2D(H,V)),
             loc2D(Obj,OH,OV), ignore(center2G(Obj,CX,CY)), object_glyph(Obj,Glyph),


     Grids = [Title=GridS|_],     

     copy_term(Obj,CObj),
     nop((object_ngrid(CObj,NGrid), append(_,["NGrid"=NGrid|_],Grids))),

     ShowQ=_,

     term_variables(Grid,GV1),
     normalize_grid(NOps,Grid,Normalized), % if_t(Normalized\=@=Grid,append(_,["NORMALIZED!!!"=Normalized|_],Grids)),
     term_variables(Normalized,RV1),
     nop(((((GV1\=@=RV1 ; (Normalized\=@=Grid,Normalized=[[_]])) -> ShowQ = true ; ShowQ = _)))),

     compress_grid(COps,Grid,Compressed), if_t(Compressed\=@=Normalized,append(_,["Compressed!!!"=Compressed|_],Grids)),

     if_t(DoFF,((constrain_grid(f,_TrigF,Grid,GridFF), if_t(GridFF\=@=Grid,append(_,["Find"=GridFF|_],Grids)),
       copy_term(Grid+GridFF,GG1+GridFFNV,GoalsFF), numbervars(GG1+GridFFNV+GoalsFF,10,_,[attvar(bind),singletons(false)])))),

     append(_,[],Grids),
     HH is (OH - 1) * 2, 
     call_w_pad(HH, print_side_by_side(Grids)),

     if_t(has_goals(GridFFNV),writeg(gridFF=GridFFNV)),

     if_t((nonvar(COps),COps\==[]), 
       (writeg(cops=COps), 
        nop((unreduce_grid(Compressed,COps,Uncompressed), 
        if_t(Uncompressed\=@=Grid, (ShowQ=true,writeg("Bad Uncompressed"=Uncompressed))))))),

     if_t((nonvar(NOps),NOps\==[]), 
       (writeg(nops=NOps),
        if_t((ShowQ==true;Normalized\=@=Grid;has_goals(Normalized);true), writeg(normalized=Normalized)),
        nop((unreduce_grid(Normalized,NOps,Unnormalized), 
        if_t(Unnormalized\=@=Grid, (ShowQ=true,writeg("Bad Unnormalized"=Unnormalized))))))),
     
     if_t((ShowQ==true;has_goals(Grid)),    writeg(grid=Grid)),
     

     %writeg("NGrid"=NGrid),
   true))),
  WillHaveShown = [loc2D(OH,OV),center2G(CX,CY),size2D(H,V)|SubGroupS],    
  if_t(is_object(Obj),
    (format('~N~n'),
     if_t(menu_or_upper('i'),
       locally(nb_setval(debug_as_grid,nil),
         underline_print(debug_indiv_obj(Obj,WillHaveShown)))))),

  format('~N'),dash_chars(15))),!.

:- ansi_term:import(debug_indiv_obj/2).
debug_indiv_obj(AS0):-debug_indiv_obj(AS0,[]).
debug_indiv_obj(Obj,ExceptFor):- Obj = obj(A), nonvar(A),!,debug_indiv_obj(A,ExceptFor).
debug_indiv_obj(Props,ExceptFor):- is_open_list(Props),!,must_det_ll((append(Props,[],CProps),!,debug_indiv_obj(CProps,ExceptFor))).
debug_indiv_obj(AS0,ExceptFor):- 
 must_det_ll((
  Obj = obj(AS0),
  append(AS0,[],Props),
  %ignore((o2g(Obj,GGG), nonvar(GGG),set_glyph_to_object(GGG,Obj))),
 % will_show_grid(Obj,TF),
  TF = false,
 % obj_to_oid(Obj,MyOID),
  %o2ansi(MyOID,MissGlyph),
  object_color_glyph_short(Obj,SGlyph),

  my_partition(is_functor(link),Props,ISLINK,AS1), r_props(ISLINK,ISLINKR),
  my_partition(is_o3,AS1,TVSO0,AS),  r_props(TVSO0,TVSO), predsort_on(arg(2),TVSO,TVSOR),reverse(TVSOR,TVSOS),
  short_indv_props(AS,TVSI1,TVSI2),append(TVSI1,TVSI2,TVSI),
  %flatten(TV,F),predsort(longer_strings,F,[Caps|_]), 
  append([TVSI,Props],ASFROM), choose_header(ASFROM,Caps), toPropercase(Caps,PC),
  
  ignore((TF==true,dash_chars)),
  sformat(S,"% ~w:\t~w  ",[PC,SGlyph]), format('~N~s',[S]),
  print_if_non_nil(ExceptFor,TVSI),
  print_if_non_nil(ExceptFor,ISLINKR),
  print_if_non_nil(ExceptFor,TVSOS),
  ignore(( TF==true, mass(Obj,Mass),!,Mass>4, vis2D(Obj,H,V),!,H>1,V>1, localpoints(Obj,Points), print_grid(H,V,Points))),
  ignore(( fail, mass(Obj,Mass),!,Mass>4, vis2D(Obj,H,V),!,H>1,V>1, show_st_map(Obj))),
  %pp(Props),
  ignore(( TF==true,dash_chars)))),!.

print_if_non_nil(ExceptFor,TVSI):- 
  include(is_not_in(ExceptFor),TVSI,TVSI_PP),
  ignore(((TVSI_PP \==[], format('~N  '),pp(TVSI_PP)))).





:- discontiguous debug_indiv/1. 

debug_indiv(_):- is_print_collapsed,!.
debug_indiv(R):- is_object_props(R),!,debug_indiv(obj(R)).
debug_indiv(_):- format('~N'),fail.
debug_indiv(Var):- plain_var(Var),pp(debug_indiv(Var)),!.
debug_indiv(Grid):- is_grid(Grid),!,debug_as_grid(is_grid,Grid),!.
debug_indiv(Grid):- is_cpoints_list(Grid),!,debug_as_grid(is_cpoint,Grid).
debug_indiv(Grid):- maplist(is_point,Grid),!,debug_as_grid(is_point,Grid).
debug_indiv(Atom):- atom(Atom),into_gridoid(Atom,Gridoid),pp(into_gridoid(Atom)),!,debug_as_grid(Gridoid).

debug_indiv(List):- is_list(List),length(List,Len),!,
 w_section((debug_indiv = Len),((
  ppnl(debug_indiv = Len),
  print_list_of(debug_indiv,debug_indiv_list,List),
  %max_min(Len,40,_,Min),
  %forall(between(1,Min,N),(N=<40->(nth1(N,List,E),debug_indiv(E));ppnl(total = 40/Len))),
  !))).


/*
debug_indiv(A):- is_point_obj(A,Color,Point),
  obj_to_oid(A,Tst,Id), i_glyph(Id,Sym),
  hv_point(H,V,Point), i_glyph(Id,Sym),
  ppnl([' % Point: ', color_print(Color,Sym), dot, color(Color), fav1(Tst), nth(Id), loc2D(H,V)]),!. 
*/
object_color_desc(PA,PenColors):- 
  pen(PA,Pen), colors_cc(PA,Colors), 
  display_length(Pen,PenL), display_length(Colors,ColorsL), 
  ((PenL=<ColorsL) -> PenColors=pen(Pen);PenColors=colors_cc(Colors)).

object_birth_desc(PA,Birth):-
  indv_props_list(PA,Props),findall(B,member(iz(B),Props),BBs),
  predsort_on(birth_info,BBs,ByDL),last(ByDL,Birth).

birth_info(ifti(Birth),2+InfoLen):- !, display_length(Birth,InfoLen).
birth_info(indiv(Birth),0+InfoLen):- !, display_length(Birth,InfoLen).
birth_info(Birth,1+InfoLen):- display_length(Birth,InfoLen).

object_ref_desc(Obj, OUTS):- 
  into_obj(Obj,PA),
  object_color_glyph_long(PA,GA),% mass(PA,Mass),
  colorlesspoints(PA,Shape),loc2D(PA,X,Y), rot2L(PA,ROT), vis2D(PA,XX,YY),
  shape_id(Shape,ShapeID),
  object_birth_desc(PA,Birth),
  object_color_desc(PA,PenColors),
  OUT = objFn(GA,[b(Birth),loc2D(X,Y),rot2L(ROT),vis2D(XX,YY),sid(ShapeID),PenColors]),
  colorize_oterms(OUT,OT),
  wots(SS,write(OT)),!,
  OUTS = SS.
object_ref_desc(PA,objFn(GA)):- object_color_glyph_long(PA,GA),!.
object_ref_desc(PA,objFn(GA)):- object_color_glyph_short(PA,GA),!.


object_ref_desc_no_loop(PA, OUTS):- object_ref_desc(PA, OUTS),!.
object_ref_desc_no_loop(PA, OUTS):-
 must_det_ll((
  object_color_glyph_long(PA, CGA),
  mass(PA,Mass),
  colorlesspoints(PA,Shape),pen(PA,Pen),loc2D(PA,X,Y), rot2L(PA,ROT),
  shape_id(Shape,ShapeID),  
  OUT = oFn(CGA,Mass,loc2D(X,Y),ROT,pen(Pen),ShapeID),
  wots(SS,wqs_l(OUT)),
  OUTS = SS)).

/*
tersify1(I,Q):- is_object(I),object_color_glyph_short(I,FC), o2g(I,O),!,wots(A,color_print(FC,call(format('"~w"',[O])))),
   mass(I,M),
   wots(S,call(write(objFn(A,M)))),atom_string(Q,S).
*/
%object_dglyph(O,G):- object_cglyph(O,G). % want this
%object_cglyph(G,CGlyph):- color(G,C),object_glyph(G,Glyph),wots(CGlyph,color_print(C,Glyph)).
%object_dglyph(O,D):- object_glyph(O,G), atom_concat(' ',G,D),!.

to_realer_color(Var,white):- plain_var(Var),!.
to_realer_color(C,C):- is_real_color(C),!.
to_realer_color(C,C):- is_unreal_color(C),!.
to_realer_color(C,N):- into_color_name_always(C,CN),C\=@=CN,!,to_realer_color(CN,N).
object_glyph_colorz(Obj,Colors):-
  unique_colors(Obj,[UC|CL]),
  findall(RC,(member(FC,[UC|CL]),to_realer_color(FC,RC)),NColors),
  (nonvar(UC)-> flatten_set([NColors,UC],Colors);Colors=NColors),!.
object_glyph_colorz(_,[fg]).

object_color_glyph_long(PA, CGA):- 
 must_det_ll((
  obj_to_oid(PA,OID),
  object_glyph_colorz(PA,Colors),
  print_colors_on_ss(OID,Colors,CGAO),
  CGA=CGAO)).

object_color_glyph_short(Obj,SGlyph):- object_color_glyph_long(Obj,SGlyph),!.
object_color_glyph_short(Obj,SGlyph):-
 must_det_ll((
  o2g(Obj,Glyph),
  object_glyph_colorz(Obj,Colors),
  print_colors_on_ss(Glyph,Colors,SGlyph))).
  
print_colors_on_ss(Glyph,[],SSGlyph):- sformat(SSGlyph,'~q',[Glyph]),!.
print_colors_on_ss(Glyph,Colors,SGlyph):- display_length(Glyph,N), 
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

object_color_glyph_old(Obj,S):- o2g(Obj,G),colors_cc(Obj,Colors),maplist(arg(1),Colors,NColors),
  wots(S,maplist(user:print_ncolors1(G),NColors)),!.

print_ncolors(G,C):- color_print(C,G).
print_ncolors1(G,C):- sformat(F,'~w',[G]),sub_string(F,0,1,_,SS),color_print(C,SS).

o2c(Obj,Glyph):- color(Obj,Glyph),!.

o2ansi(I,S):- integer(I),int2glyph(I,G),!,o2ansi(G,S). 
o2ansi(G,S):- atom(G),!,g2o(G,O),o2ansi(O,S),!.
o2ansi(G,S):- \+ is_object(G),!,colorize_oterms(G,S).
o2ansi(Obj,S):- object_color_glyph_short(Obj,S),!.
o2ansi(Obj,S):- object_color_glyph_old(Obj,S),!.

colorize_oterms(G,GG):- c_ot(c_o,G,GG),!.
colorize_oterms(P2,G,GG):- c_ot(P2,G,GG),!.

c_o(O,A):- is_object(O),O=obj(_),object_color_glyph_short(O,A),!.

c_ot(_P2,O,A):- var(O),!,A=O.
%c_ot(P2,O,A):- term_contains_ansi(O),!,A=O.
c_ot(_P2,O,A):- term_is_ansi(O),!,A=O.
c_ot(P2,O,A):- lock_doing(c_ot,O,call(P2,O,A)),!.
c_ot(_P2,O,A):- number(O),!,wots(A,bold_print(write(O))).
c_ot(P2,-O,-A):- !, c_ot(P2,O,A).
c_ot(P2,+O,+A):- !, c_ot(P2,O,A).
c_ot(P2,O,A):- is_list(O),!,maplist(colorize_oterms(P2),O,A).
c_ot(P2,O,A):- compound(O),compound_name_arguments(O,F,Args),!,
  maplist(colorize_oterms(P2),Args,AArgs),
  (Args=@=AArgs-> O=A ; compound_name_arguments(A,F,AArgs)).

c_ot(_P2,O,A):- \+ atom(O),!,A=O.
c_ot(_P2,O,A):- is_color(O),!,wots(A,color_print(O,O)).
c_ot(_P2,O,A):- member(O,[n,s,e,w,c,ne,se,sw,nw]),!,wots(A,bold_print(write(O))).
c_ot(_P2,O,A):- lock_doing(c_ot,O,o2ansi(O,A)),!.
c_ot(_P2,O,O).

prefered(repaired).
prefered(full_grid).
prefered(flag(hidden)).
prefered(neededChanged).
prefered(changed).
prefered(nsew).
prefered(colormass).
prefered(alone_dots).
prefered(hv_line(_)).
prefered(dg_line(_)).
prefered_header(cc(Caps,_),Caps):- freeze(Caps,wbg == Caps).
prefered_header(cc(Caps,_),Caps):- get_black(Black),freeze(Caps,Black == Caps).
prefered_header(pg(_OG,_,_,Caps),Caps):- freeze(Caps,i_bg_shapes == Caps).
prefered_header(pg(_OG,/*sf*/(_),1,Caps),Caps):- freeze(Caps,atom(Caps)).
prefered_header(pg(_OG,/*sf*/(_),last(_),Caps),Caps):- freeze(Caps,atom(Caps)).
%prefered_header(/*b*/iz(Caps),PCaps):-prefered(PCaps),freeze(Caps,(nonvar(Caps),Caps = PCaps)).
%prefered_header(iz(Caps),PCaps):-prefered(PCaps),freeze(Caps,Caps == PCaps).
prefered_header(Caps,PCaps):-prefered(PCaps),freeze(Caps,(nonvar(Caps),Caps = PCaps)).
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

show_touches(Only,Obj):- must_det_ll((into_obj(Obj,RealObj),show_touches0(Only,RealObj))).
show_touches0(Only,Obj):- is_not_in(Only,Obj),!.
show_touches0(Only,Obj):- Obj = obj(List), 
 must_det_ll((
  object_ref_desc_no_loop(Obj,SGlyph),
  include(is_functor(link),List,TP),
  include(is_arg_in(Only),TP,TPO),!,
  show_touches2(SGlyph,TPO))).
show_touches2(_,[]):-!.
show_touches2(SGlyph,TP):-   
  remove_too_verbose(SGlyph,TP,OO),
  format("~N~n% ~w:  ",[SGlyph]),wqs_l(OO),format('~N').

dbg_show_touches(Only,Obj):- must_det_ll((into_obj(Obj,RealObj),dbg_show_touches0(Only,RealObj))).
dbg_show_touches0(Only,Obj):- must_det_ll(( \+ is_not_in(Only,Obj))),!.
dbg_show_touches0(Only,Obj):- Obj = obj(List), 
 must_det_ll((
  object_ref_desc_no_loop(Obj,SGlyph),
  include(is_functor(link),List,TP),
  include(is_arg_in(Only),TP,TPO),!,
  dbg_show_touches2(SGlyph,TPO))).
dbg_show_touches2(_,[]):-!.
dbg_show_touches2(SGlyph,TP):-   
  remove_too_verbose(SGlyph,TP,OO),
  format("~N~n% ~w:  ",[SGlyph]),wqs_l(OO),format('~N').


write_indented_list(F,WQS):- format(F),wqs(WQS).

not_too_verbose(X):- X\==(''), X\==s('').

is_o3(pg(_OG,_,_,_)).

show_st_map(Obj):-
  ignore(( 
  localpoints(Obj,Points),
%  mass(Obj,Mass),!,Mass>4,
%  vis2D(Obj,H,V),!,H>1,V>1,
  format('~N'),
  solidness(Points,0,inf,Res),
  solidness_no_diag(Points,0,inf,ResND),
  solidness_is_diag(Points,0,inf,ResD),
  print_side_by_side(print_side_by_side(ResND,ResD),Res))).


debug_indiv([]):- !.

debug_indiv(diff(_)):-!.
debug_indiv([Other]):-debug_indiv(Other),!.
debug_indiv(P):- is_rule(P,Q), w_section(( pp(Q))).

is_rule(P,_):- \+ compound(P),!,fail.
is_rule(A:-true,A):-!.
is_rule(A:-B,A:-B):-!.


debug_indiv(Other):-
 w_section((
  functor(Other,F,A),
  ppnl(other = F/A),
  pp(Other))).



%alt_id(_MyID,ID,Alt):- int2glyph(ID,Alt).
alt_id(MyOID,ID,Alt):- Alt is abs(MyOID-ID).
remove_too_verbose(_MyID,Var,plain_var(Var)):- plain_var(Var),!.
remove_too_verbose(_MyID,H,''):- too_verbose(H),!.
remove_too_verbose(MyOID,List,ListO):- is_list(List),!,maplist(remove_too_verbose(MyOID),List,ListO),!.

% @TODO UNCOMMENT THIS remove_too_verbose(MyOID,dot,"point"):- !.
%remove_too_verbose(MyOID,line(HV),S):- sformat(S,'~w-Line',[HV]).
%remove_too_verbose(MyOID,square,S):- sformat(S,'square',[]).
% @TODO UNCOMMENT THIS remove_too_verbose(MyOID,background,S):- sformat(S,'bckgrnd',[]).
%remove_too_verbose(MyOID,iz(H),HH):- compound(H), remove_too_verbose(MyOID,H,HH),!.
remove_too_verbose(MyOID,giz(H),HH):- compound(H), remove_too_verbose(MyOID,H,HH),!.

%remove_too_verbose(_MyID,obj_to _oid(_ * _ * X,Y),NTH):- NTH=..[X,Y].
%remove_too_verbose(_MyID,obj_to_oid(_ * _+_ * X,Y),NTH):- NTH=..[X,Y].
%remove_too_verbose(_MyID,obj_to _oid(_ * X,Y),NTH):- NTH=..[X,Y].

remove_too_verbose(MyOID,link(Touched,ID,Dir),HH):- %number(MyOID),
  MyOID\==0,integer(ID),alt_id(MyOID,ID,Alt),o2ansi(ID,Glyph),
  remove_too_verbose(0,link(Touched,Alt,Dir,Glyph),HH).

remove_too_verbose(MyOID,link(Touched,ID),HH):- % number(MyOID),
  MyOID\==0, integer(ID),alt_id(MyOID,ID,Alt),o2ansi(ID,Glyph),
  remove_too_verbose(0,link(Touched,Alt,Glyph),HH).

remove_too_verbose(MyOID,TP,OO):- fail, compound(TP),compound_name_arguments(TP,link,[F|A]),atom(F),
   maplist(colorize_oterms,A,AA),
   compound_name_arguments(TPP,F,AA),!,remove_too_verbose(MyOID,TPP,HH),
   OO= HH,!.

remove_too_verbose(MyOID,colors_cc(H),HH):- !, remove_too_verbose(MyOID,H,HH).
%remove_too_verbose(MyOID,loc2D(X,Y),loc2D(X,Y)).
%remove_too_verbose(MyOID,vis2D(X,Y),size2D(X,Y)).
remove_too_verbose(_MyID,changes([]),'').
remove_too_verbose(_MyID,rot2L(sameR),'').
remove_too_verbose(MyOID,L,LL):- is_list(L),!, maplist(remove_too_verbose(MyOID),L,LL).
remove_too_verbose(_MyID,H,HH):- compound(H),arg(1,H,L), is_list(L), maybe_four_terse(L,T),H=..[F,L|Args],HH=..[F,T|Args].
remove_too_verbose(_MyID,H,H).

too_verbose(P):- compound(P),compound_name_arity(P,F,_),!,too_verbose(F).
too_verbose(globalpoints).
too_verbose(monochrome).
too_verbose(colorlesspoints).
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
too_verbose(cenGX).
too_verbose(cenGY).

debug_indiv(_,_,X,_):- too_verbose(X),!.
debug_indiv(Obj,_,F,[A]):- is_cpoints_list(A),!,
  vis2D(Obj,H,V), ppnl(F), 
  loc2D(Obj,OH,OV),
  EH is OH+H-1,
  EV is OV+V-1,
  object_glyph(Obj,Glyph),
  Pad1 is floor(H),  
  Pad is floor(20-V/2),
  max_min(Pad,OH,PadH,_),
  call_w_pad(PadH,
    (dash_chars(Pad1,' '),write(F=Glyph),
     print_grid(OH,OV,EH,EV,Obj))).
/*
  nop(wots(S,
    (dash_chars(Pad1,' '),write(Id=Glyph),
     print_grid(H,V,A)))),
*/



debug_indiv(_,P,_,_):- pp(P).




:- include(kaggle_arc_footer).


