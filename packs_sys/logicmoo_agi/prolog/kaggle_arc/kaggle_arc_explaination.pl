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

describe_feature(Grid,List):- is_list(List),!,my_maplist(describe_feature(Grid),List).
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
combine_grids(append,H,G,GM):- grid_size(H,W,_),length(Row,W), append(G,[Row|H],GM).
  
print_info:- test_config(noprint_info),!,fail.
print_info:- test_config(print_info),!.
print_info:- test_config(indiv(_)),!.

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


print_info_1(G):- print_info(G).

print_info_l(Global):- my_maplist(print_info_1,Global).


:- discontiguous print_info/1. 
print_info(_):- is_print_collapsed,!.
print_info(_):- format('~N'),fail.
print_info(Var):- plain_var(Var),pp(print_info(Var)),!.
print_info(Atom):- atom(Atom),into_gridoid(Atom,Gridoid),pp(into_gridoid(Atom)),!,print_info(Gridoid).
print_info(Grid):- is_grid(Grid),!,show_indiv(is_grid,Grid),!.
print_info(OBJ):- compound(OBJ), OBJ=obj(_), show_indiv(print_info,OBJ),!.
print_info(R):- is_object_props(R),!,print_info(obj(R)).
print_info(TName):- fix_test_name(TName,TestID,ExampleNum), is_valid_testname(TestID), !, pp(testid(TestID>ExampleNum)).

print_info(A):- is_grid(A),print_grid(print_info,A),!.
print_info(Grid):- is_cpoints_list(Grid),!,print_grid(is_cpoints_list,Grid).
print_info(Grid):- my_maplist(is_point,Grid),!,print_grid(is_points_list,Grid).
  %print_info(obj(A)):- is_list(A),!, dash_chars,   my_maplist(print_info_2(obj(A)),A), dash_chars,!.  print_info_2(Obj,P):- compound(P),!,compound_name_arguments(P,F,A),print_info(Obj,P,F,A),!.
print_info(diff(_)):-!.

print_info([Other]):-print_info(Other),!.
print_info(P):- is_rule(P,Q), w_section(( pp(Q))).

is_rule(P,_):- \+ compound(P),!,fail.
is_rule(A:-true,A):-!.
is_rule(A:-B,A:-B):-!.

%print_info(A):- is_group(A),maybe_cache_glyphs(A),show_indiv('group',A),!.
print_info(List):- is_list(List),length(List,Len),!,
 w_section(title(print_info = Len),
 ((print_list_of(print_info,print_info = Len,List),
  %max_min(Len,40,_,Min),
  %forall(between(1,Min,N),(N=<40->(nth1(N,List,E),print_info(E));ppnl(total = 40/Len))),
  !))).
  
%print_info(A):- into_obj(A,Obj),print_info(Obj).
print_info(A):- is_point_obj(A,Color,Point),
  obj_to_oid(A,Id), i_glyph(Id,Sym),
  hv_point(H,V,Point), i_glyph(Id,Sym),
  ppnl([' % Point: ', color_print(Color,Sym), dot, color(Color), nth(Id), loc2D(H,V)]),!. 
print_info(Other):-
 functor(Other,F,A),
 w_section((
  ppnl(other = F/A),
  pp(Other))).
print_info(A):- pp(A),!.



show_indiv(Grid):- is_grid(Grid),!,print_grid(Grid),nl.
show_indiv(Grid):- show_indiv('',Grid),!.
:- discontiguous show_indiv/2.
show_indiv(Why,R):- atom(R), atom_contains(R,'_'), pp_parent([LF|_]), \+ (LF==sf;LF==objFn), 
  resolve_reference(R,Var), R\==Var, \+ plain_var(Var),!, 
  write(' '), writeq(R), write(' /* '), show_indiv(Why,Var), write(' */ ').
%show_indiv(Why,R):- is_object_props(R),!,show_indiv(Why,obj(R)).
show_indiv(Why,obj(A)):- is_cons(A), \+ is_list(A), \+ \+ ( append(A,[],A),show_indiv(Why,obj(A))),!.
%show_indiv(Why,obj(A)):- \+ is_list(A),!, pp(show_indiv(Why,obj(A))).
%show_indiv(Why,Grid):- \+ is_object(Grid), !, writeln(why(Why)),!,print_info(Grid).

show_indiv(Why,A):- nb_current(debug_indiv,f), 
   \+ \+ locally(nb_setval(debug_indiv,f),show_indiv_textinfo(Why,A,[])),!.
%show_indiv(  I,   A):- is_1gridoid(A), !, make_bg_visible(A,AA), print_grid(I,AA).
show_indiv(Why, Obj):- wants_html,!,show_indiv_html(Why, Obj).
show_indiv(Why,A):- \+ \+ show_indiv_object(Why,A).

fruity:- fruity([green,red,yellow,cyan,blue,white]).
fruity(Colors):- my_maplist(fruity,Colors).
fruity(Color):- color_print(Color,call(dash_chars(45))).

show_indiv_object(Why, Obj):-
  format('~N'),dash_chars(45),fruity,dash_chars(45),
  must_det_ll((
  vis2D(Obj,H,V),
  DoFF = false,

  %findall(SubGroup,is_in_subgroup(Grp,Obj,SubGroup),SubGroupS), 
  %pp(subGroupS=SubGroupS),
  
 
   %object_or_global_grid(Obj,LG+Type,Global),                
   global_grid(Obj,Global),
   loc2D(Obj,OH,OV), ignore(center2G(Obj,CX,CY)), object_glyph(Obj,Glyph),
   Title = show_indiv(Why,objFn(Glyph),loc2D(OH,OV),center2G(CX,CY),size2D(H,V)),
   object_grid(Obj,Grid),
   pp(Title),
   print_grid(global,Global),
   Grids = _, %["Global"=Global|_],
   if_t((H\==1;V\==1),
     
    (append(_,["Object"=Grid|_],Grids), 
     term_variables(Grid,GV1),
     (((((copy_term(Obj,CObj),object_ngrid(CObj,NGrid), append(_,["NGrid"=NGrid|_],Grids)))))),

     ShowQ=_,
     
     (((((normalize_grid(NOps,Grid,Normalized), 
         if_t(Normalized\=@=Grid,(sformat(SN,'~q',['NORMALIZED!!!'(NOps)]),append(_,[SN=Normalized|_],Grids)))))))),

     term_variables(Normalized,RV1),
     (((((GV1\=@=RV1 ; (Normalized\=@=Grid,Normalized=[[_]])) -> ShowQ = true ; ShowQ = _)))),

     if_t(nonvar(Normalized),
       (compress_grid(COps,Normalized,Compressed), 
         if_t(Compressed\=@=Grid,(sformat(CN,'~q',['Compressed!!!'(COps)]),append(_,[CN=Compressed|_],Grids))))),

     if_t(DoFF,((constrain_grid(f,_TrigF,Grid,GridFF), if_t(GridFF\=@=Grid,append(_,["Find"=GridFF|_],Grids)),
       copy_term(Grid+GridFF,GG1+GridFFNV,GoalsFF), numbervars(GG1+GridFFNV+GoalsFF,10,_,[attvar(bind),singletons(false)]))))
   )),
     append(_,[],Grids),!,
     HH is (OH - 1) * 2, 
     call_w_pad(HH, print_side_by_side(Grids)),

     if_t(has_goals(GridFFNV),writeg(gridFF=GridFFNV)),

      if_t((nonvar(NOps)), 
        (writeg(nops=NOps),
         if_t((ShowQ==true;Normalized\=@=Grid;has_goals(Normalized);true), writeg(normalized=Normalized)),
         nop((unreduce_grid(Normalized,NOps,Unnormalized), 
         if_t(Unnormalized\=@=Grid, (ShowQ=true,writeg("Bad Unnormalized"=Unnormalized))))))),

     if_t((nonvar(COps)), 
       (writeg(cops=COps), 
        nop((unreduce_grid(Compressed,COps,Uncompressed), 
        if_t(Uncompressed\=@=Normalized, (ShowQ=true,writeg("Bad Uncompressed"=Uncompressed))))))),

     if_t((ShowQ==true;has_goals(Grid)),    writeg(grid=Grid)),
     

     %writeg("NGrid"=NGrid),
   true)),
  WillHaveShown = [loc2D(OH,OV),center2G(CX,CY),size2D(H,V),globalpoints(Global),grid_ops(norm,Normalized)],    
  if_t(is_object(Obj),
    (format('~N~n'),
     if_t((true;menu_or_upper('i');menu_or_upper('o')),
       locally(nb_setval(debug_indiv,f),
         underline_print(show_indiv_textinfo(Why,Obj,WillHaveShown)))),

  format('~N'),dash_chars(15))),!.

show_indiv_textinfo(AS):-show_indiv_textinfo('',AS,[]).
show_indiv_textinfo(Why,Obj,ExceptFor):- indv_props_list(Obj,Props),Obj\=@=Props,!,show_indiv_textinfo(Why,Props,ExceptFor).
show_indiv_textinfo(Why,Obj,ExceptFor):- Obj = obj(A), nonvar(A),!,show_indiv_textinfo(Why,A,ExceptFor).
show_indiv_textinfo(Why,Props,ExceptFor):- is_open_list(Props),!,must_det_ll((append(Props,[],CProps),!,show_indiv_textinfo(Why,CProps,ExceptFor))).
show_indiv_textinfo(Why,AS0,ExceptFor):- catch(show_indiv_textinfo1(Why,AS0,ExceptFor),_,true),!.
show_indiv_textinfo(Why,AS0,_ExceptFor):- pp(show_indiv_textinfo(Why)=AS0),!.
show_indiv_textinfo1(Why,AS0,ExceptFor):- user:'@'(show_indiv_textinfo2(Why,AS0,ExceptFor),user),!.
show_indiv_textinfo2(Why,AS0,ExceptFor):-
 must_det_ll((
  %Obj = obj(AS0),
  append(AS0,[],Props11),


  % pp(vAS0=AS0),
  extend_grp_proplist([obj(Props11)],[obj(Props)]),

  %ignore((o2g(Obj,GGG), nonvar(GGG),set_glyph_to_object(GGG,Obj))),
 % will_show_grid(Obj,TF),
  TF = false,
 % obj_to_oid(Obj,MyOID),
  %o2ansi(MyOID,MissGlyph),
  Obj = obj(Props), 

  %pp(props=Props),
  object_color_glyph_short(Obj,SGlyph),

  my_partition(p1_subterm(p1_or(is_points_list,is_gridoid)),Props,ContainsGrid,Props1), 

  my_partition(p1_or(is_functor(link),is_functor(elink)),Props1,ISLINK,Props2),

  my_partition(is_o3,Props2,Rankings0,Props3),  sr_props(Rankings0,Rankings1), 
  predsort_on(arg(2),Rankings1,Rankings2),reverse(Rankings2,Rankings),

  %short_indv_props(Props3,TVSI1,TVSI2), 
  %append(TVSI1,TVSI2,TVSI),
   TVSI = Props3,
  %flatten(TV,F),predsort(longer_strings,F,[Caps|_]), 
  append([TVSI,Props],ASFROM), choose_header(ASFROM,Caps), toPropercase(Caps,PC),
  
  ignore((TF==true,dash_chars)),
  sformat(S,"% ~w ~w:\t~w  ",[Why,PC,SGlyph]), format('~N~s',[S]),
  wots(SZ,((
  print_if_non_nil(props,ExceptFor,TVSI),
  print_if_non_nil(links,ExceptFor,ISLINK),
  print_if_non_nil(gridoids,ExceptFor,ContainsGrid),
  print_if_non_nil(rankings,ExceptFor,Rankings)))),
  
  nonvar(SZ),
  atom_length(SZ,Len),(Len<10 -> pp(AS0);write(SZ)),
  ignore(( TF==true, mass(Obj,Mass),!,Mass>4, vis2D(Obj,H,V),!,H>1,V>1, localpoints(Obj,Points), print_grid(H,V,Points))),
  ignore(( fail, mass(Obj,Mass),!,Mass>4, vis2D(Obj,H,V),!,H>1,V>1, show_st_map(Obj))),
  %pp(Props),
  ignore(( TF==true,dash_chars)))),!.


:- export(show_indiv_textinfo/3).
:- ansi_term:import(show_indiv_textinfo/3).

sr_props(Sort,SortRO):- \+ is_list(Sort),!,sr_props([Sort],SortRO).
sr_props(Sort,SortRO):- predsort(sort_on(prop_display_order),Sort,SortR),Sort\=@=SortR,!,sr_props(SortR,SortRO).
sr_props(Sort,SortRO):- colorize_oterms(Sort,SortRO),!.
sr_props(Sort,Sort).
%sr_props(Sort,Sort).

prop_display_order(iz(P),iz+F):- prop_display_order(P,F).
prop_display_order(giz(P),giz+F):- prop_display_order(P,F).
prop_display_order(P,az+F):- functor(P,F,_).

print_if_non_nil(Title,ExceptFor,TVSI):- sr_props(TVSI,SortRO),
  include(is_not_in(ExceptFor),SortRO,TVSI_PP),
  ignore(((TVSI_PP \==[], nl_if_needed, 
    format('~w == ',[Title]),print_o_props(TVSI_PP)))).

%sr_props2(Sort,SortRO):- remove_too_verbose(0,Sort,SortR),Sort\=@=SortR,!,sr_props2(SortR,SortRO).
sr_props2(Sort,SortRO):- colorize_oterms(Sort,SortRO),!.
sr_props2(Sort,Sort).

%print_if_non_nil(I):- wants_html,!,pp(I).
print_o_props(I):- is_list(I),my_maplist(print_o_props,I),!.
print_o_props(I):- sr_props2(I,M),wots(SS,wqs(M)),!,write_trim_h_space(SS).
%print_o_props(I):- !, pp(I).

write_trim_h_space(SS):- atom_concat(' ',S,SS),!,write_trim_h_space(S).
write_trim_h_space(SS):- atom_concat('\t',S,SS),!,write_trim_h_space(S).
write_trim_h_space(SS):- atom_concat('\n',S,SS),!,write_trim_h_space(S).
write_trim_h_space(SS):- nl_if_needed, write('  \t  '),write(SS).


show_indiv_html(_Why, Obj):- obj(ObjL)=Obj,
  object_color_glyph_short(Obj,SGlyph),
  my_partition(arg1(is_grid),ObjL,_GridArgs,NonGridArgs),
  my_partition(arg1(is_cpoints_list),NonGridArgs,_CArgs,NonCArgs),
  my_partition(arg1(is_points_list),NonCArgs,_PArgs,NonPArgs),
  sformat(S,'Object ~w',[SGlyph]),
  global_grid(Obj,GG),grid_rep(norm,Obj,NormGrid),
  grid_ops(norm,Obj,NormOps),
  wots(S1,writeq(global_grid(SGlyph))),wots(S2,writeq(NormOps)),
  print_side_by_side(green,GG,S1,_,NormGrid,S2),!,
  %print_side_by_side(green,GG,SGlyph,_,NormGrid,NormOps),!,
  pp([S|NonPArgs]).



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


printable_grid(_,_,Grid,GridO,GridO):-localpoints_include_bg(Grid,GridO).
printable_grid(H,V,Grid,GridO,NGrid):-
     localpoints_include_bg(Grid,GridOM),
     points_to_grid(H,V,GridOM,GridO),
     make_bg_visible(GridO,PrintGrid),    
     copy_term(PrintGrid,PrintGridC),
     ( false -> into_ngrid(PrintGridC,NGrid); NGrid = PrintGridC),!.     

%show_indiv(_Why,Obj):- is_bg_object(Obj),!.
arg1(P1,E):- arg(1,E,E1),call(P1,E1),!.




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
  shape_rep(grav,PA,Shape),loc2D(PA,X,Y), rot2D(PA,ROT), vis2D(PA,XX,YY),
  shape_id(Shape,ShapeID),
  object_birth_desc(PA,Birth),
  object_color_desc(PA,PenColors),
  OUT = objFn(GA,[b(Birth),loc2D(X,Y),rot2D(ROT),vis2D(XX,YY),sid(ShapeID),PenColors]),
  colorize_oterms(OUT,OT),
  %trace,
  OUTS = OT,!.
  %wots(SS,write(OT)),!,
  %OUTS = SS.
object_ref_desc(PA,objFn(GA)):- object_color_glyph_short(PA,GA),!.
object_ref_desc(PA,objFn(GA)):- object_color_glyph_long(PA,GA),!.



object_ref_desc_no_loop(PA, OUTS):- object_ref_desc(PA, OUTS),!.
object_ref_desc_no_loop(PA, OUTS):-
 must_det_ll((
  object_color_glyph_long(PA, CGA),
  mass(PA,Mass),
  shape_rep(grav,PA,Shape),pen(PA,Pen),loc2D(PA,X,Y), rot2D(PA,ROT),
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
  object_glyph_long(PA,OID),
  object_glyph_colorz(PA,Colors),
  print_colors_on_ss(OID,Colors,CGAO),
  CGA=CGAO)).

object_glyph_long(PA, OID):- 
 must_det_ll((
  obj_to_oid(PA,OID))),!.

%object_color_glyph_short(Obj,SGlyph):- object_color_glyph_long(Obj,SGlyph),!.
object_color_glyph_short(Obj,SGlyph):-
 must_det_ll((
  o2g(Obj,Glyph),
  object_glyph_colorz(Obj,Colors),
  print_colors_on_ss(Glyph,Colors,SGlyph))).
  
print_colors_on_ss(Glyph,[],SSGlyph):- sformat(SSGlyph,'~q',[Glyph]),!.
print_colors_on_ss(Glyph,Colors,SGlyph):- display_length(Glyph,N), 
  wots(SGlyph,print_colors_on(Colors,N,Glyph)).
print_colors_on([Color],_,Glyph):- color_print(Color,call(writeq(Glyph))),!.
print_colors_on(Colors,L,Glyph):- length(Colors,CL), CL>L,write('\''), user:my_maplist(print_ncolors(Glyph),Colors), write('\''),!.
print_colors_on(Colors,_,Glyph):- atom_chars(Glyph,Chars),write('\''),print_colors_on_s(Colors,Chars),write('\''),!.
print_colors_on(Colors,Glyph):- write('\''), user:my_maplist(print_ncolors(Glyph),Colors), write('\'').
print_colors_on_s([],G):-  format('~s',[G]).
print_colors_on_s([C],G):- sformat(GS,'~s',[G]),color_print(C,GS).
print_colors_on_s([C|Color],Glyph):- length([C|Color],CL),length(Glyph,GL),CLL is CL div GL,CLL>1,length(GLL,CLL),append(GLL,More,Glyph),
  sformat(G,'~s',[GLL]),!,color_print(C,G),print_colors_on_s(Color,More).
print_colors_on_s([C|Color],[G|Glyph]):- color_print(C,G),print_colors_on_s(Color,Glyph).

object_color_glyph_old(Obj,S):- o2g(Obj,G),colors_cc(Obj,Colors),my_maplist(arg(1),Colors,NColors),
  wots(S,my_maplist(user:print_ncolors1(G),NColors)),!.

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
c_ot(_,giz(O),giz(O)):- !.
c_ot(P2,O,A):- lock_doing(c_ot,O,call(P2,O,A)),!.
c_ot(_P2,O,A):- number(O),!,wots(A,bold_print(write(O))).
c_ot(P2,-O,-A):- !, c_ot(P2,O,A).
c_ot(P2,+O,+A):- !, c_ot(P2,O,A).
c_ot(P2,O,A):- is_list(O),!,my_maplist(colorize_oterms(P2),O,A).
c_ot(P2,O,A):- compound(O),compound_name_arguments(O,F,Args),!,
  my_maplist(colorize_oterms(P2),Args,AArgs),
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

is_o3(pg(_OG,_,_,_)):- fail.

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


%alt_id(_MyID,ID,Alt):- int2glyph(ID,Alt).
alt_id(MyOID,ID,Alt):- Alt is abs(MyOID-ID).

remove_too_verbose(_MyID,Var,plain_var(Var)):- plain_var(Var),!.
remove_too_verbose(_MyID,H,''):- too_verbose(H),!.
remove_too_verbose(MyOID,List,ListO):- is_list(List),my_maplist(remove_too_verbose(MyOID),ListM,ListO),exclude(==(''),ListM,ListO),!.
remove_too_verbose(_MyOID,List,ListO):- is_list(List),exclude(==(''),List,ListO).
% @TODO UNCOMMENT THIS remove_too_verbose(MyOID,dot,"point"):- !.
%remove_too_verbose(MyOID,line(HV),S):- sformat(S,'~w-Line',[HV]).
%remove_too_verbose(MyOID,square,S):- sformat(S,'square',[]).
% @TODO UNCOMMENT THIS remove_too_verbose(MyOID,background,S):- sformat(S,'bckgrnd',[]).
remove_too_verbose(MyOID,iz(H),HH):- compound(H), remove_too_verbose(MyOID,H,HH),!.
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
   my_maplist(colorize_oterms,A,AA),
   compound_name_arguments(TPP,F,AA),!,remove_too_verbose(MyOID,TPP,HH),
   OO= HH,!.

remove_too_verbose(MyOID,colors_cc(H),HH):- !, remove_too_verbose(MyOID,H,HH).
%remove_too_verbose(MyOID,loc2D(X,Y),loc2D(X,Y)).
%remove_too_verbose(MyOID,vis2D(X,Y),size2D(X,Y)).
remove_too_verbose(_MyID,changes([]),'').
%remove_too_verbose(_MyID,rot2D(sameR),'').
%remove_too_verbose(MyOID,L,LL):- is_list(L),!, my_maplist(remove_too_verbose(MyOID),L,LL).
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

print_info(_,_,X,_):- too_verbose(X),!.
print_info(Obj,_,F,[A]):- is_cpoints_list(A),!,
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



print_info(_,P,_,_):- pp(P).




:- include(kaggle_arc_footer).


/*
Partitioned control structures
When there are a thousand or more rules as in major current expert systems, the rules can interrelate in many ways, and a bug can be hard to trace. So just like large computer programs, it's a good idea to divide rules into groups, modules, or partitions for which members of each group have minimal interactions with members of other groups. You can think of each group as a separate rule-based system that may occasionally decide to call on another for a separate analysis. An advantage is that the rule groups can be written and debugged mostly separately. This idea is called a partitioning or context-limiting control structure.

Diagnosis expert systems provide good examples. For instance in diagnosis of a malfunctioning car, there are major systems of the car that don't interact much with one another. If electrical devices aren't working, you can be pretty sure that the problem is in the electrical system; or if the car goes forward but not backward, you can be pretty sure the problem is in the transmission. So you can put rules for electrical problems in one partition, and rules for transmission problems in another, rules for the engine and fuel system in another, rules for the car body in another, and so on. You'll need one other partition of rules, a "startup" partition, to look at key evidence and decide which partition appears most relevant to a problem. And partitions can choose to transfer control to another partition, if say none of their own rules succeed.

Meta-rules
A general approach can encompass all the control structure ideas so far: specification of control by a rule-based system itself. Meta-rules are just rules whose domain of knowledge is the operation of another rule-based system; they're a kind of heuristic, a topic we'll investigate more thoroughly in Chapter 9. Rules deciding to load partitions (Section 6.6) are one simple example of meta-rules, but they can do many other things. Remember that one goal for putting knowledge into computers was to make explicit the complicated "common-sense" knowledge people have but don't realize they have. How to order rules and use them is another kind of common-sense knowledge, also formalizable. Here are some example meta-rules for a backward-chaining-like rule-based system, to control selection of the next rule to try to satisfy instead of following the database order of rules:

--Prefer the rule that handles the most serious issue.
--Prefer the rule that was written by the most knowledgeable human.
--Prefer the rule that is fastest to execute.
--Prefer the rule that has been used successfully the most times.
--Prefer the rule with the most things in common with the last rule successfully applied.
The big advantage of meta-rules is their flexibility and modifiability, which allows precise control of a rule-based system.
Meta-rules can express things besides rule orderings and partition referrals. Prolog interpreters make the closed-world assumption or lack-of-knowledge inference: if you can't prove something true, assume it false. This may be unfair for some predicates; a meta-rule could then override normal reasoning. For instance, a meta-rule could say to use the closed-world assumption only when querying predicates from a certain list, and to assume a failure means yes otherwise.

Meta-rules seem to be important in human reasoning. People aren't generally systematic enough to use any of the chaining methods successfully, but instead they rely on problem-specific meta-rules for deciding what to do next. So to reason more naturally, meta-rules are critical. But figuring out just what meta-rules people do use is hard.

Decision lattices
We'll now consider some lower-level control structure ideas: decision lattices, concurrency, and and-or-not lattices. In the terminology of computer science, these are compiled structures. But they're compiled in a different sense than what programming-language "compilers" produce: they represent a simplifying first step before the traditional compiler operates. Some people don't consider these compiled structures truly artificial intelligence, but they're so closely linked to artificial intelligence that we'd better explain them.

Choosing a good sequence for rules can be important and hard, as we discussed in Section 6.5. But computers can use storage structures besides sequences (see Appendix C). They can organize rules in a hierarchy, what is called a decision lattice or discrimination net. Decision lattices do a restricted but very efficient kind of reasoning, a kind of classification. The idea is to always specify where to go next in the computer based on question answers. In other words, a kind of finite-state machine. (Sometimes they're called decision trees, but technically they're lattices since branches that diverge can converge or "grow back together" later. Any graph without cycles in which this convergence can happen is a lattice and not a tree; cycles wouldn't make much sense here because you'd be asking the same question twice.)

For instance, consider an expert system to diagnose malfunctions of small household appliances (see Figure 6-5). It is important first to distinguish problems within the appliance from problems outside the appliance. A good way is to ask if the appliance works at all. If it doesn't, ask if it is plugged in. If it isn't, that is the problem. If it is, ask if other electric devices nearby (lights, clocks, etc.) work. If they don't, the problem sounds like a blown fuse. If other appliances definitely work, the problem must be internal to the faulty appliance. If no such observations can be made (as when there are no electrical appliances nearby), try plugging the faulty appliance into another outlet to see if the problem reappears.

On the other hand, if the appliance partially works, then it matters what kind of appliance it is. That's because interpretation of partial-failure clues is quite appliance-dependent, like smoke when the device has a heating element. As another example, strange noises are more serious in a device with no moving parts than in a blender. So the next question for a partially-working appliance should classify it.

So decision lattices impose a classification hierarchy on the universe based on observations. They are useful for simple expert systems, with several advantages:

1. Implementation is easy: just use pointers (memory references). They can even be implemented without a computer, as printed text with cross-references.
2. They need not explicitly question a human being: they can examine buffer contents or sensor readings. Then they can be fast, faster than the chaining methods, because no matching, binding, or backtracking is needed.
3. They can be designed to ask the absolutely minimal number of questions necessary to establish conclusions, unlike chaining methods for which such optimization can be difficult.

But decision lattices have major disadvantages as a compiled or "low-level" control structure:
1. They can't reason properly or efficiently for many applications because they don't easily permit variables or backtracking.
2. They are difficult to modify and debug, since later questions must assume certain results to earlier questions.
3. They can't easily reuse query answers since they don't explicitly cache.
4. They may be hard to build, because at each point you try to determine the best question to ask, something not so easy to judge.

Decision lattices were around long before computers. Expert-system technology only made significant progress when decision-lattice control structures were mostly abandoned, due to the limitations mentioned.
Concurrency in control structures
If speed of a rule-based system is important (as in a real-time application), and multiple processors are available, a control structure can use concurrency. Usually the processors must share and access the same database of facts and rules for this to work well. For a Prolog-style rule-based system, four types of parallelism for concurrency are identified (see Figure 6-6): (1) partition parallelism, (2) or parallelism, (3) and parallelism, and (4) variable-matching parallelism. These parallelisms are useful with all three kinds of chaining.

Partition parallelism means running different partitions of the rules simultaneously. Each partition can reason separately, though they can explicitly pass conclusions to one another, or cache into a global database. This is good if we've got groups of rules that don't interact much, each group relevant to a problem.

"And" parallelism is parallelism among expressions "and"ed on the right side of a rule or a query. Usually it is only done for the predicate expressions that do not bind variables, the "tests" in the generate-and-test concept (see Section 3.12). These tests can be done concurrently on separate processors; if any test fails, the whole "and" should fail, and the other processors should be sent a message to stop work. Otherwise, the "and" should succeed. "And" parallelism is probably not a good idea when some tests are much harder to satisfy than others; then the hard tests should go first (see Chapter 13).

"Or" parallelism usually means parallelism between rules with the same left-side predicate name. It is good when there are many such rule groups. Or-parallel rules are sometimes called demons because they're like little people each independently waiting for a particular set of conditions to be satisfied. "Or" parallelism can also mean parallel pursuit of facts in forward chaining.

Variable-matching parallelism is parallelism in the argument matching done when matching two predicate expressions to one another. It makes each match attempt faster, but it doesn't change the usual sequential examining of the database. It only pays off when you have a significant number of predicates with two or more arguments.

Concurrency can be simulated on a sequential machine. This gives a new class of control structures, the sequential reductions of concurrent process descriptions. This idea is often associated with the "agenda" search methods in Chapter 10.

Parallelism is not just an efficiency trick, however. Parallelism is necessary to model many real-world phenomena. These phenomena are often addressed in object-oriented programming, for which the world is divided into clusters of facts representing objects, each with its own partitioned module of rules governing its behavior. Object-oriented programming is especially useful for simulations. For instance, objects (and their facts) can represent organisms in an ecosystem, and rule modules for each kind of organism can govern the behavior of each object. Another application is to modeling components of a car, where each object represents a part of a car. We'll talk more about object-oriented programming in Chapter 12. While it emphasizes rule-partition parallelism, it can also involve the other three kinds. For instance in modeling organisms in an ecosystem, "and" and "or" parallelism can reflect the ability of organisms to do and think several things simultaneously.

And-or-not lattices
The extreme case of parallelism in rule-based systems is the and-or-not lattice representation of rules, in which each rule can be thought (or maybe actually is) a hardware logic gate incessantly computing a certain logical combination of input logic signals representing facts and intermediate conclusions. (It's sometimes incorrectly called an and-or tree, but like in the decision lattice, the paths can recombine after splitting, so it isn't a tree necessarily.) "And"s in a rule become "and" gates, "or"s become "or" gates, and "not"s becomes inverter gates.

*/