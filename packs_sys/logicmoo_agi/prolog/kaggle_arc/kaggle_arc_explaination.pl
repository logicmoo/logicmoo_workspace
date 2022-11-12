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
print_list_of(_,Title,[]):- pp(no_data(Title)),!.
print_list_of(P1,Title,O):- \+ is_list(O),!,print_list_of(P1,Title,[O]).
print_list_of(P1,Title,O):-
 length(O,Len),
 collapsible_section(info,Title,maybe,
 (((Title\=[] -> pp(Title=Len); pp(P1=Len)),
  maybe_cache_glyphs(O),
  %save_grouped(print_list_of(Title),O),
  g_out( maplist(ignore_call(P1),O))))),!.

ignore_call(P1,A):- ignore(call(P1,A)).

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

object_grid_to_str(Grid,Str,Title):- 
  vis2D(Grid,H,V), 
  object_glyph(Grid,Glyph),
  Title = object_grid(loc2D(OH,OV),size2D(H,V)),
  loc2D(Grid,OH,OV),
  localpoints_include_bg(Grid,GridO),
  ((IH=H,IV=V)), % (IH = 30,IV=30), 
  get_black(Black),subst001(GridO,Black,wbg,GridOO),
  wots(GS,(print_grid(IH,IV,GridOO))),
  replace_in_string(['®'=Glyph],GS,GSS),  
  HH is (OH - 1) * 2, wots(Str,(print_w_pad(HH,GSS))).



debug_as_grid(Grid):- debug_as_grid('',Grid),!.

:- discontiguous debug_as_grid/2.
debug_as_grid(Why,R):- is_object_props(R),!,debug_as_grid(Why,obj(R)).
debug_as_grid(Why,R):- atom(R), atom_contains(R,'_'), pp_parent([LF|_]), \+ (LF==sf;LF==objFn), 
  resolve_reference(R,Var), R\==Var, \+ plain_var(Var),!, 
  write(' '), writeq(R), write(' /* '), debug_as_grid(Why,Var), write(' */ ').

/*
     localpoints_include_bg(Grid,GridO),
*/
debugged_object_grid(Grid,GridO):- object_grid(Grid,GridO).
%debugged_object_grid(Grid,GridO):- global_grid(Grid,GridO).
debugged_object_grid(Grid,GridOO):- 
  localpoints_include_bg(Grid,GridO),  
  get_black(Black),subst001(GridO,Black,wbg,GridOO).

%debug_as_grid(Why,R):- resolve_reference(R,Var)-> R\==Var, write(' ( '), writeq(R),write(' , '),debug_as_grid(Why,Var),write(' )'),!.
debug_as_grid(Why,Grid):- (is_object(Grid)/*;is_grid(Grid)*/),!,
  must_det_ll((
  vis2D(Grid,H,V),
  object_glyph(Grid,Glyph),  
  Title = debug_as_grid(Why,objFn(Glyph),loc2D(OH,OV),size2D(H,V)),
  if_t((H\==1;V\==1;true),
    must_det_ll((
     loc2D(Grid,OH,OV),     
     localpoints_include_bg(Grid,GridO),
     get_black(Black),
     subst001(GridO,Black,wbg,PrintGrid),    
     copy_term(PrintGrid,PrintGridC),
     ignore((
        O = PrintGrid, once(grid_to_norm(O,Ops,N)), O\=@=N, print_side_by_side(Ops,O,N),writeln(Ops))),
     into_ngrid(PrintGridC,NGrid),
     nop((shape2D(Grid,SX,SY),max_min(H,SX,IH,_),max_min(V,SY,IV,_))),
     ignore(IV=V),ignore(IH=H),
     %wots(GS,print_grid(IH,IV,Title,PrintGridC)),replace_in_string(['®'=Glyph,'@'=Glyph],GS,GSS),
     %wots(S,print_side_by_side(GSS,print_grid(IH,IV,ngrid,NGrid))),
     wots(S,print_grid(IH,IV,Title,NGrid)),
     HH is (OH - 1) * 2, print_w_pad(HH,S)))),
  if_t(is_object(Grid),
    (format('~N~n'),
     locally(nb_setval(debug_as_grid,nil),underline_print(debug_indiv(Grid))))),
     format('~N'),dash_chars(15))),!.

debug_as_grid(  I,   A):- is_1gridoid(A), !, get_black(Black), subst001(A,Black,'wbg',AA), print_grid(I,AA).
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
  max_min(Len,40,_,Min),
  forall(between(1,Min,N),(N=<40->(nth1(N,List,E),debug_indiv(E));wqnl(total = 40/Len))),
  dash_chars,!.


debug_indiv(obj(A)):- \+ is_list(A),!, pp(debug_indiv(obj(A))).

/*
debug_indiv(A):- is_point_obj(A,Color,Point),
  obj_to_oid(A,Tst,Id), i_glyph(Id,Sym),
  hv_point(H,V,Point), i_glyph(Id,Sym),
  wqnl([' % Point: ', color_print(Color,Sym), dot, color(Color), fav1(Tst), nth(Id), loc2D(H,V)]),!. 
*/
object_glyph_color(Obj,FC):- once((unique_colors(Obj,CL),member(FC0,CL),is_real_color(FC0));FC0=wfg),
  (FC0==black_n -> FC= wbg ; FC = FC0).

object_s_glyph(Obj,SGlyph):- 
  object_glyph(Obj,Glyph), 
  unique_colors(Obj,NColors),
  % writeq(NColors),
  object_glyph_color(Obj,FC),
  wots(SGlyph,
   (
     (( \+ (member(E,NColors),E==FC))->color_print(FC,Glyph);true),
     user:maplist(print_ncolors(Glyph),NColors))).


prefered(repaired).
prefered(full_grid).
prefered(invisible).
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

% I didn't really have the programming chops to take his program and give it human level reasoning until about 5 years ago
debug_indiv(obj(A)):- 
  wots_vs(SS,\+ \+ debug_indiv_obj(A)),!,
  %writeq(SS),
  write(SS).


choose_header(ASFROM,Caps):- once((prefered_header(P,Caps),member(P,ASFROM),\+ skip_header(Caps),ground(Caps))),!.
choose_header(ASFROM,Caps):- once((prefered_header(P,CapsO),member(P,ASFROM),term_to_atom(CapsO,Caps))).

skip_header(X):- compound(X).
skip_header(grid_sz(_,_)).
skip_header(sizeX(_)).
skip_header(sizeY(_)).


debug_indiv_obj(A):- nb_current(debug_as_grid,t),debug_as_grid(A),!.
debug_indiv_obj(A):- Obj = obj(A), is_list(A),!,
 maplist(must_det_ll,[
  %ignore((o2g(Obj,GGG), nonvar(GGG),set_glyph_to_object(GGG,Obj))),
%debug_indiv(Obj):- Obj = obj(A), is_list(A),  
  
  =(A,AS0),
 % will_show_grid(Obj,TF),
  TF = false,
  obj_to_oid(Obj,MyOID),
  %o2ansi(MyOID,MissGlyph),
  object_s_glyph(Obj,SGlyph),
  append(AS0,[],AS),  
  remove_too_verbose(MyOID,AS,TV0), include(not_too_verbose,TV0,TV),

  %flatten(TV,F),predsort(longer_strings,F,[Caps|_]), 
  =(TV,ASA),reverse(ASA,ASAR),
  append(ASAR,AS,ASFROM),
  choose_header(ASFROM,Caps),  
  toPropercase(Caps,PC),
  sort(TV,TVS),
  my_partition(is_o3,TVS,TVSO,TVSI),
  predsort(sort_on(arg(2)),TVSO,TVSOR),reverse(TVSOR,TVSOS),
  ignore((TF==true,dash_chars)),
  sformat(SF,"% ~w:\t\t~w\t",[PC,SGlyph]),
  ignore(( g_out_style(style('font-size2D','75%'),(write(SF), wqs(TVSI))))),
  %maplist(write_indented_list('~N    '),wqs(TVSOS),
  format('~N    '),wqs(TVSOS),
  ignore(( TF==true, amass(Obj,Mass),!,Mass>4, vis2D(Obj,H,V),!,H>1,V>1, localpoints(Obj,Points), print_grid(H,V,Points))),
  ignore(( fail, amass(Obj,Mass),!,Mass>4, vis2D(Obj,H,V),!,H>1,V>1, show_st_map(Obj))),
  %pp(A),
  ignore(( TF==true,dash_chars))]),!.

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

debug_indiv(obj(A)):- \+is_list(A),!, append(A,[],A),debug_indiv(obj(A)),!.
debug_indiv(obj(A)):- is_list(A),!, 
  dash_chars,  
  maplist(debug_indiv_2(obj(A)),A),
  dash_chars,!.

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

debug_indiv_2(Obj,P):- compound(P),!,compound_name_arguments(P,F,A),debug_indiv(Obj,P,F,A),!.


%alt_id(_MyID,ID,Alt):- int2glyph(ID,Alt).
alt_id(MyOID,ID,Alt):- Alt is abs(MyOID-ID).
remove_too_verbose(_MyID,Var,plain_var(Var)):- plain_var(Var),!.
remove_too_verbose(_MyID,H,''):- too_verbose(H),!.
remove_too_verbose(MyOID,List,ListO):- is_list(List),!,maplist(remove_too_verbose(MyOID),List,ListO),!.

% @TODO UNCOMMENT THIS remove_too_verbose(MyOID,dot,"point"):- !.
%remove_too_verbose(MyOID,line(HV),S):- sformat(S,'~w-Line',[HV]).
%remove_too_verbose(MyOID,square,S):- sformat(S,'square',[]).
% @TODO UNCOMMENT THIS remove_too_verbose(MyOID,background,S):- sformat(S,'bckgrnd',[]).
remove_too_verbose(MyOID,iz(H),HH):- remove_too_verbose(MyOID,H,HH),!.

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
   compound_name_arguments(TPP,F,A),!,remove_too_verbose(MyOID,TPP,HH),
   OO= g(HH),!.


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
too_verbose(gid).
too_verbose(giz).
too_verbose(grid_sz).
too_verbose(localpoints).
too_verbose(grid).
%too_verbose(link).
too_verbose(grid_size).
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




:- fixup_exports.


