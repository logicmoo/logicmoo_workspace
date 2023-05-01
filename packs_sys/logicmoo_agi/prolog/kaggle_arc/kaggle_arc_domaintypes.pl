/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

:- ensure_loaded(kaggle_arc_typecheck).


relax_color_arg(C,C):- var(C),!.
relax_color_arg(C,bg):- is_bgp(C).
relax_color_arg(E,fg):- is_color(E),!.

to_real_grid(G,GO):- notrace((unnumbervars(G,G1),get_bgc(BG),subst001(G1,bg,BG,GO))),!. % ,ignore([[BG|_]|_]=GO).


has_color(C,Cell):- only_color_data(Cell,CD), cmatch(C,CD).

cmatch(C,CD):- sub_var(C,CD),!.
cmatch(C,CD):- plain_var(C),!,var(CD),C==CD.
cmatch(C,CD):- var(C),!, once(C=@=CD; \+ C\=CD).
cmatch(plain_var,CD):- !, plain_var(CD).
cmatch(C,CD):- is_point(CD),!,compound(CD), arg(_,CD,E),cmatch(C,E),!.
cmatch(is_colorish_var,CD):- !,var(CD),is_colorish(CD).
cmatch(fg,CD):- !, CD\==wbg, is_fg_color(CD),!.
cmatch(wbg,CD):- !,(CD==wbg;is_bg_color(CD)),!.
cmatch(bg,CD):- !, (CD==black; is_bg_color(CD)),!.
%cmatch(P,CD):- is_real_color(P),!, \+ P\==CD.
cmatch(P,CD):- is_colorish(P),!, \+ P\=CD.
cmatch(P,CD):- call(P,CD),!.

free_bg(BGC,S,FB):- is_list(S),!,mapgrid(free_bg(BGC),S,FB).
free_bg(_,S,FB):- plain_var(S),!,FB=S.
free_bg(_BGC,BG,X):- BG==bg, put_attr(X,ci,free(bg)),!.
free_bg(BGC,Black,X):- contains_var(BGC,Black),!, put_attr(X,ci,free(BGC)).
free_bg(_,X,X).

unfree_bg(BGC,S,FB):- is_list(S),!,mapgrid(unfree_bg(BGC),S,FB).
unfree_bg(_,S,BGW):- var(S),!,((get_attr(S,ci,free(BGW)))->true;S=BGW).
unfree_bg(BGC,A/**/-B,AABB):- unfree_bg(BGC,A,B,AABB),!.
unfree_bg(_,X,X).

unfree_bg(_,S,T,FB):- plain_var(S),plain_var(T),!,FB=T.
unfree_bg(BGC,A,B,AA/**/-BB):- unfree_bg(BGC,A,AA),unfree_bg(BGC,B,BB).

get_bg_label(wbg).
get_fg_label(fg).
bg_sym('bg').
fg_sym('fg').


dif_color(C,CD):- C==CD,!,fail.
dif_color(C,CD):- C==bgw,!,CD\==bgw.
dif_color(C,CD):- C==bg,!,CD\==bg.
dif_color(_,_).

% =============================
% Color types
% =============================
is_fg_color(C):- C==black, !, fail,get_black(Black),!,Black\==black.
is_fg_color(C):- is_bg_color(C),!,fail.
is_fg_color(C):- attvar(C),!,get_attr(C,ci,fg(_)).
is_fg_color(C):- is_color(C),!.
%is_fg_color(C):- C == fg.

%is_bg_color(BG):- plain_var(BG),!,fail.
is_bg_color(BG):- var(BG),!,get_attr(BG,ci,bg(_)),!.
is_bg_color(C):- bg_sym(BG),C==BG,!.
is_bg_color(wbg).
is_bg_color('#104010').
is_bg_color(C):- get_bgc(BG),C==BG,!.

is_real_bg_color(C):- is_bg_color(C),is_real_color(C).

is_black_or_bg(BG):- is_black(BG)-> true; is_bg_color(BG).
%is_black_or_bg(0).

:- decl_pt(helper,is_black(color)). 
is_black(C):- get_black(B),!,C==B.


:- use_module(library(logicmoo/util_bb_frame)).
set_fg_vars(Vars):-
  all_different_bindings(Vars),
  my_maplist(decl_many_fg_colors,Vars).

is_fg_color_if_nonvar(Trig,V):- plain_var(V),Trig==run,!,fail,constrain_type(V,is_fg_color_if_nonvar(Trig,V)).
is_fg_color_if_nonvar(Trig,V):- nop(ppnl(is_fg_color_if_nonvar(Trig,V))),fail.
is_fg_color_if_nonvar(_Trig,C):- is_fg_color(C),!.
is_bg(C):- is_bg_color(C).
is_bgc(C):- is_bg_color(C).

is_bgp(Cell):- plain_var(Cell),!,fail.
is_bgp(Cell):- only_color_data(Cell,C), is_bg_color(C),!.
is_bgp(Cell):- var(Cell),!,fail.
is_bgp(C-_):- !, is_bgp(C).
is_bgp('$VAR'(C)):-!, is_bgp(C).
is_bgp('_').

is_fgp(Cell):- only_color_data(Cell,C), is_fg_color(C).

is_bg_sym_or_var(C):- attvar(C),get_attr(C,ci,fg(_)),!,fail.
is_bg_sym_or_var(C):- (attvar(C); bg_sym(C); C==' '; C==''; C=='bg'; C == 0),!.

mv_peek_color(C,V):- mv_peek1(C,V),V\==C,is_color(C).
mv_peek1(C,W):- multivar:is_mv(C),multivar:mv_peek_value1(C,W),!.

%:- system:import(mv_peek_color/2).

mv_ansi_color(C,Color):- attvar(C),get_attr(C,ci,fg(N)), (nonvar(N) -> arc_acolor(N,Color);arc_acolor(fg,Color)),!.
mv_ansi_color(C,fg(Color)):- attvar(C),get_attr(C,ci,bg(_)),get_bgc(BG),!,arc_acolor(BG,Color),!.
mv_ansi_color(C,Color):- attvar(C),mv_peek_color(C,N), (nonvar(N) -> arc_acolor(N,Color);(N\==C,arc_acolor(fg,Color))),!.

same_colors(_Trig,C1I,_C1O):- \+ is_spec_fg_color(C1I,_),!.
same_colors(_Trig,C1I,C1O):- nonvar(C1O),!,C1I=C1O.
same_colors(Trig,C1I,C1O):- constrain_type(C1O,same_colors(Trig,C1I,C1O)).

color_texture_point_data(Cell,C,Texture,Point):- only_color_data(Cell,C),only_neib_data(Cell,Texture),!,only_point_data(Cell,Point).

%set_as_fg(V,fg(N)):- atomic(N), put_attr(V,ci,fg(N)),!,atom_concat(fg,N,Lookup),luser_linkval(Lookup,V).
%set_as_fg(V,Sym):- put_attr(V,ci,Sym).
attach_fg_ci(CO,_C):- nonvar(CO),!.
attach_fg_ci(CO,C) :- decl_many_fg_colors(CO),C=CO.

is_spec_bg_color(C,CO):- is_bg_color(C),!,is_spec_color(C,CO).
is_spec_fg_color(C,CO):- is_fg_color(C),!,is_spec_color(C,CO).

%is_spec_color(C,CO):- attvar(C),!,get_attr(C,ci,fg(_)), CO=C.


mv_color_name(C,W):- attvar(C),into_color_name_always(C,V),C\==V,color_name(V,W).

into_color_name_always(Grid,Grid):- !.
into_color_name_always(Grid,Grid):- ground(Grid),!.
into_color_name_always(Grid,GridI):- is_grid(Grid), !, mapgrid(into_color_name_always,Grid,GridI),!.
into_color_name_always(Grid,GridI):- compound(Grid), !, map_pred(into_color_name_always,Grid,GridI),!.
into_color_name_always(V,C):- is_real_color(V),!,C=V.
into_color_name_always(V,C):- is_fg_color(V),!,(mv_peek_color(V,C)->true;C=fg).
into_color_name_always(V,C):- is_bg_color(V),!,(mv_peek_color(V,C)->true;C=wbg).
into_color_name_always(V,V):- plain_var(V),!.
into_color_name_always(Grid,Grid).
%into_color_name_always(C,C):- attvar(C),cant_be_color(C,_E),!.
%into_color_name_always(_,fg).

is_spec_color(V,C):- into_color_name_always(V,C),!,atom(C),!,C\==fg,C\==fg,C\==wbg,C\==bg.

is_color(CO):- notrace(is_color0(CO)).
is_color0(CO):- attvar(CO),!,get_attr(CO,ci,_).
is_color0(CO):- is_unreal_color(CO).
is_color0(CO):- is_real_color(CO).

:- multifile(color_decls/0).
:- dynamic(color_decls/0).
color_decls.

is_unreal_color(C):- (C==fg; C==fg; C==wbg ; C==bg ; C==is_colorish_var ; C==plain_var),!.
is_real_color(C):- \+ atom(C),!,fail.
is_real_color(C):- is_unreal_color(C),!,fail.
is_real_color(C):- atom_concat('#',Rest,C),!,atom_length(Rest,6).
is_real_color(C):- named_colors(L),member(C,L),!.
get_real_fg_color(C):- named_colors(L),member(C,L),is_fg_color(C).
is_real_fg_color(C):- C\== is_colorish_var, is_real_color(C),is_fg_color(C).

decl_one_fg_color(Color):- put_attr(Color,ci,fg(Color)).
decl_many_fg_colors(X):- put_attr(X,ci,fg(X)),decl_multivar(X).
decl_many_bg_colors(X):- put_attr(X,ci,bg(X)),decl_multivar(X).
decl_bg_color(X):- put_attr(X,ci,bg(X)).
decl_fill_color(X):-  put_attr(X,ci,hollow(X)),
    freeze(X,(mv_peek1(X,V)->(var(V)->true;is_color(V));true)),decl_multivar(X).

decl_multivar(_):- !, fail, itrace.
decl_multivar(X):- multivar:multivar(X).

decl_not_color(NC,GC):- is_bg_color(NC),!,decl_many_fg_colors(GC).
decl_not_color(NC,GC):- decl_fill_color(GC),!,dif(GC,NC).

decl_one_color(CEdge):-  var(CEdge)->put_attr(CEdge,ci,hollow(CEdge));true.

%cauh(Atts,Value):- attvar(Value), !, arg(1,Atts,Self),  get_attrs(Self,SAttrs), get_attrs(Value,VAttrs).

cauh(_,free(_),_Val):- !.
cauh(_,bg(_),Val):- is_fg_color(Val),!, fail.
cauh(Self,bg(_),Val):- Val ==bg,!, nop((get_bgc(Black), Self = Black)).
cauh(_,fg(_),Val):- is_bg_color(Val),!, fail.
cauh(_,fg(_),Val):- Val ==fg,!.
cauh(Self,_,Val):- cant_be_color(Self,CBC), is_spec_color(Val,C), C == CBC, !, fail.
cauh(_Self,Atts,Val):- var(Val), !, (get_attr(Val,ci,_Other) -> true ; put_attr(Val,ci,Atts)).
cauh(_Self,_Atts,Val):- \+ is_color(Val),!, fail.
cauh(_,_,_).

ci:attr_unify_hook(Atts,Val):- (arg(1,Atts,Self)-> cauh(Self,Atts,Val) ; true).
%ci:project_attributes(QueryVars, ResidualVars):-
ci:attribute_goals(Var) --> {findall(Meaning,ci_meaning(Var,Meaning),MeaningList)},MeaningList.

ci_meaning(Var,cant_be_color(Var,Color)):- cant_be_color(Var,Color).
ci_meaning(Var,cbg(Var)):- get_attr(Var,ci,bg(_)).
ci_meaning(Var,cfg(Var)):- get_attr(Var,ci,fg(_)).
% ci:attr_portray_hook(Var)

cant_be_color(Y):- get_attr(Y,dif,_),!.
cant_be_color(Y):- get_attr(Y,cc,_),!.
cant_be_color(C,E):- attvar(C), get_attr(C,dif,XX),!, sub_term(E,XX),is_color(E).

%is_colorish(C):- attvar(C),!,get_attr(C,ci,_).
is_colorish(C):- is_color(C),!.
is_colorish(C):- cant_be_color(C,_),!.
is_colorish(C):- get_bgc(BG),BG==C,!.
is_colorish(C):- bg_sym(BG),BG==C,!.
is_colorish(C):- fg_sym(FG),FG==C,!.
%is_colorish(C):- compound_var(C,_),!.
%is_colorish(C):- compound(C),!,arg(1,C,A),nonvar(A),is_colorish(A).

is_grid_color(C):- plain_var(C),!,fail.
% makes grid color-s an integer.. 
%is_grid_color(C):- !,integer(C).
% we are using atoms currently
is_grid_color(C/**/-_):- !, is_color(C).
is_grid_color(C):- is_color(C).

is_color_dat(C):- atomic(C),user:color_code(C,W),!,C==W.

:- export(set_bgc/1).
:- decl_pt(set_bgc(color)).
set_bgc(BGC):- var(BGC),!,luser_unsetval(grid_bgc),!.
set_bgc(BGC):- luser_linkval(grid_bgc,BGC).


:- export(get_bgc/1).
:- decl_pt(get_bgc(color)).
get_bgc(BGC):- !,BGC = black.
get_bgc(BGC):- luser_getval(grid_bgc,BGC),!.
get_bgc(BGC):- get_black(BGC).
:- nb_delete(grid_bgc).
:- luser_default(grid_bgc,bgc).

grid_bgc(_IO,BGC):- get_bgc(BGC).

:- export(set_black/1).
:- decl_pt(set_black(color)).
set_black(BGC):- var(BGC),!,ignore(nb_delete(grid_black)),luser_linkval(grid_black,BGC),!.
set_black(BGC):- luser_linkval(grid_black,BGC).

:- export(get_black/1).
:- decl_pt(get_black(color)).
get_black(BGC):- !,BGC = black.
get_black(BGC):- luser_getval(grid_black,BGC),!.
get_black(black).
:- nb_delete(grid_black).
:- luser_default(grid_black,black).


is_color_no_bgc(X):- \+ is_bg_color(X), is_color(X).

:- decl_pt(is_bg_or_var(is_color,is_color)).
is_bg_or_var(_,X):- free_cell(X),!.
is_bg_or_var(BG,X):- X==BG.

free_cell(Var):- plain_var(Var),!.
free_cell(wbg):-!.
free_cell(bg):-!.
%adjacent_point_allowe
free_cell(C):- get_bgc(X),C==X.

non_free_fg(C):- \+ free_cell(C), \+ is_bg_color(C).

/*
?- decl_many_fg_colors(X), X = red, X = blue.

?- decl_bg_color(X), X = black, X = bg.

?- decl_fill_color(X), X = red, X = blue.

decl_many_fg_colors(X), X = red, dif(X,red).
xvarx(X),
put_attr(X,ci,fg(X)),
mv_set_values(X,[red]),
dif(X,red).

138 ?- decl_many_fg_colors(X), X = red, dif(X,red), X=red.
false.

?- decl_many_fg_colors(X), X = red, dif(X,red), X=blue, X = red.
true.

 decl_many_fg_colors(X), X = red, dif(X,red), X=blue,  dif(X,red), X = red.

*/


















filter_indivs(In,Filter,Out):- include(matches_filter(Filter),In,Out).

matches_filter(not(A),OBJ):- !, \+ matches_filter(A,OBJ).
matches_filter(\+ (A),OBJ):- !, \+ matches_filter(A,OBJ).
matches_filter((A;B),OBJ):- !, (matches_filter(A,OBJ);matches_filter(B,OBJ)).
matches_filter([H|T],obj(List)):- !, \+ \+ forall(member(E,[H|T]),member(E,List)).
matches_filter((A,B),OBJ):- !, (matches_filter(A,OBJ),matches_filter(B,OBJ)).
matches_filter(E,obj(List)):- member(EE,List),matches_prop(E,EE).

matches_prop(E,EE):- (var(E);var(EE)),!,E==EE.
matches_prop(E,EE):- E=EE,!.
matches_prop(iz(E),EE):-!,matches_prop(E,EE).
matches_prop(E,iz(EE)):-!,matches_prop(E,EE).

  
pass_thru_group(G):- var(G),!.
pass_thru_group([]).
pass_thru_group([options(_)]).

override_group(P):- P=..[F,M,R], override_group_call(F,M,[],R).
override_group(P):- P=..[F,A,M,R],  override_group_call(F,M,[A],R).
override_group(P):- P=..[F,A,B,M,R], override_group_call(F,M,[A,B],R).

override_group_call(_F,Group,_AB,R):- pass_thru_group(Group),!,R=Group.
override_group_call(F,Group,AB,R):- is_object_group(Group),!, C=..[F|AB],
 findall(R,(member(M,Group),call(C,M,R)),AllRots), append_sets([AllRots],R).


allowed_dir(Type,X):- subtypes(Type,SType),allow_dir_list(SType,List),member(X,List).
allowed_dir([Type|_],X):- !, nonvar(Type), allowed_dir(Type,X).

subtypes(C,C).
subtypes(C,S):- subClassOf(S,C).


%allow_dir_list(squire,[s,e,w]).

allow_dir_list(nsew,[n,s,e,w]). %s,e,n,w 

allow_dir_list(nsew_5,[n,s,e,w]). %s,e,n,w 
allow_dir_list(nsew_2,[n,s,e,w]). %s,e,n,w 

allow_dir_list(s_e,[s,e]). %s,e,n,w 
allow_dir_list(n_e,[n,e]). %s,e,n,w 
allow_dir_list(n_w,[n,w]). %s,e,n,w 
allow_dir_list(s_w,[s,w]). %s,e,n,w 
allow_dir_list(dir_list(List),List). 
allow_dir_list(ne_sw,[ne,sw]). %s,e,n,w 
allow_dir_list(nw_se,[nw,se]). %s,e,n,w 
%allow_dir_list(rectangles,[s,e]). 

%allow_dir_list(colormass,[n,s,e,w]):- arc_option(no_diags),!.
%allow_dir_list(colormass,[n,s,e,w,nw,ne,se,sw]). 
allow_dir_list(diamonds,[nw,sw,se,ne]).
allow_dir_list(colormass,[n,s,e,w,nw,sw,se,ne]).

%allow_dir_list(all,[n,s,e,w]):- arc_option(no_diags),!.
allow_dir_list(all,   [nw,sw,se,ne,n,w,s,e]).
allow_dir_list(hv_line(h),[e,w]).
allow_dir_list(hv_line(v),[n,s]).
allow_dir_list(dg_line(u),[sw,ne]).
allow_dir_list(dg_line(d),[nw,se]).

inv_points_corner(square,diamonds).
inv_points_corner(rectangle,diamonds).
inv_points_corner(diamonds,square).
inv_points_corner(outline,none).
inv_points_corner(all,none).

points_corner_dir(Shape,Dir):- \+ inv_points_corner(Shape,_), !, allowed_dir(Shape,Dir).
points_corner_dir(Shape,Dir):- inv_points_corner(Shape,OShape), allowed_dir(OShape,Dir).

shape_type_dirs(ST,DIRS):- allow_dir_list(ST,DIRS).
shape_type_dir(ST,DIR):- allowed_dir(ST,DIR).
%allow_dir_list(hv_line(h),[e,w]). allow_dir_list(hv_line(v),[n,s]). 
%allow_dir_list(dg_line(u),[ne,sw]). allow_dir_list(dg_line(d),[nw,se]).

%circles, dots, , rays, walls

shape_filter(X,rectangle):- free_cell(X).
shape_filter(X,colormass):- non_free_fg(X).

polyg(border(square), [hv_line(H),hv_line(V),hv_line(H),hv_line(V)]):- h_v(H,V).
polyg(border(diamond),[dg_line(U),dg_line(D),dg_line(U),dg_line(D)]):- u_d(U,D).

h_v(h,v).
u_d(u,d).

sameOrSubClass(Y,Y).
sameOrSubClass(X,Y):- subClassOf(X,Y).

:- dynamic(iz/2).
iz(X,_):- is_grid(X),!,fail.
iz(X,Y):- nonvar(X),var(Y),!,(isz(X,XY),sameOrSubClass(XY,Y),deterministic(YN)), (YN==true->!;true).
iz(X,Y):- nonvar_or_ci(Y)->(subClassOf(P,Y),iz(X,P));(nonvar_or_ci(X),iz(X,P),subClassOf(P,Y)).
iz(X,Y):- nonvar(X),(isz(X,XY),sameOrSubClass(XY,Y),deterministic(YN)), (YN==true->!;true).
iz(X,Y):- (var(X)->enum_object(X);true),isz(X,Y).

subClassOf(noexit,outline(_)).
subClassOf(hollow,outline(_)).
%subClassOf(outl,hollow).
%subClassOf(spaceship,outl).
%subClassOf(outl,spaceship).
subClassOf(thick1,outline(_)).
%subClassOf(outl,rectangle).

subClassOf(hv_line(_),line).
subClassOf(dg_line(_),line).

subClassOf(dot,hv_symmetric).
subClassOf(square,hv_symmetric).
subClassOf(diamond,hv_symmetric).
subClassOf(circle,hv_symmetric).
subClassOf(round,hv_symmetric).
subClassOf(symmetry_type(sym_hv),hv_symmetric).
subClassOf(symmetry_type(sym_hv_non_90),hv_symmetric).
subClassOf(hv_symmetric,h_symmetric).
subClassOf(hv_symmetric,v_symmetric).


subClassOf(triangle,h_symmetric).
subClassOf(hv_line(v),h_symmetric).
subClassOf([monochrome,contiguous,hv_line(v)],v_symmetric).

subClassOf(hv_line(h),v_symmetric).
subClassOf([monochrome,contiguous,hv_line(h)],h_symmetric).


data_type(O,T):- nonvar(T),data_type(O,C),T=C,!.


data_type(O,plain_var):- plain_var(O),!.
data_type(O,point(color)):- is_cpoint(O),!.
data_type(O,point(no_color)):- is_ncpoint(O),!.
data_type(O,point(_)):- is_point(O),!.
data_type(O,CT):- compound(O),!,data_typec(O,CT).
data_type(O,lst(_)=0):- O==[],!.
data_type(O,int):- integer(O),!.
data_type(O,float):- float(O),!.
data_type(O,rational):- rational(O),!.
data_type(O,string):- string(O),!.
data_type(O,color(bg,C,_)):- is_spec_bg_color(O,C),!.
data_type(O,color(fg,C,_)):- is_spec_fg_color(O,C),!.
data_type(O,color(bg,_,_)):- is_bg_color(O),!.
data_type(O,color(fg,_,_)):- is_fg_color(O),!.
data_type(O,color(_,_,_)):- is_colorish(O),!.
data_type(O,blob(Type)):- blob(O,Type),Type\==text,!.
data_type(O,atom):- atom(O),!.
data_type(O,atomic):- atomic(O),!.
data_type(O,unk(O)):-!.

data_typec(num(vals([N|_]),_,_),Type):- nonvar(N),!,data_type(N,Type).
data_typec(lst(vals([N|_]),_,_),Type):- nonvar(N),!,data_type(N,Type).
data_typec(O,object):- is_object(O),!.
data_typec(iz(O),iz(T)):- !, data_type(O,T).
data_typec(diff(_->O),T):- nonvar(O),!, data_type(O,T).
data_typec(O,dict(L)):- is_vm_map(O),get_kov(objs,O,Value),!,data_type(Value,L).
data_typec(O,group(N)):- is_group(O),into_list(O,L),!,length(L,N).
data_typec(Out,grid(H,V)):- is_grid(Out),!,grid_size(Out,H,V).
data_typec(Out,lst(DT)=H):- is_list(Out),!,length(Out,H), last(Out,Last),data_type(Last,DT).
data_typec(_=O,=(N)):- nonvar(O),!,data_type(O,N).
data_typec(Out,S):- compound_name_arity(Out,print_grid,A),arg(A,Out,P),data_type(P,S),!.
data_typec(Out,Type):- compound_name_arguments(Out,F,Args),my_maplist(data_type,Args,DTs),compound_name_arguments(Type,F,DTs),!.
data_typec(Out,FS):- compound_name_arity(Out,F,A),arg(A,Out,P),data_type(P,S),!,FS=..[F,S].


is_point(P):- var(P),!,fail.
is_point(P):- is_ncpoint(P),!.
is_point(P):- is_cpoint(P),!.
is_point(_-P):- is_ncpoint(P),!.

%elems_are(L,P1):- L\==[],is_list(L),my_maplist(P1,L).
elems_are([E|_],P1):- !, call(P1,E),!.

is_points_list(L):- elems_are(L,is_point).
is_cpoints_list(L):- elems_are(L,is_cpoint).
is_ncpoints_list(L):- elems_are(L,is_ncpoint).



%is_cpoints_list(P):- P==[],!.
%is_cpoints_list(List):- is_list(List),!,is_cpoints_list(List).
%is_cpoints_list([G|L]):- is_cpoint(G),!,(L==[];is_cpoints_list(L)),!.

enum_colors(OtherColor):- named_colors(Colors),!,member(OtherColor,Colors).
enum_fg_colors(FG):- enum_colors(FG), is_fg_color(FG), \+ is_bg_color(FG), FG\==fg.
enum_real_colors(FG):- enum_colors(FG), is_real_color(FG).
enum_fg_real_colors(FG):- enum_colors(FG), is_real_color(FG), is_fg_color(FG).
%enum_fg_colors(Color):- enum_colors(Color),is_color_no_bgc(Color).
fill_color(Color,OtherColor):- enum_colors(OtherColor),Color\==OtherColor,is_color_no_bgc(OtherColor).

is_bg_indiv(O):- colors_cc(O,[cc(C,CC)]),CC>0,is_bg_color(C).


is_not_cpoint(I):- \+ is_cpoint(I).

is_not_gpoint(I):- \+ is_gpoint(I).

is_not_tpoint(I):- \+ is_tpoint(I).


is_tpoint(C):- \+ compound(C),!,fail.
is_tpoint(T/**/-P):- is_t(T),!,is_point(P).

is_t(T):- atom(T), atom_length(T,1).

is_cpoint(C):- \+ compound(C),!,fail.
%is_cpoint(C/**/-P):- (nonvar_or_ci(C);cant_be_color(C)),!,is_ncpoint(P).
is_cpoint(T/**/-P):- is_t(T),!,is_cpoint(P).
is_cpoint(_/**/-P):- is_ncpoint(P).

%is_list_of_gt0(P1,List):- is_list(List),my_maplist(P1,List).

:- dynamic(hv_point/3).

is_ncpoint(P):- nonvar(P),hv_point(_,_,P).

is_gpoint(G):- plain_var(G),!,fail.
is_gpoint(_/**/-G):-!,is_gpoint(G).
is_gpoint(G):- hv_point(H,_,G),!,nonvar_or_ci(H),my_assertion(number(H)).

% Grid-oids
is_list_of_gridoids([G|V]):- \+ is_grid([G|V]), is_gridoid(G), is_list(V), my_maplist(is_gridoid,V).

is_1gridoid(G):- is_object(G),!.
is_1gridoid(G):- is_grid(G),!.
is_1gridoid(G):- is_list(G), member(E,G),non_gridoid_list_ele(E), !, fail.
is_1gridoid(G):- is_points_list(G),!.

is_gridoid(G):- plain_var(G),!, fail.
is_gridoid(G):- is_1gridoid(G),!.
%is_gridoid(G):- is_obj_props(G),contains_enough_for_print(G,_),!.
is_gridoid([C|_]):- is_ncpoint(C),!,fail.
is_gridoid(G):- is_list_of_gridoids(G).

non_gridoid_list_ele(C):- plain_var(C),!,fail.
non_gridoid_list_ele(C):- is_color(C),!.
non_gridoid_list_ele(ord(_,_)).
non_gridoid_list_ele(cc(_,_)).
non_gridoid_list_ele(_-Num):- number(Num).
%non_gridoid_list_ele(Num-_):- number(Num).

is_printable_gridoid(G):- plain_var(G),!, fail.
is_printable_gridoid(G):- is_gridoid(G),!.
is_printable_gridoid(G):- is_point(G),!.
is_printable_gridoid(G):- is_cpoint(G),!.
is_printable_gridoid(G):- is_ncpoints_list(G),!.
is_printable_gridoid(D):- is_vm_map(D),get_kov(grid,D,_).
is_printable_gridoid(G):- is_list(G),!,my_maplist(is_printable_gridoid,G).
is_printable_gridoid(G):- resolve_reference(G,R),!,nonvar(R),!.
is_printable_gridoid(G):- known_gridoid(G,R),!,nonvar(R),!.

vm_grid(VM,VM.grid).
vm_obj(VM,O):- member(O,VM.objs).

:- export(is_grid/1).
is_grid(G):- \+ \+  quietly(fast_is_grid(G)).
%is_grid(G):- nonvar(G), \+ \+  quietly(is_grid_of(is_grid_cell,G)).

fast_is_grid(List):- nonvar(List), List\==[], my_maplist(fast_is_row(_LenMinus1),List).
fast_is_row(LenMinus1,[C|List]):- is_list(List), is_grid_cell(C), !, length(List,LenMinus1),!.

is_grid_of(P1,[[C|H]|R]):- 
  call(P1,C),!,is_list(H),is_list(R),
  length([C|H],L),!,
  my_maplist(P1,H),!,
  my_maplist(is_row_len(L),R).
is_row_len(N,L):- is_list(L),length(L,N).

%is_object(H):- is_list(H),is_cpoints_list(H).
%is_grid_cell(AB):- ,!, \+ is_list(AB), sub_term(E,AB),(var(E);is_colorish(E)),!.
is_grid_cell(C):- var(C),!.
is_grid_cell(C):- integer(C),!.
is_grid_cell(A):- \+ compound(A),!,is_grid_cell_e(A).
is_grid_cell(att(_,_)):-!.
is_grid_cell('cell'(_)):-!.
is_grid_cell('{}'(_)):-!.
is_grid_cell('$VAR'(_)):-!.
is_grid_cell((A-B)):- !,(is_grid_cell_e(B);is_grid_cell_e(A)).

is_grid_cell_e(C):- atom(C),!,(is_colorish(C);atom_length(C,1)).
is_grid_cell_e(C):- is_color(C),!.
is_grid_cell_e(C):- integer(C),!,C<13,C>=0.

%is_grid_cell(C):- atomic(C),!.


h_symmetric(Obj):- is_object(Obj),!,object_grid(Obj,Grid),!,h_symmetric(Grid).
h_symmetric(Grid):- is_grid(Grid),!, mirror_h(I,_C,Grid),grid_size(Grid,H,_V), I is floor(H/2).
h_symmetric(Group):- true,into_grid(Group,Grid),!,h_symmetric(Grid).

is_object(O):- compound(O), O = obj(_).

%is_object_group([G|V]):- is_object(G),is_list(V),my_maplist(is_object,V).
%is_group(Dict):- is_vm_map(Dict),!,get_kov(objs,Dict,_).
is_group([G|V]):- is_object_group([G|V]),!. % is_object_or_grid(G),is_list(V),my_maplist(is_object_or_grid,V),!.
is_group(Grp):- is_rule_mapping(Grp),!.

:- ansi_term:import(is_group/1).


is_functor(F,E):- compound(E),functor(E,F,_).
is_functor(F,A,E):- compound(E),functor(E,F,A).
is_object_group(V):- is_list(V),my_maplist(is_functor(obj),V),!.
is_grid_group([G|V]):- is_grid(G),is_list(V),my_maplist(call(is_grid),V),!.

is_object_or_grid(Grid):- is_list(Grid),!,is_grid(Grid).
is_object_or_grid(Obj):- is_object(Obj).

is_pointy(O):- is_object_or_grid(O);is_group(O);is_points_list(O).

is_point_obj(O,Color,Point):- nonvar_or_ci(O),O= Color/**/-Point,!.
is_point_obj(O,Color,Point):- is_object(O),vis2D(O,1,1),
  globalpoints(O,[Color/**/-Point]),!.


%free_cell(0).
%free_cell(8).

%trim_unused_vert_square(BG,Grid,GridO).
%trim_unused_vert_square(BG,GridR,GridO):- append(Grid,[Row],GridR),my_maplist(is_bg_or_var(BG),Row),trim_unused_vert_square(BG,Grid,GridO).
%trim_unused_vert_square(_,G,G).*/

non_h_rot(sameR).
non_h_rot(rot90).
non_h_rot(rot270).

enum_rotation(sameR).
enum_rotation(rot90).
enum_rotation(rot180). % = rot180
enum_rotation(rot270).

non_h_ori(sameR).
non_h_ori(rot90).
non_h_ori(flipV).
non_h_ori(rot270).

non_diag_ori(sameR).
non_diag_ori(flipV).

non_v_ori(sameR).
non_v_ori(rot90).
non_v_ori(flipH).
non_v_ori(rot270).

enum_orientation(sameR).
enum_orientation(rot180). % = rot180
enum_orientation(rot90).
enum_orientation(rot270).
enum_orientation(flipH).
enum_orientation(flipV).
enum_orientation(rollD).
enum_orientation(flipD).

  
ap(scotch_patterns). ap(rug_patterns). ap(rougue_like). ap(space_invaders).
ap(shapes_on_black). ap(lines_on_black). ap(answer_keys). ap(repeating_codes).

ap(color_changes).
ap(holes).

ap(spins).  ap(contained). ap(sticky). ap(immobile). ap(mobile). ap(gravity).
ap(thick0). ap(thick1). ap(thick2). ap(thick3). 
ap(dashed).  ap(two_color). ap(multi_color). ap(monochrome).
ap(underneath). ap(painted_surface).
ap(movement_group). ap(controls_others).
ap(holds_dots).  ap(filler).  ap(blank). 

ap(changes).
ap(diagonal_line). ap(horizontal_line). ap(vertical_line). ap(open_edge). ap(container).  ap(ray).

ap(rotated45). ap(resizes). ap(diamond).
apv(square(len)). apv(round(h,w)). apv(triangle). apv(rectangular(h,w)). apv(polygon(sides)).
apv(shape_rep(grav,num)).  apv(facing(dir)). apv(min(n)). apv(max(n)).  apv(vis2D(h,w)). apv(loc2D(h,w)). 
apv(scale(n)).  apv(ext_key(k)). apv(io_bud(k)). apv(linked_bud(k)).

apv(points_old([])).
apv(sub_points([])).



color_and_rotation(Group,List):- override_group(color_and_rotation(Group,List)),!.
color_and_rotation(Hammer0,Hammer):-
  all_rotations(Hammer0,Hammer1),
     into_any_color(Hammer1,Hammer).

into_any_color(Group,List):- override_group(into_any_color(Group,List)),!.
into_any_color(RedHammer,Hammer):- change_color(RedHammer,Hammer).
into_any_color(RedHammer,RedHammer).

change_color_blue(Group,List):- change_color_to(blue,Group,List).

change_color_to(Blue,Group,List):- override_group(change_color_to(Blue,Group,List)),!.
change_color_to(Blue,RedHammer,BlueHammer):- 
  color(RedHammer,CurrentColor),
  swap_colors(Blue,CurrentColor,RedHammer,BlueHammer).


change_color(Group,List):- override_group(change_color(Group,List)),!.
change_color(RedHammer,Hammer):- 
   color(RedHammer,CurrentColor),
   fill_color(CurrentColor,OtherColor),
  swap_colors(CurrentColor,OtherColor,RedHammer,Hammer).

all_rotations(Group,List):- override_group(all_rotations(Group,List)),!.
all_rotations(RedHammer,Hammer):- 
 (var(RedHammer) -> freeze(RedHammer,all_rotations(RedHammer,Hammer)) 
   ; no_repeats(Grid,(shape_rotations(RedHammer,Hammer),object_grid(Hammer,Grid)))).

shape_rotations(Shape,Shape):- iz(Shape,hv_symmetric),!.
shape_rotations(Shape,Hammer):- iz(Shape,h_symmetric),!, non_h_rot(Rot),call(Rot,Shape,Hammer).
shape_rotations(RedHammer,Hammer):- enum_rotation(ROT), call(ROT,RedHammer,Hammer).


all_orientations(Group,List):- override_group(all_orientations(Group,List)),!.
all_orientations(RedHammer,Hammer):- 
 (var(RedHammer) -> freeze(RedHammer,all_orientations(RedHammer,Hammer)) 
   ; no_repeats(Grid,(shape_orientations(RedHammer,Hammer),object_grid(Hammer,Grid)))).

shape_orientations(Shape,Shape):- iz(Shape,leftover_as_one),!.
shape_orientations(Shape,Shape):- iz(Shape,hv_symmetric),!.
shape_orientations(Shape,Line):- iz(Shape,diag_symmetric),!, non_diag_ori(Rot),call(Rot,Shape,Line).
shape_orientations(Shape,Line):- iz(Shape,h_symmetric),!, non_h_ori(Rot),call(Rot,Shape,Line).
shape_orientations(Shape,Line):- iz(Shape,v_symmetric),!, non_v_ori(Rot),call(Rot,Shape,Line).
shape_orientations(RedHammer,Hammer):- enum_orientation(ROT), call(ROT,RedHammer,Hammer).


bg_to_fresh_vars(BGrid,Grid):- map_pred(bg_to_fresh_vars_e,BGrid,Grid).
bg_to_fresh_vars_e(X,Y):- bg_sym(BG), X==BG, Y= _.

use_growth_chart(Pixels,GC,NewPixels):-  globalpoints(GC,GP), do_gp(GP,Pixels,NewPixels).
 
  
%learned_color_inner_shape(Shape,Color,Color,P,GC),globalpoints(GC,GP),use_growth_chart(P,GC,PO).


%scale_grid(Scale,GrowthChart,Grid,ScaledGrid)
scale_grid(1,_GrowthChart,Grid,Grid).


enum_scale(1).

:- include(kaggle_arc_footer).

