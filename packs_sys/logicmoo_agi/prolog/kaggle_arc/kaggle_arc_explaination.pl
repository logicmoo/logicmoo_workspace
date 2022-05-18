


:- dynamic(is_gridname/2).

set_gridname(Grid,Name):- nb_setval(grid_name,Name),asserta_new(is_gridname(Grid,Name)).

get_gridname(Grid,Name):- is_gridname(Grid,Name)*->true; nb_current(grid_name,Name).
get_gridname(In,Name*ExampleNum*in):- kaggle_arc(Name,ExampleNum,In,_).
get_gridname(Out,Name*ExampleNum*out):- kaggle_arc(Name,ExampleNum,_,Out).


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

get_selector1(obj(PI),obj(PO)):-
  get_selector_n1(PIN),do_nth(PIN,PI,PO).

do_nth([],_,_):-!.
do_nth([H|T],I,O):-!, do_nth(H,I,O),do_nth(T,I,O).
do_nth(N,I,O):- nth1(N,I,E),nth1(N,O,E).


get_selector_n1([N1,N2,N3,N4],[]):- Top=6,
  between(1,Top,N1), between(N1,Top,N2),N2>N1, between(N2,Top,N3),N3>N2, between(N3,Top,N4),N4>N3.
get_selector_n1([1,2,3]). 
get_selector_n1([2,3]). % Colors Dif
get_selector_n1([1,2]). % Offset Dif
get_selector_n1([1,3]). % Size Dif
get_selector_n1([3]). %Offset
get_selector_n1([2]). %Size
get_selector_n1([1]). %Colors

get_selector(PI,PO):- get_selector2(PI,PO).
/*
get_selector(PI,PO):- get_selector1(PI,PO).
get_selector(PI,PO):- get_selector2(PI,PO), \+ (get_selector1(PII,POO), PI=@=PII,PO=@=POO).
*/
get_selector(_,_).
get_selector2(obj(PI),obj(PO)):- 
  get_selector_n2(PIN),do_nth(PIN,PI,PO).


get_selector_n2([N1,N2,N3,N4]):- top(Top),
  between(1,Top,N1), between(N1,Top,N2),N2>N1, between(N2,Top,N3),N3>N2,between(N3,Top,N4),N4>N3.
get_selector_n2([N1,N2,N3]):- top(Top),
  between(1,Top,N1), between(N1,Top,N2),N2>N1, between(N2,Top,N3),N3>N2.
get_selector_n2([N1,N2]):- top(Top),
  between(1,Top,N1), between(N1,Top,N2),N2>N1.
get_selector_n2([N1]):- top(Top), between(4,Top,N1).


combine_diffs([],D,D):-!.
combine_diffs(D,[],D):-!.
combine_diffs(D,D,D):-!.
combine_diffs(D1,D2,L12):- listify(D1,L1),listify(D2,L2),!,append(L1,L2,L12).


%compute_diff(IPs,OPs,Difs2):- compute_diff(IPs,OPs,Difs2).
number_dif(I,O,0):- I =:= O.
number_dif(I,O,-D):- I<O, D is O -I.
number_dif(I,O,+D):- D is I -O.

usefull_compare(P):- compound(P),functor(P,F,_),!,usefull_compare(F).
usefull_compare(P):- changed_by(P,_).

changed_by(points_only,rotate).
changed_by(object_offset,move).
changed_by(point_count,grow).
changed_by(colors_count,repaint).
changed_by(object_size,copy).
changed_by(object_shape,rotate).

uncomparable(grid_size).
uncomparable(object_indv_id).

make_comparable(I,I):- var(I).
make_comparable(I,I):- \+ compound(I),!.
make_comparable(I,II):- is_list(I),!,maplist(make_comparable,I,II).
make_comparable(obj(L),obj(LL)):- !,make_comparable(L,LL).
make_comparable(L,L):- usefull_compare(L),!.
make_comparable(I,II):- functor(I,II,_).

dif_lop([],O,left_extra(O)):-!.
dif_lop(O,[],left_over(O)):-!.
dif_lop(I0,O0,DD):- 
  make_comparable(I0,I),
  make_comparable(O0,O),
  get_selector(PI,PO),
  select(PI,I,II),
  select(PO,O,OO),
  compute_diff(PI,PO,D1),
  dif_lop(II,OO,D),
  combine_diffs(D1,D,DD).

include_fav_points(I,II):- include(fav_points,I,II),I=[_,_|_],!.
include_fav_points(I,I).

fav_points(I):- \+ dislike_points(I).

dislike_points(obj(I)):-!,dislike_points(I).
dislike_points(I):- is_list(I),dislike_points1(L),forall(member(E,L),member(E,I)).

%dislike_points1([object_shape(dot),grid_size(H,V)]):- freeze(H, freeze(V, (HV is H * V, HV > 49))).
dislike_points1([colors_count([color_count(BG, _)]),object_shape(nonsolid)]):- freeze(BG,is_black_or_bg(BG)).

no_diff(in,out).
simular([],_,_,[]):- !.
simular(object_offset=Where,I,O,object_has_moved(Where)):- 
  \+ (point_count(O,OC), OC < 6) ,
  \+ (colors_count(O,[color_count(BG, _)|_]),is_black_or_bg(BG)),
  object_indv_id(I,Tst,_Id1), \+ object_indv_id(O,Tst,_Id2).

compute_diff_objs(obj(I),II,obj(O),OO,OUT):- append(L,[A|R],II),append(L,[B|R],OO),
  compute_diff(A,B,Diffs),
    (simular(Diffs,obj(I),obj(O),Sameness) ->  OUT = same_object(Sameness) ; OUT = props_diff(obj(I),obj(O),Diffs)).

compute_diff(I,O,[]):- (var(I);var(O)),!.
compute_diff(obj(I),obj(O),OUT):- !,
  make_comparable(I,II),
  make_comparable(O,OO),
  compute_diff_objs(obj(I),II,obj(O),OO,OUT).


compute_diff(I,O,DD):- is_objectlist(I),is_objectlist(O),!,
  include_fav_points(I,II),
  include_fav_points(O,OO),
  dif_lop(II,OO,DD).


compute_diff(I,O,D):- number(I),number(O),number_dif(I,O,D).
compute_diff(I,O,[]):- O=@=I,!.
compute_diff(I,O,[]):- no_diff(I,O).
compute_diff(O,I,[]):- no_diff(I,O).
compute_diff(I,O,O):- I==[],!.
compute_diff(I,O,I):- O==[],!.
compute_diff([IH,IV],[OH,OV],D):- maplist(number,[IH,IV,OH,OV]),!,maplist(number_dif,[IH,IV],[OH,OV],D).
compute_diff(I,O,D):- is_list(I),!,is_list(O),sort(I,IS),sort(O,OS),!,list_diff(IS,OS,D).
compute_diff(I,O,D):- is_dict(I),!,findall(D1,(get_dict(K, I, V),compute_diff(K=V,O,D1)),D).
compute_diff(IF=IA,O,IF=D):-!, find_kv(O,IF,OA),!,compute_diff(IA,OA,D).
compute_diff(I,O,D):- compound(I),compound(O),compound_name_arguments(I,IF,IA),compound_name_arguments(O,_OF,OA),!,
  compute_diff(IF=IA,IF=OA,D).
compute_diff(I,O,I->O).

list_diff([],O,O):-!.
list_diff(O,[],O):-!.
list_diff(I,O,[]):- I==O,!.
list_diff(I,O,[]):- I=@=O,!.
list_diff(I,O,D):- select(C-E,I,II),select(C-E,O,OO),!,list_diff(II,OO,D).
list_diff(I,O,D1D):- select(C-E,I,II),select(C2-E,O,OO),!,list_diff(II,OO,D),combine_diffs(change_color(C->C2),D,D1D).
list_diff(I,O,D1D):- select(C-E,I,II),select(C-E2,O,OO),!,list_diff(II,OO,D),combine_diffs(change_point(E->E2),D,D1D).
list_diff(I,O,D):- select(CE,I,II),select(CE,O,OO),!,list_diff(II,OO,D).
list_diff(I,O,D1D):- select(CI,I,II),callable(CI),functor(CI,F,A),functor(CO,F,A),select(CO,O,OO),!,compute_diff(CI,CO,D1),list_diff(II,OO,D),combine_diffs(D1,D,D1D).
list_diff([CE|I],[CE2|O],D1D):- compute_diff(CE,CE2,D1),!, list_diff(I,O,D),combine_diffs(D1,D,D1D).


find_kv(OF=OA,OF,OA):- !.
find_kv(List,OF,OA):- is_list(List),member(E,List),nonvar(E),find_kv(E,OF,OA).
find_kv(Dict,OF,OA):- is_dict(Dict),get_dict(OF,Dict,OA).
find_kv(O,OF,OA):- compound(O),compound_name_arguments(O,OF,[OA]).
find_kv(obj(O),OF,OA):- !, find_kv(O,OF,OA).
find_kv(O,OF,OA):- compound(O),compound_name_arguments(O,OF,OA).


must_intersect_all(Indv,Points,NextScanPoints):-
   globalpointlist(Indv,IndvPoints),
   unique_of_each(IndvPoints,Points,[],NextScanPoints),!.

unique_of_each(IndvPoints,Points,UniqueInvO,UniquePointsO):-
  remove_cpoints(Points,IndvPoints,UniquePoints),
  remove_cpoints(IndvPoints,Points,UniqueInv),!,
  UniquePoints=UniquePointsO,
  UniqueInv=UniqueInvO.

remove_cpoints(UniqueInv,[],UniqueInv).
remove_cpoints(IndvPoints,[C-Point|Points],UniqueInv):-
   select(C2-Point,IndvPoints,NextIndvPoints),
   C2==C,!,
   remove_cpoints(NextIndvPoints,Points,UniqueInv).



count_difs(A,B,C):- into_grid(A,AA), into_grid(B,BB),!,count_difs0(AA,BB,C).
count_difs0(Out,GridO,0):- Out=@=GridO,!.
count_difs0(Out,GridO,1):- ((\+ compound(Out)) ; \+ compound(GridO)),!.
count_difs0([A|Out],[B|GridO],Errors):- !,
      count_difs0(A,B,Errors1),
      count_difs0(Out,GridO,Errors2),
      Errors is Errors1 + Errors2.
count_difs0(Out,GridO,Errors):-
  compound_name_arguments(Out,F,A),
  compound_name_arguments(GridO,FO,AO),
  count_difs0([F|A],[FO|AO],Errors).
  

describe_feature(Grid,List):- is_list(List),!,maplist(describe_feature(Grid),List).
describe_feature(Grid,Pred):- call(Pred,Grid,Res)->print_equals(Grid,Pred,Res);print_equals(Pred,f),!.


/*
print_points_grid(Points):- 
  points_range(Points,LoH,LoV,HiH,HiV,H,V),
  wqnl(offset_ranges(LoH,LoV,HiH,HiV,H,V)),
  points_to_grid(Points,Grid),
  print_grid(Grid).
print_points_grid(Grid):-  
  points_range(Grid,LoH,LoV,HiH,HiV,_H,_V),
  print_grid(Grid,LoH,LoV,HiH,HiV,Grid).
*/



%object_size(Points,H,V):- is_dict(Points),!,Points.object_size=object_size(H,V).



combine_grids(_,[G],G):-!.
combine_grids(How,[G1,G2|Gs],GO):- combine_grids(How,[G2|Gs],G1,GO).

combine_grids(_How,[],G,G):-!.
combine_grids(How,[H|T],G,GO):- !,
  %in_cmt((writeln(How),print_grid(H))),
  combine_grids(How,H,G,GM),
  combine_grids(How,T,GM,GO).
combine_grids(overlay,H,G,GM):- globalpointlist(H,Points),set_point(Points,G,GM),!.
combine_grids(append,H,G,GM):- grid_size(H,W,_),length(Row,W), append(G,[Row|H],GM).
  
debug_indiv:- test_config(nodebug_indiv),!,fail.
debug_indiv:- test_config(debug_indiv),!.
debug_indiv:- test_config(indiv(_)),!.

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

debug_indiv(Obj):- Obj = obj(A), is_list(A),
  object_indv_id(Obj,_,Id),
  ignore(colors_count(Obj,[color_count(FC,_)|_])),
  remove_too_verbose(A,AA), 
  flatten([AA],F),
  sort(F,AAA),
  include('\\=='(''),AAA,[Caps|AAAA]),
  toPropercase(Caps,PC), 
  %i_glyph(Id,Sym), wqnl([writef("%% %Nr%w \t",[PC]), color_print(FC,Sym) | AAAA ]),!. 
  i_glyph(Id,Sym), wqnl([format("%  ~w:\t",[PC]), color_print(FC,Sym) | AAAA ]),!. 

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

remove_too_verbose(Var,var(Var)):- var(Var),!.
remove_too_verbose(H,''):- too_verbose(H),!.
remove_too_verbose(dot,"point"):- !.
remove_too_verbose(line(HV),S):- sformat(S,'~w-Line',[HV]).
remove_too_verbose(square,S):- sformat(S,'square',[]).
remove_too_verbose(background,S):- sformat(S,'bckgrnd',[]).
remove_too_verbose(object_shape(H),HH):- !, remove_too_verbose(H,HH).
remove_too_verbose(colors_count(H),HH):- !, remove_too_verbose(H,HH).
remove_too_verbose(object_indv_id(_ * X,Y),[layer(XX),nth(Y)]):- upcase_atom(X,XX).
remove_too_verbose(object_offset(X,Y),offset(X,Y)).
remove_too_verbose(object_size(X,Y),size(X,Y)).
remove_too_verbose(point_count(X),pixels(X)).
remove_too_verbose(L,LL):- is_list(L),!, maplist(remove_too_verbose,L,LL).
remove_too_verbose(H,H).
too_verbose(P):- compound(P),compound_name_arity(P,F,_),!,too_verbose(F).
too_verbose(globalpointlist).
too_verbose(localcolorlesspointlist).
too_verbose(localpointlist).
too_verbose(grid).
too_verbose(grid_size).

debug_indiv(Obj,P):- compound_name_arguments(P,F,A),debug_indiv(Obj,P,F,A).
debug_indiv(_,_,X,_):-too_verbose(X),!.
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


