
p1_found_box(L_S,NSEW,OH,OV,C,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   inside, insideSolid(C)):-  fail,
  pbox_phase_check(L_S,s_l(1)),
   (H>1,V>1), maplist_ls(==(C),Inside), \+ maplist_ls(==(C),OBorder), maplist_ls(=(B),OBorder).

p1_found_box(L_S,NSEW,OH,OV,C,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   center, unRibs(L_S,C,H,V)):- 
   H>3,V>3, pbox_phase_check(L_S,s_l(1)),  C\==black,  not_whole_row_or_col(C,Center), \+ ((member_ls(Row,Center),append(_,[C,C,C|_],Row))).

p1_found_box(L_S,NSEW,OH,OV,C,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   center, ring(L_S,C,H,V)):- H>4,V>4, fail,
   \+ \+ (member_ls(NB,Center), NB \=C), not_whole_row_or_col(C,Center), C==black.

p1_found_box(L_S,NSEW,OH,OV,C,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   center, innerThang(L_S,C,H,V)):- 
   maplist_ls(=(C),OBorder),!, C\==black, pbox_phase_check(L_S,l_s(2)), \+ member_ls(C,Center).


p1_found_box(L_S,NSEW,OH,OV,C,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   center, innerThang(C)):- 
   maplist_ls(=(C),OBorder),!, C\==black, \+ member_ls(C,Center).

p1_found_box(L_S,NSEW,OH,OV,C,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   Ans, midThang(L_S,C,H,V)):- 
   pbox_phase_check(L_S,l_s(2)),   
   \+ maplist_ls(=(C),OBorder), 
   not_whole_row_or_col(C,Center),
   member_ls(C,Center),
   \+ is_black(C),  
   (is_black(C)->  Ans=center ; (Ans=inside, not_edge(C,Find))).


p1_found_box(L_S,NSEW,OH,OV,C,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   inside, midThang1(C)):- 
  (pbox_phase_check(L_S,s_l(2)), \+ \+ ([B,B,B,B]=NSEW, maplist_ls(=(C),OBorder), \+ maplist_ls(==(C),IBorder), 
    nop( \+ (member_ls(Row,Inside), maplist_ls(=(C),Row) )))).          

p1_found_box(L_S,NSEW,OH,OV,C,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   inside, midThang2(L_S,C,H,V)):- 
        %  fail,
  (pbox_phase_check(L_S,l_s(2)), \+ \+ ([n,s,e,w]=NSEW, different_enough(L_S,Inside,Find,Inside,OBorder))).

p1_found_box(L_S,NSEW,OH,OV,C,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   inside, outerThing(BC)):-
  pbox_phase_check(L_S,l_s(2)),
  is_black(C), 
  maplist_ls(=(BC),OBorder), BC\==C,!.

p1_found_box(L_S,NSEW,OH,OV,C,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   inside, outerThing2n(L_S,BC,C,H,V)):- 
  pbox_phase_check(L_S,l_s(2)),
  maplist_ls(=(BC),OBorder), BC\==C,!.
  

p1_found_box(L_S,NSEW,OH,OV,C,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   center, twang):- fail,

    \+ \+ if_t((maplist_ls(==(C),Inside), \+ maplist_ls(=(C),OBorder)),
       \+ \+ if_t(not_edge(C,Find), if_t(\+ append(_,[C,C,C|_],OBorder), nop(print_grid(ogs_in0(L_S,C,OH,OV,H,V,OBorder),Inside))))),
    %if_t( \+ mass(Inside,0),print_grid(ogs_in(L_S,C,OH,OV,H,V),Inside)),

    % ( \+ member_ls(C,OBorder) ; C==black),
/*
    (L_S==l_s(2) ->  \+member_ls(C,OBorder) ; ( \+member_ls(C,OBorder) ; C==black)),
      if_t(L_S==l_s(2),(
       \+ (member_ls(Row,Center), maplist_ls(=(C),Row)),

       \+ (Center\==[], Center\==[[]],rot90(Center,Cols),member_ls(Col,Cols), maplist_ls(=(C),Col)) )),
*/

(
    if_t( \+ \+ (is_black(C),not_whole_row_or_col(C,Center)),
      if_t(Center\=[[_]],print_grid(inside1(L_S,C,OH,OV,H,V),Center))),
 

    (  ( fail, \+ member_ls(C, Center)) -> print_grid(inside(L_S,C,OH,OV,H,V),Center);
         \+ \+  if_t(Center\==Inside, 
               \+ \+  if_t(not_whole_row_or_col(C,Inside),print_grid(found1(L_S,C,OH,OV,H,V),Inside)))),
   \+ \+  if_t(( \+ member_ls(C,OBorder), \+ maplist_ls(=(Black),OBorder)),
       ((print_grid(inGood(L_S,C,OH,OV,H,V),Inside),nop((print_grid(found2(L_S,C,OH,OV,H,V),Find))))))).
    %if_t(maplistmaplist(==(C),Inside),print_grid(find1(L_S,C,OH,OV,H,V),Find)),
    %maplist_ls(=(C),Center,Center) -> make_obj(Inside,solid_rect);
    %-> make_obj(Inside,solid_rect);


  %  (maplist_ls(=(C),Inside), \+ member_ls(C,OBorder)) -> ObjG=Inside ;
  %  (maplist_ls(=(C),Inside), \+ member_ls(C,OBorder))

is_compass(A):- atom(A),member_ls(A,[n,s,e,w]).


found_box_s(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  maplist_ls(=(C),Inside), Center=[_|_], nonvar(C),
  \+ flatten_set_bf(IBorder,[C]),
  \+ member_ls(C,OBorder), OBJ=center,
  WHY=solidCenter(Center,C).

found_box_s(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  maplist_ls(=(C),Inside), nonvar(C),
  \+ member_ls(C,OBorder), OBJ=inside,
  WHY=solidCenterIn(Center,C).

found_box_s(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  IBorder=[N,S,E,W],
  member1(C,N),member1(C,S),member1(C,E),member1(C,W),
  \+ member_ls(C,Center),
  \+ member_ls(C,OBorder),
  %restOfGrid(XSG,OH,OV,H,V,Rest), \+ member_ls(C,Rest).
  OBJ=inside,
  WHY=fourDots(Center,C).


found_box_s(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  Inside==[_,_|_],
  OBorder==[_,_|_],
  \+ member_ls(black,Inside), 
  member_ls(black,OBorder), 
  OBJ=center,
  WHY=solidCenter1(Inside,OBorder).


found_box_s(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  maplist_ls(=(C),OBorder),
  %\+ maplist_ls(=(I),IBorder),
  %if_t(maplist_ls(=(B),OBorder),(C\==B;C==black)),
  if_t(nonvar(C),Inside\==[C]),
  Inside=[_|_],
  if_t(nonvar(C),not_whole_row_or_col(C,Inside)),
  %(),
  %if_t(C\==black, \+ member_ls(C,Center)),
  %nonvar(C),Center\==[C],Center=[_|_],
  OBJ=inside,
  WHY=solidInnerBorderI_O(Center,C).


found_box_s(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  maplist_ls(=(C),IBorder),
  C\==black,
  %if_t(maplist_ls(=(B),OBorder),(C\==B;C==black)),
  if_t(nonvar(C),Center\==[C]),
  Center=[_|_],
  if_t(nonvar(C),not_whole_row_or_col(C,Center)),
  %(),
  %if_t(C\==black, \+ member_ls(C,Center)),
  %nonvar(C),Center\==[C],Center=[_|_],
  %(C==black -> OBJ=inside; OBJ=inside),
  %OBJ=center,
  \+ member_ls(Inside,Black),
  OBJ=inside,
  WHY=solidInnerBorder0(Center,C).





found_box_s(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  fail,
  Inside=[C],C\==black,
  flatten_set_bf(OBorder,[_,_|_]),
  \+ member_ls(black,OBorder),
  \+ member_ls(C,NSEW),
  OBJ=inside,
  WHY=dashedOuterBorder(OBorder,C).



found_box_s(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,  OBJ,WHY):- fail,
  pbox_phase_check(L_S,l_s(2)),
  maplist_ls(=(C),IBorder),
  if_t(maplist_ls(=(B),OBorder),(C\==B;C==black)),
  if_t(nonvar(C),Center\==[C]),
  Center=[_|_],
  if_t(C\==black,not_whole_row_or_col(C,Center)),
  %(),
  %if_t(C\==black, \+ member_ls(C,Center)),
  %nonvar(C),Center\==[C],Center=[_|_],
  OBJ=center, WHY=solidInnerBorder1(Center,C).


found_box_s(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,  OBJ,WHY):- fail,
  V>3,H>3,
  pbox_phase_check(L_S,l_s(1)),
  black_and(IBorder,C),maplist_ls(\==(C),OBorder),
  (black_and(Center,C); \+ member_ls(C,Center)),
  OBJ=center,!,WHY=solidInnerBorder2(Center).

found_box_s(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,  OBJ,WHY):- fail,
  pbox_phase_check(L_S,l_s(1)),
  Inside=[C], \+ maplist_ls(\==(C),OBorder),
  OBJ=inside,!,WHY=solidIn(C).

found_box_s(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,  OBJ,WHY):-
  pbox_phase_check(L_S,l_s(1)),
  flatten_set_bf(IBorder,[C]),maplist_ls(\==(C),Inside),
  OBJ=center,!,WHY=solidInnerBorder2(C).

found_box(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   inside, solidInIBorder):- 
  member_ls(C,Inside),is_real_color(C),maplist_ls(==(C),Inside), \+ maplist_ls(==(C),OBorder),!.

found_box(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   _, _):- fail,
  member_ls(C,Inside),is_real_color(C),maplist_ls(==(C),Inside), maplist_ls(=(C),OBorder),!,fail.

found_box(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   OBJ,WHY):- 
 % pbox_phase_check(L_S,l_s(2)),
  H>1,V>1,
  found_box_s(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,  OBJ,WHY),
  nop((MOBJ\==Inside -> MOBJ=OBJ ; ( (maplist_ls(=(C),IBorder), nop((\+ member_ls(C,Center))) ) -> OBJ=center ; OBJ=MOBJ))).


found_box(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   center, 
                       dottedBoxCenter(border1(C1,C2),NSEW,L_S,H,V)):- H>3,V>3, 
  pbox_phase_check(L_S,s_l(1)),
    ((my_partition(=(C),IBorder,C1s,C2s),length(C1s,N1),length(C2s,N2),max_min(N1,N2,Max,Min),Min==4)),!.
%    \+ member_ls(C,OBorder), \+ member_ls(C,Center))).


found_box(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   inside, largeInBlack(NSEW,L_S,H,V)):-  
  pbox_phase_check(L_S,l_s(2)),
  CNSEW=[B,B,B,B], B=black,
   \+ \+ ((NSEW=CNSEW,
         maplist_ls(=(B),OBorder), (member_ls(C,IBorder), \+ is_black(C)))),!, not_whole_row_or_col(C,Inside).


found_box(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   inside, find_pbox(L_S,C1-C2=C3-C4)):-  
  pbox_phase_check(L_S,l_s(2)),
  H>3,V>3,
  once((
  %list_to_set(OBorder, OBorder),
  pred_intersection(==,FindS,OBorder,Common,_,FindU,OBorderU))),
  [Common,FindU,OBorderU]=All,
  ground(All),
  [Common,FindU,OBorderU]=[[_],[_],[_]].


found_box(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   inside, find_ogs(L_S,C,NN,OBorderC)):- 
  pbox_phase_check(L_S, l_s(1)),
  H>3,V>3,
  maplist_ls(=(C),Inside),
  get_edges(Inside,Top,Bottem,Left,Right),
  maplist(trim_ends,[Top,Bottem,Left,Right],TrimmedRows),
  findall(E,(member_ls(E,TrimmedRows),maplist_ls(=(C),E)),Borders),
  length(Borders,NN),
  \+ maplist_ls(==(black),Inside),
  list_to_set(OBorder,OBorderC).

/*

found_box(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   center, boxCenter(NSEW,L_S,H,V)):-
  pbox_phase_check(L_S,l_s(2)),
   \+ \+ ((maplist_ls(=(C),IBorder), \+ member_ls(C,OBorder), \+ member_ls(C,Center))).

*/
found_box(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   OBJ, WHY):-
  maplist_ls(=(C),IBorder),!,p1_found_box(L_S,NSEW,OH,OV,C,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   OBJ, WHY).

found_box(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   inside, different_enough(L_S,CNSEW)):-  
  [n,s,e,w]=CNSEW,
 % !, fail,    
  \+ \+ ((CNSEW=NSEW,different_enough(L_S,Inside,Find,Inside,OBorder))),!,
  if_t((member_ls(C,OBorder),nonvar(C)),
       not_whole_row_or_col(C,Inside)).

/*
found_box(L_S,NSEW,OH,OV,Srch,XSG,H,V,Center,Inside,Find,IBorder,OBorder,   inside, different_enough(NSEW)):- 
  %[n,s,e,w]=NSEW,
  !, fail,
  different_enough(L_S,Inside,Find,Inside,OBorder).
*/


different_enough(L_S,Inside,Find,Inside,OBorder):-
  a_portion_colors(Inside,InsideC,InsideV,InsideBG,InsideFG),
  a_portion_colors(OBorder, BorderC,BorderV,BorderBG,BorderFG),
  maplist(sort_safe,[BorderC,BorderV,BorderBG,BorderFG,InsideC,InsideV,InsideBG,InsideFG],
               [SBorderC,SBorderV,SBorderBG,SBorderFG,SInsideC,SInsideV,SInsideBG,SInsideFG]),
  different_enough_c(Inside,Find,L_S,SBorderC,SBorderV,SBorderBG,SBorderFG,
                                               SInsideC,SInsideV,SInsideBG,SInsideFG),!.

a_portion_colors(IA,Compass,Vars,BG,FG):- 
  my_partition(is_compass,IA,Compass,Colors),
  my_partition(is_bg_color,Colors,BG,VFG),my_partition(is_fg_color,VFG,FG,Vars).


  %\+ not_different_enough_ii(BorderV,BorderBG,BorderFG,InsideV,InsideBG,InsideFG).

%        different_enough_c(_I,_F,BorderV,BorderBG,BorderFG,   insideV,InsideBG,InsideFG):-!.

has_blank_rows(I,N):- findall(Row,(append([_|_],[Row,_|_],I), maplist(cmatch(is_bg_color),Row)),N).
%has_blank_rows(I,N):- findall(Row,(member(Row,I), maplist(cmatch(is_bg_color),Row)),N).
has_blank_cols(C,N):- rot90(C,I),has_blank_rows(I,N).

different_enough_c(_I,_F,l_s(2),   _, _,           [_],        [],    
                               _, _,            [],    [_,_|_]):- !, % 0b148d64
  true.
/*
different_enough_c(I,_F,l_s(2),  [N,W], _,        [_],        [],    
                               _,   [],        [_],       [_]):- w_in_90(N,W),!, % 0b148d64
  \+ has_blank_rows_or_cols(I).
*/


different_enough_c(_I,_F,_,  _,_,      [_],      [],    
                              _,_,     [_],      []):- !,fail.

different_enough_c(I,_F,l_s(2),   _, _,           [_],        [],    
                               _,   [],        _,       [_]):- !, % 0b148d64
  max_blanks(I,1).




different_enough_c(_I,_F,l_s(2),  _, _,            [],        [FG],    
                              _, [],          _ ,         [H|T]):- \+ member(FG,[H|T]).  % 09629e4f

different_enough_c(I,_F,_S_l,  [],    _,        [_],        [],    
                                _,   [],        [_],       [_]):- !, % 05f2a901
                                 max_blanks(I,1).

different_enough_c(_I,_F,l_s(2),  _,    _,         [],        [_],    
                                _,   [],        [_],       []):- !, % 06df4c85
   true.

/*
*/

different_enough_c(I,_F,l_s(2),   _, _,           [_],        [],    
                               _,   [],        _,       NN):- NN\==[], !, % 0b148d64
                                  length(NN,Max),
                                  max_blanks(I,Max).


/*

different_enough_c(_I,_F,_,  _,_,       _,       [],    
                                 _,_,       _,       []):- !,fail.


different_enough_c(_I,_F,_,   _,_,        [],          [],    _,_,       _,     _):-!,fail.
different_enough_c(_I,_F,_,   _,_,        [],          [SC],      _,_,         [],    InsideFG):- InsideFG==[SC],!,fail.
different_enough_c(_I,_F,_,   _,_,        [_],         [],    _,InsideV,       _,     InsideFG):-  InsideV\==[];InsideFG==[_]. 

%different_enough_c(_I,_F,_,_,        [_],        [],    _,[],       [_],     [_]). 
different_enough_c(_I,_F,_,   [_,_,_],_,        [_],        [],    _,[],       [_],     [_,_|_]). % 0b148d64
different_enough_c(_I,_F,_,   _,_,        _,         [_],    _,[],       [_],     []).
*/
/*
%different_enough_c(_I,_F, _,        [],          [_],      _,          _,        _).
%different_enough_ii(BorderV,BorderBG,BorderFG,InsideV,InsideBG,InsideFG).
%not_different_enough_ii(_BorderV,_BorderBG,_BorderFG,_InsideV,_InsideBG,_InsideFG):- fail.
*/
max_blanks(I,Max):-
  \+ (has_blank_rows(I,N), length(N,L), L>=Max), 
  \+ (has_blank_cols(I,N), length(N,L), L>=Max).

