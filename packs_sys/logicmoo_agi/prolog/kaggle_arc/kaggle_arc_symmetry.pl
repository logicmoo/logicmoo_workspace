
:- use_module(library(lists)).

is_symgrid(t('3631a71a')*(trn+_)*out).
is_symgrid(v(de493100)*(trn+_)*in).
/*
*/
repair_symmetry:- clsmake, repair_symmetry0.
repair_symmetry0:- 
 forall(
 (is_symgrid(Symgrid),
  known_gridoid(Symgrid,Grid),
  print_grid(Grid)),
  ignore((
   wdmsg(repair_symmetry),
   repair_symmetry(Grid,GridO),
   print_side_by_side(Grid,GridO)))).


/*
4-Way Symmetry

  
|SXQ2  |SXCC  |SXQ4  
       |      |      
   EXQ2|  EXCC|  EXQ4|
|--------------------| 
|      |      |      | SYQ2 
|  Q2  |  CN  | Q1   | 
|      |      |      | EYQ2
|------+------+------| 
|      |      |      | SYCC
|  CW  |  CC  | CE   | 
|      |      |      | EYCC
|------+------+------| 
|      |      |      | SYQ4
|  Q3  |  CS  | Q4   |
|      |      |      | EYQ4
|--------------------| 
*/


symetric_xy(SXQ2,SYQ2,EXQ2,EYQ2,SXCC,SYCC,EXCC,EYCC,SXQ4,SYQ4,EXQ4,EYQ4,G):-
   grid_size(G,EXQ4,EYQ4),
   SXQ2=SYQ2,SXQ2=1,
   symmetric_hv(EXQ2,CX,EYQ2,CY,EXQ4,EYQ4,G),
   %sort_row_by_num_colors(G,Rows),
   %print_grid(Rows),
   %wqnl(cx=CX+EXQ2),print_grid(EXQ2,EYQ2,G),wqnl(cy=CY+EYQ2),
   check_symetric_xy(CX,CY,SXQ2,SYQ2,EXQ2,EYQ2,SXCC,SYCC,EXCC,EYCC,SXQ4,SYQ4,EXQ4,EYQ4).

check_symetric_xy([],[],1,1,EXQ2,EYQ2,0,0,0,0,SXQ4,SYQ4,EXQ4,EYQ4):-
     SXQ4 is EXQ2+1,
     SYQ4 is EYQ2+1,!,
     nop((EXQ4,EYQ4)).


check_symetric_xy(CX,CY,SXQ2,_SYQ2,EXQ2,EYQ2,SXCC,SYCC,EXCC,EYCC,SXQ4,SYQ4,EXQ4,_EYQ4):- 
   
   length(CX,LCLX),SXCC is EXQ2 + 1,EXCC is LCLX + EXQ2 -1,SXQ4 is LCLX + EXQ2,
   maplist(assertion,[SXQ2=<EXQ2,EXQ2=<SXCC,EXQ2=<SXCC,EXCC=<SXQ4,SXQ4=<EXQ4]),
   length(CY,LCLY),SYCC is EYQ2 + 1,EYCC is LCLY + EYQ2 -1,SYQ4 is LCLY + EYQ2,
   !.


crop(X,Y,G,GO):- make_grid(X,Y,GO),maplist_until(aligned_rows,G,GO).


maplist_until(Pred2,[X|XX],[Y|YY]):- call(Pred2,X,Y),maplist_until0(Pred2,XX,YY),!.
%maplist_until(Pred2,[_|XX],[_|YY]):- maplist_until(Pred2,XX,YY),!.
maplist_until(_,[],[]).

maplist_until0(Pred2,[X|XX],[Y|YY]):- call(Pred2,X,Y)->maplist_until0(Pred2,XX,YY).
maplist_until0(_,_,[]).

maplist_until(Pred2,[X|XX]):- call(Pred2,X)->maplist_until(Pred2,XX).
maplist_until(_,_).


empty_or_open(L):- \+ \+ L=[].

first_quarter(Grid,GridQ):- first_half(Grid,GL),first_half(GL,GridQ).
first_half(Grid,GL):- length(Grid,L),H is floor(L/2), length(GL,H), append(GL,_,Grid).
second_half(Grid,GL):- length(Grid,L),H is floor(L/2), length(GL,H), append(_,GL,Grid).

aligned_rows([E1,E2|L],[E1,E2|R]):- aligned_rows0(L,R).

aligned_rows0([],_):-!. % ( empty_or_open(L) ; empty_or_open(R) ), !.
aligned_rows0(_,[]):-!.
aligned_rows0([E|L],[E|R]):- !, aligned_rows0(L,R).

no_symmetry_yet(Row):- maplist(var,Row),!. % no data yet
no_symmetry_yet(Row):- maplist(=(_),Row),!. % all same element

sort_row_by_num_colors(G,G0):-
   maplist(row_color_changes,G,C),keysort(C,KS),reverse(KS,Now),
   maplist(arg(2),Now,G0).

row_color_changes(List,0-List):- (var(List);List==[]),!.
row_color_changes([H|List],C-[H|List]):- row_color_changes(0,H,List,R), C is floor(R/2).

row_color_changes(Res,_,[],Res):-!.
row_color_changes(PrevCh,H,[N|List],Res):- var(N),!,row_color_changes(PrevCh,H,List,Res).
row_color_changes(PrevCh,H,[N|List],Res):- H\==N, row_color_changes(PrevCh+1,N,List,Res).
row_color_changes(PrevCh,H,[N|List],Res):- H==N,  row_color_changes(PrevCh,N,List,Res).

symetric_row(I,C,Row,L):- append(C,[E1|R],Right), append(L,Right,Row),reverse(L,[E1|LL]),aligned_rows(LL,R),length(L,I).

symmetric_lrw(I,C,[],GL):- !.
symmetric_lrw(I,C,G,GL):- maplist_until(symetric_row(I,C),G,GL),
   reverse(G,GR), maplist_until0(symetric_row(I,C),GR,_).

symmetric_lr(I,C,Grid,GL):- first_half(G,G0),symmetric_lr0(I,C,Grid,GL).
symmetric_lr(I,C,Grid,GL):- second_half(G,G0),symmetric_lr0(I,C,Grid,GL).
%symmetric_lr(I,C,Grid,GL):- symmetric_lr0(I,C,Grid,GL).
symmetric_lr0(I,C,[],GL):- !.
symmetric_lr0(I,C,Grid,GL):- maplist(symetric_row(I,C),Grid,GL),!.

not_no_symmetry_yet(P):- \+ no_symmetry_yet(P).

%symmetric_h(I,C,G):- include(not_no_symmetry_yet,G,G0),sort_row_by_num_colors(G0,G1),symmetric_lr(I,C,G1,_).
%symmetric_h(I,C,G):- symmetric_lr(I,C,G,_),!.

symmetric_h(I,C,G):- symmetric_lrw(I,C,G,_).
symmetric_h(I,C,G):- flipV(G,G1),symmetric_lrw(I,C,G1,_).
symmetric_v(I,C,G):- rot270(G,G0),symmetric_lrw(I,C,G0,_).
symmetric_v(I,C,G):- rot270(G,G0),flipV(G0,G1),symmetric_lrw(I,C,G1,_).

symmetric_hv(EXQ2,CX,EYQ2,CY,H,V,G):- symmetric_h(EXQ2,CX,G),symmetric_v(EYQ2,CY,G),TY is EYQ2*3, TY > V, TX is EXQ2*3, TX > H,!.
symmetric_hv(EXQ2,CX,EYQ2,CY,H,V,G):- symmetric_hh(EXQ2,CX,G),symmetric_vv(EYQ2,CY,G).

symmetric_hh(I,C,G):- symmetric_lr(I,C,G,_).
symmetric_vv(I,C,G):- rot270(G,G0),symmetric_lr(I,C,G0,_),!.


incr(X,X1):- X1 is X + 1.

symetric_xy_3x3(G,
[[Q2,  CN,  Q1],
 [CW,  CC,  CE],
 [Q3,  CS,  Q4]]):- 
 notrace(symetric_xy(SXQ2,SYQ2,EXQ2,EYQ2,SXCC,SYCC,EXCC,EYCC,SXQ4,SYQ4,EXQ4,EYQ4,G)),!,
  clip(SXQ2,SYQ2,EXQ2,EYQ2,G,Q2),
  clip(SXQ4,SYQ2,EXQ4,EYQ2,G,Q1),
  clip(SXQ2,SYQ4,EXQ2,EYQ4,G,Q3),
  clip(SXQ4,SYQ4,EXQ4,EYQ4,G,Q4),

    clip(SXCC,SYCC,EXCC,EYCC,G,CC),
    clip(SXCC,SYQ2,EXCC,EYQ2,G,CN),
    clip(SXCC,SYQ4,EXCC,EYQ4,G,CS),
    clip(SXQ4,SYCC,EXQ4,EYCC,G,CE),
    clip(SXQ2,SYCC,EXQ2,EYCC,G,CW),


  print_side_by_side(Q2,Q1),
  wdmsg("printed_pie II   I"),
  print_side_by_side(Q3,Q4),
  wdmsg("printed_pie III  IV"),
  !.



make_empty_grid(GO):- GO=_.

clip(SX,SY,EX,EY,_,GO):- (EY<SY ; EX<SX) , !, make_empty_grid(GO).
%clip(SX,SY,EX,EY,G,GO):- G==[[]],!,GO=[[]].
clip(SX,SY,EX,EY,G,GO):-
  %assertion(SX=<EX),assertion(SY=<EY),
  SzX is EX-SX+1,
  SzY is EY-SY+1,
  %print_grid(G),
  make_grid(SzX,SzY,GO),
  copy_to_clip_r(1,1,1,SX,SX,SY,EX,EY,G,GO),
  nop(print_grid(GO)).

copy_to_clip_r(AtX,AtY,ResetX,ResetSX,SX,SY,EX,EY,G,GO):-

  ignore((hvc_value(SX,SY,C,G),nb_set_chv(C,AtX,AtY,GO))),
  incr(SX,SX2),
 ((SX2=<EX) -> 
    (incr(AtX,AtX2),copy_to_clip_r(AtX2,AtY,ResetX,ResetSX,SX2,SY,EX,EY,G,GO)) ;
    (incr(SY,SY2),
      ((SY2=<EY) -> (incr(AtY,AtY2),copy_to_clip_r(ResetX,AtY2,ResetX,ResetSX,ResetSX,SY2,EX,EY,G,GO));
         true))).

nb_set_nth1_oob(_,[],_):-!.
nb_set_nth1_oob(1,Row,C):- !, nb_setarg(1,Row,C).
nb_set_nth1_oob(N,[_|Row],C):- Nm1 is N -1, nb_set_nth1_oob(Nm1,Row,C).

nb_set_chv(C,H,V,Grid):- nth1(V,Grid,Row),nb_set_nth1_oob(H,Row,C).
hvc_value(H,V,C,Grid):-  nth1(V,Grid,Row),nth1(H,Row,C).

idealistic_symetric_xy_3x3(
[[Q2,         CN,        flipH(Q2)],
 [CW,        _CC,        flipH(CW)],
 [flipV(Q2),  flipV(CN), flipHV(Q2)]]).

repair_symmetry(G,GR):-

   [[Q2,  CN,   Q1],
    [CW,  CC,   CE],
    [Q3,  CS,   Q4]] = Grid,

  [[Q2R,  CNR,   Q1R],
   [CWR,   CC,   CER],
   [Q3R,  CSR,   Q4R]] = Repair,

 symetric_xy_3x3(G,Grid),
 reassemble(Grid,Repaired),
 assemble(Repaired,GR).

reassemble(X,X):-!.
reassemble(
   [[Q2,  CN,   Q1],
    [CW,  CC,   CE],
    [Q3,  CS,   Q4]],

  [[Q2R,  CNR,   Q1R],
   [CWR,   CC,   CER],
   [Q3R,  CSR,   Q4R]]):-

 must_det_l((
  votes_v_h_hv(Q2,Q3,Q1,Q4,Q2R),
  votes_v_h_hv(Q1,Q4,Q2,Q3,Q1R),
  votes_v_h_hv(Q3,Q2,Q4,Q1,Q3R),
  votes_v_h_hv(Q4,Q1,Q3,Q2,Q4R),
  votes_90_180_270(CN,CE,CS,CW,CNR),
  votes_90_180_270(CE,CS,CW,CN,CER),
  votes_90_180_270(CS,CW,CN,CE,CSR),
  votes_90_180_270(CW,CN,CE,CS,CWR))).


assemble(
 [[Q2,  CN,   Q1],
  [CW,  CC,   CE],
  [Q3,  CS,   Q4]],
  GR):-
        join_cols([Q2,  CN,   Q1],RowsA),
        join_cols([CW,  CC,   CE],RowsC),
        join_cols([Q3,  CS,   Q4],RowsC),
        append([RowsA,RowsC,RowsC], GR).


votes_v_h_hv(Q2,Q3,Q1,Q4,Q2R):-     votes4(Q2,[flipV(Q3),flipH(Q1),flipHV(Q4)],Q2R).
votes_90_180_270(CN,CE,CS,CW,CNR):- votes4(CN,[rot90(CE),rot180(CS),rot270(CW)],CNR).

is_empty_grid(Empty):- make_empty_grid(MG),MG=@=Empty,!.
is_empty_grid(Empty):-  (Empty==[] ; Empty ==[[]]),!.
my_call(rot90,Q,Empty):- is_empty_grid(Empty),!,Q=Empty.
my_call(rot90,Q,Arg2):- rot270(Arg2,Q).
my_call(rot180,Q,Arg2):- rot180(Arg2,Q).
my_call(rot270,Q,Arg2):- rot90(Arg2,Q).

cast_votes4([],[]).
cast_votes4([P|PP],[ExpectedQ|VV]):- 
   P=..[F,Arg2],my_call(F,ExpectedQ,Arg2),
   cast_votes4(PP,VV),!.

votes4(Q,Images,Winners):- 
  cast_votes4(Images,Votes),
  tally_some_votes([Q|Votes],Results),
  keep_winners(Results,Winners),!.



keep_winners(Results,Winners):- is_list(Results),maplist(keep_winners,Results,Winners).
keep_winners(W,_):- assertion(ground(W)),fail.
keep_winners(V4-V3-V2-V1,V1):- V1==V2;V1==V3;V1==V4.
keep_winners(V4-V3-V2-V1,V2):- V2==V1;V2==V3;V2==V4.
keep_winners(V4-V3-V2-V1,V3):- V3==V1;V3==V2;V3==V4.
keep_winners(V4-V3-V2-V1,V3):- V4==V1;V4==V2;V4==V3.
keep_winners(V1,V1):- V1 \= (_-_).
keep_winners(W,undecided(W)).

tally_some_votes([I|Images],Q):- 
  tally_votes_list(I,Images,Q).

tally_votes_list(I,[],I).
tally_votes_list(Q,[I|Images],O):-
  tally_votes(Q,I,M),
  tally_votes_list(M,Images,O).

tally_votes(I,V,I):- var(V),!.
tally_votes(I,V,V):- var(I),!.
tally_votes([],I,I):- !.
tally_votes(I,[],I):- !.
tally_votes([I|II],[H|TT],[V|MM]):-  tally_votes(I,H,V),tally_votes(II,TT,MM).
tally_votes(I,V,V-I).


