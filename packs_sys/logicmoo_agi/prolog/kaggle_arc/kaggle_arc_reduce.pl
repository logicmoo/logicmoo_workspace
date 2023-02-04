/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).
:- include(kaggle_arc_header).

:- use_module(library(nb_set)).
:- use_module(library(lists)).

split_list(N1,Left,Grid,Right):- length(Left,N1),append(Left,Right,Grid).

:- decl_pt(unreduce_grid(infoR,grid)).
unreduce_grid(gridOpFn(GridR,OP),GridO):- !, unreduce_grid(GridR,OP,GridO).
unreduce_grid(GridO,GridO).

:- decl_pt(unreduce_grid(grid,list,grid)).
unreduce_grid(G,[],G):-!.
unreduce_grid(G,OP,GO):- maybe_into_grid_io(G,GG),!,unreduce_grid(GG,OP,GO).
unreduce_grid(G,[OP|List],GO):- !, unreduce_grid(G,OP,M),unreduce_grid(M,List,GO).
unreduce_grid(A^B,OP,AO^BO):-!, unreduce_grid(A,OP,AO),unreduce_grid(B,OP,BO).
unreduce_grid(G,OP,GO):- must_grid_call(OP,G,GO).

reorder_cbody(Rev,Left,Right):- (call(Rev) -> (Left,Right); (Right,Left)).


ensure_reduction_guide(O,O):-!.

ensure_reduction_guide(OPA,OPB):- var(OPA), \+ attvar(OPA),!,freeze(OPA,ensure_reduction_guide(OPA,OPB)).
ensure_reduction_guide(A,B):- \+ compound(A), !, A=B,!. %freeze(B,A=B).
ensure_reduction_guide([A|OPA],[B|OPB]):- !, ensure_reduction_guide(A,B),ensure_reduction_guide(OPA,OPB).
ensure_reduction_guide(A,B):- \+ compound(A), !, freeze(B,A=B).
ensure_reduction_guide(OPA,OPB):- functor(OPA,F,N),functor(OPB,F,N),arg(1,OPA,A),arg(1,OPB,B),!,ensure_reduction_guide(A,B).

  /*(length(E,L),length(EB,L),arg(1,OPB,EB))),!.*/

combin_pair_op(OPA,OPB,OP):- (OPA=OPB->OP=OPA;OP=op(OPA,OPB)).

/*
reduce_op2(PassNo,A^B,OPA^OPB,AA^BB):- reduce_op1(PassNo,B,OPB,BB), ensure_reduction_guide(OPB,OPA),reduce_op1(PassNo,A,OPA,AA),!.
reduce_op1(PassNo,A^B,OPA^OPB,AA^BB):- reduce_op1(PassNo,A,OPA,AA), reduce_op1(PassNo,B,OPB,BB),ensure_reduction_guide(OPA,OPB).
reduce_op1(PassNo,A^B,OPA^OPB,AA^BB):- reduce_op2(PassNo,A^B,OPA^OPB,AA^BB).
reduce_op1(PassNo,A^B,OPA^OPB,AA^BB):- nth1(N,A,EA,A0),maplist(=(_),EA),nth1(N,B,EB,B0),EA=@=EB,reduce_op2(PassNo,A0^B0,OPA^OPB,AA^BB).
reduce_op1(PassNo,A^B,OPA^OPB,AA^BB):- !, reduce_op2(PassNo,B^A,OPB^OPA,BB^AA).
*/

get_reduction_types(Types):- nb_current(grid_reductions,Types),Types\==[],!.
get_reduction_types([shrink]).

reduce_op1(PassNo,A^B,OP,AA^BB):-  stack_check_or_call(2000,(dmsg(stackcheck>2000),break)),
  get_reduction_types(Types),
  reduce_op1_by_size(Types,PassNo,A,OPA,AA),is_grid(AA),
  ensure_reduction_guide(OPA,OPB),
  reduce_op1_by_size(Types,PassNo,B,OPB,BB),is_grid(BB),
  combin_pair_op(OPA,OPB,OP).

reverse_p2(P2,R,RR):- nonvar(R), !, reverse(R,R1),grid_call(P2,R1,RR1),reverse(RR1,RR).
reverse_p2(P2,R,RR):- nonvar(RR), !, reverse(RR,RR1),grid_call(P2,R1,RR1),reverse(R1,R).
reverse_p2(P2,R,RR):- grid_call(P2,R1,RR1),reverse(RR1,RR),reverse(R1,R).


copy_first(1,[RowA|Right],[RowA,RowB|Right]):-  RowA=@=RowB.
copy_last(N,R,RR):- reverse_p2(copy_first(N),R,RR).
first_second_half(Grid,GL,GR):- length(Grid,L),H is floor(L/2), length(GL,H), append(GL,GR,Grid).

reduce_op1_by_size(Types,PassNo,A,OPS,AA):- 
  length(A,Len1),Half1 is floor(Len1/2)+1,  
 % ensure_usefull(OPA),
  reduce_op1_1d(Types,Len1,Half1,PassNo,A,OPA,AA),
  % ensure_usefull(OPA),
  fix_opa(OPA,OPS).

ensure_usefull(OPA):- var(OPA),!,freeze(OPA,ensure_usefull(OPA)).
ensure_usefull([OP1|OPA]):- ensure_usefull(OP1,OPA).

ensure_usefull(_,X):-X==[],!.
ensure_usefull(OP1,OPA):- var(OP1),!,freeze(OP1,ensure_usefull(OP1,OPA)).
ensure_usefull(Was,[Step2|OPA]):- var(Step2),!, freeze(Step2,ensure_usefull(Was,[Step2|OPA])).
ensure_usefull(rollD,[Step2|_OPA]):- rotP2(Step2),!,fail.
ensure_usefull(reversed(Step1^_),[reversed(Step2^_)|OPA]):- !, ensure_usefull(Step1,[Step2|OPA]).
ensure_usefull(_,[A|B]):-!, ensure_usefull(A,B).

fix_opa([as_rot(flipD,inverseRot(flipD),grav_unrot180(flipV)), c_r(grav_unrot180(rot90)),
                                        grav_unrot180(rot90),grid_progress(g(perfect))|More],More):-!.
fix_opa([as_rot(flipD,inverseRot(flipD),grav_unrot180(flipV)), c_r(grav_unrot180(rot90)),grav_unrot180(rot90)|More],More):-!.
fix_opa(More,More).

/*
reduce_1op(Types,Half,Len,PassNo,Grid,blur(flipD),Left):- flipD(Grid,GridR), GridR==Grid,!,keep_flipD(Grid,Left).
*/
%reduce_1op(Types,Half,Len,PassNo,Grid,blur(A,flipV),Left):- length(Grid,L),LS is floor(L/2),length(Left,LS),reverse(Left,Right), RS is L-LS, 
%  (RS==LS->  (append(Left,Right,Grid),A=[]) ; (append(Left,[E|Right],Grid),A=[E])).

%reduce_1op(Types,Half,Len,_,Grid,call(=),Grid):- too_small_reduce(Grid,3),!.

/*reduce_1op(Types,Half,Len,_,Grid,copy_insert(N1),GridR):- number(N1),!,N2 is N1+1,
   length(Left,N2),append(Left,[RowA,RowB|Right],Grid),RowA=@=RowB,length(Left,N2),append(Left,[RowA|Right],GridR).
*/
% reduce_1op(Types,Half,Len,_,Grid,copy_insert(N1,N3),GridR):- fail, append(Left,[RowA,G,RowB|Right],Grid),RowA=@=RowB,length([_,_|Left],N1),append(Left,[RowA,G|Right],GridR),N3 is N1+2,!.
%reduce_1op(Types,Half,Len,_,Grid,copy_first(N),GridR):- copy_first(N,GridR,Grid).
%reduce_1op(Types,Half,Len,_,Grid,copy_last(N),GridR):- copy_last(N,GridR,Grid).

/*
h,w,cellsize,OborderThick,IborderThick

****
*****
*************
*/


reduce_op1_1d(Types,Len1,Half1,PassNo,A,OPS,AAA):- % stack_depth(Level),Level<1000,
  (reduce_op1_1e(Types,Len1,Half1,PassNo,A,OPS,AAA)*->true;
    extend_rollD_make_align(Types,Len1,Half1,PassNo,A,OPS,AAA)).

reduce_op1_1e(Types,Len1,Half1,PassNo,A,OP,AAA):-
  reduce_1op(Types,Len1,Half1,PassNo,A,OP1,AA),
  ( extend_rollD_make_align(Types,Len1,Half1,PassNo,AA,OP2,AAA)-> OP = and(OP2,OP1); (AA=AAA,OP=OP1)).


% COMPRESS 
extend_rollD_make_align(Types,Len1,Half1,PassNo,I,and(RollDR,OP1),O):-  fail,
  rollD_make_align(RollDR,I,M), reduce_1op(Types,Len1,Half1,PassNo,M,OP1,O),improvement(I,O).

% COMPRESS 
extend_rollD_make_align(Types,Len1,Half1,PassNo,I,OP1,O):- % fail,
  reduce_1op(Types,Len1,Half1,PassNo,I,OP1,O).

test_reduce_grid_mo:-  
 into_grid(t_c3f564a4_trn_0_out,I),reduce_grid_mo(I,OPS,O),
 unreduce_grid(O,OPS,OO),print_ss([I,O,OO]), writeg([I,O,OPS,OO]).

reduce_grid_mo(I,OPS,OOO):- reduce_grid_mo1(I,OPS,OOO),!.

reduce_grid_mo1(I,OPS,OOO):- 
  reduce_grid(I,OPS1,O),
  ((rollD_make_align(RollDR,O,OO),  
     reduce_grid(OO,OPS2,OOO),
      improvement(O,OOO),
     append(OPS2,[RollDR|OPS1],OPS))
   -> true 
   ; (OOO=O,OPS=OPS1)).

rollD_make_align(rollD,I,O):- rollDR(I,O). %,improvement(I,O).
rollD_make_align(rollDR,I,O):- rollD(I,O). %,improvement(I,O).
%rollD_make_align(rot270,I,O):- rot90(I,O),nop(improvement(I,O)).
%rollD_make_align(flipD,I,O):- flipD(I,O),improvement(I,O).



maybe_improves(rollD).
maybe_improves(rollDR).
is_improvement(I,RollD):- maybe_improves(RollD),grid_call(RollD,I,O),improvement(I,O).

improvement(I,O):- grid_size(I,IH,IV),grid_size(O,OH,OV), OH*OV<IH*IV,!.
improvement(I,O):- improvement_v(I,O,_),!.
improvement(I,O):- rot90(I,II),improvement_v(II,O,_),!.

improvement_v(I,O,dupe_lines):- I\=@=O,same_lines(I,_,IC),same_lines(O,_,OC),OC>IC,!.
improvement_v(I,O,soild_lines):- solid_lines(I,_,IC),solid_lines(O,_,OC),OC>IC,!.
 same_lines(Grid,Ns,C):- findall(N1-N2,(nth1(N1,Grid,Row1),nth1(N2,Grid,Row2),N1<N2,Row1=@=Row2),Ns),length(Ns,C).
solid_lines(Grid,Ns,C):- findall(N1,(nth1(N1,Grid,Row1),entire_row(Row1)),Ns),length(Ns,C).


:- discontiguous(reduce_1op/7).

too_small_reduce(H,_L,Two):- H=<Two. %X=<N,Y=<N,!.

:- style_check(-singleton).

reduce_1op(_Type,Len1,Half1,PassNo,GridIn,[],Out):- GridIn = [[C1,C1]],GridIn = Out,!.
reduce_1op(_Type,Len1,Half1,PassNo,GridIn,[],Out):- GridIn = [[C1],[C1]],GridIn = Out,!.
%reduce_1op(_Type,Len1,Half1,PassNo,GridIn,[make_solid_object(square,1,1)],Out):- GridIn = [[C1]],GridIn = Out,!.

%reduce_1op(_Type,Len1,Half1,PassNo,GridIn, make_solid_object(Rect,H,V),OUT):-  GridIn = [Row1|GridR], maplist(=@=(Row1),GridR),
%  Row1= [C1|Row], maplist(=@=(C1),Row), grid_size(GridIn,H,V), once(H > 1 ; V > 1),!,
%    (H==V->(Rect=square,OUT=[[C1]]);(H>V->(Rect=rect,OUT=[[C1,C1]]);(Rect=rect,OUT=[[C1],[C1]]))).
reduce_1op(_Type,Len1,Half1,PassNo,GridIn, make_solid_object(Rect,H,V),OUT):-  fail, GridIn = [Row1|GridR], maplist(=@=(Row1),GridR),
  Row1= [C1|Row], maplist(=@=(C1),Row), grid_size(GridIn,H,V), once(H > 1 ; V > 1),!,
    (H==V->(Rect=square,OUT=[[C1]]);(H>V->(Rect=rect,OUT=[[C1,C1]]);(Rect=rect,OUT=[[C1],[C1]]))).

reduce_1op(_Type,Len1,Half1,PassNo,GridIn, make_solid_object(Rect,H,V),OUT):-  GridIn = [Row1|GridR], maplist(=@=(Row1),GridR),
  Row1= [C1|Row], maplist(=@=(C1),Row), grid_size(GridIn,H,V), once(H > 1 ; V > 1),!,
    (H==V->(Rect=square,OUT=[[C1]]);(H>V->(Rect=rect,OUT=[[C1]]);(Rect=rect,OUT=[[C1]]))).


reduce_1op(_Type,Half,Len,_,[Row1|Grid],copy_row_ntimes(1,Times),[Row1]):- 
  maplist(=@=(Row1),Grid),length([Row1|Grid],Times),Times>1.

reduce_1op(Types,Half,Len,_,I,double_size,O):- is_reduce_type(shrink,Types), I=[I1,I2|_],I1=@=I2, half_size(I,O),!.

%reduce_1op(Types,Half,Len,_,Grid,_,_):- grid_size(Grid,X,Y), max_min(X,Y,H,L),too_small_reduce(H,L,2),!,fail.
% COMPRESS
reduce_1op(Types,Half,Len,_,_Grid,_,_):- fail, Len=<1,!,fail.

make_solid_object(_,H,V,[[Color|_]|_],Grid):- solid_rect(Color,H,V,Grid).

is_reduce_type(E,L):- member(E,L),!.

% COMPRESS/SHRINK 
reduce_1op(Types,Len1,Half1,PassNo,A,copy_row_ntimes(N1,N2),AAA):- is_reduce_type(shrink,Types),
  reduce_1op_cr(Len1,Half1,PassNo,A,copy_row_ntimes(N1,N2),AAA),
  my_assertion(number(N2)),my_assertion(number(N1)).

reduce_1op_cr(Half,Len,_,Grid,copy_row_ntimes(N,Count),NewGrid):- append(Rows,RowRowRest,Grid),
  [Row1,Row2|Rest]=RowRowRest,Row1=@=Row2,length([_|Rows],N),
  maplist_until_count(Count,=@=(Row1),RowRowRest),
  length(Remove,Count),append(Remove,More,RowRowRest),
  append(Rows,[Row1|More],NewGrid).

%reduce_1op_cr(Half,Len,_,[Row2,Row3|Grid],copy_row_ntimes(1,1),[Row2|Grid]):- Row2=@=Row3.
%reduce_1op_cr(Half,Len,_,[Row1,Row2,Row3|Grid],copy_row_ntimes(2,1),[Row1,Row2|Grid]):- Row2=@=Row3.

/*
reduce_1op(Types,Half,Len,_,Grid,copy_row_ntimes(N1,2),GridR):- 
          append(L,[A,B,C,D,E|R],Grid),A=@=B,B=@=C,C=@=D,D=@=E, 
          append(L,[A,B,C|R],GridR),length([_|L],N1).
reduce_1op(Types,Half,Len,_,Grid ,copy_row_ntimes(N1,2),GridR):- 
          append(L,[A,B,C,D|R],Grid),A=@=B,B=@=C,C=@=D, 
          append(L,[A,B|R],GridR),length([_|L],N1).
%reduce_1op(Types,Half,Len,_,Grid,copy_row_ntimes(N1,2),GridR):- append(L,[A,B,C|R],Grid),A=@=B,B=@=C, append(L,[A|R],GridR),length([_|L],N1).

*/

copy_row_ntimes(N1,Times,Grid,GridR):- Times1 is Times-1,copy_row_ntimes_1(N1,Times1,Grid,GridR).

copy_row_ntimes_1(1,1,[Row1|Grid],[Row1,Row1|Grid]):-!.
copy_row_ntimes_1(1,N,[Row1|Grid],[Row1,Row1,Row1|GridO]):- N>3,!,
  N3 is N-3,copy_row_ntimes_1(1,N3,[Row1|Grid],GridO).
copy_row_ntimes_1(1,N,[Row1|Grid],[Row1|GridO]):- 
  N3 is N-1,copy_row_ntimes_1(1,N3,[Row1|Grid],GridO).
copy_row_ntimes_1(N1,Times,Grid,GridR):- length([_|Left],N1), append(Left,[Row1|Right],Grid),
  copy_row_ntimes_1(1,Times,[Row1|Right],Result),
  append(Left,Result,GridR).
%copy_row_ntimes(N1,Two,Row,Result):- make_list(Row,N1,NRows),make_list(NRows,Two,Result).


reduce_1op(Types,Len1,Half1,PassNo,A,OP,O):- once(is_reduce_type(grid,Types);is_reduce_type(compress,Types)),
    reduce_1op_ca(Len1,Half1,PassNo,A,OP,O),length(O,OL),OL>=3.
reduce_1op_ca(Half,Len,_,I,reapply_cutaway(LBR),O):- reduce_cutaway(LBR,I,O).
reduce_1op_ca(Half,Len,_,I,reapply_cutaway_row(LBR),O):- reduce_cutaway_row(LBR,I,O).


% COMPRESS 
reduce_1op(Types,Len1,Half1,PassNo,A,OP,O):- is_reduce_type(compress,Types), reduce_1op_ci(Len1,Half1,PassNo,A,OP,O),length(O,OL),OL>=3.
reduce_1op_ci(Half,Len,_,Grid,copy_insert(N1,N22),GridR):- nth1(N1,Grid,Row1),
  findall(N2,(nth1(N2,Grid,Row2),N1<N2, Row1=@=Row2),N22), N22\==[],!,
  split_list(N1,Left,Grid,Right),
  include(\=@=(Row1),Right,GridRR),append(Left,GridRR,GridR).


%one_reduction1(insert_row(N,[E|Row1]),G1,G2,G1R,G2R):- nth1(N,G1,[E|Row1],G1R),maplist(=@=(E),Row1),nth1(N,G2,[E|Row2],G2R),Row1=@=Row2.
% COMPRESS
reduce_1op(Types,Half,Len,_,Grid,insert_row_of(Row,Black),GridR):- is_reduce_type(compress,Types),
  (Row==1;Row==Len),
  nth1(Row,Grid,[Black|Same],GridR),
  maplist(=@=(Black),Same).  


%reduce_1op(Types,Half,Len,_,Grid,copy_insert(N1,N2),GridR):- nth1(N1,Grid,Row1), nth1(N2,Grid,Row2,GridR),N1<N2, Row1=@=Row2.

reduce_1op(Types,Half,Len,_PassNo,Grid,border_blur(LS,flipV),MidRight):- is_reduce_type(compress,Types),
   Half>3, LS is Half-1, length(Left,LS),reverse(Left,Right),
  append(Left,MidRight,Grid),append(_Mid,Right,MidRight).

border_blur(LS,flipV,MidRight,Grid):- length(Left,LS),reverse(Left,Right),
  append(_Mid,Right,MidRight),append(Left,MidRight,Grid).

%reduce_1op(Types,Half,Len,_,Outside,make_frame(Pen),Inside):- grid_size(Outside,X,Y),X>2,Y>2,make_frame(Pen,Inside,Outside).

% ENABLED 
reduce_1op(Types,Half,Len,_,Grid,copy_n_rows_n_times(NRows,Times),ARowsLeftOver):- is_reduce_type(shrink,Types), fail,
  NRows = 4, between(1,Half,NRows), uncp_n_rows_n_times(NRows,Times,ARows,Grid,LeftOver), Times\==0,
   append(ARows,LeftOver,ARowsLeftOver).

uncp_n_rows_n_times(NRows,Times,ARows,Grid,LeftOver):-
  between(2,15,NRows),length(ARows,NRows),
  %append(ARows,LeftOver,ARowsLeftOver),
  append(ARows,Rest,Grid),
  rest_grid_repeats(0,ARows,Rest,Times,LeftOver).

copy_n_rows_n_times(NRows,Times,ARowsLeftOver,Grid):- 
 must_det_ll((
  between(2,15,NRows),length(ARows,NRows),
  append(ARows,LeftOver,ARowsLeftOver),
  append(ARows,Rest,Grid),
  rest_grid_repeats(0,ARows,Rest,Times,LeftOver))).

%rest_grid_repeats(_,Rows,[],0,[]).
rest_grid_repeats(N,Rows,Grid,M,Slack):- 
  append(Rows,Rest,Grid),plus(1,N,MM),!,
  rest_grid_repeats(MM,Rows,Rest,M,Slack).
rest_grid_repeats(N,_,Slack,N,Slack).


% COMPRESS 
reduce_1op(Types,Half,Len,_,Grid,mult_rows_n_times(Times),ARow):- is_reduce_type(compress,Types),
   between(1,Half,Times), mult_rows_n_times(Times,ARow,Grid), Times\==0.

% Not COMPRESS 
reduce_1op(Types,Len1,Half1,PassNo,Grid,to_just_middle(OPS),AAA):- is_reduce_type(compress,Types),
  append([R1|Middle],[RLast],Grid), 
  reduce_op1_by_size(Types,PassNo,Middle,OPS,[M|MMM]),
  length(M,Width), length(R1,Width), append([R1,M|MMM],[RLast],AAA).

to_just_middle(OPS,I,O):- append([R1|Middle],[RLast],I),
   unreduce_grid(Middle,OPS,Unreduce),
   append([R1|Unreduce],[RLast],O).

reduce_1op(Types,Half,Len,_,Grid,grav_unrot180(RotG),Shape):- fail,
   once(grav_roll(Grid,RotG,Shape180)),RotG\==sameR,rot180(Shape180,Shape).

reduce_1op(Types,Half,Len,_,Grid,grav_unrot(RotG),Shape):- fail,
   once(grav_roll(Grid,RotG,Shape)),RotG\==sameR.

:- style_check(+singleton).

/*
reduce_1op(Types,Half,Len,_,Grid,left_right(Left,Reduced),GridR):- fail, 
   length(Grid,L), nth1(N1,Grid,A), nth1(N2,Grid,B), A=@=B,
   LS is floor(L/2),
   between(1,LS,LR),LRR is LS-LR,LRR>0, length(Left,LRR),reverse(Left,Right), 
   append([Left,GridR,Right],Grid),
   reduce_grid(Left^Left,Reduced).


reduce_1op(Types,Half,Len,_,Grid,left_right(Left,Reduced),GridR):- fail, 
   length(Grid,L), nth1(1,Grid,A), nth1(L,Grid,B), A=@=B,
   LS is floor(L/2),
   between(1,LS,LR),LRR is LS-LR,LRR>0, length(Left,LRR),reverse(Left,Right), 
   append([Left,GridR,Right],Grid),
   reduce_grid(Left^Left,Reduced).
*/

% reduce_1op(Types,Half,Len,_,Grid,_,_):- length(Grid,L3),L3=<3,!,fail.
% % %  
%reduce_1op(Types,Half,Len,_,Grid,_,_):- too_small_reduce(Grid,2),!,fail.
% % % reduce_1op(Types,Half,Len,_,Grid,copy_insert(N1,N2),GridR):- nth1(N2,Grid,A),N2>1, between(1,N2,N1),N1<N2,nth1(N1,Grid,B,GridR), A=@=B, 1 is abs(N1-N2).


%remove_row(Row,G,GG):- nth1(Row,G,GG).

reversed(R^R,G,GG):- !, undo_effect(R,G,GG).
reversed(R,G,GG):- !, undo_effect(R,G,GG).

copy_n_times(N,[C],Rows):- between(2,15,N), length(Rows,N),maplist(=(C),Rows).

mult_rows_n_times(N,Grid,NGrid):- mult_rows_in_pattern(copy_n_times(N),Grid,NGrid).

ensure_pattern_gen(row_padding(_Type,_Pad)).
ensure_pattern_gen(copy_n_times(_)).
ensure_pattern_gen(row_padding_lists(_T,_B)).
%ensure_pattern_gen(stripes(_StripeColor)).

row_padding_tb(Pad,[C],[Pad,C,Pad]).
row_padding_t(Pad,[C],[Pad,C]).
row_padding_b(Pad,[C],[C,Pad]).
ep3(row_padding_tb).
ep3(row_padding_b).
ep3(row_padding_t).
row_padding(P3,Pad,A,B):- ep3(P3), call(P3,Pad,A,B).

row_padding_lists(Before,After,Source,FullPattern):-
  size_between(1,2,Source),append([Before,Source,After],FullPattern),
  size_between(0,4,Before),size_between(0,4,After),Before\==After.

size_between(L,H,After):- nonvar(After)->true;(between(L,H,N),length(After,N)).

mult_rows_in_pattern(PatGen,NoPattern,HasPattern):- (var(PatGen)->ensure_pattern_gen(PatGen);true),
  mult_rows_in_pattern0(PatGen,NoPattern,HasPattern).

%mult_rows_in_pattern0(PatGen,C,[R,S|Rows]):- make_pattern(PatGen,C,[R,S|Rows]).

mult_rows_in_pattern0(_PatGen,[],[]).
mult_rows_in_pattern0(PatGen,[C1,C2|NoPattern],[R,S|Grid3]):- 
  make_pattern(PatGen,[C1,C2],[R,S|Append]),
  append(Append,HasPattern,Grid3),
  mult_rows_in_pattern0(PatGen,NoPattern,HasPattern).
mult_rows_in_pattern0(PatGen,[C|NoPattern],[R,S|Grid3]):- 
  make_pattern(PatGen,[C],[R,S|Append]),
  append(Append,HasPattern,Grid3),
  mult_rows_in_pattern0(PatGen,NoPattern,HasPattern).


make_pattern(PatGen,C,RRows):- call(PatGen,C,RRows).

/*

mult_rows_n_times(NRows,1,[R], [R|Grid]):- make_repeater(NRows,R,[R|Grid]).
mult_rows_n_times(_NRows,0,[],[]):-!.
mult_rows_n_times(NRows,NTimes,[R|NoPattern],[R|Grid]):- 
  make_repeater(NRows,R,Rows),
  append(Rows,Rest,[R|Grid]),
  mult_rows_n_times(NRows,Times,NoPattern,Rest),
  plus(Times,1,NTimes).

*/
/*

copy_n_rows_n_times(Rows,Times,ARow,Grid):- between(2,15,Rows),length(ARow,Rows),
  append([ARow,ARow,Rest],Grid),
  ((Rest=[],Times=1) ;
   (TimesTop is floor(30/Rows),between(2,TimesTop,Times),
    make_list(ARow,Times,GridL),append([ARow|GridL],Grid))).
*/
make_frame(BorderFind,Inside,Outside):- 
  var(Inside),var(Outside),!,s_l_4sides(IX,IY),
  make_grid(IX,IY,Inside),length(Bot,IX),length(Top,IX),
  OY is IY+2,length(Left,OY),length(Rt,OY),
  OX is IX+2,make_grid(OX,OY,Outside),
  append([Top|Inside],[Bot],OutsideSkiny),
  rot90(OutsideSkiny,OutsideSkiny90),
  append([Left|OutsideSkiny90],[Rt],Outside90),
  rot270(Outside90,Outside),
  nsew_edges_trimed(Outside,BorderFind).

make_frame(BorderFind,Inside,Outside):-
  (nonvar(Inside)->grid_size(Inside,IX,IY);(grid_size(Outside,OX,OY),IX is OX-2,IY is OY-2, make_grid(IX,IY,Inside))),
  (nonvar(Outside)->grid_size(Outside,OX,OY);(OX is IX+2,OY is IY+2, make_grid(OX,OY,Outside))), 
  conform_frames(BorderFind,Inside,Outside).


conform_frames(BorderFind,Inside,Outside):-
  append([_Top|OutsideMid],[_Bot],Outside),
  append([_Left|Inside90],[_Rigth],OutsideMid),
  rot90(Inside,Inside90),
  nsew_edges_trimed(Outside,BorderFind).
  


%reduce_1pair_op(PassNo,Grid,RotR,GridR):- grav_rot(Grid,RotG,GridR), reversed(RotG,RotR).

%reduce_1pair_op(PassNo,G,M,O):- maybe_into_grid_io(G,GG),!,reduce_1pair_op(PassNo,GG,M,O).
%reduce_1pair_op(PassNo,GridL,as_rot(RotG,UnRotG,Op),GridRR):- grav_rot(GridL,RotG,Grid), reversed(RotG,UnRotG), reduce_op1(PassNo,Grid,Op,GridR),call(UnRotG,GridR,GridRR).
%reduce_1pair_op(_,Grids,List,GridRs):- List=[_|_],copy_rows(Grids,List,GridRs),!.
reduce_1pair_op(PassNo,Grid, Op,GridR):- reduce_op1(PassNo,Grid,Op,GridR).
reduce_1pair_op(PassNo,A^B,OOO,AA^BB):- 
  rot_pair(Rot90,Rot270),
  grid_call(Rot90,A,AL),grid_call(Rot90,B,BL),  
  (A\==AL;B\==BL),
  reduce_op1(PassNo,AL^BL,Op,AR^BR),
  grid_call(Rot270,AR,AA),grid_call(Rot270,BR,BB),
  xfr_write_op(as_rot(Rot90,Rot270,Op),OOO).

copy_rows(Grid1^Grid2,[delete_row(N1,Color)|More],GridR1^GridR2):- 
   nth1(N1,Grid1,A1,GridM1),maplist(=(Color),A1),
   nth1(N1,Grid2,A2,GridM2),maplist(=(Color),A2),
   copy_rows(GridM1^GridM2,More,GridR1^GridR2).
copy_rows(Grid1^Grid2,[copy_insert(N1,N2)|More],GridR1^GridR2):- 
   nth1(N2,Grid1,A1),N2>1, between(1,N2,N1),N1<N2,nth1(N1,Grid1,B1,GridM1), A1=@=B1,
   nth1(N2,Grid2,A2),                             nth1(N1,Grid2,B2,GridM2), A2=@=B2,!,
   copy_rows(GridM1^GridM2,More,GridR1^GridR2).
copy_rows(O,[],O).


xfr_write_op(as_rot(Rot90,Rot270,Op),c_r(Op)):- Rot90==rot90,Rot270==rot270,!.
xfr_write_op(as_rot(Rot90,Rot270,Op),r_c(Op)):- Rot90==rot270,Rot270==rot90,!.
%xfr_write_op(as_rot(rollD,rollDR,Op),d_r(Op)).
%xfr_write_op(as_rot(rollDR,rollD,Op),d_l(Op)).
xfr_write_op(OOO,OOO).
  
%reduce_1pair_op(PassNo,GridL,as_rot(rot270,rot90,Op),GridRR):- rot90(GridL,Grid),reduce_op1(PassNo,Grid,Op,GridR),rot270(GridR,GridRR).
rot_pair(rot90,rot270).
rot_pair(rot270,rot90).
rot_pair(flipD,inverseRot(flipD)).
rot_pair(rollD,rollDR).

inverseRot(Rot,X,Y):- grid_call(Rot,X,_),grid_call(Rot,Y,X).

as_rot(L,R,Op,A^B,AA^BB):-!, as_rot(L,R,Op,A,AA),as_rot(L,R,Op,B,BB).
as_rot(L,R,Op,X,Y):- grid_call(L,X,Grid),unreduce_grid(Grid,Op,GridR),grid_call(R,GridR,Y).
left_right(LR,G,GOO):- into_grid_io(LR,Left), reverse(Left,Right),append([Left,G,Right],GO),mat_grid(GO,GOO).

%copy_insert(From,To,Grid,NewGrid):- nth1(From,Grid,Row),insert_row(To,Row,Grid,NewGrid),!.
copy_insert(N1,     N2, G,GOO):- number(N2),!,nth1(N1,G,Row),N12 is N2-1,length(Left,N12), append(Left,Right,G),append(Left,[Row|Right],GO),mat_grid(GO,GOO).
copy_insert(N1,    [N2],G,GOO):- !, copy_insert(N1,N2,G,GOO).
copy_insert(N1,[N2|N22],G,GOO):- copy_insert(N1,N2,G,GO), copy_insert(N1,N22,GO,GOO).



mat_grid(A^B,AA^BB):-!, mat_grid(A,AA),mat_grid(B,BB).
mat_grid(GO,GOO):- mapgrid(=,GO,GOO).

into_grid_io(A^B,AA^BB):- !, into_grid_io(A,AA),!,into_grid_io(B,BB).
into_grid_io(A,B):-A=[],!,B=[].
into_grid_io(A,B):- into_grid(A,B).

maybe_into_grid_io(A,B):- into_grid_io(A,B),!,A\=@=B.


%ungrav_rot(G,sameR,G):-!.
%ungrav_rot(G,sameR,G):- grid_size(Grid,X,Y), too_small_reduce(X,Y,2),!.
ungrav_rot(G,UnRotG,GG):- grav_rot(G,RotG,GG),(G==GG->UnRotG=sameR;undo_p2(RotG,UnRotG)).

:- decl_pt(reduce_grid(grid,list,grid)).

/*
with_protected_vars(AB,Goal):- ground(AB), !, call(Goal).
with_protected_vars(AB,Goal):- term_variables(AB,ABV),term_variables(Goal,GoalV),
   include(not_in(ABV),GoalV,RGoalV),   
   copy_term(Goal+AB+RGoalV,GoalC+ABC+_,ABGoals), 
   numbervars(ABC+ABGoals,110,Ten,[attvar(bind),singletons(true)]),
   %reduce_grid_pair1(ABC,OPSC,ARBRC),
   call(GoalC),
   resubst_vars(110,Ten,(GoalC,ABGoals),(Goal,ABGoalsCallable)),
   maplist(call,ABGoalsCallable).
*/


%resubst_vars(S,E,Info,ReInfo):- subst_2LC(S,E,Goal+Info,RGoal+RInfo). %,maplist(call,RGoal).
term_var_types(AB,Attvars,PlainVars,PlainSingles):-
  term_variables(AB,Vars),
  term_attvars(AB,Attvars),
  term_singletons(AB,Singles),  
  include(not_in(Attvars),Vars,Nonattvars),
  include(not_in(Attvars),Singles,PlainSingles),
  include(not_in(PlainSingles),Nonattvars,PlainVars).


protect_vars(AB,ABC,HowUnprotect):-
  term_var_types(AB,Attvars,PlainVars,PlainSingles),
  append([Attvars,PlainVars,PlainSingles],Into),
  copy_term(AB+Into,ABC+From,_Goals),
  append([AttvarsC,PlainVarsC,PlainSinglesC],From),  
  numbervars(PlainSinglesC,10,STen,[singletons(true)]),
  numbervars(ABC+AttvarsC+PlainVarsC,STen,_,[singletons(false)]),
  HowUnprotect = subst_2LC_safe(From,Into).
  
subst_2LC_safe([F|From],[B|Into],I,O):- 
  (var(B)->subst_2LC([F],[B],I,O);true),
  subst_2LC_safe(From,Into,I,O).
subst_2LC_safe([],[],I,I).
  
%reduce_grid(A^B,ROP,AA^BB):- reduce_grid(A^A,ROP,AA^AA),reduce_grid(B^B,ROP,BB^BB),!.
lpoints_to_norm(Width,Height,LPoints,IOps,LPointsNorm):- 
   points_to_grid(Width,Height,LPoints,LGrid), normalize_grid(IOps,LGrid,LPointsNorm).

%add_bg(III,III,_):- \+ fail,!.
add_bg(III,II,fg):- sub_var('black',III), \+ sub_var('bg',III), subst(III,'$VAR'('_'),'bg',II0),subst(II0,'fg','bg',II),!.
add_bg(III,II,fg):- sub_var('black',III), \+ sub_var('fg',III), subst(III,'$VAR'('_'),'fg',II0),subst(II0,'bg','fg',II),!.
add_bg(III,II,bg):- subst(III,'$VAR'('_'),'bg',II).


:- nodebug(reduce).
%b_grid_to_norm(I,OUT):- b_grid_to_norm(Op,I,OO),OUT=(OO-w(Op)).
%b_grid_to_norm(Op,I,O):- Op==[],!,I=O.
%b_grid_to_norm(Op,IO,IIOO):-  compound(IO),IO=(I^O),b_grid_to_norm(Op,I,II),!,Op\==[],b_grid_to_norm(Op2,O,OO),!,Op=Op2,IIOO=II^OO.
normalize_grid(Op,I,GOO):-   
 must_det_ll((
  debug_c(reduce,writeg(normalize_grid=I)),
  protect_vars(I,III,How),
  add_bg(III,II,Which),
  debug_c(reduce,writeg(normalize_grid_nv=II)),
  must_be_free(OO),  
  reduce_grid(II,COp,COO),
  debug_c(reduce,writeg([reduce_grid=COO,cop=COp])),
  call(How,COp,Op),
  call(How,COO,OO),
  mapgrid(fix_bg(Which),OO,GOO),
  debug_c(reduce,writeg([un_reduce_grid=OO,un_cop=Op])))).

fix_bg(Which,OO,X):- fail, Which=@=OO,!,freeze(X,X\=(_,_)).
%fix_bg(Which,OO,_):-OO=fg,!.
fix_bg(_,OO,OO).

compress_grid(Op,I,OO):- 
  normalize_grid(NOp,I,II),
  locally(nb_setval(grid_reductions,[compress]),reduce_grid(II,COp,OO)),
  append(COp,NOp,Op).
%b_grid_to_norm(IOps,LGrid,LPointsNorm):- reduce_grid(LGrid^LGrid,IOps,LPointsNorm^_).
%b_grid_to_norm([],I,I).

:- decl_pt(reduce_grid(grid,infoR)).
reduce_grid(G,O):- maybe_into_grid_io(G,GG),!,reduce_grid(GG,O).
reduce_grid(Grid,gridOpFn(GridR,OP)):- reduce_grid(Grid,OP,GridR),OP\==[],!.
reduce_grid(Grid,Grid).

reduce_grid(G,OPA,AAOO):- G\=(_^_), into_grid(G,Grid),
  A=Grid,B=A,
   reduce_two(A,B,ROP,AAO,_BBO),reverse_op(ROP,OP),
   ((OP==[reversed(rot90^rot90)])-> (OPA=[rollD|OP],rollDR(G,AA),reduce_grid(AA,OP,AAOO)) ; (OPA=OP,AAOO=AAO)).
reduce_grid(A^B,OP,AAO^BBO):- reduce_two(A,B,ROP,AAO,BBO),reverse(ROP,OP),!.
reduce_grid(AB,[],AB):-!.

reverse_op(I,R):- reverse(I,R),!.
reverse_op(I,[I]).

reduce_grid_pair1(AB,OPS,ARBR):- \+ ground(AB), 
 must_det_ll((
  protect_vars(AB,ABC,How),
  my_assertion(ground(ABC)),  
  reduce_grid_pair111(ABC,OPSP,ARBRP),
  call(How,OPSP+ARBRP,OPS+ARBR),
  nop((my_assertion((\+ (sub_term(E,OPS+ARBR),compound(E), E='$VAR'(_)))))))).

reduce_grid_pair1(AB,OPS,ARBR):- reduce_grid_pair111(AB,OPS,ARBR),!.

reduce_grid_pair111(A^B,[grid_progress(g(perfect))|ROPA],AR^BR):-
  once((reduce_grid_pass(1,A^A,[A^A],ROPA,AR^AR),
        reduce_grid_pass(1,B^B,[B^B],ROPB,BR^BR))),
  ROPA\==[],
  ROPB= ROPA,!.

reduce_grid_pair111(A^B,[grid_progress(g(ok))|ROPA],AR^BR):-
  once((reduce_grid_pass(1,A^A,[A^A],ROPA,AR^AR))),ROPA\==[], reduce_grid_pass(1,B^B,[B^B],ROPA,BR^BR).

reduce_grid_pair111(A^B,ROP,AAO^BBO):-
  reduce_grid_pass(1,A^B,[A^B],OP,AR^BR), 
  dif(UnRot,rollD),grav_rot(BR,UnRot,BB),
  grid_call(UnRot,AR,AA),
  reduce_grid_pair2(ROP,OP,UnRot^UnRot,AR^BR,AA^BB,AAO^BBO).

%reduce_grid_pair2(ROP,OP,UnRotGA^UnRotGB,AR^BR,AA^BB,AAO^BBO):- 
reduce_grid_pair2(ROP,OP,UnRotGA^UnRotGB,AR^BR,AA^BB,AAO^BBO):- 
  ((AA==AR, BB==BR) -> (ROP = OP,AAO=AA,BBO=BB)
   ;( RotOP = reversed(UnRotGA^UnRotGB),
      reduce_two(AA,BB,ROPL,AAO,BBO),
      append([OP,[RotOP],ROPL],ROP))),!.

grid_progress(Msg,G,G):- nop(print_grid(Msg,G)).

pick_reductions_from_two(AB,OPS,A,B,AA,BB):- into_grid_list(AB,List),!,
  no_repeats(OPS,(( select(A,List,Rest),
    member(B,Rest),
         reductions_from_two(OPS,A,B,AA,BB)))).


all_common_reductions(AB,OPS,Reduced):- reductions(AB,OPS,Reduced).

%all_reductions1(AB,OPS,Reduced):- reductions(AB,OPS,Reduced),!.
%all_reductions1([A,B,C|DE],OPS,Reduced):- 
/* select(A,List,Rest),
    member(B,Rest),
         reductions_from_two(OPS,A,B,AA,BB)
*/
%
%   pick_reductions_from_two(List,OPS,_,_,_,_), OPS\==[],
%   reductions(List,OPS,Reduced). 
common_reductions_from_two(GL,OPS,G1,G2,G1R,G2R):-
   into_grid_list(GL,List), 
   select(G1,List,Rest),member(G2,Rest),
   reductions_from_two(OPS,G1,G2,G1R,G2R).


reductions(GL,OPS,REDUCE):- into_grid_list(GL,List), reductions1(List,OPS,REDUCE),!.
%reductions([A,B],OPS,[AA,BB]):- !, reductions_from_two(OPS,A,B,AA,BB).
reductions1(List,[Reduce|OPS],REDUCE):-
   select(G1,List,Rest0),select(G2,Rest0,Rest),
   reductions_from_two(Reduce,G1,G2,G1R,G2R),
   maplist(reductions_from_two(Reduce),Rest,Rest,RRest,RRest),
   reductions1([G1R,G2R|RRest],OPS,REDUCE),!.
reductions1(AB,[],AB).

reduce_two(G1,G2,Reduce,G1R,G2R):- reductions_from_two(Reduce,G1,G2,G1R,G2R).

reductions_from_two(Reduce,G1,G2,G1R,G2R):- one_reduction1(Reduce,G1,G2,G1R,G2R).
reductions_from_two(r_c(Reduce),G1,G2,G1RR,G2RR):- rot90(G1,G1R),rot90(G2,G2R),one_reduction1(Reduce,G1R,G2R,G1RR,G2RR),!,Reduce\==[].
reductions_from_two(c_r(Reduce),G1,G2,G1RR,G2RR):- rot270(G1,G1R),rot270(G2,G2R),one_reduction1(Reduce,G1R,G2R,G1RR,G2RR),!,Reduce\==[].


one_reduction1(OP,G1,G2,G1R,G2R):- nonvar(OP),OP\==[],!,one_reduction1(OP,G1,G1R),!,(G2=@=G1->G2R=G1R ; one_reduction2(OP,G2,G2R)).
one_reduction1(Reduce,G1,G2,G1R,G2R):- var(Reduce),reduce_grid_pair1(G1^G2,Reduce,G1R^G2R).

one_reduction2(OP,G1,G1R):- one_reduction1(OP,G1,G1R).

%one_reduction1(OP,G=Var,GG):- var(Var),!, one_reduction1(OP,G,GG).
one_reduction1([],G1,G1R):- !, G1=G1R.
one_reduction1(OPS,G1,G1RR):- nonvar(OPS), OPS=[OP|OPL], !, one_reduction1(OP,G1,G1R),
  one_reduction1(OPL,G1R,G1RR).
one_reduction1(reversed(OP),G1,G1R):-!, one_reduction1(OP,G1,G1R).
one_reduction1(OP^OP,G1,G1R):-!,one_reduction1(OP,G1,G1R).
one_reduction1((OP),G1,G1R):-!, grid_call(OP,G1,G1R). % dmsg(one_reduction1(OP,G1,G1R)).


%reduce_grid(A^B,OPA^OPB,AA^BB):- A\=@=B,nth1(N,A,EA,A0),maplist(=(_),EA),nth1(N,B,EB,B0),EA=@=EB,reduce_grid(A0^B0,OPA^OPB,AA^BB).

reduce_grid_pass(PassNo,Grid,NBC,OP,GridR):- reduce_pair_op(PassNo,Grid,NBC,OP,GridR),!.
reduce_grid_pass(PassNo,Grid,NBC,OP,GridR):- PassNo<4,plus(PassNo,1,PassNo2), reduce_grid_pass(PassNo2,Grid,NBC,OP,GridR),!.
reduce_grid_pass(_PassNo,Grid,_,[],Grid).

reduce_pair_op(PassNo,Grid,NBC,[OP|More],Reduced):- 
    reduce_1pair_op(PassNo,Grid,OP,GridR),
    Grid\==GridR, \+ (member(E,NBC), E==GridR),
    reduce_grid_pass(PassNo,GridR,[GridR|NBC],More,Reduced),!.
reduce_pair_op(_,NR,_,[],NR):-!.

test_reduce_grid(Grid,GridO):- reduce_grid(Grid,Ops,GridO),unreduce_grid(GridO,Ops,Unred),show_sf_if_lame(Ops,Unred,Grid).


  

test_reduce_grid:- test_p2(test_reduce_grid).

show_sf_if_lame(Info,Solution,ExpectedOut):- 
       count_difs(ExpectedOut,Solution,Errors),
        (Errors\==0 -> 
          (banner_lines(red),print_side_by_side(red,Solution,'Our Solution'(Errors),_,ExpectedOut,"Expected Solution"),
          pp(Info), banner_lines(red));

          arcdbg_info(green,Info)),!. 
  


%reduce_grid(PassNo,Grid,res(Opers,Result)):- reduce_grid(PassNo,Grid,Opers,res(Opers,Result)),!.

pixel_space(4). pixel_space(3). pixel_space(2). pixel_space(1).
common_cutaway(nlbr(N,[C], [],  [C])):- pixel_space(N), is_fg_color(C).
common_cutaway(nlbr(N,[C],[C],  [C])):- pixel_space(N), C= black.
common_cutaway(nlbr(N,[C],[C,C],[C])):- pixel_space(N), is_fg_color(C).
common_cutaway(nlbr(N,[], [C],  [])):- pixel_space(N), is_fg_color(C).
common_cutaway(nlbr(N,[C],[C],  [])):- pixel_space(N), is_fg_color(C).
common_cutaway(nlbr(N,[], [C], [C])):- pixel_space(N), is_fg_color(C).
common_cutaway(nlbr(N,[], [C],  [])):- pixel_space(N), is_bg_color(C).
common_cutaway(nlbr(N,[], [C,C],[])):- pixel_space(N), is_fg_color(C).



reapply_cutaway_row(LBR,[Row1|I],OO):- LBR=nlbr(N,L,B,R),
    length(Row1,Width),
    make_list(L,Width,Left),
    make_list(B,Width,Between),
    make_list(R,Width,Right),
    reapply_between_each_row(N,Between,[Row1|I],O),
    append([Left|O],[Right],OO).

reapply_between_each_row(1,_Between,[I],[I]).
reapply_between_each_row(1,Between,[Row1,I],[Row1,Between,I]).
reapply_between_each_row(1,Between,[Row1|I],[Row1,Between|O]):-  reapply_between_each_row(1,Between,I,O).
reapply_between_each_row(N,Between,IN,OUT):- length(I,N),length(J,N),append(I,J,IN),!,append([I,Between,J],OUT).
reapply_between_each_row(N,_Between,I,I):- length(I,N),!.
reapply_between_each_row(N,Between,IN,OUT):- length(I,N),append(I,J,IN),
  reapply_between_each_row(N,Between,J,O),!,append([I,Between,O],OUT).

reapply_cutaway(LBR,I,O):- h_and_v(reapply_cutaway_row(LBR),I,O).



reduce_cutaway(LBR,I,O):- h_and_v(reduce_cutaway_row(LBR),I,O).

reduce_cutaway_row(LBR,I,O):-              LBR = nlbr(1,Left,Between,Right),
                                            once((member(Row,I), \+ maplist(=(_),Row))),
                                            (var(Between) -> common_cutaway(LBR) ; true),                                                 
                                            once((cutaway_row(Left,Between,Right,Row,_))),
                                            do_cutaway_rows(LBR,I,O),
                                            LBR\=nlbr(_,[],[],[]).

do_cutaway_rows(nlbr(1,L,B,R),I,O):- maplist(cutaway_row(L,B,R),I,O).
cutaway_row(L,B,R,Row,NewRow):- append([L,Mid,R],Row),cut_mid(B,Mid,NewRow),!.

cut_mid(_,[C],[C]).
cut_mid(Between,[C|Mid],[C|NewRow]):-
  append(Between,More,Mid),
  cut_mid(Between,More,NewRow).

