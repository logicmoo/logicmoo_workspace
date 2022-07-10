/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- use_module(library(nb_set)).
:- use_module(library(lists)).

/*
% detect_supergrid(Grid,SGrid):- ...
line Separated
Symmetry sorta happening
Individuals by colormass % t(b230c067)
all_is_one
supergrid_is_point_dotted % '94133066'
supergrid_is_output_size % t('8d5021e8') 
supergrid_is_all % t('6150a2bd')
suprgrid_is_dots_same_count % t(ff28f65a)
supergrid_input_1 % v(cad67732)*  
supergrid_input_1 output=1x1
superinput_keypad= souput=kewypad
superinput_columns 
superinput_rows
superinput_swashica
superinput_starts_at_input_loc
superinput_blob
*/

test_supergrid:- clsmake, forall(kaggle_arc(TestID,ExampleNum,In,Out),detect_supergrid(TestID,ExampleNum,In,Out)).

detect_supergrid:- clsmake, get_current_test(TestID),detect_supergrid(TestID).
detect_supergrid(TestID):- forall(kaggle_arc(TestID,ExampleNum,In,Out),detect_supergrid(TestID,ExampleNum,In,Out)).

detect_supergrid1:- clsmake, get_current_test(TestID),detect_supergrid1(TestID).
detect_supergrid1(TestID):- ignore(nb_current(example,ExampleNum)),
  forall(kaggle_arc(TestID,ExampleNum,In,Out),detect_supergrid(TestID,ExampleNum,In,Out)).

color_subst([],[],[]):-!.
color_subst([O|OSC],[I|ISC],[O-I|O2I]):-
  color_subst(OSC,ISC,O2I).
color_subst(_OSC,_ISC,[]):-!.

detect_supergrid(TestID,ExampleNum,In,Out):- 
  detect_supergrid(TestID,ExampleNum,In,Out,TT),  
  guess_board(TT),
  print(TT),
  dash_chars,
  dash_chars,!.

guess_board(TT):- arc_setval(TT,guess_board,t).

detect_supergrid(TestID,ExampleNum,In0,Out0,TT):-
  % grid_size(In0,IH,IV), 
  grid_size(Out0,OH,OV), % max_min(OV,IV,V,_),max_min(OH,IH,H,_),
  subst(In0,black,wbg,In),
  subst(Out0,black,wbg,OOut),
  pair_dictation(TestID,ExampleNum,In0,Out0,T),
   T.in = In0, T.out = Out0,
  ((OV==1,OH==1) -> (O2I=[]) ;
  (T.in_specific_colors = ISC,
   T.out_specific_colors = OSC,   
   color_subst(OSC,ISC,O2I))),
  subst_1L(O2I,OOut,Out),
  subst_1L(O2I,Out0,OutF),
  dict_pairs(T,_,Pairs),
  list_to_rbtree_safe(Pairs,TT),!,
  arc_setval(TT,out_color_remap, O2I),

  show_colorfull_idioms(In0),
  show_colorfull_idioms(Out0),

  maplist(must_det_ll,[
  (most_d_colors(Out,CO,NO),arc_setval(TT,out_d_colors,CO),arc_setval(TT,out_map,NO)),  
  (most_d_colors(In,CI,NI),arc_setval(TT,in_d_colors,CI),arc_setval(TT,in_map,NI)),
  fif(find_ogs(HOI,VOI,In0,OutF),arc_setval(TT,z_in_contains_out,(HOI,VOI))),
  fif(find_ogs(HIO,VIO,OutF,In0),arc_setval(TT,z_out_contains_in,(HIO,VIO))),
  %@TODO record in the input _may_ hold the output 
  dash_chars,
  dash_chars,
  dmsg(detect_supergrid(TestID,ExampleNum)),
  print_side_by_side(cyan,In0,test_in(ExampleNum),_,Out0,test_out(ExampleNum)),
  pt(O2I),
  print_side_by_side(cyan,NI,CI,_,NO,CO),
  
  %show_patterns(In),show_patterns(Out),
  true]),!.
  %gset(TT.z_contains_out)=in(HIO,VIO),
  %gset(TT.z_contains_in)=out(HOI,VOI),
  
%fav(t(d631b094),human(globalpoints,target=[get(points)],maplist(arg(1)))).
fav(t(d631b094),human(i(remaining_points),obj_into_cells,print_grid,get(full_grid))).
  
is_fti_step(obj_into_cells).
is_fti_step(print_grid).
obj_into_cells(VM):- maplist(color,VM.grid,Row1),set_vm(full_grid,[Row1]).

show_found(HOI,VOI,H,V,Info,F):-
  HO is HOI-3, VO is VOI-3, 
  show_found2(HO,VO,H,V,Info,F).

show_found2(HO,VO,H,V,Info,_F):- wdmsg(show_found2(HO,VO,H,V,Info)),!.
show_found2(HO,VO,H,V,Info,F):-
  offset_grid(HO,VO,F,OF),
  constrain_grid(f,_TrigF,OF,FF),!,
  print_grid(H,V,Info,FF),!.

test_grid_hint:- clsmake, forall(arc_test_name(TestID),test_grid_hint(TestID)).

save_grid_hints:-  forall(arc_test_name(TestID),test_grid_hint(TestID)),
  listing(arc_test_property/3).

%test_grid_hint:- get_current_test(TestID),test_grid_hint(TestID).
test_grid_hint(TestID):- grid_hint(TestID).

grid_hint(TestID):- format('~N'),
  kaggle_arc(TestID,(trn+0),In,Out),!,
  findall(Hint,(grid_hint(In,Out,Hint),
   (  %write('testing... '),print(Hint),write(' '),
       forall((kaggle_arc(TestID,(trn+N),In1,Out1),N>0),
                (grid_hint(In1,Out1,Hint1),
                  % print(Hint1),write(' '),
                  Hint1==Hint)))), Confirmed),
      format('~Nconfirmed... ~w~n',[Confirmed]),
      maplist(assert_test_property(TestID,grid_hint),Confirmed).

assert_test_property(TestID,Prop,Data):-
  my_asserta_if_new(arc_test_property(TestID,Prop,Data)).

grid_hint(In,Out,color_change_only):- once((into_monochrome(In,In0),into_monochrome(Out,Out0),Out0=@=In0)).
grid_hint(In,Out, input(Hint)):- grid_hint_io(i,Out,In,Hint).
grid_hint(In,Out,output(Hint)):- grid_hint_io(o,In,Out,Hint).

%maybe_fail_over_time(Time,Goal):- fail_over_time(Time,Goal).
maybe_fail_over_time(_Time,Goal):- once(Goal).

col_color(CC,CC).
col_color(black,bg):-!.
col_color(_,fg).

grid_hint_io(IO,_In,Out,has_x_columns(CC)):- maybe_fail_over_time(1.2,has_x_columns(Out,_,Color,_)),col_color(Color,CC).
grid_hint_io(IO,_In,Out,has_y_columns(CC)):- maybe_fail_over_time(1.2,has_y_columns(Out,_,Color,_)),col_color(Color,CC).
%grid_hint_io(IO,In,Out,find_ogs):- maybe_fail_over_time(1.2,find_ogs(_,_,In,Out)).
grid_hint_io(IO,In,Out,find_ogs_no_pad):- maybe_fail_over_time(1.2,((ogs_11(_,_,In,Out)))).
%grid_hint_iso(IO,In,_Out,_IH,_IV,OH,OV,is_xy_columns):- once(has_xy_columns(In,OH,OV,_Color)).
grid_hint_io(IO,In,Out,Hint):- grid_size(In,IH,IV),grid_size(Out,OH,OV),grid_hint_iso(IO,In,Out,IH,IV,OH,OV,Hint).

grid_hint_iso(IO,In,Out,IH,IV,OH,OV,grid_size(IO,OH,OV)).
grid_hint_iso(IO,In,Out,GH,GV,GH,GV,containsAll(LeftOver)):- mapgrid(remove_color_if_same,Out,In,NewIn),
   mass(NewIn,Mass), (Mass==0 -> LeftOver=[] ; ignore(unique_colors(NewIn,LeftOver))).
grid_hint_iso(o,_In,_Out,IH,IV,OH,OV,purportional(H,V)):- once(IH\==OH ; IV\==OV), V is rationalize(IV/OV), H is rationalize(IH/OH).
grid_hint_iso(o,In,Out,IH,IV,OH,OV,purportional_mass(Mass)):- mass(In,IMass),mass(Out,OMass), IMass\==0,Mass is rationalize(OMass/IMass),Mass\==1.
grid_hint_iso(IO,In,Out,IH,IV,OH,OV,color_spread(IO,OU,Shared,UI)):- unique_colors(In,IMass),unique_colors(Out,OMass),intersection(IMass,OMass,Shared,UI,UO).

entire_row(Color,Row):- maplist(==(Color),Row).

mentire_row(black,Row):- !, maplist(=(black),Row).
mentire_row(C2,OtherRow):- include(\==(C2),OtherRow,Missing),
  once(maplist(=(C2),Missing);(length(Missing,L),L=<1)).

grid_area(In,Area):- grid_size(In,H,V), Area is H*V.

remove_color_if_same(X,Y,_):- X==Y,!.
remove_color_if_same(_X,Y,Y).

has_xy_columns(In,X,Y,Color,Chunks):- 
  grid_area(In,HV), HV > 18,
  grid_size(In,GH,GV), 
  has_x_columns(In,X,Color,BorderNumsX),
  has_y_columns(In,Y,Color,BorderNumsY),!,
  (X>1 ; Y>1),
  make_grid(X,Y,Chunks),
  %pt(slicing(BorderNumsX,BorderNumsY,onto(X,Y))),
  gather_chunks(Color,In,Chunks,1,1,X,Y,BorderNumsX,BorderNumsY).

gather_chunks(_Color,In,Chunks,X,Y,GX,GY,BorderNumsX,BorderNumsY):- Y>GY,!.
gather_chunks(Color,In,Chunks,X,Y,GX,GY,BorderNumsX,BorderNumsY):-
  nth1(Y,Chunks,Row),nth1(X,Row,Cell),
  nth1(X,BorderNumsX,SX,RX),nth1(X,RX,EX),
  nth1(Y,BorderNumsY,SY,RY),nth1(Y,RY,EY),
  SX1 is SX + 1, 
  SY1 is SY + 1,
  EX1 is EX - 1,
  EY1 is EY - 1,
  clip(SX1,SY1,EX1,EY1,In,Clip),
  % print_grid((X,Y),Clip), pt(clipped(SX,SY,EX,EY,into(X,Y))),
  % once(Clip = [_,[_,Cell|_]|_];Clip = [[Cell|_]|_]),  
  once(((Clip = [_,[_,C|_]|_];Clip = [[C|_]|_];(member(CR,Clip),member(C,CR))),C\==Color,C\==black,Cell=C)),
  (GX =< X -> (Yi is Y + 1, Xi is 1) ; (Xi is X+1, Yi is Y)),
  gather_chunks(Color,In,Chunks,Xi,Yi,GX,GY,BorderNumsX,BorderNumsY).


  



has_x_columns(In,X,Color,BorderNums):- rot90(In,In90), !, has_y_columns(In90,X,Color,BorderNums).

has_y_columns(In,Y,Color,BorderNums):- var(Color), unique_colors(In,Colors),reverse(Colors,ColorsR),!,
  member(Color,ColorsR),nonvar(Color), 
  has_y_columns(In,Y,Color,BorderNums).
has_y_columns(In,_Y,Color,_):- (append(_,[RowBefore,RowNext|_],In), entire_row(Color,RowBefore), entire_row(Color,RowNext)),!,fail.

has_y_columns(In,Y,Color,BorderNums):- 
  has_y_columns1(In,Y,Color,BorderNums),
  (\+ illegal_column_data1(In,Color,BorderNums)).

% bleeding of this color
illegal_column_data1(In,Color,BorderNums):- 
  nth1(Nth,In,OtherRow),\+ member(Nth,BorderNums),
  append(_,[C1,C2|_],OtherRow),C1 == C2, C1 == Color,!.

% completely differnt colored border
illegal_column_data1(In,Color,BorderNums):- 
  nth1(Nth,In,OtherRow),\+ member(Nth,BorderNums),
  enum_colors(C2),C2\==Color,entire_row(C2,OtherRow),!.

illegal_column_data(In,Color,BorderNums):- 
  member(Nth,BorderNums),
  NthLess is Nth+1 , NthMore is Nth+2,
  \+ member(NthLess,BorderNums),
  \+ member(NthMore,BorderNums),
  nth1(NthLess,In,Row1),
  nth1(NthMore,In,Row2),
  (member(C1,Row2),member(C2,Row1)),
  C1 == C2, C1 == Color,!.



has_y_columns1(In,Y,Color,BorderNums):-
  append([First|_],[Last],In),
  mentire_row(Color,First),mentire_row(Color,Last),!,
  findall(Nth,(nth1(Nth,In,Row),mentire_row(Color,Row)),BorderNums),
  length(BorderNums,YM1),Y is YM1 - 1.

has_y_columns1(In,Y,Color,Out):-  
  length(In,GY1),GY is GY1 + 1,
  append([First|_],[Last],In),
  \+ mentire_row(Color,Last), \+ mentire_row(Color,First),!,
  Color = C2,
  findall(Nth,(nth1(Nth,In,Row),enum_colors(C2),maplist(==(C2),Row)),BorderNums),
  length(BorderNums,YM1),Y is YM1 + 1,
  append([0|BorderNums],[GY],Out).

%globalpoints(grid,points)
