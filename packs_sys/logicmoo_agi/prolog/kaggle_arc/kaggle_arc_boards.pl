/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

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

detect_supergrid(TestID,ExampleNum,In,Out):-
  dash_chars,
  dash_chars,
  grid_size(In,IH,IV),
  grid_size(Out,OH,OV),
  max_min(OV,IV,V,_),max_min(OH,IH,H,_),
  ignore((dmsg(detect_supergrid(TestID,ExampleNum)),
  most_d_colors(Out,CO,NO),
  most_d_colors(In,CI,NI), 
  maplist(ignore,[
  print_side_by_side(cyan,In,test_in(ExampleNum),_,Out,test_out(ExampleNum)),
  print_side_by_side(cyan,NI,CI,_,NO,CO),
  fif(pair_dictation(TestID,ExampleNum,In,Out,T),pt(T)),
  show_patterns(In),show_patterns(Out),
  fif(find_ogs(HIO,VIO,In,Out),show_found(HIO,VIO,H,V,contains(out,in),In)),
  fif(find_ogs(HOI,VOI,Out,In),show_found(HOI,VOI,H,V,contains(in,out),Out))]))),!,
  dash_chars,
  dash_chars,!.

show_found(HOI,VOI,H,V,Info,F):-
  HO is HOI-3, VO is VOI-3, 
  offset_grid(HO,VO,F,OF),
  constrain_grid(f,_TrigF,OF,FF),!,
  print_grid(H,V,Info,FF),!.

