/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- ensure_loaded(muarc_public).

test_encoders_mining(TestID):- var(TestID),!,simp_encode(TestID),test_encoders_mining(TestID).
test_encoders_mining(TestID):- ensure_test(TestID), simple_output_encoder(TestID,_ExampleNum,I,O,II,OO),
  once(I\==II;O\==OO), asserta(is_encode_tst(TestID)).
     
test_encoders(TestID):- var(TestID),!,simp_encode(TestID),test_encoders(TestID).
test_encoders(TestID):- ensure_test(TestID),
  forall(simple_output_encoder(TestID,ExampleNums,Ins,Outs,ReEncIn,ReEncOut,EIns,EOuts),
    show_encoder_outputs(TestID,ExampleNums,Ins,Outs,ReEncIn,ReEncOut,EIns,EOuts)).

test_encoder_for_simple_output(TestID):- var(TestID),!,simp_encode(TestID),test_encoder_for_simple_output(TestID).
test_encoder_for_simple_output(TestID):- ensure_test(TestID),
  once((get_raw_input_outputs(TestID,ExampleNums,Ins,Outs),
     % [In|_] = Ins, [Out|_] = Outs,

        once((simple_reencode(ReEncIn,Ins,EIns),
         (ReEncIn==[] -> print_ss("No Common Input encoder for"=TestID) ; 
           (pp(TestID=ExampleNums), print_ss(ins=Ins), pp(reEncIn=ReEncIn), print_ss(eins=EIns))))),

        once((simple_reencode(ReEncOut,Outs,EOuts),
         (ReEncOut==[] -> print_ss("No Common Output encoder for"=TestID) ; 
           (pp(TestID=ExampleNums), print_ss(outs=Outs), pp(reEncOut=ReEncOut), print_ss(eouts=EOuts))))),

    if_t((ReEncIn\==[];ReEncOut\==[]), maplist(show_single_pair(TestID),ExampleNums,EIns,EOuts)))).


show_single_pair(TestID,ExampleNum,In,Out):-
   as_d_grid(In,In1),as_d_grid(Out,Out1),   
   xfer_zeros(In1,Out1), easy_diff_idea(TestID,ExampleNum,In1,Out1,LIST),
   print_side_by_side(LIST),
   format('~N').

show_encoder_outputs(TestID,ExampleNums,Ins,Outs,ReEncIn,ReEncOut,EIns,EOuts):-
   pp(TestID=ExampleNums), 
   print_ss(outs=Outs), pp(reEncOut=ReEncOut), print_ss(eouts=EOuts),
   print_ss(ins=Ins),   pp(reEncIn=ReEncIn), print_ss(eins=EIns),!.
  
with_std_encoder(Goal):- with_encoder(simple_output_encoder,Goal).

:- dynamic(task_pre_encoder/3).
  

simple_output_encoder(TestID,ExampleNum,II,OO):-
  simple_output_encoder(TestID,ExampleNum,_,_,II,OO).

simple_output_encoder(TestID,ExampleNum,I,O,II,OO):-
  simple_output_encoder(TestID,ExampleNums,Ins,Outs,_ReEncIn,_ReEncOut,EIns,EOuts),
  nth0(N,ExampleNums,ExampleNum),
  nth0(N,Ins,I), nth0(N,Outs,O),
  nth0(N,EIns,II), nth0(N,EOuts,OO).

simple_output_encoder(TestID,ExampleNums,Ins,Outs,ReEncIn,ReEncOut,EIns,EOuts):-
  (var(ExampleNums);var(Ins);var(Outs)),!,
  get_raw_input_outputs(TestID,ExampleNums,Ins,Outs),
  simple_output_encoder(TestID,ExampleNums,Ins,Outs,ReEncIn,ReEncOut,EIns,EOuts).

simple_output_encoder(TestID,ExampleNums,Ins,Outs,ReEncIn,ReEncOut,EIns,EOuts):-
  get_raw_input_outputs(TestID,ExampleNums,Ins,Outs),
  (task_pre_encoder(TestID,ReEncIn,ReEncOut)
     *-> (simple_reencode(ReEncIn,Ins,EIns), simple_reencode(ReEncOut,Outs,EOuts))
       ; (simple_reencode(Ins,ReEncIn,EIns), simple_reencode(Outs,ReEncOut,EOuts), 
                 asserta(task_pre_encoder(TestID,ReEncIn,ReEncOut)))).

%:- meta_predicate(simple_reencode(//)).
simple_reencode(P2s,Ins,EIns):- var(P2s),!,all_common_reductions(Ins,P2s,EIns).
simple_reencode(P2s,Ins,EIns):- reduce_grid(Ins,P2s,EIns).


