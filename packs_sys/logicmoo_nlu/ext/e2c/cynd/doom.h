/*
   Copyright (c) 1995 - 2006 Cycorp, Inc.  All rights reserved.
   file:        doom3.h 
   created:     2006/06/05 11:36:07
   source file: CynD 
 */

extern Dp f_weakql(Dp v_v1, Dp v_v2);
extern Dp f_meakql(Dp v_v1, Dp v_v2);
extern Dp f_removal_gameapi_pos_check(Dp v_values);
extern Dp f_removal_gameapi_neg_check(Dp v_values);
extern Dp f_removal_gameapi_unify_generate(Dp v_value);
extern Dp f_removal_gameproperty_unify_generate(Dp v_value);
extern Dp f_removal_gameproperty_pos_check(Dp v_values);
extern Dp f_removal_gameinstance_unify_generate(Dp v_value);
extern Dp f_removal_gameinstance_pos_check(Dp v_values);
extern Dp f_removal_gameinstance_unify_unbound(Dp v_values);
extern Dp f_cyc_game_eval_stub(Dp v_outval);
extern Dp f_setup_doom3_ke(void);
extern Dp f_setup_doom3_removals(void);
extern Dp f_transd3(void);
extern Dp f_declare_cynd_file(void);
extern Dp f_init_cynd_file(void);
extern Dp f_setup_cynd_file(void);
