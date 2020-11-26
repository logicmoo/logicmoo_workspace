/*
   Copyright (c) 1995 - 2006 Cycorp, Inc.  All rights reserved.
   file:        doom3.c 
   created:     2006/06/05 11:36:07
   source file: CynD 
 */

#include "rtl.h"

#ifdef CYCL_PROFILING
#define CynD_PROFILING
#endif

#ifdef CynD_PROFILING
#define PROFILING
#endif

#include "code-flow-hooks.h"

/* referenced globals */
extern Dp gv_gameeval;
extern Dp gv_gameinstance;
extern Dp gv_gameproperty;
extern Dp gv_retval;
extern Dp gv_stream;

/* referenced functions */
extern Dp f_bq_cons(Dp v_req_0, Dp v_req_1);
extern Dp f_c_backend_output_file_and_header_file(Dp v_req_0, Dp v_req_1);
extern Dp f_cyc_assert(Dp v_req_0, Dp v_opt_1, Dp v_opt_2);
extern Dp f_cyc_game_eval_stub(Dp v_outval);
extern Dp f_find_constant(Dp v_req_0);
extern Dp f_find_or_create_constant(Dp v_req_0, Dp v_opt_1);
extern Dp f_inference_removal_module(Dp v_req_0, Dp v_req_1);
extern Dp f_meakql(Dp v_v1, Dp v_v2);
extern Dp f_reader_make_constant_shell(Dp v_req_0);
extern Dp f_register_solely_specific_removal_module_predicate(Dp v_req_0);
extern Dp f_removal_gameapi_unify_generate(Dp v_value);
extern Dp f_removal_gameinstance_unify_generate(Dp v_value);
extern Dp f_removal_gameproperty_unify_generate(Dp v_value);
extern Dp f_show_trans_subl_file(Dp v_req_0, Dp v_opt_1);
extern Dp f_translate_file(Dp v_req_0, Dp v_req_1);
extern Dp f_weakql(Dp v_v1, Dp v_v2);

static Dp _constants[76];  /* container for all constant values in file */

Dp gv_gameeval;

Dp gv_gameproperty;

Dp gv_gameinstance;

Dp f_weakql(Dp v_v1, Dp v_v2) {
  ENTER();
  RETURN(BTD((eql(v_v1, v_v2)
        || (numberp(v_v1)
           && numberp(v_v2)
           && numE(v_v1, v_v2))
        || (stringp(v_v1)
           && ((stringp(v_v2)
               && string_equal(v_v1, v_v2, DUM, DUM, DUM, DUM))
            || (DTB(v_v2)
               && eql(f_find_constant(v_v1), v_v2))))
        || (stringp(v_v2)
           && DTB(v_v1)
           && eql(f_find_constant(v_v2), v_v1)))));
}

Dp f_meakql(Dp v_v1, Dp v_v2) {
  ENTER();
  RETURN(BTD((DTB(f_weakql(v_v1, v_v2))
        || (consp(v_v2)
           && DTB(f_weakql(v_v1, car(v_v2))))
        || (consp(v_v1)
           && DTB(f_weakql(v_v2, car(v_v1)))))));
}

Dp f_removal_gameapi_pos_check(Dp v_values) {
  ENTER();
  RETURN(f_meakql(f_removal_gameapi_unify_generate(first(v_values)), second(v_values)));
}

Dp f_removal_gameapi_neg_check(Dp v_values) {
  ENTER();
  RETURN(BTD((!(DTB(f_meakql(f_removal_gameapi_unify_generate(first(v_values)), second(v_values)))))));
}

Dp f_removal_gameapi_unify_generate(Dp v_value) {
  ENTER();
  RETURN(f_cyc_game_eval_stub(v_value));
}

Dp f_removal_gameproperty_unify_generate(Dp v_value) {
  ENTER();
  RETURN(f_cyc_game_eval_stub(list(two, _constants[3], v_value)));
}

Dp f_removal_gameproperty_pos_check(Dp v_values) {
  ENTER();
  RETURN(f_meakql(f_removal_gameproperty_unify_generate(first(v_values)), second(v_values)));
}

Dp f_removal_gameinstance_unify_generate(Dp v_value) {
  ENTER();
  RETURN(f_cyc_game_eval_stub(list(two, _constants[4], v_value)));
}

Dp f_removal_gameinstance_pos_check(Dp v_values) {
  ENTER();
  RETURN(f_meakql(f_removal_gameinstance_unify_generate(first(v_values)), second(v_values)));
}

Dp f_removal_gameinstance_unify_unbound(Dp v_values) {
  ENTER();
  RETURN(f_cyc_game_eval_stub(list(two, _constants[4], v_values)));
}

Dp f_cyc_game_eval_stub(Dp v_outval) {
  ENTER();
  RETURN({
      BIND(gv_retval, NIL);
      BIND(gv_stream, open_tcp_stream(_constants[5], _constants[6]));
      prin1(v_outval, DYN(gv_stream));
      terpri(DYN(gv_stream));
      force_output(DYN(gv_stream));
      SET_DYN(gv_retval, sublisp_read(DYN(gv_stream), DUM, DUM, DUM));
      sublisp_close(DYN(gv_stream), DUM);
      DYN(gv_retval);
      REBIND(gv_stream);
      REBIND(gv_retval);
    });
}

Dp f_setup_doom3_ke(void) {
  ENTER();
  f_cyc_assert(listS(three, _constants[7], DYN(gv_gameeval), _constants[8]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[7], DYN(gv_gameeval), _constants[10]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[11], DYN(gv_gameeval), _constants[12]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[7], DYN(gv_gameeval), _constants[13]), _constants[14], DUM);
  f_cyc_assert(listS(three, _constants[15], DYN(gv_gameeval), _constants[16]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[17], DYN(gv_gameeval), _constants[18]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[19], DYN(gv_gameeval), _constants[18]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[7], DYN(gv_gameproperty), _constants[8]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[7], DYN(gv_gameproperty), _constants[10]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[11], DYN(gv_gameproperty), _constants[20]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[7], DYN(gv_gameproperty), _constants[13]), _constants[14], DUM);
  f_cyc_assert(listS(three, _constants[15], DYN(gv_gameproperty), _constants[16]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[17], DYN(gv_gameproperty), _constants[18]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[19], DYN(gv_gameproperty), _constants[18]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[7], DYN(gv_gameinstance), _constants[8]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[7], DYN(gv_gameinstance), _constants[10]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[11], DYN(gv_gameinstance), _constants[21]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[7], DYN(gv_gameinstance), _constants[13]), _constants[14], DUM);
  f_cyc_assert(listS(three, _constants[15], DYN(gv_gameinstance), _constants[16]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[17], DYN(gv_gameinstance), _constants[18]), _constants[9], DUM);
  f_cyc_assert(listS(three, _constants[19], DYN(gv_gameinstance), _constants[18]), _constants[9], DUM);
  RETURN(NIL);
}

Dp f_setup_doom3_removals(void) {
  ENTER();
  f_inference_removal_module(_constants[22], listS(nineteen, _constants[23], _constants[24], _constants[25], DYN(gv_gameeval), _constants[26], f_bq_cons(DYN(gv_gameeval), _constants[27]), _constants[28], zero, _constants[29], _constants[30], _constants[31], listS(three, _constants[32], f_bq_cons(DYN(gv_gameeval), _constants[33]), _constants[34]), _constants[35], _constants[36], _constants[37], _constants[38], _constants[39], f_bq_cons(DYN(gv_gameeval), _constants[40]), _constants[41]));
  f_inference_removal_module(_constants[42], listS(fifteen, _constants[23], _constants[24], _constants[25], DYN(gv_gameeval), _constants[43], T, _constants[26], f_bq_cons(DYN(gv_gameeval), _constants[44]), _constants[28], zero, _constants[29], _constants[30], _constants[31], listS(three, _constants[32], f_bq_cons(DYN(gv_gameeval), _constants[45]), _constants[46]), _constants[47]));
  f_register_solely_specific_removal_module_predicate(DYN(gv_gameeval));
  f_inference_removal_module(_constants[48], listS(nineteen, _constants[23], _constants[24], _constants[25], DYN(gv_gameproperty), _constants[26], f_bq_cons(DYN(gv_gameproperty), _constants[27]), _constants[28], zero, _constants[29], _constants[30], _constants[31], listS(three, _constants[32], f_bq_cons(DYN(gv_gameproperty), _constants[33]), _constants[34]), _constants[35], _constants[36], _constants[37], _constants[49], _constants[39], f_bq_cons(DYN(gv_gameproperty), _constants[40]), _constants[50]));
  f_inference_removal_module(_constants[51], listS(fifteen, _constants[23], _constants[24], _constants[25], DYN(gv_gameproperty), _constants[43], T, _constants[26], f_bq_cons(DYN(gv_gameproperty), _constants[44]), _constants[28], zero, _constants[29], _constants[30], _constants[31], listS(three, _constants[32], f_bq_cons(DYN(gv_gameproperty), _constants[45]), _constants[46]), _constants[52]));
  f_register_solely_specific_removal_module_predicate(DYN(gv_gameproperty));
  f_inference_removal_module(_constants[53], listS(nineteen, _constants[23], _constants[24], _constants[25], DYN(gv_gameinstance), _constants[26], f_bq_cons(DYN(gv_gameinstance), _constants[27]), _constants[28], zero, _constants[29], _constants[30], _constants[31], listS(three, _constants[32], f_bq_cons(DYN(gv_gameinstance), _constants[33]), _constants[34]), _constants[35], _constants[36], _constants[37], _constants[54], _constants[39], f_bq_cons(DYN(gv_gameinstance), _constants[40]), _constants[55]));
  f_inference_removal_module(_constants[56], listS(nineteen, _constants[23], _constants[24], _constants[25], DYN(gv_gameinstance), _constants[26], f_bq_cons(DYN(gv_gameinstance), _constants[57]), _constants[28], zero, _constants[29], _constants[30], _constants[31], listS(three, _constants[32], f_bq_cons(DYN(gv_gameinstance), _constants[45]), _constants[46]), _constants[35], _constants[58], _constants[37], _constants[59], _constants[39], f_bq_cons(DYN(gv_gameinstance), _constants[60]), _constants[61]));
  f_inference_removal_module(_constants[62], listS(fifteen, _constants[23], _constants[24], _constants[25], DYN(gv_gameinstance), _constants[43], T, _constants[26], f_bq_cons(DYN(gv_gameinstance), _constants[44]), _constants[28], zero, _constants[29], _constants[30], _constants[31], listS(three, _constants[32], f_bq_cons(DYN(gv_gameinstance), _constants[45]), _constants[46]), _constants[63]));
  f_register_solely_specific_removal_module_predicate(DYN(gv_gameinstance));
  f_inference_removal_module(_constants[42], listS(fifteen, _constants[23], _constants[64], _constants[25], DYN(gv_gameeval), _constants[43], T, _constants[26], f_bq_cons(DYN(gv_gameeval), _constants[44]), _constants[28], zero, _constants[29], _constants[30], _constants[31], listS(three, _constants[32], f_bq_cons(DYN(gv_gameeval), _constants[45]), _constants[46]), _constants[65]));
  f_inference_removal_module(_constants[66], listS(nineteen, _constants[23], _constants[24], _constants[25], DYN(gv_gameeval), _constants[26], f_bq_cons(DYN(gv_gameeval), _constants[67]), _constants[28], zero, _constants[29], _constants[30], _constants[31], listS(three, _constants[32], f_bq_cons(DYN(gv_gameeval), _constants[68]), _constants[34]), _constants[35], _constants[36], _constants[37], _constants[38], _constants[39], f_bq_cons(DYN(gv_gameeval), _constants[69]), _constants[70]));
  RETURN(NIL);
}

Dp f_transd3(void) {
  ENTER();
  {
    Dp v_ts_file = f_translate_file(_constants[71], _constants[72]);
    Dp v_fout = open_text(_constants[73], _constants[74]);
    f_show_trans_subl_file(v_ts_file, v_fout);
    sublisp_close(v_fout, DUM);
    f_c_backend_output_file_and_header_file(v_ts_file, _constants[75]);
    RETURN(v_ts_file);
  }
  RETURN(NIL);
}

Dp f_declare_cynd_file(void) {
  ENTER();
  DEFINE(f_weakql, "WEAKQL", 2, 0, FALSE);
  DEFINE(f_meakql, "MEAKQL", 2, 0, FALSE);
  DEFINE(f_removal_gameapi_pos_check, "REMOVAL-GAMEAPI-POS-CHECK", 1, 0, FALSE);
  DEFINE(f_removal_gameapi_neg_check, "REMOVAL-GAMEAPI-NEG-CHECK", 1, 0, FALSE);
  DEFINE(f_removal_gameapi_unify_generate, "REMOVAL-GAMEAPI-UNIFY-GENERATE", 1, 0, FALSE);
  DEFINE(f_removal_gameproperty_unify_generate, "REMOVAL-GAMEPROPERTY-UNIFY-GENERATE", 1, 0, FALSE);
  DEFINE(f_removal_gameproperty_pos_check, "REMOVAL-GAMEPROPERTY-POS-CHECK", 1, 0, FALSE);
  DEFINE(f_removal_gameinstance_unify_generate, "REMOVAL-GAMEINSTANCE-UNIFY-GENERATE", 1, 0, FALSE);
  DEFINE(f_removal_gameinstance_pos_check, "REMOVAL-GAMEINSTANCE-POS-CHECK", 1, 0, FALSE);
  DEFINE(f_removal_gameinstance_unify_unbound, "REMOVAL-GAMEINSTANCE-UNIFY-UNBOUND", 1, 0, FALSE);
  DEFINE(f_cyc_game_eval_stub, "CYC-GAME-EVAL-STUB", 1, 0, FALSE);
  DEFINE(f_setup_doom3_ke, "SETUP-DOOM3-KE", 0, 0, FALSE);
  DEFINE(f_setup_doom3_removals, "SETUP-DOOM3-REMOVALS", 0, 0, FALSE);
  DEFINE(f_transd3, "TRANSD3", 0, 0, FALSE);
  RETURN(NIL);
}

Dp f_init_cynd_file(void) {
  ENTER();
  INITIALIZE_CONSTANT(_constants[0], CSTRING("doom:gameEval"));
  INITIALIZE_CONSTANT(_constants[1], CSTRING("doom:gameProperty"));
  INITIALIZE_CONSTANT(_constants[2], CSTRING("doom:gameInstance"));
  INITIALIZE_CONSTANT(_constants[3], CSTRING("gameProperty"));
  INITIALIZE_CONSTANT(_constants[4], CSTRING("gameInstance"));
  INITIALIZE_CONSTANT(_constants[5], CSTRING("10.1.1.104"));
  INITIALIZE_CONSTANT(_constants[6], CFIX(3699));
  INITIALIZE_CONSTANT(_constants[7], f_reader_make_constant_shell(CSTRING("isa")));
  INITIALIZE_CONSTANT(_constants[8], list(one, f_reader_make_constant_shell(CSTRING("AsymmetricBinaryPredicate"))));
  INITIALIZE_CONSTANT(_constants[9], f_reader_make_constant_shell(CSTRING("UniversalVocabularyMt")));
  INITIALIZE_CONSTANT(_constants[10], list(one, f_reader_make_constant_shell(CSTRING("IntangibleObjectPredicate"))));
  INITIALIZE_CONSTANT(_constants[11], f_reader_make_constant_shell(CSTRING("comment")));
  INITIALIZE_CONSTANT(_constants[12], list(one, CSTRING("(,*gameEval* (#$TheList .....) ?Result)")));
  INITIALIZE_CONSTANT(_constants[13], list(one, f_reader_make_constant_shell(CSTRING("RemovalModuleSupportedPredicate-Specific"))));
  INITIALIZE_CONSTANT(_constants[14], f_reader_make_constant_shell(CSTRING("CycAPIMt")));
  INITIALIZE_CONSTANT(_constants[15], f_reader_make_constant_shell(CSTRING("arity")));
  INITIALIZE_CONSTANT(_constants[16], list(one, two));
  INITIALIZE_CONSTANT(_constants[17], f_reader_make_constant_shell(CSTRING("arg1Isa")));
  INITIALIZE_CONSTANT(_constants[18], list(one, f_reader_make_constant_shell(CSTRING("Thing"))));
  INITIALIZE_CONSTANT(_constants[19], f_reader_make_constant_shell(CSTRING("arg2Isa")));
  INITIALIZE_CONSTANT(_constants[20], list(one, CSTRING("(,*gameProperty* ?Instance ?Result)")));
  INITIALIZE_CONSTANT(_constants[21], list(one, CSTRING("(,*gameInstance* ?Instance ?Result)")));
  INITIALIZE_CONSTANT(_constants[22], CKEYWORD("REMOVAL-GAMEAPI-DOOM-BOUND-UNBOUND"));
  INITIALIZE_CONSTANT(_constants[23], CKEYWORD("SENSE"));
  INITIALIZE_CONSTANT(_constants[24], CKEYWORD("POS"));
  INITIALIZE_CONSTANT(_constants[25], CKEYWORD("PREDICATE"));
  INITIALIZE_CONSTANT(_constants[26], CKEYWORD("REQUIRED-PATTERN"));
  INITIALIZE_CONSTANT(_constants[27], list(two, CKEYWORD("FULLY-BOUND"), CKEYWORD("NOT-FULLY-BOUND")));
  INITIALIZE_CONSTANT(_constants[28], CKEYWORD("COST-EXPRESSION"));
  INITIALIZE_CONSTANT(_constants[29], CKEYWORD("COMPLETENESS"));
  INITIALIZE_CONSTANT(_constants[30], CKEYWORD("COMPLETE"));
  INITIALIZE_CONSTANT(_constants[31], CKEYWORD("INPUT-EXTRACT-PATTERN"));
  INITIALIZE_CONSTANT(_constants[32], CKEYWORD("TEMPLATE"));
  INITIALIZE_CONSTANT(_constants[33], list(two, list(two, CKEYWORD("BIND"), CINTERN("THE-VALUE")), CKEYWORD("ANYTHING")));
  INITIALIZE_CONSTANT(_constants[34], list(one, list(two, CKEYWORD("VALUE"), CINTERN("THE-VALUE"))));
  INITIALIZE_CONSTANT(_constants[35], CKEYWORD("INPUT-VERIFY-PATTERN"));
  INITIALIZE_CONSTANT(_constants[36], CKEYWORD("ANYTHING"));
  INITIALIZE_CONSTANT(_constants[37], CKEYWORD("OUTPUT-GENERATE-PATTERN"));
  INITIALIZE_CONSTANT(_constants[38], list(three, CKEYWORD("CALL"), CINTERN("REMOVAL-GAMEAPI-UNIFY-GENERATE"), CKEYWORD("INPUT")));
  INITIALIZE_CONSTANT(_constants[39], CKEYWORD("OUTPUT-CONSTRUCT-PATTERN"));
  INITIALIZE_CONSTANT(_constants[40], list(two, list(two, CKEYWORD("VALUE"), CINTERN("THE-VALUE")), CKEYWORD("INPUT")));
  INITIALIZE_CONSTANT(_constants[41], list(four, CKEYWORD("DOCUMENTATION"), CSTRING("(,*gameEval* <fully-bound> <not-fully-bound>)"), CKEYWORD("EXAMPLE"), CSTRING("(,*gameEval* -1 ?WHAT)")));
  INITIALIZE_CONSTANT(_constants[42], CKEYWORD("REMOVAL-GAMEAPI-DOOM-BOUND-BOUND"));
  INITIALIZE_CONSTANT(_constants[43], CKEYWORD("CHECK"));
  INITIALIZE_CONSTANT(_constants[44], list(two, CKEYWORD("FULLY-BOUND"), CKEYWORD("FULLY-BOUND")));
  INITIALIZE_CONSTANT(_constants[45], list(two, list(two, CKEYWORD("BIND"), CINTERN("VALUE-1")), list(two, CKEYWORD("BIND"), CINTERN("VALUE-2"))));
  INITIALIZE_CONSTANT(_constants[46], list(one, list(two, list(two, CKEYWORD("VALUE"), CINTERN("VALUE-1")), list(two, CKEYWORD("VALUE"), CINTERN("VALUE-2")))));
  INITIALIZE_CONSTANT(_constants[47], list(eight, CKEYWORD("INPUT-VERIFY-PATTERN"), list(two, CKEYWORD("ANYTHING"), CKEYWORD("ANYTHING")), CKEYWORD("OUTPUT-CHECK-PATTERN"), list(three, CKEYWORD("CALL"), CINTERN("REMOVAL-GAMEAPI-POS-CHECK"), list(three, CKEYWORD("TUPLE"), list(two, CINTERN("VALUE-1"), CINTERN("VALUE-2")), list(two, list(two, CKEYWORD("VALUE"), CINTERN("VALUE-1")), list(two, CKEYWORD("VALUE"), CINTERN("VALUE-2"))))), CKEYWORD("DOCUMENTATION"), CSTRING("(,*gameEval* <fully-bound> <fully-bound>)"), CKEYWORD("EXAMPLE"), CSTRING("(,*gameEval* 1 -1)")));
  INITIALIZE_CONSTANT(_constants[48], CKEYWORD("REMOVAL-GAMEPROPERTY-DOOM-BOUND-UNBOUND"));
  INITIALIZE_CONSTANT(_constants[49], list(three, CKEYWORD("CALL"), CINTERN("REMOVAL-GAMEPROPERTY-UNIFY-GENERATE"), CKEYWORD("INPUT")));
  INITIALIZE_CONSTANT(_constants[50], list(four, CKEYWORD("DOCUMENTATION"), CSTRING("(,*gameProperty* <fully-bound> <not-fully-bound>)"), CKEYWORD("EXAMPLE"), CSTRING("(,*gameProperty* #doom:cyc_bot_1 ?WHAT)")));
  INITIALIZE_CONSTANT(_constants[51], CKEYWORD("REMOVAL-GAMEPROPERTY-DOOM-BOUND-BOUND"));
  INITIALIZE_CONSTANT(_constants[52], list(eight, CKEYWORD("INPUT-VERIFY-PATTERN"), list(two, CKEYWORD("ANYTHING"), CKEYWORD("ANYTHING")), CKEYWORD("OUTPUT-CHECK-PATTERN"), list(three, CKEYWORD("CALL"), CINTERN("REMOVAL-GAMEPROPERTY-POS-CHECK"), list(three, CKEYWORD("TUPLE"), list(two, CINTERN("VALUE-1"), CINTERN("VALUE-2")), list(two, list(two, CKEYWORD("VALUE"), CINTERN("VALUE-1")), list(two, CKEYWORD("VALUE"), CINTERN("VALUE-2"))))), CKEYWORD("DOCUMENTATION"), CSTRING("(,*gameProperty* <fully-bound> <fully-bound>)"), CKEYWORD("EXAMPLE"), CSTRING("(,*gameProperty* 1 -1)")));
  INITIALIZE_CONSTANT(_constants[53], CKEYWORD("REMOVAL-GAMEINSTANCE-DOOM-BOUND-UNBOUND"));
  INITIALIZE_CONSTANT(_constants[54], list(three, CKEYWORD("CALL"), CINTERN("REMOVAL-GAMEINSTANCE-UNIFY-GENERATE"), CKEYWORD("INPUT")));
  INITIALIZE_CONSTANT(_constants[55], list(four, CKEYWORD("DOCUMENTATION"), CSTRING("(,*gameInstance* <fully-bound> <not-fully-bound>)"), CKEYWORD("EXAMPLE"), CSTRING("(,*gameInstance* #doom:cyc_bot_1 ?WHAT)")));
  INITIALIZE_CONSTANT(_constants[56], CKEYWORD("REMOVAL-GAMEINSTANCE-DOOM-UNBOUND-UNBOUND"));
  INITIALIZE_CONSTANT(_constants[57], list(two, CKEYWORD("NOT-FULLY-BOUND"), CKEYWORD("NOT-FULLY-BOUND")));
  INITIALIZE_CONSTANT(_constants[58], list(two, CKEYWORD("ANYTHING"), CKEYWORD("ANYTHING")));
  INITIALIZE_CONSTANT(_constants[59], list(three, CKEYWORD("CALL"), CINTERN("REMOVAL-GAMEINSTANCE-UNIFY-UNBOUND"), CKEYWORD("INPUT")));
  INITIALIZE_CONSTANT(_constants[60], list(two, list(three, CKEYWORD("CALL"), CINTERN("PRINT"), CKEYWORD("INPUT")), list(three, CKEYWORD("CALL"), CINTERN("PRINT"), CKEYWORD("INPUT"))));
  INITIALIZE_CONSTANT(_constants[61], list(four, CKEYWORD("DOCUMENTATION"), CSTRING("(,*gameInstance* <not-fully-bound> <not-fully-bound>)"), CKEYWORD("EXAMPLE"), CSTRING("(,*gameInstance* #doom:cyc_bot_1 ?WHAT)")));
  INITIALIZE_CONSTANT(_constants[62], CKEYWORD("REMOVAL-GAMEINSTANCE-DOOM-BOUND-BOUND"));
  INITIALIZE_CONSTANT(_constants[63], list(eight, CKEYWORD("INPUT-VERIFY-PATTERN"), list(two, CKEYWORD("ANYTHING"), CKEYWORD("ANYTHING")), CKEYWORD("OUTPUT-CHECK-PATTERN"), list(three, CKEYWORD("CALL"), CINTERN("REMOVAL-GAMEINSTANCE-POS-CHECK"), list(three, CKEYWORD("TUPLE"), list(two, CINTERN("VALUE-1"), CINTERN("VALUE-2")), list(two, list(two, CKEYWORD("VALUE"), CINTERN("VALUE-1")), list(two, CKEYWORD("VALUE"), CINTERN("VALUE-2"))))), CKEYWORD("DOCUMENTATION"), CSTRING("(,*gameInstance* <fully-bound> <fully-bound>)"), CKEYWORD("EXAMPLE"), CSTRING("(,*gameInstance* 1 -1)")));
  INITIALIZE_CONSTANT(_constants[64], CKEYWORD("NEG"));
  INITIALIZE_CONSTANT(_constants[65], list(eight, CKEYWORD("INPUT-VERIFY-PATTERN"), list(two, CKEYWORD("ANYTHING"), CKEYWORD("ANYTHING")), CKEYWORD("OUTPUT-CHECK-PATTERN"), list(three, CKEYWORD("CALL"), CINTERN("REMOVAL-GAMEAPI-NEG-CHECK"), list(three, CKEYWORD("TUPLE"), list(two, CINTERN("VALUE-1"), CINTERN("VALUE-2")), list(two, list(two, CKEYWORD("VALUE"), CINTERN("VALUE-1")), list(two, CKEYWORD("VALUE"), CINTERN("VALUE-2"))))), CKEYWORD("DOCUMENTATION"), CSTRING("(,*gameEval* <fully-bound> <fully-bound>)"), CKEYWORD("EXAMPLE"), CSTRING("(#$not (,*gameEval* 1 -1))")));
  INITIALIZE_CONSTANT(_constants[66], CKEYWORD("REMOVAL-GAMEAPI-DOOM-UNBOUND-BOUND"));
  INITIALIZE_CONSTANT(_constants[67], list(two, CKEYWORD("NOT-FULLY-BOUND"), CKEYWORD("FULLY-BOUND")));
  INITIALIZE_CONSTANT(_constants[68], list(two, CKEYWORD("ANYTHING"), list(two, CKEYWORD("BIND"), CINTERN("THE-VALUE"))));
  INITIALIZE_CONSTANT(_constants[69], list(two, CKEYWORD("INPUT"), list(two, CKEYWORD("VALUE"), CINTERN("THE-VALUE"))));
  INITIALIZE_CONSTANT(_constants[70], list(four, CKEYWORD("DOCUMENTATION"), CSTRING("(,*gameEval* <not-fully-bound> <fully-bound>)"), CKEYWORD("EXAMPLE"), CSTRING("(,*gameEval* ?WHAT -1)")));
  INITIALIZE_CONSTANT(_constants[71], CSTRING("CynD"));
  INITIALIZE_CONSTANT(_constants[72], CSTRING("doom3.lisp"));
  INITIALIZE_CONSTANT(_constants[73], CSTRING("doom3.trans"));
  INITIALIZE_CONSTANT(_constants[74], CKEYWORD("OUTPUT"));
  INITIALIZE_CONSTANT(_constants[75], CSTRING("doom3.c"));
  DEFVAR(gv_gameeval, "*GAMEEVAL*", f_find_or_create_constant(_constants[0], DUM));
  DEFVAR(gv_gameproperty, "*GAMEPROPERTY*", f_find_or_create_constant(_constants[1], DUM));
  DEFVAR(gv_gameinstance, "*GAMEINSTANCE*", f_find_or_create_constant(_constants[2], DUM));
  RETURN(NIL);
}

Dp f_setup_cynd_file(void) {
  ENTER();
  RETURN(NIL);
}
