--# -path=.:prelude

concrete GodisSystemSem of GodisSystem = PredefCnc ** open Prolog in {

lincat

Action, 
Reason, 
Proposition, 
ShortAns, 
Question, 
ListYNQ, 
Move,
QMove,
ListMove,
S         = PStr;


lin

not    = pp1 "not";
fail   = pp2 "fail";
done   = pp1 "done";
issue  = pp1 "issue";
action = pp1 "action";

answer_yes = pp1 "answer" (pp0 "yes");
answer_no  = pp1 "answer" (pp0 "no");
notS       = pp1 "not";

whqAction = pWhQ "action";
whqIssue  = pWhQ "issue";

ynq     q = q;
ynqBase q = q;
ynqCons q = pSeq q;
altq   qs = pp1 "set" (pBrackets qs);

greet     = pp0 "greet";
quit      = pp0 "quit";
ask       = pp1 "ask";
answer    = pp1 "answer";
shortAns  = pp1 "answer";

request   = pp1 "request";
confirm a = variants{ pp1 "confirm" a; 
		      pp2 "report" a (pp0 "done") };
reportFailure a r = pp2 "report" a (pp1 "failed" r);

icm_acc_pos       = icmFeedback0 "acc" "pos";
icm_con_neg       = icmFeedback0 "con" "neg";
icm_per_neg       = icmFeedback0 "per" "neg";
icm_sem_neg       = icmFeedback0 "sem" "neg";
icm_und_neg       = icmFeedback0 "und" "neg";
icm_reraise       = icmSingle0 "reraise";
icm_loadplan      = icmSingle0 "loadplan";
icm_accommodate   = icmSingle0 "accomodate";

icm_per_pos           = icmFeedback1 "per" "pos";
icm_sem_pos_prop      = icmFeedback1 "sem" "pos";
icm_sem_pos_shortAns  = icmFeedback1 "sem" "pos";
icm_und_pos         p = icmFeedback1 "und" "pos" (pOper "*" (pp0 "usr") p);
icm_und_int         p = icmFeedback1 "und" "int" (pOper "*" (pp0 "usr") p);
icm_reraise_act       = icmSingle1 "reraise";
icm_reraise_que       = icmSingle1 "reraise";
icm_accommodate_que   = icmSingle1 "accomodate";
icm_reaccommodate_que = icmSingle1 "reaccomodate";

BaseMove m = m;
ConsMove m = pSeq m;
BaseQMove q = q;
ConsQMove q = pSeq q;

godis_utterance = pBrackets;


oper

icmSingle0   : Str -> PStr  = \icm -> pStr ("icm" ++ ":" ++ icm);
icmSingle1   : Str -> PPStr = \icm -> pOper ":" (icmSingle0 icm);

icmFeedback0 : Str -> Str -> PStr  = \lvl,pol -> icmSingle0 (lvl ++ "*" ++ pol);
icmFeedback1 : Str -> Str -> PPStr = \lvl,pol -> icmSingle1 (lvl ++ "*" ++ pol);


}
