--# -path=.:prelude:resource-1.0/abstract:resource-1.0/common

incomplete concrete GodisSystemI of GodisSystem = PredefCnc ** 
    open Prelude, Grammar, GodisLang, ConstructX, ParamX in {

lincat

Proposition = Cl ** ClauseForm;

ShortAns    = NP;

Move,
QMove       = Utt;

ListMove,
S           = Text;

ListYNQ, 
Question    = QCl ** ClauseForm;

Action      = VP ** ClauseForm;

Reason      = S;

lin


whqAction  = isDoing **
    QuestSlash whatSg_IP (AdvSlash (SlashVVV2 (UsePron i_Pron) can_VV do_V2)
			      (PrepNP for_Prep you_NP));
whqIssue   = isDoing ** 
    QuestCl (PredVP you_NP (ComplVV want_VV
				(ComplV2 have_V2 (DetCN someSg_Det (UseN information_N)))));

ynq     p    = clauseForm p ** QuestCl p;

ynqBase p    = clauseForm p ** QuestCl p;
ynqCons p qs = clauseForm p ** disjunct_QCl (QuestCl p) qs;
altq      qs = qs; 

not      p = p ** negate_Cl p;
fail   q r = hasDone ** 
    negate_Cl (PredVP (UsePron i_Pron) 
		   (AdvVP (ComplVQ know_VQ (UseQCl TPres (anter q) PPos q)) 
			(SubjS because_Subj r)));
done     a = clauseForm a ** PredVP (UsePron i_Pron) (VPing a);
issue    q = isDoing ** PredVP you_NP (ComplVQ wonder_VQ (useQCl q));
action   a = isDoing ** PredVP you_NP (ComplVV want_VV a);

notS     a = PredetNP not_Predet a;

answer_yes = yes;
answer_no  = no;

greet      = hello;
quit       = goodbye;
shortAns a = UttNP a;
request  a = UttImpSg PPos (ImpVP a);

ask      q = UttQS (useQCl q);

answer   p = UttS (useCl p);

confirm  a = UttS (useCl (clauseForm a ** PredVP (UsePron i_Pron) (VPing a)));

reportFailure a r = 
    UttS (UseCl TPast ASimul PPos 
	      (PredVP (UsePron i_Pron) 
		   (AdvVP (ComplVV fail_VV a) (SubjS because_Subj r))));


icm_acc_pos       = icm_acc_pos;
icm_con_neg       = icm_con_neg;
icm_per_neg       = UttQS (UseQCl TPast ASimul PPos
			       (QuestSlash whatSg_IP
				    (SlashV2 you_NP (UseVS say_VS))));

icm_sem_neg       = i_dont_understand;
icm_und_neg       = i_dont_understand;

icm_reraise       = icm_reraise;
icm_loadplan      = icm_loadplan;
icm_accommodate   = icm_accommodate;

icm_per_pos           x = icm_per_pos x;
icm_sem_pos_prop      p = UttS (useCl p);
icm_sem_pos_shortAns  a = UttNP a;
icm_und_pos           p = UttS (useCl p);
icm_und_int           p = mkUtt ((UttS (useCl p)).s ++ is_that_correct_Post.s);
icm_reraise_act       a = vp2Utt (VPing a);
icm_reraise_que       q = mkUtt (returning_to_Pre.s ++
				     (UttAdv (AdvSC (EmbedQS (useQCl q)))).s);
icm_accommodate_que   q = icm_accommodate;
icm_reaccommodate_que q = mkUtt (returning_to_issue_Pre.s ++
				     (UttAdv (AdvSC (EmbedQS (useQCl q)))).s);


BaseMove m = TFullStop (PhrUtt NoPConj m NoVoc) TEmpty;
ConsMove m = TFullStop (PhrUtt NoPConj m NoVoc);
BaseQMove q = TQuestMark (PhrUtt NoPConj q NoVoc) TEmpty;
ConsQMove q = TQuestMark (PhrUtt NoPConj q NoVoc);

godis_utterance x = x;


}
