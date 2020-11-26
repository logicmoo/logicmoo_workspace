--# -path=.:prelude:resource-1.0/abstract:resource-1.0/common

interface GodisLang = open Cat, PredefAbs in {

----------------------------------------------------------------------
-- different clause forms

param ClForm = HasDone | IsDoing;

oper

ClauseForm : Type;
clauseForm : ClauseForm -> ClauseForm;

hasDone : ClauseForm;
isDoing : ClauseForm;

anter : ClauseForm -> Ant;

----------------------------------------------------------------------
-- user utterances

UserQuestion,
UserAction,
UserAnswer,
UserShortAns : Type;

askQS : QCl -> UserQuestion;
ansCl : Cl  -> UserAnswer;
ansNP : NP  -> UserShortAns;

reqVP : VP  -> UserAction;
req1  : Str -> UserAction;
req1x : Str -> Str -> UserAction;
req2x : Str -> Str -> Str -> UserAction;

i_want_to_Str,
please_Str     : Str;


----------------------------------------------------------------------
-- system utterances

hello,
goodbye,
yes,
no,

is_that_correct_Post,
returning_to_Pre,
returning_to_issue_Pre,
i_dont_understand,

icm_acc_pos,
icm_con_neg,
icm_reraise,
icm_loadplan,
icm_accommodate  : Utt;

icm_per_pos : String -> Utt;

----------------------------------------------------------------------
-- interrogative phrases

which_N_sg : N -> IP;
which_N_pl : N -> IP;

----------------------------------------------------------------------
-- noun phrases

sing_NP,
plur_NP    : Str -> NP;

the_N_sg,
the_N_pl,
indef_N_sg,
indef_N_pl,
this_N_sg,
these_N_pl : N -> NP;

the_Asuper_N_sg,
indef_Aposit_N_sg : A -> N -> NP;

NP_Prep_NP : Prep -> NP -> NP -> NP;
NP_of_NP,
NPgen_NP   : NP -> NP -> NP;

----------------------------------------------------------------------
-- questions, q-clauses

useQCl : (QCl ** ClauseForm) -> QS;

which_N_are_AP,
which_N_is_AP             : N -> AP -> QCl;
what_is_NP                : NP -> QCl;
who_VP                    : VP -> QCl;

which_N_do_you_want_to_V2 : N -> V2 -> QCl;
which_N_has_NP_V2         : N -> NP -> V2 -> QCl;

----------------------------------------------------------------------
-- clauses, sentences, answers, propositions

useCl : (Cl ** ClauseForm) -> S;

you_want_to_VP,
you_are_VPing    : VP -> Cl;
you_VV_to_VP     : VV -> VP -> Cl;
it_is_NP_who_VP  : NP -> (VP ** ClauseForm) -> Cl;

----------------------------------------------------------------------
-- relative clauses

useRCl : (RCl ** ClauseForm) -> RS;

----------------------------------------------------------------------
-- verb phrases, actions

V2_the_N : V2 -> N -> VP;
V2_a_N   : V2 -> N -> VP;
VPing    : (VP ** ClauseForm) -> VP;
vp2Utt   : VP -> Utt;

----------------------------------------------------------------------
-- general syntactical operations

disjunct_QCl : QCl -> QCl -> QCl;
negate_Cl    : Cl -> Cl;


----------------------------------------------------------------------
-- verbs

see_V : V;

do_V2,
have_V2,
understand_V2 : V2;

know_VQ,
wonder_VQ : VQ;

say_VS : VS;

fail_VV  : VV;

----------------------------------------------------------------------
-- nouns, proper nouns, common nouns and noun phrases

information_N : N;

you_NP : NP;

----------------------------------------------------------------------
-- closed word categories

of_på_Prep : Prep;

not_Predet : Predet;



----------------------------------------------------------------------
-- language independent implementations
----------------------------------------------------------------------

oper

----------------------------------------------------------------------
-- different clause forms

ClauseForm = {clform : ClForm};
clauseForm c = c;

hasDone = {clform = HasDone};
isDoing = {clform = IsDoing};

anter c = case c.clform of {HasDone => AAnter; IsDoing => ASimul};

----------------------------------------------------------------------
-- user utterances

UserAction,
UserQuestion,
UserAnswer,
UserShortAns = SS;

askQS q = UttQS (UseQCl TPres ASimul PPos q);
ansCl c = UttS (UseCl TPres ASimul PPos c);
ansNP a = UttNP a;

reqVP vp =
    PhrUtt NoPConj
    (variants{ UttImpSg PPos (ImpVP vp);
	       UttS (UseCl (variants{TPres;TCond}) ASimul PPos
			 (PredVP (UsePron i_Pron) (ComplVV want_VV vp))) })
    (variants{ NoVoc; please_Voc });

req1  act = req1x act [];
req1x act = req2x act act;
req2x imp inf extra =
    ss (variants{ imp; i_want_to_Str ++ inf } ++ extra ++ optStr please_Str);

----------------------------------------------------------------------
-- interrogative phrases

which_N_sg n = IDetCN whichSg_IDet NoNum NoOrd (UseN n);
which_N_pl n = IDetCN whichPl_IDet NoNum NoOrd (UseN n);

----------------------------------------------------------------------
-- noun phrases

the_N_sg   n = DetCN (DetSg (SgQuant DefArt) NoOrd) (UseN n);
the_N_pl   n = DetCN (DetPl (PlQuant DefArt) NoNum NoOrd) (UseN n);
indef_N_sg n = DetCN (DetSg (SgQuant IndefArt) NoOrd) (UseN n);
indef_N_pl n = DetCN (DetPl (PlQuant IndefArt) NoNum NoOrd) (UseN n);
this_N_sg  n = DetCN (DetSg (SgQuant this_Quant) NoOrd) (UseN n);
these_N_pl n = DetCN (DetPl (PlQuant this_Quant) NoNum NoOrd) (UseN n);

the_Asuper_N_sg   a n = DetCN (DetSg (SgQuant DefArt) (OrdSuperl a)) (UseN n);
indef_Aposit_N_sg a n = DetCN (DetSg (SgQuant IndefArt) NoOrd) (AdjCN (PositA a) (UseN n));

NP_Prep_NP prep np np' = AdvNP np (PrepNP prep np');

NP_of_NP = NP_Prep_NP of_på_Prep;

----------------------------------------------------------------------
-- questions

useQCl q = UseQCl TPres (anter q) PPos q;

which_N_are_AP n ap = QuestVP (which_N_pl n) (UseComp (CompAP ap));
which_N_is_AP  n ap = QuestVP (which_N_sg n) (UseComp (CompAP ap));
what_is_NP       np = QuestVP whatSg_IP (UseComp (CompNP np));
who_VP           vp = QuestVP whoSg_IP vp;

which_N_do_you_want_to_V2 n v2 = QuestSlash (which_N_sg n) (SlashVVV2 you_NP want_VV v2);
which_N_has_NP_V2      n np v2 = QuestSlash (which_N_pl n) (SlashV2 np v2);

----------------------------------------------------------------------
-- clauses, sentences, answers, propositions

useCl c = UseCl TPres (anter c) PPos c;

you_want_to_VP     vp = PredVP you_NP (ComplVV want_VV vp);
you_are_VPing      vp = PredVP you_NP (VPing (isDoing ** vp));
you_VV_to_VP    vv vp = PredVP you_NP (ComplVV vv vp);
it_is_NP_who_VP np vp = CleftNP np (UseRCl TPres (anter vp) PPos (RelVP IdRP vp));

----------------------------------------------------------------------
-- relative clauses

useRCl r = UseRCl TPres (anter r) PPos r;

----------------------------------------------------------------------
-- verb phrases, actions

V2_the_N v2 n = ComplV2 v2 (the_N_sg n);
V2_a_N   v2 n = ComplV2 v2 (indef_N_sg n);

}
