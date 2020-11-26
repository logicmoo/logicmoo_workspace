--# -path=.:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/english

instance GodisLangEng of GodisLang = 
    open Prelude, PredefCnc, GrammarEng, ParadigmsEng, (ResEng=ResEng), ParamX, ConstructX, 
         (Lex=LangEng), (Irreg=IrregEng) in {

oper

----------------------------------------------------------------------
-- user utterances

i_want_to_Str = variants{"i" ++ variants{"want"; ["would like"]} ++ "to";
                         ["can you"]};
please_Str    = "please";


----------------------------------------------------------------------
-- system utterances

hello           = mkUtt ["Hello"];
goodbye         = mkUtt ["Goodbye"];
yes             = mkUtt ["Yes"];
no              = mkUtt ["No"];

is_that_correct_Post   = mkUtt [", is that correct"];
returning_to_Pre       = mkUtt ["Returning to"];
returning_to_issue_Pre = mkUtt ["Returning to the issue about"];
i_dont_understand      = mkUtt ["I don't quite understand"];

icm_acc_pos     = mkUtt ["Okey"];
icm_con_neg     = mkUtt ["Hello?"];
icm_reraise     = mkUtt ["So ,"];
icm_loadplan    = mkUtt ["Let's see"];
icm_accommodate = mkUtt ["sure"];

icm_per_pos   x = mkUtt (["I thought you said"] ++ x.s);


----------------------------------------------------------------------
-- noun phrases

sing_NP    s = ResEng.regNP s Sg ** {lock_NP = <>};
plur_NP    s = ResEng.regNP s Pl ** {lock_NP = <>};

NPgen_NP = NP_of_NP;

----------------------------------------------------------------------
-- verb phrases, actions

VPing vp    = case vp.clform of {HasDone => vp; IsDoing => ProgrVP vp};
vp2Utt vp   = mkUtt (vp.s2 ! (ResEng.agrP3 Sg));

----------------------------------------------------------------------
-- general syntactical operations

disjunct_QCl q q' = 
    {s = \\t,a,p,x => q.s!t!a!p!x ++ "or" ++ q'.s!t!a!p!x;
     lock_QCl = <>};

negate_Cl c = 
    {s = \\t,a,p,o => c.s!t!a!(case p of {ResEng.CNeg _ => ResEng.CPos; ResEng.CPos => ResEng.CNeg Prelude.True})!o;
     lock_Cl = <>};


----------------------------------------------------------------------
-- verbs

see_V           = Irreg.see_V;

do_V2           = Lex.do_V2;
have_V2         = Lex.have_V2;
understand_V2   = Lex.understand_V2;

know_VQ         = mkVQ Irreg.know_V;
wonder_VQ       = Lex.wonder_VQ;

say_VS          = Lex.say_VS;

fail_VV         = mkVV (regV "fail");

----------------------------------------------------------------------
-- nouns, proper nouns, common nouns and noun phrases

information_N   = regN "information";

you_NP          = UsePron Lex.youSg_Pron;

----------------------------------------------------------------------
-- closed word categories

of_på_Prep      = mkPrep "of";
for_Prep        = mkPrep "for";

not_Predet      = ss "not" ** {lock_Predet = <>};

------------------------------------------------------------------


}
