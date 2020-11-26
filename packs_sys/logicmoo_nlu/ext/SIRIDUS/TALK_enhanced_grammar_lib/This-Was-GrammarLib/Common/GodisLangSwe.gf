--# -path=.:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/scandinavian:resource-1.0/swedish

instance GodisLangSwe of GodisLang = 
    open Prelude, PredefCnc, GrammarSwe, CommonScand, ParadigmsSwe, ConstructX,
         (Lex=LangSwe), (Irreg=IrregSwe) in {

oper

----------------------------------------------------------------------
-- user utterances

i_want_to_Str = variants{ "jag" ++ variants{ "vill"; ["skulle vilja"] };
                          ["kan du"] };
please_Str    = "tack";


----------------------------------------------------------------------
-- system utterances

hello           = mkUtt ["Hej"];
goodbye         = mkUtt ["Hejdå"];
yes             = mkUtt ["Ja"];
no              = mkUtt ["Nej"];

is_that_correct_Post   = mkUtt [", är det korrekt"];
returning_to_Pre       = mkUtt ["Återgår till"];
returning_to_issue_Pre = mkUtt ["Återgår till frågan om"];
i_dont_understand      = mkUtt ["Jag förstår inte riktigt"];

icm_acc_pos     = mkUtt ["Okej"];
icm_con_neg     = mkUtt ["Hallå?"];
icm_reraise     = mkUtt ["Så ,"];
icm_loadplan    = mkUtt ["Få se"];
icm_accommodate = mkUtt ["Visst"];

icm_per_pos   x = mkUtt (["Jag tyckte du sa"] ++ x.s);


----------------------------------------------------------------------
-- noun phrases

sing_NP    s = regNP s (s+"s") SgUtr ** {lock_NP = <>};
plur_NP    s = regNP s (s+"s") Plg   ** {lock_NP = <>};

NPgen_NP = NP_of_NP;

----------------------------------------------------------------------
-- actions/verb phrases

VPing act = act;
vp2Utt vp = UttVP vp;

----------------------------------------------------------------------
-- general syntactical operations

disjunct_QCl q q' = 
    {s = \\t,a,p,x => q.s!t!a!p!x ++ "eller" ++ q'.s!t!a!p!x;
     lock_QCl = <>};

negate_Cl c = 
    {s = \\t,a,p,o => c.s!t!a!(case p of {Neg=>Pos; Pos=>Neg})!o;
     lock_Cl = <>};


----------------------------------------------------------------------
-- verbs

see_V           = Irreg.se_V;

do_V2           = Lex.do_V2;
have_V2         = Lex.have_V2;
understand_V2   = Lex.understand_V2;

know_VQ         = mkVQ Irreg.veta_V;
wonder_VQ       = Lex.wonder_VQ;

say_VS          = Lex.say_VS;

fail_VV         = mkVV (mkV "misslyckas" "misslyckas" "misslyckas" "misslyckades" "misslyckats" "misslyckad");

----------------------------------------------------------------------
-- nouns, proper nouns, common nouns and noun phrases

information_N   = mk2N "information" "informationer";

you_NP          = UsePron Lex.youSg_Pron;

----------------------------------------------------------------------
-- closed word categories

of_på_Prep      = mkPrep "på";
for_Prep        = mkPrep "för";

not_Predet      = {s = \\_ => "inte"} ** {lock_Predet = <>};

}
