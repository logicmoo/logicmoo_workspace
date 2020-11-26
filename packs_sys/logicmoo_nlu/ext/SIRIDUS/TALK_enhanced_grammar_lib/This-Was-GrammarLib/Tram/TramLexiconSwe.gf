--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/swedish:resource-1.0/scandinavian

concrete TramLexiconSwe of TramLexicon = CatSwe ** 
    open Prelude, ParadigmsSwe, ParamX, (Lex=LexiconSwe), (Irr=IrregSwe), GodisLangSwe in {

lin

-- Adjectives
short_A = Lex.short_A;

-- Conjunctions
and_then_Conj = {s = ["och sedan"]; n = Pl; lock_Conj = <>};

-- Nouns
route_N     = regGenN "rutt" utrum;
stop_N      = regGenN "hållplats" utrum;

-- Verb-1
help_V      = regV "hjälpa";
restart_V   = partV (regV "starta") "om";

-- Verb-2
go_from_V2   = dirV2 (partV åka_V "från");
go_to_V2     = dirV2 (partV åka_V "till");
find_V2      = dirV2 (regV "hitta");
take_V2      = dirV2 (ta_V);


oper 
åka_V : V = irregV "åka" "åkte" "åkt";
ta_V  : V = mkV "ta" "tar" "ta" "tog" "tagit" "tagen";


}
