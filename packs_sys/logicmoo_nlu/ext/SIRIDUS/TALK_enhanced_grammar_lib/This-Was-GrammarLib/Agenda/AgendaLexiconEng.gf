--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/english

concrete AgendaLexiconEng of AgendaLexicon = CatEng ** 
    open Prelude, ParadigmsEng, LexiconEng in {

lin


-- Nouns
booking_N = regN "booking";
agenda_N = regN "agenda";
time_N = regN "time";
event_N = regN "event";
date_N = regN "date";


-- Verbs-1

add_V = regV "add";

--Verbs-2

add_V2    = dirV2 (regV "add");
change_V2 = dirV2 change_V;
delete_V2 = dirV2 delete_V;
move_V2   = dirV2 (regV "move");

--Verbs-3

add_V3    = add_V3;
change_V3 = dirV3 change_V (mkPrep "for");
delete_V3 = dirV3 delete_V (mkPrep "from");

oper 
change_V : V = regV "change";
delete_V : V = regV "delete";

}
