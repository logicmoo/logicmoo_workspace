--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/english

concrete AgendaSystemEng of AgendaSystem = GodisSystemEng, BookingEng ** AgendaSystemI with 
    (GodisLang=GodisLangEng), (Grammar=GrammarEng), (AgendaLexicon=AgendaLexiconEng);

