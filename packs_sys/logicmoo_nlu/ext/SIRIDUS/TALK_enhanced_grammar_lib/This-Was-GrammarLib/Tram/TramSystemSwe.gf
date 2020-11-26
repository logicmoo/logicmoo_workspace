--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/swedish:resource-1.0/scandinavian

concrete TramSystemSwe of TramSystem = GodisSystemSwe, StopsSwe, LinesSwe ** TramSystemI with
    (Grammar=GrammarSwe), (GodisLang=GodisLangSwe), (TramLexicon=TramLexiconSwe);
