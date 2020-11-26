--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/english

concrete MP3SystemEng of MP3System = GodisSystemEng, MusicEng ** MP3SystemI with 
    (Grammar=GrammarEng), (GodisLang=GodisLangEng), (MP3Lexicon=MP3LexiconEng);

