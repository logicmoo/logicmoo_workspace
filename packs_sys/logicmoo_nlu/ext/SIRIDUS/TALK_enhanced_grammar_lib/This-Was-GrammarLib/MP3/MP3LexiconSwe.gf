--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/swedish:resource-1.0/scandinavian

concrete MP3LexiconSwe of MP3Lexicon = CatSwe ** 
    open Prelude, ParadigmsSwe, LexiconSwe, IrregSwe in {

lin

-- Adjectives
available_A = regA "tillgänglig";

-- Adverbs
forward_Adv = mkAdv "framåt";

-- Nouns
artist_N    = regGenN "artist" utrum;
music_N     = music_N;
name_N      = name_N;
playback_N  = regGenN "uppspelning" utrum;
player_N    = regGenN "spelare" utrum;
playlist_N  = regGenN "spellista" utrum;
song_N      = regGenN "låt" utrum;
volume_N    = regGenN "volym" utrum;

-- Verbs-1
help_V      = mk2V "hjälpa" "hjälpte";
play_V      = regV "spela";
restart_V   = partV (regV "börjar") "om";
rewind_V    = partV (regV "spolar") "bakåt";
stop_V      = stop_V;
wind_V      = partV (regV "spolar") "framåt";

-- Verbs-2
add_V2       = dirV2 (partV lägga_V "till");
clear_V2     = dirV2 (regV "rensar");
control_V2   = dirV2 (regV "kontrollerar");
delete_V2    = dirV2 (partV (mkV "ta" "tar" "ta" "tog" "tagit" "tagen") "bort");
handle_V2    = dirV2 (regV "hanterar");
increase_V2  = dirV2 (regV "ökar");
listen_to_V2 = dirV2 (partV (regV "lyssnar") "på");
lower_V2     = dirV2 (regV "minskar");
made_V2      = dirV2 (mkV "göra" "gör" "gör" "gjorde" "gjort" "gjord");
manage_V2    = dirV2 (regV "hanterar");
play_V2      = play_V2;
shuffle_V2   = dirV2 (regV "blandar");
stop_V2      = dirV2 stop_V;
switchon_V2 = switch8on_V2;
turnon_V2   = dirV2 (partV (mkV "sätta" "sätter" "sätt" "satte" "satt" "satt") "på");
write_V2     = write_V2;

-- Verbs-3
add_to_V3      = add_V3;
delete_from_V3 = dirV3 (partV (mkV "ta" "tar" "ta" "tog" "tagit" "tagen") "bort") (mkPrep "från");

}
