--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/english

concrete MP3LexiconEng of MP3Lexicon = CatEng ** 
    open Prelude, ParadigmsEng, LexiconEng in {

lin

-- Adjectives
available_A = regA "available";

-- Adverbs
forward_Adv = mkAdv "forward";

-- Nouns
artist_N    = regN "artist";
music_N     = music_N;
name_N      = name_N;
playback_N  = regN "playback";    
player_N    = regN "player";
playlist_N  = regN "playlist";
song_N      = song_N;
volume_N    = regN "volume";

-- Verb-1
help_V      = regV "help";
play_V      = play_V;
restart_V   = regV "restart";
rewind_V    = irregV "rewind" "rewound" "rewound";
stop_V      = stop_V;
wind_V      = irregV "wind" "wound" "wound";

-- Verb-2
add_V2       = dirV2 (regV "add");
clear_V2     = dirV2 (regV "clear");
control_V2   = dirV2 (regV "control");
delete_V2    = dirV2 (regV "delete");
handle_V2    = dirV2 (regV "handle");
increase_V2  = dirV2 (regV "increase");
listen_to_V2 = dirV2 (partV (regV "listen") "to");
lower_V2     = dirV2 (regV "lower");
made_V2      = dirV2 (irregV "make" "made" "made");
manage_V2    = dirV2 (regV "manage");
play_V2      = play_V2;
shuffle_V2   = dirV2 (regV "shuffle");
stop_V2      = dirV2 stop_V;
switchon_V2  = switch8on_V2;
turnon_V2    = dirV2 (partV turn_V "on");
write_V2     = write_V2;


-- Verb-3
add_to_V3      = add_V3;
delete_from_V3 = dirV3 (regV "delete") (mkPrep "from");

}
