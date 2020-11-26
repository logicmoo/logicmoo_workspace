--# -path=.:../Common:prelude

concrete MP3SystemSem of MP3System = GodisSystemSem, MusicSem ** open Prolog in {

lin

-- Predicates

available_song_Q   = pWhQ "available_song";
available_song_P   = pp1  "available_song";

current_song_Q     = pWhQ "current_song";
current_song_P     = pp1  "current_song";

song_to_play_Q     = pWhQ "song_to_play";
song_to_play_P     = pp1  "song_to_play";

song_to_add_Q      = pWhQ "song_to_add";
song_to_add_P      = pp1  "song_to_add";

song_to_delete_Q   = pWhQ "song_to_delete";
song_to_delete_P   = pp1  "song_to_delete";

artist_available_song_Q = pWhQ "artist_available_song";
artist_available_song_P = pp1  "artist_available_song";

artist_to_play_Q   = pWhQ "artist_to_play";
artist_to_play_P   = pp1  "artist_to_play";

artist_to_delete_Q = pWhQ "artist_to_delete";
artist_to_delete_P = pp1  "artist_to_delete";

artist_to_add_Q    = pWhQ "artist_to_add";
artist_to_add_P    = pp1  "artist_to_add";


-- Short answers

artist = pp1 "artist";
song   = pp1 "song";


-- Actions

top              = pp0 "top";
help             = pp0 "help";
control_playback = pp0 "control_playback";
play             = pp0 "play";
pause            = pp0 "pause";
play_item        = pp0 "play_item";
fast_forward     = pp0 "fast_forward";
rewind           = pp0 "rewind";
control_volume   = pp0 "control_volume";
vol_down         = pp0 "vol_down";
vol_up           = pp0 "vol_up";
manage_playlist  = pp0 "manage_playlist";
playlist_add     = pp0 "playlist_add";
playlist_delete  = pp0 "playlist_delete";
playlist_clear   = pp0 "playlist_clear";
playlist_shuffle = pp0 "playlist_shuffle";

}
