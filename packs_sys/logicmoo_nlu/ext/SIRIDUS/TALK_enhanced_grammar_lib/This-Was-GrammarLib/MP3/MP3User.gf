--# -path=.:../Common:prelude

abstract MP3User = GodisUser, Music ** {

fun

-- Predicates
-- Questions used by the User
 
-- U: what songs are available
available_song : Question;

-- U: what songs are there by Artist
available_song__artist : Artist -> Question;

-- U: what is the name of the current song?
current_song : Question;


-- Sorts
-- Used for Short Answers by the User

-- U: Artist == U: it is Artist i want to ...
artist : Artist -> ShortAns;
-- U: Song == U: it is Song i want to ...
song : Song -> ShortAns;
-- U: Song by Artist == U: it is Song by Artist i want to ...
song_artist : Song -> Artist -> ShortAns;


-- Actions
-- User utterances to trigger Actions

-- U: return to the top menu
top : Action;

-- U: help
help : Action;

-- U: control the playback
control_playback : Action;

-- U: stop
pause : Action;

-- U: play a song / play Song / play Artist / play Song by Artist
play_item              : Action;
play_item__song        : Song -> Action;
play_item__artist      : Artist -> Action;
play_item__song_artist : Song -> Artist -> Action;

-- U: wind forward 
fast_forward : Action;

-- U: rewind
rewind : Action;

-- U: control the volume
control_volume : Action;

-- U: lower the volume
vol_down : Action;
  
-- U: raise the volume
vol_up : Action;

-- U: manage the playlist
manage_playlist : Action;

-- U: add a song / add Song / add Artist / add Song by Artist
playlist_add              : Action;
playlist_add__song        : Song -> Action;
playlist_add__artist      : Artist -> Action;
playlist_add__song_artist : Song -> Artist -> Action;

-- U: remove a song / remove Song / remove Artist / remove Song by Artist
playlist_delete              : Action;
playlist_delete__song        : Song -> Action;
playlist_delete__artist      : Artist -> Action;
playlist_delete__song_artist : Song -> Artist -> Action;

-- U: clear the playlist
playlist_clear : Action;

-- U: shuffle the playlist
playlist_shuffle : Action;

}
