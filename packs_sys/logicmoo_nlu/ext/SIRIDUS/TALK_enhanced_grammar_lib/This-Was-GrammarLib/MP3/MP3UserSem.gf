--# -path=.:../Common:prelude

concrete MP3UserSem of MP3User = GodisUserSem, MusicSem ** 
    open Prolog, MP3SystemSem in {

lin

-- Predicates

available_song = pm1 (ask available_song_Q);
available_song__artist x = pm2 (ask available_song_Q) 
                               (shortAns (artist x));
current_song = pm1 (ask current_song_Q);


-- Short answers

artist x = pm1 (shortAns (artist x));
song   x = pm1 (shortAns (song x));
song_artist x y = pm2 (shortAns (song x)) 
                      (shortAns (artist y));


-- Actions

top = pm1 (request top);

help = pm1 (request help);

control_playback = pm1 (request control_playback);

pause = pm1 (request pause);

play_item = pm1 (request play_item);
play_item__song x = pm2 (request play_item) 
                        (answer (song_to_play_P x));
play_item__artist y = pm2 (request play_item) 
                          (answer (artist_to_play_P y));
play_item__song_artist x y = pm3 (request play_item) 
                                 (answer (song_to_play_P x)) 
                                 (answer (artist_to_play_P y));

fast_forward = pm1 (request fast_forward);

rewind = pm1 (request rewind);

control_volume = pm1 (request control_volume);

vol_down = pm1 (request vol_down);
  
vol_up = pm1 (request vol_up);

manage_playlist = pm1 (request manage_playlist);

playlist_add = pm1 (request playlist_add);
playlist_add__song x = pm2 (request playlist_add) 
                           (answer (song_to_add_P x));
playlist_add__artist y = pm2 (request playlist_add) 
                             (answer (artist_to_add_P y));
playlist_add__song_artist x y = pm3 (request playlist_add) 
                                    (answer (song_to_add_P x)) 
                                    (answer (artist_to_add_P y));

playlist_delete = pm1 (request playlist_delete);
playlist_delete__song x = pm2 (request playlist_delete) 
                              (answer (song_to_delete_P x));
playlist_delete__artist y = pm2 (request playlist_delete) 
                                (answer (artist_to_delete_P y));
playlist_delete__song_artist x y = pm3 (request playlist_delete) 
                                       (answer (song_to_delete_P x)) 
                                       (answer (artist_to_delete_P y));

playlist_clear = pm1 (request playlist_clear);

playlist_shuffle = pm1 (request playlist_shuffle);

}

