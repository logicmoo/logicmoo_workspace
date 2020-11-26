--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/english

concrete MP3UserEng of MP3User = GodisUserEng, MusicEng ** 
    open Prelude, GodisLangEng, MP3SystemEng, ResEng, MP3LexiconEng in {

oper

S_by_A : NP -> NP -> Str
    = \s,a -> variants{s.s!Nom ++ variants{"with";"by"} ++ a.s!Nom;
		       a.s!Nom ++ "with" ++ s.s!Nom;
		       "the" ++ a.s!Nom ++ "song" ++ s.s!Nom;
		       a.s!Gen ++ optStr "song" ++ s.s!Nom};


-- predicates 
lin

available_song
    = variants{ askQS available_song_Q;
		ss (["what songs"] ++
			variants{variants{["have they"];["has he"];["has she"]} ++ written;
				 ["do you have"]})};

available_song__artist x 
    = variants{ askQS (which_N_has_NP_V2 song_N x write_V2);
		ss (["what songs"] ++
			variants{ ["are there by"] ++ x.s!Nom; 
				  has!x.a.n ++ x.s!Nom ++ written;
				  ["do you have by"] ++ x.s!Nom });
		ss (["do you have"] ++ variants{["any songs"];["anything"]} ++ "by" ++ x.s!Nom) };


current_song
    = variants{askQS current_song_Q;
               ss ["what is the name of the current song"];
               ss ["which song is this"]};


oper written : Str = variants{"done";"made";"written"};
oper has     : Number => Str = table {Sg => "has"; Pl => "have"};


-- short answers
lin

artist x = ansNP x;
song   x = ansNP x;
song_artist x y = ss (S_by_A x y);


-- actions
lin

top = reqVP top;

help = reqVP help; 

control_playback = reqVP control_playback;

pause = reqVP pause;
                 
play_item = variants{ reqVP play_item; 
		 req1 ["turn on the player"]; 
		 req1 "play" };
play_item__song   x = req1x "play" (x.s!Nom);
play_item__artist y = req1x "play" (y.s!Nom);
play_item__song_artist x y = req1x "play" (S_by_A x y);

fast_forward = reqVP fast_forward;

rewind = reqVP rewind;

control_volume = reqVP control_volume;

vol_down = reqVP vol_down;

vol_up = reqVP vol_up;

manage_playlist = reqVP manage_playlist;

playlist_add = variants{ reqVP playlist_add;
		req1x "add" (optStr (["a song"] ++ optStr to_playlist)) };
playlist_add__song   x = req1x "add" (x.s!Nom ++ to_playlist);
playlist_add__artist y = req1x "add" (y.s!Nom ++ to_playlist);
playlist_add__song_artist x y = req1x "add" (S_by_A x y ++ to_playlist);

playlist_delete = reqVP playlist_delete;
playlist_delete__song   x = req1x "delete" (x.s!Nom ++ from_playlist);
playlist_delete__artist y = req1x "delete" (y.s!Nom ++ from_playlist);
playlist_delete__song_artist x y = req1x "delete" (S_by_A x y ++ from_playlist);

playlist_clear = reqVP playlist_clear;

playlist_shuffle = variants{reqVP playlist_shuffle;
			    req1 ["shuffle"]};

oper to_playlist : Str = optStr ["to the playlist"];
     from_playlist : Str = optStr ["from the playlist"];

}

