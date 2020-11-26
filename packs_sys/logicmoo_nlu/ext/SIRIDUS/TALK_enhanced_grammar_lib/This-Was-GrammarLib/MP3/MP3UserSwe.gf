--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/swedish:resource-1.0/scandinavian

concrete MP3UserSwe of MP3User = GodisUserSwe, MusicSwe ** 
    open Prelude, GrammarSwe, GodisLangSwe, MP3LexiconSwe, MP3SystemSwe, CommonScand in {

oper S_by_A : NP -> NP -> Str
    = \s,a -> variants{s.s!NPNom ++ variants{"med";"av"} ++ a.s!NPNom;
		       a.s!NPNom ++ "med" ++ s.s!NPNom;
		       a.s!(NPPoss SgUtr) ++ optStr "låt" ++ s.s!NPNom};


-- predicates 

lin
available_song
    = variants{ askQS available_song_Q;
		ss vilka_finns;
		ss (vilka_har ++ honhan ++ gjort) };

available_song__artist x 
    = variants{ ss (vilka_finns ++ "med" ++ x.s!NPNom);
		ss (vilka_har ++ x.s!NPNom ++ gjort);
		ss (["har du"] ++ någon_låt ++ "med" ++ x.s!NPNom) };

current_song
    = variants{ askQS current_song_Q;
 		ss ["vilken låt är det här"] };

oper vilka_finns : Str = ["vilka låtar finns det"];
oper vilka_har   : Str = ["vilka låtar har"];
oper honhan      : Str = variants{"de";"hon";"han"};
oper gjort       : Str = variants{"gjort";"skrivit"};
oper någon_låt   : Str = variants{["något"];["någonting"];["nånting"];
				  ["någon låt"];["några låtar"]};


-- short answers

lin
artist x = ansNP x;
song   x = ansNP x;
song_artist x y = ss (S_by_A x y);


-- actions

lin
top = reqVP top;

help = variants{ reqVP help;
		 ss ["hur gör jag nu"] }; 

control_playback = reqVP control_playback;

pause = variants{ reqVP pause;
                  req2x "paus" "pausa" ""};   
                 
play_item = variants{ reqVP play ; 
		 reqVP (UseV play_V) ; 
		 reqVP (V2_the_N
			    (variants{turnon_V2;switchon_V2}) 
			    (variants{music_N;player_N})) };

play_item__song   x = variants{spela (x.s!NPNom);
                               req1x "starta" (x.s!NPNom)};
play_item__artist y = variants{spela (y.s!NPNom);
                               req1x "starta" (y.s!NPNom)};
play_item__song_artist x y = variants{spela (S_by_A x y);
                                      req1x "starta" (y.s!NPNom)};

fast_forward = reqVP fast_forward;

rewind = reqVP rewind;

control_volume = reqVP control_volume;

vol_down = reqVP vol_down;

vol_up = reqVP vol_up;

manage_playlist = reqVP manage_playlist;

playlist_add = variants{ lägg_till (optStr (["en låt"]));
                         utöka (optStr (["med en låt"]))};
playlist_add__song   x = variants{ lägg_till (x.s!NPNom);
                                   utöka ("med" ++ (x.s!NPNom))};
playlist_add__artist y = variants{ lägg_till (y.s!NPNom);
                                   utöka ("med" ++ (y.s!NPNom))};
playlist_add__song_artist x y = variants{ lägg_till (S_by_A x y);
                                          utöka ("med" ++ S_by_A x y)};

playlist_delete = reqVP playlist_delete;
playlist_delete__song   x =  ta_bort (x.s!NPNom);
playlist_delete__artist y =  ta_bort (y.s!NPNom);
playlist_delete__song_artist x y = ta_bort (S_by_A x y);

playlist_clear = reqVP playlist_clear;

playlist_shuffle = reqVP playlist_shuffle;

oper spela : Str -> UserAction = req1x "spela";
oper lägg_till : Str -> UserAction = \arg ->
	 req2x "lägg" "lägga" ("till" ++ arg ++ optStr (variants{"till";"i"} ++ "spellistan"));
oper utöka : Str -> UserAction = \arg ->
         req1x "utöka" ("spellistan" ++ arg);  
oper ta_bort : Str -> UserAction = \arg ->
         req1x ["ta bort"] (arg ++ optStr ["från spellistan"]);

}

