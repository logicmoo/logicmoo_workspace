--# -path=.:../:../../:../Shared/:../../../Resource/Media/:../../../Resource/Media/English/:../../../Resource/Media/Swedish:../../../Resource/Media/Swedish:../../../Resource/Numbers/:../../../Core:../../../Core/Shared/:../../../Core/User



concrete usr_domain_player_sem of usr_domain_player = userCorePro, sharedDomainPro ** {

flags conversion=finite;


lin
	answerSongArtistPlay song artist = { s = ["answer ( item ("] ++ song.s ++ [" ) ) ,"] ++ 
						["answer ( group ("] ++ artist.s ++ [") )"]};
	answerSongArtistAdd song artist = { s = ["answer ( item ("] ++ song.s ++ [" ) ) ,"] ++ 
						["answer ( group ("] ++ artist.s ++ [") )"]};
}