
:- module(database,
	[meeting/7,
	 person/6,
	 attends/2,
	 location/5]
    ).

% meeting(ID, Day, Month, Year, StartTime, EndTime, LocID).
meeting(meeting_1, 29, 5, 2007, 9:30, 12:0, nikos_room_1).
meeting(meeting_2, 9, 7, 2007, 10:30, 12:0, saint_margarets_road).
meeting(meeting_3, 9, 9, 2007, 10:30, 18:0, nikos_room_1).
meeting(meeting_4, 10, 9, 2007, 10:30, 18:0, nikos_room_1).
meeting(meeting_5, 3, 9, 2007, 16:30, 17:0, pierrette_room_3).
meeting(meeting_6, 15, 10, 2007, 10:00, 12:0, pierrette_room_3).
meeting(meeting_7, 15, 10, 2007, 14:00, 16:0, sonia_room_1).
meeting(meeting_8, 24, 10, 2007, 14:00, 16:0, pierrette_room_3).
meeting(meeting_9, 19, 10, 2007, 10:00, 12:0, pierrette_room_3).
meeting(meeting_10, 25, 11, 2007, 10:00, 18:00, pierrette_room_3).

meeting(meeting_35, 8, 9, 2008, 9:30, 12:0, manny_room_1).

% person(ID, FirstName, LastName, Affiliation, Phone, Email).
person(elisabeth_kron, erizabesu, kuron, juneevu, "+44 1223 276 838", "elisabethkron@yahoo.co.uk").
person(manny_rayner, manii, reinaa, juneevu, "+44 7957 549 339", "Emmanuel.Rayner@issco.unige.ch").
person(maria_georgescul, maria, jorujesukyuru, juneevu, "+41 22 379 8683", "Maria.Georgescul@eti.unige.ch").
person(marianne_santaholma, mariannu, santahoruma, juneevu, "+41 22 379 8679", "Marianne.Santaholma@eti.unige.ch").
person(marianne_starlander, mariannu, sutaarandaa, juneevu, "+41 22 379 8679", "Marianne.Starlander@eti.unige.ch").
person(nikos_tsourakis, nikosu, tsurakisu, juneevu, "+41 22 379 8683", "Nikos.Tsourakis@issco.unige.ch").
person(pierrette_bouillon, pieretto, buiyon, juneevu, "+41 22 379 8679", "Pierrette.Bouillon@issco.unige.ch").
person(susan_armstrong, suuzan, aamusutorongu, juneevu, "+41 22 379 8679", "Susan.Armstrong@issco.unige.ch").
person(agnes_lisowska, agunesu, lisousuka, juneevu, "+41 22 379 8685", "agnes.lisowska@issco.unige.ch").
person(martin_rajman, maatin, rajiman, roozannu, "+41 21 693 5274", "martin.rajman@epfl.ch").
person(marita_ailomaa, marita, ailomaa, roozannu, "+41 21 693 5274", "marita.ailomaa@epfl.ch").
person(sonia_halimi, sonia, harimi, juneevu, "+44 7957 549 339", "sonia.halimi@eti.unige.ch").
person(yukie_nakao, yukie, nakao, nanto, "+33 7957 549 335", "ynakao1922@hotmail.com").

% attends(PersonID, MeetingID).
attends(pierrette_bouillon, meeting_1).
attends(maria_georgescul, meeting_1).
attends(nikos_tsourakis, meeting_1).
attends(manny_rayner, meeting_1).

attends(pierrette_bouillon, meeting_2).
attends(elisabeth_kron, meeting_2).
attends(manny_rayner, meeting_2).

attends(pierrette_bouillon, meeting_3).
attends(maria_georgescul, meeting_3).
attends(nikos_tsourakis, meeting_3).
attends(manny_rayner, meeting_3).

attends(pierrette_bouillon, meeting_4).     
attends(elisabeth_kron, meeting_4).
attends(manny_rayner, meeting_4).

attends(pierrette_bouillon, meeting_5).     
attends(nikos_tsourakis, meeting_5).

attends(yukie_nakao, meeting_6).     
attends(sonia_halimi, meeting_6).
attends(pierrette_bouillon, meeting_6).

attends(yukie_nakao, meeting_7).     
attends(sonia_halimi, meeting_7).

attends(pierrette_bouillon, meeting_8).     
attends(marianne_santaholma, meeting_8).

attends(pierrette_bouillon, meeting_9).     
attends(agnes_lisowska, meeting_9).

attends(pierrette_bouillon, meeting_10).     
attends(yukie_nakao, meeting_10).
attends(manny_rayner, meeting_10).

attends(manny_rayner, meeting_35).
attends(yukie_nakao, meeting_35).

% location(ID, Name, Country, City, Organisation).
location(manny_room_1, 'manii_no_heya', suisu, juneevu, juneevu_daigaku).
location(idiap_room_1, 'idiapu_no_heya', suisu, marutini, iidiiappu).
location(nikos_room_1, 'nikosu_no_heya', suisu, juneevu, juneevu_daigaku).
location(sonia_room_1, 'sonia_no_heya', suisu, juneevu, juneevu_daigaku).
location(pierrette_room_3, 'pieretto_no_heya', suisu, juneevu, juneevu_daigaku).
location(saint_margarets_road, sento_maagarettsu_roodo, igirisu, kenburijji, null).
location(epfl_room, 'eepiiefueru_no_heya', suisu, roozannu, eepiiefueru).

