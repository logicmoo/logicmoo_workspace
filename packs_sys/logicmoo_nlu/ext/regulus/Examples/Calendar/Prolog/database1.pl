
:- module(database,
	[meeting/7,
	 person/6,
	 attends/2,
	 location/5]
    ).

% meeting(ID, Day, Month, Year, StartTime, EndTime, ToD, LocID, Type).
meeting(meeting_1, 21, 11, 2008, 9:30, 12:0, conf_room_1).
meeting(meeting_2, 26, 10, 2008, 10:30, 12:0, conf_room_2).
meeting(meeting_3, 29, 9, 2008, 10:30, 18:0, room_623).
meeting(meeting_4, 3, 10, 2019, 10:30, 18:0, room_100).
meeting(meeting_5, 15, 10, 2019, 16:30, 17:0, susan_office).
meeting(meeting_6, 20, 10, 2008, 10:00, 12:0, room_100).
meeting(meeting_7, 21, 10, 2019, 14:00, 16:0, mark_office).
meeting(meeting_8, 3, 11, 2019, 14:00, 16:0, george_office).
meeting(meeting_9, 22, 11, 2019, 10:00, 12:0, susan_office).
meeting(meeting_10, 24, 11, 2019, 9:30, 12:0, conf_room_2).
meeting(meeting_11, 20, 12, 2008, 9:30, 12:0, conf_room_1).
meeting(meeting_12, 9, 12, 2019, 10:00, 11:0, room_623).
meeting(meeting_13, 8, 12, 2019, 9:00, 12:0, room_100).
meeting(meeting_14, 3, 01, 2009, 15:30, 17:0, conf_room_1).
meeting(meeting_15, 15, 12, 2019, 9:30, 10:0, room_100).
meeting(meeting_16, 17, 12, 2019, 14:00, 16:0, conf_room_2).
meeting(meeting_17, 5, 1, 2019, 14:00, 17:0, conf_room_1).
meeting(meeting_18, 5, 1, 2009, 11:30, 12:0, susan_office).
meeting(meeting_19, 14, 1, 2019, 11:00, 12:0, mark_office).
meeting(meeting_20, 16, 2, 2009, 10:30, 12:0, room_623).
meeting(meeting_21, 29, 9, 2009, 9:30, 12:0, nikos_room_1).
meeting(meeting_22, 28, 9, 2009, 10:30, 12:0, saint_margarets_road).
meeting(meeting_23, 1, 9, 2009, 10:30, 18:0, nikos_room_1).
meeting(meeting_24, 15, 9, 2009, 10:30, 18:0, nikos_room_1).
meeting(meeting_25, 3, 9, 2009, 16:30, 17:0, pierrette_room_3).
meeting(meeting_26, 21, 9, 2009, 10:00, 12:0, pierrette_room_3).
meeting(meeting_27, 19, 9, 2009, 14:00, 16:0, sonia_room_1).
meeting(meeting_28, 24, 9, 2009, 14:00, 16:0, pierrette_room_3).
meeting(meeting_29, 7, 9, 2009, 10:00, 12:0, pierrette_room_3).
meeting(meeting_30, 30, 9, 2009, 9:30, 12:0, nikos_room_1).
meeting(meeting_31, 23, 9, 2009, 9:30, 12:0, nikos_room_1).
meeting(meeting_32, 15, 9, 2009, 10:00, 11:0, idiap_room).
meeting(meeting_33, 28, 9, 2009, 9:00, 12:0, epfl_room).
meeting(meeting_34, 7, 9, 2009, 15:30, 17:0, saint_margarets_road).
meeting(meeting_35, 4, 9, 2009, 9:30, 10:0, sonia_room_1).
meeting(meeting_36, 10, 9, 2009, 14:00, 16:0, epfl_room).
meeting(meeting_37, 17, 9, 2009, 14:00, 17:0, idiap_room).
meeting(meeting_38, 2, 9, 2009, 11:30, 12:0, pierrette_room_3).
meeting(meeting_39, 6, 9, 2009, 11:00, 12:0, epfl_room).
meeting(meeting_40, 14, 9, 2009, 10:30, 12:0, idiap_room).


% person(ID, FirstName, LastName, Affiliation, Phone, Email).

person(george_clooney, george, clooney, london, "+44 122 555 1234", "g.clooney@mymail.co.uk").
person(brad_pitt, brad, pitt, london, "+44 122 555 5274", "b.pitt@mymail.co.uk").
person(david_smith, david, smith, london, "+44 122 555 6838", "d.smith@mymail.co.uk").
person(susan_davis, susan, davis, stockholm, "+46 22 555 8679", "Susan.Davis@mymail.se").
person(sonia_wright, sonia, wright, stockholm, "+46 21 555 5274", "sonia.wright@mymail.se").
person(mark_green, mark, green, nantes, "+41 22 555 8683", "Mark.Green@mymail.ch").
person(mike_jones, mike, jones, geneva, "+41 22 555 8679", "Mike.Jones@mymail.ch").
person(sophie_bradford, sophie, bradford, athens, "+41 22 555 8685", "sophie.bradford@mymail.ch").
person(nina_carpenter, nina, carpenter, geneva, "+30 79 555 9339", "m.carpenter@mymail.gr").
person(alex_miller, alex, miller, athens, "+30 22 555 8679", "m.miller@mymail.gr").
person(john_dunn, john, dunn, lausanne, "+41 21 555 339", "john.dunn@mymail.ch").
person(maria_williams, maria, williams, lausanne, "+41 21 379 8679", "maria.williams@mymail.ch").
person(elisabeth_kron, elisabeth, kron, lausanne, "+44 1223 276 838", "elisabethkron@yahoo.co.uk").
person(manny_rayner, manny, rayner, lausanne, "+44 7957 549 339", "Emmanuel.Rayner@issco.unige.ch").
person(maria_georgescul, maria, georgescul, lausanne, "+41 22 379 8683", "Maria.Georgescul@eti.unige.ch").
person(marianne_santaholma, marianne, santaholma, lausanne, "+41 22 379 8679", "Marianne.Santaholma@eti.unige.ch").
person(marianne_starlander, marianne, starlander, lausanne, "+41 22 379 8679", "Marianne.Starlander@eti.unige.ch").
person(nikos_tsurakis, nikos, tsurakis, lausanne, "+41 22 379 8683", "Nikos.Tsurakis@issco.unige.ch").
person(pierrette_bouillon, pierrette, bouillon, lausanne, "+41 22 379 8679", "Pierrette.Bouillon@issco.unige.ch").
person(susan_armstrong, susan, armstrong, nantes, "+41 22 379 8679", "Susan.Armstrong@issco.unige.ch").
person(agnes_lisowska, agnes, lisowska, lausanne, "+41 22 379 8685", "agnes.lisowska@issco.unige.ch").
person(martin_rajman, martin, rajman, lausanne, "+41 21 693 5274", "martin.rajman@epfl.ch").
person(marita_ailomaa, marita, ailomaa, lausanne, "+41 21 693 5274", "marita.ailomaa@epfl.ch").
person(sonia_halimi, sonia, halimi, lausanne, "+44 7957 549 339", "sonia.halimi@eti.unige.ch").
person(yukie_nakao, yukie, nakao, nantes, "+33 7957 549 335", "ynakao1922@hotmail.com").





% attends(PersonID, MeetingID).

attends(john_dunn, meeting_1).
attends(susan_davis, meeting_1).
attends(mike_jones, meeting_1).

attends(john_dunn, meeting_2).
attends(mike_jones, meeting_2).
attends(alex_miller, meeting_2).

attends(david_smith, meeting_3).
attends(sonia_wright, meeting_3).
attends(alex_miller, meeting_3).
attends(maria_williams, meeting_3).

attends(nina_carpenter, meeting_4).     
attends(david_smith, meeting_4).
attends(sonia_wright, meeting_4).

attends(susan_davis, meeting_5).     
attends(nina_carpenter, meeting_5).

attends(mike_jones, meeting_6).     
attends(john_dunn, meeting_6).
attends(sophie_bradford, meeting_6).

attends(mark_green, meeting_7).     
attends(brad_pitt, meeting_7).

attends(george_clooney, meeting_8).     
attends(brad_pitt, meeting_8).

attends(susan_davis, meeting_9).     
attends(maria_williams, meeting_9).

attends(john_dunn, meeting_10).     
attends(david_smith, meeting_10).     
attends(sonia_wright, meeting_10).

attends(mike_jones, meeting_11).     
attends(susan_davis, meeting_11).     
attends(mark_green, meeting_11).

attends(david_smith, meeting_12).     
attends(sophie_bradford, meeting_12).     
attends(mark_green, meeting_12).

attends(nina_carpenter, meeting_13).     
attends(alex_miller, meeting_13).     
attends(susan_davis, meeting_13).
attends(maria_williams, meeting_13).

attends(sonia_wright, meeting_14).     
attends(mike_jones, meeting_14).
attends(alex_miller, meeting_14).
attends(brad_pitt, meeting_14).

attends(alex_miller, meeting_15).     
attends(nina_carpenter, meeting_15).     
attends(sophie_bradford, meeting_15).

attends(maria_williams, meeting_16).     
attends(john_dunn, meeting_16).     
attends(david_smith, meeting_16).

attends(sophie_bradford, meeting_17).     
attends(maria_williams, meeting_17).     
attends(mark_green, meeting_17).
attends(sonia_wright, meeting_17).

attends(david_smith, meeting_18).     
attends(nina_carpenter, meeting_18).     
attends(mike_jones, meeting_18).

attends(mark_green, meeting_19).     
attends(alex_miller, meeting_19).   

attends(george_clooney, meeting_20).     
attends(sonia_wright, meeting_20).
attends(mike_jones, meeting_20).

attends(pierrette_bouillon, meeting_21).
attends(maria_georgescul, meeting_21).
attends(nikos_tsurakis, meeting_21).
attends(manny_rayner, meeting_21).

attends(pierrette_bouillon, meeting_22).
attends(elisabeth_kron, meeting_22).
attends(manny_rayner, meeting_22).

attends(pierrette_bouillon, meeting_23).
attends(maria_georgescul, meeting_23).
attends(nikos_tsurakis, meeting_23).
attends(manny_rayner, meeting_23).

attends(pierrette_bouillon, meeting_24).     
attends(elisabeth_kron, meeting_24).
attends(manny_rayner, meeting_24).

attends(pierrette_bouillon, meeting_25).     
attends(nikos_tsurakis, meeting_25).

attends(yukie_nakao, meeting_26).     
attends(sonia_halimi, meeting_26).
attends(pierrette_bouillon, meeting_26).

attends(yukie_nakao, meeting_27).     
attends(sonia_halimi, meeting_27).

attends(pierrette_bouillon, meeting_28).     
attends(marianne_santaholma, meeting_28).

attends(pierrette_bouillon, meeting_29).     
attends(agnes_lisowska, meeting_29).

attends(maria_georgescul, meeting_30).     
attends(pierrette_bouillon, meeting_30).     
attends(manny_rayner, meeting_30).

attends(maria_georgescul, meeting_31).     
attends(pierrette_bouillon, meeting_31).     
attends(manny_rayner, meeting_31).

attends(agnes_lisowska, meeting_32).     
attends(pierrette_bouillon, meeting_32).     
attends(manny_rayner, meeting_32).
attends(nikos_tsurakis, meeting_32).

attends(marita_ailomaa, meeting_33).     
attends(pierrette_bouillon, meeting_33).     
attends(martin_rajman, meeting_33).
attends(manny_rayner, meeting_33).

attends(elisabeth_kron, meeting_34).     
attends(pierrette_bouillon, meeting_34).     
attends(manny_rayner, meeting_34).
attends(marianne_santaholma, meeting_34).

attends(sonia_halimi, meeting_35).     
attends(pierrette_bouillon, meeting_35).     
attends(manny_rayner, meeting_35).

attends(marita_ailomaa, meeting_36).     
attends(pierrette_bouillon, meeting_36).     
attends(martin_rajman, meeting_36).

attends(martin_rajman, meeting_37).     
attends(pierrette_bouillon, meeting_37).     
attends(marianne_starlander, meeting_37).
attends(susan_armstrong, meeting_37).

attends(marianne_starlander, meeting_38).     
attends(pierrette_bouillon, meeting_38).     
attends(susan_armstrong, meeting_38).

attends(marita_ailomaa, meeting_39).     
attends(martin_rajman, meeting_39).

attends(maria_georgescul, meeting_40).     
attends(pierrette_bouillon, meeting_40).

% location(ID, Name, Country, City, Organisation).
location(mark_office, 'marks_\'s_office', switzerland, geneva, geneva_university).
location(conf_room_1, 'geneva_conference_room', switzerland, geneva, geneva_university).
location(room_623, 'room_623', england, london, london_university).
location(conf_room_2, 'epfl_conference_room', switzerland, lausanne, epfl).
location(susan_office, 'susan_\'s_office', sweden, stockholm, stockholm_university).
location(george_office, 'george_\'s_office', england, london, london_university).
location(room_100, 'room_100', greece, athens, athens_university).

location(nikos_room_1, 'nikos_\'s_room', switzerland, geneva, geneva_university).
location(sonia_room_1, 'sonia_\'s_room', switzerland, geneva, geneva_university).
location(pierrette_room_3, 'pierrette_\'s_room', switzerland, geneva, geneva_university).
location(saint_margarets_road, saint_margarets_road, england, cambridge, null).
location(idiap_room, a_room_at_idiap, switzerland, martigny, idiap).
location(epfl_room, a_room_at_epfl, switzerland, lausanne, epfl).