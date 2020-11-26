
:- module(database,
	[meeting/7,
	 person/6,
	 attends/2,
	 location/5]
    ).

% meeting(ID, Day, Month, Year, StartTime, EndTime, LocID).
meeting(meeting_1, 25, 2, 2008, 10:00, 11:30, lorraine_office).
meeting(meeting_2, 26, 3, 2008, 10:00, 12:30, yumiko_house).
meeting(meeting_3, 7, 4, 2008, 14:00, 15:00, lorraine_office).
meeting(meeting_4, 11, 4, 2008, 13:15, 15:30, L1).
meeting(meeting_5, 13, 4, 2008, 9:00, 16:00, manny_office).
meeting(meeting_6, 14, 5, 2008, 13:30, 15:30, L1).
meeting(meeting_7, 26, 5, 2008, 13:00, 17:00, kageura_office).
meeting(meeting_8, 27, 5, 2008, 10:00, 12:00, kyoko_room).
meeting(meeting_9, 31, 7, 2008, 10:20, 12:00, kyoko_room).
meeting(meeting_10, 8, 9, 2008, 10:00, 11:30, lorraine_office).
meeting(meeting_11, 8, 9, 2008, 14:00, 15:00, daille_office).
meeting(meeting_12, 9, 9, 2008, 14:00, 16:30, yumiko_house).
meeting(meeting_13, 10, 9, 2008, 12:00, 14:00, yukie).
meeting(meeting_14, 11, 9, 2008, 8:00, 12:00, L1).
meeting(meeting_15, 11, 9, 2008, 14:00, 15:00, yukie).
meeting(meeting_16, 11, 9, 2008, 19:00, 21:00, hotel_mercure).
meeting(meeting_17, 12, 9, 2008, 9:00, 11:00, yukie).
meeting(meeting_18, 12, 9, 2008, 18:00, 19:00, pierrette_room_3).
meeting(meeting_19, 13, 9, 2008, 9:00, 12:00, manny_office).
meeting(meeting_20, 13, 9, 2008, 12:30, 14:00, town_geneva).
meeting(meeting_21, 13, 9, 2008, 14:30, 18:00, manny_office).
meeting(meeting_22, 14, 9, 2008, 14:00, 16:00, yukie).
meeting(meeting_23, 16, 9, 2008, 10:00, 12:00, lorraine_office).
meeting(meeting_24, 22, 9, 2008, 13:30, 15:30, L1).
meeting(meeting_25, 23, 9, 2008, 10:00, 12:00, yukie).
meeting(meeting_26, 25, 9, 2008, 8:00, 12:00, daille_office).
meeting(meeting_27, 29, 9, 2008, 9:00, 11:00, yukie).
meeting(meeting_28, 30, 9, 2008, 8:00, 12:00, daille_office).
meeting(meeting_29, 2, 10, 2008, 10:00, 18:00, saint_margarets_road).
meeting(meeting_30, 3, 10, 2008, 10:00, 18:00, saint_margarets_road).
meeting(meeting_31, 7, 10, 2008, 8:00, 12:00, L1).
meeting(meeting_32, 9, 10, 2008, 10:00, 12:00, Lorraine_office).
meeting(meeting_33, 14, 10, 2008, 8:00, 12:00, pierrette_room_3).
meeting(meeting_34, 16, 10, 2008, 10:00, 12:00, manny_office).
meeting(meeting_35, 17, 10, 2008, 16:00, 17:00, daille_office).
meeting(meeting_36, 8, 12, 2008, 10:00, 11:30, yukie).

% person(ID, FirstName, LastName, Affiliation, Phone, Email).
person(manny_rayner, manii, reinaa, juneevu, "+44 7957 549 339", "Emmanuel.Rayner@issco.unige.ch").
person(maria_georgescul, maria, jorujesukyuru, juneevu, "+41 22 379 8683", "Maria.Georgescul@eti.unige.ch").
person(nikos_tsourakis, nikosu, tsurakisu, juneevu, "+41 22 379 8683", "Nikos.Tsourakis@issco.unige.ch").
person(pierrette_bouillon, pieretto, buiyon, juneevu, "+41 22 379 8679", "Pierrette.Bouillon@issco.unige.ch").
person(yukie_nakao, yukie, nakao, nanto, "+33 7957 549 335", "ynakao1922@hotmail.com").
person(lorraine_lemoine, roreenu, lumoan, nanto, "+33 7058 599 385", "lolemoine@hotmail.fr").
person(yumiko_tanaka, yumiko, tanaka, nanto, "+33 4593 498 008", "y0909ta@yahoo.co.jp").
person(anna_gainer, ana, genaa, nanto, "+33 6 45 98 23 94", "annagainer@wanadoo.fr").
person(hitoshi_isahara, hitoshi, isahara, kyoto, "+81 742 44 8977", "hitoshi_isahara@nict.co.jp").
person(beatrice_daille, beatorisu, daiyu, nanto, "+33 2 40 11 22 33 44", beatrice_daille@univ-nantes.fr").
person(kyo_kageura, kyo, kageura, tokyo, "+81 344 4523", kyokageura@univ-tokyo.jp").
person(kyoko_kanzaki, kyoko, kanzaki, kyoto, "+81 742 44 8978", "kyoko_kanzaki@nict.co.jp").
person(marie_sadlik, marii, sadorikku, nanto, "+33 2 40 55 49 22", "m_sadlik@hotmail.fr").
person(makiko_parent, mariko, paron, san_nazeeru, "+33 02 52 90 89 23", "mariko_parent@free.fr").
person(christine_briand, kurisutiinu, burian, paris, "+33 6 45 29 33 00", christine.briand@univ-nantes.fr").
person(hiroshi_masuda, hiroshi, masuda, osaka, "+81 9074 3454", hiroshi.masuda@univ-osaka.co.jp").


% attends(PersonID, MeetingID).
attends(yukie_nakao,meeting_1).
attends(lorraine_lemoine, meeting_1).

attends(yumiko_tanaka, meeting_2).
attends(yukie_nakao, meeting_2).

attends(yukie_nakao,meeting_3).
attends(lorraine_lemoine, meeting_3).
attends(christine_briand,meeting_3).

attends(yumiko_tanaka, meeting_4).
attends(yukie_nakao, meeting_4).
attends(anna_gainer, meeting_4).

attends(yukie_nakao, meeting_5).     
attends(hitoshi_isahara, meeting_5).
attends(manny_rayner, meeting_5).

attends(yukie_nakao, meeting_6).     
attends(yumiko_tanaka, meeting_6).


attends(yukie_nakao, meeting_7).     
attends(beatrice_daille, meeting_7).
attends(kyo_kageura, meeting_7).

attends(yukie_nakao, meeting_8).     
attends(kyoko_kanzaki, meeting_8).

attends(yukie_nakao, meeting_9).     
attends(hitoshi_isahara, meeting_9).
attends(kyoko_kanzaki, meeting_9).

attends(lorraine_lomoine, meeting_10).     
attends(yukie_nakao, meeting_10).

attends(beatrice_daille, meeting_11).
attends(yukie_nakao, meeting_11).

attends(yumiko_tanaka, meeting 12).
attends(yukie_nakao, meeting 12).

attends(yukie_nakao, meeting_13).
attends(mariko_parent, meeting_13).

attends(yukie_nakao, meeting_14).
attends(yumiko_tanaka, meeting_14).

attends(yukie_nakao, meeting_15).
attends(mariko_parent, meeting_15).

attends(hiroshi_masuda, meeting_16)
attends(yukie_nakao, meeting_16).

attends(marie_sadlik,meeting_17).
attends(yukie_nakao, meeting_17).
 
attends(manny_rayner, meeting_18).
attends(hiroshi_masuda, meeting_18).
attends(yukie_nakao, meeting_18).
attends(pierrette_bouillon, meeting_18).

attends(manny_rayner, meeting_19).
attends(yukie_nakao, meeting_19).
 
attends(manny_rayner, meeting_20).
attends(yukie_nakao, meeting_20).
attends(nikos_tsourakis, meeting_20).
attends(maria_georgescul, meeting_20).
attends(pierrette_bouillon, meeting_20).

attends(manny_rayner, meeting_22).
attends(yukie_nakao, meeting_22).

attends(lorraine_lemoine, meeting_23).
attends(yukie_nakao, meeting_23).

attends(yumiko_tanaka, meeting_24).
attends(yukie_nakao, meeting_24).
attends(anna_gainer, meeting_24).

attends(marie_sadlik,meeting_25).
attends(yukie_nakao, meeting_25).

attends(beatrice_daille, meeting_26).
attends(yukie_nakao, meeting_26).

attends(marie_sadlik,meeting_27).
attends(yukie_nakao, meeting_27).

attends(beatrice_daille, meeting_28).
attends(yukie_nakao, meeting_28).

attends(manny_rayner, meeting_29).
attends(yukie_nakao, meeting_29).

attends(manny_rayner, meeting_30).
attends(yukie_nakao, meeting_30).

attends(yukie_nakao, meeting_31).
attends(anna_gainer, meeting_31).

attends(yukie_nakao,meeting_32).
attends(lorraine_lemoine, meeting_32).
attends(christine_briand,meeting_32).

attends(attends(manny_rayner, meeting_33).
attends(yukie_nakao, meeting_33).
attends(nikos_tsourakis, meeting_33).
attends(maria_georgescul, meeting_33).
attends(pierrette_bouillon, meeting_33).

attends(manny_rayner, meeting_34).
attends(yukie_nakao, meeting_34).
attends(pierrette_bouillon, meeting_34).

attends(lorraine_lomoine, meeting_35).     
attends(yukie_nakao, meeting_35).
attends(beatrice_daille, meeting_35).

attends(marie_sadlik,meeting_36).
attends(yukie_nakao, meeting_36).


% location(ID, Name, Country, City, Organisation).
location(manny_office, 'manii_no_kenkyuushitsu', suisu, juneevu, juneevu_daigaku).
location(nikos_room_1, 'nikosu_no_heya', suisu, juneevu, juneevu_daigaku).
location(pierrette_room_3, 'pieretto_no_heya', suisu, juneevu, juneevu_daigaku).
location(saint_margarets_road, sento_maagarettsu_roodo, igirisu, kenburijji, null).
location(lorraine_office, 'loreenu_no_kenkyuusitsu', furansu, nanto, nanto_daigaku).
location(yumiko_house, 'yumiko_san_no_ie', furansu, nanto, null).
location(l_1, 'eru_ichi_kaigishitsu', furansu, nanto, ekooru_centoraru_nanto).
location(kageura_office, 'kageura_sensei_no_kenkyuushitsu', nihon, tokyo, tokyo_daigaku).
location(kyoko_room, 'kyoko_no_heya', nihon, kyoto, enuaishiitii).
location(daille_office, 'dairu_sensei_no_kenkyuushitsu', furansu, nanto, nanto_daigaku).
location(hotel_mercure, "hoteru_merukyuuru", furansu, nanto, null)
location(yukie, "nakao_yukie_no_jitaku", furansu, nanto, null).
location(town_geneva, "juneevu_shinai", suisu, juneevu, null).


