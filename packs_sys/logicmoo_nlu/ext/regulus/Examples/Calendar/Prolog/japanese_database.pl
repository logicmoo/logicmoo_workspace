
:- module(database,
	[meeting/7,
	 person/6,
	 attends/2,
	 location/5]
    ).

% meeting(ID, Day, Month, Year, StartTime, EndTime, LocID).
meeting(original_meeting_1, 29, 5, 2007, 9:30, 12:0, nikos_room_1).
meeting(original_meeting_2, 9, 7, 2007, 10:30, 12:0, saint_margarets_road).
meeting(original_meeting_3, 9, 9, 2007, 10:30, 18:0, nikos_room_1).
meeting(original_meeting_4, 10, 9, 2007, 10:30, 18:0, nikos_room_1).
meeting(original_meeting_5, 3, 9, 2007, 16:30, 17:0, pierrette_room_3).
meeting(original_meeting_6, 15, 10, 2007, 10:00, 12:0, pierrette_room_3).
meeting(original_meeting_7, 15, 10, 2007, 14:00, 16:0, sonia_room_1).
meeting(original_meeting_8, 24, 10, 2007, 14:00, 16:0, pierrette_room_3).
meeting(original_meeting_9, 19, 10, 2007, 10:00, 12:0, pierrette_room_3).
meeting(original_meeting_10, 25, 11, 2007, 10:00, 18:00, pierrette_room_3).
meeting(original_meeting_35, 8, 9, 2008, 9:30, 12:0, manny_office).

meeting(new_meeting_1, 25, 2, 2008, 10:00, 11:30, lorraine_office).
meeting(new_meeting_2, 26, 3, 2008, 10:00, 12:30, yumiko_house).
meeting(new_meeting_3, 7, 4, 2008, 14:00, 15:00, lorraine_office).
meeting(new_meeting_4, 11, 4, 2008, 13:15, 15:30, l_1).
meeting(new_meeting_5, 13, 4, 2008, 9:00, 16:00, manny_office).
meeting(new_meeting_6, 14, 5, 2008, 13:30, 15:30, l_1).
meeting(new_meeting_7, 26, 5, 2008, 13:00, 17:00, kageura_office).
meeting(new_meeting_8, 27, 5, 2008, 10:00, 12:00, kyoko_room).
meeting(new_meeting_9, 31, 6, 2008, 10:20, 12:00, kyoko_room).
meeting(new_meeting_10, 21, 7, 2008, 10:00, 11:30, lorraine_office).
meeting(new_meeting_11, 22, 7, 2008, 14:00, 15:00, daille_office).
meeting(new_meeting_12, 11, 8, 2008, 14:00, 16:30, yumiko_house).
meeting(new_meeting_13, 2, 9, 2008, 12:00, 14:00, yukie_house).
meeting(new_meeting_14, 10, 9, 2008, 8:00, 12:00, l_1).
meeting(new_meeting_15, 16, 9, 2008, 14:00, 15:00, l_1).
meeting(new_meeting_16, 20, 9, 2008, 19:00, 21:00, hotel_mercure).
meeting(new_meeting_17, 26, 9, 2008, 9:00, 11:00, yukie_house).
meeting(new_meeting_18, 29, 9, 2008, 18:00, 19:00, pierrette_room_3).
meeting(new_meeting_19, 30, 9, 2008, 9:00, 12:00, manny_office).
meeting(new_meeting_20, 1, 10, 2008, 12:30, 14:00, town_geneva).
meeting(new_meeting_21, 2, 10, 2008, 10:00, 18:00, saint_margarets_road).
meeting(new_meeting_22, 3, 10, 2008, 10:00, 18:00, saint_margarets_road).
meeting(new_meeting_23, 3, 10, 2008, 14:30, 18:00, manny_office).
meeting(new_meeting_24, 7, 10, 2008, 8:00, 12:00, l_1).
meeting(new_meeting_25, 9, 10, 2008, 10:00, 12:00, lorraine_office).
meeting(new_meeting_26, 14, 10, 2008, 14:00, 16:00, yukie_house).
meeting(new_meeting_27, 26, 10, 2008, 10:00, 12:00, lorraine_office).
meeting(new_meeting_28, 2, 11, 2008, 13:30, 15:30, l_1).
meeting(new_meeting_29, 5, 11, 2008, 10:00, 12:00, yukie_house).
meeting(new_meeting_30, 9, 11, 2008, 8:00, 12:00, daille_office).
meeting(new_meeting_31, 20, 11, 2008, 9:00, 11:00, yukie_house).
meeting(new_meeting_32, 30, 11, 2008, 8:00, 12:00, daille_office).
meeting(new_meeting_33, 14, 11, 2008, 8:00, 12:00, pierrette_room_3).
meeting(new_meeting_34, 16, 11, 2008, 10:00, 12:00, manny_office).
meeting(new_meeting_35, 27, 11, 2008, 16:00, 17:00, daille_office).
meeting(new_meeting_36, 8, 12, 2008, 10:00, 11:30, yukie_house).

% person(ID, FirstName, LastName, Affiliation, Phone, Email).
person(agnes_lisowska, agunesu, lisousuka, juneevu, "+41 22 379 8685", "agnes.lisowska@issco.unige.ch").
person(anna_gainer, ana, genaa, nanto, "+33 6 45 98 23 94", "annagainer@wanadoo.fr").
person(beatrice_daille, beatorisu, daiyu, nanto, "+33 2 40 11 22 33 44", "beatrice.daille@univ-nantes.fr").
person(christine_briand, kurisutiinu, burian, paris, "+33 6 45 29 33 00", "christine.briand@univ-nantes.fr").
person(elisabeth_kron, erizabesu, kuron, juneevu, "+44 1223 276 838", "elisabethkron@yahoo.co.uk").
person(masuda_hiroshi, masuda, hiroshi, osaka, "+81 9074 3454", "hiroshi.masuda@univ-osaka.co.jp").
person(isahara_hitoshi,isahara, hitoshi,  kyoto, "+81 742 44 8977", "isahara.hitoshi@nict.co.jp").
person(kageura_kyo,  kageura, kyo, tokyo, "+81 344 4523", "kyo.kageura@univ-tokyo.jp").
person(kanzaki_kyoko, kyoko, kanzaki, kyoto, "+81 742 44 8978", "kanzaki.kyoko@nict.co.jp").
person(lorraine_lemoine, roreenu, lumoan, nanto, "+33 7058 599 385", "lolemoine@hotmail.fr").
person(parent_mariko, paron,mariko,  san_nazeeru, "+33 02 52 90 89 23", "parent.mariko@free.fr").
person(manny_rayner, manii, reinaa, juneevu, "+44 7957 549 339", "Emmanuel.Rayner@issco.unige.ch").
person(maria_georgescul, maria, jorujesukyuru, juneevu, "+41 22 379 8683", "Maria.Georgescul@eti.unige.ch").
person(marianne_santaholma, mariannu, santahoruma, juneevu, "+41 22 379 8679", "Marianne.Santaholma@eti.unige.ch").
person(marianne_starlander, mariannu, sutaarandaa, juneevu, "+41 22 379 8679", "Marianne.Starlander@eti.unige.ch").
person(marie_sadlik, marii, sadorikku, nanto, "+33 2 40 55 49 22", "m.sadlik@hotmail.fr").
person(marita_ailomaa, marita, ailomaa, roozannu, "+41 21 693 5274", "marita.ailomaa@epfl.ch").
person(martin_rajman, maatin, rajiman, roozannu, "+41 21 693 5274", "martin.rajman@epfl.ch").
person(nikos_tsourakis, nikosu, tsurakisu, juneevu, "+41 22 379 8683", "Nikos.Tsourakis@issco.unige.ch").
person(pierrette_bouillon, pieretto, buiyon, juneevu, "+41 22 379 8679", "Pierrette.Bouillon@issco.unige.ch").
person(sonia_halimi, sonia, harimi, juneevu, "+44 7957 549 339", "sonia.halimi@eti.unige.ch").
person(susan_armstrong, suuzan, aamusutorongu, juneevu, "+41 22 379 8679", "Susan.Armstrong@issco.unige.ch").
person(nakao_yukie,  nakao, yukie,nanto, "+33 7957 549 335", "ynakao1922@hotmail.com").
person(tanaka_yumiko, tanaka,yumiko,  nanto, "+33 4593 498 008", "y0909ta@yahoo.co.jp").

% attends(PersonID, MeetingID).
attends(pierrette_bouillon, original_meeting_1).
attends(maria_georgescul, original_meeting_1).
attends(nikos_tsourakis, original_meeting_1).
attends(manny_rayner, original_meeting_1).

attends(pierrette_bouillon, original_meeting_2).
attends(elisabeth_kron, original_meeting_2).
attends(manny_rayner, original_meeting_2).

attends(pierrette_bouillon, original_meeting_3).
attends(maria_georgescul, original_meeting_3).
attends(nikos_tsourakis, original_meeting_3).
attends(manny_rayner, original_meeting_3).

attends(pierrette_bouillon, original_meeting_4).     
attends(elisabeth_kron, original_meeting_4).
attends(manny_rayner, original_meeting_4).

attends(pierrette_bouillon, original_meeting_5).     
attends(nikos_tsourakis, original_meeting_5).

attends(nakao_yukie, original_meeting_6).     
attends(sonia_halimi, original_meeting_6).
attends(pierrette_bouillon, original_meeting_6).

attends(nakao_yukie, original_meeting_7).     
attends(sonia_halimi, original_meeting_7).

attends(pierrette_bouillon, original_meeting_8).     
attends(marianne_santaholma, original_meeting_8).

attends(pierrette_bouillon, original_meeting_9).     
attends(agnes_lisowska, original_meeting_9).

attends(pierrette_bouillon, original_meeting_10).     
attends(nakao_yukie, original_meeting_10).
attends(manny_rayner, original_meeting_10).

attends(manny_rayner, original_meeting_35).
attends(nakao_yukie, original_meeting_35).

% New meetings from Yukie

attends(nakao_yukie, new_meeting_1).
attends(lorraine_lemoine, new_meeting_1).

attends(tanaka_yumiko, new_meeting_2).
attends(nakao_yukie, new_meeting_2).

attends(beatrice_daille, new_meeting_3).
attends(lorraine_lemoine, new_meeting_3).
attends(christine_briand, new_meeting_3).

attends(tanaka_yumiko, new_meeting_4).
attends(nakao_yukie, new_meeting_4).
attends(anna_gainer, new_meeting_4).

attends(kanzaki_kyoko, new_meeting_5).     
attends(isahara_hitoshi, new_meeting_5).
attends(manny_rayner, new_meeting_5).

attends(nakao_yukie, new_meeting_6).     
attends(tanaka_yumiko, new_meeting_6).

attends(nakao_yukie, new_meeting_7).     
attends(beatrice_daille, new_meeting_7).
attends(kageura_kyo, new_meeting_7).

attends(isahara_hitoshi, new_meeting_8).     
attends(kanzaki_kyoko, new_meeting_8).

attends(masuda_hiroshi, new_meeting_9).     
attends(isahara_hitoshi, new_meeting_9).
attends(kanzaki_kyoko, new_meeting_9).

attends(lorraine_lemoine, new_meeting_10).     
attends(nakao_yukie, new_meeting_10).

attends(beatrice_daille, new_meeting_11).
attends(christine_briand, new_meeting_11).

attends(tanaka_yumiko, new_meeting_12).
attends(nakao_yukie, new_meeting_12).

attends(nakao_yukie, new_meeting_13).
attends(parent_mariko, new_meeting_13).

attends(parent_mariko, new_meeting_14).
attends(tanaka_yumiko, new_meeting_14).

attends(masuda_hiroshi, new_meeting_15).
attends(parent_mariko, new_meeting_15).

attends(masuda_hiroshi, new_meeting_16).
attends(nakao_yukie, new_meeting_16).

attends(marie_sadlik,new_meeting_17).
attends(nakao_yukie, new_meeting_17).
 
attends(manny_rayner, new_meeting_18).
attends(masuda_hiroshi, new_meeting_18).
attends(nakao_yukie, new_meeting_18).
attends(pierrette_bouillon, new_meeting_18).

attends(manny_rayner, new_meeting_19).
attends(nakao_yukie, new_meeting_19).
 
attends(manny_rayner, new_meeting_20).
attends(nakao_yukie, new_meeting_20).
attends(nikos_tsourakis, new_meeting_20).
attends(maria_georgescul, new_meeting_20).
attends(pierrette_bouillon, new_meeting_20).

attends(manny_rayner, new_meeting_21).
attends(nakao_yukie, new_meeting_21).

attends(manny_rayner, new_meeting_22).
attends(nakao_yukie, new_meeting_22).

attends(lorraine_lemoine, new_meeting_23).
attends(nakao_yukie, new_meeting_23).

attends(tanaka_yumiko, new_meeting_24).
attends(nakao_yukie, new_meeting_24).
attends(anna_gainer, new_meeting_24).

attends(marie_sadlik,new_meeting_25).
attends(nakao_yukie, new_meeting_25).

attends(beatrice_daille, new_meeting_26).
attends(pierrette_bouillon, new_meeting_26). 
attends(nakao_yukie, new_meeting_26).

attends(marie_sadlik,new_meeting_27).
attends(nakao_yukie, new_meeting_27).

attends(beatrice_daille, new_meeting_28).
attends(nakao_yukie, new_meeting_28).

attends(manny_rayner, new_meeting_29).
attends(nakao_yukie, new_meeting_29).

attends(manny_rayner, new_meeting_30).
attends(nakao_yukie, new_meeting_30).

attends(nakao_yukie, new_meeting_31).
attends(anna_gainer, new_meeting_31).

attends(nakao_yukie, new_meeting_32).
attends(lorraine_lemoine, new_meeting_32).
attends(christine_briand,new_meeting_32).

attends(manny_rayner, new_meeting_33).
attends(nakao_yukie, new_meeting_33).
attends(nikos_tsourakis, new_meeting_33).
attends(maria_georgescul, new_meeting_33).
attends(pierrette_bouillon, new_meeting_33).

attends(manny_rayner, new_meeting_34).
attends(nakao_yukie, new_meeting_34).
attends(pierrette_bouillon, new_meeting_34).

attends(lorraine_lemoine, new_meeting_35).     
attends(nakao_yukie, new_meeting_35).
attends(beatrice_daille, new_meeting_35).
attends(manny_rayner, new_meeting_35).

attends(marie_sadlik,new_meeting_36).
attends(nakao_yukie, new_meeting_36).

% location(ID, Name, Country, City, Organisation).
location(daille_office, 'daiyu_sensei_no_kenkyuushitsu', furansu, nanto, nanto_daigaku).
location(epfl_room, 'eepiiefueru_no_kenkyuushitsu', suisu, roozannu, eepiiefueru).
location(hotel_mercure, 'hoteru_merukyuuru', furansu, nanto, null).
location(idiap_room_1, 'iidiiappu_no_heya', suisu, marutini, iidiiappu).
location(kageura_office, 'kageura_sensei_no_kenkyuushitsu', nihon, tokyo, tokyo_daigaku).
location(kyoko_room, 'kyoko_no_heya', nihon, kyoto, enuaishiitii).
location(l_1, 'eru_ichi_kaigishitsu', furansu, nanto, ekooru_centoraru_nanto).
location(lorraine_office, 'loreenu_no_kenkyuusitsu', furansu, nanto, nanto_daigaku).
location(manny_office, 'manii_no_kenkyuushitsu', suisu, juneevu, juneevu_daigaku).
location(nikos_room_1, 'nikosu_no_kenkyuushitsu', suisu, juneevu, juneevu_daigaku).
location(pierrette_room_3, 'pieretto_no_kenkyuushitsu', suisu, juneevu, juneevu_daigaku).
location(saint_margarets_road, sento_maagarettsu_roodo, igirisu, kenburijji, null).
location(sonia_room_1, 'sonia_no_kenkyuushitsu', suisu, juneevu, juneevu_daigaku).
location(town_geneva, 'juneevu_shinai', suisu, juneevu, null).
location(yukie_house, 'nakao_yukie_no_jitaku', furansu, nanto, null).
location(yumiko_house, 'yumiko_san_no_ie', furansu, nanto, null).
