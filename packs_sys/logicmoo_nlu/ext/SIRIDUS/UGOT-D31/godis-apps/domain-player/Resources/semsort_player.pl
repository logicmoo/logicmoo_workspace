/*************************************************************************

         name: semsort_player.pl 
	 date: 2004-10-25
       author: Andreas Wallentin
 
*************************************************************************/
%:-multifile sem_sort/2.

%:- ensure_loaded( stations_player ).
:- use_module( dbase, [song_list/1,
		       group_in_db/1,
		       album_in_db/1,
		       station_in_db/1] ).

%:- ensure_loaded( [albums,songs,groups,digits_svenska_player,digits_english_player] ).
:- use_module(library(lists),[append/3,
			      member/2,
			      is_list/1]).

/*----------------------------------------------------------------------
    sort_restr( +Prop )

    Prop fulfils the sortal restrictions on propositions
----------------------------------------------------------------------*/

sem_sort( english, language ).
sem_sort( svenska, language ).

sem_sort( player, domain ).

% sem_sort(Name,name):-
% 	name(Name).

%%%   ACTIONS
sem_sort( restart,           action ).
sem_sort( handle_player,     action ).
sem_sort( handle_playlist,   action ).
sem_sort( handle_stations,   action ).

sem_sort( start,             action ).
sem_sort( start_specific,    action ).
sem_sort( stop,              action ).
sem_sort( pause,             action ).
sem_sort( resume,            action ).
sem_sort( fast_rewind,       action ).
sem_sort( play_playlist,     action ).
sem_sort( fast_forward,      action ).
sem_sort( rewind,            action ).

sem_sort( playlist_add,      action ).
sem_sort( playlist_del,      action ).
sem_sort( playlist_del_specific,  action ).
sem_sort( playlist_shuffle,  action ).
sem_sort( remove,            action ).
sem_sort( listen_to,         action ).

sem_sort( vol_up,            action ).
sem_sort( vol_down,          action ).
sem_sort( next_song,         action ).
sem_sort( previous_song,     action ).

sem_sort( show_list,         action ).


sem_sort( next, index).
sem_sort( previous, index).
sem_sort( 1, index).
sem_sort( 2, index).
sem_sort( 3, index).
sem_sort( 4, index).
sem_sort( 5, index).
sem_sort( 6, index).
sem_sort( 7, index).
sem_sort( 8, index).
sem_sort( 9, index).
sem_sort( 10, index).
sem_sort( 11, index).
sem_sort( 12, index).
sem_sort( 13, index).

sem_sort( marvin_gaye , group ).
sem_sort( atomic_swing , group ).
sem_sort( madonna, group).
sem_sort( enigma , group ). 
sem_sort( europe , group ). 
sem_sort( garbage , group ). 
sem_sort( jam , group ). 
sem_sort( kate_bush , group ). 
sem_sort( lee_morgan , group ). 
sem_sort( massive_attack , group ). 
sem_sort( morlocks , group ). 
sem_sort( mr_vegas , group ). 
sem_sort( mudhoney , group ). 
sem_sort( nitzer_ebb , group ). 
sem_sort( pain, group ). 
sem_sort( pet_shop_boys , group ). 
sem_sort( pixies , group ). 
sem_sort( prodigy , group ). 
sem_sort( project_pitchfork , group ). 
sem_sort( stephen_simmonds , group ).
sem_sort( ark , group ).
sem_sort( trance_dance , group ).
sem_sort( vnv_nation , group ).
sem_sort( ace_of_base , group ).
sem_sort( beborn_beton , group ).
sem_sort( clash , group ).
sem_sort( covenant , group ).
sem_sort( creeps , group ).
sem_sort( cure , group ).
sem_sort( eagle_eye_cherry , group ).


sem_sort(wilmer_x,group).
sem_sort(uno_svenningsson,group).
sem_sort(ulf_lundell,group).
sem_sort(tomas_ledin,group).
sem_sort(tomas_ledin,group).
sem_sort(thomas_di_leva,group).
sem_sort(staffan_hellstrand,group).
sem_sort(petter,group).
sem_sort(peter_lemarc,group).
sem_sort(peter_lemarc,group).
sem_sort(patrik_isaksson,group).
sem_sort(orup,group).
sem_sort(monica_törnell,group).
sem_sort(mikael_wiehe,group).
sem_sort(mikael_rickfors,group).
sem_sort(mauro_scocco,group).
sem_sort(mauro_scocco,group).
sem_sort(marie_fredriksson,group).
sem_sort(lustans_lakejer,group).
sem_sort(lisa_nilsson,group).
sem_sort(lisa_ekdahl,group).
sem_sort(lars_winnerbäck,group).
sem_sort(kent,group).
sem_sort(jakob_hellman,group).
sem_sort(irma,group).
sem_sort(imperiet,group).
sem_sort(gyllene_tider,group).
sem_sort(freda,group).
sem_sort(eva_dahlgren,group).
sem_sort(eva_dahlgren,group).
sem_sort(eldkvarn,group).
sem_sort(ebba_grön,group).
sem_sort(docent_död,group).
sem_sort(christer_sandelin,group).
sem_sort(bo_kaspers_orkester,group).
sem_sort(annelie_ryde,group).
sem_sort(adolphson_och_falk,group).


%item
sem_sort( all_that_she_wants, item ).
sem_sort( stone_me_into_the_groove , item ).
sem_sort( another_world , item ).
sem_sort( deeper_than_the_usual_feeling , item ).
sem_sort( london_calling , item ).
sem_sort( should_i_stay_or_should_i_go , item ).
sem_sort( dead_stars , item ).
sem_sort( figurehead , item ).
sem_sort( leviathan , item ).
sem_sort( like_tears_in_rain , item ).
sem_sort( stalker , item ).
sem_sort( oh_i_like_it , item ).
sem_sort( friday_im_in_love , item ).
sem_sort( save_tonight , item ).
sem_sort( sadness , item ).
sem_sort( the_final_countdown , item ).
sem_sort( i_think_im_paranoid , item ).
sem_sort( in_the_city , item ).
sem_sort( time_for_truth , item ).
sem_sort( the_man_with_the_child_in_his_eyes , item ).
sem_sort( totem_pole , item ).
sem_sort( like_a_prayer , item ).
sem_sort( lucky_star , item ).
sem_sort( material_girl , item ).
sem_sort( if_i_should_die_tonight , item ).
sem_sort( angel , item ).
sem_sort( sly , item ).
sem_sort( teardrop , item ).
sem_sort( ars_magica , item ).
sem_sort( razors_through_flesh , item ).
sem_sort( sex_by_force , item ).
sem_sort( heads_high , item ).
sem_sort( latest_news , item ).
sem_sort( good_enough , item ).
sem_sort( thorn , item ).
sem_sort( let_beauty_loose , item ).
sem_sort( eleanor_rigby , item ).
sem_sort( suburbia , item ).
sem_sort( west_end_girls , item ).
sem_sort( debaser , item ).
sem_sort( poison , item ).
sem_sort( existence , item ).
sem_sort( tears_never_dry , item ).
sem_sort( it_takes_a_fool_to_remain_sane , item ).
sem_sort( youre_gonna_get_it , item ).
sem_sort( i_breathe , item ).
sem_sort( darkangel , item ).
sem_sort( legion , item ).
sem_sort( rubicon , item ).
sem_sort( standing , item ).

sem_sort(teknikens_under,item).
sem_sort(under_ytan,item).
sem_sort(öppna_landskap,item).
sem_sort(sommaren_är_kort,item).
sem_sort(en_del_av_mitt_hjärta,item).
sem_sort(vem_skall_jag_tro_på,item).
sem_sort(lilla_fågel_blå,item).
sem_sort(vinden_har_vänt,item).
sem_sort(säg_som_det_är,item).
sem_sort(håll_om_mig,item).
sem_sort(du_får_göra_som_du_vill,item).
sem_sort(jag_blir_hellre_jagad_av_vargar,item).
sem_sort(vintersaga,item).
sem_sort(flickan_och_kråkan,item).
sem_sort(vingar,item).
sem_sort(sarah,item).
sem_sort(det_finns,item).
sem_sort(efter_stormen,item).
sem_sort(diamanter,item).
sem_sort(himlen_runt_hörnet,item).
sem_sort(vem_vet,item).
sem_sort(kom_ihåg_mig,item).
sem_sort(om_du_var_här,item).
sem_sort(vara_vänner,item).
sem_sort(precis_som_du,item).
sem_sort(du_ska_va_president,item).
sem_sort(flickorna_på_tv_två,item).
sem_sort(vindarna,item).
sem_sort(ängeln_i_rummet,item).
sem_sort(vem_tänder_stjärnorna,item).
sem_sort(kärlekens_tunga,item).
sem_sort(åtta_hundra_grader,item).
sem_sort(solglasögon,item).
sem_sort(det_hon_vill_ha,item).
sem_sort(undantag,item).
sem_sort(segla_på_ett_moln,item).
sem_sort(blinkar_blå,item).

sem_sort( 'http://129.16.159.166:8000', station ).
sem_sort( 'http://130.240.207.88:9090', station ).
sem_sort( 'http://chatradio.myftp.org:10010', station ).

sem_sort( [a,punkt,mptreu], playlist ).

sem_sort( the_immaculate_collection, album ).

/*--------------------
conceptual hierarichy
--------------------*/

%isa(station,item).
isa(itemAdd,item).
%isa(itemRem,item).
isa(song,item).

isa(itemRem,index).
isa(what_to_play,index).

%isa(solo,artist).
isa(artist,group).
isa(song_artist,group).

%isa(artist,artist).
isa(groupToAdd,group).
isa(composer,group).

isa(genreOfArtist, genre).
isa(genreOfMusic, genre).


isa( T0, T2 ):-
	T0 \= T2,
	isa( T0, T1 ),
	isa( T1, T2 ).

