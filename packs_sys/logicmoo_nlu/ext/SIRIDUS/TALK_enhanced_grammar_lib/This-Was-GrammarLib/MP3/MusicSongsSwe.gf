--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/swedish:resource-1.0/scandinavian

concrete MusicSongsSwe of MusicSongs = open Prelude, CatSwe, GodisLangSwe in {

lincat Song = NP;

lin

-- Swedish titles
blinkar_bla = sing_NP ["blinkar blå"];
segla_pa_ett_moln = sing_NP ["segla på ett moln"];
undantag = sing_NP ["undantag"];
det_hon_vill_ha = sing_NP ["det hon vill ha"];
skuggor_i_skymningen = sing_NP ["skuggor i skymningen"];
vem_ska_jag_tro_pa = sing_NP ["vem ska jag tro på"];
solglasogon = sing_NP ["solglasögon"];
atta_hundra_grader = sing_NP ["åtta hundra grader"];
karlekens_tunga = sing_NP ["kärlekens tunga"];
angeln_i_rummet = sing_NP ["ängeln i rummet"];
vem_tander_stjarnorna = sing_NP ["vem tänder stjärnorna"];
vindarna = sing_NP ["vindarna"];
flickorna_pa_tv_tva = sing_NP ["flickorna på tv två"];
du_ska_va_president = sing_NP ["du ska va president"];
precis_som_du = sing_NP ["precis som du"];
vara_vanner = sing_NP ["vara vänner"];
om_du_var_har = sing_NP ["om du var här"];
kom_ihag_mig = sing_NP ["kom ihåg mig"];
vem_vet = sing_NP ["vem vet"];
himlen_runt_hornet = sing_NP ["himlen runt hörnet"];
diamanter = sing_NP ["diamanter"];
efter_stormen = sing_NP ["efter stormen"];
det_finns = sing_NP ["det finns"];
sarah = sing_NP ["sarah"];
vingar = sing_NP ["vingar"];
flickan_och_krakan = sing_NP ["flickan och kråkan"];
vintersaga = sing_NP ["vintersaga"];
allt_som_jag_kanner = sing_NP ["allt som jag känner"];
jag_blir_hellre_jagad_av_vargar = sing_NP ["jag blir hellre jagad av vargar"];
du_far_gora_som_du_vill = sing_NP ["du får göra som du vill"];
hall_om_mig = sing_NP ["håll om mig"];
sag_som_det_ar = sing_NP ["säg som det är"];
vinden_har_vant = sing_NP ["vinden har vänt"];
jackie = sing_NP ["jackie"];
lilla_fagel_bla = sing_NP ["lilla fågel blå"];
sommaren_ar_kort = sing_NP ["sommaren är kort"];
en_del_av_mitt_hjarta = sing_NP ["en del av mitt hjärta"];
oppna_landskap = sing_NP ["öppna landskap"];
under_ytan = sing_NP ["under ytan"];
teknikens_under = sing_NP ["teknikens under"];
tva_av_oss = sing_NP ["två av oss"];
vandraren = sing_NP ["vandraren"];
under_norrskenet = sing_NP ["under norrskenet"];
vill_ha_mer = sing_NP ["vill ha mer"];
flykting = sing_NP ["flykting"];
sanningens_krigare = sing_NP ["sanningens krigare"];
i_vargens_spar = sing_NP ["i vargens spår"];
nu_tandas_tusen_julejus = sing_NP ["nu tändas tusen julejus"];
alla_alskar_dig = sing_NP ["alla älskar dig"];
hon_far = sing_NP ["hon får"];

-- English titles
-- The swedish Nuance TTS can't pronounce anything useful of english names.
-- The empty variants guarantees that the concrete grammar fulfills the abstract syntax
-- without yielding any linearizations.
another_world = variants{}; -- ["another world"];
deeper_than_the_usual_feeling = variants{}; -- ["deeper than the usual feeling"];
london_calling = variants{}; -- ["london calling"];
should_i_stay_or_should_i_go = variants{}; -- ["should i stay or should i go"];
dead_stars = variants{}; -- ["dead stars"];
like_tears_in_rain = variants{}; -- ["like tears in rain"];
figurehead = variants{}; -- ["figurehead"];
final_countdown = variants{};
leviathan = variants{}; -- ["leviathan"];
stalker = variants{}; -- ["stalker"];
friday_im_in_love = variants{}; -- ["friday im in love"];
sadness = variants{}; -- ["sadness"];
i_think_im_paranoid = variants{}; -- ["i think im paranoid"];
in_the_city = variants{}; -- ["in the city"];
time_for_truth = variants{}; -- ["time for truth"];
the_man_with_the_child_in_his_eyes = variants{}; -- ["the man with the child in his eyes"];
totem_pole = variants{}; -- ["totem pole"];
lucky_star = variants{}; -- ["lucky star"];
material_girl = variants{}; -- ["material girl"];
if_i_should_die_tonight = variants{}; -- ["if i should die tonight"];
angel = variants{}; -- ["angel"];
teardrop = variants{}; -- ["teardrop"];
sly = variants{}; -- ["sly"];
sex_by_force = variants{}; -- ["sex by force"];
ars_magica = variants{}; -- ["ars magica"];
razors_through_flesh = variants{}; -- ["razors through flesh"];
heads_high = variants{}; -- ["heads high"];
latest_news = variants{}; -- ["latest news"];
good_enough = variants{}; -- ["good enough"];
thorn = variants{}; -- ["thorn"];
let_beauty_loose = variants{}; -- ["let beauty loose"];
eleanor_rigby = variants{}; -- ["eleanor rigby"];
west_end_girls = variants{}; -- ["west end girls"];
suburbia = variants{}; -- ["suburbia"];
debaser = variants{}; -- ["debaser"];
poison = variants{}; -- ["poison"];
existence = variants{}; -- ["existence"];
darkangel = variants{}; -- ["darkangel"];
rubicon = variants{}; -- ["rubicon"];
all_that_she_wants = variants{}; -- ["all that she wants"];
the_final_countdown = variants{}; -- ["the final countdown"];
stone_me_into_the_groove = variants{}; -- ["stone me into the groove"];
oh_i_like_it = variants{}; -- ["oh i like it"];
save_tonight = variants{}; -- ["save tonight"];
tears_never_dry = variants{}; -- ["tears never dry"];
it_takes_a_fool_to_remain_sain = variants{}; -- ["it takes a fool to remain sane"];
youre_gonna_get_it = variants{}; -- ["youre gonna get it"];
i_breathe = variants{}; -- ["i breathe"];
legion = variants{}; -- ["legion"];
standing = variants{}; -- ["standing"];
like_a_prayer = variants{}; -- ["like a prayer"];

}
