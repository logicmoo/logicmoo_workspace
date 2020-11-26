--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/swedish:resource-1.0/scandinavian

concrete MusicArtistsSwe of MusicArtists = open Prelude, CatSwe, GodisLangSwe in {

lincat Artist = NP;

lin

-- Swedish artists
abba = plur_NP ["abba"];
adolphson_och_falk = plur_NP ["adolphson och falk"];
anne_lie_ryde = sing_NP ["anne-lie rydé"];
bo_kaspers_orkester = plur_NP (variants { ["bo kaspers orkester"]; ["bosse k"] });
christer_sandelin = plur_NP ["christer sandelin"];
dan_hylander_och_rmb = sing_NP (variants { ["dan hylander och rmb"]; ["dan hylander"]; ["dan hylander och raj montana band"]});
di_leva = sing_NP (variants { ["tomas di leva"]; ["di leva"] });
dilba = sing_NP ["dilba"];
docent_dod = plur_NP ["docent död"];
dr_alban = sing_NP ["doktor alban"];
ebba_gron = plur_NP ["ebba grön"];
eldkvarn = plur_NP ["eldkvarn"];
eric_gadd = sing_NP ["eric gadd"];
eva_dahlgren = sing_NP ["eva dahlgren"];
freda = plur_NP ["freda"];
gyllene_tider = plur_NP ["gyllene tider"];
imperiet = plur_NP ["imperiet"];
irma = sing_NP ["irma"];
jakob_hellman = sing_NP ["jakob hellman"];
jumper = plur_NP ["jumper"];
kent = plur_NP ["kent"];
lars_winnerback = sing_NP ["lars winnerbäck"];
lisa_ekdahl = sing_NP ["lisa ekdahl"];
lisa_nilsson = sing_NP ["lisa nilsson"];
lolita_pop = plur_NP ["lolita pop"];
lustans_lakejer = plur_NP ["lustans lakejer"];
marie_fredriksson = sing_NP ["marie fredriksson"];
mauro_scocco = sing_NP ["mauro scocco"];
mikael_rickfors = sing_NP ["mikael rickfors"];
mikael_wiehe = sing_NP ["mikael wiehe"];
monica_tornell = sing_NP ["monica törnell"];
nordman = plur_NP ["nordman"];
norum_och_nilsson = plur_NP ["norum och nilsson"];
orup = sing_NP ["orup"];
patrik_isaksson = sing_NP ["patrik isaksson"];
peter_lemarc = sing_NP ["peter lemarc"];
petter = sing_NP ["petter"];
peps_persson = sing_NP ["peps persson"];
ratata = plur_NP ["ratata"];
robyn = sing_NP ["robyn"];
roger_pontare = sing_NP ["roger pontare"];
roxette = plur_NP ["roxette"];
staffan_hellstrand = sing_NP ["staffan hellstrand"];
stakka_bo = plur_NP ["stakka_bo"];
stefan_andersson = sing_NP ["stefan andersson"];
stina_nordenstam = sing_NP ["stina nordenstam"];
tomas_ledin = sing_NP ["tomas ledin"];
ulf_lundell = sing_NP ["ulf lundell"];
uno_svenningsson = sing_NP ["uno svenningsson"];
viba_femba = plur_NP ["viba femba"];

-- 'English' artists
-- Artist using enlish pronounciation
-- The swedish Nuance TTS can't pronounce anything useful of english names.
-- The empty variants guarantees that the concrete grammar fulfills the abstract syntax
-- without yielding any linearizations.
beatles = variants{}; --["beatles"];
beborn_beton = variants{}; --["beborn beton"];
clash = variants{}; --["clash"];
covenant = variants{}; --["covenant"];
cure = variants{}; --["cure"];
dolly_parton = variants{}; --["dolly parton"];
enigma = variants{}; --["enigma"];
enya  = sing_NP ["enya"];
garbage = variants{}; --["garbage"];
god_speed_black_emperor = variants{}; --["god speed you black emperor"];
jam = variants{}; --["jam"];
kate_bush = variants{}; --["kate bush"];
lee_morgan = variants{}; --["lee morgan"];
madonna = variants{}; --["madonna"];
marvin_gaye = variants{}; --["marvin gaye"];
massive_attack = variants{}; --["massive attack"];
michael_jackson = variants{}; --["michael jackson"];
morlocks = variants{}; --["morlocks"];
morrisey = variants{}; --["morrisey"];
mr_vegas = variants{}; --["mr vegas"];
mudhoney = variants{}; --["mudhoney"];
nitzer_ebb = variants{}; --["nitzer ebb"];
pain = variants{}; --["pain"];
pet_shop_boys = variants{}; --["pet shop boys"];
pixies = variants{}; --["pixies"];
pink_floyd = variants{}; --["pink floyd"];
prodigy = variants{}; --["prodigy"];
project_pitchfork = variants{}; --["project pitchfork"];
rolling_stones = variants{}; --["rolling stones"];
vnv_nation = variants{}; --["vnv nation"];
britney_spears = variants{}; --["britney spears"];
usher = variants{}; --["usher"];
fifty_cent = variants{}; --["fifty cent"];
green_day = variants{}; --["green day"];
billy_idol = variants{}; --["billy idol"];
elvis_presley = variants{}; --["elvis presley"];
shania_twain = variants{}; --["shania twain"];
trace_adkins = variants{}; --["trace adkins"];
chemical_brothers = variants{}; --["the chemical brothers"];
eminem = variants{}; --["eminem"];
ozzy_osbourne = variants{}; --["ozzy osbourne"];
norah_jones = variants{}; --["norah jones"];
jessica_simpson = variants{}; --["jessica simpson"];
guns_and_roses = variants{}; --["guns and roses"];
europe = variants{}; --["europe"];
led_zeppelin = variants{}; --["led zeppelin"];
cardigans = variants{}; --["the cardigans"];
ace_of_base = variants{}; --["ace of base"];
atomic_swing = variants{}; --["atomic swing"];
jessica_simpson = variants{}; --["jessica simpson"];
creeps = variants{}; --["creeps"];
eagle_eye_cherry = variants{}; --["eagle eye cherry"];
stephen_simmonds = variants{}; --["stephen simmonds"];
ark = variants{}; --["the ark"];
trance_dance = variants{}; --["trance dance"];
vacuum = variants{}; --["vacuum"];

}
