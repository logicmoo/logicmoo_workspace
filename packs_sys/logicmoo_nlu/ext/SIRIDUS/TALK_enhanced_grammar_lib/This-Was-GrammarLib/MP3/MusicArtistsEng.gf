--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/english

concrete MusicArtistsEng of MusicArtists = open Prelude, CatEng, GodisLangEng in {

lincat Artist = NP;

lin

-- Swedish
-- The english Nuance TTS can't pronounce anything useful of swedish names.
-- The empty variants guarantees that the concrete grammar fulfills the abstract syntax
-- without yielding any linearizations.   
abba = variants{}; -- ["abba"];
adolphson_och_falk = variants{}; -- ["adolphson and falk"];
anne_lie_ryde = variants{}; -- ["anne-lie rydé"];
bo_kaspers_orkester = variants{}; -- ["bo kasper's orchestra"];
christer_sandelin = variants{}; -- ["christer sandelin"];
dan_hylander_och_rmb = variants{}; -- variants { ["dan hylander"]; ["dan hylander and raj montana band"]};
di_leva = variants{}; -- variants { ["tomas di leva"]; ["di leva"] };
dilba = variants{}; -- ["dilba"];
docent_dod = variants{}; -- ["docent dead"];
dr_alban = variants{}; -- ["doktor alban"];
ebba_gron = variants{}; -- ["ebba green"];
eldkvarn = variants{}; -- ["eldkvarn"];
eric_gadd = variants{}; -- ["eric gadd"];
eva_dahlgren = variants{}; -- ["eva dahlgren"];
freda = variants{}; -- ["freda"];
gyllene_tider = variants{}; -- ["gyllene tider"];
imperiet = variants{}; -- ["imperiet"];
irma = variants{}; -- ["irma"];
jakob_hellman = variants{}; -- ["jakob hellman"];
jumper = variants{}; -- ["jumper"];
kent = variants{}; -- ["kent"];
lars_winnerback = variants{}; -- ["lars winnerbäck"];
lisa_ekdahl = variants{}; -- ["lisa ekdahl"];
lisa_nilsson = variants{}; -- ["lisa nilsson"];
lolita_pop = variants{}; -- ["lolita pop"];
lustans_lakejer = variants{}; -- ["lustans lakejer"];
marie_fredriksson = variants{}; -- ["marie fredriksson"];
mauro_scocco = variants{}; --  ["mauro scocco"];
mikael_rickfors = variants{}; --  ["mikael rickfors"];
mikael_wiehe = variants{}; --  ["mikael wiehe"];
monica_tornell = variants{}; --  ["monica törnell"];
nordman = variants{}; --  ["nordman"];
norum_och_nilsson = variants{}; --  ["norum och nilsson"];
orup = variants{}; --  ["orup"];
patrik_isaksson = variants{}; --  ["patrik isaksson"];
peter_lemarc = variants{}; --  ["peter lemarc"];
petter = variants{}; --  ["petter"];
peps_persson = variants{}; --  ["peps persson"];
ratata = variants{}; --  ["ratata"];
robyn = variants{}; --  ["robyn"];
roger_pontare = variants{}; --  ["roger pontare"];
roxette = variants{}; --  ["roxette"];
staffan_hellstrand = variants{}; --  ["staffan hellstrand"];
stakka_bo = variants{}; --  ["stakka_bo"];
stefan_andersson = variants{}; --  ["stefan andersson"];
stina_nordenstam = variants{}; --  ["stina nordenstam"];
tomas_ledin = variants{}; --  ["tomas ledin"];
ulf_lundell = variants{}; --  ["ulf lundell"];
uno_svenningsson = variants{}; --  ["uno svenningsson"];
viba_femba = variants{}; --  ["viba femba"];

-- English
beatles = plur_NP ["the beatles"];
beborn_beton = plur_NP ["beborn beton"];
clash = plur_NP ["the clash"];
covenant = plur_NP ["the covenant"];
cure = plur_NP ["the cure"];
dolly_parton = sing_NP ["dolly parton"];
enigma = plur_NP ["enigma"];
enya = sing_NP ["enya"];
garbage = plur_NP ["garbage"];
god_speed_black_emperor = plur_NP ["god speed you black emperor"];
jam = plur_NP ["the jam"];
kate_bush = sing_NP ["kate bush"];
lee_morgan = sing_NP ["lee morgan"];
madonna = sing_NP ["madonna"];
marvin_gaye = sing_NP ["marvin gaye"];
massive_attack = plur_NP ["massive attack"];
michael_jackson = sing_NP ["michael jackson"];
morlocks = plur_NP ["the morlocks"];
morrisey = sing_NP ["morrisey"];
mr_vegas = sing_NP ["mr vegas"];
mudhoney = plur_NP ["mudhoney"];
nitzer_ebb = plur_NP ["nitzer ebb"];
pain = plur_NP ["pain"];
pet_shop_boys = plur_NP ["the pet shop boys"];
pink_floyd = plur_NP ["pink floyd"];
pixies = plur_NP ["the pixies"];
prodigy = plur_NP ["prodigy"];
project_pitchfork = plur_NP ["project pitchfork"];
rolling_stones = plur_NP ["the rolling stones"];
vnv_nation = plur_NP ["vnv nation"];
britney_spears = sing_NP ["britney spears"];
usher = plur_NP ["usher"];
fifty_cent = sing_NP ["fifty cent"];
green_day = plur_NP ["green day"];
billy_idol = sing_NP ["billy idol"];
elvis_presley = sing_NP ["elvis presley"];
shania_twain = sing_NP ["shania twain"];
trace_adkins = sing_NP ["trace adkins"];
chemical_brothers = plur_NP ["the chemical brothers"];
eminem = sing_NP ["eminem"];
ozzy_osbourne = sing_NP ["ozzy osbourne"];
norah_jones = sing_NP ["norah jones"];
jessica_simpson = sing_NP ["jessica simpson"];
guns_and_roses = plur_NP ["the guns and roses"];
europe = plur_NP ["the europe"];
led_zeppelin = plur_NP ["led zeppelin"];
cardigans = plur_NP ["the cardigans"];
ace_of_base = plur_NP ["the ace of base"];
atomic_swing = plur_NP ["atomic swing"];
jessica_simpson = sing_NP ["jessica simpson"];
creeps = plur_NP ["the creeps"];
eagle_eye_cherry = sing_NP ["eagle eye cherry"];
stephen_simmonds = sing_NP ["stephen simmonds"];
ark = plur_NP ["the ark"];
trance_dance = plur_NP ["trance dance"];
vacuum = plur_NP ["vacuum"];

}
