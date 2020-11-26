--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/english

concrete BookingEventsEng of BookingEvents = 
    open Prelude, CatEng, GodisLangEng in {

lincat Event = NP;

lin

meeting = sing_NP ["a meeting"];
movie = sing_NP ["a movie"];

}
