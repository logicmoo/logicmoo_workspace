--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/english

concrete BookingDatesEng of BookingDates = 
	open ResEng, GodisLangEng, Prelude, CatEng, GodisLangEng, LangEng, ParadigmsEng in {

lincat Date = NP;

lincat Date_Str = {s : Str};
lincat Year = {s : Str};
lincat Month = {s : Str};
lincat Day = {s : Str};
lincat Weekday = {s : Str};


lin

mkDate s = mkNP s.s s.s Sg Masc;

date__day d = d;
date__month m = m;
date__year y = y;
date__day_month d m = {s = d.s ++ "of" ++ m.s};
date__month_year m y = {s = m.s ++ y.s};
date__day_month_year d m y = {s = d.s ++ m.s ++ y.s};

date__weekday w = w;
date__weekday_day w d = {s = w.s ++ d.s};
date__weekday_day_month w d m = {s = w.s ++  d.s ++ "of" ++  m.s};
date__weekday_day_month_year w d m y = {s = w.s ++ d.s ++ "of" ++  m.s ++ y.s};


lin

twothousandfour = ss (["two thousand and four"]);
twouthousandfive = ss (["two thousand and five"]);
twothousandsix = ss (["two thousand and six"]);
twothousandseven = ss (["two thousand and seven"]);


january = ss (["january"]);
february = ss (["february"]); 
march = ss (["march"]); 
april = ss (["april"]);
june = ss (["june"]); 
july = ss (["july"]); 
august = ss (["august"]);
september = ss (["september"]);
october = ss (["oktober"]); 
november = ss (["november"]);
december = ss (["december"]);


first = ss (variants {["first"];["the first"]});
second = ss (variants {["second"];["the second"]});
third = ss (variants {["third"];["the third"]});
fourth = ss (variants {["fourth"];["the fourth"]});
fifth = ss (variants {["fifth"];["the fifth"]});
sixth = ss (variants {["sixth"];["the sixth"]});
seventh = ss (variants {["seventh"];["the seventh"]});
eighth = ss (variants {["eight"];["the eight"]});
ninth = ss (variants {["ninth"];["the ninth"]});
tenth = ss (variants {["tenth"];["the tenth"]});
eleventh = ss (variants {["eleventh"];["the eleventh"]}); 
twelvth = ss (variants {["twelvth"];["the twelvth"]});
thirteenth = ss (variants {["thirteenth"];["the thirteenth"]});
fourteenth = ss (variants {["fourteenth"];["the fourteenth"]});
fifteenth = ss (variants {["fifteenth"];["the fifteenth"]});
sixteenth = ss (variants {["sixteenth"];["the sixteenth"]});
seventeenth = ss (variants {["seventeenth"];["the seventeenth"]});
eighteenth = ss (variants {["eighteenth"];["the eighteenth"]});
nineteenth = ss (variants {["ninteenth"];["the ninteenth"]});
twentieth = ss (variants {["twentieth"];["the twentieth"]}); 
twentyfirst = ss (variants {["twentyfirst"];["the twentyfirst"]}); 
twentysecond = ss (variants {["twentysecond"];["the twentysecond"]}); 
twentythird = ss (variants {["twentythird"];["the twentythird"]}); 
twentyfourth = ss (variants {["twentyfourth"];["the twentyfourth"]}); 
twentyfifth = ss (variants {["twentyfifth"];["the twentyfifth"]}); 
twentysixth = ss (variants {["twentysixth"];["the twentysixth"]}); 
twentyseventh = ss (variants {["twentyseventh"];["the twentyseventh"]}); 
twentyeight = ss (variants {["twentyeighth"];["the twentyeighth"]}); 
twentyninth = ss (variants {["twentyninth"];["the twentyninth"]}); 
thirtieth = ss (variants {["thirtieth"];["the thirtieth"]}); 
thirtyfirst = ss (variants {["thirtyfirst"];["the thirtyfirst"]});


monday = ss (["monday"]);
tuesday = ss (["tuesday"]);
wednesday = ss (["wednesday"]);
thursday = ss (["thursday"]);
friday = ss (["friday"]);
saturday = ss (["saturday"]);
sunday = ss (["sunday"]);

today = ss (["today"]);
tomorrow = ss (["tomorrow"]);
yesterday = ss (["yesterday"]);
day_after_tomorrow = ss (["the day after tomorrow"]);
day_before_yesterday = ss (["the day before yesterday"]);


}
