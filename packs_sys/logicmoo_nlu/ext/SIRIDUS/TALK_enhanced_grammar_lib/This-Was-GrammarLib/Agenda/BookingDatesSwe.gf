--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common:resource-1.0/scandinavian:resource-1.0/swedish

concrete BookingDatesSwe of BookingDates = 
	open CommonScand, GodisLangSwe, Prelude, CatSwe, GodisLangSwe, LangSwe, ParadigmsSwe in {

lincat Date = NP;

lincat Date_Str = {s : Str};
lincat Year = {s : Str};
lincat Month = {s : Str};
lincat Day = {s : Str};
lincat Weekday = {s : Str};


lin

mkDate s = mkNP s.s s.s Sg utrum;

date__day d = d;
date__month m = m;
date__year y = y;
date__day_month d m = {s = d.s ++  m.s};
date__month_year m y = {s = m.s ++ y.s};
date__day_month_year d m y = {s = d.s ++ m.s ++ y.s};

date__weekday w = w;
date__weekday_day w d = {s = w.s ++ d.s};
date__weekday_day_month w d m = {s = w.s ++  d.s ++  m.s};
date__weekday_day_month_year w d m y = {s = w.s ++ d.s ++ m.s ++ y.s};


lin

twothousandfour = ss (["tvåtusenfyra"]);
twouthousandfive = ss (["tvåtusenfem"]);
twothousandsix = ss (["tvåtusensex"]);
twothousandseven = ss (["tvåtusensju"]);


january = ss (["januari"]);
february = ss (["februari"]); 
march = ss (["mars"]); 
april = ss (["april"]);
june = ss (["juni"]); 
july = ss (["juli"]); 
august = ss (["augusti"]);
september = ss (["september"]);
october = ss (["oktober"]); 
november = ss (["november"]);
december = ss (["december"]);


first = ss (variants {["första"];["den första"]});
second = ss (variants {["andra"];["den andra"]});
third = ss (variants {["tredje"];["den tredje"]});
fourth = ss (variants {["fjärde"];["den fjärde"]});
fifth = ss (variants {["femte"];["den femte"]});
sixth = ss (variants {["sjätte"];["den sjätte"]});
seventh = ss (variants {["sjunde"];["den sjunde"]});
eighth = ss (variants {["åttonde"];["den åttonde"]});
ninth = ss (variants {["nionde"];["den nionde"]});
tenth = ss (variants {["tionde"];["den tionde"]});
eleventh = ss (variants {["elfte"];["den elfte"]}); 
twelvth = ss (variants {["tolfte"];["den tolfte"]});
thirteenth = ss (variants {["trettonde"];["den trettonde"]});
fourteenth = ss (variants {["fjortonde"];["den fjortonde"]});
fifteenth = ss (variants {["femtonde"];["den femtonde"]});
sixteenth = ss (variants {["sextonde"];["den sextonde"]});
seventeenth = ss (variants {["sjutonde"];["sjutonde"]});
eighteenth = ss (variants {["artonde"];["den artonde"]});
nineteenth = ss (variants {["nittonde"];["den nittonde"]});
twentieth = ss (variants {["tjugonde"];["den tjugonde"]}); 
twentyfirst = ss (variants {["tjugoförsta"];["den tjugoförsta"]}); 
twentysecond = ss (variants {["tjugoandra"];["den tjugoandra"]}); 
twentythird = ss (variants {["tjugotredje"];["den tjugotredje"]}); 
twentyfourth = ss (variants {["tjugofjärde"];["den tjugofjärde"]}); 
twentyfifth = ss (variants {["tjugofemte"];["den tjugofemte"]}); 
twentysixth = ss (variants {["tjugosjätte"];["den tjugosjätte"]}); 
twentyseventh = ss (variants {["tjugosjunde"];["den tjugosjunde"]}); 
twentyeight = ss (variants {["tjugoåttonde"];["den tjugoåttonde"]}); 
twentyninth = ss (variants {["tjugonionde"];["den tjugonionde"]}); 
thirtieth = ss (variants {["trettionde"];["den trettionde"]}); 
thirtyfirst = ss (variants {["trettioförsta"];["den trettioförsta"]});


monday = ss (variants {["måndag"];["måndagen"]});
tuesday = ss (variants {["tisdag"];["tisdagen"]});
wednesday = ss (variants {["onsdag"];["onsdagen"]});
thursday = ss (variants {["torsdag"];["torsdagen"]});
friday = ss (variants {["fredag"];["fredagen"]});
saturday = ss (variants {["lördag"];["lördagen"]});
sunday = ss (variants {["söndag"];["söndagen"]});

today = ss (["idag"]);
tomorrow = ss (["i morgon"]);
yesterday = ss (["i går"]);
day_after_tomorrow = ss (["i övermorgon"]);
day_before_yesterday = ss (["i förrgår"]);







