--# -path=.:../Common:prelude

concrete BookingDatesSem of BookingDates = open Prolog in {

lincat Date = PStr;

lincat Date_Str = {s : Str};
lincat Year = {s : Str};
lincat Month = {s : Str};
lincat Day = {s : Str};
lincat Weekday = {s : Str};


lin

mkDate s = pp0 s.s;

date__day d = d;
date__month m = m;
date__year y = y;
date__day_month d m = {s = d.s ++ "," ++ m.s};
date__month_year m y = {s = m.s ++ "," ++ y.s};
date__day_month_year d m y = {s = d.s ++ "," ++  m.s ++ "," ++ y.s};

date__weekday w = w;
date__weekday_day w d = {s = w.s ++ "," ++ d.s};
date__weekday_day_month w d m = {s = w.s ++ "," ++ d.s ++ "," ++  m.s};
date__weekday_day_month_year w d m y = {s = w.s ++ "," ++ d.s ++ "," ++ m.s ++ "," ++ y.s};


lin

twothousandfour = {s = "2004"};
twouthousandfive = {s = "2005"};
twothousandsix = {s = "2006"};
twothousandseven = {s = "2007"};


january = {s = "januari"};
february = {s = "februari"}; 
march = {s = "mars"}; 
april = {s = "april"};
june = {s = "juni"}; 
july = {s = "juli"}; 
august = {s = "augusti"};
september = {s = "september"};
october = {s = "oktober"}; 
november = {s = "november"};
december = {s = "december"};


first = {s = "01"};
second = {s = "02"};
third = {s = "03"};
fourth = {s = "04"};
fifth = {s = "05"};
sixth = {s = "06"};
seventh = {s = "07"};
eighth = {s = "08"};
ninth = {s = "09"};
tenth = {s = "10"};
eleventh = {s = "11"}; 
twelvth = {s = "12"};
thirteenth = {s = "13"};
fourteenth = {s = "14"};
fifteenth = {s = "15"};
sixteenth = {s = "16"};
seventeenth = {s = "17"};
eighteenth = {s = "18"};
nineteenth = { s = "19"};
twentieth = {s = "20"}; 
twentyfirst = {s = "21"}; 
twentysecond = {s = "22"}; 
twentythird = {s = "23"}; 
twentyfourth = {s = "24"}; 
twentyfifth = {s = "25"}; 
twentysixth = {s = "26"}; 
twentyseventh = {s = "27"}; 
twentyeight = {s = "28"}; 
twentyninth = {s = "29"}; 
thirtieth = {s = "30"}; 
thirtyfirst = {s = "31"};


monday = {s = "monday"};
tuesday = {s = "tuesday"};
wednesday = {s = "wednesday"};
thursday = {s = "thursday"};
friday = {s = "friday"};
saturday = {s = "saturday"};
sunday = {s = "sunday"};

today = {s = "today"};
tomorrow = {s = "tomorrow"};
yesterday = {s = "yesterday"};
day_after_tomorrow = {s = "day_after_tomorrow"};
day_before_yesterday = {s = "day_before_yesterday"};

}




