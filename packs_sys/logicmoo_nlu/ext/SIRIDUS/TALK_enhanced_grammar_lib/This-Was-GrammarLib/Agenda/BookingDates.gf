--# -path=.:../Common:prelude:resource-1.0/abstract:resource-1.0/common

abstract BookingDates = {

cat Date;

cat Date_Str;

cat Year;
cat Month;
cat Day;
cat Weekday;

fun

mkDate : Date_Str -> Date;

date__day : Day -> Date_Str;
date__month : Month -> Date_Str;
date__year : Year -> Date_Str;
date__day_month : Day -> Month -> Date_Str;
date__month_year : Month -> Year -> Date_Str;
date__day_month_year : Day -> Month -> Year -> Date_Str;


date__weekday : Weekday -> Date_Str;
date__weekday_day : Weekday -> Day -> Date_Str;
date__weekday_day_month : Weekday -> Day -> Month -> Date_Str;
date__weekday_day_month_year : Weekday -> Day -> Month -> Year -> Date_Str;



twothousandfour,
twouthousandfive,
twothousandsix,
twothousandseven : Year;


january,
february,
march,
april,
june,
july,
august,
september,
october,
november,
december : Month;

first,
second,
third,
fourth,
fifth,
sixth,
seventh,
eighth,
ninth,
tenth,
eleventh,
twelvth,
thirteenth,
fourteenth,
fifteenth,
sixteenth,
seventeenth,
eighteenth,
nineteenth,
twentieth,
twentyfirst,
twentysecond,
twentythird,
twentyfourth,
twentyfifth,
twentysixth,
twentyseventh,
twentyeight,
twentyninth,
thirtieth,
thirtyfirst : Day;

monday,
tuesday,
wednesday,
thursday,
friday,
saturday,
sunday : Weekday;

today,
tomorrow,
yesterday,
day_after_tomorrow,
day_before_yesterday : Date_Str;

}


