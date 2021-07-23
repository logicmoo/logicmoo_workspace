function isLeapYear(year) {
    if ((year % 4) == 0) {
        if ((year % 100) == 0) {
            return true;
        }
        else {
            if ((year % 400) == 0) {
                return true;
            }
            else {
                return false;
            }
        }
    }
    else {
        return false;
    }
}
function getCurrentDaySecond(in_game_seconds) {
    // each game cycle is 1 second of time:
    return (in_game_seconds + start_day_second) % (24 * 60 * 60);
}
function getCurrentDayMinute(in_game_seconds) {
    // each game cycle is 1 second of time:
    return Math.floor(((in_game_seconds + start_day_second) % (24 * 60 * 60)) / 60);
}
function getCurrentDayHour(in_game_seconds) {
    // each game cycle is 1 second of time:
    return Math.floor(((in_game_seconds + start_day_second) % (24 * 60 * 60)) / (60 * 60));
}
function getCurrentYearDay(in_game_seconds) {
    var days = Math.floor(in_game_seconds / (24 * 60 * 60)) + start_year_day;
    var year = start_year;
    var days_in_year = 365;
    if (isLeapYear(year))
        days_in_year = 366;
    while (days >= days_in_year) {
        year++;
        days -= days_in_year;
        days_in_year = 365;
        if (isLeapYear(year))
            days_in_year = 366;
    }
    return days;
}
function getCurrentYear(in_game_seconds) {
    var days = Math.floor(in_game_seconds / (24 * 60 * 60)) + start_year_day;
    var year = start_year;
    var days_in_year = 365;
    if (isLeapYear(year))
        days_in_year = 366;
    while (days >= days_in_year) {
        year++;
        days -= days_in_year;
        days_in_year = 365;
        if (isLeapYear(year))
            days_in_year = 366;
    }
    return year;
}
function getCurrentDayOfTheMonth(in_game_seconds) {
    var day = getCurrentYearDay(in_game_seconds);
    var year = getCurrentYear(in_game_seconds);
    var monthDays = [31, 28, 31, 30, 31, 30,
        31, 31, 30, 31, 30, 31];
    if (isLeapYear(year))
        monthDays[1] = 29;
    var month = 0;
    while (day >= monthDays[month]) {
        day -= monthDays[month];
        month++;
    }
    return day;
}
function getCurrentMonth(in_game_seconds) {
    var day = getCurrentYearDay(in_game_seconds);
    var year = getCurrentYear(in_game_seconds);
    var monthDays = [31, 28, 31, 30, 31, 30,
        31, 31, 30, 31, 30, 31];
    if (isLeapYear(year))
        monthDays[1] = 29;
    var month = 0;
    while (day >= monthDays[month]) {
        day -= monthDays[month];
        month++;
    }
    return month;
}
function getCurrentDayOfTheWeek(in_game_seconds) {
    var day = Math.floor(in_game_seconds / (24 * 60 * 60)) + start_day_of_the_week;
    return day % 7;
}
function getCurrentDateString(in_game_seconds) {
    var months = ["January", "February", "March", "April",
        "May", "June", "July", "August",
        "September", "October", "November", "December"];
    var days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"];
    var d = days[getCurrentDayOfTheWeek(in_game_seconds)] + ", " +
        (getCurrentDayOfTheMonth(in_game_seconds) + 1) + " of " +
        months[getCurrentMonth(in_game_seconds)] + " of " + getCurrentYear(in_game_seconds);
    return d;
}
function getCurrentTimeString(in_game_seconds) {
    var t = getCurrentDayHour(in_game_seconds) + ":" + (getCurrentDayMinute(in_game_seconds) % 60) + ":" + (getCurrentDaySecond(in_game_seconds) % 60);
    return t;
}
/*
var start_year:number = 2432;
var start_year_day:number = 293;          // october 21st
var start_day_second:number = 8*60*60;    // 8am
var start_day_of_the_week:number = 3;     // october 21st, 2432 is a Thursday
*/
var start_year = 1000; // not all the way to 0 so that the time values are not too large (scared we would get out of integer range)
var start_year_day = 0; // october 21st
var start_day_second = 8 * 60 * 60; // 8am
var start_day_of_the_week = 0; // Jan 1st, 1000 is a Monday
