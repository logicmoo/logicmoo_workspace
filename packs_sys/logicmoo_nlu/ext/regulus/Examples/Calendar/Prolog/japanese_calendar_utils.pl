
:- module(japanese_calendar_utils,
	[japanese_dayofweek/2,
	 japanese_day_word/1,
	 japanese_timeperiod/1,
	 japanese_time_of_day/1]
    ).

%======================================================================

:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module('$REGULUS/PrologLib/utilities').

%======================================================================

japanese_dayofweek(getsuyoubi, monday).
japanese_dayofweek(kayoubi, tuesday).
japanese_dayofweek(suiyoubi, wednesday).
japanese_dayofweek(mokuyoubi, thursday).
japanese_dayofweek(kinyoubi, friday).
japanese_dayofweek(doyoubi, saturday).
japanese_dayofweek(nichiyoubi, sunday).

japanese_day_word(kyou).
japanese_day_word(ototoi).
japanese_day_word(kinou).
japanese_day_word(asu).
japanese_day_word(ashita).
japanese_day_word(asatte).

japanese_timeperiod(sensyuu).
japanese_timeperiod(konsyuu).
japanese_timeperiod(raisyuu).
japanese_timeperiod(sengetsu).
japanese_timeperiod(kongetsu).
japanese_timeperiod(raigetsu).
japanese_timeperiod(saraigetsu).
japanese_timeperiod(kotoshi).
japanese_timeperiod(kyonen).
japanese_timeperiod(rainen).
japanese_timeperiod(nichikan).
japanese_timeperiod(syuukan).
japanese_timeperiod(getsukan).
japanese_timeperiod(kagetsukan).

japanese_time_of_day(asa).
japanese_time_of_day(gozen).
japanese_time_of_day(gogo).
japanese_time_of_day(kesa).
