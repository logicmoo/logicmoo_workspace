%**********************************************************************
%
%  Rules for Date/Time names
%
%  date_np : date NP (days of the week, months, years)
%  time_np : time NP (eg. 12 a.m.)
%
%**********************************************************************


:- multifile best_parse_cats/1, rule/2.
:- dynamic best_parse_cats/1, rule/2.

%
% Best Parse Categories
best_parse_cats([ne_np]).


%
%   NP Rules
%
%% NE_NP --> DATE_NP
rule(ne_np(edge:Edge,s_form:F, % assume all non-days will be caught in timex
     sem:E^[[date,E],[name,E,F],[ne_tag,E,Edge],[realisation,E,Edge]]),
	[date_np(s_form:F)]).


%
% DATE_NP rules
% 

%% DATE_NP --> CD N(/) CD N(/) CD (in DD line)
rule(date_np(s_form:[F1,'/',F3,'/',F5]),[
        cd(s_form:F1),
        n(s_form:'/'),
	cd(s_form:F3),
        n(s_form:'/'),
        cd(s_form:F5)
    ]).

%% LIST_NP(date) VBD(ended) DATE CD
% rule for 'ended'
rule(date_np(s_form:[F1,' ended ',F2,' ',F3]),[
        list_np(s_form:F1,ne_tag:date),
        vbd(s_form:'ended'),
        date(s_form:F2),
	cd(s_form:F3)
	]).


%% DATE_NP --> DATE COMMA DATE CD COMMA LIST_NP(date)
% Mon, Jun 2, 1900
rule(date_np(s_form:[F1,', ',F2,' ',F3,', ',F4]),[
        date(s_form:F1),
	comma(s_form:','),
        date(s_form:F2),
	cd(s_form:F3),
        comma(s_form:','),
        [list_np(s_form:F4,ne_tag:date),cd_np(s_form:F4)]
    ]).

%% DATE_NP --> DATE PERIOD CD COMMA LIST_NP(date)
% Jun. 2, 1900
rule(date_np(s_form:[F1,F2,' ',F3,F4,' ',F5]),[
        date(s_form:F1),
        period(s_form:F2),
	cd(s_form:F3),
        comma(s_form:F4),
        [list_np(s_form:F5,ne_tag:date),cd_np(s_form:F5)]
    ]).

%% DATE_NP --> DATE CD COMMA LIST_NP(date)
% Jun 2, 1900
rule(date_np(s_form:[F1,' ',F2,F3,' ',F4]),[
        date(s_form:F1),
	cd(s_form:F2),
        comma(s_form:F3),
        [list_np(s_form:F4,ne_tag:date),cd_np(s_form:F4)]
    ]).


%% DATE_NP --> LIST_NP(date) IN(of) LIST_NP(date)
rule(date_np(s_form:[F1,' of ',F2]),[
        list_np(s_form:F1,ne_tag:date),
	in(s_form:'of'),
        list_np(s_form:F2,ne_tag:date)
    ]).

%%% DATE_NP --> LIST_NP(date) LIST_NP(date)
%rule(date_np(s_form:[F1,' ',F2]),[
%        list_np(s_form:F1,ne_tag:date),
%        list_np(s_form:F2,ne_tag:date)
%    ]).

%% DATE_NP --> LIST_NP(date) (for special date phrases from list-lookup)
rule(date_np(edge:Edge,s_form:F1,sem:E^[ne_tag,E,Edge]),[
        list_np(s_form:F1,ne_tag:date)
    ]).

%% DATE_NP --> DATE PERIOD LIST_NP(date)
rule(date_np(s_form:[F1,F2,' ',F3]),[
        date(s_form:F1),
        period(s_form:F2),
	list_np(s_form:F3,ne_tag:date)
    ]).

%% DATE_NP --> DATE COMMA LIST_NP(date)
rule(date_np(s_form:[F1,', ',F3]),[
        date(s_form:F1),
        comma(s_form:','),
	list_np(s_form:F3,ne_tag:date)
    ]).

%% DATE_NP --> DATE LIST_NP(date)
rule(date_np(s_form:[F1,' ',F2]),[
        date(s_form:F1),
        list_np(s_form:F2,ne_tag:date)
    ]).

%% DATE_NP --> DATE PERIOD CD
rule(date_np(s_form:[F1,F2,' ',F3]),[
        date(s_form:F1),
        period(s_form:F2),
	cd(s_form:F3)
    ]).

%% DATE_NP --> DATE CD
rule(date_np(s_form:[F1,' ',F2]),[
        date(s_form:F1),
        cd(s_form:F2)
    ]).

%% DATE_NP --> DATE

rule(date_np(s_form:F),[
        date(s_form:F)
    ]).

%% NE_NP --> TIME_NP
rule(ne_np(edge:Edge,s_form:F,source:S,
	   sem:E^[[time,E],[name,E,F],[ne_tag,E,Edge]]),
	[time_np(s_form:F,source:S)]).

%% TIME_NP --> CD LIST_NP(time)
rule(time_np(s_form:[F1,' ',F2]),[
        cd(s_form:F1),
        list_np(s_form:F2,ne_tag:time)
    ]).

%% TIME_NP --> CD : CD LIST_NP(time)
rule(time_np(s_form:[F1,':',F2,' ',F3]),[
        cd(s_form:F1),
	sym(s_form:':'),
        cd(s_form:F2),
        list_np(s_form:F3,ne_tag:time)
    ]).

%% TIME_NP --> LIST_NP(time)
rule(time_np(s_form:[F1],source:list),[
        list_np(s_form:F1,ne_tag:time)
    ]).


