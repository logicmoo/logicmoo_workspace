:- expects_dialect(lps).

end_of_file.
% @TODO unbreak

% contagion
% A simple program to predict contagion
% as requested by https://twitter.com/mengwong 
% by @jacintodavila
% 
% starting lines by M. Calejo 
:- include(system('date_utils.pl')).  % get us the end_of_day(Date) event

% These 3 lines are for development only; to be commented on deployment, if this happens at contract start:
simulatedRealTimeBeginning('2020-03-01'). 
simulatedRealTimePerCycle(RTPC) :- minCycleTime(RTPC). 
maxTime(3200).

maxRealTime(2628000). % 2 month
minCycleTime(21600). % 4 cycles per calendar day

fluents	dont_touch/1. % isolate Person

end_of_day(Y/M/D)
initiates dont_touch(Person)
if 	contaminated(Person, [], _, (T1, T2)),
    within(T1, date(Y,M,D,0,0,0,0,'UTC',-), T2). 

end_of_day(Y/M/D)
terminates dont_touch(Person)
if 	contaminated(Person, [], _, (T1, T2)),
    not(within(T1, date(Y,M,D,0,0,0,0,'UTC',-), T2)). 

contaminated(A, Path, Path, (T1, T2)) :- 
   observe(tested(A, positive), Date), 
   five_days_before(T1, Date), 
   two_week_after(Date, T2). 
 
% "contact's contamination period begins 5 days after 
% they have been observed meeting an existing zombie"
% https://twitter.com/mengwong/status/1239936341393076227
contaminated(A, Path, FPath, (T1,T2)) :- 
   met(A,B,Tm), not(member(B, Path)), 
   contaminated(B, [B|Path], FPath, (TB1, TB2)),
   within(TB1, Tm, TB2),
   five_days_after(Tm, T1), 
   two_week_after(Tm, T2).   

met(A,B,T) :- ( observe(meets(A, B), T) ; observe(meets(B, A), T) ). 

% five_days_before(T1, T2): T1 is five days before T2
five_days_before(date(Y,M,D,H,Mn,S,Off,TZ,DST), Date2) :- 
   nonvar(D), 
   NewD is D + 5, 
   date_time_stamp(date(Y,M,NewD,H,Mn,S,Off,TZ,DST), Stamp),
   stamp_date_time(Stamp, Date2, 0).

five_days_before(Date2, date(Y,M,D,H,Mn,S,Off,TZ,DST)) :-
   nonvar(D),  
   NewD is D - 5, 
   date_time_stamp(date(Y,M,NewD,H,Mn,S,Off,TZ,DST), Stamp),
   stamp_date_time(Stamp, Date2, 0).

% two_week_after(T1, T2): T2 is two_week_after T1
two_week_after(date(Y,M,D,H,Mn,S,Off,TZ,DST), Date2) :- 
   nonvar(D), 
   NewD is D + 15, 
   date_time_stamp(date(Y,M,NewD,H,Mn,S,Off,TZ,DST), Stamp),
   stamp_date_time(Stamp, Date2, 0).

two_week_after(Date2, date(Y,M,D,H,Mn,S,Off,TZ,DST)) :-
   nonvar(D),  
   NewD is D - 15, 
   date_time_stamp(date(Y,M,NewD,H,Mn,S,Off,TZ,DST), Stamp),
   stamp_date_time(Stamp, Date2, 0).

% five_days_after(T1, T2): T2 is five_days_after T1
five_days_after(date(Y,M,D,H,Mn,S,Off,TZ,DST), Date2) :- 
   nonvar(D), 
   NewD is D + 5, 
   date_time_stamp(date(Y,M,NewD,H,Mn,S,Off,TZ,DST), Stamp),
   stamp_date_time(Stamp, Date2, 0).

five_days_after(Date2, date(Y,M,D,H,Mn,S,Off,TZ,DST)) :-
   nonvar(D),  
   NewD is D - 5, 
   date_time_stamp(date(Y,M,NewD,H,Mn,S,Off,TZ,DST), Stamp),
   stamp_date_time(Stamp, Date2, 0).

% within(T1, T, T2) : T is within T1 and T2. 
within(Date1, T, Date2) :-
   nonvar(Date1),
   nonvar(Date2), 
   nonvar(T), 
   date_time_stamp(Date1, Stamp1), 
   date_time_stamp(Date2, Stamp2), 
   date_time_stamp(T, Stamp3), 
   Stamp1 =< Stamp3, 
   Stamp3 =< Stamp2. 

% On 2020-03-01 Alice met Bob.
observe(meets(alice, bob), date(2020,3,1,0,0,0,0,'UTC',-)).
                                                            
% On 2020-03-06 Bob met Charlie and Delilah.
observe(meets(bob, charlie), date(2020,3,6,0,0,0,0,'UTC',-)).
observe(meets(bob, delilah), date(2020,3,6,0,0,0,0,'UTC',-)).

% On 2020-03-12 Delilah and Iona met Edgar, Fiona, and Gertrude.
observe(meets(delilah, edgar), date(2020,3,12,0,0,0,0,'UTC',-)).
observe(meets(delilah, fiona), date(2020,3,12,0,0,0,0,'UTC',-)).
observe(meets(delilah, gertrude), date(2020,3,12,0,0,0,0,'UTC',-)).
observe(meets(iona, edgar), date(2020,3,12,0,0,0,0,'UTC',-)).
observe(meets(iona, fiona), date(2020,3,12,0,0,0,0,'UTC',-)).
observe(meets(iona, gertrude), date(2020,3,12,0,0,0,0,'UTC',-)).

% On 2020-03-14 Edgar, Fiona and Gertrude met Hannah and Iona.
observe(meets(edgar, hannah), date(2020,3,14,0,0,0,0,'UTC',-)).
observe(meets(fiona, hannah), date(2020,3,14,0,0,0,0,'UTC',-)).
observe(meets(gertrude, hannah), date(2020,3,14,0,0,0,0,'UTC',-)).
observe(meets(edgar, iona), date(2020,3,14,0,0,0,0,'UTC',-)).
observe(meets(fiona, iona), date(2020,3,14,0,0,0,0,'UTC',-)).
observe(meets(gertrude, iona), date(2020,3,14,0,0,0,0,'UTC',-)).

% On 2020-03-15 Alice tested positive.
observe(tested(alice, positive), date(2020,3,2,0,0,0,0,'UTC',-)).

/** <examples>
?- go(Timeline).
*/
