:- expects_dialect(lps).

% contagionnet.pl by @jacintodavila
% A simple program to predict contagion
% as requested by https://twitter.com/mengwong 
% 
% recommended query: ?contaminated(Patient, [], Path, Period).

contaminated(A, Path, Path, (T1, T2)) :- 
   observe(tested(A, positive), Date), 
   five_days_before(T1, Date), 
   two_week_after(Date, T2). 
 
contaminated(A, Path, FPath, (Tm,T2)) :- 
   met(A,B,Tm), not(member(B, Path)), 
   contaminated(B, [B|Path], FPath, (TB1, TB2)),
   within(TB1, Tm, TB2), 
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
observe(tested(alice, positive), date(2020,3,15,0,0,0,0,'UTC',-)).

