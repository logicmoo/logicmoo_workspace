:- expects_dialect(lps).

% contagionlps
% A simple program to predict contagion
% as requested by https://twitter.com/mengwong 
% by @jacintodavila 
% Trying LPS
% 
% The date/time A is tested positive.
% The date/time A meets B.
% 

maxtime(20). 

fluents contaminated/3, infected/3, met/3.

events tested(A, Result, Date),
       meets(A, B, Date). 

tested(Person, positive, Date) initiates contaminated(Person, T1, T2) if
   two_week_after(Date, T2),
   five_days_before(T1, Date).

meets(A,B, Date) initiates contaminated(B, Ta, Tb) if
  contamination(A, [], _, (T1, T2)) at _, 
  within(T1, Date, T2), 
  five_days_after(T1, Ta), 
  two_week_after(T1, Tb). 

meets(A,B,Date) initiates met(A,B,Date). 
meets(B,A,Date) initiates met(A,B,Date).

contamination(A, Path, Path, (T1, T2)) if
   contaminated(A, T1, T2). 
 
% "contact's contamination period begins 5 days after 
% they have been observed meeting an existing zombie"
% https://twitter.com/mengwong/status/1239936341393076227
contamination(A, Path, FPath, (T1,T2)) if
   met(A,B,Tm), not(member(B, Path)), 
   contamination(B, [B|Path], FPath, (TB1, TB2)),
   within(TB1, Tm, TB2),
   five_days_after(Tm, T1), 
   two_week_after(Tm, T2).   

% "contact's contamination period begins 5 days after 
% they have been observed meeting an existing zombie"
% https://twitter.com/mengwong/status/1239936341393076227
% 
% 
% observe tested(alice, positive, date(2020,3,14,0,0,0,0,'UTC','-')) from 1 to 2.
observe meets(gertrude, alice, date(2020,3,15,0,0,0,0,'UTC','-')) from 2 to 3.
observe tested(alice, positive, date(2020,3,14,0,0,0,0,'UTC','-')) from 4 to 5.

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
               
               
               
               