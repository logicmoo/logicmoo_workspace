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

fluents contaminated/3, infected/3, met/3, tested/3.
events test(A, Result, Date),    meets(A, B, Date). 
actions notify/3.

meets(A,B,Date) initiates met(A,B,Date). 
meets(B,A,Date) initiates met(A,B,Date).

test(Person, Result, Date) initiates tested(Person, Result, Date).

/*
if contaminated(Person, T1, T2) 
then notify(Person, T1, T2).
*/

 contaminated(Person, T1, T2) if
	tested(Person, positive, Date),
   two_week_after(Date, T2),
  five_days_before(T1, Date).
 
% "contact's contamination period begins 5 days after 
% they have been observed meeting an existing zombie"
% https://twitter.com/mengwong/status/1239936341393076227
contaminated(A, Tm,T2) if
   met(A,B,Tm),
   contaminated(B, TB1, TB2),
   within(TB1, Tm, TB2),
  five_days_after(Tm, T1), % plays no role.
   two_week_after(Tm, T2).   

% observe tested(alice, positive, date(2020,3,14,0,0,0,0,'UTC','-')) from 1 to 2.
observe meets(gertrude, alice, date(2020,3,15,0,0,0,0,'UTC','-')) from 2 to 3.
observe test(alice, positive, date(2020,3,14,0,0,0,0,'UTC','-')) from 4 to 5.

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
               
               
               
               

/** <examples>
?- go.
?- go(T).
?- two_week_after(date(2020,3,15,0,0,0,0,'UTC','-'), Date2).
?- five_days_before(date(2020,3,15,0,0,0,0,'UTC','-'), Date2)
?- go(Timeline,[sample([contaminated(_,_,_)])]).
*/
