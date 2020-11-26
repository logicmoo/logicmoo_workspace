:- expects_dialect(lps).

% testingLPSrecursionLPS.pl
% by @jacintodavila
% 
% 
maxTime(30).

fluents	dont_touch/1, contaminated/4. % isolate Person

events verify. 

verify from T to _ initiates dont_touch(Person)
if 	contaminated(Person, [], _, (T1, T2)),
    within(T1, T, T2). 

verify from T to _ terminates dont_touch(Person)
if 	contaminated(Person, [], _, (T1, T2)),
    not(within(T1, T, T2)). 

observe verify from 1 to 2.
observe verify from 2 to 3.
observe verify from 3 to 4.
observe verify from 4 to 5.
observe verify from 5 to 6.
observe verify from 6 to 7.
observe verify from 7 to 8.
observe verify from 8 to 9.
observe verify from 9 to 10.
observe verify from 10 to 11.
observe verify from 11 to 12.
observe verify from 12 to 13.
observe verify from 13 to 14.
observe verify from 14 to 15.
observe verify from 15 to 16.
observe verify from 16 to 17.
observe verify from 17 to 18.
observe verify from 18 to 19.
observe verify from 20 to 21.
observe verify from 21 to 22.
observe verify from 22 to 23.
observe verify from 23 to 24.
observe verify from 24 to 25.
observe verify from 25 to 26.
observe verify from 26 to 27.
observe verify from 27 to 28.
observe verify from 28 to 29. 

% contaminated(A, Path, Path, (T1, T2)) :- 
%    observe([tested(A, positive)], T),  
%    five_days_before(T1, T), 
%    two_week_after(T, T2). 

% contaminated(A, Path, FPath, (T1,T2)) :- 
%    met(A,B,Tm), not(member(B, Path)), 
%    contaminated(B, [B|Path], FPath, (TB1, TB2)),
%    within(TB1, Tm, TB2),
%    five_days_after(Tm, T1), 
%    two_week_after(Tm, T2).   

contaminated(A, Path, Path, (T1, T2)) if
   observe([tested(A, positive)], T),  
   five_days_before(T1, T), 
   two_week_after(T, T2). 
 
contaminated(A, Path, FPath, (T1,T2)) if
   met(A,B,Tm), not(member(B, Path)), 
   contaminated(B, [B|Path], FPath, (TB1, TB2)),
   within(TB1, Tm, TB2),
   five_days_after(Tm, T1), 
   two_week_after(Tm, T2).   

met(A,B,T) :- observe([meets(A, B)], T) ; 
              observe([meets(B, A)], T). 

% five_days_before(T1, T2): T1 is five days before T2
five_days_before(D, NewD) :- 
   nonvar(D), 
   NewD is D + 5.

five_days_before(D, NewD) :-
   nonvar(NewD),
   Di is NewD - 5,
   Di > 1 ->  D is Di; D is 1. 

% two_week_after(T1, T2): T2 is two_week_after T1
two_week_after(D, NewD) :- 
   nonvar(D), 
   NewD is D + 15.

two_week_after(D, NewD) :-
   nonvar(NewD),
   Di is NewD - 15,
   Di > 1 ->  D is Di; D is 1.

% five_days_after(T1, T2): T2 is five_days_after T1
five_days_after(D, NewD) :- 
   nonvar(D), 
   NewD is D + 5.

five_days_after(D, NewD) :-
   nonvar(NewD),  
   Di is NewD - 5,
   Di > 1 ->  D is Di; D is 1.

% within(T1, T, T2) : T is within T1 and T2. 
within(D1, T, D2) :-
   nonvar(D1),
   nonvar(D2), 
   nonvar(T), 
   D1 =< T, 
   T =< D2. 

% On 2020-03-01 Alice met Bob.
observe meets(alice, bob) from 1 to 2.
                                                            
% On 2020-03-06 Bob met Charlie and Delilah.
observe meets(bob, charlie) from 6 to 7.
observe meets(bob, delilah) from 6 to 7.

% On 2020-03-12 Delilah and Iona met Edgar, Fiona, and Gertrude.
observe meets(delilah, edgar) from 12 to 13.
observe meets(delilah, fiona) from 12 to 13.
observe meets(delilah, gertrude) from 12 to 13.
observe meets(iona, edgar) from 12 to 13.
observe meets(iona, fiona) from 12 to 13.
observe meets(iona, gertrude) from 12 to 13.

% On 2020-03-14 Edgar, Fiona and Gertrude met Hannah and Iona.
observe meets(edgar, hannah) from 14 to 15.
observe meets(fiona, hannah) from 14 to 15.
observe meets(gertrude, hannah) from 14 to 15.
observe meets(edgar, iona) from 14 to 15.
observe meets(fiona, iona) from 14 to 15.
observe meets(gertrude, iona) from 14 to 15.

% On 2020-03-15 Alice tested positive.
observe tested(alice, positive) from 7 to 8.

/** <examples>
?- go(Timeline).
*/