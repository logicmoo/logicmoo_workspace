% Computational Intelligence: a logical approach. Example 2.28.
% CILOG Code. Copyright 1998, Poole, Mackworth, Goebel and OUP.

% am(H,M) where H is an integer in [1,12] and M is integer in range [0,59]
% denotes the function H:M am.
% For example am(5,33) denotes 5:33am. am(12,30) denotes half-past midnight.
% am(11,59) denotes the time one minute before noon.

% pm(H,M) denotes the time H:M after noon.

% Note that midnight is am(12,00) and noon is pm(12,00).

% before(T1,T2) is true if time T1 is before time T2 in the same day.
before(am(H1,M1),pm(H2,M2)).
before(am(12,M1),am(H2,M2)) <-
   H2<12.
before(am(H1,M1),am(H2,M2)) <-
   H1<H2 &
   H2<12.
before(am(H,M1),am(H,M2)) <-
   M1<M2.
before(pm(12,M1),pm(H2,M2)) <-
   H2<12.
before(pm(H1,M1),pm(H2,M2)) <-
   H1<H2 &
   H2<12.
before(pm(H,M1),pm(H,M2)) <-
   M1<M2.

% EXAMPLE QUERIES
% ask before(am(10,15),pm(3,22)).
% ask before(am(10,15),am(3,22)).
% ask before(am(10,15),am(10,22)).
% ask before(am(10,15),am(11,02)).
