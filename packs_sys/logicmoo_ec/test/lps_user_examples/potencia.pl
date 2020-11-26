:- expects_dialect(lps).

end_of_file.

% System predicates defining simulation of real time:
:- include(system('date_utils.pl')). 

simulatedRealTimeBeginning('2018-04-01'). 
simulatedRealTimePerCycle(21600). % Each cycle = 21600 seconds = 1/4th of a day.
maxTime(40). % 40 cycles = 10 days.
days_difference(Year/Month/Day1,Difference,Year2/Month2/Day2) :-
        n_days_from(Year/Month/Day1,Difference,Year2/Month2/Day2).

fluents		day/1, potencia_total(P). 
events      end_of_day/1.

initially potencial_total(0). 

end_of_day(Year/Month/Day1) updates Old to New in potencia_total(Old) if 
    date_time_stamp(date(Year,Month,Day1,0,0,0,0,-,-), T),
    potencia(s1, T, P1),
    potencia(s2, T, P2), 
    potencia(s3, T, P3), 
    New is P1 + P2 + P3.

d(s1, 0).
d(s2, 10).
d(s3, 5).

a(s1, 5).
a(s2, 3).
a(s3, 4).

b(s1, 10).
b(s2, 6).
b(s3, 8).

c(S, C) :- a(S, A), b(S, B), C is A+B.

v(s1, 400).
v(s2, 200).
v(s3, 150).

potencia(S, T, P) :- d(S, D), v(S, V), a(S, A), c(S, C),
    R is (T - D) mod C,
    (R =< A -> P = 0; P=V).

     