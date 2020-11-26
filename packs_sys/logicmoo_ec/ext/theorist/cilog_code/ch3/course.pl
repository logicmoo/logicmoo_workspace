% Computational Intelligence: a logical approach.
% CILOG Code. Example database from Section 3.3
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press

% course(C) is true if C is a university course
course(312).
course(322).
course(315).
course(371).

% department(C,D) is true if course C is offered in department D.
department(312,comp_science).
department(322,comp_science).
department(315,math).
department(371,physics).

% student(S) is true if S is a student
student(mary).
student(jane).
student(john).
student(harold).

% female(P) is true if person P is female
female(mary).
female(jane).

% enrolled(S,C) is true if student S is enrolled in course C
enrolled(mary,322).
enrolled(mary,312).
enrolled(john,322).
enrolled(john,315).
enrolled(harold,322).
enrolled(mary,315).
enrolled(jane,312).
enrolled(jane,322).

% cs_course(C) is true if course C is offered in
% the computer science department
cs_course(C) <- department(C,comp_science).

% math_course(C) is true if course C is offered in
% the mathematics department
math_course(C) <- department(C,math).

% cs_or_math_course(C) is true if course C is offered in
% either the computer science department or the
% mathematics department
cs_or_math_course(C) <- cs_course(C).
cs_or_math_course(C) <- math_course(C).

% in_dept(S,D) is true if student S is enrolled
% in a course offered in deparment D
in_dept(S,D) <- enrolled(S,C) & department(C,D).

% EXAMPLE QUERIES
% ask enrolled(S,C) & department(C,D).
% ask cs_course(C).
% ask cs_or_math_course(C).
% ask in_dept(S,D).
