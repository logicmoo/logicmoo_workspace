% Computational Intelligence: a logical approach.
% CILOG Code. Section 3.6.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press

% The following example shows how we can combine databases and
% sophisticated rules in order to represent non-trivial regulations
% of a university. This is intended to show a knowledge-based
% application of our definite clause language.

% This assumes that we have databases about individual students and about
% what grades they achieved in different courses.

% grade(St,Course,Mark) is true if student St achieved Mark in Course.
grade(robin,engl101,87).
grade(robin,phys101,89).
grade(robin,chem101,67).
grade(robin,math101,77).
grade(robin,cs126,84).
grade(robin,cs202,88).
grade(robin,geol101,74).
grade(robin,phys202,92).
grade(robin,math202,81).
grade(robin,cs204,87).
grade(robin,hist101,66).
grade(robin,cs333,77).
grade(robin,cs312,74).
grade(robin,math302,41).
grade(robin,math302,87).
grade(robin,cs304,79).
grade(robin,cs804,80).
grade(robin,psyc303,85).
grade(robin,stats324,91).
grade(robin,cs405,77).

% We also have database relations about the structure of the university,
% and information about different courses.
% dept(Dept,Fac) is true if department Dept is in faculty Fac
dept(history,arts).
dept(english,arts).
dept(psychology,science).
dept(statistics,science).
dept(mathematics,science).
dept(cs,science).

% course(Course,Dept,Level) is true if Course is a course in department Dept,
% at level Level.
course(hist101,history,first).
course(engl101,english,first).
course(psyc303,psychology,third).
course(stats324,statistics,third).
course(math302,mathematics,third).
course(phys101,physics,first).

% We can use rules to represent what is needed for degree requirements. Here
% we give the subset of the degree requirements for a fictional university that
% are applicable for computer science students.
% These will be used to illustrate the power of our representation.

% completed_degree(St,Dept) is true if student St has completed a four-year 
% degree in department Dept. In order to have satisfied these requirements,
% the student must have covered the core courses of the department, satisfied
% the faculty requirements of the faculty in which the department is, they
% must have fulfilled the electives requirement, and must have enough units.
satisfied_degree_requirements(St,Dept) <-
   covers_core_courses(St,Dept) &
   dept(Dept,Fac) &
   satisfies_faculty_req(Fac,St) &
   fulfilled_electives(St,Dept) &
   enough_units(St,Dept).

% covers_core_courses(St,Dept) is true if student St has covered the core 
% courses for department Dept.
covers_core_courses(St,Dept) <-
   core_courses(Dept,CC,MinPass) &
   passed_each(CC,St,MinPass).

% core_courses(Dept,CC,MinPass) is true if CC is the list of core courses
% for department Dept, and MinPass is the passing grade for these core courses.

% The core courses for a cs degree are cs202, math202, cs204, cs304 and math302
% The student must pass each with at least 65% to pass
core_courses(cs,[cs202,math202,cs204,cs304,math302],65).

% passed_each(CL,St,MinPass) is true if student St has a grade of at least
% MinPass in each course in the courses in list CL of courses.
passed_each([],_,_).
passed_each([C|R],St,MinPass) <-
   passed(St,C,MinPass) &
   passed_each(R,St,MinPass).

% passed(St,C,MinPass) is true if student St has a grade of at least
% MinPass in course C
passed(St,C,MinPass) <-
   grade(St,C,Gr) &
   Gr >= MinPass.

% The faculty requirements for the science faculty are that a student must 
% have passed (i.e., have greater than 50%)
% in engl101, phys101, chem101, math101, either cs101 or stats101 and
% either biol101, geol101 or astr101.
satisfies_faculty_req(science,St) <-
   passed_each([engl101,phys101,chem101,math101],St,50) &
   passed_one([cs126,stats101],St,50) &
   passed_one([bil101,geol101,astr101],St,50).

% passed_one(CL,St,MinPass) is true if student St has a grade of at least
% MinPass in one of the courses in the courses in list CL of courses.
passed_one(CL,St,MinPass) <-
   member(C,CL) &
   passed(St,C,MinPass).


% fulfilled_electives(St,Dept) is student St has fulfilled the elective 
% requirements for department Dept.
% A student in a science department needs an arts course 
% (other than engl101) &
% and two science courses from departments other than their own.
% The science courses have to be third or fourth year courses.
fulfilled_electives(St,SciDept) <-
   dept(SciDept,science) &
   has_arts_elective(St) &
   has_sci_elective(St,SciDept,Sci1) &
   has_sci_elective(St,SciDept,Sci2) &
   Sci1 \= Sci2.

% has_arts_elective(St) is true if student St has passed an arts course 
% (other than engl101)
has_arts_elective(St) <-
   passed(St,ArtsEl,50) &
   course(ArtsEl,ArtsDept,_) &
   dept(ArtsDept,arts) &
   ArtsEl \= engl101.

% has_sci_elective(St,Major,SciC) is true if SciC is a third or fourth year 
% course passed by student 
% St. SciC must be in a science department other than department Major.
has_sci_elective(St,Major,SciC) <-
   passed(St,SciC,50) &
   course(SciC,Dept,Level) &
   Dept \= Major &
   dept(Dept,science) &
   member(Level,[third,fourth]).

% enough_units(St,Dept) is true if student St has enough units for a degree
% in department Dept.
enough_units(St,SciDept) <-
   dept(SciDept,science) &
   has_courses(St,_,18,50).

% has_courses(St,L,N,MinPass) is true if L is a list of N courses that student 
% St has passed with a grade of at least MinPass.
has_courses(_,[],0,_).
has_courses(St,[C|R],N1,MinPass) <-
   N is N1-1 &
   has_courses(St,R,N,MinPass) &
   passed(St,C,MinPass) &
   notin(C,R).

% member(X,L) is true if X is an element of list L
member(X,[X|_]).
member(X,[_|R]) <-
   member(X,R).

% notin(X,L) is true if X is not an element of list L
notin(_,[]).
notin(A,[H|T]) <-
   A \= H &
   notin(A,T).

% EXAMPLE QUERIES
% ask satisfied_degree_requirements(robin,cs).
% ask has_courses(robin,L,19,50).
% ask has_courses(robin,L,20,50).  %WARNING: THIS TAKES A *VERY* LONG TIME

% EXERCISES
% Fix up our knowledge base so that a student cannot count
% core courses as electives. 
% Explain why the inequality is needed at the end of the definition of
% fulfilled_electives. 
% Explain why enough_units constructs a list of the courses taken. What is
% the major efficiency problem with this procedure? How can this be fixed?
