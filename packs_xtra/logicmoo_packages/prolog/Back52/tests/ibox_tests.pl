
%% ******************************************************************
%%
%% Test file for testing ibox inferences  Version 1.2  May 13, 91
%%
%% Number of tests: 15
%%
 
 
iboxtest :-
	write('*** TEST FILE: ibox_tests.pl'), nl,
	itest(1).
 
itest(I) :-
	iboxtest(I),
	I1 is I + 1,
	I1 < 16,
	itest(I1), !.
itest(_).
 
write_ibox_ok(T) :-
	write('+++ ibox test '), write(T), write(' succeeded'),
	nl.

write_ibox_failed(T) :-
	( T == 8 ->
	      write('~~~ ibox test'), write(T), 
	      write(' failed (known incompleteness)')
	;  write('--- ibox test '), write(T), write(' failed'),
	   assert(test(ibox_test(T), failed))),
	nl.

 
%% ===================================================================
 
%% Test H1
%%   Hybrid inheritance of implication links; the left hand
%%   concept has a primitive subconcept, the right hand concept
%%   has a primitive superconcept
 
iboxtest(T) :-
	T == 1,
    backinit,
 
    c1   :<   anything,
    c4   :<   anything,
    c2   :<   c4,
    c3   :<   c1,
    r    :<   domain(anything) and range(anything),
 
    c1   =>   c2,
 
%% expected inferences:
    c1 ?< c4,
    c3 ?< c2,
    c3 ?< c4,
%no_vr    all(r,c1) ?< all(r,c4),
%no_vr    all(r,c3) ?< all(r,c2),
%no_vr    all(r,c3) ?< all(r,c4),
    write_ibox_ok(T), !.

 
%%--------------------------------------------------------------------
 
 
%% Test H2
%%   Same as H1 but with i-link on value restrictions
 
iboxtest(T) :-
    T == 2, backinit, 
 
           p1   :<   anything,
           p4   :<   anything,
           p2   :<   p4,
           p3   :<   p1,
           r    :<   domain(anything) and range(anything),
 
    all(r,p1)  =>   all(r,p2),
 
%% expected inferences:
    all(r,p1) ?< all(r,p4),
    all(r,p3) ?< all(r,p2),
    all(r,p3) ?< all(r,p4),
    write_ibox_ok(T), !.
 
%%--------------------------------------------------------------------
%% Test H3
%%   Same as before but defined concepts with number restrictions
%%   instead of primitive ones
 
iboxtest(T) :-
    T == 3, backinit,
 
    r1   :<   domain(anything) and range(anything),
    r2   :<   domain(anything) and range(anything),
    c1   :=   atleast(4,r1),
    c2   :=   atmost(5,r2),
 
    atleast(2,r1)  => atmost(4,r2),
 
%% expected inferences:
    atleast(2,r1) ?< c2,
    c1 ?< atmost(4,r2),
    c1 ?< c2,
    write_ibox_ok(T), !.
 
%%--------------------------------------------------------------------
%% Test H4
%%   Hybrid equivalence: the right hand concept is a terminological
%%   subconcept of the left hand one
 
iboxtest(T) :-
    T == 4, backinit,
 
    c1   :<   anything,
    c2   :<   c1,
 
    c1   =>   c2,
 
%% expected inferences:
    c2 ?< c1,
    c1 ?<  c2,
    write_ibox_ok(T), !.

%%--------------------------------------------------------------------
 
%% Test H5
%%   Same as H5, but defined concepts
 
iboxtest(T) :-
    T == 5, backinit,
 
    r    :<   domain(anything) and range(anything),
    c1   :=   atleast(6,r) and atmost(7,r),
 
    atleast(5,r) => atleast(10,r),
 
%% expected inferences:
    c1 ?< nothing,
    write_ibox_ok(T), !.
 
%%--------------------------------------------------------------------
 
%% Test H6
%%   Explicit disjointness of left hand side and right hand side
 
iboxtest(T) :-
    T == 6, backinit,
 
    p1   :<   anything,
    p2   :<   anything and not(p1),
    r    :<   domain(anything) and range(anything),
    no   :=   atleast(1,r) and atmost(0,r),
 
    p1   =>   p2 ,
 
%% expected inferences:
    p1 ?< nothing,
%no_vr    all(r, p1) ?< all(r,no),
%no_vr    all(r,p1) ?< atmost(0,r),
    write_ibox_ok(T), !.
 
%%--------------------------------------------------------------------
 
%% Test H7
%%   Same as H6 with more i-links
 
iboxtest(T) :-
    T == 7, backinit,
 
    p1   :<   anything,
    p2   :<   anything and not(p1),
    p3   :<   anything,
    p4   :<   anything,
    r    :<   domain(anything) and range(anything),
    no   :=   atleast(1,r) and atmost(0,r),
 
    p1   =>   p2 ,
    p1   =>   p3 ,
    p1   =>   p4 ,
 
%% expected inferences:
    p1 ?< nothing,
%no_vr    all(r,p1) ?<  all(r,no),
%no_vr    all(r,p1) ?< atmost(0,r),
    write_ibox_ok(T), !.
 
%%--------------------------------------------------------------------
 
%% Test H8
%%   Same as H6 with implicit disjointness
 
iboxtest(T) :-
    T == 8, backinit,
 
    r    :<   domain(anything) and range(anything),
    s    :<   domain(anything) and range(anything),
    no   :=   atleast(1,r) and atmost(0,r),
 
    atleast(1,s)   =>   atmost(0,s),
 
%% expected inferences:
    atleast(1,s) ?< no,
    anything ?< atmost(0,s),
% FFS: complete jjq
     write_ibox_ok(T), !.

%%====================================================================
 
%% Test H9
%%   Multiple i-links from the same left hand concept
 
iboxtest(T) :-
    T == 9, backinit,
 
    p1   :<   anything,
    p2   :<   anything,
    p3   :<   anything,
    r    :<   domain(anything) and range(anything),
 
    p1   =>   p2,
    p1   =>   p3,
 
%% expected inferences:
    p1 ?< p2 and p3,
%no_vr    all(r,p1) ?< all(r,p2 and p3),
    write_ibox_ok(T), !.

%%--------------------------------------------------------------------
 
%% Test H10
%%   Same as H9 but with defined concepts
 
iboxtest(T) :-
    T == 10, backinit,
 
    r    :<   domain(anything) and range(anything),
    s    :<   domain(anything) and range(anything),
 
    atleast(2,r)   =>   atleast(5,s) and atmost(8,s),
    atleast(2,r)   =>   atleast(3,s) and atmost(6,s),
 
%% expected inferences:
    atleast(2,r) ?< atleast(5,s) and atmost(6,s),
    write_ibox_ok(T), !.

%%=====================================================================
 
%% Test H11
%%   Transitive implication links
 
iboxtest(T) :-
    T == 11, backinit,
 
    p1   :<   anything,
    p2   :<   anything,
    p3   :<   anything,
    r    :<   domain(anything) and range(anything),
 
    p1   =>   p2 ,
    p2   =>   p3 ,
 
%% expected inferences:
    p1 ?< p2 and p3,
%no_vr    all(r,p1) ?< all(r,p2 and p3),
    write_ibox_ok(T), !.
 
%%--------------------------------------------------------------------
 
%% Test H12
%%   same as H11 but with defined concepts
 
iboxtest(T) :-
    T == 12, backinit,
 
    r    :<   domain(anything) and range(anything),
    s    :<   domain(anything) and range(anything),
    t    :<   domain(anything) and range(anything),
 
 
     atmost(3,r)   =>   atleast(4,s),
     atleast(4,s)  =>   atleast(6,t),
 
%% expected inferences:
    atmost(3,r) ?< atleast(4,s) and atleast(6,t),
    write_ibox_ok(T), !.

%%--------------------------------------------------------------------
 
%% Test H13
%%   same as H11 but transitive disjointness
 
iboxtest(T) :-
    T == 13, backinit,
 
    p1   :<   anything,
    p2   :<   anything,
    p3   :<   anything and not(p1),
    r    :<   domain(anything) and range(anything),
    no   :=   atleast(1,r) and atmost(0,r),
 
    p1   =>   p2 ,
    p2   =>   p3 ,
 
%% expected inferences:
    p1 ?< no,
%no_vr    all(r,p1) ?< all(r,no),
    write_ibox_ok(T), !.
 
%%--------------------------------------------------------------------
 
%% Test H14
%%   same as H13 but with defined concepts
 
iboxtest(T) :-
    T == 14, backinit,
 
    r    :<   domain(anything) and range(anything),
    s    :<   domain(anything) and range(anything),
    t    :<   domain(anything) and range(anything),
    no   :=   atleast(1,t) and atmost(0,t),
 
    atmost(3,r)   =>   atleast(4,s),
    atleast(4,s)  =>   atleast(4,r),
 
%% expected inferences:
    atmost(3,r) ?< no,
    write_ibox_ok(T), !.

%%--------------------------------------------------------------------
 
%% Test H15
%%   same as H14 but with indirect transitivity because of
%%   inheritance
 
iboxtest(T) :-
    T == 15, backinit,
 
    r    :<   domain(anything) and range(anything),
    s    :<   domain(anything) and range(anything),
    t    :<   domain(anything) and range(anything),
    no   :=   atleast(1,t) and atmost(0,t),
 
    atmost(3,r)   =>   atleast(4,s),
    atleast(2,s)  =>   atleast(4,r),
 
%% expected inferences:
    atmost(3,r) ?< no,
    write_ibox_ok(T), !.



%% catch failed tests


iboxtest(T) :-
	write_ibox_failed(T).

