/**********************************************************************
 *
 * @(#) examples.pl 1.4@(#)
 *
 */

example(1) :-
	makeEnvironment('ex1','von HJO'),
	initEnvironment,
	defconcept(fatherAcademic,and([male,some(child,academic)])),
	defconcept(grandfatherAcademic,and([male,some(child,fatherAcademic)])),
	assert_ind(tom,tim,child),
	assert_ind(tim,mike,child),
	assert_ind(mike,male),
	assert_ind(mike,academic),
	assert_ind(tim,male),
	assert_ind(tom,male).
%%% Example  2:
%%% KRIS-Example
% setof(C,ask(elementOf(mary,C)),L)
% gives L = ['top',grandparent,parent,parent_with_sons_only,
%            parent_with_two_children,person] 
% in Total runtime 12.167 sec. (05.06.92)
example(2) :-
	makeEnvironment('ex2','krisExample'),
	initEnvironment,
	defprimconcept(male),
	defprimconcept(female,not(male)),
	defconcept(males,some(sex,male)),
	defconcept(females,some(sex,female)),
	defprimconcept(person,some(sex,or([male,female]))),
	defconcept(parent,and([person,some(child,person)])),
	defconcept(mother,and([parent,some(sex,female)])),
	defconcept(father,and([parent,not(mother)])),
	defconcept(grandparent,and([parent,some(child,parent)])),
	defconcept(parent_with_sons_only,and([parent,all(child,some(sex,male))])),
	defconcept(parent_with_two_children,and([parent,atleast(2,child)])),
	assert_ind(tom,father),
	assert_ind(tom,peter,child),
	assert_ind(tom,harry,child),
	assert_ind(mary,parent_with_sons_only),
	assert_ind(mary,tom,child),
	assert_ind(mary,chris,child).
%%% Example  3:
% inconsistent([])
% succeeds in Total runtime 0.000 sec. (05.06.92)
example(3) :-
	makeEnvironment('ex3','Inconsistence'),
	initEnvironment,
	defprimconcept(parent_with_one_child,atmost(1,child)),
	assert_ind(mary,parent_with_one_child),
	assert_ind(mary,tom,child),
	assert_ind(mary,chris,child).
%%% Example  4:
% Modal Operators
example(4) :-
	makeEnvironment('ex4','Inconsistence'),
	initEnvironment,
	defconcept([b(believe,a1)],c1,b(want,a2,car)),
	defconcept([b(believe,a1)],c2,b(want,a3,car)),
	defprimconcept([b(believe,a1)],c1,c2),
	assert_ind([b(believe,a1)],audi,c1).
%%% Example  5:
% subsumes([],c1,c2).
% fails    in Total runtime 0.050 sec. (05.06.92)
% subsumes([],c2,c1).
% succeeds in Total runtime 0.050 sec. (05.06.92)
example(5) :-
	makeEnvironment('ex5','Subsumption'),
	initEnvironment,
	defconcept(c1,and([all(r,a),all(and([r,q]),b)])),
	defconcept(c2,all(and([r,q]),and([a,b]))).
%%% Example  6:
% subsumes([],c1,c2).
% fails    in Total runtime 0.033 sec. (05.06.92)
% subsumes([],c2,c1).
% succeeds in Total runtime 0.033 sec. (05.06.92)
example(6) :-
	makeEnvironment('ex6','Subsumption'),
	initEnvironment,
	defrole(r1,and([r,q])),
	defconcept(d0,and([a,b])),
	defconcept(d1,all(r,a)),
	defconcept(d2,all(r1,b)),
	defconcept(c1,and([d1,d2])),
	defconcept(c2,all(r1,d0)).
%%% Example  7:
example(7) :-
	makeEnvironment('ex7','Subsumption'),
	initEnvironment,
	defconcept(c1,atleast(3,r)),
	defconcept(c2,and([all(and([r,p]),a),all(and([r,q]),not(a)),atleast(2,and([r,p])),atleast(2,and([r,q]))])).
%%% Example  8;
% ask(elementOf(tom,heterosexual))
% succeeds in Total runtime 0.033 sec. (05.06.92)
example(8) :-
	makeEnvironment('ex8','Disjunction of complementary concepts'),
	initEnvironment,
	defprimconcept(male),
	defconcept(female,not(male)),
	defconcept(heterosexual,or([male,female])).
%%% Example  9:
% Variation of the KRIS-Example
% ask(elementOf(chris,male))
% succeeds in Total runtime 0.000 sec. (05.06.92)
example(9) :-
	makeEnvironment('ex9','Variation of the KRIS example'),
	initEnvironment,
	defprimconcept(male),
	defprimconcept(female,not(male)),
	defprimconcept(person,or([male,female])),
	defconcept(parent,and([person,some(child,person)])),
	defconcept(mother,and([parent,female])),
	defconcept(father,and([parent,not(mother)])),
	defconcept(grandparent,and([parent,some(child,parent)])),
	defconcept(parent_with_sons_only,and([parent,all(child,male)])),
	defconcept(parent_with_two_children,and([parent,atleast(2,child)])),
	assert_ind(tom,father),
	assert_ind(tom,peter,child),
	assert_ind(tom,harry,child),
	assert_ind(mary,parent_with_sons_only),
	assert_ind(mary,tom,child),
	assert_ind(mary,chris,child).
%%% Example 10:
% ask(elementOf(tom,c2)) 
% succeeds in Total runtime 0.017 sec. (05.06.92)
example(10) :-
	makeEnvironment('ex10','Inverse Role'),
	initEnvironment,
	defrole(r2,inverse(r1)),
	defconcept(c1,all(r1,c2)),
	defconcept(c3,some(r2,c1)),
	assert_ind(tom,c3).
%%% Example 11:
% inconsistent([])
% succeeds in Total runtime 0.034 sec. (05.06.92)
example(11) :-
	makeEnvironment('ex11','Inconsistence'),
	initEnvironment,
	defconcept(c1,and([atleast(2,child),atmost(1,child)])),
	assert_ind(tom,c1).
%%% Example 12:
% subsumes([],c1,c2)
% succeeds in Total runtime 0.050 sec. (05.06.92)
% subsumes([],c2,c1)
% fails    in Total runtime 0.200 sec. (05.06.92)
example(12) :-
	makeEnvironment('ex12','Subsumption'),
	initEnvironment,
	defconcept(c1,and([person,atleast(2,child)])),
	defconcept(c2,and([person,atleast(3,restr(child,lawyer))])).
%%% Example 13:
% subsumes([],c1,c2)
% succeeds in Total runtime 0.117 sec. (05.06.92)
example(13) :-
	makeEnvironment('ex13','Subsumption'),
	initEnvironment,
	defconcept(c1,and([person,atmost(4,restr(child,doctor))])),
	defconcept(c2,and([person,female,atmost(3,child)])).
%%% Example 14:
% subsumes([],c1,c2)
% succeeds ???
% subsumes([],c2,c1)
% succeeds in Total runtime 0.250 sec. (06.06.92)
example(14) :-
	makeEnvironment('ex14','Subsumption'),
	initEnvironment,
	defconcept(c1,atmost(0,restr(r,and([atleast(3,s1),atleast(4,s2)])))),
	defconcept(c2,all(restr(r,atleast(2,s1)),atmost(2,s2))).
%%% Example 15:
% subsumes([],c2,c1)
% succeeds in Total runtime 0.067 sec. (05.06.92)
example(15) :-
	makeEnvironment('ex15','Subsumption'),
	initEnvironment,
	defconcept(c1,and([person,all(friend,doctor),all(restr(friend,doctor),atleast(1,speciality))])),
	defconcept(c2,and([person,all(friend,atleast(1,speciality))])).
%%% Example 16:
% subsumes([],c2,c1)
% succeeds in Total runtime 0.450 sec. (06.06.92)
example(16) :-
	makeEnvironment('ex16','Subsumption'),
	initEnvironment,
	defconcept(c1,and([atleast(1,restr(child,lawyer)),atleast(1,restr(child,doctor))])),
	defconcept(c2,or([atleast(2,child),atleast(1,restr(child,and([lawyer,doctor])))])).
%%% Example 17:
% subsumes([],c2,c1)
% succeeds in Total runtime 0.034 sec. (05.06.92)
example(17) :-
	makeEnvironment('ex17','Subsumption'),
	initEnvironment,
	defconcept(c1,some(and([child,friend]),doctor)),
	defconcept(c2,and([some(child,doctor),some(friend,doctor)])).
%%% Example 18:
% ask(elementOf(mary,c4))
% succeeds in Total runtime 0.117 sec. (05.06.92)
example(18) :-
	makeEnvironment('ex18','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defconcept(c3,and([atmost(4,child),atleast(2,restr(child,female))])),
	defconcept(c4,atmost(2,restr(child,female))),
	assert_ind(tom,male),
	assert_ind(peter,male),
	assert_ind(mary,peter,child),
	assert_ind(mary,tom,child),
	assert_ind(mary,c3).
%%% Example 19
% ask(elementOf(amy,female))
% succeeds in Total runtime 0.067 sec. (06.06.92)
example(19) :-
	makeEnvironment('ex19','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defconcept(c5,and([atmost(2,restr(child,male))])),
	assert_ind(tom,male),
	assert_ind(peter,male),
	assert_ind(mary,tom,child),
	assert_ind(mary,peter,child),
	assert_ind(mary,amy,child),
	assert_ind(mary,c5).
%%% Example 20
% inconsistent([])
% succeeds in Total runtime 5.167 sec. (05.06.92)
example(20) :-
	makeEnvironment('ex20','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defconcept(c5,and([atmost(2,restr(child,male)),atmost(1,restr(child,female))])),
	assert_ind(tom,male),
	assert_ind(peter,male),
	assert_ind(mary,tom,child),
	assert_ind(mary,peter,child),
	assert_ind(mary,amy,child),
	assert_ind(mary,jane,child),
	assert_ind(mary,c5).
%%% Example 21
% ask(elementOf(betty,female))
example(21) :-
	makeEnvironment('ex21','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defconcept(c1,and([atmost(1,restr(teacher,male)),atmost(1,restr(teacher,female))])),
	defconcept(c2,and([atmost(2,restr(teacher,male)),atmost(1,restr(teacher,female))])),
	assert_ind(tom,c1),
	assert_ind(sue,c1),
	assert_ind(david,c2),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,peter,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher),
	assert_ind(david,betty,teacher),
	assert_ind(david,chris,teacher),
	assert_ind(david,peter,teacher).
%%% Example 22
% ask(elementOf(amy,female))
% should succeeds
% but fails in the current implementation
example(22) :-
	makeEnvironment('ex22','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defrole(maleTeacher,restr(teacher,male)),
	defrole(femaleTeacher,restr(teacher,female)),
	defconcept(c1,and([atmost(1,maleTeacher),atmost(1,femaleTeacher)])),
	defconcept(c2,atmost(1,maleTeacher)),
	assert_ind(tom,c2),
	assert_ind(sue,c1),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,chris,teacher),
	assert_ind(tom,robin,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher).
%%% Example 23
% is a variant of example 23 with user provided names for the 
% restricted roles.
% ask(elementOf(amy,female))
% should succeeds
% but fails in the current implementation
example(23) :-
	makeEnvironment('ex23','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defprimrole(maleTeacher,teacher),
	defprimrole(femaleTeacher,teacher),
	defconcept(c1,and([atmost(1,maleTeacher),atmost(1,femaleTeacher)])),
	defconcept(c2,atmost(1,maleTeacher)),
	assert_ind(tom,c2),
	assert_ind(sue,c1),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,chris,teacher),
	assert_ind(tom,robin,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher).
%%% Example 24
% ask(elementOf(audi,c3))
% succeeds in Total runtime 1.634 sec. (24.06.92)
example(24) :-
	makeEnvironment('ex24','Modal operators'),
	initEnvironment,	
	modalAxioms(kd45,believe,a1),
	defconcept(c1,b(believe,a1,auto)),
	defconcept(c3,b(believe,a1,c1)),
	defconcept([b(believe,a1)],c1,b(believe,a1,auto)),
	defconcept([b(believe,a1)],c3,b(believe,a1,c1)),
	assert_ind(audi,c1).
%%% Example 25
% not(ask(elementOf(audi,c3)))
% succeeds in Total runtime 0.033 sec. (24.06.92)
example(25) :-
	makeEnvironment('ex25','Modal operators'),
	initEnvironment,	
	modalAxioms(kd45,believe,a1),
	defconcept([b(believe,a1)],c1,b(believe,a1,auto)),
	defconcept([b(believe,a1)],c3,b(believe,a1,c1)),
	assert_ind(audi,c1).
%%% Example 26
% subsumes([],c2,c1)
% succeeds in Total runtime 0.034 sec. (24.06.92)
% not(subsumes([],c1,c2))
% succeeds in Total runtime 1.333 sec. (24.06.92)
example(26) :-
	makeEnvironment('ex27','Subsumption'),
	initEnvironment,
	defconcept(c1,atmost(0,r)),
	defconcept(c2,all(r,c5)).
%%% Example 27
% subsumes([],c2,c1) 
% succeeds in Total runtime 0.067 sec. (24.06.92)
% not(subsumes([],c1,c2))
% succeeds
example(27) :-
	makeEnvironment('ex28','Subsumption'),
	initEnvironment,
	defconcept(c1,not(some(r,'top'))),
	defconcept(c2,all(r,c5)).
%%% Example 28
% ask(ex28,[b(believe,john)],elementOf(audi,auto),P)
% succeeds
example(28) :-
	makeEnvironment('ex28','Modal operators'),
	initEnvironment,	
	modalAxioms(kd45,believe,a1),
	modalAxioms(kd45,believe,all),
	defprimconcept(auto),
	assert_ind([b(believe,all)],audi,auto).
%%% Example 29
% is a variant of example 23 with a more restricted definition of c1
% ask(elementOf(amy,female))
% should succeeds
% but fails in the current implementation
example(29) :-
	makeEnvironment('ex29','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defconcept(male,not(female)),
	defprimrole(teacher),
	defrole(maleTeacher,restr(teacher,male)),
	defrole(femaleTeacher,restr(teacher,female)),
	defconcept(c1,and([atmost(1,maleTeacher),atmost(2,femaleTeacher)])),
	assert_ind(tom,c1),
	assert_ind(sue,c1),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,chris,teacher),
	assert_ind(tom,robin,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher).
example(30) :-
	makeEnvironment('ex30','Number restrictions'),
	initEnvironment,
	defprimconcept(female),
	defrole(maleTeacher,restr(teacher,not(female))),
	defrole(femaleTeacher,restr(teacher,female)),
	defconcept(c1,and([atmost(1,maleTeacher),atmost(1,femaleTeacher)])),
	defconcept(c2,atmost(1,maleTeacher)),
	assert_ind(tom,c2),
	assert_ind(sue,c1),
	assert_ind(tom,betty,teacher),
	assert_ind(tom,chris,teacher),
	assert_ind(tom,robin,teacher),
	assert_ind(sue,betty,teacher),
	assert_ind(sue,chris,teacher).
%%% Example 31
% First test example for defclosed
% ask(elementOf(tom,onlyMaleChildren))
% succeeds
example(31) :-
	makeEnvironment('ex31','defclosed'),
	initEnvironment,
	defconcept(onlyMaleChildren,all(child,male)),
	assert_ind(tom,peter,child),
	assert_ind(tom,chris,child),
	assert_ind(tom,tim,child),
	assert_ind(peter,male),
	assert_ind(chris,male),
	assert_ind(tim,male),
	defclosed(tom,_Y,child).
%%% Example 32
% First test example for abduction
% abduce(elementOf(robin,male),H,E)
% abduce(elementOf(robin,female),H,E)
example(32) :-
	makeEnvironment('ex32','abduction'),
	initEnvironment,
	defconcept(male,not(female)).
%%% Example 33
% Second test example for abduction
% abduce(elementOf(nixon,dove),H,E)
% abduce(elementOf(nixon,hawk),H,E)
% gives unexpected results!!!
example(33) :-
	makeEnvironment('ex33','abduction'),
	initEnvironment,
	defconcept(c1,and([quaker,normalQuaker])),
	defconcept(c2,and([republican,normalRepublican])),
	defprimconcept(c1,dove),
	defprimconcept(c2,hawk),
	assert_ind(nixon,quaker),
	assert_ind(nixon,republican).
%%% Example 34
% The following gives an inconsistent specification of
% the penguin - bird problem. So
% inconsistent(ex34)
% succeeds
example(34) :-
	makeEnvironment('ex34',abduction),
	initEnvironment,
	defprimconcept(penguin,and([bird,not(fly)])),
	defprimconcept(bird,fly),
	assert_ind(tweety,penguin),
	assert_ind(john,bird).
%%% Example 35
% This is a consistent specification of the penguin - bird problem.
% abduce(ex35,[],elementOf(john,fly),H,E).
% succeeds with
% H = [in(env(e1),rn(_7982,_7983,_7984,_7985),modal([]),normalBird,john,
%         hyp(_7989),ab(_7991),call(_7993),
%         proved(in([],normalBird,john),hyp(_7989),basedOn(_8005)))],
% E = proved(in([],fly,john),hyp([]),
%            basedOn(and([proved(in([],bird,john),hyp([]),basedOn(abox)),
%                         proved(in([],normalBird,john),hyp([]),
%     basedOn(usingAbHyp(in(env(e1),rn(_7525,_7526,_7527,_7528),modal([]),
%                           normalBird,john,hyp(_7532),ab(_7534),call(_7536),
%                           proved(in([],normalBird,john),hyp(_7532),
%                           basedOn(_7548))))))])))
% and
% abduce(ex35,[],elementOf(tweety,fly),H,E).
% fails
example(35) :-
	makeEnvironment('ex35',abduction),
	initEnvironment,
	defprimconcept(penguin,and([bird,not(normalBird)])),
	defprimconcept(and([bird,normalBird]),fly),
	assert_ind(tweety,penguin),
	assert_ind(john,bird).
%%% Example 36
% Variant of example 33 giving the expected results:
% abduce(ex36,[],elementOf(nixon,dove),H,E).
% succeeds with
% H = [in(env(e4),rn(_8077,_8078,_8079,_8080),modal([]),
%         normalQuaker,nixon,hyp(_8084),ab(_8086),call(_8088),
%         proved(in([],normalQuaker,nixon),hyp(_8084),basedOn(_8100)))],
% E = proved(in([],dove,nixon),hyp([]),
%        basedOn(and([proved(in([],quaker,nixon),hyp([]),basedOn(abox)),
%                     proved(in([],normalQuaker,nixon),hyp([]),
%           basedOn(usingAbHyp(in(env(e4),rn(_7620,_7621,_7622,_7623),
%                   modal([]),normalQuaker,nixon,hyp(_7627),ab(_7629),
%                   call(_7631),proved(in([],normalQuaker,nixon),
%                   hyp(_7627),basedOn(_7643))))))]))) 
% and
% abduce(ex36,[],elementOf(nixon,hawk),H,E).
% succeeds with
% H = [in(env(e4),rn(_8077,_8078,_8079,_8080),modal([]),
%         normalRepublican,nixon, hyp(_8084),ab(_8086),call(_8088),
%         proved(in([],normalRepublican,nixon),hyp(_8084),basedOn(_8100)))],
% E = proved(in([],dove,nixon),hyp([]),
%        basedOn(and([proved(in([],republican,nixon),hyp([]),basedOn(abox)),
%                     proved(in([],normalRepublican,nixon),hyp([]),
%           basedOn(usingAbHyp(in(env(e4),rn(_7620,_7621,_7622,_7623),
%                   modal([]),normalRepublican,nixon,hyp(_7627),ab(_7629),
%                   call(_7631),proved(in([],normalRepublican,nixon),
%                   hyp(_7627),basedOn(_7643))))))]))) 
example(36) :-
	makeEnvironment('ex36','abduction'),
	initEnvironment,
	defprimconcept(and([quaker,normalQuaker]),dove),
	defprimconcept(and([republican,normalRepublican]),hawk),
	assert_ind(nixon,quaker),
	assert_ind(nixon,republican).
%%% Example 37
example(37) :-
	makeEnvironment('ex37','abduction'),
	initEnvironment,
	defprimconcept(rained_last_night,grass_is_wet),
	defprimconcept(sprinkler_was_on,grass_is_wet),
	defprimconcept(grass_is_wet,shoes_are_wet).
%%% Example 38
% ask(elementOf(ideaste,c2))
% should succeed
example(38) :-
	makeEnvironment('ex38','disjunctive_information'),
	initEnvironment,
	assert_ind(ideaste,oedipus,hasChild),
	assert_ind(oedipus,polyneikes,hasChild),
	assert_ind(ideaste,polyneikes,hasChild),
	assert_ind(polyneikes,thersandros,hasChild),
	assert_ind(oedipus,fatherMurderer),
	assert_ind(thersandros,not(fatherMurderer)),
	defconcept(c1,and([fatherMurderer,some(hasChild,not(fatherMurderer))])),
	defconcept(c2,some(hasChild,c1)).
%%% Example 39
% ask(elementOf(lucky,female))
% succeeds
example(39) :-
	makeEnvironment('ex39','negation_as_failure'),
	initEnvironment,
	defrole(parentOf,inverse(childOf)),
	defconcept(male,not(female)),
	defprimconcept(and([some(parentOf,top),naf(not(female))]),female),
	assert_ind(mary,lucky,childOf).
%%% Example 40
% ask(elementOf(peter,richPerson))
% succeeds.
% After
% assert_ind(peter,poorPerson)
% the query
% ask(elementOf(peter,richPerson))
% fails
example(40) :-
	makeEnvironment('ex40','negation_as_failure'),
	initEnvironment,
	defprimconcept(and([doctor,naf(not(richPerson))]),richPerson),
	defconcept(poorPerson,not(richPerson)),
	assert_ind(peter,doctor).
%%% Example 41
% ask(elementOf(tom,richPerson))
% succeeds.
% After 
% assert_ind(tom,poorPerson)
% the query
% ask(elementOf(tom,richPerson))
% fails
example(41) :-
	makeEnvironment('ex41','negation_as_failure'),
	initEnvironment,
	defrole(doctorParentOf,restr(inverse(childOf),doctor)),
	defrole(childOfDoctor,inverse(r1)),
	defprimconcept(and([some(doctorParentOf,top),naf(not(richPerson))]),richPerson),
	defconcept(poorPerson,not(richPerson)),
	assert_ind(chris,doctor),
	assert_ind(chris,tom,childOf).
%%% Example 42
% ask(elementOf(audi,fourWheels))
% succeeds.
% After
% assert_ind(audi,fiveWheels)
% the query
% ask(elementOf(audi,fourWheels))
% fails
example(42) :-
	makeEnvironment('ex42','negation_as_failure'),
	initEnvironment,
	defconcept(fourWheels,and([atleast(4,wheels),atmost(4,wheels)])),
	defconcept(fiveWheels,and([atleast(5,wheels),atmost(5,wheels)])),
	defprimconcept(and([car,naf(not(fourWheels))]),fourWheels),
	assert_ind(audi,car).
%%% Example 43
example(43) :-
	makeEnvironment('ex43','concrete_domains'),
	initEnvironment,
	defconcept(colors,set([b,y,r])),
	defconcept(blueOrYellow,set([b,y])),
	defconcept(red,set([r])),
	defconcept(blue,set([b])),
	defconcept(yellow,set([y])),
	defconcept(redOrYellow,set([r,y])),
	defconcept(blueOrRed,set([b,r])),
	defconcept(yellowOrBlue,set([y,b])).
%%% Example 44
% subsumes(c2,c1)
% should succeed
example(44) :-
	makeEnvironment('ex44','concrete_domains'),
	initEnvironment,
	defconcept(c1,set([a,b])),
	defconcept(c2,set([a,b,c])).
%%% Example 45
example(45) :-
	makeEnvironment('ex45','concrete_domains'),
	initEnvironment,
	defconcept(c1,set([a,b,c])),
	defconcept(c2,set([a,b])),
	defconcept(nc2,not(c2)).
%%% Example 46
% An insufficient specification of 
% The bmw is either yellow, blue, or red but not yellow. 
% ask(elementOf(bmw,c3))
% fails
example(46) :-
	makeEnvironment('ex46','concrete_domains'),
	initEnvironment,
	defconcept(c1,some(hasCol,set([yellow,blue,red]))),
	defconcept(c2,some(hasCol,not(set([yellow])))),
	defconcept(c3,some(hasCol,set([blue,red]))),
	assert_ind(bmw,c1),
	assert_ind(bmw,c2).
%%% Example 47
% A correct specification of
% The bmw is either yellow, blue, or red but not yellow. 
% ask(elementOf(bmw,c3))
% succeeds
example(47) :-
	makeEnvironment('ex47','concrete_domains'),
	initEnvironment,
	defconcept(c1,and([some(hasCol,set([yellow,blue,red])),all(hasCol,set([yellow,blue,red]))])),
	defconcept(c2,some(hasCol,not(set([yellow])))),
	defconcept(c3,some(hasCol,set([blue,red]))),
	assert_ind(bmw,c1),
	assert_ind(bmw,c2).
example(48) :-
	makeEnvironment('ex48','concrete_concepts'),
	initEnvironment,
	defconcept(oneSpouse,and([atleast(1,spouse),atmost(1,spouse)])),
	assert_ind(m1,oneSpouse),
	defprimconcept(some(inverse(spouse),set([m1])),set([g0,g1,g2])),
	assert_ind(g0,oneSpouse),
	defprimconcept(some(inverse(spouse),set([g0])),set([m1,g1,g2])),
	assert_ind(g1,oneSpouse),
	defprimconcept(some(inverse(spouse),set([g1])),set([m1,g0,g2])),
	assert_ind(g2,oneSpouse),
	defprimconcept(some(inverse(spouse),set([g2])),set([m1,g0,g1])),
	defconcept(zeroSH,and([atleast(0,sh),atmost(0,sh)])),
	defconcept(oneSH,and([atleast(1,sh),atmost(1,sh)])),
	defconcept(twoSH,and([atleast(2,sh),atmost(2,sh)])),
	assert_ind(g0,zeroSH),
	assert_ind(g1,oneSH),
	assert_ind(g2,twoSH),
	defprimconcept(and([some(inverse(sh),set([m1])),set([m1])]),bot),
	defprimconcept(and([some(inverse(sh),set([g0])),set([g0])]),bot),
	defprimconcept(and([some(inverse(sh),set([g1])),set([g1])]),bot),
	defprimconcept(and([some(inverse(sh),set([g2])),set([g2])]),bot),
	defprimconcept(and([some(inverse(spouse),set([m1])),some(inverse(sh),set([m1]))]),bot),
	defprimconcept(and([some(inverse(spouse),set([g0])),some(inverse(sh),set([g0]))]),bot),
	defprimconcept(and([some(inverse(spouse),set([g1])),some(inverse(sh),set([g1]))]),bot),
	defprimconcept(and([some(inverse(spouse),set([g2])),some(inverse(sh),set([g2]))]),bot),
%	defconcept(some(sh,set([m1])),some(inverse(sh),set([m1]))),
%	defconcept(some(sh,set([g0])),some(inverse(sh),set([g0]))),
%	defconcept(some(sh,set([g1])),some(inverse(sh),set([g1]))),
%	defconcept(some(sh,set([g2])),some(inverse(sh),set([g2]))).
	defrole(sh,inverse(sh)),
	defrole(spouse,inverse(spouse)).
%%% Example 49
% ask(elementOf(p,c4))
% should fail
example(49) :-
	makeEnvironment('ex49','defaults'),
	initEnvironment,
	defconcept(c4,and([c5,c6])),
	defprimconcept(and([c0,naf(not(c2))]),c5),
	defprimconcept(and([c0,naf(not(c3))]),c6),
	defconcept(c1,or([not(c2),not(c3)])),
	assert_ind(p,c0),
	assert_ind(p,c1).
example(50) :-
	makeEnvironment('ex50','complete_or'),
	initEnvironment,
	defprimconcept(c1,c0),
	defprimconcept(not(c1),c0).
example(51) :-
	makeEnvironment('ex51','functional_dependencies'),
	initEnvironment,
	def(posInfl(f,d)),
	def(posInfl(h,f)),
	def(posInfl(a,b)),
	def(posInfl(b,c)),
	def(posInfl(c,d)),
	def(negInfl(b,e)),
	def(negInfl(e,d)),
	def(posInfl(g,e)),
	def(posInfl(a,g)),
	def(increase(a)).
example(52) :-
	makeEnvironment('ex52','functional_dependencies'),
	initEnvironment,
	def(increase(hasCubicCapacity)),
	def(negInfl(withRebate,hasPrice)),
	def(posInfl(hasPrice,hasOverallCost)),
	def(posInfl(hasCubicCapacity,hasListPrice)),
	def(posInfl(hasListPrice,hasPrice)),
	def(posInfl(hasCubicCapacity,hasFuelConsumption)),
	def(posInfl(hasFuelConsumption,hasOverallCost)),
	def(posInfl(hasCubicCapacity,hasMaxSpeed)),
	def(negInfl(hasCatConverter,hasMaxSpeed)),
	def(posInfl(hasCatConverter,hasFuelConsumption)),
	def(posInfl(hasCubicCapacity,hasWeight)),
	def(negInfl(hasWeight,hasMaxSpeed)).
example(53) :-
	makeEnvironment('ex53','functional_dependencies'),
	initEnvironment,
	def(increase(hasCubicCapacity)),
	def(infl(withRebate,hasPrice,-1.0)),
	def(infl(hasPrice,hasOverallCost,1.0)),
	def(infl(hasCubicCapacity,hasListPrice,1.2)),
	def(infl(hasListPrice,hasPrice,1.0)),
	def(infl(hasCubicCapacity,hasFuelConsumption,0.8)),
	def(infl(hasFuelConsumption,hasOverallCost,1.0)),
	def(infl(hasCubicCapacity,hasHorsePower,1.0)),
	def(infl(hasHorsePower,hasFuelConsumption,1.0)),
	def(infl(hasHorsePower,hasMaxSpeed,1.0)),
	def(infl(hasFuelType,hasMaxSpeed,0.8)),
	def(infl(hasCatConverter,hasHorsePower,-0.5)),
	def(infl(hasCubicCapacity,hasWeight,0.5)),
	def(infl(hasWeight,hasHorsePower,-1.0)).
example(54) :-
	makeEnvironment('ex54','functional_dependencies'),
	initEnvironment,
	def(negInfl(a,b)),
	def(posInfl(b,e)),
	def(posInfl(e,d)),
	def(negInfl(g,e)),
	def(negInfl(a,g)).
%
%	Apart from the notation identical to ex54.
%
example(55) :-
	makeEnvironment('ex55','functional_dependencies'),
	initEnvironment,
	def(infl(a,b,1.0)),
	def(infl(b,e,1.0)),
	def(infl(e,d,1.0)),
	def(infl(g,e,1.0)),
	def(infl(a,g,-1.0)).
example(56) :-
	makeEnvironment('ex56','functional_dependencies'),
	initEnvironment,
	def(infl(a,b,1.0)),
	def(infl(b,e,1.0)),
	def(infl(e,d,1.0)),
	def(infl(g,e,1.0)),
	def(infl(a,g,-1.0)),
	def(infl(f,g,0.5)),
	def(infl(f,h,-0.5)),
	def(infl(h,d,0.3)).
example(57) :-
	makeEnvironment('ex57','functional_dependencies'),
	initEnvironment,
	def(posInfl(a,b)),
	def(posInfl(b,c)),
	def(posInfl(c,d)).
example(58) :- 
	makeEnvironment('ex58','functional_dependencies'),
	initEnvironment,
	def(posInfl(a,b)),
	def(posInfl(b,c)),
	def(posInfl(c,d)),
	def(infl(e,b,-1.0)),
	def(infl(e,c,0.5)).
example(59) :-
	sb_defenv('mybox','sb.lit'),
	sb_initenv,
	sb_primconcept(person),
	sb_primconcept(woman,[supers([person])]),
	sb_primconcept(man,[supers([person])]),
	sb_disjoint(man,woman),
	sb_primelemrole(child,'domain-range'(parent,person,person)),
	sb_defconcept(parent,[supers([person]),
                              nr(child,1,30,2)]),
	sb_defconcept(mother,[supers([parent,woman])]),
	sb_defconcept(father,[supers([parent,man])]),
	sb_defconcept(granni,[supers([grandparent,mother])]),
	sb_defelem(harry,[isa(parent)]),
	sb_defelem(mary,[isa(mother), 
                         irole(child, 
                               iname('marys-child'),
                               [nr(1,30,2), vr(harry)])]).
example(60) :-
	makeEnvironment('ex60','Modal operators'),
	initEnvironment,	
	modalAxioms(kd45,believe,peter),
	defprimconcept([b(believe,peter)],doctor,richPerson),
	assert_ind([b(believe,peter)],tom,doctor).

