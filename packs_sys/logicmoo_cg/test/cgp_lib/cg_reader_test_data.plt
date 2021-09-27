
:- ensure_loaded(library(cgp_lib/cgp_swipl)).
:- ensure_loaded(library(pfc_lib)).

:- multifile_data(cg_test_data/2). 

cg_test_data([cg_dialect([df,plcg])], "
[PERSON: x] :- [CITIZEN : x].").


cg_test_data([cg_dialect([df,plcg])], "
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :- 
     [PERSON: x]<-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].").


cg_test_data([cg_dialect([df,plcg])], "
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :- 
     [PERSON: ?x]<-childOf-[PERSON: y], 
     [CITIZEN : y]<-memberOf-[COUNTRY : Oz].").


cg_test_data([cg_dialect([df,plcg])], "
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :- 
     [PERSON : x]<-RCPT-[NATURALIZE]-LOC->[COUNTRY : Oz].").


cg_test_data([cg_dialect([df,plcg])], "
[PERSON : Tinman]-
	      -childOf->[GIRL : Dorothy],
	      <-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].").

% end_of_file.



cg_test_data([cg_dialect([lf,sowa])],"[Mat]1-(Attrib)->[Color #1]").
cg_test_data([cg_dialect([lf,sowa])],"[Mat]1-(Attrib)->[Color]2").

cg_test_data([cg_dialect([df])],"[CAT_QUANT:@every]-(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[A_CAT]->(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[THE_CAT:#666]->(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[NAMED_CAT:Moris]->(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[LENGTH:@5ft]<-(SizeOf)-[Mat]").
cg_test_data([cg_dialect([df])],"[LENGTH:@5ft.]<-(SizeOf)-[Mat]").
cg_test_data([cg_dialect([df])],"[CAT_SET_NONE:{}]-(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[CATS_ONE_OR_MORE:{*}]-(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[CAT_FIVE:{*}@5]-(On)->[Mat]").
clacg_test_data([cg_dialect([df])],"[CAT_M:{Moris}]-(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[CAT_FM:{Felix,Moris}]-(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[CAT_SET_MIN_TWO:{Felix,Moris,*}]-(On)->[Mat]").
cg_test_data([cg_dialect([df])],"[CAT_SET_FIVE:{Felix,Moris,*}@5]-(On)->[Mat]").

cg_test_data([cg_dialect([df]), group(1)], "['Man':imad]<-agnt-['Drive']-obj->['Car']").
cg_test_data([cg_dialect([df]), group(1)], "[Cat #1]-(On)->[Mat #1]-(Attrib)->[Color #1]").
cg_test_data([cg_dialect([df]), group(1)], "[Cat: ?x]-(Attrib)->[C1]->(On)->[Mat]").
cg_test_data([cg_dialect([df]), group(1)], "[Cat: ?x]-(On)->[Mat]").
cg_test_data([cg_dialect([df]), group(1)], "[Cat: ?x]-(On)->[*MatC]").
cg_test_data([cg_dialect([df]), group(1)], "[Cat: ?x]-(On)->[Mat: *MatC]").
cg_test_data([cg_dialect([df]), group(1)], "[Man:karim]<-agnt-[Drink]-obj->[Water]").
cg_test_data([cg_dialect([df]), group(1)], "[Mat #1]<- (on)- [Cat #1]").
cg_test_data([cg_dialect([df]), group(1)], "[Mat]<-(On)-[Cat: ?x]").
cg_test_data([cg_dialect([df]), group(1)], "[Color #1]<-(Attrib)-[Mat #1]").
cg_test_data([cg_dialect([df]), group(2)], "[Cat #1]-(On)->[Mat #1]-(Attrib)->[Color #1]").
cg_test_data([cg_dialect([df]), group(2)], "[Man:karim]<-agnt-[Drink]-obj->[Water]").
cg_test_data([cg_dialect([df]), group(2)], "[Color #1] <- (Attrib) -[Mat #1]<- (on)- [Cat#1]").
cg_test_data([cg_dialect([df]), group(3)], "[Cat: @every]->(On)->[Mat]").
cg_test_data([cg_dialect([df]), group(3)], "[CAT]->(STAT)->[SIT]->(LOC)->[MAT].").
cg_test_data([cg_dialect([df]), group(3)], "[CAT]->(STAT)->[SIT]->(LOC)->[MAT]").

cg_test_data([cg_dialect([df]), group(3)], "
   [Drive *x] [Person: Bob] [City: \"St. Louis\"] [Chevy *y]
   (Agnt ?x Bob) (Dest ?x \"St. Louis\") (Thme ?x ?y) (Poss Bob ?y)").

% "

cg_test_data([cg_dialect([df]), group(3)], "  
   [A_CAT] -> (KnowsAbout) ->
   [THE_CAT: #666]  -> (KnowsAbout) ->
   [NAMED_CAT: Moris]  -> (KnowsAbout) ->
   [LENGTH: @ 5ft]  -> (KnowsAbout) ->
   [CAT_SET:{*}]  -> (KnowsAbout) ->
   [CAT5:{*} @ 5 ]  -> (KnowsAbout) -> 
   [CATS_TWO:{Moris, Felix}]  -> (KnowsAbout) ->
   [CATS_ONE_OR_MORE:{Moris,*}]").


cg_test_data([skip,cg_dialect([df]), group(3)], "(IntegerDivide [Integer: *x] [Integer: 7] | [*u] [*v])").

cg_test_data([skip,cg_dialect([df]), group(3)], "
[Function: *Quotient] [Function: *Remainder]
[[@every*x1] [@every*x2] [@every*x3] [@every*x4]
[Equiv: [Iff: (IntegerDivide ?x1 ?x2 | ?x3 ?x4)]
        [Iff: (#?Quotient ?x1 ?x2 | ?x3) (#?Remainder ?x1 ?x2 | ?x4)]]]").

cg_test_data([cg_dialect([df]), group(3)], "[Relation: *r] (Familial ?r) (#?r Bob Sue)").

cg_test_data([skip,cg_dialect([df]), group(3)], "(exists ((r Relation)) (and (Familial r) (r Bob Sue)))").

cg_test_data([cg_dialect([df]), group(3)], "
[SIT]-
  <-(STAT)<-[CAT]
  ->(LOC)->[MAT],.").


cg_test_data([xcall,easy,cg_dialect([df])], "?x -(Attrib)-> [Color #1]").

cg_test_data([xcall,easy,cg_dialect([df])], "?x -(On)->[Mat #1]-(Attrib)->[Color #1]").
cg_test_data([xcall,easy,cg_dialect([df])], "?x -(On)->[Mat #1]").
cg_test_data([xcall,easy,cg_dialect([df])], "[?x] -(Attrib)-> [Color #1]").
cg_test_data([xcall,easy,cg_dialect([df])], "[?x]-(On)->[Mat #1]-(Attrib)->[Color #1]").
cg_test_data([xcall,easy,cg_dialect([df])], "[Mat ?x]-(Attrib)->[Color #1]").
cg_test_data([xcall, group(0)], "[Cat: ?x]-(On)->[Mat #1]-(Attrib)->[Color #2]").

cg_test_data([cg_dialect([df]), group(3)], "[a] - (belives) -> [statement: [Cat: @every]->(On)->[Mat] ]").
cg_test_data([cg_dialect([df]), group(3)], "[a] - (belives) -> [statement2= [Cat: @every]->(On)->[Mat] ]").

cg_test_data([cg_dialect([df]), group(4)], "

[Go]- -
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]").

cg_test_data([skip, cg_dialect([df]), group(4)], "
// ontology required (to load first): aminePlatform/samples/ontology/ManOntology2.xml
[Eat #0] -
   - obj ->[Apple],
   - manr->[Fast],
   - agnt->[Man]").


cg_test_data([cg_dialect([df]), group(4)], "
   [Person: John2] <- (Agnt) - 
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) - [Go2]").


cg_test_data([cg_dialect([df]), group(3)], "
[Begin]-
        -obj->[Session],
        -srce->[Proposition = 
                   [Press] -
                      -obj -> [Key : enter] -partOf-> [Keyboard],
                      -agnt -> [Person : John] 
               ],
        -agnt->[Person : John]").


cg_test_data([cg_dialect([df]), group(3)], "
 [a] - (belives) -> 
 [statement = [Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]  ]").


cg_test_data([cg_dialect([df]), group(3)], "[Go*x][Person:'John'*y][City:'Boston'*z][Bus*w](Agnt?x?y)(Dest?x?z)(Inst?x?w)").

cg_test_data([skip, cg_dialect([df]), group(4)], "
// ontology required (to load first): aminePlatform/samples/ontology/ManOntology2.xml
[Eat #0] -
   - obj->[Apple],
   - manr->[Fast],
   - agnt->[Man]").

cg_test_data([cg_dialect([df]), group(3)], "[Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->[table]").

cg_test_data([cg_dialect([df]), group(3)], "[?x]<-(Agnt)-[Marry]-(Thme)->[Sailor]").

cg_test_data([cg_dialect([df]), group(3)], "
[Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]").

cg_test_data([cg_dialect([df]), group(3)], "
[Proposition: [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]").

cg_test_data([cg_dialect([df]), group(4)], "
[Person: Tom]<-(Expr)-[Believe]-(Thme)->
     [Proposition:  [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]").

cg_test_data([failing,cg_dialect([df]), group(4)], "
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]").

%%Date: Thu, 28 May 92 08:45:26 -0500
%%From: esch%email.sp.unisys.com@metro.ucc.su.OZ.AU (John Esch)
%%To: cg@cs.umn.edu
%%Subject: CG TEST4, individuals

%%The following is used for regression testing of CONSTRUCT.

%%It contains the first set of type and individual examples from Conceptual Structures 
%%page 119 & 120.

%%CANON RESERVATIONS-AND-ELEPHANTS .

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE ARRIVAL-DATE(a) IS [UNIV:*a].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE CIRCUS(c) IS [UNIV:*c].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE CIRCUS(c) IS [UNIV:*c]").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"[ELEPHANT:*c]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS]").                              
                              
cg_test_data([cg_dialect([lf,sowa]), group(1)], "
[Go *x] (Agnt ?x [Person: John]) (Dest ?x [City: Boston]) (Inst ?x [Bus]) 
").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE DEPARTURE-DATE(d) IS [UNIV:*d].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE ELEPHANT(e) IS [UNIV:*e].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE HOTEL(h) IS [UNIV:*h].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE HOTEL-RESERVATION(RESERVATION-NO) IS
[RESERVATION:*RESERVATION-NO]-
  ->(RCPT)->[PERSON]
  ->(OBJ)->[ROOM]->(LOC)->[HOTEL]
  ->(DUR)->[TIME-PERIOD]-
             ->(STRT)->[ARRIVAL-DATE]
             ->(UNTL)->[DEPARTURE-DATE],,.").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE PERFORM(p) IS [UNIV:*p].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE PERSON(p) IS [UNIV:*p].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE PROPOSITION(p) IS [UNIV:*p].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE RESERVATION(r) IS [UNIV:*r].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE ROOM(r) IS [UNIV:*r].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], 
"TYPE TIME-PERIOD(t) IS [UNIV:*t].").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
[RESERVATION #316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
INDIVIDUAL HOTEL-RESERVATION(#316209) IS
[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983],,.").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
INDIVIDUAL HOTEL-RESERVATION(#316210) IS
[RESERVATION:#316210]-
  ->(RCPT)->[PERSON:JOHN ESCH]
  ->(OBJ)->[ROOM:Q3]->(LOC)->[HOTEL:Sidney]
  ->(DUR)->[TIME-PERIOD:@7 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 12 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 19 1983],,.").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
INDIVIDUAL CIRCUS-ELEPHANT(#BUMBO) IS
[ELEPHANT:#BUMBO]<-(AGNT)<-[PERFORM: {*}]->(LOC)->[CIRCUS:Flying Tigers].").

cg_test_data([cg_dialect([lf,sowa]), tokenize_cg, group(1)], "
INDIVIDUAL CIRCUS-ELEPHANT(#JUMBO) IS
[ELEPHANT:#JUMBO]<-(AGNT)<-[PERFORM: {*}]->(LOC)->[CIRCUS:Barnum & Bailey].").

ext_test:- tokenize_cg(T,`INDIVIDUAL CIRCUS-ELEPHANT(#JUMBO) IS
[ELEPHANT:#JUMBO]<-(AGNT)<-[PERFORM: {*}]->(LOC)->[CIRCUS:Barnum & Bailey].`,O),wdmsg(T+O).

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
Leopard::[Animal : x]-isA->[Leopard] :-
   Mammal::[Animal : x]-isA->[Mammal],
   Carnivorous::[Animal : x]-isA->[Carnivorous],
   Fact::[Animal : x]-colorOf->[Color]-attr->[Wild],
   Fact::[Animal : x]-partOf->[Component]-attr->[Dark]. ").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
Mammal::[Animal : x]-isA->[Mammal] :-
   	Fact::[Animal : x]-poss->[Hair].
").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
Carnivorous::[Animal : x]-isA->[Carnivorous] :-
   Fact::[Animal : x]<-agnt-[Eat]-obj->[Meat].
").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "
Carnivorous::[Animal : x]-isA->[Carnivorous] :-
   Fact::[Animal : x]-poss->[Teeth]-attr->[Sharp],
   Fact::[Animal : x]-poss->[Claw],
   Fact::[Animal : x]-has->[Eyes]-attr->[Forward].
").


cg_test_data([cg_dialect([lf,sowa]), group(1)], "
Fact::[Animal : Yala]-
            <-pat-[BelongTo]-bnfcre->[Man : Robert],
            -colorOf->[Color]-attr->[Wild],
            -poss->[Teeth]-attr->[Sharp],
            -has->[Eyes]-attr->[Forward].
").

cg_test_data([cg_dialect([lf,sowa]), group(1)], "Fact::[Animal : Yala]-poss->[Claw].").

% cg_reader_tests
