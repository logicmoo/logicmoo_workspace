# logicmoo_cg
Conceptual Graph (CG) Libraries in Prolog


```
/pack/logicmoo_cg/prolog# cls ; swipl -l cgprolog.pl -t halt -g cg_reader_tests
```



```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[PERSON: x] :- [CITIZEN : x].").
% ===========================================

named_graph(
   anonymous(70000001),
   [ preconds(
        [ named_graph(
             anonymous(70000001),
             [ frame_var('X',Person_X_Citizen),
               cg_type(Person_X_Citizen,'Citizen') ]) ]),
     frame_var('X',Person_X_Citizen),
     cg_type(Person_X_Citizen,'Person') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON: x]<-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].").
% ===========================================

named_graph(
   anonymous(70000002),
   [ preconds(
        [ named_graph(
             anonymous(70000002),
             [ cg_holds('Loc',Being_Born,Oz_Country7),
               cg_name(Oz_Country7,'Oz'),
               cg_type(Oz_Country7,'Country'),
               cg_holds('Agnt',Being_Born,X_Person),
               cg_type(Being_Born,'Being_Born'),
               frame_var('X',X_Person),
               cg_type(X_Person,'Person') ]) ]),
     cg_holds(memberOf,Oz_Country7,X_Citizen),
     cg_name(Oz_Country7,'Oz'),
     cg_type(Oz_Country7,'Country'),
     frame_var('X',X_Citizen),
     cg_type(X_Citizen,'Citizen') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON: ?x]<-childOf-[PERSON: y],
     [CITIZEN : y]<-memberOf-[COUNTRY : Oz].").
% ===========================================

named_graph(
   anonymous(70000003),
   [ preconds(
        [ named_graph(
             anonymous(70000003),
             [ cg_holds(memberOf,Oz_Country10,Y_Citizen8),
               cg_name(Oz_Country10,'Oz'),
               cg_type(Oz_Country10,'Country'),
               frame_var('Y',Y_Citizen8),
               cg_type(Y_Citizen8,'Citizen'),
               cg_holds(childOf,Y_Person6,X),
               frame_var('Y',Y_Person6),
               cg_type(Y_Person6,'Person'),
               cg_quantz(e,X),
               cg_type(X,'Person') ]) ]),
     cg_holds(memberOf,Oz_Country10,X_Citizen),
     cg_name(Oz_Country10,'Oz'),
     cg_type(Oz_Country10,'Country'),
     frame_var('X',X_Citizen),
     cg_type(X_Citizen,'Citizen') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[CITIZEN : x]<-memberOf-[COUNTRY : Oz] :-
     [PERSON : x]<-RCPT-[NATURALIZE]-LOC->[COUNTRY : Oz].").
% ===========================================

named_graph(
   anonymous(70000004),
   [ preconds(
        [ named_graph(
             anonymous(70000004),
             [ cg_holds('Loc',Naturalize,Oz_Country7),
               cg_name(Oz_Country7,'Oz'),
               cg_type(Oz_Country7,'Country'),
               cg_holds('Rcpt',Naturalize,X_Person),
               cg_type(Naturalize,'Naturalize'),
               frame_var('X',X_Person),
               cg_type(X_Person,'Person') ]) ]),
     cg_holds(memberOf,Oz_Country7,X_Citizen),
     cg_name(Oz_Country7,'Oz'),
     cg_type(Oz_Country7,'Country'),
     frame_var('X',X_Citizen),
     cg_type(X_Citizen,'Citizen') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[PERSON : Tinman]-
              -childOf->[GIRL : Dorothy],
              <-AGNT-[Being_Born]-LOC->[COUNTRY : Oz].").
% ===========================================

named_graph(
   anonymous(70000005),
   [ cg_holds('Loc',Being_Born,Oz_Country),
     cg_name(Oz_Country,'Oz'),
     cg_type(Oz_Country,'Country'),
     cg_holds('Agnt',Being_Born,Tinman_Person),
     cg_type(Being_Born,'Being_Born'),
     cg_holds(childOf,Tinman_Person,Dorothy_Girl),
     cg_name(Dorothy_Girl,'Dorothy'),
     cg_type(Dorothy_Girl,'Girl'),
     cg_name(Tinman_Person,'Tinman'),
     cg_type(Tinman_Person,'Person') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Mat]1-(Attrib)->[Color #1]").
% ===========================================

named_graph(
   anonymous(70000006),
   [ cg_holds('Attrib',Mat,Color),
     cg_equal(Color,'Color#1'),
     cg_type(Color,'Color'),
     cg_equal(Mat,'Mat#1'),
     cg_type(Mat,'Mat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Mat]1-(Attrib)->[Color]2").
% ===========================================

named_graph(
   anonymous(70000007),
   [ cg_holds('Attrib',Mat,Color),
     cg_equal(Color,'Color#2'),
     cg_type(Color,'Color'),
     cg_equal(Mat,'Mat#1'),
     cg_type(Mat,'Mat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT_QUANT:@every]-(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000008),
   [ cg_holds('On',Every_Cat_Quant,Mat),
     cg_type(Mat,'Mat'),
     cg_quantz(every,Every_Cat_Quant),
     cg_type(Every_Cat_Quant,'Cat_Quant') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[A_CAT]->(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000009),
   [ cg_holds('On',A_Cat,Mat),
     cg_type(Mat,'Mat'),
     cg_type(A_Cat,'A_Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[THE_CAT:#666]->(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000010),
   [ cg_holds('On',The_Cat,Mat),
     cg_type(Mat,'Mat'),
     cg_equal(The_Cat,'The_Cat#666'),
     cg_type(The_Cat,'The_Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[NAMED_CAT:Moris]->(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000011),
   [ cg_holds('On',Moris_Named_Cat,Mat),
     cg_type(Mat,'Mat'),
     cg_name(Moris_Named_Cat,'Moris'),
     cg_type(Moris_Named_Cat,'Named_Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[LENGTH:@5ft]<-(SizeOf)-[Mat]").
% ===========================================

named_graph(
   anonymous(70000012),
   [ cg_holds('SizeOf',Mat,Ft_Length_Num5_Num5),
     cg_type(Mat,'Mat'),
     frame_var('FT',Ft_Length_Num5_Num5),
     cg_quantz(5,Ft_Length_Num5_Num5),
     cg_type(Ft_Length_Num5_Num5,'Length') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[LENGTH:@5ft.]<-(SizeOf)-[Mat]").
% ===========================================

named_graph(
   anonymous(70000013),
   [ cg_holds('SizeOf',Mat,Ft_Length_Num5_Num5),
     cg_type(Mat,'Mat'),
     frame_var('FT',Ft_Length_Num5_Num5),
     cg_quantz(5,Ft_Length_Num5_Num5),
     cg_type(Ft_Length_Num5_Num5,'Length') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT_SET_NONE:{}]-(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000014),
   [ cg_holds('On',Cat_Set_None,Mat),
     cg_type(Mat,'Mat'),
     cg_count(Cat_Set_None,0,0),
     cg_type(Cat_Set_None,'Cat_Set_None') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CATS_ONE_OR_MORE:{*}]-(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000015),
   [ cg_holds('On',Set_Cats_One_Or_More,Mat),
     cg_type(Mat,'Mat'),
     cg_count(Set_Cats_One_Or_More,1,Cg_Count),
     cg_quantz(set,Set_Cats_One_Or_More),
     cg_type(Set_Cats_One_Or_More,'Cats_One_Or_More') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT_FIVE:{*}@5]-(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000016),
   [ cg_holds('On',Set_Cat_Five_Num5_Num5,Mat),
     cg_type(Mat,'Mat'),
     cg_quantz(5,Set_Cat_Five_Num5_Num5),
     cg_count(Set_Cat_Five_Num5_Num5,1,Cg_Count),
     cg_quantz(set,Set_Cat_Five_Num5_Num5),
     cg_type(Set_Cat_Five_Num5_Num5,'Cat_Five') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT_FM:{Felix,Moris}]-(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000017),
   [ cg_holds('On',FelixMoris_Set_Cat_Fm,Mat),
     cg_type(Mat,'Mat'),
     cg_values(
        FelixMoris_Set_Cat_Fm,
        [ 'Felix',
          'Moris' ]),
     cg_count(FelixMoris_Set_Cat_Fm,2,Cg_Count),
     cg_quantz(set,FelixMoris_Set_Cat_Fm),
     cg_type(FelixMoris_Set_Cat_Fm,'Cat_Fm') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT_SET_MIN_TWO:{Felix,Moris,*}]-(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000018),
   [ cg_holds('On',FelixMoris_Cat_Set_Min_Two,Mat),
     cg_type(Mat,'Mat'),
     cg_values(
        FelixMoris_Cat_Set_Min_Two,
        [ 'Felix',
          'Moris' ]),
     cg_count(FelixMoris_Cat_Set_Min_Two,2,Cg_Count),
     cg_quantz(set,FelixMoris_Cat_Set_Min_Two),
     cg_type(FelixMoris_Cat_Set_Min_Two,'Cat_Set_Min_Two') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT_SET_FIVE:{Felix,Moris,*}@5]-(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000019),
   [ cg_holds('On',FelixMoris_Cat_Set_Five_Num5_Num5,Mat),
     cg_type(Mat,'Mat'),
     cg_quantz(5,FelixMoris_Cat_Set_Five_Num5_Num5),
     cg_values(
        FelixMoris_Cat_Set_Five_Num5_Num5,
        [ 'Felix',
          'Moris' ]),
     cg_count(FelixMoris_Cat_Set_Five_Num5_Num5,2,Cg_Count),
     cg_quantz(set,FelixMoris_Cat_Set_Five_Num5_Num5),
     cg_type(FelixMoris_Cat_Set_Five_Num5_Num5,'Cat_Set_Five') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"['Man':imad]<-agnt-['Drive']-obj->['Car']").
% ===========================================

named_graph(
   anonymous(70000020),
   [ cg_holds(obj,Drive,Car),
     cg_type(Car,'Car'),
     cg_holds(agnt,Drive,Imad_Man),
     cg_type(Drive,'Drive'),
     frame_var('IMAD',Imad_Man),
     cg_type(Imad_Man,'Man') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat #1]-(On)->[Mat #1]-(Attrib)->[Color #1]").
% ===========================================

named_graph(
   anonymous(70000021),
   [ cg_holds('Attrib',Mat,Color),
     cg_equal(Color,'Color#1'),
     cg_type(Color,'Color'),
     cg_holds('On',Cat,Mat),
     cg_equal(Mat,'Mat#1'),
     cg_type(Mat,'Mat'),
     cg_equal(Cat,'Cat#1'),
     cg_type(Cat,'Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat: ?x]-(Attrib)->[C1]->(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000022),
   [ cg_holds('On',C1,Mat),
     cg_type(Mat,'Mat'),
     cg_holds('Attrib',X,C1),
     cg_type(C1,'C1'),
     cg_quantz(e,X),
     cg_type(X,'Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat: ?x]-(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000023),
   [ cg_holds('On',X,Mat),
     cg_type(Mat,'Mat'),
     cg_quantz(e,X),
     cg_type(X,'Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat: ?x]-(On)->[*MatC]").
% ===========================================

named_graph(
   anonymous(70000024),
   [ cg_holds('On',X,Matc),
     frame_var('MATC',Matc),
     cg_quantz(e,X),
     cg_type(X,'Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat: ?x]-(On)->[Mat: *MatC]").
% ===========================================

named_graph(
   anonymous(70000025),
   [ cg_holds('On',X,Mat),
     frame_var('MATC',Mat),
     cg_type(Mat,'Mat'),
     cg_quantz(e,X),
     cg_type(X,'Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Man:karim]<-agnt-[Drink]-obj->[Water]").
% ===========================================

named_graph(
   anonymous(70000026),
   [ cg_holds(obj,Drink,Water),
     cg_type(Water,'Water'),
     cg_holds(agnt,Drink,Karim_Man),
     cg_type(Drink,'Drink'),
     frame_var('KARIM',Karim_Man),
     cg_type(Karim_Man,'Man') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Mat #1]<- (on)- [Cat #1]").
% ===========================================

named_graph(
   anonymous(70000027),
   [ cg_holds(on,Cat,Mat),
     cg_equal(Cat,'Cat#1'),
     cg_type(Cat,'Cat'),
     cg_equal(Mat,'Mat#1'),
     cg_type(Mat,'Mat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Mat]<-(On)-[Cat: ?x]").
% ===========================================

named_graph(
   anonymous(70000028),
   [ cg_holds('On',X,Mat),
     cg_quantz(e,X),
     cg_type(X,'Cat'),
     cg_type(Mat,'Mat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Color #1]<-(Attrib)-[Mat #1]").
% ===========================================

named_graph(
   anonymous(70000029),
   [ cg_holds('Attrib',Mat,Color),
     cg_equal(Mat,'Mat#1'),
     cg_type(Mat,'Mat'),
     cg_equal(Color,'Color#1'),
     cg_type(Color,'Color') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat #1]-(On)->[Mat #1]-(Attrib)->[Color #1]").
% ===========================================

named_graph(
   anonymous(70000030),
   [ cg_holds('Attrib',Mat,Color),
     cg_equal(Color,'Color#1'),
     cg_type(Color,'Color'),
     cg_holds('On',Cat,Mat),
     cg_equal(Mat,'Mat#1'),
     cg_type(Mat,'Mat'),
     cg_equal(Cat,'Cat#1'),
     cg_type(Cat,'Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Man:karim]<-agnt-[Drink]-obj->[Water]").
% ===========================================

named_graph(
   anonymous(70000031),
   [ cg_holds(obj,Drink,Water),
     cg_type(Water,'Water'),
     cg_holds(agnt,Drink,Karim_Man),
     cg_type(Drink,'Drink'),
     frame_var('KARIM',Karim_Man),
     cg_type(Karim_Man,'Man') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Color #1] <- (Attrib) -[Mat #1]<- (on)- [Cat#1]").
% ===========================================

named_graph(
   anonymous(70000032),
   [ cg_holds(on,Cat,Mat),
     cg_equal(Cat,'Cat#1'),
     cg_type(Cat,'Cat'),
     cg_holds('Attrib',Mat,Color),
     cg_equal(Mat,'Mat#1'),
     cg_type(Mat,'Mat'),
     cg_equal(Color,'Color#1'),
     cg_type(Color,'Color') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Cat: @every]->(On)->[Mat]").
% ===========================================

named_graph(
   anonymous(70000033),
   [ cg_holds('On',Every_Cat,Mat),
     cg_type(Mat,'Mat'),
     cg_quantz(every,Every_Cat),
     cg_type(Every_Cat,'Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT]->(STAT)->[SIT]->(LOC)->[MAT].").
% ===========================================

named_graph(
   anonymous(70000034),
   [ cg_holds('Loc',Sit,Mat),
     cg_type(Mat,'Mat'),
     cg_holds('Stat',Cat,Sit),
     cg_type(Sit,'Sit'),
     cg_type(Cat,'Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[CAT]->(STAT)->[SIT]->(LOC)->[MAT]").
% ===========================================

named_graph(
   anonymous(70000035),
   [ cg_holds('Loc',Sit,Mat),
     cg_type(Mat,'Mat'),
     cg_holds('Stat',Cat,Sit),
     cg_type(Sit,'Sit'),
     cg_type(Cat,'Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
   [Drive *x] [Person: Bob] [City: "St. Louis"] [Chevy *y]
   (Agnt ?x Bob) (Dest ?x "St. Louis") (Thme ?x ?y) (Poss Bob ?y)").
% ===========================================

named_graph(
   anonymous(70000036),
   [ cg_holds('Poss','Bob',Y_Chevy),
     cg_holds(
        'Thme',
        ?('X'),
        Y_Chevy),
     cg_holds(
        'Dest',
        ?('X'),
        "St. Louis"),
     cg_holds(
        'Agnt',
        ?('X'),
        'Bob'),
     frame_var('Y',Y_Chevy),
     cg_type(Y_Chevy,'Chevy'),
     textOf(C34_St_C46_C32_Louis_C34_City,"St. Louis"),
     cg_type(C34_St_C46_C32_Louis_C34_City,'City'),
     cg_name(Bob_Person,'Bob'),
     cg_type(Bob_Person,'Person'),
     frame_var('X',X_Drive),
     cg_type(X_Drive,'Drive') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
   [A_CAT] -> (KnowsAbout) ->
   [THE_CAT: #666]  -> (KnowsAbout) ->
   [NAMED_CAT: Moris]  -> (KnowsAbout) ->
   [LENGTH: @ 5ft]  -> (KnowsAbout) ->
   [CAT_SET:{*}]  -> (KnowsAbout) ->
   [CAT5:{*} @ 5 ]  -> (KnowsAbout) ->
   [CATS_TWO:{Moris, Felix}]  -> (KnowsAbout) ->
   [CATS_ONE_OR_MORE:{Moris,*}]").
% ===========================================

named_graph(
   anonymous(70000037),
   [ cg_holds('KnowsAbout',MorisFelix_Set_Cats_Two,Moris_Set_Cats_One_Or_More),
     cg_values(
        Moris_Set_Cats_One_Or_More,
        ['Moris']),
     cg_count(Moris_Set_Cats_One_Or_More,1,Cg_Count),
     cg_quantz(set,Moris_Set_Cats_One_Or_More),
     cg_type(Moris_Set_Cats_One_Or_More,'Cats_One_Or_More'),
     cg_holds('KnowsAbout',Set_Cat5_Num5_Num5,MorisFelix_Set_Cats_Two),
     cg_values(
        MorisFelix_Set_Cats_Two,
        [ 'Moris',
          'Felix' ]),
     cg_count(MorisFelix_Set_Cats_Two,2,Cg_Count20),
     cg_quantz(set,MorisFelix_Set_Cats_Two),
     cg_type(MorisFelix_Set_Cats_Two,'Cats_Two'),
     cg_holds('KnowsAbout',Cat_Set,Set_Cat5_Num5_Num5),
     cg_quantz(5,Set_Cat5_Num5_Num5),
     cg_count(Set_Cat5_Num5_Num5,1,Cg_Count21),
     cg_quantz(set,Set_Cat5_Num5_Num5),
     cg_type(Set_Cat5_Num5_Num5,'Cat5'),
     cg_holds('KnowsAbout',Ft_Length_Num5_Num5,Cat_Set),
     cg_count(Cat_Set,1,Cg_Count22),
     cg_quantz(set,Cat_Set),
     cg_type(Cat_Set,'Cat_Set'),
     cg_holds('KnowsAbout',Moris_Named_Cat,Ft_Length_Num5_Num5),
     frame_var('FT',Ft_Length_Num5_Num5),
     cg_quantz(5,Ft_Length_Num5_Num5),
     cg_type(Ft_Length_Num5_Num5,'Length'),
     cg_holds('KnowsAbout',The_Cat,Moris_Named_Cat),
     cg_name(Moris_Named_Cat,'Moris'),
     cg_type(Moris_Named_Cat,'Named_Cat'),
     cg_holds('KnowsAbout',A_Cat,The_Cat),
     cg_equal(The_Cat,'The_Cat#666'),
     cg_type(The_Cat,'The_Cat'),
     cg_type(A_Cat,'A_Cat') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Relation: *r] (Familial ?r) (#?r Bob Sue)").
% ===========================================

named_graph(
   anonymous(70000038),
   [ cg_holds(R,'Bob','Sue'),
     R=Relation,
     cg_holds('Familial',Relation),
     frame_var('R',Relation),
     cg_type(Relation,'Relation') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[SIT]-
  <-(STAT)<-[CAT]
  ->(LOC)->[MAT],.").
% ===========================================

named_graph(
   anonymous(70000039),
   [ cg_holds('Loc',Cat,Mat),
     cg_type(Mat,'Mat'),
     cg_holds('Stat',Cat,Sit),
     cg_type(Cat,'Cat'),
     cg_type(Sit,'Sit') ]).
```


```
% ===========================================
% ?- pred_cg(call_cg_real,"?x -(Attrib)-> [Color #1]").
% ===========================================

call_cg(
   named_graph(
      anonymous(frame1),
      [ cg_holds('Attrib',X,Color),
        cg_equal(Color,'Color#1'),
        cg_type(Color,'Color'),
        cg_quantz(e,X) ])).
```


```
% ===========================================
% ?- pred_cg(call_cg_real,"?x -(On)->[Mat #1]-(Attrib)->[Color #1]").
% ===========================================

call_cg(
   named_graph(
      anonymous(frame2),
      [ cg_holds('Attrib',Mat,Color),
        cg_equal(Color,'Color#1'),
        cg_type(Color,'Color'),
        cg_holds('On',X,Mat),
        cg_equal(Mat,'Mat#1'),
        cg_type(Mat,'Mat'),
        cg_quantz(e,X) ])).
```


```
% ===========================================
% ?- pred_cg(call_cg_real,"?x -(On)->[Mat #1]").
% ===========================================

call_cg(
   named_graph(
      anonymous(frame3),
      [ cg_holds('On',X,Mat),
        cg_equal(Mat,'Mat#1'),
        cg_type(Mat,'Mat'),
        cg_quantz(e,X) ])).
```


```
% ===========================================
% ?- pred_cg(call_cg_real,"[?x] -(Attrib)-> [Color #1]").
% ===========================================

call_cg(
   named_graph(
      anonymous(frame4),
      [ cg_holds('Attrib',X,Color),
        cg_equal(Color,'Color#1'),
        cg_type(Color,'Color'),
        cg_type(
           X,
           ?('X')) ])).
```


```
% ===========================================
% ?- pred_cg(call_cg_real,"[?x]-(On)->[Mat #1]-(Attrib)->[Color #1]").
% ===========================================

call_cg(
   named_graph(
      anonymous(frame5),
      [ cg_holds('Attrib',Mat,Color),
        cg_equal(Color,'Color#1'),
        cg_type(Color,'Color'),
        cg_holds('On',X,Mat),
        cg_equal(Mat,'Mat#1'),
        cg_type(Mat,'Mat'),
        cg_type(
           X,
           ?('X')) ])).
```


```
% ===========================================
% ?- pred_cg(call_cg_real,"[Mat ?x]-(Attrib)->[Color #1]").
% ===========================================

call_cg(
   named_graph(
      anonymous(frame6),
      [ cg_holds('Attrib',X,Color),
        cg_equal(Color,'Color#1'),
        cg_type(Color,'Color'),
        cg_quantz(e,X),
        cg_type(X,'Mat') ])).
```


```
% ===========================================
% ?- pred_cg(call_cg_real,"[Cat: ?x]-(On)->[Mat #1]-(Attrib)->[Color #2]").
% ===========================================

call_cg(
   named_graph(
      anonymous(frame7),
      [ cg_holds('Attrib',Mat,Color),
        cg_equal(Color,'Color#2'),
        cg_type(Color,'Color'),
        cg_holds('On',X,Mat),
        cg_equal(Mat,'Mat#1'),
        cg_type(Mat,'Mat'),
        cg_quantz(e,X),
        cg_type(X,'Cat') ])).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[a] - (belives) -> [statement: [Cat: @every]->(On)->[Mat] ]").
% ===========================================

named_graph(
   anonymous(70000040),
   [ cg_holds(belives,A,Statement),
     'GRAPH'(
        Statement,
        named_graph(
           anonymous(70000040),
           [ cg_holds('On',Every_Cat,Mat),
             cg_type(Mat,'Mat'),
             cg_quantz(every,Every_Cat),
             cg_type(Every_Cat,'Cat') ])),
     cg_type(Statement,statement),
     cg_type(A,a) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[a] - (belives) -> [statement2= [Cat: @every]->(On)->[Mat] ]").
% ===========================================

named_graph(
   anonymous(70000041),
   [ cg_holds(belives,A,Statement2),
     '='(
        Statement2,
        'GRAPH'(
           named_graph(
              anonymous(70000041),
              [ cg_holds('On',Every_Cat,Mat),
                cg_type(Mat,'Mat'),
                cg_quantz(every,Every_Cat),
                cg_type(Every_Cat,'Cat') ]))),
     cg_type(Statement2,statement2),
     cg_type(A,a) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"

[Go]- -
   (Agnt)->[Person: John] -
   (Dest)->[City: Boston] -
   (Inst)->[Bus]").
% ===========================================

named_graph(
   anonymous(70000042),
   [ cg_holds('Inst',Boston_City,Bus),
     cg_type(Bus,'Bus'),
     cg_holds('Dest',John_Person,Boston_City),
     cg_name(Boston_City,'Boston'),
     cg_type(Boston_City,'City'),
     cg_holds('Agnt',Go,John_Person),
     cg_name(John_Person,'John'),
     cg_type(John_Person,'Person'),
     cg_type(Go,'Go') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
   [Person: John2] <- (Agnt) -
   [City: Boston2] <- (Dest) -
   [Bus2] <- (Inst) - [Go2]").
% ===========================================

named_graph(
   anonymous(70000043),
   [ cg_holds('Inst',Go2,Bus2),
     cg_type(Go2,'Go2'),
     cg_holds('Dest',Bus2,Boston2_City),
     cg_type(Bus2,'Bus2'),
     cg_holds('Agnt',Boston2_City,John2_Person),
     cg_name(Boston2_City,'Boston2'),
     cg_type(Boston2_City,'City'),
     cg_name(John2_Person,'John2'),
     cg_type(John2_Person,'Person') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[Begin]-
        -obj->[Session],
        -srce->[Proposition =
                   [Press] -
                      -obj -> [Key : enter] -partOf-> [Keyboard],
                      -agnt -> [Person : John]
               ],
        -agnt->[Person : John]").
% ===========================================

named_graph(
   anonymous(70000044),
   [ cg_holds(agnt,Begin,John_Person9),
     cg_name(John_Person9,'John'),
     cg_type(John_Person9,'Person'),
     cg_holds(srce,Begin,Proposition),
     '='(
        Proposition,
        'GRAPH'(
           named_graph(
              anonymous(70000044),
              [ cg_holds(agnt,Press,John_Person),
                cg_name(John_Person,'John'),
                cg_type(John_Person,'Person'),
                cg_holds(partOf,Enter_Key,Keyboard),
                cg_type(Keyboard,'Keyboard'),
                cg_holds(obj,Press,Enter_Key),
                frame_var('ENTER',Enter_Key),
                cg_type(Enter_Key,'Key'),
                cg_type(Press,'Press') ]))),
     cg_type(Proposition,'Proposition'),
     cg_holds(obj,Begin,Session),
     cg_type(Session,'Session'),
     cg_type(Begin,'Begin') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
 [a] - (belives) ->
 [statement = [Go2]
   - (Agnt)->[Person: John2]
   - (Dest)->[City: Boston2]
   - (Inst)->[Bus2]  ]").
% ===========================================

named_graph(
   anonymous(70000045),
   [ cg_holds(belives,A,Statement),
     '='(
        Statement,
        'GRAPH'(
           named_graph(
              anonymous(70000045),
              [ cg_holds('Inst',Boston2_City,Bus2),
                cg_type(Bus2,'Bus2'),
                cg_holds('Dest',John2_Person,Boston2_City),
                cg_name(Boston2_City,'Boston2'),
                cg_type(Boston2_City,'City'),
                cg_holds('Agnt',Go2,John2_Person),
                cg_name(John2_Person,'John2'),
                cg_type(John2_Person,'Person'),
                cg_type(Go2,'Go2') ]))),
     cg_type(Statement,statement),
     cg_type(A,a) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Go*x][Person:'John'*y][City:'Boston'*z][Bus*w](Agnt?x?y)(Dest?x?z)(Inst?x?w)").
% ===========================================

named_graph(
   anonymous(70000046),
   [ cg_holds(
        'Inst',
        ?('X'),
        W_Bus),
     cg_holds(
        'Dest',
        ?('X'),
        ?('Z')),
     cg_holds(
        'Agnt',
        ?('X'),
        ?('Y')),
     frame_var('W',W_Bus),
     cg_type(W_Bus,'Bus'),
     frame_var('Z',Z_Boston_City),
     cg_name(Z_Boston_City,'Boston'),
     cg_type(Z_Boston_City,'City'),
     frame_var('Y',Y_John_Person),
     cg_name(Y_John_Person,'John'),
     cg_type(Y_John_Person,'Person'),
     frame_var('X',X_Go),
     cg_type(X_Go,'Go') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[Woman:red]<-knows-[Man:karim]<-agnt-[Eat]-obj->[Apple]-(on)->[table]").
% ===========================================

named_graph(
   anonymous(70000047),
   [ cg_holds(on,Apple,Table),
     cg_type(Table,table),
     cg_holds(obj,Eat,Apple),
     cg_type(Apple,'Apple'),
     cg_holds(agnt,Eat,Karim_Man),
     cg_type(Eat,'Eat'),
     cg_holds(knows,Karim_Man,Red_Woman),
     frame_var('KARIM',Karim_Man),
     cg_type(Karim_Man,'Man'),
     frame_var('RED',Red_Woman),
     cg_type(Red_Woman,'Woman') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[?x]<-(Agnt)-[Marry]-(Thme)->[Sailor]").
% ===========================================

named_graph(
   anonymous(70000048),
   [ cg_holds('Thme',Marry,Sailor),
     cg_type(Sailor,'Sailor'),
     cg_holds('Agnt',Marry,X),
     cg_type(Marry,'Marry'),
     cg_type(
        X,
        ?('X')) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]").
% ===========================================

named_graph(
   anonymous(70000049),
   [ cg_holds('Thme',Want,Situation),
     'GRAPH'(
        Situation,
        named_graph(
           anonymous(70000049),
           [ cg_holds('Thme',Marry,Sailor),
             cg_type(Sailor,'Sailor'),
             cg_holds('Agnt',Marry,X),
             cg_type(Marry,'Marry'),
             cg_type(X,X_Mary_Person) ])),
     cg_type(Situation,'Situation'),
     cg_holds('Expr',Want,X_Mary_Person),
     cg_type(Want,'Want'),
     frame_var('X',X_Mary_Person),
     cg_name(X_Mary_Person,'Mary'),
     cg_type(X_Mary_Person,'Person') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[Proposition: [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]").
% ===========================================

named_graph(
   anonymous(70000050),
   [ 'GRAPH'(
        Proposition,
        named_graph(
           anonymous(70000050),
           [ cg_holds('Thme',Want,Situation),
             'GRAPH'(
                Situation,
                named_graph(
                   anonymous(70000050),
                   [ cg_holds('Thme',Marry,Sailor),
                     cg_type(Sailor,'Sailor'),
                     cg_holds('Agnt',Marry,X),
                     cg_type(Marry,'Marry'),
                     cg_type(X,X_Mary_Person) ])),
             cg_type(Situation,'Situation'),
             cg_holds('Expr',Want,X_Mary_Person),
             cg_type(Want,'Want'),
             frame_var('X',X_Mary_Person),
             cg_name(X_Mary_Person,'Mary'),
             cg_type(X_Mary_Person,'Person') ])),
     cg_type(Proposition,'Proposition') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[Person: Tom]<-(Expr)-[Believe]-(Thme)->
     [Proposition:  [Person: Mary *x]<-(Expr)-[Want]-(Thme)->
     [Situation:  [?x]<-(Agnt)-[Marry]-(Thme)->[Sailor] ]]").
% ===========================================

named_graph(
   anonymous(70000051),
   [ cg_holds('Thme',Believe,Proposition),
     'GRAPH'(
        Proposition,
        named_graph(
           anonymous(70000051),
           [ cg_holds('Thme',Want,Situation),
             'GRAPH'(
                Situation,
                named_graph(
                   anonymous(70000051),
                   [ cg_holds('Thme',Marry,Sailor),
                     cg_type(Sailor,'Sailor'),
                     cg_holds('Agnt',Marry,X),
                     cg_type(Marry,'Marry'),
                     cg_type(X,X_Mary_Person3) ])),
             cg_type(Situation,'Situation'),
             cg_holds('Expr',Want,X_Mary_Person3),
             cg_type(Want,'Want'),
             frame_var('X',X_Mary_Person3),
             cg_name(X_Mary_Person3,'Mary'),
             cg_type(X_Mary_Person3,'Person') ])),
     cg_type(Proposition,'Proposition'),
     cg_holds('Expr',Believe,Tom_Person),
     cg_type(Believe,'Believe'),
     cg_name(Tom_Person,'Tom'),
     cg_type(Tom_Person,'Person') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[Person: Tom]<-(Expr)<-[Believe]->(Thme)-
     [Proposition:  [Person: Mary *x]<-(Expr)<-[Want]->(Thme)-
     [Situation:  [?x]<-(Agnt)<-[Marry]->(Thme)->[Sailor] ]]").
% ===========================================

named_graph(
   anonymous(70000052),
   [ cg_holds('Thme',Believe,Proposition),
     'GRAPH'(
        Proposition,
        named_graph(
           anonymous(70000052),
           [ cg_holds('Thme',Want,Situation),
             'GRAPH'(
                Situation,
                named_graph(
                   anonymous(70000052),
                   [ cg_holds('Thme',Marry,Sailor),
                     cg_type(Sailor,'Sailor'),
                     cg_holds('Agnt',Marry,X),
                     cg_type(Marry,'Marry'),
                     cg_type(X,X_Mary_Person3) ])),
             cg_type(Situation,'Situation'),
             cg_holds('Expr',Want,X_Mary_Person3),
             cg_type(Want,'Want'),
             frame_var('X',X_Mary_Person3),
             cg_name(X_Mary_Person3,'Mary'),
             cg_type(X_Mary_Person3,'Person') ])),
     cg_type(Proposition,'Proposition'),
     cg_holds('Expr',Believe,Tom_Person),
     cg_type(Believe,'Believe'),
     cg_name(Tom_Person,'Tom'),
     cg_type(Tom_Person,'Person') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE ARRIVAL-DATE(a) IS [UNIV:*a].").
% ===========================================

named_graph(
   anonymous(70000053),
   [ frame_var('A',A_Univ),
     cg_type(A_Univ,'Univ'),
     cg_holds(A_Univ) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE CIRCUS(c) IS [UNIV:*c].").
% ===========================================

named_graph(
   anonymous(70000054),
   [ frame_var('C',C_Univ),
     cg_type(C_Univ,'Univ'),
     cg_holds(C_Univ) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE CIRCUS(c) IS [UNIV:*c]").
% ===========================================

named_graph(
   anonymous(70000055),
   [ frame_var('C',C_Univ),
     cg_type(C_Univ,'Univ'),
     cg_holds(C_Univ) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"[ELEPHANT:*c]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS]").
% ===========================================

named_graph(
   anonymous(70000056),
   [ cg_holds('Loc',Perform,Circus),
     cg_type(Circus,'Circus'),
     cg_holds('Agnt',Perform,C_Elephant),
     cg_type(Perform,'Perform'),
     frame_var('C',C_Elephant),
     cg_type(C_Elephant,'Elephant') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[Go *x] (Agnt ?x [Person: John]) (Dest ?x [City: Boston]) (Inst ?x [Bus])
").
% ===========================================

named_graph(
   anonymous(70000057),
   [ cg_holds(
        'Inst',
        ?('X'),
        Bus),
     cg_type(Bus,'Bus'),
     cg_holds(
        'Dest',
        ?('X'),
        Boston_City),
     cg_name(Boston_City,'Boston'),
     cg_type(Boston_City,'City'),
     cg_holds(
        'Agnt',
        ?('X'),
        John_Person),
     cg_name(John_Person,'John'),
     cg_type(John_Person,'Person'),
     frame_var('X',X_Go),
     cg_type(X_Go,'Go') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].").
% ===========================================

named_graph(
   anonymous(70000058),
   [ cg_holds('Loc',Perform,Circus),
     cg_type(Circus,'Circus'),
     cg_holds('Agnt',Perform,C_Elephant),
     cg_type(Perform,'Perform'),
     frame_var('C',C_Elephant),
     cg_type(C_Elephant,'Elephant'),
     cg_holds(C_Elephant) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE CIRCUS-ELEPHANT(C) IS
 [ELEPHANT:*C]<-(AGNT)<-[PERFORM]->(LOC)->[CIRCUS].").
% ===========================================

named_graph(
   anonymous(70000059),
   [ cg_holds('Loc',Perform,Circus),
     cg_type(Circus,'Circus'),
     cg_holds('Agnt',Perform,C_Elephant),
     cg_type(Perform,'Perform'),
     frame_var('C',C_Elephant),
     cg_type(C_Elephant,'Elephant'),
     cg_holds(C_Elephant) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE DEPARTURE-DATE(d) IS [UNIV:*d].").
% ===========================================

named_graph(
   anonymous(70000060),
   [ frame_var('D',D_Univ),
     cg_type(D_Univ,'Univ'),
     cg_holds(D_Univ) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE ELEPHANT(e) IS [UNIV:*e].").
% ===========================================

named_graph(
   anonymous(70000061),
   [ frame_var('E',E_Univ),
     cg_type(E_Univ,'Univ'),
     cg_holds(E_Univ) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE HOTEL(h) IS [UNIV:*h].").
% ===========================================

named_graph(
   anonymous(70000062),
   [ frame_var('H',H_Univ),
     cg_type(H_Univ,'Univ'),
     cg_holds(H_Univ) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE HOTEL-RESERVATION(RESERVATION-NO) IS
[RESERVATION:*RESERVATION-NO]-
  ->(RCPT)->[PERSON]
  ->(OBJ)->[ROOM]->(LOC)->[HOTEL]
  ->(DUR)->[TIME-PERIOD]-
             ->(STRT)->[ARRIVAL-DATE]
             ->(UNTL)->[DEPARTURE-DATE],,.").
% ===========================================

named_graph(
   anonymous(70000063),
   [ cg_holds('Untl',Arrival_Date,Departure_Date),
     cg_type(Departure_Date,'Departure_Date'),
     cg_holds('Strt',Reservation,Arrival_Date),
     cg_type(Arrival_Date,'Arrival_Date'),
     cg_holds('Dur',Hotel,Time_Period),
     cg_type(Time_Period,'Time_Period'),
     cg_holds('Loc',Room,Hotel),
     cg_type(Hotel,'Hotel'),
     cg_holds('Obj',Person,Room),
     cg_type(Room,'Room'),
     cg_holds('Rcpt',Reservation,Person),
     cg_type(Person,'Person'),
     frame_var('RESERVATION_NO',Reservation),
     cg_type(Reservation,'Reservation'),
     cg_holds('Reservation_No') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE PERFORM(p) IS [UNIV:*p].").
% ===========================================

named_graph(
   anonymous(70000064),
   [ frame_var('P',P_Univ),
     cg_type(P_Univ,'Univ'),
     cg_holds(P_Univ) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE PERSON(p) IS [UNIV:*p].").
% ===========================================

named_graph(
   anonymous(70000065),
   [ frame_var('P',P_Univ),
     cg_type(P_Univ,'Univ'),
     cg_holds(P_Univ) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE PROPOSITION(p) IS [UNIV:*p].").
% ===========================================

named_graph(
   anonymous(70000066),
   [ frame_var('P',P_Univ),
     cg_type(P_Univ,'Univ'),
     cg_holds(P_Univ) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE RESERVATION(r) IS [UNIV:*r].").
% ===========================================

named_graph(
   anonymous(70000067),
   [ frame_var('R',R_Univ),
     cg_type(R_Univ,'Univ'),
     cg_holds(R_Univ) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE ROOM(r) IS [UNIV:*r].").
% ===========================================

named_graph(
   anonymous(70000068),
   [ frame_var('R',R_Univ),
     cg_type(R_Univ,'Univ'),
     cg_holds(R_Univ) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"TYPE TIME-PERIOD(t) IS [UNIV:*t].").
% ===========================================

named_graph(
   anonymous(70000069),
   [ frame_var('T',T_Univ),
     cg_type(T_Univ,'Univ'),
     cg_holds(T_Univ) ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[RESERVATION #316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]").
% ===========================================

named_graph(
   anonymous(70000070),
   [ cg_holds('Untl',March_14_1983_Arrival_Date,March_18_1983_Departure_Date),
     cg_name(March_18_1983_Departure_Date,'March_18_1983'),
     cg_type(March_18_1983_Departure_Date,'Departure_Date'),
     cg_holds('Strt',Reservation,March_14_1983_Arrival_Date),
     cg_name(March_14_1983_Arrival_Date,'March_14_1983'),
     cg_type(March_14_1983_Arrival_Date,'Arrival_Date'),
     cg_holds('Dur',Shelburne_Hotel,Nights_Time_Period_Num4_Num4),
     cg_name(Nights_Time_Period_Num4_Num4,'Nights'),
     cg_quantz(4,Nights_Time_Period_Num4_Num4),
     cg_type(Nights_Time_Period_Num4_Num4,'Time_Period'),
     cg_holds('Loc',Q2_Room,Shelburne_Hotel),
     cg_name(Shelburne_Hotel,'Shelburne'),
     cg_type(Shelburne_Hotel,'Hotel'),
     cg_holds('Obj',John_Sowa_Person,Q2_Room),
     cg_name(Q2_Room,'Q2'),
     cg_type(Q2_Room,'Room'),
     cg_holds('Rcpt',Reservation,John_Sowa_Person),
     cg_name(John_Sowa_Person,'John_Sowa'),
     cg_type(John_Sowa_Person,'Person'),
     cg_equal(Reservation,'Reservation#316209'),
     cg_type(Reservation,'Reservation') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983]").
% ===========================================

named_graph(
   anonymous(70000071),
   [ cg_holds('Untl',March_14_1983_Arrival_Date,March_18_1983_Departure_Date),
     cg_name(March_18_1983_Departure_Date,'March_18_1983'),
     cg_type(March_18_1983_Departure_Date,'Departure_Date'),
     cg_holds('Strt',Reservation,March_14_1983_Arrival_Date),
     cg_name(March_14_1983_Arrival_Date,'March_14_1983'),
     cg_type(March_14_1983_Arrival_Date,'Arrival_Date'),
     cg_holds('Dur',Shelburne_Hotel,Nights_Time_Period_Num4_Num4),
     cg_name(Nights_Time_Period_Num4_Num4,'Nights'),
     cg_quantz(4,Nights_Time_Period_Num4_Num4),
     cg_type(Nights_Time_Period_Num4_Num4,'Time_Period'),
     cg_holds('Loc',Q2_Room,Shelburne_Hotel),
     cg_name(Shelburne_Hotel,'Shelburne'),
     cg_type(Shelburne_Hotel,'Hotel'),
     cg_holds('Obj',John_Sowa_Person,Q2_Room),
     cg_name(Q2_Room,'Q2'),
     cg_type(Q2_Room,'Room'),
     cg_holds('Rcpt',Reservation,John_Sowa_Person),
     cg_name(John_Sowa_Person,'John_Sowa'),
     cg_type(John_Sowa_Person,'Person'),
     cg_equal(Reservation,'Reservation#316209'),
     cg_type(Reservation,'Reservation') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
INDIVIDUAL HOTEL-RESERVATION(#316209) IS
[RESERVATION:#316209]-
  ->(RCPT)->[PERSON:JOHN SOWA]
  ->(OBJ)->[ROOM:Q2]->(LOC)->[HOTEL:Shelburne]
  ->(DUR)->[TIME-PERIOD:@4 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 14 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 18 1983],,.").
% ===========================================

named_graph(
   anonymous(70000072),
   [ cg_holds('Untl',March_14_1983_Arrival_Date,March_18_1983_Departure_Date),
     cg_name(March_18_1983_Departure_Date,'March_18_1983'),
     cg_type(March_18_1983_Departure_Date,'Departure_Date'),
     cg_holds('Strt',Reservation,March_14_1983_Arrival_Date),
     cg_name(March_14_1983_Arrival_Date,'March_14_1983'),
     cg_type(March_14_1983_Arrival_Date,'Arrival_Date'),
     cg_holds('Dur',Shelburne_Hotel,Nights_Time_Period_Num4_Num4),
     cg_name(Nights_Time_Period_Num4_Num4,'Nights'),
     cg_quantz(4,Nights_Time_Period_Num4_Num4),
     cg_type(Nights_Time_Period_Num4_Num4,'Time_Period'),
     cg_holds('Loc',Q2_Room,Shelburne_Hotel),
     cg_name(Shelburne_Hotel,'Shelburne'),
     cg_type(Shelburne_Hotel,'Hotel'),
     cg_holds('Obj',John_Sowa_Person,Q2_Room),
     cg_name(Q2_Room,'Q2'),
     cg_type(Q2_Room,'Room'),
     cg_holds('Rcpt',Reservation,John_Sowa_Person),
     cg_name(John_Sowa_Person,'John_Sowa'),
     cg_type(John_Sowa_Person,'Person'),
     cg_equal(Reservation,'Reservation#316209'),
     cg_type(Reservation,'Reservation'),
     cg_holds(Cg_Holds),
     Cg_Holds#316209 ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
INDIVIDUAL HOTEL-RESERVATION(#316210) IS
[RESERVATION:#316210]-
  ->(RCPT)->[PERSON:JOHN ESCH]
  ->(OBJ)->[ROOM:Q3]->(LOC)->[HOTEL:Sidney]
  ->(DUR)->[TIME-PERIOD:@7 NIGHTS]-
             ->(STRT)->[ARRIVAL-DATE:MARCH 12 1983]
             ->(UNTL)->[DEPARTURE-DATE:MARCH 19 1983],,.").
% ===========================================

named_graph(
   anonymous(70000073),
   [ cg_holds('Untl',March_12_1983_Arrival_Date,March_19_1983_Departure_Date),
     cg_name(March_19_1983_Departure_Date,'March_19_1983'),
     cg_type(March_19_1983_Departure_Date,'Departure_Date'),
     cg_holds('Strt',Reservation,March_12_1983_Arrival_Date),
     cg_name(March_12_1983_Arrival_Date,'March_12_1983'),
     cg_type(March_12_1983_Arrival_Date,'Arrival_Date'),
     cg_holds('Dur',Sidney_Hotel,Nights_Time_Period_Num7_Num7),
     cg_name(Nights_Time_Period_Num7_Num7,'Nights'),
     cg_quantz(7,Nights_Time_Period_Num7_Num7),
     cg_type(Nights_Time_Period_Num7_Num7,'Time_Period'),
     cg_holds('Loc',Q3_Room,Sidney_Hotel),
     cg_name(Sidney_Hotel,'Sidney'),
     cg_type(Sidney_Hotel,'Hotel'),
     cg_holds('Obj',John_Esch_Person,Q3_Room),
     cg_name(Q3_Room,'Q3'),
     cg_type(Q3_Room,'Room'),
     cg_holds('Rcpt',Reservation,John_Esch_Person),
     cg_name(John_Esch_Person,'John_Esch'),
     cg_type(John_Esch_Person,'Person'),
     cg_equal(Reservation,'Reservation#316210'),
     cg_type(Reservation,'Reservation'),
     cg_holds(Cg_Holds),
     Cg_Holds#316210 ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
INDIVIDUAL CIRCUS-ELEPHANT(#BUMBO) IS
[ELEPHANT:#BUMBO]<-(AGNT)<-[PERFORM: {*}]->(LOC)->[CIRCUS:Flying Tigers].").
% ===========================================

named_graph(
   anonymous(70000074),
   [ cg_holds('Loc',Set_Perform,Flying_Tigers_Circus),
     cg_name(Flying_Tigers_Circus,'Flying_Tigers'),
     cg_type(Flying_Tigers_Circus,'Circus'),
     cg_holds('Agnt',Set_Perform,Elephant),
     cg_count(Set_Perform,1,Cg_Count),
     cg_quantz(set,Set_Perform),
     cg_type(Set_Perform,'Perform'),
     cg_equal(Elephant,'Elephant#Bumbo'),
     cg_type(Elephant,'Elephant'),
     cg_holds(Bumbo),
     Bumbo#'Bumbo' ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
INDIVIDUAL CIRCUS-ELEPHANT(#JUMBO) IS
[ELEPHANT:#JUMBO]<-(AGNT)<-[PERFORM: {*}]->(LOC)->[CIRCUS:Barnum & Bailey].").
% ===========================================

named_graph(
   anonymous(70000075),
   [ cg_holds('Loc',Set_Perform,Barnum_C38_Bailey_Circus),
     cg_name(Barnum_C38_Bailey_Circus,'Barnum_&_Bailey'),
     cg_type(Barnum_C38_Bailey_Circus,'Circus'),
     cg_holds('Agnt',Set_Perform,Elephant),
     cg_count(Set_Perform,1,Cg_Count),
     cg_quantz(set,Set_Perform),
     cg_type(Set_Perform,'Perform'),
     cg_equal(Elephant,'Elephant#Jumbo'),
     cg_type(Elephant,'Elephant'),
     cg_holds(Jumbo),
     Jumbo#'Jumbo' ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
Leopard::[Animal : x]-isA->[Leopard] :-
   Mammal::[Animal : x]-isA->[Mammal],
   Carnivorous::[Animal : x]-isA->[Carnivorous],
   Fact::[Animal : x]-colorOf->[Color]-attr->[Wild],
   Fact::[Animal : x]-partOf->[Component]-attr->[Dark]. ").
% ===========================================

named_graph(
   'Leopard',
   [ preconds(
        [ named_graph(
             'Leopard',
             [ named_graph(
                  'Mammal',
                  [ named_graph(
                       'Carnivorous',
                       [ named_graph(
                            'Fact',
                            [ cg_type(X_Animal13,'Animal'),
                              frame_var('X',X_Animal13),
                              cg_type(Component,'Component'),
                              cg_holds(partOf,X_Animal13,Component),
                              cg_type(Dark,'Dark'),
                              cg_holds(attr,Component,Dark),
                              cg_holds(attr,Color,Wild),
                              cg_type(Wild,'Wild'),
                              cg_holds(colorOf,X_Animal9,Color),
                              cg_type(Color,'Color'),
                              frame_var('X',X_Animal9),
                              cg_type(X_Animal9,'Animal') ]),
                         cg_holds(isA,X_Animal6,Carnivorous),
                         cg_type(Carnivorous,'Carnivorous'),
                         frame_var('X',X_Animal6),
                         cg_type(X_Animal6,'Animal') ]),
                    cg_holds(isA,X_Animal3,Mammal),
                    cg_type(Mammal,'Mammal'),
                    frame_var('X',X_Animal3),
                    cg_type(X_Animal3,'Animal') ]) ]) ]),
     cg_holds(isA,X_Animal13,Leopard),
     cg_type(Leopard,'Leopard'),
     frame_var('X',X_Animal13),
     cg_type(X_Animal13,'Animal') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
Mammal::[Animal : x]-isA->[Mammal] :-
        Fact::[Animal : x]-poss->[Hair].
").
% ===========================================

named_graph(
   'Mammal',
   [ preconds(
        [ named_graph(
             'Mammal',
             [ named_graph(
                  'Fact',
                  [ cg_holds(poss,X_Animal3,Hair),
                    cg_type(Hair,'Hair'),
                    frame_var('X',X_Animal3),
                    cg_type(X_Animal3,'Animal') ]) ]) ]),
     cg_holds(isA,X_Animal3,Mammal),
     cg_type(Mammal,'Mammal'),
     frame_var('X',X_Animal3),
     cg_type(X_Animal3,'Animal') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
Carnivorous::[Animal : x]-isA->[Carnivorous] :-
   Fact::[Animal : x]<-agnt-[Eat]-obj->[Meat].
").
% ===========================================

named_graph(
   'Carnivorous',
   [ preconds(
        [ named_graph(
             'Carnivorous',
             [ named_graph(
                  'Fact',
                  [ cg_holds(obj,Eat,Meat),
                    cg_type(Meat,'Meat'),
                    cg_holds(agnt,Eat,X_Animal3),
                    cg_type(Eat,'Eat'),
                    frame_var('X',X_Animal3),
                    cg_type(X_Animal3,'Animal') ]) ]) ]),
     cg_holds(isA,X_Animal3,Carnivorous),
     cg_type(Carnivorous,'Carnivorous'),
     frame_var('X',X_Animal3),
     cg_type(X_Animal3,'Animal') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
Carnivorous::[Animal : x]-isA->[Carnivorous] :-
   Fact::[Animal : x]-poss->[Teeth]-attr->[Sharp],
   Fact::[Animal : x]-poss->[Claw],
   Fact::[Animal : x]-has->[Eyes]-attr->[Forward].
").
% ===========================================

named_graph(
   'Carnivorous',
   [ preconds(
        [ named_graph(
             'Carnivorous',
             [ named_graph(
                  'Fact',
                  [ cg_type(X_Animal7,'Animal'),
                    frame_var('X',X_Animal7),
                    cg_type(Claw,'Claw'),
                    cg_holds(poss,X_Animal7,Claw),
                    cg_holds(attr,Eyes,Forward),
                    cg_type(Forward,'Forward'),
                    cg_holds(has,X_Animal10,Eyes),
                    cg_type(Eyes,'Eyes'),
                    frame_var('X',X_Animal10),
                    cg_type(X_Animal10,'Animal'),
                    cg_holds(attr,Teeth,Sharp),
                    cg_type(Sharp,'Sharp'),
                    cg_holds(poss,X_Animal3,Teeth),
                    cg_type(Teeth,'Teeth'),
                    frame_var('X',X_Animal3),
                    cg_type(X_Animal3,'Animal') ]) ]) ]),
     cg_holds(isA,X_Animal7,Carnivorous),
     cg_type(Carnivorous,'Carnivorous'),
     frame_var('X',X_Animal7),
     cg_type(X_Animal7,'Animal') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"
Fact::[Animal : Yala]-
            <-pat-[BelongTo]-bnfcre->[Man : Robert],
            -colorOf->[Color]-attr->[Wild],
            -poss->[Teeth]-attr->[Sharp],
            -has->[Eyes]-attr->[Forward].
").
% ===========================================

named_graph(
   'Fact',
   [ cg_holds(attr,Eyes,Forward),
     cg_type(Forward,'Forward'),
     cg_holds(has,Yala_Animal,Eyes),
     cg_type(Eyes,'Eyes'),
     cg_holds(attr,Teeth,Sharp),
     cg_type(Sharp,'Sharp'),
     cg_holds(poss,Yala_Animal,Teeth),
     cg_type(Teeth,'Teeth'),
     cg_holds(attr,Color,Wild),
     cg_type(Wild,'Wild'),
     cg_holds(colorOf,Yala_Animal,Color),
     cg_type(Color,'Color'),
     cg_holds(bnfcre,Belongto,Robert_Man),
     cg_name(Robert_Man,'Robert'),
     cg_type(Robert_Man,'Man'),
     cg_holds(pat,Belongto,Yala_Animal),
     cg_type(Belongto,'BelongTo'),
     cg_name(Yala_Animal,'Yala'),
     cg_type(Yala_Animal,'Animal') ]).
```


```
% ===========================================
% ?- pred_cg(assert_cg_real,"Fact::[Animal : Yala]-poss->[Claw].").
% ===========================================

named_graph(
   'Fact',
   [ cg_holds(poss,Yala_Animal,Claw),
     cg_type(Claw,'Claw'),
     cg_name(Yala_Animal,'Yala'),
     cg_type(Yala_Animal,'Animal') ]).
```


```
Welcome to SWI-Prolog (threaded, 64 bits, version 8.1.28)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

cgpro:  ?-
```

