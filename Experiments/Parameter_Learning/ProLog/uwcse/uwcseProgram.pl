t(_)::advisedby(Fold,A,B).
t(_)::advisedby(Fold,A,B) :- professor(Fold,B),hidden_1(Fold,A,B).
t(_)::hidden_1(Fold,A,B) :- student(Fold,A),hidden_1_1(Fold,A,B).
t(_)::hidden_1(Fold,A,B) :- student(Fold,A),hidden_1_2(Fold,A,B).
t(_)::hidden_1_1(Fold,A,B) :- hasposition(Fold,B,faculty).
t(_)::hidden_1_1(Fold,A,B) :- inphase(Fold,A,post_quals).
t(_)::hidden_1_1(Fold,A,B) :- taughtby(Fold,C,B,D),hidden_1_1_1(Fold,A,B,C,D).
t(_)::hidden_1_1(Fold,A,B) :- publication(Fold,C,A),hidden_1_1_2(Fold,A,B,C).
t(_)::hidden_1_2(Fold,A,B) :- publication(Fold,C,A),hidden_1_2_1(Fold,A,B,C).
t(_)::hidden_1_1_1(Fold,A,B,C,D) :- ta(Fold,C,A,D).
t(_)::hidden_1_1_2(Fold,A,B,C) :- publication(Fold,C,B).
t(_)::hidden_1_2_1(Fold,A,B,C) :- publication(Fold,C,D),hidden_1_2_1_1(Fold,A,B,C,D).
t(_)::hidden_1_2_1_1(Fold,A,B,C,D) :- D\==A,hidden_1_2_1_1_1(Fold,A,B,C,D).
t(_)::hidden_1_2_1_1_1(Fold,A,B,C,D) :- D\==B,hidden_1_2_1_1_1_1(Fold,A,B,C,D).
t(_)::hidden_1_2_1_1_1_1(Fold,A,B,C,D) :- inphase(Fold,A,P),hidden_1_2_1_1_1_1_1(Fold,A,B,C,D,P).
t(_)::hidden_1_2_1_1_1_1(Fold,A,B,C,D) :- yearsinprogram(Fold,A,Y),hidden_1_2_1_1_1_1_2(Fold,A,B,C,D,Y).
t(_)::hidden_1_2_1_1_1_1_1(Fold,A,B,C,D,P) :- inphase(Fold,D,P).
t(_)::hidden_1_2_1_1_1_1_2(Fold,A,B,C,D,Y) :- yearsinprogram(Fold,D,Y).
taughtby(ai,course44, person171, autumn_0001).
taughtby(ai,course24, person240, autumn_0001).
taughtby(ai,course12, person211, autumn_0001).
taughtby(ai,course123, person150, autumn_0001).
taughtby(ai,course44, person293, winter_0001).
taughtby(ai,course143, person211, winter_0001).
taughtby(ai,course50, person171, winter_0001).
taughtby(ai,course170, person79, winter_0001).
taughtby(ai,course15, person292, winter_0001).
taughtby(ai,course32, person319, winter_0001).
taughtby(ai,course158, person240, winter_0001).
taughtby(ai,course24, person150, spring_0001).
taughtby(ai,course52, person168, spring_0001).
taughtby(ai,course16, person240, spring_0001).
taughtby(ai,course173, person171, spring_0001).
taughtby(ai,course64, person79, spring_0001).
taughtby(ai,course44, person171, autumn_0102).
taughtby(ai,course24, person211, autumn_0102).
taughtby(ai,course156, person240, autumn_0102).
taughtby(ai,course12, person79, autumn_0102).
taughtby(ai,course143, person407, winter_0102).
taughtby(ai,course170, person211, winter_0102).
taughtby(ai,course44, person415, spring_0102).
taughtby(ai,course24, person240, spring_0102).
taughtby(ai,course52, person168, spring_0102).
taughtby(ai,course50, person171, spring_0102).
taughtby(ai,course39, person415, spring_0102).
taughtby(ai,course123, person150, spring_0102).
taughtby(ai,course76, person319, spring_0102).
taughtby(ai,course44, person171, autumn_0203).
taughtby(ai,course24, person240, autumn_0203).
taughtby(ai,course44, person415, winter_0203).
taughtby(ai,course52, person168, winter_0203).
taughtby(ai,course141, person150, winter_0203).
taughtby(ai,course12, person211, winter_0203).
taughtby(ai,course16, person79, winter_0203).
taughtby(ai,course24, person211, spring_0203).
taughtby(ai,course170, person407, spring_0203).
taughtby(ai,course15, person292, spring_0203).
taughtby(ai,course168, person240, spring_0203).
taughtby(ai,course64, person79, spring_0203).
taughtby(ai,course44, person171, autumn_0304).
taughtby(ai,course24, person79, autumn_0304).
taughtby(ai,course156, person240, autumn_0304).
taughtby(ai,course12, person407, autumn_0304).
taughtby(ai,course76, person319, autumn_0304).
taughtby(ai,course44, person415, winter_0304).
taughtby(ai,course57, person150, winter_0304).
taughtby(ai,course52, person168, winter_0304).
taughtby(ai,course170, person79, winter_0304).
taughtby(ai,course24, person407, spring_0304).
taughtby(ai,course50, person171, spring_0304).
taughtby(ai,course158, person240, spring_0304).
taughtby(ai,course7, person415, spring_0304).
courselevel(ai,course52, level_400).
courselevel(ai,course44, level_400).
courselevel(ai,course24, level_400).
courselevel(ai,course128, level_400).
courselevel(ai,course57, level_400).
courselevel(ai,course82, level_400).
courselevel(ai,course143, level_400).
courselevel(ai,course50, level_500).
courselevel(ai,course156, level_500).
courselevel(ai,course141, level_500).
courselevel(ai,course12, level_500).
courselevel(ai,course170, level_500).
courselevel(ai,course65, level_500).
courselevel(ai,course123, level_500).
courselevel(ai,course173, level_500).
courselevel(ai,course86, level_500).
courselevel(ai,course131, level_500).
courselevel(ai,course85, level_500).
courselevel(ai,course64, level_500).
courselevel(ai,course168, level_500).
courselevel(ai,course158, level_500).
courselevel(ai,course132, level_500).
courselevel(ai,course76, level_500).
courselevel(ai,course16, level_500).
courselevel(ai,course15, level_500).
courselevel(ai,course39, level_500).
courselevel(ai,course32, level_500).
courselevel(ai,course7, level_500).
courselevel(ai,course134, level_500).
courselevel(ai,course135, level_500).
hasposition(ai,person292, faculty_affiliate).
hasposition(ai,person293, faculty_affiliate).
hasposition(ai,person240, faculty).
hasposition(ai,person211, faculty).
hasposition(ai,person150, faculty).
hasposition(ai,person415, faculty).
hasposition(ai,person79, faculty).
hasposition(ai,person349, faculty_adjunct).
hasposition(ai,person7, faculty_adjunct).
hasposition(ai,person319, faculty).
hasposition(ai,person185, faculty_adjunct).
hasposition(ai,person171, faculty).
hasposition(ai,person168, faculty).
hasposition(ai,person407, faculty).
projectmember(ai,project62, person319).
inphase(ai,person408, pre_quals).
inphase(ai,person265, post_generals).
inphase(ai,person70, pre_quals).
inphase(ai,person381, post_generals).
inphase(ai,person139, post_quals).
inphase(ai,person382, post_quals).
inphase(ai,person333, pre_quals).
inphase(ai,person94, pre_quals).
inphase(ai,person176, post_quals).
inphase(ai,person272, post_quals).
inphase(ai,person37, pre_quals).
inphase(ai,person353, post_quals).
inphase(ai,person432, post_quals).
inphase(ai,person377, pre_quals).
inphase(ai,person239, post_quals).
inphase(ai,person13, post_generals).
inphase(ai,person286, post_quals).
inphase(ai,person412, post_quals).
inphase(ai,person418, post_quals).
inphase(ai,person14, post_generals).
inphase(ai,person320, post_quals).
inphase(ai,person42, pre_quals).
inphase(ai,person20, pre_quals).
inphase(ai,person352, post_generals).
inphase(ai,person276, pre_quals).
inphase(ai,person45, post_generals).
inphase(ai,person233, pre_quals).
inphase(ai,person148, post_quals).
inphase(ai,person193, pre_quals).
inphase(ai,person314, post_generals).
inphase(ai,person275, post_generals).
inphase(ai,person21, post_generals).
inphase(ai,person262, post_generals).
inphase(ai,person257, post_generals).
inphase(ai,person73, post_quals).
inphase(ai,person380, post_generals).
inphase(ai,person384, post_quals).
inphase(ai,person406, post_generals).
inphase(ai,person266, post_quals).
inphase(ai,person312, pre_quals).
inphase(ai,person208, post_quals).
inphase(ai,person311, post_quals).
inphase(ai,person63, post_generals).
inphase(ai,person318, pre_quals).
inphase(ai,person83, post_quals).
inphase(ai,person161, post_generals).
inphase(ai,person284, post_quals).
tempadvisedby(ai,person408, person150).
tempadvisedby(ai,person382, person415).
tempadvisedby(ai,person333, person211).
tempadvisedby(ai,person94, person79).
tempadvisedby(ai,person377, person292).
tempadvisedby(ai,person412, person168).
tempadvisedby(ai,person42, person150).
tempadvisedby(ai,person20, person240).
tempadvisedby(ai,person233, person319).
tempadvisedby(ai,person193, person415).
tempadvisedby(ai,person284, person211).
yearsinprogram(ai,person408, year_2).
yearsinprogram(ai,person265, year_9).
yearsinprogram(ai,person70, year_1).
yearsinprogram(ai,person381, year_10).
yearsinprogram(ai,person139, year_3).
yearsinprogram(ai,person382, year_3).
yearsinprogram(ai,person333, year_2).
yearsinprogram(ai,person94, year_1).
yearsinprogram(ai,person176, year_2).
yearsinprogram(ai,person272, year_2).
yearsinprogram(ai,person37, year_1).
yearsinprogram(ai,person353, year_4).
yearsinprogram(ai,person432, year_5).
yearsinprogram(ai,person377, year_1).
yearsinprogram(ai,person239, year_4).
yearsinprogram(ai,person13, year_7).
yearsinprogram(ai,person286, year_3).
yearsinprogram(ai,person412, year_3).
yearsinprogram(ai,person418, year_3).
yearsinprogram(ai,person14, year_10).
yearsinprogram(ai,person320, year_3).
yearsinprogram(ai,person42, year_1).
yearsinprogram(ai,person20, year_1).
yearsinprogram(ai,person352, year_5).
yearsinprogram(ai,person276, year_3).
yearsinprogram(ai,person45, year_5).
yearsinprogram(ai,person233, year_1).
yearsinprogram(ai,person148, year_5).
yearsinprogram(ai,person193, year_1).
yearsinprogram(ai,person314, year_4).
yearsinprogram(ai,person275, year_5).
yearsinprogram(ai,person21, year_5).
yearsinprogram(ai,person262, year_7).
yearsinprogram(ai,person257, year_7).
yearsinprogram(ai,person73, year_4).
yearsinprogram(ai,person380, year_6).
yearsinprogram(ai,person384, year_3).
yearsinprogram(ai,person406, year_5).
yearsinprogram(ai,person266, year_5).
yearsinprogram(ai,person312, year_4).
yearsinprogram(ai,person208, year_4).
yearsinprogram(ai,person311, year_3).
yearsinprogram(ai,person63, year_5).
yearsinprogram(ai,person318, year_5).
yearsinprogram(ai,person83, year_5).
yearsinprogram(ai,person161, year_7).
yearsinprogram(ai,person284, year_3).
ta(ai,course52, person70, winter_0304).
ta(ai,course44, person193, winter_0304).
ta(ai,course128, person271, winter_0304).
ta(ai,course128, person392, winter_0304).
ta(ai,course44, person377, autumn_0304).
ta(ai,course24, person70, autumn_0304).
ta(ai,course156, person257, autumn_0304).
ta(ai,course132, person94, autumn_0304).
ta(ai,course24, person21, spring_0203).
ta(ai,course44, person420, winter_0203).
ta(ai,course44, person382, winter_0203).
ta(ai,course141, person14, winter_0203).
ta(ai,course12, person21, winter_0203).
ta(ai,course44, person286, autumn_0203).
ta(ai,course52, person318, spring_0102).
ta(ai,course44, person382, spring_0102).
ta(ai,course44, person86, spring_0102).
ta(ai,course50, person314, spring_0102).
ta(ai,course39, person73, spring_0102).
ta(ai,course82, person381, winter_0102).
taughtby(ai,course128 , person150, winter_0304).
taughtby(ai,course132 , person319, autumn_0304).
taughtby(ai,course134, person240, spring_0203).
taughtby(ai,course82 , person407, winter_0102).
professor(ai,person319).
student(ai,person284).
student(ai,person311).
student(ai,person14).
student(ai,person275).
student(ai,person259).
student(ai,person139).
student(ai,person176).
student(ai,person400).
student(ai,person318).
student(ai,person161).
student(ai,person347).
professor(ai,person292).
professor(ai,person293).
professor(ai,person240).
professor(ai,person211).
professor(ai,person150).
professor(ai,person415).
professor(ai,person79).
professor(ai,person349).
professor(ai,person7).
professor(ai,person185).
professor(ai,person171).
professor(ai,person168).
professor(ai,person407).
student(ai,person408).
student(ai,person265).
student(ai,person70).
student(ai,person381).
student(ai,person382).
student(ai,person333).
student(ai,person94).
student(ai,person272).
student(ai,person37).
student(ai,person353).
student(ai,person432).
student(ai,person377).
student(ai,person239).
student(ai,person13).
student(ai,person286).
student(ai,person412).
student(ai,person418).
student(ai,person320).
student(ai,person42).
student(ai,person20).
student(ai,person352).
student(ai,person276).
student(ai,person45).
student(ai,person233).
student(ai,person148).
student(ai,person193).
student(ai,person314).
student(ai,person21).
student(ai,person262).
student(ai,person257).
student(ai,person73).
student(ai,person380).
student(ai,person384).
student(ai,person406).
student(ai,person266).
student(ai,person312).
student(ai,person208).
student(ai,person63).
student(ai,person83).
student(ai,person271).
student(ai,person392).
student(ai,person420).
student(ai,person86).
sameperson(ai,person319, person319).
sameperson(ai,person284, person284).
sameperson(ai,person311, person311).
sameperson(ai,person14, person14).
sameperson(ai,person275, person275).
sameperson(ai,person259, person259).
sameperson(ai,person139, person139).
sameperson(ai,person176, person176).
sameperson(ai,person400, person400).
sameperson(ai,person318, person318).
sameperson(ai,person161, person161).
sameperson(ai,person347, person347).
sameperson(ai,person292, person292).
sameperson(ai,person293, person293).
sameperson(ai,person240, person240).
sameperson(ai,person211, person211).
sameperson(ai,person150, person150).
sameperson(ai,person415, person415).
sameperson(ai,person79, person79).
sameperson(ai,person349, person349).
sameperson(ai,person7, person7).
sameperson(ai,person185, person185).
sameperson(ai,person171, person171).
sameperson(ai,person168, person168).
sameperson(ai,person407, person407).
sameperson(ai,person408, person408).
sameperson(ai,person265, person265).
sameperson(ai,person70, person70).
sameperson(ai,person381, person381).
sameperson(ai,person382, person382).
sameperson(ai,person333, person333).
sameperson(ai,person94, person94).
sameperson(ai,person272, person272).
sameperson(ai,person37, person37).
sameperson(ai,person353, person353).
sameperson(ai,person432, person432).
sameperson(ai,person377, person377).
sameperson(ai,person239, person239).
sameperson(ai,person13, person13).
sameperson(ai,person286, person286).
sameperson(ai,person412, person412).
sameperson(ai,person418, person418).
sameperson(ai,person320, person320).
sameperson(ai,person42, person42).
sameperson(ai,person20, person20).
sameperson(ai,person352, person352).
sameperson(ai,person276, person276).
sameperson(ai,person45, person45).
sameperson(ai,person233, person233).
sameperson(ai,person148, person148).
sameperson(ai,person193, person193).
sameperson(ai,person314, person314).
sameperson(ai,person21, person21).
sameperson(ai,person262, person262).
sameperson(ai,person257, person257).
sameperson(ai,person73, person73).
sameperson(ai,person380, person380).
sameperson(ai,person384, person384).
sameperson(ai,person406, person406).
sameperson(ai,person266, person266).
sameperson(ai,person312, person312).
sameperson(ai,person208, person208).
sameperson(ai,person63, person63).
sameperson(ai,person83, person83).
sameperson(ai,person271, person271).
sameperson(ai,person392, person392).
sameperson(ai,person420, person420).
sameperson(ai,person86, person86).
samecourse(ai,course52, course52).
samecourse(ai,course44, course44).
samecourse(ai,course24, course24).
samecourse(ai,course57, course57).
samecourse(ai,course143, course143).
samecourse(ai,course50, course50).
samecourse(ai,course156, course156).
samecourse(ai,course141, course141).
samecourse(ai,course12, course12).
samecourse(ai,course170, course170).
samecourse(ai,course123, course123).
samecourse(ai,course173, course173).
samecourse(ai,course85, course85).
samecourse(ai,course64, course64).
samecourse(ai,course168, course168).
samecourse(ai,course158, course158).
samecourse(ai,course76, course76).
samecourse(ai,course16, course16).
samecourse(ai,course15, course15).
samecourse(ai,course39, course39).
samecourse(ai,course32, course32).
samecourse(ai,course7, course7).
samecourse(ai,course134, course134).
samecourse(ai,course135, course135).
samecourse(ai,course65, course65).
samecourse(ai,course86, course86).
samecourse(ai,course131, course131).
samecourse(ai,course128, course128).
samecourse(ai,course82, course82).
samecourse(ai,course132, course132).
sameproject(ai,project50, project50).
sameproject(ai,project58, project58).
sameproject(ai,project104, project104).
sameproject(ai,project20, project20).
sameproject(ai,project74, project74).
sameproject(ai,project62, project62).
sameproject(ai,project17, project17).
sameproject(ai,project82, project82).
sameproject(ai,project24, project24).
sameproject(ai,project127, project127).
sameproject(ai,project131, project131).
sameproject(ai,project38, project38).
sameproject(ai,project41, project41).
sameproject(ai,project100, project100).
sameproject(ai,project76, project76).
sameproject(ai,project111, project111).
sameproject(ai,project124, project124).
sameproject(ai,project77, project77).
sameproject(ai,project11, project11).
sameproject(ai,project52, project52).
sameproject(ai,project51, project51).
sameproject(ai,project115, project115).
sameproject(ai,project141, project141).
sameproject(ai,project70, project70).
sameproject(ai,project146, project146).
sameproject(ai,project150, project150).
sameproject(ai,project73, project73).
sameproject(ai,project42, project42).
sameproject(ai,project36, project36).
sameproject(ai,project13, project13).
sameproject(ai,project85, project85).
sameproject(ai,project7, project7).
sameproject(ai,project121, project121).
sameproject(ai,project0, project0).
sameproject(ai,project33, project33).
sameproject(ai,project29, project29).
sameproject(ai,project84, project84).
sameproject(ai,project90, project90).
sameproject(ai,project83, project83).
sameproject(ai,project97, project97).
sameproject(ai,project113, project113).
sameproject(ai,project116, project116).
sameproject(ai,project143, project143).
sameproject(ai,project66, project66).
sameproject(ai,project101, project101).
publication(ai,title25 , person284).
publication(ai,title284 , person14).
publication(ai,title110 , person14).
publication(ai,title118 , person14).
publication(ai,title71 , person14).
publication(ai,title316 , person14).
publication(ai,title118 , person318).
publication(ai,title217 , person161).
publication(ai,title55 , person161).
publication(ai,title331 , person161).
publication(ai,title250 , person161).
publication(ai,title268 , person161).
publication(ai,title271 , person161).
publication(ai,title171 , person161).
publication(ai,title120 , person347).
publication(ai,title86 , person347).
publication(ai,title338 , person347).
publication(ai,title224 , person347).
publication(ai,title260 , person347).
publication(ai,title112 , person347).
publication(ai,title97 , person347).
publication(ai,title50 , person292).
publication(ai,title103 , person292).
publication(ai,title166 , person292).
publication(ai,title72 , person292).
publication(ai,title47 , person292).
publication(ai,title41 , person292).
publication(ai,title40 , person293).
publication(ai,title13 , person240).
publication(ai,title140 , person240).
publication(ai,title217 , person240).
publication(ai,title92 , person240).
publication(ai,title167 , person240).
publication(ai,title331 , person240).
publication(ai,title26 , person240).
publication(ai,title275 , person240).
publication(ai,title333 , person240).
publication(ai,title270 , person240).
publication(ai,title208 , person240).
publication(ai,title103 , person240).
publication(ai,title268 , person240).
publication(ai,title340 , person240).
publication(ai,title192 , person240).
publication(ai,title54 , person240).
publication(ai,title177 , person240).
publication(ai,title33 , person240).
publication(ai,title10 , person240).
publication(ai,title84 , person240).
publication(ai,title161 , person240).
publication(ai,title248 , person240).
publication(ai,title102 , person240).
publication(ai,title274 , person240).
publication(ai,title47 , person240).
publication(ai,title0 , person240).
publication(ai,title82 , person240).
publication(ai,title337 , person240).
publication(ai,title344 , person240).
publication(ai,title254 , person240).
publication(ai,title119 , person240).
publication(ai,title114 , person211).
publication(ai,title259 , person211).
publication(ai,title59 , person211).
publication(ai,title160 , person211).
publication(ai,title88 , person211).
publication(ai,title24 , person211).
publication(ai,title323 , person211).
publication(ai,title190 , person211).
publication(ai,title11 , person211).
publication(ai,title199 , person211).
publication(ai,title240 , person211).
publication(ai,title335 , person211).
publication(ai,title241 , person211).
publication(ai,title212 , person211).
publication(ai,title228 , person211).
publication(ai,title345 , person211).
publication(ai,title89 , person211).
publication(ai,title165 , person211).
publication(ai,title113 , person211).
publication(ai,title233 , person211).
publication(ai,title132 , person211).
publication(ai,title310 , person211).
publication(ai,title218 , person211).
publication(ai,title71 , person211).
publication(ai,title341 , person211).
publication(ai,title207 , person211).
publication(ai,title229 , person211).
publication(ai,title292 , person211).
publication(ai,title49 , person211).
publication(ai,title238 , person211).
publication(ai,title255 , person211).
publication(ai,title329 , person211).
publication(ai,title79 , person211).
publication(ai,title325 , person211).
publication(ai,title44 , person211).
publication(ai,title25 , person211).
publication(ai,title118 , person150).
publication(ai,title140 , person415).
publication(ai,title12 , person415).
publication(ai,title182 , person415).
publication(ai,title122 , person415).
publication(ai,title208 , person415).
publication(ai,title103 , person415).
publication(ai,title347 , person415).
publication(ai,title266 , person415).
publication(ai,title340 , person415).
publication(ai,title269 , person415).
publication(ai,title5 , person415).
publication(ai,title70 , person415).
publication(ai,title179 , person415).
publication(ai,title29 , person415).
publication(ai,title72 , person415).
publication(ai,title47 , person415).
publication(ai,title0 , person415).
publication(ai,title38 , person415).
publication(ai,title290 , person415).
publication(ai,title63 , person415).
publication(ai,title82 , person415).
publication(ai,title283 , person415).
publication(ai,title337 , person415).
publication(ai,title94 , person415).
publication(ai,title147 , person415).
publication(ai,title329 , person415).
publication(ai,title297 , person415).
publication(ai,title79 , person415).
publication(ai,title312 , person415).
publication(ai,title107 , person415).
publication(ai,title273 , person415).
publication(ai,title172 , person415).
publication(ai,title295 , person415).
publication(ai,title41 , person415).
publication(ai,title325 , person415).
publication(ai,title44 , person415).
publication(ai,title87 , person415).
publication(ai,title222 , person415).
publication(ai,title236 , person415).
publication(ai,title258 , person415).
publication(ai,title301 , person415).
publication(ai,title318 , person79).
publication(ai,title115 , person79).
publication(ai,title231 , person79).
publication(ai,title226 , person79).
publication(ai,title195 , person79).
publication(ai,title162 , person185).
publication(ai,title178 , person171).
publication(ai,title225 , person171).
publication(ai,title269 , person171).
publication(ai,title150 , person171).
publication(ai,title70 , person171).
publication(ai,title63 , person171).
publication(ai,title94 , person171).
publication(ai,title147 , person171).
publication(ai,title170 , person171).
publication(ai,title125 , person171).
publication(ai,title90 , person171).
publication(ai,title114 , person407).
publication(ai,title12 , person407).
publication(ai,title259 , person407).
publication(ai,title217 , person407).
publication(ai,title92 , person407).
publication(ai,title182 , person407).
publication(ai,title59 , person407).
publication(ai,title160 , person407).
publication(ai,title55 , person407).
publication(ai,title88 , person407).
publication(ai,title167 , person407).
publication(ai,title24 , person407).
publication(ai,title323 , person407).
publication(ai,title331 , person407).
publication(ai,title190 , person407).
publication(ai,title120 , person407).
publication(ai,title250 , person407).
publication(ai,title11 , person407).
publication(ai,title284 , person407).
publication(ai,title199 , person407).
publication(ai,title240 , person407).
publication(ai,title335 , person407).
publication(ai,title270 , person407).
publication(ai,title241 , person407).
publication(ai,title212 , person407).
publication(ai,title110 , person407).
publication(ai,title268 , person407).
publication(ai,title228 , person407).
publication(ai,title347 , person407).
publication(ai,title266 , person407).
publication(ai,title192 , person407).
publication(ai,title345 , person407).
publication(ai,title5 , person407).
publication(ai,title271 , person407).
publication(ai,title89 , person407).
publication(ai,title165 , person407).
publication(ai,title113 , person407).
publication(ai,title233 , person407).
publication(ai,title179 , person407).
publication(ai,title132 , person407).
publication(ai,title177 , person407).
publication(ai,title310 , person407).
publication(ai,title171 , person407).
publication(ai,title33 , person407).
publication(ai,title218 , person407).
publication(ai,title71 , person407).
publication(ai,title341 , person407).
publication(ai,title207 , person407).
publication(ai,title229 , person407).
publication(ai,title292 , person407).
publication(ai,title316 , person407).
publication(ai,title49 , person407).
publication(ai,title38 , person407).
publication(ai,title238 , person407).
publication(ai,title283 , person407).
publication(ai,title255 , person407).
publication(ai,title224 , person407).
publication(ai,title260 , person407).
publication(ai,title297 , person407).
publication(ai,title312 , person407).
publication(ai,title273 , person407).
publication(ai,title25 , person407).
publication(ai,title258 , person407).
publication(ai,title118 , person408).
publication(ai,title118 , person353).
publication(ai,title40 , person239).
publication(ai,title13 , person13).
publication(ai,title26 , person13).
publication(ai,title275 , person13).
publication(ai,title333 , person13).
publication(ai,title54 , person13).
publication(ai,title10 , person13).
publication(ai,title84 , person13).
publication(ai,title161 , person13).
publication(ai,title248 , person13).
publication(ai,title344 , person13).
publication(ai,title50 , person352).
publication(ai,title208 , person352).
publication(ai,title103 , person352).
publication(ai,title166 , person352).
publication(ai,title314 , person352).
publication(ai,title47 , person352).
publication(ai,title86 , person352).
publication(ai,title82 , person352).
publication(ai,title79 , person352).
publication(ai,title261 , person352).
publication(ai,title87 , person352).
publication(ai,title329 , person45).
publication(ai,title79 , person45).
publication(ai,title325 , person45).
publication(ai,title44 , person45).
publication(ai,title150 , person148).
publication(ai,title125 , person148).
publication(ai,title90 , person148).
publication(ai,title162 , person193).
publication(ai,title170 , person314).
publication(ai,title107 , person314).
publication(ai,title172 , person314).
publication(ai,title295 , person314).
publication(ai,title222 , person314).
publication(ai,title301 , person314).
publication(ai,title25 , person21).
publication(ai,title122 , person262).
publication(ai,title314 , person262).
publication(ai,title29 , person262).
publication(ai,title72 , person262).
publication(ai,title290 , person262).
publication(ai,title86 , person262).
publication(ai,title261 , person262).
publication(ai,title41 , person262).
publication(ai,title102 , person257).
publication(ai,title274 , person257).
publication(ai,title254 , person257).
publication(ai,title119 , person257).
publication(ai,title269 , person73).
publication(ai,title63 , person73).
publication(ai,title318 , person380).
publication(ai,title115 , person380).
publication(ai,title231 , person380).
publication(ai,title226 , person380).
publication(ai,title195 , person380).
publication(ai,title314 , person406).
publication(ai,title86 , person406).
publication(ai,title261 , person406).
publication(ai,title118 , person208).
publication(ai,title182 , person63).
publication(ai,title178 , person63).
publication(ai,title225 , person63).
publication(ai,title5 , person63).
publication(ai,title314 , person63).
publication(ai,title86 , person63).
publication(ai,title147 , person63).
publication(ai,title261 , person63).
publication(ai,title97 , person63).
publication(ai,title222 , person63).
publication(ai,title236 , person63).
publication(ai,title301 , person63).
publication(ai,title325 , person83).
taughtby(graphics,course157, person342, autumn_0001).
taughtby(graphics,course110, person351, winter_0001).
taughtby(graphics,course13, person72, winter_0001).
taughtby(graphics,course67, person394, winter_0001).
taughtby(graphics,course157, person72, spring_0001).
taughtby(graphics,course164, person351, spring_0001).
taughtby(graphics,course0, person40, spring_0001).
taughtby(graphics,course115, person342, spring_0001).
taughtby(graphics,course101, person279, spring_0001).
taughtby(graphics,course153, person394, spring_0001).
taughtby(graphics,course157, person72, autumn_0102).
taughtby(graphics,course110, person351, autumn_0102).
taughtby(graphics,course125, person351, winter_0102).
taughtby(graphics,course28, person394, winter_0102).
taughtby(graphics,course13, person342, winter_0102).
taughtby(graphics,course1, person40, winter_0102).
taughtby(graphics,course157, person394, spring_0102).
taughtby(graphics,course164, person351, spring_0102).
taughtby(graphics,course115, person72, spring_0102).
taughtby(graphics,course153, person342, spring_0102).
taughtby(graphics,course157, person72, autumn_0203).
taughtby(graphics,course110, person351, autumn_0203).
taughtby(graphics,course108, person279, autumn_0203).
taughtby(graphics,course89, person394, winter_0203).
taughtby(graphics,course125, person351, winter_0203).
taughtby(graphics,course13, person342, winter_0203).
taughtby(graphics,course157, person72, spring_0203).
taughtby(graphics,course164, person351, spring_0203).
taughtby(graphics,course115, person342, spring_0203).
taughtby(graphics,course101, person394, spring_0203).
taughtby(graphics,course110, person351, autumn_0304).
taughtby(graphics,course79, person72, autumn_0304).
taughtby(graphics,course89, person394, winter_0304).
taughtby(graphics,course125, person351, winter_0304).
taughtby(graphics,course13, person342, winter_0304).
taughtby(graphics,course157, person342, spring_0304).
taughtby(graphics,course164, person351, spring_0304).
taughtby(graphics,course101, person279, spring_0304).
taughtby(graphics,course136, person394, spring_0304).
courselevel(graphics,course89, level_400).
courselevel(graphics,course157, level_400).
courselevel(graphics,course110, level_400).
courselevel(graphics,course41, level_400).
courselevel(graphics,course148, level_400).
courselevel(graphics,course125, level_400).
courselevel(graphics,course93, level_400).
courselevel(graphics,course164, level_400).
courselevel(graphics,course159, level_400).
courselevel(graphics,course28, level_400).
courselevel(graphics,course154, level_400).
courselevel(graphics,course118, level_400).
courselevel(graphics,course107, level_400).
courselevel(graphics,course0, level_500).
courselevel(graphics,course13, level_500).
courselevel(graphics,course115, level_500).
courselevel(graphics,course101, level_500).
courselevel(graphics,course136, level_500).
courselevel(graphics,course150, level_500).
courselevel(graphics,course109, level_500).
courselevel(graphics,course3, level_500).
courselevel(graphics,course108, level_500).
courselevel(graphics,course56, level_500).
courselevel(graphics,course67, level_500).
courselevel(graphics,course153, level_500).
courselevel(graphics,course1, level_500).
courselevel(graphics,course83, level_500).
courselevel(graphics,course79, level_500).
courselevel(graphics,course114, level_500).
hasposition(graphics,person40, faculty).
hasposition(graphics,person342, faculty).
hasposition(graphics,person111, faculty_adjunct).
hasposition(graphics,person115, faculty).
hasposition(graphics,person351, faculty).
hasposition(graphics,person72, faculty).
hasposition(graphics,person393, faculty).
hasposition(graphics,person394, faculty).
hasposition(graphics,person279, faculty).
inphase(graphics,person241, post_quals).
inphase(graphics,person217, post_generals).
inphase(graphics,person270, pre_quals).
inphase(graphics,person206, post_generals).
inphase(graphics,person81, post_generals).
inphase(graphics,person122, post_quals).
inphase(graphics,person228, post_quals).
inphase(graphics,person51, pre_quals).
inphase(graphics,person41, post_quals).
inphase(graphics,person163, post_quals).
inphase(graphics,person435, post_quals).
inphase(graphics,person404, post_generals).
inphase(graphics,person363, pre_quals).
inphase(graphics,person427, post_quals).
inphase(graphics,person142, post_generals).
inphase(graphics,person431, pre_quals).
inphase(graphics,person283, pre_quals).
inphase(graphics,person149, post_quals).
inphase(graphics,person300, post_generals).
inphase(graphics,person200, post_quals).
inphase(graphics,person157, post_quals).
inphase(graphics,person113, post_generals).
tempadvisedby(graphics,person241, person393).
tempadvisedby(graphics,person270, person393).
tempadvisedby(graphics,person51, person72).
tempadvisedby(graphics,person363, person72).
tempadvisedby(graphics,person427, person393).
tempadvisedby(graphics,person431, person393).
tempadvisedby(graphics,person283, person394).
yearsinprogram(graphics,person241, year_3).
yearsinprogram(graphics,person217, year_5).
yearsinprogram(graphics,person270, year_1).
yearsinprogram(graphics,person206, year_6).
yearsinprogram(graphics,person81, year_6).
yearsinprogram(graphics,person122, year_4).
yearsinprogram(graphics,person228, year_3).
yearsinprogram(graphics,person51, year_2).
yearsinprogram(graphics,person41, year_5).
yearsinprogram(graphics,person163, year_4).
yearsinprogram(graphics,person435, year_4).
yearsinprogram(graphics,person404, year_4).
yearsinprogram(graphics,person363, year_3).
yearsinprogram(graphics,person427, year_4).
yearsinprogram(graphics,person142, year_9).
yearsinprogram(graphics,person431, year_2).
yearsinprogram(graphics,person283, year_1).
yearsinprogram(graphics,person149, year_5).
yearsinprogram(graphics,person300, year_8).
yearsinprogram(graphics,person200, year_4).
yearsinprogram(graphics,person157, year_4).
yearsinprogram(graphics,person113, year_4).
ta(graphics,course89, person228, winter_0304).
ta(graphics,course41, person296, winter_0304).
ta(graphics,course41, person36, winter_0304).
ta(graphics,course13, person431, winter_0304).
ta(graphics,course157, person328, autumn_0304).
ta(graphics,course157, person31, autumn_0304).
ta(graphics,course110, person61, autumn_0304).
ta(graphics,course110, person36, autumn_0304).
ta(graphics,course79, person157, autumn_0304).
ta(graphics,course79, person119, autumn_0304).
ta(graphics,course118, person296, summer_0203).
ta(graphics,course118, person317, summer_0203).
ta(graphics,course157, person119, spring_0203).
ta(graphics,course157, person230, spring_0203).
ta(graphics,course157, person3, spring_0203).
ta(graphics,course157, person258, spring_0203).
ta(graphics,course101, person241, spring_0203).
ta(graphics,course89, person188, winter_0203).
ta(graphics,course89, person41, winter_0203).
ta(graphics,course148, person327, winter_0203).
ta(graphics,course148, person140, winter_0203).
ta(graphics,course13, person51, winter_0203).
ta(graphics,course157, person321, autumn_0203).
ta(graphics,course157, person428, autumn_0203).
ta(graphics,course157, person3, autumn_0203).
ta(graphics,course157, person158, autumn_0203).
ta(graphics,course110, person327, autumn_0203).
ta(graphics,course110, person317, autumn_0203).
ta(graphics,course3, person431, autumn_0203).
ta(graphics,course150, person327, summer_0102).
ta(graphics,course150, person102, summer_0102).
ta(graphics,course157, person90, spring_0102).
ta(graphics,course157, person214, spring_0102).
ta(graphics,course157, person146, spring_0102).
ta(graphics,course157, person88, spring_0102).
ta(graphics,course93, person228, spring_0102).
ta(graphics,course93, person31, spring_0102).
ta(graphics,course93, person178, spring_0102).
ta(graphics,course153, person195, spring_0102).
ta(graphics,course153, person428, spring_0102).
ta(graphics,course148, person31, winter_0102).
ta(graphics,course159, person113, winter_0102).
ta(graphics,course13, person217, winter_0102).
taughtby(graphics,course41 , person351, winter_0304).
taughtby(graphics,course118, person351, summer_0203).
taughtby(graphics,course148, person351, winter_0203).
taughtby(graphics,course3 , person279, autumn_0203).
taughtby(graphics,course150 , person351, summer_0102).
taughtby(graphics,course93, person351, spring_0102).
taughtby(graphics,course148, person351, winter_0102).
taughtby(graphics,course159, person394, winter_0102).
professor(graphics,person40).
professor(graphics,person279).
professor(graphics,person394).
student(graphics,person38).
student(graphics,person261).
student(graphics,person149).
student(graphics,person306).
student(graphics,person410).
student(graphics,person157).
student(graphics,person200).
student(graphics,person404).
student(graphics,person122).
student(graphics,person322).
student(graphics,person131).
student(graphics,person85).
professor(graphics,person342).
professor(graphics,person111).
professor(graphics,person115).
professor(graphics,person351).
professor(graphics,person72).
professor(graphics,person393).
student(graphics,person241).
student(graphics,person217).
student(graphics,person270).
student(graphics,person206).
student(graphics,person81).
student(graphics,person228).
student(graphics,person51).
student(graphics,person41).
student(graphics,person163).
student(graphics,person435).
student(graphics,person363).
student(graphics,person427).
student(graphics,person142).
student(graphics,person431).
student(graphics,person283).
student(graphics,person300).
student(graphics,person113).
student(graphics,person296).
student(graphics,person36).
student(graphics,person328).
student(graphics,person31).
student(graphics,person61).
student(graphics,person119).
student(graphics,person3).
student(graphics,person317).
student(graphics,person230).
student(graphics,person258).
student(graphics,person188).
student(graphics,person327).
student(graphics,person140).
student(graphics,person321).
student(graphics,person428).
student(graphics,person158).
student(graphics,person102).
student(graphics,person90).
student(graphics,person214).
student(graphics,person146).
student(graphics,person88).
student(graphics,person178).
student(graphics,person195).
sameperson(graphics,person40, person40).
sameperson(graphics,person279, person279).
sameperson(graphics,person394, person394).
sameperson(graphics,person38, person38).
sameperson(graphics,person261, person261).
sameperson(graphics,person149, person149).
sameperson(graphics,person306, person306).
sameperson(graphics,person410, person410).
sameperson(graphics,person157, person157).
sameperson(graphics,person200, person200).
sameperson(graphics,person404, person404).
sameperson(graphics,person122, person122).
sameperson(graphics,person322, person322).
sameperson(graphics,person131, person131).
sameperson(graphics,person85, person85).
sameperson(graphics,person342, person342).
sameperson(graphics,person111, person111).
sameperson(graphics,person115, person115).
sameperson(graphics,person351, person351).
sameperson(graphics,person72, person72).
sameperson(graphics,person393, person393).
sameperson(graphics,person241, person241).
sameperson(graphics,person217, person217).
sameperson(graphics,person270, person270).
sameperson(graphics,person206, person206).
sameperson(graphics,person81, person81).
sameperson(graphics,person228, person228).
sameperson(graphics,person51, person51).
sameperson(graphics,person41, person41).
sameperson(graphics,person163, person163).
sameperson(graphics,person435, person435).
sameperson(graphics,person363, person363).
sameperson(graphics,person427, person427).
sameperson(graphics,person142, person142).
sameperson(graphics,person431, person431).
sameperson(graphics,person283, person283).
sameperson(graphics,person300, person300).
sameperson(graphics,person113, person113).
sameperson(graphics,person296, person296).
sameperson(graphics,person36, person36).
sameperson(graphics,person328, person328).
sameperson(graphics,person31, person31).
sameperson(graphics,person61, person61).
sameperson(graphics,person119, person119).
sameperson(graphics,person3, person3).
sameperson(graphics,person317, person317).
sameperson(graphics,person230, person230).
sameperson(graphics,person258, person258).
sameperson(graphics,person188, person188).
sameperson(graphics,person327, person327).
sameperson(graphics,person140, person140).
sameperson(graphics,person321, person321).
sameperson(graphics,person428, person428).
sameperson(graphics,person158, person158).
sameperson(graphics,person102, person102).
sameperson(graphics,person90, person90).
sameperson(graphics,person214, person214).
sameperson(graphics,person146, person146).
sameperson(graphics,person88, person88).
sameperson(graphics,person178, person178).
sameperson(graphics,person195, person195).
samecourse(graphics,course89, course89).
samecourse(graphics,course157, course157).
samecourse(graphics,course110, course110).
samecourse(graphics,course125, course125).
samecourse(graphics,course164, course164).
samecourse(graphics,course28, course28).
samecourse(graphics,course107, course107).
samecourse(graphics,course0, course0).
samecourse(graphics,course13, course13).
samecourse(graphics,course115, course115).
samecourse(graphics,course101, course101).
samecourse(graphics,course136, course136).
samecourse(graphics,course108, course108).
samecourse(graphics,course67, course67).
samecourse(graphics,course153, course153).
samecourse(graphics,course1, course1).
samecourse(graphics,course83, course83).
samecourse(graphics,course79, course79).
samecourse(graphics,course114, course114).
samecourse(graphics,course148, course148).
samecourse(graphics,course93, course93).
samecourse(graphics,course159, course159).
samecourse(graphics,course154, course154).
samecourse(graphics,course118, course118).
samecourse(graphics,course109, course109).
samecourse(graphics,course56, course56).
samecourse(graphics,course41, course41).
samecourse(graphics,course150, course150).
samecourse(graphics,course3, course3).
sameproject(graphics,project103, project103).
sameproject(graphics,project91, project91).
sameproject(graphics,project96, project96).
sameproject(graphics,project15, project15).
sameproject(graphics,project140, project140).
sameproject(graphics,project78, project78).
sameproject(graphics,project47, project47).
sameproject(graphics,project106, project106).
sameproject(graphics,project118, project118).
sameproject(graphics,project133, project133).
sameproject(graphics,project34, project34).
sameproject(graphics,project151, project151).
sameproject(graphics,project81, project81).
sameproject(graphics,project123, project123).
sameproject(graphics,project23, project23).
sameproject(graphics,project132, project132).
sameproject(graphics,project71, project71).
sameproject(graphics,project135, project135).
sameproject(graphics,project149, project149).
sameproject(graphics,project25, project25).
sameproject(graphics,project65, project65).
sameproject(graphics,project6, project6).
sameproject(graphics,project88, project88).
sameproject(graphics,project54, project54).
sameproject(graphics,project105, project105).
sameproject(graphics,project46, project46).
sameproject(graphics,project142, project142).
sameproject(graphics,project48, project48).
sameproject(graphics,project2, project2).
sameproject(graphics,project92, project92).
sameproject(graphics,project86, project86).
sameproject(graphics,project19, project19).
sameproject(graphics,project139, project139).
sameproject(graphics,project117, project117).
sameproject(graphics,project98, project98).
sameproject(graphics,project59, project59).
sameproject(graphics,project145, project145).
sameproject(graphics,project69, project69).
sameproject(graphics,project53, project53).
sameproject(graphics,project107, project107).
sameproject(graphics,project136, project136).
sameproject(graphics,project87, project87).
sameproject(graphics,project45, project45).
sameproject(graphics,project79, project79).
sameproject(graphics,project35, project35).
sameproject(graphics,project49, project49).
publication(graphics,title322 , person40).
publication(graphics,title346 , person40).
publication(graphics,title1 , person40).
publication(graphics,title183 , person394).
publication(graphics,title30 , person394).
publication(graphics,title153 , person306).
publication(graphics,title111 , person306).
publication(graphics,title15 , person410).
publication(graphics,title45 , person410).
publication(graphics,title60 , person410).
publication(graphics,title93 , person410).
publication(graphics,title272 , person410).
publication(graphics,title116 , person410).
publication(graphics,title203 , person404).
publication(graphics,title185 , person404).
publication(graphics,title201 , person404).
publication(graphics,title137 , person322).
publication(graphics,title109 , person342).
publication(graphics,title181 , person342).
publication(graphics,title135 , person342).
publication(graphics,title15 , person342).
publication(graphics,title45 , person342).
publication(graphics,title263 , person342).
publication(graphics,title35 , person342).
publication(graphics,title272 , person342).
publication(graphics,title116 , person342).
publication(graphics,title85 , person342).
publication(graphics,title304 , person342).
publication(graphics,title302 , person342).
publication(graphics,title137 , person342).
publication(graphics,title183 , person72).
publication(graphics,title30 , person72).
publication(graphics,title201 , person72).
publication(graphics,title302 , person72).
publication(graphics,title137 , person72).
publication(graphics,title109 , person393).
publication(graphics,title153 , person393).
publication(graphics,title123 , person393).
publication(graphics,title181 , person393).
publication(graphics,title135 , person393).
publication(graphics,title45 , person393).
publication(graphics,title60 , person393).
publication(graphics,title93 , person393).
publication(graphics,title35 , person393).
publication(graphics,title220 , person393).
publication(graphics,title272 , person393).
publication(graphics,title116 , person393).
publication(graphics,title111 , person393).
publication(graphics,title4 , person393).
publication(graphics,title85 , person393).
publication(graphics,title304 , person393).
publication(graphics,title322 , person393).
publication(graphics,title346 , person393).
publication(graphics,title1 , person393).
publication(graphics,title304 , person241).
publication(graphics,title302 , person217).
publication(graphics,title137 , person206).
publication(graphics,title109 , person81).
publication(graphics,title45 , person81).
publication(graphics,title116 , person81).
publication(graphics,title304 , person81).
publication(graphics,title100 , person41).
publication(graphics,title99 , person435).
publication(graphics,title123 , person142).
publication(graphics,title263 , person142).
publication(graphics,title220 , person142).
publication(graphics,title99 , person300).
publication(graphics,title181 , person300).
publication(graphics,title35 , person300).
publication(graphics,title4 , person300).
publication(graphics,title203 , person113).
publication(graphics,title185 , person113).
publication(graphics,title100 , person113).
taughtby(language,course51, person5, autumn_0001).
taughtby(language,course172, person335, autumn_0001).
taughtby(language,course46, person335, winter_0001).
taughtby(language,course71, person5, winter_0001).
taughtby(language,course124, person335, spring_0001).
taughtby(language,course51, person166, autumn_0102).
taughtby(language,course49, person263, winter_0102).
taughtby(language,course19, person5, winter_0102).
taughtby(language,course63, person335, spring_0102).
taughtby(language,course51, person18, autumn_0203).
taughtby(language,course53, person248, autumn_0203).
taughtby(language,course172, person335, autumn_0203).
taughtby(language,course49, person248, winter_0203).
taughtby(language,course46, person335, winter_0203).
taughtby(language,course146, person335, spring_0203).
taughtby(language,course49, person248, spring_0203).
taughtby(language,course53, person189, autumn_0304).
taughtby(language,course172, person46, autumn_0304).
taughtby(language,course138, person335, autumn_0304).
taughtby(language,course124, person9, winter_0304).
taughtby(language,course49, person64, winter_0304).
taughtby(language,course46, person335, winter_0304).
taughtby(language,course146, person335, spring_0304).
taughtby(language,course124, person46, spring_0304).
taughtby(language,course49, person189, spring_0304).
taughtby(language,course19, person370, spring_0304).
courselevel(language,course146, level_300).
courselevel(language,course124, level_300).
courselevel(language,course51, level_400).
courselevel(language,course49, level_400).
courselevel(language,course53, level_400).
courselevel(language,course46, level_500).
courselevel(language,course19, level_500).
courselevel(language,course172, level_500).
courselevel(language,course71, level_500).
courselevel(language,course63, level_500).
courselevel(language,course152, level_500).
courselevel(language,course54, level_500).
courselevel(language,course138, level_500).
courselevel(language,course35, level_500).
hasposition(language,person335, faculty).
hasposition(language,person46, faculty).
hasposition(language,person189, faculty_adjunct).
hasposition(language,person5, faculty).
inphase(language,person18, pre_quals).
inphase(language,person9, post_generals).
inphase(language,person429, post_quals).
inphase(language,person27, pre_quals).
inphase(language,person362, post_quals).
inphase(language,person96, post_generals).
inphase(language,person361, post_generals).
inphase(language,person263, post_generals).
inphase(language,person183, pre_quals).
inphase(language,person118, post_generals).
tempadvisedby(language,person27, person335).
yearsinprogram(language,person18, year_3).
yearsinprogram(language,person9, year_5).
yearsinprogram(language,person429, year_5).
yearsinprogram(language,person27, year_1).
yearsinprogram(language,person362, year_3).
yearsinprogram(language,person96, year_5).
yearsinprogram(language,person361, year_6).
yearsinprogram(language,person263, year_6).
yearsinprogram(language,person183, year_4).
yearsinprogram(language,person118, year_4).
ta(language,course49, person361, winter_0304).
ta(language,course46, person429, winter_0304).
ta(language,course124, person105, autumn_0304).
ta(language,course51, person27, autumn_0304).
ta(language,course138, person18, autumn_0304).
ta(language,course49, person361, summer_0203).
ta(language,course124, person108, spring_0203).
ta(language,course124, person203, spring_0203).
ta(language,course51, person96, spring_0203).
ta(language,course49, person287, spring_0203).
ta(language,course49, person87, spring_0203).
ta(language,course124, person18, winter_0203).
ta(language,course124, person35, winter_0203).
ta(language,course49, person287, winter_0203).
ta(language,course49, person87, winter_0203).
ta(language,course46, person429, winter_0203).
ta(language,course124, person108, autumn_0203).
ta(language,course124, person203, autumn_0203).
ta(language,course53, person287, autumn_0203).
ta(language,course172, person325, autumn_0203).
ta(language,course49, person361, summer_0102).
ta(language,course51, person39, spring_0102).
ta(language,course124, person76, winter_0102).
ta(language,course124, person9, winter_0102).
ta(language,course49, person96, winter_0102).
ta(language,course19, person232, winter_0102).
taughtby(language,course53, person248, autumn_0304).
taughtby(language,course49, person64, summer_0203).
taughtby(language,course49, person64, summer_0102).
professor(language,person248).
professor(language,person64).
professor(language,person166).
professor(language,person370).
professor(language,person335).
professor(language,person46).
professor(language,person189).
professor(language,person5).
student(language,person18).
student(language,person9).
student(language,person429).
student(language,person27).
student(language,person362).
student(language,person96).
student(language,person361).
student(language,person263).
student(language,person183).
student(language,person118).
student(language,person105).
student(language,person108).
student(language,person203).
student(language,person287).
student(language,person87).
student(language,person39).
student(language,person35).
student(language,person325).
student(language,person76).
student(language,person232).
sameperson(language,person248, person248).
sameperson(language,person64, person64).
sameperson(language,person166, person166).
sameperson(language,person370, person370).
sameperson(language,person335, person335).
sameperson(language,person46, person46).
sameperson(language,person189, person189).
sameperson(language,person5, person5).
sameperson(language,person18, person18).
sameperson(language,person9, person9).
sameperson(language,person429, person429).
sameperson(language,person27, person27).
sameperson(language,person362, person362).
sameperson(language,person96, person96).
sameperson(language,person361, person361).
sameperson(language,person263, person263).
sameperson(language,person183, person183).
sameperson(language,person118, person118).
sameperson(language,person105, person105).
sameperson(language,person108, person108).
sameperson(language,person203, person203).
sameperson(language,person287, person287).
sameperson(language,person87, person87).
sameperson(language,person39, person39).
sameperson(language,person35, person35).
sameperson(language,person325, person325).
sameperson(language,person76, person76).
sameperson(language,person232, person232).
samecourse(language,course146, course146).
samecourse(language,course124, course124).
samecourse(language,course51, course51).
samecourse(language,course49, course49).
samecourse(language,course53, course53).
samecourse(language,course46, course46).
samecourse(language,course19, course19).
samecourse(language,course172, course172).
samecourse(language,course71, course71).
samecourse(language,course63, course63).
samecourse(language,course152, course152).
samecourse(language,course54, course54).
samecourse(language,course138, course138).
samecourse(language,course35, course35).
sameproject(language,project9, project9).
sameproject(language,project102, project102).
sameproject(language,project108, project108).
sameproject(language,project89, project89).
sameproject(language,project147, project147).
sameproject(language,project3, project3).
sameproject(language,project95, project95).
sameproject(language,project120, project120).
publication(language,title106 , person335).
publication(language,title14 , person335).
publication(language,title130 , person335).
publication(language,title106 , person5).
publication(language,title130 , person5).
publication(language,title257 , person429).
publication(language,title142 , person429).
publication(language,title14 , person429).
publication(language,title257 , person183).
publication(language,title142 , person183).
taughtby(systems,course18, person373, autumn_0001).
taughtby(systems,course151, person290, autumn_0001).
taughtby(systems,course38, person204, autumn_0001).
taughtby(systems,course48, person107, autumn_0001).
taughtby(systems,course21, person99, autumn_0001).
taughtby(systems,course18, person326, winter_0001).
taughtby(systems,course151, person235, winter_0001).
taughtby(systems,course38, person104, winter_0001).
taughtby(systems,course20, person180, winter_0001).
taughtby(systems,course62, person101, winter_0001).
taughtby(systems,course129, person373, winter_0001).
taughtby(systems,course2, person180, winter_0001).
taughtby(systems,course18, person107, spring_0001).
taughtby(systems,course151, person267, spring_0001).
taughtby(systems,course80, person180, spring_0001).
taughtby(systems,course30, person290, spring_0001).
taughtby(systems,course8, person297, spring_0001).
taughtby(systems,course120, person235, spring_0001).
taughtby(systems,course74, person124, spring_0001).
taughtby(systems,course18, person213, autumn_0102).
taughtby(systems,course151, person179, autumn_0102).
taughtby(systems,course38, person104, autumn_0102).
taughtby(systems,course48, person375, autumn_0102).
taughtby(systems,course4, person107, autumn_0102).
taughtby(systems,course18, person107, winter_0102).
taughtby(systems,course151, person290, winter_0102).
taughtby(systems,course38, person124, winter_0102).
taughtby(systems,course20, person180, winter_0102).
taughtby(systems,course62, person101, winter_0102).
taughtby(systems,course129, person213, winter_0102).
taughtby(systems,course166, person235, winter_0102).
taughtby(systems,course2, person180, winter_0102).
taughtby(systems,course34, person179, winter_0102).
taughtby(systems,course18, person326, spring_0102).
taughtby(systems,course151, person234, spring_0102).
taughtby(systems,course80, person98, spring_0102).
taughtby(systems,course30, person290, spring_0102).
taughtby(systems,course75, person267, spring_0102).
taughtby(systems,course8, person297, spring_0102).
taughtby(systems,course116, person375, spring_0102).
taughtby(systems,course120, person235, spring_0102).
taughtby(systems,course74, person104, spring_0102).
taughtby(systems,course14, person124, spring_0102).
taughtby(systems,course162, person213, spring_0102).
taughtby(systems,course18, person107, autumn_0203).
taughtby(systems,course151, person267, autumn_0203).
taughtby(systems,course38, person104, autumn_0203).
taughtby(systems,course48, person375, autumn_0203).
taughtby(systems,course30, person290, autumn_0203).
taughtby(systems,course129, person213, autumn_0203).
taughtby(systems,course74, person124, autumn_0203).
taughtby(systems,course18, person290, winter_0203).
taughtby(systems,course151, person179, winter_0203).
taughtby(systems,course38, person104, winter_0203).
taughtby(systems,course75, person267, winter_0203).
taughtby(systems,course139, person235, winter_0203).
taughtby(systems,course167, person98, winter_0203).
taughtby(systems,course18, person375, spring_0203).
taughtby(systems,course151, person234, spring_0203).
taughtby(systems,course80, person98, spring_0203).
taughtby(systems,course30, person290, spring_0203).
taughtby(systems,course21, person22, spring_0203).
taughtby(systems,course120, person235, spring_0203).
taughtby(systems,course4, person107, spring_0203).
taughtby(systems,course151, person179, autumn_0304).
taughtby(systems,course38, person124, autumn_0304).
taughtby(systems,course48, person213, autumn_0304).
taughtby(systems,course74, person104, autumn_0304).
taughtby(systems,course18, person290, winter_0304).
taughtby(systems,course151, person82, winter_0304).
taughtby(systems,course38, person255, winter_0304).
taughtby(systems,course20, person180, winter_0304).
taughtby(systems,course75, person267, winter_0304).
taughtby(systems,course129, person213, winter_0304).
taughtby(systems,course23, person179, winter_0304).
taughtby(systems,course9, person235, winter_0304).
taughtby(systems,course18, person375, spring_0304).
taughtby(systems,course151, person234, spring_0304).
taughtby(systems,course80, person101, spring_0304).
taughtby(systems,course30, person290, spring_0304).
taughtby(systems,course120, person235, spring_0304).
taughtby(systems,course120, person82, spring_0304).
courselevel(systems,course5, level_300).
courselevel(systems,course18, level_300).
courselevel(systems,course21, level_400).
courselevel(systems,course151, level_400).
courselevel(systems,course38, level_400).
courselevel(systems,course45, level_400).
courselevel(systems,course20, level_400).
courselevel(systems,course48, level_400).
courselevel(systems,course62, level_400).
courselevel(systems,course80, level_400).
courselevel(systems,course30, level_400).
courselevel(systems,course174, level_400).
courselevel(systems,course75, level_400).
courselevel(systems,course8, level_400).
courselevel(systems,course129, level_500).
courselevel(systems,course116, level_500).
courselevel(systems,course120, level_500).
courselevel(systems,course166, level_500).
courselevel(systems,course74, level_500).
courselevel(systems,course2, level_500).
courselevel(systems,course4, level_500).
courselevel(systems,course34, level_500).
courselevel(systems,course14, level_500).
courselevel(systems,course167, level_500).
courselevel(systems,course139, level_500).
courselevel(systems,course162, level_500).
courselevel(systems,course61, level_500).
courselevel(systems,course23, level_500).
courselevel(systems,course9, level_500).
courselevel(systems,course87, level_500).
courselevel(systems,course88, level_500).
hasposition(systems,person124, faculty).
hasposition(systems,person375, faculty_emeritus).
hasposition(systems,person234, faculty).
hasposition(systems,person101, faculty).
hasposition(systems,person180, faculty).
hasposition(systems,person98, faculty).
hasposition(systems,person107, faculty).
hasposition(systems,person235, faculty).
hasposition(systems,person297, faculty_emeritus).
hasposition(systems,person82, faculty).
hasposition(systems,person179, faculty).
hasposition(systems,person213, faculty).
hasposition(systems,person22, faculty_emeritus).
hasposition(systems,person373, faculty).
hasposition(systems,person104, faculty).
hasposition(systems,person290, faculty).
inphase(systems,person19, pre_quals).
inphase(systems,person398, pre_quals).
inphase(systems,person368, post_generals).
inphase(systems,person130, post_generals).
inphase(systems,person299, pre_quals).
inphase(systems,person175, post_generals).
inphase(systems,person255, post_generals).
inphase(systems,person411, post_generals).
inphase(systems,person426, post_quals).
inphase(systems,person99, post_quals).
inphase(systems,person212, post_generals).
inphase(systems,person403, post_generals).
inphase(systems,person402, pre_quals).
inphase(systems,person391, post_quals).
inphase(systems,person253, post_generals).
inphase(systems,person280, pre_quals).
inphase(systems,person417, pre_quals).
inphase(systems,person92, post_generals).
inphase(systems,person419, post_generals).
inphase(systems,person357, post_quals).
inphase(systems,person67, post_generals).
inphase(systems,person222, pre_quals).
inphase(systems,person89, post_generals).
inphase(systems,person277, pre_quals).
inphase(systems,person15, post_quals).
inphase(systems,person80, post_generals).
inphase(systems,person376, post_quals).
inphase(systems,person62, pre_quals).
inphase(systems,person218, post_generals).
inphase(systems,person186, pre_quals).
inphase(systems,person187, pre_quals).
inphase(systems,person343, pre_quals).
inphase(systems,person154, post_quals).
inphase(systems,person204, post_generals).
inphase(systems,person126, post_quals).
inphase(systems,person129, post_generals).
inphase(systems,person374, post_generals).
inphase(systems,person155, pre_quals).
inphase(systems,person100, post_quals).
inphase(systems,person116, pre_quals).
tempadvisedby(systems,person19, person98).
tempadvisedby(systems,person398, person213).
tempadvisedby(systems,person299, person235).
tempadvisedby(systems,person175, person107).
tempadvisedby(systems,person402, person234).
tempadvisedby(systems,person417, person104).
tempadvisedby(systems,person277, person235).
tempadvisedby(systems,person186, person290).
tempadvisedby(systems,person187, person180).
tempadvisedby(systems,person343, person213).
yearsinprogram(systems,person19, year_1).
yearsinprogram(systems,person398, year_1).
yearsinprogram(systems,person368, year_4).
yearsinprogram(systems,person130, year_8).
yearsinprogram(systems,person299, year_3).
yearsinprogram(systems,person175, year_2).
yearsinprogram(systems,person255, year_5).
yearsinprogram(systems,person411, year_6).
yearsinprogram(systems,person426, year_5).
yearsinprogram(systems,person99, year_2).
yearsinprogram(systems,person212, year_7).
yearsinprogram(systems,person403, year_12).
yearsinprogram(systems,person402, year_2).
yearsinprogram(systems,person391, year_4).
yearsinprogram(systems,person253, year_5).
yearsinprogram(systems,person280, year_3).
yearsinprogram(systems,person417, year_1).
yearsinprogram(systems,person92, year_5).
yearsinprogram(systems,person419, year_7).
yearsinprogram(systems,person357, year_4).
yearsinprogram(systems,person67, year_6).
yearsinprogram(systems,person222, year_1).
yearsinprogram(systems,person89, year_5).
yearsinprogram(systems,person277, year_1).
yearsinprogram(systems,person15, year_3).
yearsinprogram(systems,person80, year_6).
yearsinprogram(systems,person376, year_4).
yearsinprogram(systems,person62, year_2).
yearsinprogram(systems,person218, year_12).
yearsinprogram(systems,person186, year_1).
yearsinprogram(systems,person187, year_1).
yearsinprogram(systems,person343, year_1).
yearsinprogram(systems,person154, year_4).
yearsinprogram(systems,person204, year_6).
yearsinprogram(systems,person126, year_5).
yearsinprogram(systems,person129, year_6).
yearsinprogram(systems,person374, year_12).
yearsinprogram(systems,person155, year_2).
yearsinprogram(systems,person100, year_5).
yearsinprogram(systems,person116, year_3).
ta(systems,course18, person398, winter_0304).
ta(systems,course18, person274, winter_0304).
ta(systems,course151, person4, winter_0304).
ta(systems,course151, person299, winter_0304).
ta(systems,course151, person71, winter_0304).
ta(systems,course38, person222, winter_0304).
ta(systems,course38, person207, winter_0304).
ta(systems,course20, person368, winter_0304).
ta(systems,course129, person67, winter_0304).
ta(systems,course23, person116, winter_0304).
ta(systems,course88, person130, winter_0304).
ta(systems,course18, person277, autumn_0304).
ta(systems,course18, person67, autumn_0304).
ta(systems,course151, person4, autumn_0304).
ta(systems,course151, person129, autumn_0304).
ta(systems,course38, person190, autumn_0304).
ta(systems,course38, person222, autumn_0304).
ta(systems,course38, person207, autumn_0304).
ta(systems,course45, person155, autumn_0304).
ta(systems,course45, person71, autumn_0304).
ta(systems,course48, person155, autumn_0304).
ta(systems,course18, person274, spring_0203).
ta(systems,course21, person198, spring_0203).
ta(systems,course151, person269, spring_0203).
ta(systems,course80, person358, spring_0203).
ta(systems,course61, person155, spring_0203).
ta(systems,course18, person116, winter_0203).
ta(systems,course151, person155, winter_0203).
ta(systems,course38, person62, winter_0203).
ta(systems,course18, person354, autumn_0203).
ta(systems,course18, person155, autumn_0203).
ta(systems,course151, person167, autumn_0203).
ta(systems,course151, person186, autumn_0203).
ta(systems,course38, person154, autumn_0203).
ta(systems,course45, person358, autumn_0203).
ta(systems,course74, person255, autumn_0203).
ta(systems,course18, person67, spring_0102).
ta(systems,course151, person299, spring_0102).
ta(systems,course30, person116, spring_0102).
ta(systems,course174, person123, spring_0102).
ta(systems,course74, person204, spring_0102).
ta(systems,course14, person15, spring_0102).
ta(systems,course18, person15, winter_0102).
ta(systems,course18, person280, winter_0102).
ta(systems,course151, person223, winter_0102).
ta(systems,course151, person299, winter_0102).
ta(systems,course38, person357, winter_0102).
ta(systems,course38, person255, winter_0102).
ta(systems,course38, person92, winter_0102).
ta(systems,course20, person84, winter_0102).
ta(systems,course62, person126, winter_0102).
ta(systems,course129, person340, winter_0102).
ta(systems,course166, person100, winter_0102).
taughtby(systems,course88 , person235, winter_0304).
taughtby(systems,course61, person107, spring_0203).
taughtby(systems,course174 , person267, spring_0102).
professor(systems,person22).
professor(systems,person124).
professor(systems,person375).
professor(systems,person179).
professor(systems,person297).
professor(systems,person326).
professor(systems,person267).
professor(systems,person234).
professor(systems,person101).
professor(systems,person180).
professor(systems,person98).
professor(systems,person107).
professor(systems,person235).
professor(systems,person82).
professor(systems,person213).
professor(systems,person373).
professor(systems,person104).
professor(systems,person290).
student(systems,person19).
student(systems,person398).
student(systems,person368).
student(systems,person130).
student(systems,person299).
student(systems,person175).
student(systems,person255).
student(systems,person411).
student(systems,person426).
student(systems,person99).
student(systems,person212).
student(systems,person403).
student(systems,person402).
student(systems,person391).
student(systems,person253).
student(systems,person280).
student(systems,person417).
student(systems,person92).
student(systems,person419).
student(systems,person357).
student(systems,person67).
student(systems,person222).
student(systems,person89).
student(systems,person277).
student(systems,person15).
student(systems,person80).
student(systems,person376).
student(systems,person62).
student(systems,person218).
student(systems,person186).
student(systems,person187).
student(systems,person343).
student(systems,person154).
student(systems,person204).
student(systems,person126).
student(systems,person129).
student(systems,person374).
student(systems,person155).
student(systems,person100).
student(systems,person116).
student(systems,person274).
student(systems,person4).
student(systems,person71).
student(systems,person207).
student(systems,person190).
student(systems,person198).
student(systems,person269).
student(systems,person358).
student(systems,person354).
student(systems,person167).
student(systems,person123).
student(systems,person223).
student(systems,person84).
student(systems,person340).
sameperson(systems,person22, person22).
sameperson(systems,person124, person124).
sameperson(systems,person375, person375).
sameperson(systems,person179, person179).
sameperson(systems,person297, person297).
sameperson(systems,person326, person326).
sameperson(systems,person267, person267).
sameperson(systems,person234, person234).
sameperson(systems,person101, person101).
sameperson(systems,person180, person180).
sameperson(systems,person98, person98).
sameperson(systems,person107, person107).
sameperson(systems,person235, person235).
sameperson(systems,person82, person82).
sameperson(systems,person213, person213).
sameperson(systems,person373, person373).
sameperson(systems,person104, person104).
sameperson(systems,person290, person290).
sameperson(systems,person19, person19).
sameperson(systems,person398, person398).
sameperson(systems,person368, person368).
sameperson(systems,person130, person130).
sameperson(systems,person299, person299).
sameperson(systems,person175, person175).
sameperson(systems,person255, person255).
sameperson(systems,person411, person411).
sameperson(systems,person426, person426).
sameperson(systems,person99, person99).
sameperson(systems,person212, person212).
sameperson(systems,person403, person403).
sameperson(systems,person402, person402).
sameperson(systems,person391, person391).
sameperson(systems,person253, person253).
sameperson(systems,person280, person280).
sameperson(systems,person417, person417).
sameperson(systems,person92, person92).
sameperson(systems,person419, person419).
sameperson(systems,person357, person357).
sameperson(systems,person67, person67).
sameperson(systems,person222, person222).
sameperson(systems,person89, person89).
sameperson(systems,person277, person277).
sameperson(systems,person15, person15).
sameperson(systems,person80, person80).
sameperson(systems,person376, person376).
sameperson(systems,person62, person62).
sameperson(systems,person218, person218).
sameperson(systems,person186, person186).
sameperson(systems,person187, person187).
sameperson(systems,person343, person343).
sameperson(systems,person154, person154).
sameperson(systems,person204, person204).
sameperson(systems,person126, person126).
sameperson(systems,person129, person129).
sameperson(systems,person374, person374).
sameperson(systems,person155, person155).
sameperson(systems,person100, person100).
sameperson(systems,person116, person116).
sameperson(systems,person274, person274).
sameperson(systems,person4, person4).
sameperson(systems,person71, person71).
sameperson(systems,person207, person207).
sameperson(systems,person190, person190).
sameperson(systems,person198, person198).
sameperson(systems,person269, person269).
sameperson(systems,person358, person358).
sameperson(systems,person354, person354).
sameperson(systems,person167, person167).
sameperson(systems,person123, person123).
sameperson(systems,person223, person223).
sameperson(systems,person84, person84).
sameperson(systems,person340, person340).
samecourse(systems,course5, course5).
samecourse(systems,course18, course18).
samecourse(systems,course21, course21).
samecourse(systems,course151, course151).
samecourse(systems,course38, course38).
samecourse(systems,course45, course45).
samecourse(systems,course20, course20).
samecourse(systems,course48, course48).
samecourse(systems,course62, course62).
samecourse(systems,course80, course80).
samecourse(systems,course30, course30).
samecourse(systems,course75, course75).
samecourse(systems,course8, course8).
samecourse(systems,course129, course129).
samecourse(systems,course116, course116).
samecourse(systems,course120, course120).
samecourse(systems,course166, course166).
samecourse(systems,course74, course74).
samecourse(systems,course2, course2).
samecourse(systems,course4, course4).
samecourse(systems,course34, course34).
samecourse(systems,course14, course14).
samecourse(systems,course167, course167).
samecourse(systems,course139, course139).
samecourse(systems,course162, course162).
samecourse(systems,course61, course61).
samecourse(systems,course23, course23).
samecourse(systems,course9, course9).
samecourse(systems,course87, course87).
samecourse(systems,course174, course174).
samecourse(systems,course88, course88).
sameproject(systems,project27, project27).
sameproject(systems,project16, project16).
sameproject(systems,project60, project60).
sameproject(systems,project125, project125).
sameproject(systems,project39, project39).
sameproject(systems,project32, project32).
sameproject(systems,project44, project44).
sameproject(systems,project14, project14).
sameproject(systems,project114, project114).
sameproject(systems,project80, project80).
sameproject(systems,project43, project43).
sameproject(systems,project110, project110).
sameproject(systems,project68, project68).
sameproject(systems,project75, project75).
sameproject(systems,project128, project128).
sameproject(systems,project112, project112).
sameproject(systems,project37, project37).
sameproject(systems,project93, project93).
sameproject(systems,project40, project40).
sameproject(systems,project148, project148).
sameproject(systems,project26, project26).
sameproject(systems,project122, project122).
sameproject(systems,project4, project4).
sameproject(systems,project30, project30).
sameproject(systems,project67, project67).
sameproject(systems,project55, project55).
sameproject(systems,project31, project31).
sameproject(systems,project99, project99).
sameproject(systems,project134, project134).
sameproject(systems,project109, project109).
sameproject(systems,project72, project72).
sameproject(systems,project8, project8).
sameproject(systems,project28, project28).
sameproject(systems,project144, project144).
sameproject(systems,project10, project10).
sameproject(systems,project138, project138).
publication(systems,title294 , person124).
publication(systems,title214 , person124).
publication(systems,title186 , person124).
publication(systems,title141 , person124).
publication(systems,title246 , person124).
publication(systems,title253 , person124).
publication(systems,title227 , person124).
publication(systems,title48 , person124).
publication(systems,title282 , person124).
publication(systems,title267 , person124).
publication(systems,title133 , person124).
publication(systems,title245 , person124).
publication(systems,title213 , person375).
publication(systems,title91 , person375).
publication(systems,title74 , person375).
publication(systems,title9 , person375).
publication(systems,title117 , person375).
publication(systems,title239 , person375).
publication(systems,title194 , person375).
publication(systems,title64 , person179).
publication(systems,title143 , person179).
publication(systems,title338 , person179).
publication(systems,title51 , person179).
publication(systems,title294 , person234).
publication(systems,title213 , person234).
publication(systems,title168 , person234).
publication(systems,title96 , person234).
publication(systems,title3 , person234).
publication(systems,title189 , person234).
publication(systems,title46 , person234).
publication(systems,title28 , person234).
publication(systems,title141 , person234).
publication(systems,title53 , person234).
publication(systems,title176 , person234).
publication(systems,title58 , person234).
publication(systems,title65 , person234).
publication(systems,title198 , person234).
publication(systems,title315 , person234).
publication(systems,title196 , person234).
publication(systems,title91 , person234).
publication(systems,title289 , person234).
publication(systems,title43 , person234).
publication(systems,title48 , person234).
publication(systems,title74 , person234).
publication(systems,title117 , person234).
publication(systems,title239 , person234).
publication(systems,title194 , person234).
publication(systems,title237 , person234).
publication(systems,title204 , person234).
publication(systems,title121 , person234).
publication(systems,title209 , person234).
publication(systems,title332 , person234).
publication(systems,title247 , person234).
publication(systems,title342 , person234).
publication(systems,title169 , person234).
publication(systems,title144 , person234).
publication(systems,title17 , person234).
publication(systems,title80 , person234).
publication(systems,title7 , person234).
publication(systems,title234 , person234).
publication(systems,title339 , person234).
publication(systems,title69 , person234).
publication(systems,title249 , person234).
publication(systems,title76 , person234).
publication(systems,title81 , person234).
publication(systems,title285 , person234).
publication(systems,title101 , person234).
publication(systems,title223 , person101).
publication(systems,title56 , person101).
publication(systems,title294 , person101).
publication(systems,title214 , person101).
publication(systems,title157 , person101).
publication(systems,title68 , person101).
publication(systems,title197 , person101).
publication(systems,title139 , person101).
publication(systems,title141 , person101).
publication(systems,title131 , person101).
publication(systems,title2 , person101).
publication(systems,title75 , person101).
publication(systems,title174 , person101).
publication(systems,title148 , person101).
publication(systems,title8 , person101).
publication(systems,title282 , person101).
publication(systems,title52 , person101).
publication(systems,title31 , person101).
publication(systems,title133 , person101).
publication(systems,title245 , person101).
publication(systems,title67 , person101).
publication(systems,title173 , person180).
publication(systems,title223 , person98).
publication(systems,title197 , person98).
publication(systems,title139 , person98).
publication(systems,title2 , person98).
publication(systems,title174 , person98).
publication(systems,title148 , person98).
publication(systems,title8 , person98).
publication(systems,title244 , person107).
publication(systems,title300 , person107).
publication(systems,title124 , person107).
publication(systems,title96 , person107).
publication(systems,title176 , person107).
publication(systems,title198 , person107).
publication(systems,title303 , person107).
publication(systems,title209 , person107).
publication(systems,title320 , person107).
publication(systems,title169 , person107).
publication(systems,title17 , person107).
publication(systems,title264 , person107).
publication(systems,title294 , person235).
publication(systems,title108 , person235).
publication(systems,title141 , person235).
publication(systems,title42 , person235).
publication(systems,title338 , person235).
publication(systems,title51 , person235).
publication(systems,title37 , person235).
publication(systems,title281 , person235).
publication(systems,title315 , person82).
publication(systems,title196 , person82).
publication(systems,title221 , person82).
publication(systems,title232 , person82).
publication(systems,title9 , person82).
publication(systems,title32 , person82).
publication(systems,title251 , person82).
publication(systems,title211 , person82).
publication(systems,title23 , person82).
publication(systems,title204 , person82).
publication(systems,title121 , person82).
publication(systems,title332 , person82).
publication(systems,title144 , person82).
publication(systems,title163 , person82).
publication(systems,title306 , person82).
publication(systems,title80 , person82).
publication(systems,title234 , person82).
publication(systems,title256 , person82).
publication(systems,title61 , person82).
publication(systems,title343 , person82).
publication(systems,title187 , person82).
publication(systems,title249 , person82).
publication(systems,title6 , person82).
publication(systems,title76 , person82).
publication(systems,title299 , person82).
publication(systems,title34 , person82).
publication(systems,title280 , person82).
publication(systems,title36 , person82).
publication(systems,title81 , person82).
publication(systems,title146 , person373).
publication(systems,title186 , person104).
publication(systems,title277 , person104).
publication(systems,title180 , person104).
publication(systems,title141 , person104).
publication(systems,title246 , person104).
publication(systems,title227 , person104).
publication(systems,title48 , person104).
publication(systems,title267 , person104).
publication(systems,title149 , person104).
publication(systems,title253 , person290).
publication(systems,title221 , person290).
publication(systems,title232 , person290).
publication(systems,title9 , person290).
publication(systems,title32 , person290).
publication(systems,title251 , person290).
publication(systems,title211 , person290).
publication(systems,title23 , person290).
publication(systems,title163 , person290).
publication(systems,title306 , person290).
publication(systems,title256 , person290).
publication(systems,title61 , person290).
publication(systems,title343 , person290).
publication(systems,title187 , person290).
publication(systems,title6 , person290).
publication(systems,title299 , person290).
publication(systems,title34 , person290).
publication(systems,title280 , person290).
publication(systems,title36 , person290).
publication(systems,title294 , person255).
publication(systems,title141 , person255).
publication(systems,title146 , person411).
publication(systems,title186 , person99).
publication(systems,title180 , person99).
publication(systems,title286 , person99).
publication(systems,title173 , person212).
publication(systems,title96 , person403).
publication(systems,title46 , person403).
publication(systems,title53 , person403).
publication(systems,title58 , person403).
publication(systems,title289 , person403).
publication(systems,title43 , person403).
publication(systems,title169 , person403).
publication(systems,title7 , person403).
publication(systems,title339 , person403).
publication(systems,title108 , person402).
publication(systems,title42 , person402).
publication(systems,title56 , person253).
publication(systems,title214 , person253).
publication(systems,title157 , person253).
publication(systems,title68 , person253).
publication(systems,title131 , person253).
publication(systems,title75 , person253).
publication(systems,title282 , person253).
publication(systems,title245 , person253).
publication(systems,title206 , person92).
publication(systems,title242 , person92).
publication(systems,title31 , person419).
publication(systems,title64 , person419).
publication(systems,title143 , person419).
publication(systems,title294 , person357).
publication(systems,title141 , person357).
publication(systems,title262 , person89).
publication(systems,title206 , person89).
publication(systems,title242 , person89).
publication(systems,title206 , person15).
publication(systems,title168 , person80).
publication(systems,title300 , person80).
publication(systems,title96 , person80).
publication(systems,title189 , person80).
publication(systems,title46 , person80).
publication(systems,title28 , person80).
publication(systems,title176 , person80).
publication(systems,title65 , person80).
publication(systems,title237 , person80).
publication(systems,title247 , person80).
publication(systems,title342 , person80).
publication(systems,title169 , person80).
publication(systems,title17 , person80).
publication(systems,title339 , person80).
publication(systems,title285 , person80).
publication(systems,title101 , person80).
publication(systems,title244 , person376).
publication(systems,title124 , person376).
publication(systems,title303 , person376).
publication(systems,title320 , person376).
publication(systems,title264 , person376).
publication(systems,title52 , person218).
publication(systems,title67 , person218).
publication(systems,title37 , person154).
publication(systems,title281 , person154).
publication(systems,title186 , person204).
publication(systems,title277 , person204).
publication(systems,title3 , person204).
publication(systems,title286 , person204).
publication(systems,title149 , person204).
publication(systems,title69 , person204).
publication(systems,title294 , person126).
publication(systems,title141 , person126).
publication(systems,title262 , person100).
publication(systems,title37 , person100).
publication(systems,title281 , person100).
taughtby(theory,course11, person57, autumn_0001).
taughtby(theory,course147, person201, autumn_0001).
taughtby(theory,course77, person165, autumn_0001).
taughtby(theory,course160, person331, autumn_0001).
taughtby(theory,course66, person298, autumn_0001).
taughtby(theory,course11, person298, winter_0001).
taughtby(theory,course147, person165, winter_0001).
taughtby(theory,course165, person364, winter_0001).
taughtby(theory,course161, person201, winter_0001).
taughtby(theory,course68, person331, winter_0001).
taughtby(theory,course29, person298, winter_0001).
taughtby(theory,course11, person331, spring_0001).
taughtby(theory,course147, person57, spring_0001).
taughtby(theory,course27, person165, spring_0001).
taughtby(theory,course40, person378, spring_0001).
taughtby(theory,course165, person231, autumn_0102).
taughtby(theory,course104, person364, autumn_0102).
taughtby(theory,course103, person201, autumn_0102).
taughtby(theory,course77, person324, autumn_0102).
taughtby(theory,course66, person165, autumn_0102).
taughtby(theory,course147, person324, winter_0102).
taughtby(theory,course161, person298, winter_0102).
taughtby(theory,course126, person165, winter_0102).
taughtby(theory,course68, person201, winter_0102).
taughtby(theory,course11, person324, spring_0102).
taughtby(theory,course147, person364, spring_0102).
taughtby(theory,course165, person141, spring_0102).
taughtby(theory,course27, person165, spring_0102).
taughtby(theory,course40, person298, spring_0102).
taughtby(theory,course104, person165, autumn_0203).
taughtby(theory,course68, person331, autumn_0203).
taughtby(theory,course77, person52, autumn_0203).
taughtby(theory,course147, person165, winter_0203).
taughtby(theory,course161, person331, winter_0203).
taughtby(theory,course119, person324, winter_0203).
taughtby(theory,course121, person52, winter_0203).
taughtby(theory,course11, person324, spring_0203).
taughtby(theory,course147, person52, spring_0203).
taughtby(theory,course27, person331, spring_0203).
taughtby(theory,course40, person165, spring_0203).
taughtby(theory,course117, person181, spring_0203).
taughtby(theory,course11, person298, autumn_0304).
taughtby(theory,course147, person165, autumn_0304).
taughtby(theory,course165, person75, autumn_0304).
taughtby(theory,course104, person181, autumn_0304).
taughtby(theory,course103, person201, autumn_0304).
taughtby(theory,course77, person52, autumn_0304).
taughtby(theory,course144, person331, autumn_0304).
taughtby(theory,course147, person331, winter_0304).
taughtby(theory,course165, person181, winter_0304).
taughtby(theory,course161, person201, winter_0304).
taughtby(theory,course126, person165, winter_0304).
taughtby(theory,course68, person324, winter_0304).
taughtby(theory,course40, person298, winter_0304).
taughtby(theory,course11, person52, spring_0304).
taughtby(theory,course27, person165, spring_0304).
taughtby(theory,course97, person324, spring_0304).
taughtby(theory,course91, person331, spring_0304).
courselevel(theory,course11, level_300).
courselevel(theory,course147, level_300).
courselevel(theory,course165, level_300).
courselevel(theory,course104, level_300).
courselevel(theory,course68, level_400).
courselevel(theory,course161, level_400).
courselevel(theory,course27, level_400).
courselevel(theory,course137, level_400).
courselevel(theory,course126, level_400).
courselevel(theory,course97, level_400).
courselevel(theory,course122, level_400).
courselevel(theory,course40, level_500).
courselevel(theory,course29, level_500).
courselevel(theory,course103, level_500).
courselevel(theory,course77, level_500).
courselevel(theory,course91, level_500).
courselevel(theory,course160, level_500).
courselevel(theory,course155, level_500).
courselevel(theory,course66, level_500).
courselevel(theory,course169, level_500).
courselevel(theory,course119, level_500).
courselevel(theory,course84, level_500).
courselevel(theory,course121, level_500).
courselevel(theory,course98, level_500).
courselevel(theory,course117, level_500).
courselevel(theory,course36, level_500).
courselevel(theory,course144, level_500).
courselevel(theory,course149, level_500).
hasposition(theory,person378, faculty).
hasposition(theory,person331, faculty).
hasposition(theory,person103, faculty_affiliate).
hasposition(theory,person52, faculty).
hasposition(theory,person298, faculty).
hasposition(theory,person165, faculty).
hasposition(theory,person29, faculty_adjunct).
hasposition(theory,person201, faculty).
hasposition(theory,person324, faculty).
projectmember(theory,project130, person324).
projectmember(theory,project119, person201).
projectmember(theory,project152, person201).
projectmember(theory,project94, person324).
inphase(theory,person309, post_quals).
inphase(theory,person141, post_generals).
inphase(theory,person383, pre_quals).
inphase(theory,person422, post_quals).
inphase(theory,person390, pre_quals).
inphase(theory,person288, post_generals).
inphase(theory,person159, post_quals).
inphase(theory,person172, pre_quals).
inphase(theory,person226, post_quals).
inphase(theory,person242, post_generals).
inphase(theory,person191, post_quals).
inphase(theory,person416, pre_quals).
inphase(theory,person348, post_quals).
inphase(theory,person278, pre_quals).
inphase(theory,person6, post_quals).
inphase(theory,person75, post_generals).
inphase(theory,person303, post_quals).
inphase(theory,person249, post_generals).
inphase(theory,person68, post_generals).
inphase(theory,person205, pre_quals).
inphase(theory,person182, post_quals).
tempadvisedby(theory,person383, person165).
tempadvisedby(theory,person390, person331).
tempadvisedby(theory,person172, person331).
tempadvisedby(theory,person191, person298).
tempadvisedby(theory,person416, person52).
tempadvisedby(theory,person278, person378).
tempadvisedby(theory,person205, person324).
tempadvisedby(theory,person182, person201).
yearsinprogram(theory,person309, year_3).
yearsinprogram(theory,person141, year_6).
yearsinprogram(theory,person383, year_2).
yearsinprogram(theory,person422, year_3).
yearsinprogram(theory,person390, year_2).
yearsinprogram(theory,person288, year_5).
yearsinprogram(theory,person159, year_2).
yearsinprogram(theory,person172, year_1).
yearsinprogram(theory,person226, year_4).
yearsinprogram(theory,person242, year_5).
yearsinprogram(theory,person191, year_4).
yearsinprogram(theory,person416, year_1).
yearsinprogram(theory,person348, year_3).
yearsinprogram(theory,person278, year_2).
yearsinprogram(theory,person6, year_2).
yearsinprogram(theory,person75, year_6).
yearsinprogram(theory,person303, year_4).
yearsinprogram(theory,person249, year_7).
yearsinprogram(theory,person68, year_5).
yearsinprogram(theory,person205, year_1).
yearsinprogram(theory,person182, year_3).
ta(theory,course147, person23, winter_0304).
ta(theory,course165, person141, winter_0304).
ta(theory,course104, person424, winter_0304).
ta(theory,course68, person416, winter_0304).
ta(theory,course161, person191, winter_0304).
ta(theory,course137, person383, winter_0304).
ta(theory,course40, person390, winter_0304).
ta(theory,course11, person205, autumn_0304).
ta(theory,course11, person172, autumn_0304).
ta(theory,course147, person310, autumn_0304).
ta(theory,course165, person416, autumn_0304).
ta(theory,course104, person401, autumn_0304).
ta(theory,course103, person182, autumn_0304).
ta(theory,course149, person390, autumn_0304).
ta(theory,course11, person58, spring_0203).
ta(theory,course11, person144, spring_0203).
ta(theory,course147, person390, spring_0203).
ta(theory,course147, person310, spring_0203).
ta(theory,course27, person249, spring_0203).
ta(theory,course40, person303, spring_0203).
ta(theory,course36, person278, spring_0203).
ta(theory,course147, person125, winter_0203).
ta(theory,course147, person6, winter_0203).
ta(theory,course165, person58, winter_0203).
ta(theory,course165, person422, winter_0203).
ta(theory,course104, person237, winter_0203).
ta(theory,course161, person390, winter_0203).
ta(theory,course161, person350, winter_0203).
ta(theory,course84, person141, winter_0203).
ta(theory,course11, person125, autumn_0203).
ta(theory,course11, person390, autumn_0203).
ta(theory,course11, person310, autumn_0203).
ta(theory,course165, person191, autumn_0203).
ta(theory,course104, person278, autumn_0203).
ta(theory,course104, person237, autumn_0203).
ta(theory,course68, person356, autumn_0203).
ta(theory,course155, person226, autumn_0203).
ta(theory,course11, person422, spring_0102).
ta(theory,course147, person309, spring_0102).
ta(theory,course147, person356, spring_0102).
ta(theory,course165, person294, spring_0102).
ta(theory,course104, person191, spring_0102).
ta(theory,course104, person182, spring_0102).
ta(theory,course27, person75, spring_0102).
ta(theory,course40, person303, spring_0102).
ta(theory,course147, person75, winter_0102).
ta(theory,course147, person356, winter_0102).
ta(theory,course165, person315, winter_0102).
ta(theory,course68, person191, winter_0102).
ta(theory,course68, person309, winter_0102).
ta(theory,course161, person249, winter_0102).
ta(theory,course137, person288, winter_0102).
ta(theory,course98, person303, winter_0102).
taughtby(theory,course137 , person165, winter_0304).
taughtby(theory,course122 , person378, autumn_0304).
taughtby(theory,course149 , person331, autumn_0304).
taughtby(theory,course144, person278, summer_0203).
taughtby(theory,course122 , person378, spring_0203).
taughtby(theory,course36, person181, spring_0203).
taughtby(theory,course84, person324, winter_0203).
taughtby(theory,course137 , person165, winter_0102).
taughtby(theory,course98 , person103, winter_0102).
professor(theory,person378).
professor(theory,person298).
professor(theory,person52).
professor(theory,person57).
professor(theory,person231).
professor(theory,person181).
professor(theory,person364).
student(theory,person191).
student(theory,person397).
student(theory,person138).
student(theory,person303).
student(theory,person77).
student(theory,person141).
professor(theory,person331).
professor(theory,person103).
professor(theory,person165).
professor(theory,person29).
professor(theory,person201).
professor(theory,person324).
student(theory,person309).
student(theory,person383).
student(theory,person422).
student(theory,person390).
student(theory,person288).
student(theory,person159).
student(theory,person172).
student(theory,person226).
student(theory,person242).
student(theory,person416).
student(theory,person348).
student(theory,person278).
student(theory,person6).
student(theory,person75).
student(theory,person249).
student(theory,person68).
student(theory,person205).
student(theory,person182).
student(theory,person23).
student(theory,person310).
student(theory,person424).
student(theory,person401).
student(theory,person237).
student(theory,person58).
student(theory,person144).
student(theory,person125).
student(theory,person350).
student(theory,person356).
student(theory,person294).
student(theory,person315).
sameperson(theory,person378, person378).
sameperson(theory,person298, person298).
sameperson(theory,person52, person52).
sameperson(theory,person57, person57).
sameperson(theory,person231, person231).
sameperson(theory,person181, person181).
sameperson(theory,person364, person364).
sameperson(theory,person191, person191).
sameperson(theory,person397, person397).
sameperson(theory,person138, person138).
sameperson(theory,person303, person303).
sameperson(theory,person77, person77).
sameperson(theory,person141, person141).
sameperson(theory,person331, person331).
sameperson(theory,person103, person103).
sameperson(theory,person165, person165).
sameperson(theory,person29, person29).
sameperson(theory,person201, person201).
sameperson(theory,person324, person324).
sameperson(theory,person309, person309).
sameperson(theory,person383, person383).
sameperson(theory,person422, person422).
sameperson(theory,person390, person390).
sameperson(theory,person288, person288).
sameperson(theory,person159, person159).
sameperson(theory,person172, person172).
sameperson(theory,person226, person226).
sameperson(theory,person242, person242).
sameperson(theory,person416, person416).
sameperson(theory,person348, person348).
sameperson(theory,person278, person278).
sameperson(theory,person6, person6).
sameperson(theory,person75, person75).
sameperson(theory,person249, person249).
sameperson(theory,person68, person68).
sameperson(theory,person205, person205).
sameperson(theory,person182, person182).
sameperson(theory,person23, person23).
sameperson(theory,person310, person310).
sameperson(theory,person424, person424).
sameperson(theory,person401, person401).
sameperson(theory,person237, person237).
sameperson(theory,person58, person58).
sameperson(theory,person144, person144).
sameperson(theory,person125, person125).
sameperson(theory,person350, person350).
sameperson(theory,person356, person356).
sameperson(theory,person294, person294).
sameperson(theory,person315, person315).
samecourse(theory,course144, course144).
samecourse(theory,course165, course165).
samecourse(theory,course11, course11).
samecourse(theory,course147, course147).
samecourse(theory,course104, course104).
samecourse(theory,course68, course68).
samecourse(theory,course161, course161).
samecourse(theory,course27, course27).
samecourse(theory,course126, course126).
samecourse(theory,course97, course97).
samecourse(theory,course40, course40).
samecourse(theory,course29, course29).
samecourse(theory,course103, course103).
samecourse(theory,course77, course77).
samecourse(theory,course91, course91).
samecourse(theory,course160, course160).
samecourse(theory,course155, course155).
samecourse(theory,course66, course66).
samecourse(theory,course119, course119).
samecourse(theory,course121, course121).
samecourse(theory,course117, course117).
samecourse(theory,course36, course36).
samecourse(theory,course169, course169).
samecourse(theory,course84, course84).
samecourse(theory,course137, course137).
samecourse(theory,course122, course122).
samecourse(theory,course98, course98).
samecourse(theory,course149, course149).
sameproject(theory,project12, project12).
sameproject(theory,project1, project1).
sameproject(theory,project119, project119).
sameproject(theory,project63, project63).
sameproject(theory,project5, project5).
sameproject(theory,project152, project152).
sameproject(theory,project18, project18).
sameproject(theory,project56, project56).
sameproject(theory,project130, project130).
sameproject(theory,project61, project61).
sameproject(theory,project126, project126).
sameproject(theory,project94, project94).
sameproject(theory,project57, project57).
sameproject(theory,project22, project22).
sameproject(theory,project21, project21).
sameproject(theory,project64, project64).
publication(theory,title164 , person378).
publication(theory,title202 , person378).
publication(theory,title152 , person378).
publication(theory,title154 , person378).
publication(theory,title334 , person378).
publication(theory,title193 , person378).
publication(theory,title326 , person378).
publication(theory,title328 , person378).
publication(theory,title327 , person378).
publication(theory,title308 , person378).
publication(theory,title136 , person378).
publication(theory,title243 , person378).
publication(theory,title127 , person378).
publication(theory,title317 , person298).
publication(theory,title18 , person298).
publication(theory,title126 , person298).
publication(theory,title309 , person298).
publication(theory,title128 , person298).
publication(theory,title77 , person298).
publication(theory,title216 , person298).
publication(theory,title235 , person298).
publication(theory,title311 , person298).
publication(theory,title298 , person298).
publication(theory,title326 , person397).
publication(theory,title62 , person138).
publication(theory,title210 , person138).
publication(theory,title287 , person138).
publication(theory,title317 , person77).
publication(theory,title18 , person77).
publication(theory,title126 , person77).
publication(theory,title309 , person77).
publication(theory,title128 , person77).
publication(theory,title77 , person77).
publication(theory,title216 , person77).
publication(theory,title287 , person77).
publication(theory,title235 , person77).
publication(theory,title311 , person77).
publication(theory,title298 , person77).
publication(theory,title164 , person331).
publication(theory,title202 , person331).
publication(theory,title95 , person331).
publication(theory,title152 , person331).
publication(theory,title154 , person331).
publication(theory,title158 , person331).
publication(theory,title73 , person331).
publication(theory,title19 , person331).
publication(theory,title159 , person331).
publication(theory,title334 , person331).
publication(theory,title276 , person331).
publication(theory,title328 , person331).
publication(theory,title327 , person331).
publication(theory,title308 , person331).
publication(theory,title136 , person331).
publication(theory,title243 , person331).
publication(theory,title138 , person331).
publication(theory,title127 , person331).
publication(theory,title20 , person331).
publication(theory,title21 , person331).
publication(theory,title27 , person331).
publication(theory,title105 , person331).
publication(theory,title330 , person165).
publication(theory,title200 , person165).
publication(theory,title129 , person165).
publication(theory,title155 , person165).
publication(theory,title104 , person165).
publication(theory,title324 , person165).
publication(theory,title215 , person165).
publication(theory,title205 , person165).
publication(theory,title291 , person165).
publication(theory,title321 , person165).
publication(theory,title175 , person165).
publication(theory,title307 , person165).
publication(theory,title193 , person165).
publication(theory,title184 , person165).
publication(theory,title138 , person165).
publication(theory,title330 , person29).
publication(theory,title200 , person29).
publication(theory,title129 , person29).
publication(theory,title155 , person29).
publication(theory,title104 , person29).
publication(theory,title324 , person29).
publication(theory,title215 , person29).
publication(theory,title205 , person29).
publication(theory,title279 , person29).
publication(theory,title291 , person29).
publication(theory,title66 , person29).
publication(theory,title321 , person29).
publication(theory,title175 , person29).
publication(theory,title307 , person29).
publication(theory,title184 , person29).
publication(theory,title62 , person324).
publication(theory,title158 , person324).
publication(theory,title19 , person324).
publication(theory,title210 , person324).
publication(theory,title21 , person324).
publication(theory,title27 , person324).
publication(theory,title105 , person324).
publication(theory,title330 , person242).
publication(theory,title104 , person242).
publication(theory,title215 , person242).
publication(theory,title205 , person242).
publication(theory,title279 , person242).
publication(theory,title291 , person242).
publication(theory,title66 , person242).
publication(theory,title321 , person242).
publication(theory,title175 , person242).
publication(theory,title287 , person242).
publication(theory,title159 , person75).
publication(theory,title20 , person75).
publication(theory,title95 , person249).
publication(theory,title73 , person249).
publication(theory,title276 , person249).
