%%%%%%%%%%%%%%TYPE AND RELATION HIERARCHY%%%%%%%%%%%%%%%%%%%
isa(block,entity).
isa(hand,entity).
isa(location,entity).
isa(pickup,act).
isa(putdown,act).
isa(move,act).

isa_rel(at,relation).
isa_rel(above,realtion).
isa_rel(ontable,relation).
isa_rel(top,relation).
isa_rel(emptyhand,relation).

%%%%%%%%%LEVEL TWO%%%%%%%%%%%%%
:-use_module(library(lists)).
:-multifile(cg/4).
:-multifile(cgc/5).
:-multifile(isa/2).
:-multifile(ind/3).
:-multifile(kb/6).
:-dynamic kb/6.



cgc(41,simple,block,[fs(num,sing)],[]).
cgc(42,simple,color,[fs(num,sing)],[]).
cgc(43,simple,shape,[fs(num,sing)],[]).
cg(400,[cgr(attr,[41,42],_),cgr(chrc,[41,43],_)],[],[fs(kind,body_of_context),fs(comment,'Each block has a color and shape')]).
cgc(44,complex,entity,[400],[fs(name,'block')]). %n_2_g_1
/*cgc(Id,complex,type_label,[CG],[fs(FN,FV),,,]).*/

cgc(45,simple,location,[fs(num,sing)],[]).
cgc(46,simple,hand,[fs(num,sing)],[]).
cgc(48,simple,putdown,[],[]).
cg(401,[cgr(top,[41,45],_)],[],[fs(kind,body_of_context),fs(comment,'Block on top of location')]).
cgc(47,complex,situation,[401],[]).
cg(402,[cgr(ptnt,[48,41],_),cgr(dest,[48,45],_),cgr(inst,[48,46],_),
cgr(rslt,[48,47],_)],[],[fs(kind,body_of_context),fs(comment,'Each block is put down at a location from the hand')]).
cgc(49,complex,act,[402],[fs(name,'putdown')]). %n_2_g_3

cgc(40,simple,movehand,[],[]).
cg(403,[cgr(at,[46,45],_)],[],[fs(kind,body_of_context),fs(comment,'Hand at location')]).
cgc(51,complex,situation,[403],[]).
cg(404,[cgr(dest,[40,45],_),cgr(inst,[40,46],_),cgr(rslt,[40,51],_)],[],[fs(kind,body_of_context),fs(comment,'This action moves the hand to a location')]).
cgc(52,complex,act,[404],[fs(name,'movehand')]). %n_2_g_4

cg(405,[cgr(grasp,[46,41],_)],[],[fs(kind,body_of_context),fs(comment,'Hand grasp block')]).
cgc(54,complex,situation,[405],[]).
cgc(55,simple,pickup,[],[]).
cg(406,[cgr(ptnt,[55,41],_),cgr(inst,[55,46],_),cgr(rslt,[55,54],_)],[],[fs(kind,body_of_context),fs(comment,'Each block is picked up using a hand')]).
cgc(56,complex,act,[406],[fs(name,'pickup')]). %n_2_g_2

cg(407,[cgr(at,[41,45],_)],[],[fs(kind,body_of_context),fs(comment,'Block at location')]).
cgc(57,complex,situation,[407],[]).
cgc(58,simple,moveblock,[],[]).
cg(408,[cgr(dest,[58,45],_),cgr(ptnt,[58,41],_),cgr(inst,[58,46],_),cgr(rslt,[58,57],_)],[],
[fs(kind,body_of_context),fs(comment,'This action moves the block to a location')]).
cgc(59,complex,act,[408],[fs(name,'moveblock')]). %n_2_g_5

cgc(60,simple,entity,[fs(num,sing)],[]).
cg(409,[cgr(pos,[60,45],_)],[],[fs(kind,body_of_context),fs(comment,'')]).
cgc(61,complex,relation,[409],[fs(name,'at')]). %n_2_g_6

cgc(62,simple,block,[fs(num,sing)],[]).
cg(410,[cgr(above,[62,41],_)],[],[fs(kind,body_of_context),fs(comment,'Block above block')]).
cgc(63,complex,situation,[410],[]).
cgc(64,simple,top,[],[]).
cg(411,[cgr(above,[41,45],_),cgr(not,[63],_)],[],[fs(kind,body_of_context),fs(comment,'A block on top is at a location and has no blocks above it')]).
cgc(65,complex,relation,[411],[fs(name,'top')]). %n_2_g_7

cg(412,[cgr(grasp,[46,41],_)],[],[fs(kind,body_of_context),fs(comment,'Hand grasp block')]).
cgc(66,complex,situation,[412],[]).
cg(413,[cgr(not,[66],_)],[],[fs(kind,body_of_context),fs(comment,'A block is empty when no blocks are in it')]).
cgc(67,complex,relation,[413],[fs(name,'emptyhand')]). %n_2_g_8

cg(414,[cgr,(at,[41,45],_),cgr(not,[66],_)],[],[fs(kind,body_fo_context),fs(comment,'A block on the table is at a location and not in the hand')]).
cgc(68,complex,relation,[414],[fs(name,'ontable')]). %n_2_9

cg(415,[cgr(ontable,[41,45],_),cgr(ontable,[62,45],_)],[],[fs(kind,body_of_context),fs(comment,'The first block is above the second block at the same location')]).
cgc(69,complex,relation,[415],[fs(name,'above')]). %n_2_g_10


cgc(70,simple,block,[fs(num,sing),fs(type,def),fs(refID,1)],[]).%n_2_g_11
cgc(71,simple,color,[fs(num,sing),fs(name,'red')],[]).
cg(416,[cgr(attr,[70,71],_)],[],[fs(kind,normal),fs(comment,'Block #1 is red')]).%n_2_g_18

cgc(73,simple,block,[fs(num,sing),fs(type,def),fs(refID,2)],[]).%n_2_g_12
cgc(74,simple,color,[fs(num,sing),fs(name,'blue')],[]).
cg(417,[cgr(attr,[73,74],_)],[],[fs(kind,normal),fs(comment,'Block #2 is blue')]).%n_2_g_19


cgc(75,simple,block,[fs(num,sing),fs(type,def),fs(refID,3)],[]).%n_2_g_13
cgc(76,simple,color,[fs(num,sing),fs(name,'green')],[]).
cg(418,[cgr(attr,[75,76],_)],[],[fs(kind,normal),fs(comment,'Block #3 is green')]).%n_2_g_19

cgc(77,simple,location,[fs(num,sing),fs(type,def),fs(refID,5)],[]).%n_2_g_15
cg(419,[cgr(ontable,[70,77],_)],[],[fs(kind,normal),fs(comment,'')]).%n_2_g_20


cgc(78,simple,location,[fs(num,sing),fs(type,def),fs(refID,6)],[]).%n_2_g_16
cg(420,[cgr(ontable,[73,78],_)],[],[fs(kind,normal),fs(comment,'')]).%n_2_g_21


cg(421,[cgr(ontable,[75,77],_)],[],[fs(kind,normal),fs(comment,'')]).%n_2_g_22

cg(422,[cgr(above,[70,75],_)],[],[fs(kind,normal),fs(comment,'Block 1 above block 3')]).%n_2_g_23

cgc(79,simple,hand,[fs(num,sing),fs(refID,4)],[]).%n_2_g_14
cg(423,[cgr(emptyhand,[79],_)],[],[fs(kind,normal),fs(comment,'Emptyhand 4')]).%n_2_g_24

cgc(80,simple,block,[fs(num,plur),fs(type,meas),fs(quant,3)],[]).%n_2_g_17
/*[Block:@3]*/

cg(424,[cgr(attr,[70,74],_)],[],[fs(kind,body_of_context),fs(comment,'Block 1 has blue color')]).
cgc(81,complex,or,[424],[]).

cg(425,[cgr(attr,[73,74],_)],[],[fs(kind,body_of_context),fs(comment,'Block 2 has blue color')]).
cgc(82,complex,or,[425],[]).

cgc(83,complex,either,[424,425],[]).%n_2_g_25

cgc(84,simple,block,[fs(num,sing),fs(quant,every)],[]).
cgc(85,simple,shape,[fs(num,sing),fs(name,'cubical')],[]).
cg(427,[cgr(char,[84,85],_)],[],[fs(kind,normal),fs(comment,'All blocks are cubical')]).%n_2_g_26

find_th(TH):-findall(isa(X,Y),isa(X,Y),TH).
find_td_gr(TD):-findall(Id,(cg(Id,_,_,F),member(fs(kind,typedef),F)),TD).

find_rh(RH):-findall(isa_rel(X,Y),isa(X,Y),RH).
find_rd_gr(TR):-findall(Id,(cg(Id,_,_,F),member(fs(kind,reldef),F)),TR).

find_Ind(Ind):-findall(ind(X,Y,Z),ind(X,Y,Z),Ind).

find_Ass(Ass):-	findall(GrID,(cg(GrID,_,_,F),member(fs(kind,assertion),F)),A1),
	findall(CID,(cgc(CID,_,_,_,F),member(fs(kind,assertion),F)),A2),
	append(A1,A2,Ass).
kb:-find_th(TH),find_td_gr(TD),find_rh(RH),find_rd_gr(RD),find_Ind(Ind),
	find_Ass(Ass),assert(kb(TH,TD,RH,RD,Ind,Ass)).
:-kb.



