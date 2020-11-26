:-use_module(library(lists)).
:-multifile(cg/4).
:-multifile(cgc/5).
:-multifile(isa/2).
:-multifile(ind/3).
:-multifile(kb/6).
:-dynamic kb/6.

%%%%%LEVEL ONE%%%%%%%%%%%%%%%%
%sled vseki graf ot nivoto slagam komentar n_nomernivo_g_nomergraph
cgc(1,simple,block,[fs(num,sing)],[]).
cgc(2,simple,color,[fs(num,sing)],[]).
cgc(3,simple,shape,[fs(num,sing)],[]).
cg(100,[cgr(attr,[1,2],_),cgr(chrc,[1,3],_)],[],[fs(kind,body_of_context),fs(comment,'Each block has a color and shape')]).
cgc(4,complex,entity,[100],[fs(name,'block')]). %n_1_g_1
/*cgc(Id,complex,type_label,[CG],[fs(FN,FV),,,]).*/

cgc(5,simple,location,[fs(num,sing)],[]).
cgc(6,simple,hand,[fs(num,sing)],[]).
cgc(8,simple,putdown,[],[]).
cg(101,[cgr(top,[1,5],_)],[],[fs(kind,body_of_context),fs(comment,'Block on top of location')]).
cgc(7,complex,situation,[101],[]).
cg(102,[cgr(ptnt,[8,1],_),cgr(dest,[8,5],_),cgr(inst,[8,6],_),
cgr(rslt,[8,7],_)],[],[fs(kind,body_of_context),fs(comment,'Each block is put down at a location from the hand')]).
cgc(9,complex,act,[102],[fs(name,'putdown')]). %n_1_g_3

cgc(10,simple,movehand,[],[]).
cg(103,[cgr(at,[6,5],_)],[],[fs(kind,body_of_context),fs(comment,'This action moves the hand to a location')]).
cgc(11,complex,situation,[103],[]).
cg(104,[cgr(dest,[10,5],_),cgr(inst,[10,6],_),cgr(rslt,[10,11],_)],[],[fs(kind,body_of_context),fs(comment,'')]).
cgc(12,complex,act,[104],[fs(name,'movehand')]). %n_1_g_4

cg(105,[cgr(grasp,[6,1],_)],[],[fs(kind,body_of_context),fs(comment,'Hand grasp block')]).
cgc(14,complex,situation,[105],[]).
cgc(15,simple,pickup,[],[]).
cg(106,[cgr(ptnt,[15,1],_),cgr(inst,[15,6],_),cgr(rslt,[15,14],_)],[],[fs(kind,body_of_context),fs(comment,'Each block is picked up using a hand
')]).
cgc(16,complex,act,[106],[fs(name,'pickup')]). %n_1_g_2

cg(107,[cgr(at,[1,5],_)],[],[fs(kind,body_of_context),fs(comment,'Block at location')]).
cgc(17,complex,situation,[107],[]).
cgc(18,simple,moveblock,[],[]).
cg(108,[cgr(dest,[18,5],_),cgr(ptnt,[18,1],_),cgr(inst,[18,6],_),cgr(rslt,[18,17],_)],[],
[fs(kind,body_of_context),fs(comment,'This action moves the block to a location
')]).
cgc(19,complex,act,[108],[fs(name,'moveblock')]). %n_1_g_5

cgc(20,simple,entity,[fs(num,sing)],[]).
cg(109,[cgr(pos,[20,5],_)],[],[fs(kind,body_of_context),fs(comment,'')]).
cgc(21,complex,relation,[109],[fs(name,'at')]). %n_1_g_6

cgc(22,simple,block,[fs(num,sing)],[]).
cg(110,[cgr(above,[22,1],_)],[],[fs(kind,body_of_context),fs(comment,'A block on top is at a location and has no blocks above it')]).
cgc(23,complex,situation,[110],[]).
cgc(24,simple,top,[],[]).
cg(111,[cgr(ontable,[1,5],_),cgr(not,[23],_)],[],[fs(kind,body_of_context),fs(comment,'')]).
cgc(25,complex,relation,[111],[fs(name,'top')]). %n_1_g_7

cg(112,[cgr(grasp,[6,1],_)],[],[fs(kind,body_of_context),fs(comment,'Hand grasp block')]).
cgc(26,complex,situation,[112],[]).
cg(113,[cgr(not,[26],_)],[],[fs(kind,body_of_context),fs(comment,'A block is empty when no blocks are in it')]).
cgc(27,complex,relation,[113],[fs(name,'emptyhand')]). %n_1_g_8

cg(114,[cgr,(at,[1,5],_),cgr(not,[26],_)],[],[fs(kind,body_fo_context),fs(comment,'A block on the table is at a location and not in the hand')]).
cgc(28,complex,relation,[114],[fs(name,'ontable')]). %n_1_g_9

cg(115,[cgr(ontable,[1,5],_),cgr(ontable,[22,5],_)],[],[fs(kind,body_of_context),fs(comment,'The first block is above the second block at the same location
')]).
cgc(29,complex,relation,[115],[fs(name,'above')]). %n_1_g_10

cgc(30,simple,block,[fs(num,sing),fs(type,def),fs(refID,1)],[]).
/*tuk kato ima fs(type,def) moje da ti izpisva # i N fs(refID,N)t.e Block:#1*/
cgc(31,simple,color,[fs(num,sing),fs(name,'red')],[]).
cgc(32,simple,shape,[fs(num,sing),fs(name,'cubical')],[]).
cg(116,[cgr(attr,[30,31],_),cgr(chrc,[30,32],_)],[],[fs(kind,normal),fs(comment,'Block #1 is red and cubical')]).%n_1_g_11

cgc(33,simple,block,[fs(num,sing),fs(type,def),fs(refID,2)],[]).
cgc(34,simple,color,[fs(num,sing),fs(name,'blue')],[]).
cg(117,[cgr(attr,[33,34],_),cgr(chrc,[33,32],_)],[],[fs(kind,normal),fs(comment,'Block #2 is Blue and cubical')]).%n_1_g_12


cgc(35,simple,block,[fs(num,sing),fs(type,def),fs(refID,3)],[]).
cgc(36,simple,color,[fs(num,sing),fs(name,'green')],[]).
cg(118,[cgr(attr,[35,36],_),cgr(chrc,[35,32],_)],[],[fs(kind,normal),fs(comment,'Block #3 is green and cubical')]).%n_1_g_13

cgc(37,simple,location,[fs(num,sing),fs(type,def),fs(refID,5)],[]).
cg(119,[cgr(ontable,[30,37],_)],[],[fs(kind,normal),fs(comment,'Block 1 on table location 5')]).%n_1_g_14


cgc(38,simple,location,[fs(num,sing),fs(type,def),fs(refID,6)],[]).
cg(120,[cgr(ontable,[33,38],_)],[],[fs(kind,normal),fs(comment,'Block 2 on table location 6')]).%n_1_g_15


cg(121,[cgr(ontable,[35,37],_)],[],[fs(kind,normal),fs(comment,'Block 3 on table location 5')]).%n_1_g_16

cg(122,[cgr(above,[30,35],_)],[],[fs(kind,normal),fs(comment,'Block 1 above block 3')]).%n_1_g_17

cgc(39,simple,hand,[fs(num,sing),fs(type,def),fs(refID,4)],[]).
cg(123,[cgr(emptyhand,[39],_)],[],[fs(kind,normal),fs(comment,'All the blocks are on the table and not in the hand')]).%n_1_g_18

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












