%%%%%%%%%LEVEL THREE%%%%%%%%%%%%%
isa(block,entity).
isa(hand,entity).
isa(location,entity).
isa(pickup,act).
isa(putdown,act).
isa(move,act).

isa(at,relation).
isa(above,realtion).
isa(ontable,relation).
isa(top,relation).
isa(emptyhand,relation).

cgc(90,simple,entity,[fs(num,sing),fs(quant,lambda)],_).
cgc(91,simple,typelabel,[fs(num,sing),fs(name,'block')],_).
cgc(92,simple,color,[fs(num,sing)],_).
cgc(93,simple,shape,[fs(num,sing)],_).
cg(500,[cgr(attr,[90,92],_),cgr(chrc,[90,93],_)],[],[fs(kind,body_of_type_def),fs(comment,'An entity has attribute color and characteristic shape')]).
cgc(94,complex,lambdaexpression,[500],_). 
cg(501,[cgr(def,[91,94],_)],[],[fs(kind,typedef),fs(comment,'')]).

cgc(95,simple,block,[fs(num,sing)],_).
cgc(96,simple,location,[fs(num,sing)],_).
cgc(97,simple,hand,[fs(num,sing)],_).
cgc(98,simple,act,[fs(quant,lambda)],_).
cg(502,[cgr(top,[95,96],_)],[],[fs(kind,body_of_context),fs(comment,'Block on top of location')]).
cgc(99,complex,situation,[502],_).
cg(503,[cgr(ptnt,[98,95],_),cgr(dest,[98,96],_),cgr(inst,[98,97],_),
cgr(rslt,[98,99],_)],[],[fs(kind,body_of_type_def),fs(comment,'')]).
cgc(1000,complex,lambdaexpression,[503],_). 
cgc(1001,simple,typelabel,[fs(name,'putdown')],_).
cg(504,[cgr(def,[1001,1000],_)],[],[fs(kind,typedef),fs(comment,'')]).

cg(505,[cgr(at,[97,96],_)],[],[fs(kind,body_of_context),fs(comment,'Hand at location')]).
cgc(1002,complex,situation,[505],_).
cg(506,[cgr(dest,[98,96],_),cgr(inst,[98,97],_),cgr(rslt,[98,1002],_)],[],[fs(kind,body_of_type_def),fs(comment,'')]).
cgc(1003,complex,lambdaexpression,[506],_).
cgc(1004,simple,typelabel,[fs(name,'movehand')],_).
cg(507,[cgr(def,[1004,1003],_)],[],[fs(kind,typedef),fs(comment,'')]).

cg(508,[cgr(grasp,[97,95],_)],[],[fs(kind,body_of_context),fs(comment,'Hand grasp block')]).
cgc(1005,complex,situation,[508],_).
cg(509,[cgr(ptnt,[98,95],_),cgr(inst,[98,97],_),cgr(rslt,[98,1005],_)],[],[fs(kind,body_of_type_def),fs(comment,'')]).
cgc(1006,complex,lambdaexpression,[509],_).
cgc(1007,simple,typelabel,[fs(name,'pickup')],_).
cg(510,[cgr(def,[1007,1006],_)],[],[fs(kind,typedef),fs(comment,'')]).

cg(511,[cgr(at,[95,96],_)],[],[fs(kind,body_of_context),fs(comment,'Block at location')]).
cgc(1008,complex,situation,[511],_).
cg(512,[cgr(dest,[98,96],_),cgr(ptnt,[98,95],_),cgr(inst,[98,97],_),cgr(rslt,[98,1008],_)],[],[fs(kind,body_of_type_def),fs(comment,'')]).
cgc(1009,complex,lambdaexpression,[512],_).
cgc(1010,simple,typelabel,[fs(name,'moveblock')],_).
cg(513,[cgr(def,[1010,1009],_)],[],[fs(kind,typedef),fs(comment,'')]).

%%%%RELATION DEFINITIONS
cgc(1011,simple,location,[fs(num,sing),fs(quant,lambda)],_).
cgc(1012,simple,entity,[fs(num,sing),fs(quant,lambda)],_).
cg(514,[cgr(pos,[1012,1011],_)],[],[fs(kind,body_of_rel_def),fs(comment,'')]).
cgc(1013,complex,lambdaexpression,[514],_).
cgc(1014,simple,relationlabel,[fs(name,'at')],_).
cg(515,[cgr(def,[1014,1013],_)],[],[fs(kind,reldef),fs(comment,'')]).

cgc(1015,simple,block,[fs(num,sing),fs(quant,lambda)],_).
cgc(1016,simple,block,[fs(num,sing)],_).
cg(516,[cgr(above,[1015,1016],_)],[],[fs(kind,body_of_context),fs(comment,'Block above block')]).
cgc(1017,complex,situation,[516],_).
cg(517,[cgr(above,[1015,1011],_),cgr(not,[1017],_)],[],[fs(kind,body_of_rel_def),fs(comment,'')]).
cgc(1018,complex,lambdaexpression,[517],_).
cgc(1019,simple,relationlabel,[fs(name,'top')],_).
cg(518,[cgr(def,[1019,1018],_)],[],[fs(kind,reldef),fs(comment,'')]).

cgc(1020,simple,hand,[fs(num,sing),fs(quant,lambda)],_).
cg(519,[cgr(grasp,[1020,1016],_)],[],[fs(kind,body_of_context),fs(comment,'Hand grasp block')]).
cgc(1021,complex,situation,[519],_).
cg(520,[cgr(not,[1021],_)],[],[fs(kind,body_of_rel_def),fs(comment,'Hand not gpasp block')]).
cgc(1022,complex,lambdaexpression,[520],_).
cgc(1023,simple,relationlabel,[fs(name,'emptyhand')],_).
cg(521,[cgr(def,[1023,1022],_)],[],[fs(kind,reldef),fs(comment,'')]).


cg(522,[cgr(grasp,[97,1015],_)],[],[fs(kind,body_of_context),fs(comment,'Hand grasp block')]).
cgc(1024,complex,situation,[522],_).
cg(523,[cgr,(at,[1015,1011],_),cgr(not,[1024],_)],[],[fs(kind,body_fo_rel_def),fs(comment,'')]).
cgc(1025,complex,lambdaexpression,[523],_).
cgc(1026,simple,relationlabel,[fs(name,'ontable')],_).
cg(524,[cgr(def,[1026,1025],_)],[],[fs(kind,reldef),fs(comment,'')]).

cgc(1027,simple,block,[fs(num,sing),fs(quant,lambda)],_).
cg(525,[cgr(ontable,[1015,96],_),cgr(ontable,[1027,96],_)],[],[fs(kind,body_of_rel_def),fs(comment,'')]).
cgc(1028,complex,lambdaexpression,[525],_).
cgc(1029,simple,relationlabel,[fs(name,'above')],_).
cg(526,[cgr(def,[1029,1028],_)],[],[fs(kind,reldef),fs(comment,'')]).

%%%%Catalog of Individuals
/*ind(IndID,Name,TypeLabel),naprimer [Person:John#1] shte bade ind(1,John,Person)*/
ind(1,_,block).
ind(2,_,block).
ind(3,_,block).
ind(4,_,hand).
ind(5,_,location).
ind(6,_,location).

%%%%%Assertions
cgc(1030,simple,block,[fs(num,plur),fs(type,meas),fs(quant,3)],_).

cgc(1031,simple,block,[fs(num,sing),fs(type,def),fs(refID,1)],_).

cgc(1032,simple,color,[fs(num,sing),fs(name,'red')],_).
cg(528,[cgr(attr,[1031,1032],_)],[],[fs(kind,normal),fs(comment,'Block 1 has red color')]).

cgc(1033,simple,block,[fs(num,sing),fs(type,def),fs(refID,2)],_).
cgc(1034,simple,color,[fs(num,sing),fs(name,'blue')],_).
cg(529,[cgr(attr,[1033,1034],_)],[],[fs(kind,normal),fs(comment,'Block 2 has blue color')]).

cgc(1035,simple,block,[fs(num,sing),fs(type,def),fs(refID,3)],_).
cgc(1036,simple,color,[fs(num,sing),fs(name,'green')],_).
cg(530,[cgr(attr,[1035,1036],_)],[],[fs(kind,normal),fs(comment,'Block 3 has green color')]).

cgc(1037,simple,location,[fs(num,sing),fs(type,def),fs(refID,5)],_).
cg(531,[cgr(ontable,[1031,1037],_)],[],[fs(kind,normal),fs(comment,'Block 1 on table location 5')]).

cgc(1038,simple,location,[fs(num,sing),fs(type,def),fs(refID,6)],_).
cg(532,[cgr(ontable,[1033,1038],_)],[],[fs(kind,normal),fs(comment,'Block 2 on table location 6')]).

cg(532,[cgr(ontable,[1035,1037],_)],[],[fs(kind,normal),fs(comment,'Block 3 on table location 5')]).

cg(533,[cgr(above,[1031,1035],_)],[],[fs(kind,normal),fs(comment,'Block 1 above block 3')]).

cgc(1039,simple,hand,[fs(num,sing),fs(refID,4)],_).
cg(534,[cgr(emptyhand,[1039],_)],[],[fs(kind,normal),fs(comment,'Emptyhand 4')]).

cg(535,[cgr(attr,[1031,1034],_)],[],[fs(kind,body_of_context),fs(comment,'Block 1 has blue color')]).
cgc(1040,complex,or,[535],_).

cg(536,[cgr(attr,[1033,1034],_)],[],[fs(kind,body_of_context),fs(comment,'Block 2 has blue color')]).
cgc(1041,complex,or,[536],_).

cgc(1042,complex,either,[535,536],_).

cgc(1043,simple,block,[fs(num,sing),fs(quant,every)],_).
cgc(1044,simple,shape,[fs(num,sing),fs(name,'cubical')],_).
cg(537,[cgr(char,[1034,1044],_)],[],[fs(kind,normal),fs(comment,'Every block has cubical shape')]).



















