:- include(test_header).


 

all([X,Y,R],
  exists(RPlus, 
   holds(RPlus,X,Y) & holds(Ri,Y,Z) => 
           ( holds(RPlus,X,Z) & ~holds(Ri,X,Z)))).



