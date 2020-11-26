% Computational Intelligence: a logical approach.
% CILOG Code. Figure 2.3 & Example 2.13.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press

% imm_west(R1,R2) is true if room R1 is immediately west of room R2
:- 

imm_west(r101,r103).
imm_west(r103,r105).
imm_west(r105,r107).
imm_west(r107,r109).
imm_west(r109,r111).
imm_west(r131,r129).
imm_west(r129,r127).
imm_west(r127,r125).

% imm_east(R1,R2) is true if room R1 is immediately east of room R2

imm_east(R1,R2) <- 
   imm_west(R2,R1).

% next_door(R1,R2) is true if room R1 is next door to room R2

next_door(R1,R2) <-
   imm_east(R1,R2).
next_door(R1,R2) <-
   imm_west(R1,R2).

% two_doors_east(R1,R2) is true if room R1 is two doors east of room R2

two_doors_east(R1,R2) <-
   imm_east(R1,R) &
   imm_east(R,R2).

% west(R1,R2) is true if room R1 is somewhere west of room R2

west(R1,R2) <-
   imm_west(R1,R2).
west(R1,R2) <-
   imm_west(R1,R) &
   west(R,R2).

% POSSIBLE QUERIES
% ask next_door(R,r105).
% ask west(R,r105).
% ask west(r105,R).
% ask next_door(X,Y).
