conc2([_A,_B],[_C,_D],[_A,_B,_C,_D]) .

selection1([_F1,_F2,_F3,_F4,_F5,_F6],[_F1,_F2],[_F3,_F4,_F5,_F6]) .
selection1([_F1,_F2,_F3,_F4,_F5,_F6],[_F3,_F4],[_F1,_F2,_F5,_F6]) .
selection1([_F1,_F2,_F3,_F4,_F5,_F6],[_F5,_F6],[_F1,_F2,_F3,_F4]) .

selection2([_FA,_FB,_FC,_FD],[_FA,_FB]) .
selection2([_FA,_FB,_FC,_FD],[_FC,_FD]) .

rotation(_TOUT,_TOUT) .
rotation([_DEV,_DER,_HAU,_BAS],[_HAU,_BAS,_DER,_DEV]) .
rotation([_DEV,_DER,_HAU,_BAS],[_DER,_DEV,_BAS,_HAU]) .
rotation([_DEV,_DER,_HAU,_BAS],[_BAS,_HAU,_DEV,_DER]) .

disj([],[]) .
disj([_T1|_Q1],[_T2|_Q2]) :- 
     _T1\==_T2, disj(_Q1,_Q2) .

verifier(_SOL1,_SOL2,_SOL3,_SOL4) :- 
     rotation(_SOL2,_C2), disj(_SOL1,_C2), rotation(_SOL3,_C3), 
       disj(_SOL1,_C3), disj(_C2,_C3), rotation(_SOL4,_C4), 
       disj(_SOL1,_C4), disj(_C2,_C4), disj(_C3,_C4), sorm('cube 1 : '), 
       sorm(_SOL1), nl, sorm('cube 2 : '), sorm(_C2), nl, 
       sorm('cube 3 : '), sorm(_C3), nl, sorm('cube 4 : '), 
       sorm(_C4), nl .

insanity(_C1,_C2,_C3,_C4) :- 
     selection1(_C1,_P1A,_R1), selection1(_C2,_P2A,_R2), 
       selection1(_C3,_P3A,_R3), selection1(_C4,_P4A,_R4), 
       selection2(_R1,_P1B), selection2(_R2,_P2B), selection2(_R3,_P3B), 
       selection2(_R4,_P4B), conc2(_P1A,_P1B,_SOL1), 
       conc2(_P2A,_P2B,_SOL2), conc2(_P3A,_P3B,_SOL3), 
       conc2(_P4A,_P4B,_SOL4), verifier(_SOL1,_SOL2,_SOL3,_SOL4) .

sorm(_X) :- 
     write(_X) .

p1 :- 
     insanity([a,a,a,a,a,a],[b,b,b,b,b,b],[c,c,c,c,c,c],[d,d,d,d,d
         ,d]) .

p2 :- 
     insanity([j,b,j,r,v,j],[b,j,j,b,r,v],[r,j,r,b,v,b],[b,v,r,v,r
         ,v]) .

p3 :- 
     insanity([a,a,a,a,a,a],[a,a,a,a,a,a],[a,a,a,a,a,a],[a,a,a,a,a
         ,a]) .

p4 :- 
     insanity([v,j,v,r,j,b],[j,b,r,b,v,v],[r,v,b,j,r,r],[b,r,j,v,r
         ,j]) .
