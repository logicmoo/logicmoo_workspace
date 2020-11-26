
set2nat(Xs,N):-set2nat(Xs,0,N).

set2nat([],R,R).
set2nat([X|Xs],R1,Rn):-R2 is R1+(1<<X),set2nat(Xs,R2,Rn).

hfs2nat(N,R):-default_ulimit(D),hfs2nat_(D,N,R).  

hfs2nat_(_,[],R):-!,R=0.
hfs2nat_(Ulimit,N,R):-integer(N),N>0,N<Ulimit,!,R=N.
hfs2nat_(Ulimit,Ts,R):-maplist(hfs2nat_(Ulimit),Ts,T),set2nat(T,R).

default_ulimit(1).

nat2set(N,Xs):-findall(X,nat2element(N,X),Xs).

nat2element(N,K):-nat2el(N,0,K).

nat2el(N,K1,Kn):-
  N>0, B is /\(N,1), N1 is N>>1,
  nat2more(B,N1,K1,Kn).

nat2more(1,_,K,K).
nat2more(_,N,K1,Kn):-K2 is K1+1,nat2el(N,K2,Kn).

nat2hfs_(_,0,R):-!,R=[].
nat2hfs_(Ulimit,N,R):-N<Ulimit,!,R=N.
nat2hfs_(Ulimit,N,R):-nat2set(N,Ns),maplist(nat2hfs_(Ulimit),Ns,R).

nat2hfs(N,R):-default_ulimit(D),nat2hfs_(D,N,R).

nat(0).
nat(N):-nat(N1),N is N1+1.

iterative_hfs_generator(HFS):-default_ulimit(D),hfs_with_urelements(D,HFS).

hfs_with_urelements(Ulimit,HFS):-nat(N),nat2hfs_(Ulimit,N,HFS).

all_subsets([],[[]]).
all_subsets([X|Xs],Zss):-all_subsets(Xs,Yss),extend_subsets(Yss,X,Zss).

extend_subsets([],_,[]).
extend_subsets([Ys|Yss],X,[Ys,[X|Ys]|Zss]):-extend_subsets(Yss,X,Zss).

hfs_generator(NewSet):-nat(N),hfs_level(N,NewSet).
  
hfs_level(N,NewSet):-N1 is N+1,
  subsets_at_stage(N1,[],Hss1),subsets_at_stage(N,[],Hss),
  member(NewSet,Hss1),not(member(NewSet,Hss)).
  
subsets_at_stage(0,X,X).
subsets_at_stage(N,X,Xss):-N>0,N1 is N-1,
  all_subsets(X,Xs),
  subsets_at_stage(N1,Xs,Xss).

nat2hypergraph(N,Nss):-nat2set(N,Ns),maplist(nat2set,Ns,Nss).

hypergraph2nat(Nss,N):-maplist(set2nat,Nss,Ns),set2nat(Ns,N).

hfold(_,G,N,R):- integer(N),!,call(G,N,R).
hfold(F,G,Xs,R):-maplist(hfold(F,G),Xs,Rs),call(F,Rs,R).

hsize(HFS,Size):-hfold(hsize_f,hsize_g,HFS,Size).

hsize_f(Xs,S):-sumlist(Xs,S1),S is S1+1.
hsize_g(_,1). 

gfold(_,G,Ulimit,_,N,R):- integer(N),N<Ulimit,!,call(G,N,R).
gfold(F,G,Ulimit,T,N,R):-
  call(T,N,TransformedN),
  maplist(gfold(F,G,Ulimit,T),TransformedN,Rs),
  call(F,Rs,R).

nfold(F,G,Ulimit,N,R):-gfold(F,G,Ulimit,nat2set,N,R).
nfold1(F,G,N,R):-default_ulimit(D),nfold(F,G,D,N,R).

nsize(N,R):-default_ulimit(Ulimit),nsize(Ulimit,N,R).
nsize(Ulimit,N,R):-nfold(hsize_f,hsize_g,Ulimit,N,R).

toNat(F,Hs,R):-maplist(hfs2nat,Hs,Ns),call(F,Ns,N),nat2hfs(N,R).

toNat1(F,X,R):-hfs2nat(X,N),call(F,N,NR),nat2hfs(NR,R).
  
toNat2(F,X,Y,R):-
  hfs2nat(X,NX),hfs2nat(Y,NY),
    call(F,NX,NY,NR),
  nat2hfs(NR,R).

toHFS(F,Ns,N):-maplist(nat2hfs,Ns,Hs),call(F,Hs,H),hfs2nat(H,N).

toHFS1(F,X,R):-nat2hfs(X,N),call(F,N,NR),hfs2nat(NR,R).
  
toHFS2(F,X,Y,R):-
  nat2hfs(X,NX),nat2hfs(Y,NY),
  call(F,NX,NY,NR),hfs2nat(NR,R).

cantor_pair(K1,K2,P):-P is (((K1+K2)*(K1+K2+1))//2)+K2.

cantor_unpair(Z,K1,K2):-I is floor((sqrt(8*Z+1)-1)/2),
  K1 is ((I*(3+I))//2)-Z,
  K2 is Z-((I*(I+1))//2).

bitmerge_pair(A,B,P):-up0(A,X),up1(B,Y),P is X+Y.

bitmerge_unpair(P,A,B):-down0(P,A),down1(P,B).

even_up(A,R):-nat2element(A,X),E is X<<1,R is 1<<E.
odd_up(A,R):-nat2element(A,X),E is 1+(X<<1),R is 1<<E.
even_down(A,R):-nat2element(A,X),even(X),E is X>>1,R is 1<<E.
odd_down(A,R):-nat2element(A,X),odd(X),E is (X>>1), R is 1<<E.

even(X):- 0 =:= /\(1,X).
odd(X):- 1 =:= /\(1,X).

up0(A,P):-findall(R,even_up(A,R),Rs),sumlist(Rs,P).
up1(A,P):-findall(R,odd_up(A,R),Rs),sumlist(Rs,P).
down0(A,X):-findall(R,even_down(A,R),Rs),sumlist(Rs,X).
down1(A,X):-findall(R,odd_down(A,R),Rs),sumlist(Rs,X).

bitmerge_pair(X-Y,Z):-bitmerge_pair(X,Y,Z).

bitmerge_unpair(Z,X-Y):-bitmerge_unpair(Z,X,Y).

nat_powset(N,PN):-toHFS1(all_subsets,N,PN).

%nat_powset_alt i = product (map (\k->1+(exp2 . exp2) k) (nat2set i)) 

hfs_ordinal(0,[]).
hfs_ordinal(N,Os):-N>0,N1 is N-1,findall(I,between(0,N1,I),Is),
  maplist(hfs_ordinal,Is,Os).
 
nat_ordinal(N,OrdN):-hfs_ordinal(N,H),hfs2nat(H,OrdN).

nat_choice_fun(N,CFN):-nat2set(N,Es),
  maplist(nat2set,Es,Ess),maplist(choice_of_one,Ess,Hs),
  maplist(bitmerge_pair,Es,Hs,Ps),set2nat(Ps,CFN).

choice_of_one([X|_],X).

nat2memb(N,XY):-default_ulimit(D),nat2memb(D,N,XY).
nat2memb(Ulimit,N,X-Y):-nat2contains(Ulimit,N,Y-X).

nat2contains(N,XY):-default_ulimit(D),nat2contains(D,N,XY).
nat2contains(Ulimit,N,E):-nat2element(N,X),
  ( E=N-X
  ; X>=Ulimit,nat2contains(Ulimit,X,E)
  ).

nat2cdag(L,N,G):-
  findall(E,nat2contains(L,N,E),Es),
  vertices_edges_to_ugraph([],Es,G).

nat2mdag(L,N,G):-
  findall(E,nat2memb(L,N,E),Es),
  vertices_edges_to_ugraph([],Es,G).  

to_dag(N,NewG):-default_ulimit(Ulimit),to_dag(Ulimit,N,NewG).
  
to_dag(Ulimit,N,NewG):-
  findall(E,nat2contains(Ulimit,N,E),Es),
  vertices_edges_to_ugraph([],Es,G),
  vertices(G,Rs),reverse(Rs,Vs),
  empty_assoc(D),remap(Vs,0-D,_RVs,KD),remap(Es,KD,REs,_NewKD),
  vertices_edges_to_ugraph([],REs,NewG).

remap(Xs,Rs):-empty_assoc(D),remap(Xs,0-D,Rs,_KD).

remap([],KD,[],KD).
remap([X|Xs],KD1,[A|Rs],KD3):-integer(X),!,
  assoc(X,A,KD1,KD2),
  remap(Xs,KD2,Rs,KD3).
remap([X-Y|Xs],KD1,[A-B|Rs],KD4):-
  assoc(X,A,KD1,KD2),assoc(Y,B,KD2,KD3),
  remap(Xs,KD3,Rs,KD4).
  
assoc(X,R,K-D,KD):-get_assoc(X,D,A),!,R=A,KD=K-D.
assoc(X,K,K-D,NewK-NewD):-NewK is K+1,put_assoc(X,D,K,NewD).

from_dag(G,N):-vertices(G,[Root|_]),compute_decoration(G,Root,N).

compute_decoration(G,V,Ds):-neighbors(V,G,Es),compute_decorations(G,Es,Ds).
  
compute_decorations(_,[],0).
compute_decorations(G,[E|Es],N):-
  maplist(compute_decoration(G),[E|Es],Ds),
  set2nat(Ds,N).

nat2digraph(N,G):-nat2set(N,Ns),
  maplist(bitmerge_unpair,Ns,Ps),
  vertices_edges_to_ugraph([],Ps,G).
  
digraph2nat(G,N):-edges(G,Ps),
  maplist(bitmerge_pair,Ps,Ns),
  set2nat(Ns,N).

transpose_nat(N,TN):-nat2digraph(N,G),transpose(G,T),digraph2nat(T,TN).

setShow(S):-gshow(S,"{,}"),nl.

gshow(0,[L,_C,R]):-put(L),put(R).
gshow(N,_):-integer(N),N>0,!,write(N).
gshow(Hs,[L,C,R]):-put(L),gshow_all(Hs,[L,C,R]),put(R).

gshow_all([],_).
gshow_all([H],LCR):-gshow(H,LCR).
gshow_all([H,G|Hs],[L,C,R]):-
  gshow(H,[L,C,R]),
  ([C]\=="~"->put(C);true),
  gshow_all([G|Hs],[L,C,R]).
  
test:-
  G=[0-[1, 2, 5, 6, 7], 1-[7, 9], 2-[7, 10], 3-[7], 
     4-[8, 10],5-[8, 9], 6- [8], 7-[9], 8-[9], 9-[10], 10-[]],
  from_dag(G,N),
  to_dag(N,G1),
  from_dag(G1,N2),
  write(N+G),nl,nl,
  write(N2+G1),nl,nl.

c:-['pSET.pro'].
  



