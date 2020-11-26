:-[nnet].


dnn_heuristic(Nnet,G,SList,method(_,input(TreeBreadth,TreeDepth),_),Res):-
  pred_to_vec(G,SList,TreeBreadth,TreeDepth,InVec),
  nnet_forward(Nnet,[InVec],[OutVec]),
  label_vec(OutVec,1,OVL),
  sort(OVL,OVLS),
  reverse(OVLS,Res),
  !.


comp_indim(TreeBreadth,TreeDepth,NumWord,Dim):-
  get_partial_tree(a,TreeBreadth,TreeDepth,T),
  flatten(T,FT),
  length(FT,P),
  Dim is P * NumWord,
  !.


pred_to_vec(Pred,[SList,MaxN],TreeBreadth,TreeDepth,Vec):-
  copy_term(Pred,GT),
  vble_fill(GT,GTF),
  get_partial_tree(GTF,TreeBreadth,TreeDepth,PT),
  flatten(PT,PTF),
  symlist_to_numlist(PTF,SList,NL),
  numlist_to_vec(NL,MaxN,Vec),
  !.

label_vec([],_,[]):-!.
label_vec([Elem|L],Num,[[Elem,Num]|LT]):-
  N1 is Num + 1,
  label_vec(L,N1,LT),
  !.
  

dnn_train(Nnet,G,SList,AxNum,method(learning(NumEpoch,LRate),input(TreeBreadth,TreeDepth),output(OutDim))):-
  pred_to_vec(G,SList,TreeBreadth,TreeDepth,InVec),
  axnum_to_vec(AxNum,OutDim,TgtVec),
  nnet_train(Nnet,[InVec],[TgtVec],NumEpoch,LRate),
  !.

axnum_to_vec(AxNum,Dim,Vec):-
  AxNum > Dim,
  print(['Warning: AxNum is bigger than Dim: ',AxNum,Dim]),nl,
  copy_n_times(0,Dim,Vec),
  !.

axnum_to_vec(AxNum,Dim,Vec):-
  D1 is AxNum - 1,
  copy_n_times(0,D1,Vec1),
  D2 is Dim - AxNum,
  copy_n_times(0,D2,Vec2),
  append(Vec1,[1|Vec2],Vec),
  !.


:- dynamic search_time/1.
init_search_time:-
  retractall(search_time(_)),
  assert(search_time(0)).
add_search_time:-
  search_time(N),
  retractall(search_time(_)),
  N1 is N + 1,
  assert(search_time(N1)),
  !.

dnn_sl_resolution(A,B,C,D,E,F,G):-
  dnn_sl_resolution(A,B,C,standard,D,E,F,G).

dnn_sl_resolution([],_,_,_,_,_,_,[]).
dnn_sl_resolution(GList,[AList,NumA],SList,StatModName,Nnet,Mtd,Depth,Path):-
  Depth > 0,
  D1 is Depth - 1,
  GList = [-OrgG|GLT],
  copy_term(GList,PreGList),
  static_module(StatModName,OrgG,G),
  copy_term(G,GTemp),
  dnn_heuristic(Nnet,G,SList,Mtd,SAL),
  %SAL = AList,
  member([_,AxNum],SAL),
  member([AxNum,AxName,AxRule],AList),
add_search_time,
  copy_term(AxRule,Ax),
  %Ax = [+G|GN],
  append(GN,[+G],Ax),
  append(GN,GLT,GListNew),
  dnn_sl_resolution(GListNew,[AList,NumA],SList,StatModName,Nnet,Mtd,D1,PathNew),
  Path = [[PreGList,AxName]|PathNew],
  (
    Mtd = method(learning(_,_),_,_),
    dnn_train(Nnet,GTemp,SList,AxNum,Mtd)
    ;
    Mtd = method(reasoning,_,_)
  ).

print_by_line([]).
print_by_line([X|L]):-
  print(X),nl,
  print_by_line(L).



:- dynamic num_vble/1.
:- retractall(num_vble(_)).
:- assert(num_vble(0)).

vble_fill(X,X):-
  ground(X),
  !.
vble_fill(X,X):-
  var(X),
  num_vble(N1),
  N is N1 + 1,
  retractall(num_vble(_)),
  assert(num_vble(N)),
  X = vble(N),
  !.
vble_fill([],[]):-!.
vble_fill([X|L],[XT|LT]):-
  vble_fill(X,XT),
  vble_fill(L,LT),
  !.
 

copy_n_times(_,0,[]):-!.
copy_n_times(X,N,[X|L]):-
  N > 0,
  N1 is N - 1,
  copy_n_times(X,N1,L),
  !.

produce_empty_tree(_,0,novalue):-!.
produce_empty_tree(B,D,[novalue|L]):-
  D > 0,
  D1 is D - 1,
  produce_empty_tree(B,D1,Res1),
  copy_n_times(Res1,B,L),
  !.

get_partial_tree([X|_],_,0,X):-!.
get_partial_tree(X,_,0,X):-
  \+is_list(X),
  !.
get_partial_tree([X|L],Breadth,Depth,[X|LT]):-
  Depth > 0,
  D1 is Depth - 1,
  get_partial_tree2(L,Breadth,D1,LT),
  !.
get_partial_tree(X,Breadth,Depth,[X|LT]):-
  \+is_list(X),
  Depth > 0,
  D1 is Depth - 1,
  get_partial_tree2([],Breadth,D1,LT),
  !.

get_partial_tree2(L,Breadth,Depth,Res):-
  findall(XT,(member(X,L),get_partial_tree(X,Breadth,Depth,XT)),Res1),
  length(L,LenL),
  length(XT,LenL), % Check if the lengths agree.
  N1 is Breadth - LenL,
  (
    N1 >= 0,
    produce_empty_tree(Breadth,Depth,TEmpty),
    copy_n_times(TEmpty,N1,Res2),
    append(Res1,Res2,Res)
    ;
    N1 < 0,
    get_first_element(Res1,Breadth,Res)
  ),
  !.

get_first_element(_,0,[]).
get_first_element([],N,[norule|Res1]):-
  N > 0,
  N1 is N - 1,
  get_first_element([],N1,Res1).
get_first_element([X|L],N,[X|Res1]):-
  N > 0,
  N1 is N - 1,
  get_first_element(L,N1,Res1).

symlist_to_numlist([],_,[]):-!.
symlist_to_numlist([X|L],SList,[XT|LT]):-
  (
    X = vble(_),
    XT = 1
    ;
    X = novalue,
    XT = -1
    ;
    member([XT,X],SList)
  ),
  symlist_to_numlist(L,SList,LT),
  !.

num_to_vec(-1,Dim,Vec):-
  copy_n_times(0,Dim,Vec),
  !.
num_to_vec(Num,Dim,Vec):-
  Num > Dim,
  print(['Warning: Num is bigger than Dim: ',Num,Dim]),nl,
  copy_n_times(0,Dim,Vec),
  !.

num_to_vec(Num,Dim,Vec):-
  D1 is Num - 1,
  copy_n_times(0,D1,Vec1),
  D2 is Dim - Num,
  copy_n_times(0,D2,Vec2),
  append(Vec1,[1|Vec2],Vec),
  !.



numlist_to_vec([],_,[]):-!.
numlist_to_vec([X|L],Dim,Res):-
  num_to_vec(X,Dim,XT),
  numlist_to_vec(L,Dim,LT),
  append(XT,LT,Res),
  !.

for(K,P,Q):-
  P =< Q,
  K = P.
for(K,P,Q):-
  P < Q,
  P1 is P + 1,
  for(K,P1,Q).

static_module(standard,G,G).


