:-[matrix].


% Randomise.

% Seed.
:-set_random(seed(777)).

% Vector.
rand_vector(0,_,[]):-!.
rand_vector(D,[A,B],[X|R]):-
  D1 is D - 1,
  random(P),
  X is P * (B - A) + A,
  rand_vector(D1,[A,B],R),
  !.

% Matrix.
rand_matrix(0,_,_,[]):-!.
rand_matrix(P,Q,[A,B],[X|R]):-
  P1 is P - 1,
  rand_vector(Q,[A,B],X),
  rand_matrix(P1,Q,[A,B],R),
  !.

% Nnet constructor.

:-dynamic weight_matrix/2.
:-dynamic bias_vector/2.
:-dynamic activation/2.
:-dynamic layer_input/2.
:-dynamic layer_output/2.
:-dynamic layer_out_diff/2.
:-dynamic layer_in_diff/2.
:-dynamic layer_bias_grad/2.
:-dynamic layer_weight_grad/2.


layer_init(Name,InDim,OutDim,Act,[S1,S2]):-
  retractall(weight_matrix(Name,_)),
  retractall(bias_vector(Name,_)),
  retractall(activation(Name,_)),
  rand_matrix(OutDim,InDim,[S1,S2],W),
  assert(
    weight_matrix(
      Name,
      W
    )
  ),
  rand_vector(OutDim,[S1,S2],B),
  assert(
    bias_vector(
      Name,
      B
    )
  ),
  assert(
    activation(
      Name,
      Act
    )
  ),
  print([layer,Name,has,been,initialised]),nl,
  !.

  
% Activation Function.

% ReLU.

relu(X,0):-
  X < 0,
  !.
relu(X,30):-
  X > 30,
  !.
relu(X,X):-!.


relu_diff(X,0):-
  X < 0,
  !.
relu_diff(X,0):-
  X >30,
  !.
relu_diff(_,1):-!.


:-discontiguous vec_act/3.
:-discontiguous vec_act_diff/3.

vec_act(relu,V,R):-
  maplist(relu,V,R),
  !.
vec_act_diff(relu,V,R):-
  maplist(relu_diff,V,R),
  !.



% Softmax.
exp(X,Y):-
  Y is exp(X),
  !.
softmax_sub1(A,X,Y):-
  Y is X / A,
  !.
softmax(V,R):-
  maplist(exp,V,P),
  sumlist(P,S),
  maplist(softmax_sub1(S),P,R),
  !.

softmax_diff_sub(_,1):-!.
softmax_diff(V,R):-
  maplist(softmax_diff_sub,V,R),
  !.

vec_act(softmax,V,R):-
  softmax(V,R),
  !.
vec_act_diff(softmax,V,R):-
  softmax_diff(V,R),
  !.

% Cross-Entropy Loss.
ce_sub1(A,B,S):-
  S is A - B,
  !.
neg_t_ln_y(Y,T,R):-
  R is (-1) * T * log(Y),
  !.
ce_error(Y,T,E):-
  maplist(neg_t_ln_y,Y,T,P),
  sumlist(P,E),
  !.
ce_diff(Y,T,D):-
  maplist(ce_sub1,T,Y,D),
  !.

% Forward Computation.

nnet_forward([],In,In):-!.
nnet_forward([Name|LayerList],In,Out):-
  weight_matrix(Name,W),
  bias_vector(Name,B),
  activation(Name,Act),
  retractall(layer_input(Name,_)),
  assert(layer_input(Name,In)),
  % Y = Act(W * In + B)
  transpose(In,InT),
  mat_mult_mat(W,InT,X),
%print(X),nl,
  transpose(X,XT),
  maplist(vec_add_vec(B),XT,ZT),
%print(ZT),nl,
  maplist(vec_act(Act),ZT,Y),
%print(Y),nl,
  retractall(layer_output(Name,_)),
  assert(layer_output(Name,Y)),
  % Go to the next layer
  nnet_forward(LayerList,Y,Out),
  !.

% Error Computation.
nnet_comp_error(LayerList,Tgt,Err,Diff):-
  append(_,[Name],LayerList),
  layer_output(Name,Y),
  maplist(ce_error,Y,Tgt,ErrList),
  sumlist(ErrList,ErrTot),
  length(ErrList,NumData),
  Err is ErrTot / NumData,
  maplist(ce_diff,Y,Tgt,Diff),
  %retractall(layer_out_diff(Name,_)),
  %assert(layer_out_diff(Name,Diff)),
  !.

% Backward Computation.
nnet_backward([],_,_):-!.
nnet_backward(LayerList,Diff,LRate):-
  append(L1,[Name],LayerList),
  weight_matrix(Name,W),
  bias_vector(Name,B),
  activation(Name,Act),
  layer_output(Name,Y),
  maplist(vec_act_diff(Act),Y,ActDiff),
  maplist(vec_mult_vec,Diff,ActDiff,BDiff),
  transpose(BDiff,BDT),
  maplist(sumlist,BDT,BGrad),
  retractall(layer_bias_grad(Name,_)),
  assert(layer_bias_grad(Name,BGrad)),
  layer_input(Name,In),
  mat_mult_mat(BDT,In,WGrad),
  retractall(layer_weight_grad(Name,_)),
  assert(layer_weight_grad(Name,WGrad)),
  mat_mult_mat(BDiff,W,InDiff),
  % Next Layer.
  nnet_backward(L1,InDiff,LRate),
  % Update.
  mat_mult_const(WGrad,LRate,DW),
  mat_add_mat(W,DW,WNew),
  retractall(weight_matrix(Name,_)),
  assert(weight_matrix(Name,WNew)),
  vec_mult_const(BGrad,LRate,DB),
  vec_add_vec(B,DB,BNew),
  retractall(bias_vector(Name,_)),
  assert(bias_vector(Name,BNew)),
  !.

nnet_train(_,_,_,0,_):-!.
nnet_train(Nnet,In,Tgt,Iter,LRate):-
  I1 is Iter - 1,
  nnet_forward(Nnet,In,_),
  nnet_comp_error(Nnet,Tgt,Err,Diff),
  printerr(Err),
  nnet_backward(Nnet,Diff,LRate),
  nnet_train(Nnet,In,Tgt,I1,LRate).

printerr(X):-
  Y is X * 1000000,
  round(Y,Z),
  R is Z / 1000000,
  print(R),nl,
  !.

try:-
  NumHid = 32,
  layer_init(try1,4,NumHid,relu,[-0.2,0.2]),
  layer_init(try2,NumHid,NumHid,relu,[-0.2,0.2]),
  layer_init(try3,NumHid,NumHid,relu,[-0.2,0.2]),
  layer_init(try4,NumHid,NumHid,relu,[-0.2,0.2]),
  layer_init(try5,NumHid,NumHid,relu,[-0.2,0.2]),
  layer_init(try6,NumHid,5,softmax,[-0.2,0.2]),
  In = [[2,3,4,5],[6,7,8,9]],
  Tgt = [[0,1,0,0,0],[0,0,0,1,0]],
  Nnet = [try1,try2,try3,try4,try5,try6],
  nnet_train(Nnet,In,Tgt,100,0.01). 
