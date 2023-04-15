/** <examples>
?- aleph_sample:induce_tree(Program).
*/
%:- module(aleph_sample,[]).

:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- use_module(library(logicmoo_utils)).

%:- use_module(h_muarc_alephlib).
%:- use_module(aleph).
:- use_module(library(slipcover)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(prolog).
:- endif.

%:- aleph.
:- sc.
:-style_check(-discontiguous).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% specify tree type

/*
:- aleph_set(tree_type,model).
:- aleph_set(evalfn,mse).
:- aleph_set(minpos,2).       % minimum examples in leaf for splitting
:- aleph_set(mingain,0.01).	% toy example needs this to be low
:- aleph_set(dependent,2).	% second argument of f/2 is to predicted
:- aleph_set(verbosity,10).
%:- aleph_set(mingain,-1e10).
*/

:- aleph_set(verbosity, 2).
:- aleph_set(interactive, false).
:- aleph_set(i,10).
:- aleph_set(clauselength,10).
:- aleph_set(nodes,10000).

% specify predicate definition to use for model construction
:- model(zendo/1).

output(zendo/1).
input_cw(piece/2).
input_cw(lhs/1).
input_cw(lhs/1).
input_cw(color/2).
input_cw(cenGX/2).
input_cw(cenGY/2).
input_cw(size/2).
input_cw(shape/2).


:- modeh(*,[zendo(#state)],[lhs/1,rhs/1]).
:- modeb(*,piece(+state,-piece)).
:- modeb(*,lhs(#piece)).
:- modeb(*,rhs(-piece)).
:- modeb(*,color(+piece,#color)).
:- modeb(*,size(+piece,-size_nat900)).
:- modeb(*,shape(+piece,#shape_id)).
%:- modeb(*,position(+piece,-hv_nat30,-hv_nat30)).
%:- modeb(*,rotation(+piece,-rot_xform)).
%:- modeb(*,orientation(+piece,#orientation)).
:- modeb(*,cenGX(+piece,-hv_nat30)).
:- modeb(*,cenGY(+piece,-hv_nat30)).
%:- modeb(*,contact(+piece,-piece)).
%:- modeb(*,child(+piece,-piece)).
:- modeb(*,my_geq(+hv_nat30,#hv_nat30)).
:- modeb(*,my_leq(+hv_nat30,#hv_nat30)).
%:- modeb(*,my_gt(+hv_nat30,#hv_nat30)).
%:- modeb(*,my_lt(+hv_nat30,#hv_nat30)).
:- modeb(*,my_add(+hv_nat30,+hv_nat30,-hv_nat30)).
:- modeb(*,my_mult(+hv_nat30,#hv_nat30,-hv_nat30)).


:- determination(zendo/1,piece/2).
:- determination(zendo/1,color/2).
:- determination(zendo/1,size/2).
:- determination(zendo/1,cenGX/2).
:- determination(zendo/1,cenGY/2).
:- determination(zendo/1,shape/2).
:- determination(zendo/1,rhs/1).
:- determination(zendo/1,lhs/1).
%:- determination(rhs/1,lhs/1).
%:- determination(zendo/1,position/3).
%:- determination(zendo/1,rotation/2).
%:- determination(zendo/1,orientation/2).
%:- determination(zendo/1,contact/2).
%:- determination(zendo/1,child/2).
:- determination(zendo/1,my_geq/2).
:- determination(zendo/1,my_leq/2).
:- determination(zendo/1,my_gt/2).
:- determination(zendo/1,my_lt/2).
:- determination(zendo/1,my_add/3).
:- determination(zendo/1,my_mult/3).

:- lazy_evaluate(my_geq/2).
:- lazy_evaluate(my_leq/2).
:- lazy_evaluate(my_gt/2).
:- lazy_evaluate(my_lt/2).
%:- aleph_set(clauselength,5).

:-begin_bg.


my_add(A,B,C) :- nonvar(A), nonvar(B), between(1,30,A),between(1,30,B), C is A+B.
my_add(A,B,C) :- nonvar(A), nonvar(C), between(1,30,A),between(1,30,B), B is C-A.
my_add(A,B,C) :- nonvar(C), nonvar(B), between(1,30,A),between(1,30,B), A is C-B.


my_mult(A,B,C) :- nonvar(A), nonvar(B), between(1,30,A),between(1,30,B), integer(A), integer(B), C is A*B.
my_mult(A,B,C) :- nonvar(A), nonvar(C), between(1,30,A),between(1,30,B), \+(A=0), integer(A), integer(C), B is C/A.
my_mult(A,B,C) :- nonvar(C), nonvar(B), between(1,30,A),between(1,30,B), \+(B=0), integer(B), integer(C), A is C/B.



nlist([]).
nlist([_|_]).


% definition to use during lazy evaluation
my_leq([P,N],Value):-
      !,
      sort_values(ascending,N,N1),
      sort_values(descending,P,P1),
      find_threshold_leq(P1,N1,Value).
% definition to use during normal evaluation
my_leq(X,Value):-
      number(X), number(Value), !,
      X =< Value.
% definition to use during construction of bottom clause
my_leq(X,X).


find_threshold_leq([P|Ps],[N|Ns],X):-
      N =< P, !,
      find_threshold_leq(Ps,[N|Ns],X).
find_threshold_leq([P|Ps],[N|Ns],X):-
      X is (P+N)/2.

% definition to use during lazy evaluation
my_geq([P,N],Value):-
      !,
      sort_values(descending,N,N1),
      sort_values(ascending,P,P1),
      find_threshold_geq(P1,N1,Value).
% definition to use during normal evaluation
my_geq(X,Value):-
      number(X), number(Value), !,
  X >= Value.
% definition to use during construction of bottom clause
my_geq(X,X).


find_threshold_geq([P|Ps],[N|Ns],X):-
      P =< N, !,
      find_threshold_geq(Ps,[N|Ns],X).
find_threshold_geq([P|_],[N|_],X):-
      X is (P+N)/2.


% definition to use during lazy evaluation
my_lt([P,N],Value):-
      !,
      sort_values(ascending,N,N1),
      sort_values(descending,P,P1),
      find_threshold_lt(P1,N1,Value).
% definition to use during normal evaluation
my_lt(X,Value):-
      number(X), number(Value), !,
      X >= Value.
% definition to use during construction of bottom clause
my_lt(X,X).

find_threshold_lt([P|Ps],[N|Ns],X):-
      N < P, !,
      find_threshold_lt(Ps,[N|Ns],X).
find_threshold_lt([P|_],[N|_],X):-
      X is (P+N)/2.

% definition to use during lazy evaluation
my_gt([P,N],Value):-
      !,
      sort_values(descending,N,N1),
      sort_values(ascending,P,P1),
      find_threshold_gt(P1,N1,Value).
% definition to use during normal evaluation
my_gt(X,Value):-
      number(X), number(Value), !,
  X >= Value.
% definition to use during construction of bottom clause
my_gt(X,X).

find_threshold_gt([P|Ps],[N|Ns],X):-
      P < N, !,
      find_threshold_gt(Ps,[N|Ns],X).
find_threshold_gt([P|_],[N|_],X):-
      X is (P+N)/2.



sort_values(ascending,L,L1):-
      sort(L,L1).
sort_values(descending,L,L1):-
      sort(L,L0),
      reverse(L0,L1).




%my_geq(A,A).
my_geq(A,B) :- nonvar(B), !, A>=B.
%my_leq(A,A).
my_leq(A,B) :- nonvar(B), !, A=<B.
%my_gt(A,A).
my_gt(A,B) :- nonvar(B), !, A>B.
%my_lt(A,A).
my_lt(A,B) :- nonvar(B), !, A<B.

my_add(A,B,C) :- nonvar(A), nonvar(B), integer(A), integer(B),between(1,30,A),between(1,30,B), C is A+B.
my_add(A,B,C) :- nonvar(A), nonvar(C), integer(A), integer(C), between(1,30,A),between(1,30,B),B is C-A.
my_add(A,B,C) :- nonvar(C), nonvar(B), integer(B), integer(C), between(1,30,A),between(1,30,B),A is C-B.
my_mult(A,B,C) :- nonvar(A), nonvar(B), integer(A), integer(B), between(1,30,A),between(1,30,B),C is A*B.
my_mult(A,B,C) :- nonvar(A), nonvar(C), integer(A), integer(C), between(1,30,A),between(1,30,B),\+(A=0.0), \+(A=0), B is C/A.
my_mult(A,B,C) :- nonvar(C), nonvar(B), integer(B), integer(C), between(1,30,A),between(1,30,B),\+(A=0.0), \+(A=0), A is C/B.


% Simple illustration of positive-only learning within Aleph
% To run do the following:
%       a. Load Aleph
%       b. read_all(animals).
%       c. sat(1).
%       d. reduce.
%	or
%       a. Load Aleph
%       b. read_all(animals).
%       c. induce.

/** <examples>
?- induce(Program).
*/
:-end_bg.

%:- begin_in.

% Training #1
begin(model(0)).
pos.
piece(0,obj_515_t_a79310a0_trn_0_in). 
lhs(obj_515_t_a79310a0_trn_0_in). 
cenGX(obj_515_t_a79310a0_trn_0_in,1).
cenGY(obj_515_t_a79310a0_trn_0_in,1). 
color(obj_515_t_a79310a0_trn_0_in,cyan). 
size(obj_515_t_a79310a0_trn_0_in,4).
shape(obj_515_t_a79310a0_trn_0_in,sid1111).

piece(0,obj_737_t_a79310a0_trn_0_out). 
rhs(obj_737_t_a79310a0_trn_0_out). 
cenGX(obj_737_t_a79310a0_trn_0_out,1). 
cenGY(obj_737_t_a79310a0_trn_0_out,2). 
color(obj_737_t_a79310a0_trn_0_out,red). 
size(obj_737_t_a79310a0_trn_0_out,4). 
shape(obj_737_t_a79310a0_trn_0_out,sid1111).
end(model(0)).

% Training #2
begin(model(1)).
pos.
pos(1).
piece(1,obj_592_t_a79310a0_trn_1_in). 
lhs(obj_592_t_a79310a0_trn_1_in). 
cenGX(obj_592_t_a79310a0_trn_1_in,2). 
cenGY(obj_592_t_a79310a0_trn_1_in,1). 
color(obj_592_t_a79310a0_trn_1_in,cyan). 
size(obj_592_t_a79310a0_trn_1_in,1). 
shape(obj_592_t_a79310a0_trn_1_in,sid2222).

piece(1,obj_40_t_a79310a0_trn_1_out). 
rhs(obj_40_t_a79310a0_trn_1_out). 
cenGX(obj_40_t_a79310a0_trn_1_out,2). 
cenGY(obj_40_t_a79310a0_trn_1_out,2). 
color(obj_40_t_a79310a0_trn_1_out,red). 
size(obj_40_t_a79310a0_trn_1_out,1). 
shape(obj_40_t_a79310a0_trn_1_out,sid2222).
end(model(1)).


% Training #3
begin(model(2)).
pos.
piece(2,obj_107_t_a79310a0_trn_2_in). 
lhs(obj_107_t_a79310a0_trn_2_in). 
cenGX(obj_107_t_a79310a0_trn_2_in,2). 
cenGY(obj_107_t_a79310a0_trn_2_in,2). 
color(obj_107_t_a79310a0_trn_2_in,cyan). 
size(obj_107_t_a79310a0_trn_2_in,3). 
shape(obj_107_t_a79310a0_trn_2_in,sid1111).

piece(2,obj_354_t_a79310a0_trn_2_out). 
rhs(obj_354_t_a79310a0_trn_2_out). 
cenGX(obj_354_t_a79310a0_trn_2_out,2). 
cenGY(obj_354_t_a79310a0_trn_2_out,3). 
color(obj_354_t_a79310a0_trn_2_out,red). 
size(obj_354_t_a79310a0_trn_2_out,3). 
shape(obj_354_t_a79310a0_trn_2_out,sid1111).
end(model(2)).


% Test #1
piece(10,obj_105_t_a79310a0_tst_0_in). 
lhs(obj_105_t_a79310a0_tst_0_in). 
cenGX(obj_105_t_a79310a0_tst_0_in,2). 
cenGY(obj_105_t_a79310a0_tst_0_in,1). 
color(obj_105_t_a79310a0_tst_0_in,cyan). 
size(obj_105_t_a79310a0_tst_0_in,4).
shape(obj_105_t_a79310a0_tst_0_in,sid3333).

rhs(obj_245_t_a79310a0_tst_0_out).
% must predict output
/*
piece(10,obj_245_t_a79310a0_tst_0_out). 
cenGX(obj_245_t_a79310a0_tst_0_out,2). 
cenGY(obj_245_t_a79310a0_tst_0_out,2). 
color(obj_245_t_a79310a0_tst_0_out,red). 
size(obj_245_t_a79310a0_tst_0_out,4). 
shape(obj_245_t_a79310a0_tst_0_out,sid3333).
*/

%:- end_in.
:-end_bg.

%:-begin_in_pos.
zendo(0).
zendo(1).
zendo(2).
zendo(10).
%:-end_in_pos.
:-begin_in_neg.

:-end_in_neg.



%:- fixup_exports.

%:- module(user).
%:- user:ensure_loaded('../kaggle_arc').
%:- module(aleph_sample).
:- wdmsg("% try:  ?- aleph_sample:induce_tree(Program),!,pp(Program). ").

aa:- nodebug,notrace,cls,(aleph_sample:induce_tree(Program)),notrace,!,pp(Program).

end_of_file.


% Simple illustration of constructing tree-based models within Aleph
% To run do the following:
%       a. Load Aleph
%       b. read_all(wedge).
%       c. induce_tree.

% Model trees are constructed by specifying a predicate that
% will be used for model construction for examples in a leaf.
% The user has to provide a definition for this predicate that
% is able to: (a) construct the model; and (b) predict using
% the model constructed. The trick used is the equal as that
% for lazy evaluation.

% Learning a model tree
% The function to be learnt is:
%             y = f(x) =  x + 1 (x =< 0)
%                      = -x + 1 ( x > 0)
% That is:
%
%                 |
%                /|\
%               / | \
%              /  |  \
%      --------------------------
%                 0

% what Aleph actually learns with the data given is:
%             y = f(x) =  x + 1 (x =< -0.5)
%                      = -x + 1 ( x > -0.5)
% adding more examples rectifies this: see wedge.f


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% specify tree type

:- aleph_set(tree_type,model).
:- aleph_set(evalfn,mse).
:- aleph_set(minpos,2).       % minimum examples in leaf for splitting
:- aleph_set(mingain,0.01).	% toy example needs this to be low
:- aleph_set(dependent,2).	% second argument of f/2 is to predicted
:- aleph_set(verbosity,10).
%:- aleph_set(mingain,-1e10).

% specify predicate definition to use for model construction
:- model(predict/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mode declarations

:- modeh(1,f(+x,-y)).
:- modeb(1,lteq(+x,#threshold)).
:- modeb(1,predict(+x,-y,#params)).

:- determination(f/2,lteq/2).
:- determination(f/2,predict/3).

:-begin_bg.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type definitions

threshold(-0.5).
threshold(0.0).
threshold(0.5).

params([_Slope,_Constant,_Sd]).

list([_|_]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Background

lteq(X,Y):-
      var(Y), !,
      X = Y.
lteq(X,Y):-
      number(X), number(Y),
      X =< Y.

% definition for model construction (parameter estimation)
predict(X,Y,[M,C,Sd]):-
list(X),
list(Y), !,
l_regress1(Y,X,M,C,Sd).
% definition for prediction
predict(X,Y,[M,C,_]):-
number(X), var(Y), !,
Y is M*X + C.
% definition for model checking
predict(X,Y,[M,C,Sd]):-
number(Y), number(X), !,
Y1 is M*X + C,
Diff is Y - Y1,
abs_val(Diff,ADiff),
ADiff =< 3*Sd.

% very simple univariate linear regression
l_regress1([YVals|_],[XVals|_],M,C,0.0):-
      YVals = [Y1,Y2|_],
      XVals = [X1,X2|_],
      M is (Y2-Y1)/(X2-X1),
      C is Y1 - M*X1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constraints

% remove redundant checks for =<
% prune((_:-Body)):-
% 	has_literal(lteq(X,Y),Body,Left),
% 	has_literal(lteq(X1,Y1),Left,_),
% 	X == X1,
% 	Y1 =< Y.
% 
% has_literal(L,(L,L1),L1).
% has_literal(L,(_,L1),Left):-
% 	!,
% 	has_literal(L,L1,Left).
% has_literal(L,L,true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities

abs_val(X,Y):- X < 0, !, Y is -X.
abs_val(X,X):- X >= 0.

:-end_bg.
:-begin_in_pos.

f(-1.0,0.0).
f(-0.5,0.5).
% f(-0.25,0.75).	% adding this results in the correct theory
f(0.0,1.0).
f(0.5,0.5).
f(1.0,0.0).

:-end_in_pos.
:-begin_in_neg.
:-end_in_neg.




