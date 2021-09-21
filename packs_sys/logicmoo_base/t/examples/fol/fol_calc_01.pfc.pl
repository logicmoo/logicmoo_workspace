/*

 CALCULATOR WORLD

[13:28] * dmiles attempts to write a program in FOL today! just a simple 14 key caclulator
[13:29] <dmiles> just to discover the hellishness
[13:29] <anniepoo_> the calculator believes that X is 7?
[13:31] <dmiles> yes
[13:31] <dmiles> well the other parts is believing the key buffer represents cerain values and so forth
[13:32] <dmiles> and even believing there are keys to be pressed

  part of why i doing the calculator (outside of a test case for logicmoo/prologmud).. is it has been belived that programs might be hard to express physically

  Load with  ?- consult(pack(logicmoo_base/t/examples/fol/'fol_calc_01.pfc'))
*/

:- include(test_header).

:- set_prolog_flag(gc,false).


% =================================================================================
% Set our engine up
% =================================================================================

:- expects_dialect(clif).
% deduce instances from usages in args having the effect of deducing human,dwelling,beverage_class are classes
==> feature_setting(make_wff,true).
==> feature_setting(add_admitted_arguments,true).
% set truth maintainance system to remove previous assertions that new assertions disagree with 
==> feature_setting(tms_mode,remove_conflicting).
:- set_prolog_flag(runtime_debug,3). % mention it when we remove previous assertions
:- set_prolog_flag_until_eof(do_renames,mpred_expansion).
:- kif_compile.

% =================================================================================
% Begin program
% =================================================================================

% Create out 14 keys
exists(Key,glythed(Key,"1")). exists(Key,glythed(Key,"2")).
exists(Key,glythed(Key,"3")). exists(Key,glythed(Key,"4")).
exists(Key,glythed(Key,"5")). exists(Key,glythed(Key,"6")).
exists(Key,glythed(Key,"7")). exists(Key,glythed(Key,"8")).
exists(Key,glythed(Key,"9")). exists(Key,glythed(Key,"0")).
exists(Key,glythed(Key,"+")). exists(Key,glythed(Key,"-")).
exists(Key,glythed(Key,"=")). exists(Key,glythed(Key,"CLR")). 

:- kif_compile.

:- listing(glythed).

% Let same/2 be our unification identify 
all(X,same(X,X)).

:- listing(same/2).

% Make each key unique depending on its label
all([Key1,Key2,Label1,Label2],
  glythed(Key1,Label1) & glythed(Key2,Label2) & different(Label1,Label2) => different(Key1,Key2)).

% same/2 implies not differnt
all([X,Y], same(X,Y) => ~ different(X,Y)).

% Note all unquantified vars will be universal from here on out
subclass(S,C) & inst(I,S) => inst(I,C).

% an accumulator is a numeric buffer
subclass(accumulator,numbuffer).

% a display is a numeric buffer
subclass(display,numbuffer).

% numeric buffer and operation buffer are buffers
subclass(numbuffer,buffer). subclass(opbuffer,buffer).

% Create some buffers
exists(X,inst(X,accumulator)).
exists(X,inst(X,display)).
exists(X,inst(X,opbuffer)).

/*
Extra TODO ?

define how a buffer is shown in screen locations
exactly(10,X, inst(X,screenLocation)).

*/

% All number buffers have a, inital value of 0
(all(X,inst(X,numbuffer) => initalValue(X,0))).

% exists an initial world 
exists(C,initialWorld(C)).

% initial world is populated by inital values
initalValue(N,V) & initialWorld(W) =>  currentValue(W,N,V).

% All numeric buffers have a value
all(W,all(N,exists(V,inst(N,numbuffer) & world(W) => currentValue(W,N,V)))).

% Op buffer is intialized to null
all(X,inst(X,opbuffer) => initalValue(X,null)).

% exists a current world (Fluent 1)
exists(C,currentWorld(C)).

%:- rtrace.
nextWorld(C,N) => inst(C,world) & inst(N,world).
:- nortrace.

:- \+ current_predicate(nextWorld/2) -> true;
   (listing(nextWorld/2), break).

% CTL or LTL version of automation?

onEvent(Evt,Props) =>
 currentWorld(C),
   exists(W,
      inst(W,world)
    & nextWorld(C,W)
    &  true_in_world(Props,W)).
    


% after "CLR" is pressed re_init the buffers
onEvent(pressed("CLR"), all([N,V], initalValue(N,V) => currentValue(N,V))).

% after "=" is pressed the "accumulator" buffer is shown.

% after "+" or "_" is pressed the  

:- test_boxlog((non_empty_world(W) <=> 
 exists([W,Prop],
     (true_in_world(Prop,W))))).

% :- break.

end_of_file.

:- break.


my_minus_one(N,M) <=>  (N > 1)  &  (M = N-1).

% Lits in KBs are considered true
consistent(W) <=> min_lits(1, W) .

exactly_N_lits(1,W) <=>
 exists([W,Prop],  
    (   true_in_world(Prop,W) 
      & ~exists(Other_Prop,
           (  true_in_world(Other_Prop,W) 
             & ~equiv(W,Other_Prop,Prop))))) .

exactly_N_lits(2,W) <=>
 exists([W,Prop1,Prop2],  
  (  true_in_world(Prop1,W) 
   & true_in_world(Prop2,W) 
   & ~equiv(W,Prop1,Prop2)   
   & ~ exists(Other_Prop, 
         (  true_in_world(Other_Prop,W) 
          & ~equiv(W,Other_Prop,Prop1) 
          & ~equiv(W,Other_Prop,Prop2)))))

exactly_N_lits(N,W) <=> 
   range_lits(N,N,W).  


range_lits(N,M,W) <=> 
 exists(W,
  ( min_lits(N,W) 
    & max_lits(M,W))) .

min_lits(1,W) <=> 
 exists([W,Prop],
     (true_in_world(Prop,W))).

min_lits(2,W) <=>
 exists([W,Prop1,Prop2], 
   (  true_in_world(Prop1,W) 
    & true_in_world(Prop2,W)
    & ~equiv(W,Prop1,Prop2))).

min_lits(4,W) <=> 
  exists([W1,W2],
     min_lits(2,W1) 
   & min_lits(2,W2)
   & union_disjoint_truths(W1,W2)).

min_lits(N,W) <=> 
  exists([W1,W2],
     min_lits(1,W1) 
   & min_lits(M,W2)
   & consistent(W2)
   & union_disjoint_truths(W1,W2)
   & my_minus_one(N,M)).


max_lits(2,W) <=>  
exists([W,Prop1,Prop2], 
   (  true_in_world(Prop1,W) 
    & true_in_world(Prop2,W) 
    &  ~exists(Other_Prop,
          (  true_in_world(Other_Prop,W) 
            & ~equiv(W,Other_Prop,Prop1)
            & ~equiv(W,Other_Prop,Prop2))))).

% exists a W with no lits?
exactly_N_lits(0,W) <=>
  exists(W,
     ~exists(Prop1, true_in_world(Prop1,W))).

max_lits(1,W) <=>
 exactly_N_lits(0,W) v exactly_N_lits(1,W).

difference_at_least_1_truths(W1,W2) <=>
  exists(Prop, (true_in_world(Prop,W1) => ~true_in_world(Prop,W2))) .

disjoint_truths(W1,W2) <=>
  ~exists(Prop, (true_in_world(Prop,W1) & true_in_world(Prop,W2))).

subset_truths(W1,W2) <=> 
    all(Prop,
      true_in_world(Prop,W1) => true_in_world(Prop,W2) ).

union_s_v2(W1,W2,W) <=>
 all(Prop,
   (true_in_world(Prop,W) <=> 
       (true_in_world(Prop,W1) v true_in_world(Prop,W2)))).

union_truths(W1,W2,W) <=>
  exists(KB,
      union_truths(W1,W2,KB)
     & ~difference_at_least_1_truths(W,KB)).


union_disjoint_truths(W1,W2,W) <=> 
   (   union_truths(W1,W2,W) 
     & disjoint_truths(W1,W2)).



max_lits(N,W) <=> 
  exists([W1,W2],
     max_lits(1,W1) 
   & max_lits(M,W2)
   & consistent(W2)
   & union_disjoint_truths(W1,W2)
   & my_minus_one(N,M)).


equal_v1_truths(W1,W2) <=>
   ( ~difference_at_least_1_truths(W1,W2)
     & ~difference_at_least_1_truths(W2,W1)
     & consistent(W1)
     & consistent(W2)).

equal_v2_truths(W1,W2) <=>
 ( consistent(W1)
   & all(Prop, 
      (true_in_world(Prop,W1) <=> true_in_world(Prop,W2)))).


% v1: lameness
equiv_v1(W,Prop1,Prop2) <=>
  true_in_world(Prop1,W) <=>   true_in_world(Prop2,W).

% v2  Prop2 and Prop2 logically "imply" each other in the W.
equiv_v2(W,Prop1,Prop2) <=>
  true_in_world((Prop1 <=> Prop2),W).

% v3 the actual semantics implemented based that both logical sentences use the same proof
equiv(W,Prop1,Prop2) <=>
  equiv_v4(W,Prop1,Prop2) v equiv_v2(W,Prop1,Prop2).

% v4 sameness (current implementation in LogicMOO )
equiv_v4(W,X,Y) <=> 
 same(X,Y) &
 all(X, 
  exists(W, 
   true_in_world(X,W))).
                                              

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/69 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/fol_calc_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/FOL_CALC_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AFOL_CALC_01 

