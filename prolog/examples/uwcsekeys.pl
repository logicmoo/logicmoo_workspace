/* Sample example of hierachical probabilistic logic program. inspired from UWCSE dataset from
Kok S, Domingos P (2005) Learning the structure of Markov Logic Networks. In:
Proceedings of the 22nd international conference on Machine learning, ACM, pp
441-448
*/


/** <examples>
?- inference_hplp(advisedby(harry, ben),ai,Prob).
?- inference_hplp(advisedby(harry, ben),ai,Prob,Circuit).
*/



:- use_module(library(phil)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(lpad).
:- endif.

:- phil.

:- set_hplp(verbosity, 1).
% Structure learning settings
:- set_hplp(megaex_bottom, 10). % max number of mega examples to considered in the generation of bottoms clauses
:- set_hplp(initial_clauses_per_megaex, 1).
:- set_hplp(rate, 1.0). % defines the probabilityu for going from the first layer to the second layer
:- set_hplp(max_layer, -1). % Define the max number of layer: -1 for the maximum depth possible 
:- set_hplp(min_probability, 1.0e-5).  % threshold value of the probability under which a clauses is dropped out

% Parameter learning settings
:- set_hplp(algorithmType, dphil). % parameter learning algorithm dphil or emphil
% Maximun iteration and other stop conditions.
:- set_hplp(maxIter_phil, 1000).  
:- set_hplp(epsilon_deep, 0.0001). 
:- set_hplp(epsilon_deep_fraction, 1.0e-5).
:- set_hplp(useInitParams, yes).

% regularization parameters 
:- set_hplp(regularized, no). % yes to enable regularization and no otherwise 
:- set_hplp(regularizationType, 2). % 1 for L1, 2 for L2 and 3 for L3. L3 available only for emphil
:- set_hplp(gamma, 10). % regularization strength
:- set_hplp(gammaCount, 0). 

% Adam parameter for dphil algorithm
:- set_hplp(adam_params, [0.1, 0.9, 0.999, 1.0e-8]). % adam(Eta,Beta1,Beta2,Epsilon_adam_hat)
% Gradient descent strategy and the corresponding batch size
:- set_hplp(batch_strategy, minibatch(50)).
%:- set_hplp(batch_strategy,stoch_minibatch(10)).
%:- set_hplp(batch_strategy,batch).

  
bg([]).

:- begin_in.
advisedby(A,B):0.3:-
    student(A),
    professor(B),
    project(A,C),
    project(A,C),
    hidden_1(A,B,C).
advisedby(A,B):0.6 :-
    student(A),
    professor(B),
    ta(C,A),
    taughtby(C, B).
hidden_1(A,B,C):0.2 :-
    publication(P, A, C),
    publication(P, B, C).
:- end_in.


fold(ai, [ai]).

output(advisedby/2).


input(student/1).
input(professor/1).
input(project/2).
input(publication/3).
input(taughtby/2).
input(ta/2).


determination(advisedby/2, professor/1).
determination(advisedby/2, student/1).
determination(advisedby/2, publication/3).
determination(advisedby/2, taughtby/2).
determination(advisedby/2, ta/2).
determination(advisedby/2, project/2).

modeh(*, advisedby(+person, +person)).


modeb(*, publication(-title, +person, +project)).
%modeb(*, publication(+title, -person, +project)).
modeb(*, project(+person,-project)).
%modeb(*, project(-person, +project)).
modeb(*, professor(+person)).
modeb(*, student(+person)).
modeb(*, taughtby(-course, +person)).
%modeb(*, taughtby(+course, -person)).
modeb(*, ta(-course, +person)).
%modeb(*, ta(-course, +person)).


% data
advisedby(ai, harry, ben).
student(ai, harry).
professor(ai, ben).

taughtby(ai, c1, ben).
taughtby(ai, c2, ben).
ta(ai, c1, harry). 
ta(ai, c2, harry).

project(ai, harry, pr1). 
project(ai, harry, pr2).
project(ai, ben, pr1). 
project(ai, ben, pr2).
publication(ai, p1, harry, pr1).
publication(ai, p2, harry, pr1).
publication(ai, p3, harry, pr2).
publication(ai, p4, harry, pr2).
publication(ai, p1, ben, pr1).
publication(ai, p2, ben, pr1).
publication(ai, p3, ben, pr2). 
publication(ai, p4, ben, pr2).
