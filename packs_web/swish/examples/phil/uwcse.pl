/* Sample example for performing inference on hierachical probabilistic logic program. 
The program is inspired from UWCSE dataset from Kok S, Domingos P (2005) 
Learning the structure of Markov Logic Networks. In:
Proceedings of the 22nd international conference on Machine learning, ACM, pp
441-448


      Arnaud Nguembang Fadja and Fabrizio Riguzzi. 
      Hierachical probabilistic logic programs
*/


/** <examples>
?- inference_hplp(advisedby(harry, ben),ai,Prob).  % Prob contains the probability that harry is advised by ben in the ai interpretation
?- inference_hplp(advisedby(harry, ben),ai,Prob,Circuit). % Same as the previous query but also returns in Circuit a term representing the arithmetic circuit
*/



:- use_module(library(phil)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(lpad).
:- endif.

:- phil.

:- set_hplp(verbosity, 1).


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


fold(all, [ai]).

output(advisedby/2).


input(student/1).
input(professor/1).
input(project/2).
input(publication/3).
input(taughtby/2).
input(ta/2).



% data in model format
begin(model(ai)).
student(harry).
professor(ben).

taughtby(c1, ben).
taughtby(c2, ben).
ta(c1, harry). 
ta(c2, harry).

project(harry, pr1). 
project(harry, pr2).
project(ben, pr1). 
project(ben, pr2).
publication(p1, harry, pr1).
publication(p2, harry, pr1).
publication(p3, harry, pr2).
publication(p4, harry, pr2).
publication(p1, ben, pr1).
publication(p2, ben, pr1).
publication(p3, ben, pr2).
publication(p4, ben, pr2).
end(model(ai)).
