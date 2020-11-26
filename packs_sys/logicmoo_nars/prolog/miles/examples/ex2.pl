%%% clauses 1-4 define Rouveirols bg knowledge

column(X):- brick(X), standing(X), is_on(X,Y), ground(Y).

column(X):- brick(X), standing(X), is_on(X,Y), column(Y).

same_height(X,Y):- ground(X), ground(Y).

same_height(X,Y):- brick(X), standing(X), brick(Y), standing(Y), is_on(X,X1), is_on(Y,Y1), 
                   same_height(X1,Y1).


% the next 2 examples (5+6) show arches of different heights

arch(X):- part_of(A,X), part_of(B,X), part_of(C,X), is_on(A,B), is_on(A,C), is_on(B,D), 
          is_on(C,E), ground(D), ground(E), left_of(B,C), does_not_touch(B,C), lying(A), 
          wedge(A), standing(B), standing(C), brick(B), brick(C).


arch(X):- part_of(A,X), part_of(B,X), part_of(C,X), is_on(A,B), is_on(A,C), is_on(B,D),
          is_on(C,E), left_of(B,C),does_not_touch(B,C), lying(A), wedge(A), standing(B),
          standing(C), brick(B), brick(C),
          brick(D), brick(E), does_not_touch(D,E), standing(D), standing(E),
          is_on(D,F), is_on(E,G), ground(G), ground(F).


% the next 3 examples (7-9) show arches of different colors (-> lgg looks strange)

arch(X):- part_of(A,X), part_of(B,X), part_of(C,X), is_on(A,B), is_on(A,C), is_on(B,D), 
          is_on(C,E), ground(D), ground(E), left_of(B,C), does_not_touch(B,C), lying(A), 
          wedge(A), standing(B), standing(C), brick(B), brick(C), red(B), green(C).

arch(X):- part_of(A,X), part_of(B,X), part_of(C,X), is_on(A,B), is_on(A,C), is_on(B,D), 
          is_on(C,E), ground(D), ground(E), left_of(B,C), does_not_touch(B,C), lying(A), 
          wedge(A), standing(B), standing(C), brick(B), brick(C), green(B), red(C).

arch(X):- part_of(A,X), part_of(B,X), part_of(C,X), is_on(A,B), is_on(A,C), is_on(B,D), 
          is_on(C,E), ground(D), ground(E), left_of(B,C), does_not_touch(B,C), lying(A), 
          wedge(A), standing(B), standing(C), brick(B), brick(C),blue(B), red(C).


% some clauses (10-12) to test intra-construction

column(X):- brick(X), standing(X), is_on(X,Y), table(Y).

column(X):- block(X), standing(X), is_on(X,Y), ground(Y).

column(X):- block(X), standing(X), is_on(X,Y), column(Y).


end_of_file.


?- do_full_kb('examples/ex2.pl').

try      

| ?- clear_kb, init_kb('examples/ex2.pl').
| ?- show_kb.
| ?- intra_construct1(10,11,A,B,C).
| ?- show_clauses([10,11,13,14,15]).
| ?- g1_op(5,1,I).
| ?- g1_op(5,3,I).
| ?- absorb(5,1,I).
| ?- elem_saturate(5,1,I).
| ?- saturate(5,I,10).
| ?- most_spec_v(5,I,J).
| ?- inv_derivate(5,J).
| ?- show_kb.
| ?- lgg(7,9,J), show_clause(J).
| ?- nr_lgg(7,9,J), show_clause(J),
     get_clause(J,_,_,CL,_),reduce_complete(CL,CL1),
     store_clause(_,CL1,nrlgg,I), show_clause(I).

| ?- clear_kb, init_kb('examples/ex2.pl').
| ?- gen_msg(5,6,J,10),show_clause(J).
| ?- gti(8,9,J),show_clause(J).

| ?- clear_kb, init_kb('examples/ex2.pl').
| ?- rlgg(5,6,J),show_clause(J).

X-MILES Protocol:

:- clear_kb.
% knowledgebase cleared.
:- init_kb('examples/ex2.pl').
% file "/tmp_mnt/home/stahl/edl/framework/miles/examples/ex2.pl" consulted.
:- intra_construct1(10,11,Xmout1,Xmout2,Xmout3).
% yes
% rule 13 created.
% rule 14 created.
% rule 15 created.
:- g1_op(5,1,Xmout1).
% yes
% rule 16 created.
:- absorb(5,1,Xmout1).
% yes
% rule 17 created.
:- elem_saturate(5,1,Xmout1).
% yes
% rule 18 created.
:- saturate(5,Xmout1,5).
% yes
% rule 19 created.
:- most_spec_v(5,1,Xmout1).
% yes
% rule 20 created.
:- inv_derivate(5,Xmout1).
% yes
% rule 21 created.
:- lgg(7,9,Xmout1).
% yes
% rule 22 created.
:- nr_lgg(7,9,Xmout1).
% yes
% rule 23 created.
:- reduce_complete(23).
% yes
:- delete_clause(1).
:- delete_clause(2).
:- delete_clause(3).
:- delete_clause(4).
:- delete_clause(5).
:- delete_clause(6).
:- delete_clause(7).
:- delete_clause(8).
:- delete_clause(9).
:- delete_clause(10).
:- delete_clause(11).
:- delete_clause(12).
:- delete_clause(13).
:- delete_clause(14).
:- delete_clause(15).
:- delete_clause(16).
:- delete_clause(17).
:- delete_clause(18).
:- delete_clause(19).
:- delete_clause(20).
:- delete_clause(21).
:- delete_clause(22).
:- delete_clause(23).
% all rules deleted.
% all examples deleted.
:- clear_kb.
% knowledgebase cleared.
:- init_kb(/tmp_mnt/home/stahl/edl/framework/miles/examples/ex2.pl).
% file "/tmp_mnt/home/stahl/edl/framework/miles/examples/ex2.pl" consulted.
:- gen_msg(5,6,Xmout1).
% yes
% rule 15 created.
:- gti(8,9,Xmout1).
% yes
% rule 16 created.
:- delete_clause(1).
:- delete_clause(2).
:- delete_clause(3).
:- delete_clause(4).
:- delete_clause(7).
:- delete_clause(10).
:- delete_clause(11).
:- delete_clause(12).
:- delete_clause(15).
:- delete_clause(5).
:- delete_clause(6).
:- delete_clause(8).
:- delete_clause(9).
:- delete_clause(16).
% all rules deleted.
% all examples deleted.
:- clear_kb.
% knowledgebase cleared.
:- init_kb('examples/ex2.pl').
% file "/tmp_mnt/home/stahl/edl/framework/miles/examples/ex2.pl" consulted.
:- rlgg(5,6,Xmout1).
% yes
% rule 13 created.








