
lt(A,B):- A < B.
type_restriction(lt(A,B),[number(A),number(B)]).


ex(merge([],[],[]),'+').
ex(merge([],[2],[2]),'+'). 
ex(merge([],[3,4],[3,4]),'+'). 
ex(merge([5],[],[5]),'+'). 
ex(merge([2,3],[],[2,3]),'+'). 



ex(merge([1],[2],[1,2]),'+').
ex(merge([0,1],[3,4],[0,1,3,4]),'+').
ex(merge([1],[3,4],[1,3,4]),'+').

ex(merge([5],[4],[4,5]),'+').
ex(merge([6,7],[3,4],[3,4,6,7]),'+').
ex(merge([6,7],[4],[4,6,7]),'+').


ex(merge([2,3,4,6],[5,7],[2,3,4,5,6,7]),'+'). 
ex(merge([43,55,63],[22,33,44,53],[22,33,43,44,53,55,63]),'+'). 
ex(merge([29,39,49,59],[37,79,99],[29,37,39,49,59,79,99]),'+').
ex(merge([2],[4,7],[2,4,7]),'+').



ex(merge([],[],[3]),'-').
ex(merge([],[2],[1,2]),'-'). 
ex(merge([],[3,4],[4,3]),'-'). 
ex(merge([5,6],[],[5]),'-'). 
ex(merge([2,3],[],[1,2,3]),'-'). 



ex(merge([1],[2],[2,1]),'-').
ex(merge([0,1],[3,4],[0,1,4]),'-').
ex(merge([1],[3,4],[4,1,1,3]),'-').

ex(merge([5],[1,4],[4,1,5]),'-').
ex(merge([6,7],[3,4],[3,6,4,7]),'-').
ex(merge([6,7],[4],[6,7,4]),'-').

end_of_file.

| ?- clear_kb, do_full_kb('examples/ex9.pl').

try:
:- clear_kb, init_kb('examples/ex9.pl').
% file "/tmp_mnt/home/stahl/edl/framework/miles/examples/ex9.pl" consulted.
:- store_clause(merge([],[2],[2]),_,user,28).
% rule added.
:- store_clause(merge([5],[],[5]),_,user,29).
% rule added.
:- store_clause(merge([1],[2],[1,2]),_,user,30).
% rule added.
:- store_clause(merge([0,1],[3,4],[0,1,3,4]),_,user,31).
% rule added.
:- store_clause(merge([1],[3,4],[1,3,4]),_,user,32).
% rule added.
:- store_clause(merge([5],[4],[4,5]),_,user,33).
% rule added.
:- store_clause(merge([6,7],[3,4],[3,4,6,7]),_,user,34).
% rule added.
:- store_clause(merge([6,7],[4],[4,6,7]),_,user,35).
% rule added.
:- flatten_rules.
% yes
:- saturate(30,Xmout1,5).
% yes
% rule 36 created.
:- saturate(31,Xmout1,5).
% yes
% rule 37 created.
:- saturate(33,Xmout1,5).
% yes
% rule 38 created.
:- saturate(34,Xmout1,5).
% yes
% rule 39 created.
:- unflatten_kb.
% yes
:- lgg(36,37,Xmout1).
% yes
% rule 40 created.
:- lgg(38,39,Xmout1).
% yes
% rule 41 created.
:- delete_clause(36).
:- delete_clause(37).
:- delete_clause(38).
:- delete_clause(39).
% selected rules deleted.
:- delete_clause(28).
:- delete_clause(29).
:- delete_clause(30).
:- delete_clause(31).
:- delete_clause(32).
:- delete_clause(33).
:- delete_clause(34).
:- delete_clause(35).
% selected rules deleted.
:- argument_types.
% yes
:- clause_heads.
% yes
:- delete_clause(47).
:- delete_clause(48).
% selected rules deleted.
:- eval_examples.
% yes
:- complete_chk.
% yes
:- correct_chk.
% no
:- fp(Xmout1).
% yes
% resulting rules selected
:- refinement_add_body_literal(40,Xmout1).
% yes
:- eval_examples.
% yes
:- complete_chk.
% yes
:- correct_chk.
% no
:- fp(Xmout1).
% yes
% resulting rules selected
:- refinement_add_body_literal(41,Xmout1).
% yes
:- eval_examples.
% yes
:- correct_chk.
% yes
:- complete_chk.
% yes


