:- include(test_header).





 

:- begin_pfc.

:- process_this_script.

%= save compiled clauses using forward chaining storage (by default)
%= we are using forward chaining just so any logical errors, performance and program bugs manefest
%= immediately
:- set_clause_compile(fwc).

must_is_entailed(G):- must(is_entailed_u(G)).


show_test(G):- defaultAssertMt(KB),printAll(must(KB:G)).


%= ````
%= logic tests...
%= ````
%:- debug_logicmoo(logicmoo(_)).

mpred_prop(otherGender,2,prologBuiltin).
otherGender(male,female).
otherGender(female,male).

tCol(male).
:- dynamic((bore_offspring/2, gender/2)).

bore_offspring(P2,P1) <=> bore_offspring(P1,P2).

(bore_offspring(C,P),gender(P,G1), otherGender(G1,G2))
     => gender(C,G2).

gender(P,male) <=> male(P).
gender(P,female) <=> female(P).

male(P) <=> ~female(P).


% seven rules
(((parent(C,M) & female(M)) <=> mother(C,M))).


parent(PARENT,GRAND),parent(CHILD,PARENT) => grandparent(CHILD,GRAND).

grandparent(C,G),male(G) <=> grandfather(C,G).
grandparent(C,G),female(G) <=> grandmother(C,G).
mother(P,G),parent(GrandKid,P)=>grandmother(GrandKid,G).
father(P,G),parent(GrandKid,P)=>grandfather(GrandKid,G).


parent(C,P),male(P) <=> father(C,P).
parent(C,P),female(P) <=> mother(C,P).
parent(X,P),parent(Y,P),different(X,Y) =>siblings(X,Y).
parent(C,P) => ancestor(C,P).
ancestor(C,P), ancestor(P,G) => ancestor(C,G).

mother(douglas,eileen).

%= trudy is human
human(trudy).

%= catch a regression bug that may couse trudy to lose human assertion
never_retract_u(human(trudy)).
never_retract_u(human(eileen)).

:- mpred_run.

:- debug_logicmoo(_).
:- mpred_trace_exec.

%  
forall(c,exists([m,f], if(human(c), (mother(c,m) & father(c,f))))).

:- mpred_run.

:- printAll(must(father(trudy,_))).

:- mpred_run.

% ((human(P1),ancestor(P1,P2))=>human(P2)).

clif(((human(P1),ancestor(P2,P1))=>human(P2))).

:- ain(mother(eileen,trudy)).
:- printAll(must(mother(eileen,_))).
:- printAll(must(ancestor(eileen,_))).

% lol.. i have a frame problm in my solver.. whenever     forall(c,exists([m,f], if(human(c), (mother(c,m) & father(c,f))))).  ((human(P1),ancestor(P2,P1))=>human(P2)). human(douglas). I now have a skolemation of a mother who is human .. i my mothers children are human (thus i am) .. but while i am adding a named mother.. the skolem mother loses her assertion and is deduced nonhuman 
