:- include(test_header).





 

:- begin_pfc.

% :- process_this_script.

%= save compiled clauses using forward chaining storage (by default)
%= we are using forward chaining just so any logical errors, performance and program bugs manefest
%= immediately
:- set_clause_compile(fwc).

must_is_entailed(G):- cwc, must(is_entailed_u(G)).

show_test(G):- cwc, defaultAssertMt(KB),printAll(must(KB:G)).

%= ````
%= logic tests...
%= ````
%:- debug_logicmoo(logicmoo(_)).

prologBuiltin(otherGender/2).
otherGender(male,female).
otherGender(female,male).

tCol(male).
:- dynamic((bore_offspring/2, gender/2)).
bore_offspring(C,P) <=> bore_offspring(C,P).

(bore_offspring(C,P),gender(P,G1), otherGender(G1,G2))
     => gender(C,G2).


gender(P,male) <=> male(P).
gender(P,female) <=> female(P).

male(P) <=> ~female(P).



% seven rules
(((parent(C,M) & female(M)) <=> mother(C,M))).
==> clif(((parent(C,M) & female(M)) <=> mother(C,M))).


%((parent(C,M) & female(M)) <=> mother(C,M)).
:- must_is_entailed(((parent(C,M) & female(M)) <=> mother(C,M))).


:- must_is_entailed(((parent(C,M) & female(M)) => mother(C,M))).
%       [ (not(female(M)):-not(mother(C,M)), parent(C,M)),
%         (not(parent(C,M)):-not(mother(C,M)), female(M)),
%         (mother(C,M):-parent(C,M), female(M))
%       ].

:- must_is_entailed((mother(C,M) => (parent(C,M) & female(M)))).
%       [ (female(M):-mother(M, _C)),
%         (not(mother(_C,M)):-not(female(M))),
%         (not(mother(C,M)):-not(parent(C,M))),
%         (parent(C,M):-mother(C,M))
%       ].


:- must_is_entailed(not(mother(C,M)):- not(parent(C,M))).
:- must_is_entailed(not(mother(_Anyone, M)):- not(female(M))).
:- must_is_entailed((parent(C,M):- mother(C,M))).
:- must_is_entailed((female(M):- mother(_,M))).




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

:- nodebug_logicmoo(_).
% :- mpred_trace_exec.

% :-  debug(foo).

%  
forall(c,exists([m,f], if(human(c), (mother(c,m) & father(c,f))))).

:- mpred_run.

:- printAll(must(father(trudy,_))).

:- mpred_run.

:- mpred_trace_exec.
% ((human(P1),ancestor(P1,P2))=>human(P2)).
((human(P1),ancestor(P2,P1))=>human(P2)).

:- mpred_notrace_exec.

:- (ain(mother(eileen,trudy))).
:- printAll(must(mother(eileen,_))).
:- printAll(must(ancestor(eileen,_))).

% lol.. i have a frame problm in my solver.. whenever     forall(c,exists([m,f], if(human(c), (mother(c,m) & father(c,f))))).  ((human(P1),ancestor(P2,P1))=>human(P2)). human(douglas). I now have a skolemation of a mother who is human .. i my mothers children are human (thus i am) .. but while i am adding a named mother.. the skolem mother loses her assertion and is deduced nonhuman 

% :- listing([ancestor,human,parent]).
%=:- wdmsg("press Ctrl-D to resume.").
%=:- prolog.

% grandmother(douglas,trudy).

mother(robby,trudy).
mother(liana,trudy).
mother(matt,liana).
mother(liz,liana).
mother(pam,trudy).


%= human(trudy) supports anscesteral rule that her decendants are humans as well .. therefore ..
:- must_is_entailed(human(douglas)).


?- mpred_why(human(douglas)).

/*


Justifications for human(douglas):
    1.1 ancestor(douglas,eileen)
    1.2 human(eileen)
    1.3 human(_G45653),ancestor(_G45658,_G45653),{is_unit(_G45658)}==>human(_G45658)
    2.1 ancestor(douglas,trudy)
    2.2 human(trudy)
    2.3 human(_G45616),ancestor(_G45621,_G45616),{is_unit(_G45621)}==>human(_G45621)

*/

:- mpred_why(grandparent(douglas,trudy)).

/*

Justifications for grandparent(douglas,trudy):
    1.1 parent(douglas,eileen)
    1.2 parent(eileen,trudy)
    1.3 parent(_G11217,_G11218),parent(_G11223,_G11217),{is_unit(_G11218,_G11223)}==>grandparent(_G11223,_G11218)
    2.1 grandmother(douglas,trudy)
    2.2 grandmother(_G6160,_G6161),{is_unit(_G6161,_G6160)}==>grandparent(_G6160,_G6161)

*/

%=:- wdmsg("press Ctrl-D to resume.").


:- style_check(-singleton).


%= so far no males "asserted" in the KB

:- show_test(male(Who)).

/*
OUTPUT WAS..
male(skArg1ofFatherFn(pam)).
male(skArg1ofFatherFn(liz)).
male(skArg1ofFatherFn(matt)).
male(skArg1ofFatherFn(liana)).
male(skArg1ofFatherFn(robby)).
male(skArg1ofFatherFn(eileen)).
male(skArg1ofFatherFn(douglas)).
male(skArg1ofFatherFn(trudy)).
*/

%= we can report the presence on non male though...
%=    the ~/1 is our negation hook into the inference engine
:- no_varnaming( mpred_nochaining(doall((show_call(~male(Who )))))).


%= we expect to see at least there mothers here
%=  succeed(user: ~male(liana)).
%=  succeed(user: ~male(trudy)).
%=            succeed(user: ~male(skArg1ofMotherFn(trudy))).
%=  succeed(user: ~male(eileen)).

%= thus ~/1 is tnot/1 of XSB ?!?

%= there are explicly non females
:- show_test(~(female(Who))).

%= ensure skolems are made or destroyed

father(eileen,robert).

:- sanity(\+ baseKB:father( eileen, skArg2ofFather_1Fn(eileen))).

siblings(douglas,cassiopea).
father(sophiaWebb,douglas).
father(skylar,douglas).
father(sophiaWisdom,douglas).
father(zaltana,douglas).

:- must_is_entailed(human(douglas)).

:- mpred_why(human(douglas)).

:- mpred_why(grandparent(douglas,robert)).

:- show_test((mother(Who,Female))).

:- show_test(father(Who,Male)).

% :- show_test((male(Who))).

% :- show_test((female(Who))).

:- show_test((siblings(Who,AndWho))).

%= human(P) => (female(P) v male(P)).
if(gendered_human(P), (female(P) v male(P))).






