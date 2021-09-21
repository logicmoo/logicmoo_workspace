:- include(test_header).


:- include('test_header.pfc').
:- ensure_loaded(library(script_files)).
% :- process_this_script.

%=  setup pfc
:- file_begin(pfc).

%= save compiled clauses using forward chaining storage (by default)
%= we are using forward chaining just so any logical errors, performance and program bugs manefest
%= immediately
:- set_clause_compile(fwc).


:- mpred_trace_exec.

ptSymmetric(love_compatible).

%= if two thing do like  each other then they are "love compatible"
:- kif_add(
 forall(a,forall(b,
    if( (likes(a,b)  & likes(b,a)),
     love_compatible(a,b))))).

%= will have the three side effects...

%= if A is not love compatible with B .. yet B likes A.. we show_call conclude A show_call not like B back.
:- is_entailed_u((not(likes(A, B)):- not(love_compatible(A, B)), likes(B, A))).

%= if A is not love compatible with B .. yet A likes B.. we show_call conclude B show_call not like A back.
:- is_entailed_u((not(likes(B, A)):- not(love_compatible(A, B)), likes(A, B))).

%= if A and B like each other both ways then they are love compatible
:- is_entailed_u((love_compatible(A, B):- likes(A, B), likes(B, A))).



%= if people are love compatible then they show_call like each other (variables my be in lower case for John Sowa''s CLIF compatibity)
:- kif_add(
  forall(a,forall(b,
   if(love_compatible(a,b),
    (likes(a,b)  & likes(b,a)))))).


%= will have the four side effects...

%= if A and B show_call not be love compatible since A does not like B
:- is_entailed_u((not(love_compatible(A, B)):- not(likes(A, B)))).
%= if A and B show_call not be love compatible since B does not like A
:- is_entailed_u((not(love_compatible(A, B)):- not(likes(B, A)))).
%= obviously A likes B .. since they are love compatible
:- is_entailed_u((likes(A, B):- love_compatible(A, B))).
%= obviously B likes A .. after all they are love compatible
:- is_entailed_u((likes(B, A):- love_compatible(A, B))).


%= this uses biconditional implicatature
:- kif_add(
 forall(a,forall(b,
  iff(might_altercate(a,b),
    (dislikes(a,b)  & dislikes(b,a)))))).

%=  canonicalizes to..

%= A and B will not fight becasue it takes two to tango and A doesnt dislike B
:- is_entailed_u((not(might_altercate(A, B)):- not(dislikes(A, B)))).
%= A and B will not fight becasue it takes B doent dislike A (like above)
:- is_entailed_u((not(might_altercate(A, B)):- not(dislikes(B, A)))).
%= Since we can prove A and B  dislike each other we can prove they are fight compatible
:- is_entailed_u((might_altercate(A, B):- dislikes(A, B), dislikes(B, A))).
%=  A dislikes B  when we prove A and B are fight compatible somehow  (this was due to the biconditional implicatature)
:- is_entailed_u((dislikes(A, B):- might_altercate(A, B))).
%=  B dislikes A  when we prove A and B are fight compatible
:- is_entailed_u((dislikes(B, A):- might_altercate(A, B))).


%= alice likes bill
:- kif_add(likes(alice,bill)).

%= dumbo does not exist
%= TODO :- kif_add(not(isa(dumbo,_))).

%= evertyting that exists is an instance of Thing
%= TODO :- kif_add(isa(_,tThing))).

%= also she likes ted
:- kif_add(likes(alice,ted)).

%= we take as a given that bill does not like alice (why?)
:- kif_add(not(likes(bill,alice))).

%= we take as a given that bill and ted dislike each other (dont blame the woman!)
% todo: perhaps make dislikes(bill,ted) and  dislikes(ted,bill) true only if the other is not categorically provable false
% some interdepancy is reasonable.. just what?)
:- kif_add((dislikes(bill,ted) & dislikes(ted,bill))).


%= we of course support SUO-KIF (SUMO)
%= thank you triska for showing off this neat hack for term reading
:- kif_add('

(<=>
  (dislikes ?A ?B)
  (not
      (likes ?A ?B)))

'
).

%= we of course supprt CLIF!
:- kif_add('

(forall (a b)
 (iif
  (dislikes a b)
  (not
      (likes a b))))

'
).

%= we support also CycL language
:- kif_add('

(equiv
  (dislikes ?A ?B)
  (not
      (likes ?A ?B)))

'
).

%= interestingly this canonicallizes to ...
%= A does not dislike B when A like B
:- is_entailed_u((not(dislikes(A, B)):- likes(A, B))).
%= A does not like B when A dislikes B
:- is_entailed_u((not(likes(A, B)):- dislikes(A, B))).
%= A does dislikes B when we can somehow prove A not liking B
:- is_entailed_u((dislikes(A, B):- not(likes(A, B)))).
%= A does likes B when we can somehow prove A not disliking B
:- is_entailed_u((likes(A,B) => not(dislikes(A,B)))).


%= The above assertions forward chain to these 4 side-effects
:- is_entailed_u((might_altercate(ted, bill))).
:- is_entailed_u((might_altercate(bill, ted))).
:- is_entailed_u((not(love_compatible(bill, alice)))).
:- is_entailed_u((not(love_compatible(alice, bill)))).

%= get a proof
:- sanity(call(mpred_get_support(not(love_compatible(bill, alice))   ,Why))).
%= O = (not(likes(bill, alice)), pt(not(likes(bill, alice)), rhs([not(love_compatible(bill, alice))]))) ;
%= TODO fix this error O = (u, u).




% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/456 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/dislikes_03.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/DISLIKES_03/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ADISLIKES_03 

