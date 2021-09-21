:- include(test_header).





%:- kb_shared(system:(<-)/2).
%:- kb_shared(system:(<==)/2).

:- make.
/*

<
>
\==


?- N #> 3, N #< 9, N #< 19,  dom(N,[ 5 ,6,7, 20] ), dom(N,[ 7, 20] ), 
   print(N).

N=(min,max,dom)
   4   [6,7]    8 


N = 7.



CLP(FD)

CHR 

[m]


?- isa(N,juvinile) , isa(N,dog), isa(N,puppy), (isAt(N,petStoreA) ;isAt(N,petStoreB)).
N = snoopySon{isa(N,juvinile) , isa(N,dog), isa(N,puppy), (isAt(N,petStoreA) ;isAt(N,petStoreB))}

?- isa(X,male).
X = _:{isa(X,male)}.

?- N=X.
Yes.

N =  _{value(N,snoopySon) isa(N,male), isa(N,juvinile) , isa(N,dog), isa(N,puppy), (isAt(N,petStoreA) ;isAt(N,petStoreB))}

N =  snoopySon{isa(N,male), isa(N,juvinile) , isa(N,dog), isa(N,puppy), (isAt(N,petStoreA) ;isAt(N,petStoreB))}





?- isa(N,juvinile) , isa(N,dog), isa(N,puppy), (isAt(N,petStoreA) ;isAt(N,petStoreB)).
N = snoopySon.

?- isa(X,oldDog).
X = snoopy

?- N=X.
No.




snoopySon = myFriendFn(dog).


(isa(N,dog),  isa(N,juvinile) , isa(N,puppy), isAt(N,petStoreA), isAt(N,petStoreB) ==> what_you_want(N)).

(isa(N,puppy), isAt(N,petStoreA), isAt(N,petStoreB) ==> what_you_want(N)).

?- what_you_want(N).



N = isa() 
N = puppy

N=((min_isa, max_isa),dom,[pred])
  puppy, [],  +petStoreA  +petStoreB
   




:- nodebug_logicmoo(_).
:- use_module(library(gui_tracer)).
:- noguitracer.
:- guitracer.
:- debug.
:- visible(+all).
:- leash(-all).
:- leash(+exception).
%:- ((trace,ls)).
:- noguitracer.
:- zotrace(leash(+all)).
:- notrace.
*/
 
:- reconsult(library(script_files)).


%=  setup pfc
:- expects_dialect(pfc).
:- kif_compile.

% :- process_this_script.

%= save compiled clauses using forward chaining storage (by default)
%= we are using forward chaining just so any logical errors, performance and program bugs manefest
%= immediately
:- set_clause_compile(fwc).

%= ````
%= logic tests...
%= ````

%= trudy is human
human(trudy).
human(eileen).
human(douglas).
mother(douglas,eileen).


%= catch a regression bug that may couse trudy to lose human assertion
never_retract_u(human(trudy)).
never_assert_u(mother(trudy,das)).

%= these we want but i am trigging some breakpoints
% never_assert_u(father(_,_)).
% never_assert_u(mother(trudy,_)).

:- mpred_trace_exec.

:- dynamic(father/2).

clif(forall(c,exists([m,f], if(human(c), (mother(c,m) & father(c,f)))))).
%
% ~human(C) :-
%       ~mother(C, _8290).
%
% ~human(C)<- ~mother(C, _4806), {is_unit(C)}.
%
% ~human(C) :-
%       ~father(C, _3412).
%
% ~human(C)<- ~father(C, _4756), {is_unit(C)}.
%
% father(C, skArg2of_1Fn(C)) :-
%       human(C).
%
% human(C), {is_unit(C)}==>if_missing(father(C, _4770), father(C, skArg2of_1Fn(C))).
%
% mother(C, skArg2of_2Fn(C)) :-
%       human(C).
%
% human(C), {is_unit(C)}==>if_missing(mother(C, _3586), mother(C, skArg2of_2Fn(C))).


:- must(clif(forall(c,exists([m,f], if(human(c), (mother(c,m) & father(c,f))))))).

mother(eileen,trudy).
father(eileen,bob).

:- must(\+ mother(eileen,skArg1ofMother_1Fn(_))).


:- printAll(must(father(_,_))).
:- printAll(must(mother(_,_))).






% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/sanity_fi_sk_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/SANITY_FI_SK_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ASANITY_FI_SK_01 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/610
