
% check/0

:- mode(tabled(:)).

:- meta_predicate(neat(*)).

% :- meta_predicate(tabled((/))). % not //

tabled(M:P):- mk_tabled(M,P).

mk_tabled(M,[H|T]):- !, mk_tabled(M,H),mk_tabled(M,T).
mk_tabled(M,(H,T)):- !, mk_tabled(M,H),mk_tabled(M,T).
mk_tabled(M,F/A):- !, functor(P,F,A),mk_tabled_helper(M,P).
mk_tabled(M,P):- mk_tabled_helper(M,P).

mk_tabled_helper(M,P):- M:dynamic(P),writeln(((mk_tabled(M,P)))).

:- tabled(foo/1).
:- tabled(mod:bar/1).

/*
:- meta_predicate dynamic(:).
:- meta_predicate mk_tabled_helper(*,:).
:- meta_predicate mk_tabled(*,:).
*/





/*

his declaration is normally used as a directive in compiled code, to specify if and how the arguments of a predicate refer to other predicates (meta-arguments). In ECLiPSe 6.1, this declaration does not affect the semantics of the program, and is only used by development tools like the cross-referencer or coverage analyser. In future releases, the compiler may make use of the information.
The meta-argument indicators describes the control flow through the arguments of a meta-predicate. The functor and arity of MetaSpec correspond to the functor and arity of the meta-predicate. The arguments are each populated with one of the following atomic descriptors:

0-N A goal that is called as a subgoal of the declared predicate. A positive integer
: A module-sensitive argument, but none of the above.


/ A PredSpec of the form Name/Arity.
:

A subgoal is constructed by appending the number of specified arguments, and then called.
* :- A clause (a fact or a rule).
* An argument that is not one of the above. Instead of the '*', which marks a non-meta argument, a mode indicator can be given. The effect is the same as specifying '*', and giving the mode in a separate mode/1 declaration:
+ The argument is instantiated, i.e. it is not a variable.
++ The argument is ground, i.e. contains no variables.
- The argument is not instantiated, it must be a free variable without any constraints or aliases.
? The mode is not known or it is neither of the above.


dynamic(M:[H|T]):- !, dynamic(M:H),dynamic(M:T).
dynamic(M:(H,T)):- !, dynamic(M:H),dynamic(M:T).
dynamic(M:F/A):-!,dynamic0(M:F/A)
dynamic(M:P):-!,functor(P,F,A),dynamic0(M:F/A).
dynamic(F/A):- !, dynamic0(M:F/A)
dynamic(P):- functor(P,F,A),dynamic(F/A).


check/0


:- meta_predicate(tabled(:)).

tabled(M:P):- mk_tabled(M,P).

mk_tabled(M,[H|T]):- !, mk_tabled(M,H),mk_tabled(M,T).
mk_tabled(M,(H,T)):- !, mk_tabled(M,H),mk_tabled(M,T).
mk_tabled(M,F/A):- !, mk_tabled(M,F,A)
mk_tabled(M,P):- functor(P,F,A),mk_tabled(M,F,A).

mk_tabled(M,F,A):- ....

*/


