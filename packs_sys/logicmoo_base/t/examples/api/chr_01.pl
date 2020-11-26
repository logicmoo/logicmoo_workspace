:- module(chr_test_01,[]).
% c Thom Fruehwirth, March 2007
% Program for Paper "Description Logic and Rules the CHR Way", submitted March 2007
% Description Logic Solver with Rules, runs in Sicstus 3 and WebCHR
% Classic Description Logic Solver with many extensions is available at WebCHR
% WebCHR is at http://chr.informatik.uni-ulm.de/~webchr/

:- use_module(library(chr)).

handler dlrules.

operator(1200,xfx,isa).		% concept definition
operator(950,xfx,'::').		% A-Box membership and role-filler assertion
operator(940,xfy,or).		% disjunction
operator(930,xfy,and).		% conjunction
operator(700,xfx,is).		% used in restricitions 
operator(690,fy, nota).		% complement 		
operator(650,fx, some).		% exists-in restriction
operator(650,fx, every).	% value restriction

operator(100,yfx,of).           % role chain
operator(650,fx, allsome).	% allsome quantifier

:- forall(operator(A,B,C),op(A,B,C)).
:- forall(operator(A,B,C),system:op(A,B,user:C)).

:- dynamic(((isa)/2, feature/1)).  % to allow scattered clauses

% CONSISTENCY CHECK
% A-box as constraint goals
% T-box asserted(concept definitions by isa-rules)


constraints (::)/2, label/0.

% primitive clash
  I::nota Q, I::Q <=> false.		

% remove duplicates(both concepts and roles)
  I::C \ I::C <=> true.

% top and bottom
  I::top <=> true.
  I::bot <=> false.
  I::nota top <=> false.
  I::nota bot <=> true.

% complement nota/1

% nota-or
  I::nota(S or T)  <=> I::(nota S and nota T).

% nota-and
  I::nota(S and T) <=> I::(nota S or nota T).

% nota-nota
  I::nota(nota S)  <=> I::S.

% nota-every 
  I::nota(every R is S) <=> I::some R is nota S.

% nota-some 
  I::nota(some R is S)  <=> I::every R is nota S.

% conjunction
  I::S and T <=> I::S, I::T.

% exists-in restriction
  I::some R is S <=> (I,J)::R, J::S.	% generate witness
% I::every R is D \ I::some R is S <=>(I,J)::R, J::S. % more lazy

% value restriction
  I::every R is S,(I,J)::R ==> J::S.

% disjunction(is delayed as choice)
  label,(I::S or T) <=> true |(I::S ; I::T), label. % search by labeling

% T-box unfolding using concept definition
  I::C <=>(C isa T) | I::T.
  I::nota C <=>(C isa T) | I::nota T.
% X::_ \ I::C <=>(C isa T) | I::T. % more lazy


% Extensions ----------------------------------

% Allsome quantifiers, e.g.
  realparent isa allsome child is human.
  I::allsome R is S <=> I::every R is S, I::some R is S.

% Role chains(nested roles), e.g.
  realgrandfather isa(father of father).
 (I,J)::(A of B) <=>(I,K)::A,(K,J)::B.

% Inverse Roles
 (I,J)::inv(R) ==> atomic(R) |(J,I)::R. 
  %(I,J)::R ==> atomic(R) |(J,I)::inv(R).

% Transitive Roles
 (I,K)::R,(K,J)::trans(R) ==>(I,J)::trans(R).

% Functional roles(features, attributes)::
  feature(partner).
 (I,J)::F,(I,K)::F ==> feature(F) | J=K.

% Distinct, disjoint primitive concepts:: 
  distinct(number). primitive(human).
  I::C1, I::C2 ==> distinct(C1), primitive(C2) | C1=C2.

% Nominals(named individuals, singleton concepts)::
  nominal(franz).
  X::I ==> nominal(I) | X=I.

% Concrete domains(constraints from other domains):: 
  %(3,4)::smaller.
 (I,J)::smaller ==> I<J.
 (I,J)::nota smaller ==> I>=J.
 (I,A)::f1,(I,B)::f2 ==> flight(A,B).
   flight(muc,fdh).

% Examples ------------------------------------

% I. Horrocks, P. Patel-Schneider, S. Bechhofer, D. Tsarkov.
% OWl Rules: A Proposal and Prototype Implementation.
% Journal of Web Semantics, 3, 2005.

% B. Motik, U. Sattler, and R. Studer.
% Query Answering for OWL-DL with Rules.
% Journal of Web Semantics, 3, 2005.

Z::male,(Y,Z)::hassibling,(X,Y)::hasparent ==>(X,Z)::hasuncle.
% X::uncle <=>(Y,X)::hasuncle.
X::uncle ==>(Y,X)::hasuncle.
(Y,X)::hasuncle ==> X::uncle.
%(robert,paul)::hasparent,(paul,ian)::hassibling, ian::male.

X::beer ==> sean::happy.
sean::nota happy ==> X::nota beer.

(X,Y)::haschild,(X,Z)::haschild ==>(diff(Y,Z) -> X::taxcut ; true). 
X::man, X::woman ==> false.
(X,Y)::motherof ==> X::woman.

diff(Y,Z):- \+ Y=Z.

%(peter,Y)::haschild,(Y,Z)::motherof,(peter,paul)::haschild, paul::man.
% peter gets a taxcut

constraints expand/0.

X::pe \ expand <=>(X,Y)::f, Y::pe.
(X,Y)::f,(Y,Z)::f, Z::pe ==> X::gc.
(X,Y)::f ==>(X,Y)::p.
(X,Y)::p,(Z,Y)::p,(X,Z)::h, X::gc ==> X::bc.

person(X):- X::pe.
father(X,Y):-(X,Y)::f.
hates(X,Y):-(X,Y)::h.

/*
?-
person(cain),
father(cain,adam),
father(abel,adam),
hates(cain,abel),
expand, expand.
% cain is badchild(bc)

person(Romulus),
father(Remus,Y), father(Romulus,Y),
hates(Romulus,Remus),
expand, expand.
% Romulas is badchild(bc)
*/

X::gc ==> X::ch.
X::bc ==> X::ch.

% ?- oedipus:: gc or bc, label. 
% oedipus is good or bad, but always a child

% More Examples ---------------------------------------------------
% description-logic-extended.pl
% updated June 28, July 04, 2001, by Thom Fruehwirth
% LMU 980312 Thom Fruehwirth
% Ph. Hanschke and Th. Fruehwirth 920120-920217-920413-920608ff-931210 

% baader/sattler example of non-termination
%  feature(r).
%(A,A)::r, A::some r is p, A::every r is(some r is p).
% terminates with I::some R is S ==> role(R) |(I,J)::R, J::S. instead of <=>

% Family ---------------------------------------------------------------------

female isa nota male.
woman isa human and female.
man isa human and male.
parent isa human and some child is human.
father isa parent and man.
mother isa parent and woman.
grandfather isa father and some child is parent.
grandmother isa mother and some child is parent.
fatherofsons isa father and every child is male.

/*
?- X::fatherofsons and some child is female.
?- X::grandfather and every child is fatherofsons.
*/

% Part below c Thom Fruehwirth, June 28, 2001

% means of transport --------------------------------------------------------

X::moebel ==> X::gueter.
verkehrsweg isa strasse or schiene.
strasse isa nota schiene.
X::fahrzeug ==> X::some fahren_auf is verkehrsweg.
tfahrzeug isa fahrzeug and some transportieren is gueter.
kfz isa fahrzeug and every angetriebenwerden_durch is motor and every fahren_auf is strasse.
lkw isa kfz and some transportieren is gueter.
zug isa fahrzeug and every fahren_auf is schiene.
gueterzug isa zug and some transportieren is gueter.
moebelwagen isa lkw and every transportieren is moebel.

% A-Boxe Queries
% bulli::moebelwagen,z521::zug,bananen::gueter, kohlen::gueter,g112::gueter,g235::gueter,(bulli,g112)::transportieren.

% X::zug and nota kfz, label.             % two solutions
% X::gueterzug and nota tfahrzeug, label. % inconsistent

% German family ----------------------------------------------------------

zwitter isa mann and frau.
eltern_st isa some kind is mann and some kind is frau.
eltern_zw isa some kind is zwitter.

% A-Box Query
% X::eltern_st and nota eltern_zw, label. % one solution

% Exponential Example --------------------------------------------------

 c(0)    isa top.
 c(s(N)) isa some r is a and some r is b and every r is c(N).

% A-Boxe Queries
% X::c(s(0)). 
% X::c(s(s(0))). 
% X::c(s(s(s(0)))). % etc.

% end of dl.pl ========================================================

%===================================================================


:- use_module(library(chr)).

:- chr_constraint leq/2.

reflexivity  @ leq(X,X) <=> true.
antisymmetry @ leq(X,Y), leq(Y,X) <=> X = Y.
idempotence  @ leq(X,Y) \ leq(X,Y) <=> true.
transitivity @ leq(X,Y), leq(Y,Z) ==> leq(X,Z).

:-export(leq/2).

/*
min(X ,Y , Z ) <=> leq(X , Y ) | Z = X .
min(X ,Y , Z ) <=> leq(Y , X ) | Z = Y .
min(X ,Y , Z ) ==> leq(Z , X ) , leq(Z , Y ).
*/

/*
When the above program is saved in a file and loaded in SWI-Prolog, you can call the leq/2 constraints in a query, e.g.:

?- leq(X,Y), leq(Y,Z).
leq(_G23837, _G23841)
leq(_G23838, _G23841)
leq(_G23837, _G23838)
true .
When the query succeeds, the SWI-Prolog top level prints the content of the CHR constraint store and displays the bindings generated during the query. Some of the query variables may have been bound to attributed variables, as you see in the above example.
*/

% The program below implements a simple finite domain constraint solver.

% % % :- '$set_source_module'(dom).
:- export(dom/2).
:- use_module(library(chr)).

:- chr_constraint dom(?any,+list(any)).
:- chr_type list(T) ---> [] ; [T|list(T)].

dom(_X,[]) <=> fail.
dom(X,[Y]) <=> X = Y.
dom(X,L) <=> nonvar(X) | memberchk(X,L).
dom(X,L1), dom(X,L2) <=> intersection(L1,L2,L3), dom(X,L3).

% When the above program is saved in a file and loaded in SWI-Prolog, you can call the dom/2 constraints in a query, e.g.:
/*
?- dom(A,[1,4,3]), dom(A,[3,4,5]).
A = 3.
*/


:-export(((::)/2)).


