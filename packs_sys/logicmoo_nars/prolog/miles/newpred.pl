% MODULE newpred EXPORTS
:- module( newpred,
           [ specialize_with_newpred/5,
             specialize_with_newpred/7,
             specialize_with_newpred/2,
             is_newpred/1
           ]).


% IMPORTS
:- use_module(home(kb),
                   [get_clause/5, get_evaluation/2,delete_clause/1,
                    store_clause/4,store_ex/3]).
:- use_module(home(var_utils),
                   [only_vars/2]).
:- use_module(home(div_utils),
                   [mysetof/3]).
:- use_module(home(td_basic),
                   [append_body/3]).
:- use_module(home(interpreter),
                   [prooftrees/3]).
:- use_module(home(environment),
                   [ask_for_ex/1]).
:- use_module(home(argument_types),
              [types_of/3,type_restriction/2]).
:- use_module_if_exists(library(basics),
                      [member/2]).
:- use_module_if_exists(library(sets),
                      [intersection/3]).
:- use_module_if_exists(library(strings),
                      [gensym/2]).

% METAPREDICATES
% none


%***********************************************************************
%*	
%* module: newpred.pl
%*									
%* author: I.Stahl
%*									
%* changed:								
%*									
%*									
%* description: 
%* 
%* see also:								
%*									
%***********************************************************************



%***********************************************************************
%*									
%* predicate: 	specialize_with_newpred/1
%*									
%* syntax:	specialize_with_newpred(+ID)
%*									
%* args:	ID .. Clause ID
%*									
%* description: 
%*									
%* see also:								
%*									
%***********************************************************************

specialize_with_newpred(ID,(ID,L)):-
    mysetof((NC,Pos,Neg,TR),specialize_with_newpred(ID,NC,Pos,Neg,TR),L).
    

%***********************************************************************
%*									
%* predicate: 	specialize_with_newpred/5
%*									
%* syntax:	specialize_with_newpred(+ID,-Newclause,-Pos,-Neg,-Typerestriction)
%*									
%* args:	ID .. Clause ID, Newclause.. specialized clause
%*              Pos.. positive examples for the new predicate
%*              Neg.. negative examples for the new predicate
%*              Typerestriction.. type restriction for the new predicate
%*									
%* description: 
%*									
%* see also:								
%*									
%***********************************************************************

specialize_with_newpred(ID,NC,P,N,type_restriction(Newp2,TR)):-
   get_clause(ID,H,B,_,_),
   get_evaluation(ID,evaluation(_,_,Pos,_,Neg,_,_,_,_)),
   (   ( Pos = [] ; Neg = [] ) ->
       fail
   ;   only_vars((H,B),Vars),
       types_of(Vars,(H:-B),TVars),
       clause_instances(Pos,ID,H,B,Vars,PV),
       clause_instances(Neg,ID,H,B,Vars,NV),
       reduce_newpred_args(Vars,Vars,PV,NV,Vars1,P0,N0),
       gensym(newp,X),
       Newp =.. [X|Vars1],
       make_newp_ex(P0,X,P),
       make_newp_ex(N0,X,N),
       append_body((H:- B),Newp,NC),
       copy_term((Vars1,TVars,Newp),(Vars2,TVars2,Newp2)),
       make_type_restriction(Vars2,TVars2,TR)
   ).



%***********************************************************************
%*									
%* predicate: 	specialize_with_newpred/7
%*									
%* syntax:	specialize_with_newpred(+Clause,+CPos,+CNeg,-Newclause,
%*                                      -Pos,-Neg,-Typerestriction)
%*									
%* args:	Clause.. clause to be specialised with new predicate
%*              CPos,CNeg.. pos./neg. examples covered by the clause
%*              Newclause.. specialized clause
%*              Pos.. positive examples for the new predicate
%*              Neg.. negative examples for the new predicate
%*              Typerestriction.. type restriction for the new predicate
%*									
%* description: 
%*									
%* see also:								
%*									
%***********************************************************************

specialize_with_newpred((H:-B),Pos,Neg,NC,P,N,type_restriction(Newp2,TR)):-
   only_vars((H,B),Vars),
   types_of(Vars,(H:-B),TVars),
   clause_instances(Pos,ID,H,B,Vars,PV),
   clause_instances(Neg,ID,H,B,Vars,NV),
   reduce_newpred_args(Vars,Vars,PV,NV,Vars1,P0,N0),
   gensym(newp,X),
   Newp =.. [X|Vars1],
   make_newp_ex(P0,X,P),
   make_newp_ex(N0,X,N),
   append_body((H:- B),Newp,NC),
   copy_term((Vars1,TVars,Newp),(Vars2,TVars2,Newp2)),
   make_type_restriction(Vars2,TVars2,TR).
 


%***********************************************************************
%*									
%* predicate: 	clause_instances/5
%*									
%* syntax:	clause_instances(+Covered,+ID,+Head,+Body,+Vars,-Varinstances)
%*									
%* args:	Covered.. examples covered by clause ID
%*              ID .. clauseID
%*              Head,Body.. of clause ID, Vars.. variables of clause ID
%*              Varinstances.. instantiations of the clause variables according
%*               to Covered. If Vars = [V1,..,Vn] and |Covered| = m, then
%*               Varinstances = [[I11,..,I1n],..,[Im1,..,Imn]]
%*									
%* description: 
%*									
%* see also:								
%*									
%***********************************************************************

clause_instances([],_,_,_,_,[]).
clause_instances([ID:Ex|R],IDC,H,B,Vars,[Vars1|R1]):-
   clause_instances(R,IDC,H,B,Vars,R1),
   copy_term((H,B,Vars),(Ex,B1,Vars1)),
   prooftrees(ID,success,Proofs),
   setof(PBody,member([IDC,Ex,PBody],Proofs),Bodies),
   body_instances(Bodies,B1).

body_instances([],_).
body_instances([B|R],B1):-
   body_instances(R,B1),
   body_inst(B,B1).

body_inst([],true):- !.
body_inst([[_,B,_]|R],(B,R1)):-
   !, (   \+(ground(B)) ->
          ask_for_ex(B)
      ;   true
      ),
   body_inst(R,R1).
body_inst([[_,B,_]],B):-
   (   \+(ground(B)) ->
       ask_for_ex(B)
   ;   true
   ).

   
%***********************************************************************
%*									
%* predicate: 	reduce_newpred_args/7
%*									
%* syntax:	reduce_newpred_args(+Vars,+Vars,+PCovered,+Ncovered,
%*                                  -Vars,-PCovered,-NCovered)
%*									
%* args:	Vars.. argument variables of the new predicate, to be 
%*                     reduced
%*              PCovered,NCovered.. Instantiations of these argument
%*                variables according to the Pos/Neg examples covered
%*                by the clause
%*									
%* description: discrimination based reduction
%*									
%* see also: CHAMP/DBC							
%*									
%***********************************************************************

reduce_newpred_args([],Vars,PVars,NVars,Vars,PVars,NVars).
reduce_newpred_args([X|R],Vars,P,N,Vars2,P2,N2):-
   remove_arg(X,Vars,Vars1,P,P1,N,N1),
   intersection(P1,N1,[]),
   reduce_newpred_args(R,Vars1,P1,N1,Vars2,P2,N2).
reduce_newpred_args([_|R],Vars,P,N,Vars2,P2,N2):-
   reduce_newpred_args(R,Vars,P,N,Vars2,P2,N2).

remove_arg(X,Vars,Vars1,P,P1,N,N1):-
   rem_arg(X,Vars,Vars1,1,Pos),!,
   rem_ins(P,Pos,P1),
   rem_ins(N,Pos,N1).

rem_arg(X,[Y|R],R,Pos,Pos):-
   X == Y,!.
rem_arg(X,[Y|R],[Y|R1],Pos,Pos1):-
   Pos0 is Pos + 1,
   rem_arg(X,R,R1,Pos0,Pos1).

rem_ins([],_,[]).
rem_ins([V|R],Pos,[V1|R1]):-
   rem_i(V,1,Pos,V1),
   rem_ins(R,Pos,R1).

rem_i([_|R],P,P,R):- !.
rem_i([X|R],P,P1,[X|R1]):-
   P0 is P + 1,
   rem_i(R,P0,P1,R1).

   
%***********************************************************************
%*									
%* predicate: 	make_newp_ex/3
%*									
%* syntax:	make_newp_ex(Varinstances,Newp_name,Newp_examples)
%*									
%* args:	Varinstances.. instantiations of the argument variables
%*                             [[I11,..,I1n],..,[Im1,..,Imn]]
%*              Newp_examples  [New_name(I11,..,I1n),..,Newp_name(Im1,..,Imn)]
%*									
%* description: 
%*									
%* see also:								
%*									
%***********************************************************************

make_newp_ex([],_,[]).
make_newp_ex([V|R],X,[N|R1]):-
   N =.. [X|V],
   make_newp_ex(R,X,R1).


   
%***********************************************************************
%*									
%* predicate: 	make_type_restriction/4
%*									
%* syntax:	make_type_restriction(+Newpvars,+Typed_clause_vars,
%*                                    -Type_restriction)
%*									
%* args:	
%*									
%* description: 
%*									
%* see also:								
%*									
%***********************************************************************

make_type_restriction([],_,[]).
make_type_restriction([X|R],TVars,[T|R1]):-
   make_type_restriction(R,TVars,R1),
   mtr(X,TVars,TN),
   T =.. [TN,X].

mtr(X,[Y:T|_],T):- X == Y,!.
mtr(X,[_|R],T):- mtr(X,R,T).

   
%***********************************************************************
%*									
%* predicate: 	is_newpred/1
%*									
%* syntax:	is_newpred(+Pred_Name)
%*									
%* args:	
%*									
%* description: checks whether Pred_Name is of the form 'newpXX'
%*									
%* see also:								
%*									
%***********************************************************************

is_newpred(Name):-
   name(Name,[N,E,W,P|_]),
   name(newp,[N,E,W,P]).