% MODULE foil  EXPORTS

:- module(foil,
        [ learn_foil/0,
          infogain/3 ]).


% IMPORTS

:- use_module(home(kb),
                  [store_clause/4, get_evaluation/2, delete_clause/1,
                   get_example/3,delete_example/1,get_clause/5,known/6,store_ex/3]).

:- use_module(home(evaluation),
                  [eval_examples/0, encoding_length_examples/1,
                   encoding_length_clause/2]).

:- use_module(home(tdref_it),
                  [refinement_add_body_literal/2]).

:- use_module(home(div_utils),
                  [body2list/2,log2/2,log2nueberk/3,mysetof/3]).

:- use_module(home(gencon),
                  [gilppi/12]).

:- use_module(home(show_utils),
                  [show_kb/0]).



% METAPREDICATES
% none

%************************************************************************
%* 
%* module: foil.pl
%*
%* author:      Irene Stahl      date:  1. 7. 1993
%*            
%* changed:   
%*             
%* description: simple implementation of infogain heuristic
%*              foil as instantiation of the generic algorithm (25. 11. 93)
%*              
%* see also:    
%*                            
%************************************************************************

learn_foil:-
   gilppi(initialize,stop_c, quality_c, update, select, add, filter,
          one_of, spec, gen, l_newp,output).


:- dynamic(el_ex/1).
:- dynamic(total_ex/1).
initialize([([MGT]:0,active)]):-
   mysetof((P,N),ID^L^(get_example(ID,F,L),functor(F,P,N)),[(P1,N1)]),
   functor(MGT,P1,N1),
   encoding_length_examples(X),
   assert(el_ex(X)),
   mysetof(ID1,F1^L1^(get_example(ID1,F1,L1)),IDL),
   length(IDL,TE),
   assert(total_ex(TE)).


select([(C:G,active)|R],C,active,[(C:G,passive)|R]).
select([X|R],C,active,[X|R1]):-
   select(R,C,active,R1).


quality_c([C]):-
   store_clause(C,_,usr,ID),
   eval_examples,
   get_clause(ID,H,B,CL,_),
   get_evaluation(ID,E),
   (   arg(5,E,[]) ->
       arg(3,E,Pos),
       remove_covered_ex(Pos)
   ;   delete_clause(ID),
       assert(known(ID,H,B,CL,hypo,E)),!,fail
   ).

remove_covered_ex([]).
remove_covered_ex([ID:Fact|R]):-
   delete_example(ID),
   assert(saved_ex(ID,Fact)),
   remove_covered_ex(R).

update(_,[([MGT]:0,active)]):-
   mysetof((P,N),ID^(get_example(ID,F,'+'),functor(F,P,N)),[(P1,N1)]),
   functor(MGT,P1,N1),!.
update(_,[]).

one_of(_,spec).

spec([C],PSL):-
   refinement_add_body_literal(C,CL),
   infogain(CL,PSL).
   
add(PS,[],PS).
add(PS,[X|R],PS1):-
   insert_by_gain(X,PS,PS0),
   add(PS0,R,PS1).

insert_by_gain(C:G,[(C1:G1,L)|R],[(C1:G1,L)|R1]):-
   G < G1,!,
   insert_by_gain(C:G,R,R1).
insert_by_gain(C:G,L,[([C]:G,active)|L]).


filter(L,L).

stop_c(_):-
   \+(get_example(_,_,+)),!.
stop_c(CL):-
   stop_c1(CL,N),
   el_ex(X),
   N > X.

stop_c1([],0).
stop_c1([[(H:-B)]|R],M):-
   stop_c1(R,M0),
   body2list(B,BL),
   encoding_length_clause([H:p|BL],M1),
   M is M0 + M1.

output(_):-
   retract(saved_ex(ID,Fact)),
   store_ex(Fact,+,ID),
   output(_).
output(_):-
   retractall(total_ex(_)),
   retractall(el_ex(_)),
   show_kb.
   


   
   

%************************************************************************
%*
%* predicate: info_gain/3
%*
%* syntax: infogain(+Clause,+Clause_refs,-CL)
%*
%* args: Clause.. Prolog clause that is refined
%*       Clause_refs .. list of prolog clauses, refinements of Clause
%*       CL.. list of clauses with their gain: CL = [C1:Gain1,..,Cn:Gainn]
%* 
%* description: Clause is an overgeneral clause, CL is the
%*              list of refinements of this overgeneral clause,
%*              CL = [C1,...,Cn]. infogain returns a list 
%*              CL = [C1:Gain_C1,..], where
%*              Gain is the information gain of Ci in comparison
%*              with the overgeneral clause
%*
%*************************************************************************


infogain(Clause,Ref_list,CL):-
   store_clause(Clause,_,gain,ID),
   eval_examples,
   get_evaluation(ID,evaluation(_,Tip,_,Tim,_,_,_,_,_)),
   delete_clause(ID),
   OTi is Tip / (Tip + Tim),
   log2(OTi,LNOTi),
   ITi is -LNOTi,
   infogain1(Ref_list,CL,ITi).

infogain1([],[],_).
infogain1([C|R],R2,ITi):-
   infogain1(R,R1,ITi),
   store_clause(C,_,gain,ID),
   eval_examples,
   get_evaluation(ID,evaluation(_,Ti1p,_,Ti1m,_,_,_,_,_)),
   delete_clause(ID),
   (   Ti1p = 0 ->
       copy_term(C,C1),numbervars(C1,0,_),
       write('refuted: '),write(C1),
       nl,nl,
       R2 = R1
   ;   OTi1 is Ti1p/(Ti1p + Ti1m),
       log2(OTi1,LNOTi1),
       ITi1 is -LNOTi1,
       IG is Ti1p * ( ITi - ITi1),
       copy_term(C,C1),numbervars(C1,0,_),
       write('refined clause: '),write(C1),write('   '),write(IG),
       nl,nl,
       R2 = [C:IG|R1]
   ).

infogain(Ref_list,CL):-
   get_clause(ID,_,_,_,hypo),
   get_evaluation(ID,evaluation(_,Tip,_,Tim,_,_,_,_,_)),
   delete_clause(ID),
   OTi is Tip / (Tip + Tim),
   log2(OTi,LNOTi),
   ITi is -LNOTi,
   infogain2(Ref_list,CL,ITi).


infogain2([],[],_).
infogain2([C|R],R2,ITi):-
   infogain2(R,R1,ITi),
   store_clause(C,_,gain,ID),
   eval_examples,
   get_evaluation(ID,evaluation(_,Ti1p,_,Ti1m,_,_,_,_,_)),
   encoding_length(Ti1p,X),
   (   C = (H:-B) ->
       body2list(B,BL),
       encoding_length_clause([H:p|BL],XE)
   ;   encoding_length_clause([C:p],XE)
   ),
   delete_clause(ID),
   (   (Ti1p = 0; XE > X) ->
       copy_term(C,C1),numbervars(C1,0,_),
       write('refuted: '),write(C1),
       nl,nl,
       R2 = R1
   ;   OTi1 is Ti1p/(Ti1p + Ti1m),
       log2(OTi1,LNOTi1),
       ITi1 is -LNOTi1,
       IG is Ti1p * ( ITi - ITi1),
       copy_term(C,C1),numbervars(C1,0,_),
       write('refined clause: '),write(C1),write('   '),write(IG),
       nl,nl,
       R2 = [C:IG|R1]
   ).

encoding_length(PN,X):-
   total_ex(U),
   log2(U,LU),
   U1 is float(U),
   PN1 is float(PN),
   log2nueberk(U1,PN1,Y),
   X is LU + Y.
