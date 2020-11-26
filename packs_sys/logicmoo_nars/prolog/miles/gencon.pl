% MODULE gencon  EXPORTS
:- module(gencon,
        [ gilppi/12,gilppi/14
        ]).



% METAPREDICATES
:- meta_predicate gilppi(:,:,:,:,:,:,:,:,:,:,:,:),
                  gilppi(+,+,:,:,:,:,:,:,:,:,:,:,:,:).


% IMPORTS
:- use_module(home(kb),
                   [store_clauses/2]).
:- use_module(home(show_utils),[write_list/1,show_kb/0]).

%***********************************************************************
%*                                                                      
%* module:      gilppi.pl                                             
%*                                                                      
%* author: I.Stahl          				 date:7/93
%*                                                                      
%* description: generic control for induction a la GENCOL
%*      enhanced with PI capabilties
%*      Given: B, E+, E-
%*      Algorithm:
%*        Partial_Sols := initialize()
%*        Complete_Sols := {}
%*        while not(Stop_C(Complete_Sols)) do
%*              PS := Select(Partial_Sols)
%*              if Quality_C(PS)
%*              then Complete_Sols := Complete_Sols U {PS}
%*                   Partial_Sols := Update(Partial_Sols)
%*              else if active(PS)
%*                   then One_of(->Partial_Sols := Add(Partial_Sols,Spec(PS))
%*                               ->Partial_Sols := Add(Partial_Sols,Spec(PS)))
%*                        all PS in spec(PS) (gen(PS)) marked active
%*                   else Partial_Sols := Add(Partial_Sols,L_Newp(PS))
%*              mark PS as passive
%*              Partial_Sols := Filter(Partial_Sols)
%*        Output(Complete_Sols)
%*                                                                      
%* see also:                                                            
%*                                                                      
%***********************************************************************



%***********************************************************************
%*									
%* predicates:	gilppi/12
%*									
%* syntax: gilppi(+Initialize, +Stop_C, +Quality_C, +Update, +Select, +Add, +Filter,
%*                +One_of, +Spec, +Gen, +L_newp, +Output)
%*									
%* args: Initialize... name of a 1-place predicate that initializes the list
%*                     of partial solutions
%*       Stop_C... name of a 1-place predicate that checks whether complete_sols
%*                 contains a satisfactory solution
%*       Quality_C... name of a 1-place predicate that checks whether the current
%*                    theory PS is satisfactory
%*       Update... name of a 2-place predicate that updates the list of partial 
%*                 solutions after a satisfactory solution has been found
%*       Select... name of a 4-place predicate that selects a promising partial
%*                 solution from partial_sols
%*       Add... name of a 3-place predicate that adds the new partial solutions
%*              to the list partial_sols
%*       Filter... name of a two-place predicate that filters the most promising
%*                 among partial_sols
%*       One_of... name of a two-place predicate that decides whether the current
%*                 theory PS should be generalised or specialised
%*       Spec... name of a 2-place predicate that determines all specialisations
%*               of the current theory PS wrt the bias
%*       Gen...  name of a 2-place predicate that determines all generalisations
%*               of the current theory PS wrt the bias
%*       L_newp... name of a 14-place predicate, the actual PI-module
%*       Output... name of a 1-place predicate that outputs the complete solutions
%*                 
%*       
%* description: implements a generic ILP algorithm (cf GENCOL) with PI capabilities
%*              the actual learning algorithm depends on the implementations of
%*              the argument predicates
%*
%* example: 
%* 
%***********************************************************************


gilppi(Initialize, Stop_C, Quality_C, Update, Select, Add, Filter,
       One_of, Spec, Gen, L_newp,Output):-
   c_call(Initialize,[Partial_Sols]),
   gilppi(Partial_Sols, [], Initialize, Stop_C, Quality_C, Update, Select, 
          Add, Filter, One_of, Spec, Gen, L_newp,Output).


gilppi(_, Complete_Sols, _, Stop_C, _, _, _, _, _, _, _, _, _,Output):-
   c_call(Stop_C, [Complete_Sols]),!,
   c_call(Output,[Complete_Sols]).

gilppi(Partial_Sols, Complete_Sols, Initialize, Stop_C, Quality_C, Update, Select, 
       Add, Filter, One_of, Spec, Gen, L_newp,Output):-
   c_call(Select, [Partial_Sols, PS, M, Partial_Sols1]),
   (   c_call(Quality_C, [PS]) ->
       c_call(Update,[Partial_Sols1,Partial_Sols2]),
       gilppi(Partial_Sols2, [PS|Complete_Sols], Initialize, Stop_C, Quality_C, 
              Update,Select, Add, Filter, One_of, Spec, Gen, L_newp,Output)
   ;   (   M == active ->
           c_call(One_of, [PS, GS]),
           (   GS == spec ->
               write('Specialising'),nl,write_list(PS),nl,nl,
               c_call(Spec, [PS, PSL])
           ;   write('Generalising'),nl,write_list(PS),nl,nl,
               c_call(Gen, [PS, PSL])
           )
       ;   c_call(L_newp,[PS,PSL, Initialize, Stop_C, Quality_C, Update, 
                          Select, Add, Filter, One_of, Spec, Gen, L_newp,Output])
       ),
       c_call(Add, [Partial_Sols1, PSL, Partial_Sols2]),
       c_call(Filter, [Partial_Sols2, Partial_Sols3]),
       gilppi(Partial_Sols3, Complete_Sols, Initialize, Stop_C, Quality_C, Update,
              Select, Add, Filter, One_of, Spec, Gen, L_newp,Output)
   ).


c_call(MPred,Arglist):- 
     c_mod(MPred,M,Pred),
     Call =.. [Pred|Arglist],
     call(M:Call).

c_mod(M:Pred,M,Pred):-
   simple(Pred),!.
c_mod(_:P,M1,Pred):-
   !,c_mod(P,M1,Pred).
