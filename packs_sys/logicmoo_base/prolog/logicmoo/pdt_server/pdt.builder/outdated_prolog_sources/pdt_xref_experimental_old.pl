/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module( pdt_xref_backup,
         [ create_pdt_xref_data/2 % To be called before any call to the predicate(s) below
         , find_reference_to_quick/10    % (DefFile, DefModule,Functor,Arity,RefModule,RefHead,RefFile,RefLine,RefClauseRef)
         ]
         ).
         


    /*********************************************
     * FIND REFERENCES TO A PARTICULAR PREDICATE *
     ********************************************/

%% find_reference_to(+Functor,+Arity,DefFile, DefModule,RefModule,RefHead,RefFile,RefLine,Nth,Kind)
find_reference_to_quick(Functor,Arity,DefFile, DefModule,RefModule,RefHead,RefFile,RefLine,Nth,Kind) :-
	( nonvar(DefFile)
    -> module_of_file(DefFile,DefModule)
    ; true % Defining File and defining Module remain free ==> 
           % Search for references to independent definitions
           % <-- Does that make sense???
    ),
    ( var(Arity) % Need to backtrack over all declared Functor/Arity combinations:
    -> ( setof( Functor/Arity, DefModule^current_predicate(DefModule:Functor/Arity), Set),
         member(Functor/Arity, Set)
       )
    ; true % Arity already bound in input
    ),
    functor(T,Functor,Arity),
    % 
    % THE following should be replaced by access to the cache
    % 
    pdt_xref_data(DefModule:T,RefModule:RefHead,Ref,Kind),
    % 
    % Obiges sollte auf gecachete daten zugreifen
    %
    nth_clause(RefModule:RefHead,Nth,Ref),
    clause_property(Ref, file(RefFile)),
    clause_property(Ref, line_count(RefLine)).



pdt_xref_data(DefModule:T,RefModule:RefHead,Ref, Kind) :-
   current_predicate(RefModule:F/A),     % For all defined predicates
   functor(RefHead,F,A),   
   nth_clause(RefModule:RefHead,_,Ref),   % For all their clauses
   '$xr_member'(Ref, X),                  % Get a term referenced by that clause
   extractModule(X,T,DefModule,RefHead,Kind).     % (via SWI-Prolog's internal xref data)


extractModule(_:T,_,_,_,_) :- 
    var(T),
    !,
    fail.
extractModule(DefModule:Term,Term,DefModule,_RefHead, call) :-
    !.
extractModule(Term,Term,_DefModule,'$mode'(_, _), prologdoc) :-
    !.
extractModule(Term,Term,_DefModule,_RefHead, termORmetacall) .      
                     
    /***************************
     * BASIC ANALYSIS PREDICATE 
     **************************/

% For JT + PDT + SWI-libs + small JT factbase (for JT_tutorial):
% ?- performance( extract_predicate_references(T,M:H,Ref), Time, Counter ).
% Time = 1638,
% Counter = 40315.

extract_predicate_references(T,M:H,Ref ) :- 
   defined_in_module(M,F,A),             % For all defined predicates
   functor(H,F,A),
   \+ H = pdt_xref_data(_,_,_),          % ... except the one storing the results
   nth_clause(M:H,_Nth,Ref),             % For all their clauses
   '$xr_member'(Ref, T),                 % Get a term referenced by that clause
                                         % (via SWI-Prolog's internal xref data)
   filter(T).                            % Keep only predicate references

filter(Referenced) :-
      nonvar(Referenced),    
      \+ number(Referenced),
     ( Referenced = M:Head 
      -> ( nonvar(Head), functor(Head,      F,N), current_predicate(M:F/N) )
      ;  (               functor(Referenced,F,N), current_predicate(_:F/N) )
      ),
      !.  

    
    /*************************************
     * WORK WELL, NO RUNTIME DIFFERENCES *
     *************************************/

% Two equivalent definitions (forall-based, backtracking-based).
% Use the one you like. The default, for external use, is the
% forall-based version.

create_pdt_xref_data(How,NrOfClauses) :-
    extract_predicate_references(How),
    predicate_property( pdt_xref_data(_,_,_), number_of_clauses(NrOfClauses)).
     
extract_predicate_references(forall) :- 
   retractall(pdt_xref_data(_,_,_)),
   forall( extract_predicate_references(T,M:H,Ref),
           assert( pdt_xref_data(T,M:H,Ref) ) 
   ).
   

extract_predicate_references(backtrack) :- 
   retractall(pdt_xref_data(_,_,_)),
   extract_predicate_references(T,M:H,Ref),
      assert( pdt_xref_data(T,M:H,Ref) ),
   fail.  
extract_predicate_references(backtrack).

  
% For JT + PDT + SWI-libs + small JT factbase (for JT_tutorial) and an
% initially empty result set (0 clauses for pdt_xref_data/3): 
%
% The evaluation shows that the two versions are equivalent and that asserting
% the results adds just a minimal overhead to computing them. Even in a 
% pessimistic view (taking the longest runtime of computing&asserting (=1451 
% millisec) minus the shortest runtime of only computing (= 1232 millisec)
% the overhead is just 119 msec, which is below 10% of the costs of computing
% the full set once. Thus asserting the results and keeping them up to date
% incrementally, whenever files are 'pdt_reload'-ed, is the most efficient
% way to achieve very fast search results. -- GK, April 26, 2011 

% ------------------------ COMPUTE RESULTS WITHOUT ASSERTING THEM 
% 34 ?- performance( pdt_xref:extract_predicate_references(T,M:H,Ref ), Time, Proofs).
% Time = 1232 | 1233 | 1248 | 1248,
% Proofs = 28346.
%
% ------------------------ FAILURE-DRIVEN VERSION ---------------
%37 ?- time(create_pdt_xref_data(backtrack,NrOfClauses)).
%% 803,978 inferences, 1.451 CPU in 1.453 seconds (100% CPU, 554158 Lips)
%% 803,978 inferences, 1.295 CPU in 1.294 seconds (100% CPU, 620924 Lips)
%% 803,978 inferences, 1.295 CPU in 1.289 seconds (100% CPU, 620924 Lips)
%NrOfClauses = 28346.
%
% ------------------------ FORALL VERSION ---------------
%40 ?- time(create_pdt_xref_data(forall,NrOfClauses)).
%% 803,979 inferences, 1.295 CPU in 1.294 seconds (100% CPU, 620925 Lips)
%% 803,979 inferences, 1.279 CPU in 1.298 seconds (99% CPU, 628497 Lips)
%NrOfClauses = 28346.

    /***********************************
     * FINDALL ==> OUT OF GLOBAL STACK *
     ***********************************/
     
xrdata(findall) :-
    findall( (RefP, RefM:RefH, Ref), 
             extract_predicate_references(RefP, RefM:RefH, Ref), 
             ALL
    ), 
    forall( member( (RefP, RefM:RefH, Ref), ALL), 
            assert(pdt_xref_data(RefP, RefM:RefH, Ref))
    ),
    listing(pdt_xref_data).


    /*******************************
     * CTs = FINDALL ==> OUT OF GLOBAL STACK
     *******************************/
     
xrdata(ct) :- apply_ct( extract_xref_data ), listing(pdt_xref_data).

ct( extract_xref_data, 
   (( extract_predicate_references(
                            Referenced,
                            ReferencingModule:ReferencingHead,
                            Ref )
   )),
   (( add(pdt_xref_data(    Referenced,
                            ReferencingModule:ReferencingHead,
                            _Nth,
                            Ref
       ))
    ))
).

         
         
     /***************************
     * STATISTICS ON CACHED XREF DATA 
     **************************
   
?- time(find_all_refs).
180,565 inferences, 74.849 CPU in 75.112 seconds (100% CPU, 2412 Lips)

?- average_refs_per_term(NrOfSets, Sum, Average).
NrOfSets = 5008,
Sum = 24377,
Average = 4.867611821086262.

?- compute_average_refs__(NrOfSets, Sum, Average,[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10]) .
NrOfSets = 5008,
Sum = 24377,
Average = 4.867611821086262,
A1 = 6398-type(_G36723, _G36724, _G36725),  <-- JT type terms, no predicate calls!
                                            <--- need info about metacalls to filter this out
A2 = 405-file(_G30386, _G30387),
A3 = 277-true,
A4 = 224- (_G3315, _G3316),
A5 = 167- (system:assert(_G23337)),
A6 = 165-prolog,
A7 = 148-false,
A8 = 144-format(_G30500, _G30501),
A9 = 135- (system: (_G24473=.._G24474)),
A10 = 131- (system:catch(_G25939, _G25940, _G25941)).

In spite of the inclusion of many references to normal terms (not predicate invocations)
less than 5% (about 250 of 5008) referenced terms are referenced more than 9 times:

?- compute_average_refs__(NrOfSets, Sum, Average,Median,250,AllBut,Max) .
NrOfSets = 5008,
Sum = 24377,
Average = 4.867611821086262,
Median = 1-rewrite_ref(_G53662, _G53663, _G53664, _G53665),
AllBut = 9-permission_error(_G49904, _G49905, _G49906),
Max = [6398-type(_G51254, _G51255, _G51256), 
       405-file(_G41992, _G41993), 
       277-true, 224- (_G6155, _G6156), 
       167- (system:assert(_G32441)), 
       165-prolog, 
       148-false, 
       144-format(..., ...), ... - ...|...].
     ************************** */

average_refs_per_term(NrOfSets, Sum, Average) :-
   find_all_refs,
   compute_average_refs__(NrOfSets, Sum, Average).
   
find_all_refs :- 
  retractall( average_refs__(_) ),
  findall( L-T, ( setof( Ref, M^H^pdt_xref_data(T,M:H,Ref), SET), length(SET,L) ), NrOfRefs),
  assert( average_refs__(NrOfRefs) ).

compute_average_refs__(NrOfSets, Sum, Average,Median,N,AllButN,Rev/*[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13 ]*/) :-
  average_refs__(LengthsAndTerms), 
  keysort(LengthsAndTerms ,Sorted),
  reverse(Sorted,Rev),
 % Rev = [A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13 |_],
  % extract lengths
  sumlist4pairs(Sorted,Sum),
  length(Sorted,NrOfSets),
  Average is Sum/NrOfSets,
  median(Sorted,Median),
  nth1(N,Rev,AllButN).

sumlist4pairs(Pairlist,Sum) :-
    sum_list__(Pairlist,0,Sum).

sum_list__([],A,A).
sum_list__([L-_|Rest], A, Res) :-
    Anew is A+L,
    sum_list__(Rest,Anew,Res).

median(List, Median) :-
    length(List,L),
    Middle is L // 2,
    MiddleRest is L mod 2,
    ( 0 =:= MiddleRest      
    -> MedPos is Middle+1   % even
    ;  MedPos =  Middle     % odd
    ),
    nth1(MedPos,List,Median).

even(N) :- 0 =:= N rem 2.


    /***************************
     * MINI-VERSION TO HELP FIND THE FLAW IN THE ORIGINAL VERSION 
     **************************/
     
mini :-
   retractall(xxx(_,_,_)),
   analyse(M:H,Ref,T),
      assert(xxx(M:H,Ref,T)),
   fail.
mini :- 
   listing(xxx).
 
analyse(M:H,Ref,T) :-
    current_predicate_dummy(M:F/A), % should not see new predicate asserted at the end
     functor(H,F,A),         
     nth_clause(M:H,_,Ref),          % should not see new clauses asserted at the end
        '$xr_member'(Ref, T),           % should only access clause for Ref produced by prev line
            filter(T).
              
current_predicate_dummy(m:f/1).
current_predicate_dummy(m:f/2).

m:f(1)   :- bla, fail, mini.
m:f(2)   :- bla, fail, mini, xrdata(X), call(xrdata(X)).
m:f(1,1) :- blub, invoke(_,_).



