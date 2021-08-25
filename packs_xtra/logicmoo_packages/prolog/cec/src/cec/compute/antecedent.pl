/*
 *	file:		antecedent3.pl
 *	version:	1.5
 *	date:		November 6, 1989
 *	creation:	October 10, 1989
 *	author:		Ullrich Hustadt
 *
 *	description:
 *	This file contains predicates related to narrowing, especially
 *	basic narrowing.
 *
 *	history:
 *	891010	js	Added this comment
 *	891106  uh	Deleted the definitions of
 *			inlist/2		pVars/2
 *	891106 	uh	Moved the defintions of
 *			sort_list/3 		divide/5
 *			iter/5			prefix/2
 *			into file prolog/lists.pl
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*----------------------------------------------------------------------*/
/* nonvar_occurrences(+Term,-OccList)                                   */
/* computes the non-variable occurrences of Term                        */
/* tested 17.11.88 08:40 */

nonvar_occurrences(Term,OccList) :-
   occ([],Term,no_vars,OccList).


/*----------------------------------------------------------------------*/
/* var_occurrences(+Term,-OccList)                                      */
/* computes the occurrences of variables of Term                        */
/* tested 17.11.88 08:42 */

var_occurrences(Term,OccList) :-
   occ([],Term,only_vars,OccList).


/*----------------------------------------------------------------------*/
/* occurrences(+Term,-OccList)                                          */
/* computes the occurrences of Term                                     */
/* tested 17.11.88 08:36 */

occurrences(Term,OccList) :-
   occ([],Term,all,OccList).


/*----------------------------------------------------------------------*/ 
/* occ(+Pfad_bis_Term,+Term,+Type_of_wanted_occurrences,-OccList)       */
/* makes the compution of the different occurrence sets possible        */
/* 08.12.88 */

occ(PfadAlt,X,only_vars,[PfadAlt]) :- 
	var(X), !.
occ(PfadAlt,X,all,[PfadAlt]) :- 
	var(X), !.
occ(_PfadAlt,X,no_vars,[]) :- 
	var(X), ! .
occ(PfadAlt,T1,OccType,[PfadAlt|Occ]) :-
	OccType \== only_vars, !,
	T1 =.. [_F|Args], occ_args(PfadAlt,1,Args,OccType,Occ).
occ(PfadAlt,T1,only_vars,Occ) :-
	T1 =.. [_F|Args], occ_args(PfadAlt,1,Args,only_vars,Occ).
   
occ_args(_PfadAlt,_N,[],_OccType,[]) :- !.
occ_args(PfadAlt,N,[T1|TL],OccType,Occ) :-
	append(PfadAlt,[N],PFH), occ(PFH,T1,OccType,Occ1),
	M is N+1, occ_args(PfadAlt,M,TL,OccType,Occ2),
	append(Occ1,Occ2,Occ).


/*----------------------------------------------------------------------*/
/* normalize_rule(+RuleDesripton,-NormalizeRuleDescription)             */
/* given the description of a rule the elements of a normalize_rule     */
/* will be computed.                                                    */
/* The elements are:                                                    */
/* 1. The list of all variables occurring in the right side of the rule */
/* 2. A list of pairs containing a variable and the list of all         */
/*    occurrences of the variable in the left side of the rule          */
/* 3. A list of pairs containing a variable and the list of all         */
/*    occurrences of the variable in the right side of the rule         */

normalize_rule((_,_,(C,[L = R])),
               (normalize_rule,[(C,[L = R]),VarsR,VarOccsL,VarOccsR])) :-
   vars(R,VarsR),
   varoccs_for_term(L,VarOccsL),
   varoccs_for_term(R,VarOccsR).


/*----------------------------------------------------------------------*/
/* varoccs_for_term(+Term,-VarOccs)                                     */
/* computes a list of pairs containing a variable and the list of all   */
/* occurrences of this variables from Term                              */
/* 14.11.88 TRY2 */

varoccs_for_term(Term,VarOccs) :-
   vars(Term,Vars),
   mapL(occs_for_var,Vars,OccsList,[Term]),
   pairing(Vars,OccsList,VarOccs).


/*----------------------------------------------------------------------*/
/* occ_for_var(+Var,-Occs,+Term)                                        */
/* computes the list of all occurrences of variable Var in Term         */
/* 14.11.88 TRY2 belonging only to varoccs_for_term */

occs_for_var(Var,Occs,Term) :-
   bagof(Path,find_subterm(Path,Term,@Var),Occs), !.
occs_for_var(_Var,[],_Term) :- !.


/*----------------------------------------------------------------------*/
/* based((+Path,+RightRuleSideOccs,+OldNarrowOccs),-NewNarrowOccs)      */
/* computes from OldNarrowOccs the basic occurrence set NewNarrowOccs   */
/* for the application of a rule which right hand side has nonvariable  */
/* occurrences RightRuleSideOccs at Path                                */
/* 14.11.88 TRY2 tested 17.11.88 08:50 */

based((Path,MTR,MTOld),MTNew) :-
   change_path_arg(Path,MTOld,MTNew,MTR), !.


/*----------------------------------------------------------------------*/
/* left_to_right_based((+Path,+RightRuleSideOccs,+OldNarrowOccs),       */
/*                     -NewNarrowOccs)                                  */
/* computes from OldNarrowOccs the left-to-right-based occurrence set   */
/* NewNarrowOccs for the application of a rule which right hand side    */
/* has nonvariable occurrences RightRuleSideOccs at Path                */
/* 15.11.88 TRY2 tested 17.11.88 08:55 */

left_to_right_based((Path,MTR,MTOld),MTNew) :-
   change_path_arg(Path,MTOld,MT1,MTR),
   nonvar_occurrences(MT1,Occs),
   all_left_occs(Path,Occs,LeftOccs),
   iter(mark,MT1,LeftOccs,[ff],MTNew).


/*----------------------------------------------------------------------*/
/* all_left_occs(+Occ,+Occs,-LeftOccs)                                  */
/* computes the set of all occurrences which are left from Occ,         */
/* belonging to Occs                                                    */
/* 15.11.88 TRY2 tested 15.11.88 19:47 */

all_left_occs(_Occ1,[],[]) :- !.
all_left_occs(Occ1,[Occ2|Occs],[Occ2|LeftOccs]) :- 
   left(Occ2,Occ1),
   all_left_occs(Occ1,Occs,LeftOccs), !.
all_left_occs(Occ1,[_Occ2|Occs],LeftOccs) :-
   all_left_occs(Occ1,Occs,LeftOccs), !. 


/*----------------------------------------------------------------------*/
/* left(+Occ1,+Occ2)                                                    */
/* succeeds if Occ1 is left from Occ2                                   */
/* 15.11.88 TRY2 tested 15.11.88 19:48 */

left(Occ1,Occ2) :-
   not_identical_parts(Occ1,Occ2,[J1|_],[J2|_]),
   J1 < J2.

not_identical_parts([O1|Occ1],[O1|Occ2],Occ1N,Occ2N) :-
   !, not_identical_parts(Occ1,Occ2,Occ1N,Occ2N).
not_identical_parts(Occ1,Occ2,Occ1,Occ2):- !.


/*----------------------------------------------------------------------*/
/* sufficiently_large(+OccSet,+Term)                                    */
/* checks if OccSet is sufficiently large for Term                      */

sufficiently_large(_MT,_Term) :- !.


/*----------------------------------------------------------------------*/
/* weakly_based((+Path,+RuleDescription,+SubstitutedRightRuleSide,      */
/*               +OldOccs),-NewOccs)                                    */
/* computes the weakly-based occurrences NewOccs from OldOccs, if the   */
/* rule descibed by RuleDescription is applied at Path in an term and   */
/* the term SubstitutedRightRuleSide was inserted                       */
/* 17.11.88 TRY3 */

weakly_basic((Path,[(_TC,[_L = _R]),VarsR,VarOccsL,VarOccsR],RSsubst,MT1),
             MT3) :-
   marked_term(RSsubst,FMSR,vars(ff),nonvars(tt),nonvars),
   change_path_arg(Path,MT1,MT2,FMSR),
   iter(examine_a_variable,MT2,VarsR,
        [MT1,Path,RSsubst,left(VarOccsL),right(VarOccsR)],MT3).
   

/*----------------------------------------------------------------------*/
/* examine_a_variable(+Var,-NewOccs,+ResultOccsSoFar,+OriginalOccs,     */
/*                    +Path,+RightRuleSideSubstituted,                  */
/*                    left(+VarOccsL),right(+VarOccsR))                 */
/* adds all occurrences which antecedents belong completly to OldOccs   */
/* 17.11.88 TRY3 */

examine_a_variable(X,MT2,MT1,
                   MTOld,Path,RSsubst,left(VarOccsL),right(VarOccsR)) :-
    look_for_var_occs(X,XOccsL,VarOccsL),
    look_for_var_occs(X,XOccsR,VarOccsR),
    [OneXOcc|_OtherXOccs] = XOccsR,
    path_arg(OneXOcc,RSsubst,TermSubstitutedForX),
    nonvar_occurrences(TermSubstitutedForX,Occs),
    iter(add_narrowable_occ,MT1,Occs,[MTOld,Path,X,XOccsL,XOccsR],MT2).

/*----------------------------------------------------------------------*/
/* add_narrowable_occ(+Occ,-NewOccs,+OldOccs,+OriginalOccs,+Path,       */
/*                    +X,+XOccsL,+XOccsR)                               */
/* After substituting variable X there is a subterm with occurrence     */
/* Occ. First the antecedents of Occ are computed, then it is checked   */
/* that all antecedents belong to OriginalOccs. If this is true, then   */
/* all subterms are marked with tt, otherwise with ff.                  */
/* 15.11.88 TRY2 */
/* 15.11.88 20:12 revision berechnung der zu markierenden occurrences
                           erfolgt nun hier nicht in mark_all_occs */
/* 17.11.88 TRY3 keine Aenderung */

add_narrowable_occ(OccBelowX,MT2, MT1, MTOld,Path,_X,XOccsL,XOccsR) :-
   mapL(connect_occs,XOccsL,LookupList,[Path,OccBelowX]),
   mapL(markierung,LookupList,MarkList,[MTOld]),
   (all_antecedents_marked(MarkList) ->
      (mapL(connect_occs,XOccsR,CompleteOccs,[Path,OccBelowX]),
       mark_all_occs(CompleteOccs, MT2, MT1,tt))
      ;
      (mapL(connect_occs,XOccsR,CompleteOccs,[Path,OccBelowX]),
       mark_all_occs(CompleteOccs, MT2, MT1,ff))
   ).


/*----------------------------------------------------------------------*/
/* connect_occs(+SecondPart,-MarkPosition,+FirstPart,+ThirdPart)        */
/* computes the occurrence MarkPosition from the three peaces           */
/* FirstPart,SecondPart and ThirdPart                                   */
/* 15.11.88 TRY2 tested 15.11.88 20:01 */

connect_occs(O1,MarkPosition,Path,OccBelowX) :-
   append(Path,O1,FirstandSecondPart),
   append(FirstandSecondPart,OccBelowX,MarkPosition).

/*----------------------------------------------------------------------*/
/* mark_all_occs(+Occs,-NewOccs,+OldOccs,+Mark)                         */
/* compute the set of occurrences NewOccs from OldOccs by changing      */
/* all occurrences in Occs to Mark                                      */
/* 15.11.88 TRY2 */
/* 15.11.88 20:12 revision tested 15.11.88 20:17 */

mark_all_occs(Occs,MT2, MT1,Mark) :-
   iter(mark,MT1,Occs,[Mark],MT2).


/*----------------------------------------------------------------------*/
/* mark(+Path,-NewOccs,+OldOccs,+Mark)                                  */
/* changes in OldOccs the mark at position Path to Mark getting NewOccs */
/* 14.11.88 TRY2 tested 15.11.88 19:57 */

mark(MarkPosition,MT2,MT1,Mark) :-
   path_arg(MarkPosition,MT1,Subterm1),
   Subterm1 =.. [_OldMark|Args],
   Subterm2 =.. [Mark|Args],
   change_path_arg(MarkPosition,MT1,MT2,Subterm2).


/*----------------------------------------------------------------------*/
/* marked_term(+Term,-MarkedTerm,var(+Mark1),nonvar(+Mark2),+Kind)      */
/* makes a copy from a term:                                            */
/* If Kind = vars, then the variables of Term stay unchanged, but all   */
/* other functors are replaced by Mark2.                                */
/* If Kind = nonvars, then variables will be replaced by Mark1, while   */
/* all other functors are replaced by Mark2.                            */
/* 14.11.88 TRY2 tested 15.11.88 19:59 */

marked_term(X,X,vars(_Mark1),nonvars(_Mark2),vars) :- 
	var(X), !.
marked_term(X,Mark1,vars(Mark1),nonvars(_Mark2),nonvars) :- 
	var(X), !.
marked_term(T,Mark2,vars(_Mark1),nonvars(Mark2),_Kind) :- 
	atomic(T), !.
marked_term(T,FMT,vars(Mark1),nonvars(Mark2),Kind) :-
	T =.. [_OP|Args],
	mapL(marked_term,Args,FMTArgs,[vars(Mark1),nonvars(Mark2),Kind]),
	FMT =..[Mark2|FMTArgs].


/*----------------------------------------------------------------------*/
/* markierung(+Occ,-Mark,+Occs)                                         */
/* gives back tt if Occ belongs to Occs, ff otherwise                   */
/* 14.11.88 TRY2 tested 15.11.88 20:19 */

markierung(Occ,Mark,MarkedTerm) :- 
   path_arg(Occ,MarkedTerm,Subterm),
   functor(Subterm,Mark,_Arity), !.
markierung(_Occ,ff,_MarkedTerm) :- !.

/*----------------------------------------------------------------------*/
/* all_antecedents_marked(+List)                                        */
/* if all elements of List are tt, the the goal succeeds, otherwise     */
/* it wil fail.                                                         */

all_antecedents_marked([]) :- !.
all_antecedents_marked([tt|L]) :- all_antecedents_marked(L), !.
all_antecedents_marked([ff|_L]) :- !, fail.


/*----------------------------------------------------------------------*/
/* look_for_var_occs(+Var,-Occs,+VarOccs)                               */
/* gives back the set of all occurrences of Var which are remembered    */
/* in VarOccs                                                           */
/* 891106 changes inlist into member_s

look_for_var_occs(Var,Occs,VarOccs) :- 
	member_s((Var,Occs),VarOccs).
look_for_var_occs(_Var,[],_VarOccs) :- !.


   
/*----------------------------------------------------------------------*/
/* weakly_based(+OldTerm,+OldOccs,+NewTerm,-NewOccs)                    */
/* computes from OldTerm, OldOccs and NewTerm a set of occurrences      */
/* NewOccs that is at least weakly_based                                */
/* 18.11.88 TRY4 */

weakly_based(OldTerm,MTOld,NewTerm,MTNew) :-
   marked_term(NewTerm,MT,vars(ff),nonvars(ff),nonvars),
   nonvar_occurrences(NewTerm,Occs),
   sort_list(above,Occs,SortOccs),
   iter(subterm_analysis,MT,SortOccs,[OldTerm,MTOld,NewTerm],MTNew), !.
   

subterm_analysis(Path,MTNew,MTSoFar,OldTerm,MTOld,NewTerm) :-
   (markierung(Path,ff,MTSoFar) ->
      (path_arg(Path,NewTerm,Subterm),
       all_antecedents(OldTerm,Subterm,LookupList),
       (LookupList = [] ->
           (mark_along_occ(Path,MTSoFar,MTNew,tt))
           ;
           (mapL(markierung,LookupList,MarkList,[MTOld]),
            (all_antecedents_marked(MarkList) ->
                (mark_along_occ(Path,MTSoFar,MTNew,tt))
                ;
                (MTNew = MTSoFar)
            )
           )
       )
      )
      ;
      (MTNew = MTSoFar)
   ).
       

all_antecedents(OldTerm,Subterm,Occs) :-
   bagof(Occ,find_subterm(Occ,OldTerm,Subterm),Occs), !.
all_antecedents(_OldTerm,_Subterm,[]) :- !.

mark_along_occ([],MT1,MT2,Mark) :-
   MT1 =.. [_OldMark|Args],
   MT2 =.. [Mark|Args].
mark_along_occ([O1|Occ],MT1,MT2,Mark) :-
   MT1 =.. [_OldMark|Args1],
   mark_one_arg(O1,Occ,Args1,Args2,Mark),
   MT2=..[Mark|Args2].

mark_one_arg(1,Occ,[T1|Ts],[T2|Ts],Mark) :-   
   mark_along_occ(Occ,T1,T2,Mark), !.
mark_one_arg(N,Occ,[T1|T1s],[T1|T2s],Mark) :-
   M is N-1, !,
   mark_one_arg(M,Occ,T1s,T2s,Mark).
    

above([],_Occ) :- !,fail.
above(_Occ,[]) :- !.
above([_O1|Occ1],[_O2|Occ2]) :- above(Occ1,Occ2).


normalize(T,term(N),MTOld,MTNew) :-
   internalTermRep(T,TT),
   marked_term(TT,MTOld,vars(ff),nonvars(ff),nonvars),
   reduceU([],TT,N),
   weakly_based(TT,MTOld,N,MTNew).

   



