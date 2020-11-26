/*

"ics_quant.pl"

Applies D5 rules on quantification to ICs.

Intended to rewrite the ICs input by the user.

Should be OK, unless I find bugs, as of 30sept03.

*/

:-module(ics_quant,
	 [quantify_variables_in_ics/2,
      ics_scan_head/3, adjust_variable_list/2]). % used by the unfolding transition

:-use_module(quantif).

:-use_module(library(lists)).

:-use_module(library(terms),
	     [term_variables/2]).

:- use_module(solver).

operator(700,xfx,ne).

/*

>From D5:

"1. A variable must occur at least in an Event or in an Expectation.
 
2. The variables that occur both in the body and in the head are
quantified universally with scope the entire ICS .
 
3. The variables that occur only in the head must occur in at least
one Expectation in Eq. (7), and (a) if they occur in literals E or ¬E
are quantified existentially and have as scope the disjunct they
belong to; (b) otherwise they are quantified universally.

subst_unif_to_atom1([],_,[]).
subst_unif_to_atom1([Head|Tail],Hap,Body):-
	reif_unify(Head,Hap,1),
	!,
	subst_unif_to_atom1(Tail,Hap,Body).
subst_unif_to_atom1([Head|Tail],Hap,[Head|Body]):-
	subst_unif_to_atom1(Tail,Hap,Body).


empty_body_psic @
	psic([],Head)
	<=>
	impose_head(Head).

impose_head([]):-
	fail.

(MarcoA's note: the beginning of rule 3 seems redundant to me, for it
seems implied by rule 1 and the fact that in the head events cannot
occur; maybe after (7) "in the same disjunct" should be added. I will
implement this, for I think it makes sense).

4. The variables that occur only in the body have the body as scope
and (a) if they occur only in conjunctions of ¬H, NE, ¬NE or
Constraints are quantified universally; (b) otherwise are quantified
existentially.
 
5. the quantifier FORALL has higher priority than EXISTS (e.g.,
literals will be quantified EXISTS-FORALL and not viceversa)."

For the time being, I will forget about syntactic restrictions (I'll
assume the user writes legal ICS). I will also forget about the scope
of variables (in particular, the user is kindly requested to name
variables that appear in different disjuncts in the head, and not in
the body, differently)..

Summarizing, a variable is FORALL iff:

- it appears both in the head and in the body, or it appears only in
the head, but only in E or ¬E.

A variable is EXISTS iff it is not FORALL.

*/

/*

quantify_variables_in_ics(ICS,ICS1)

ICS1 is ICS (possibly with some variables renamed because of scope
rules); the variables are quantified according to the rules in D5
(forall or exist), which is written in an attribute (see module
"quantif").

*/


quantify_variables_in_ics(ICS,ICS1):-
	ics_variables(ICS,ics(Body,Head),Variables),
	quantify_variables(Variables),
	split_body(Body,Body1),
	shuffle_head(Head,Head1),
	ICS1=ics(Body1,Head1).

ics_variables(ics(Body,Head),ics(Body,Head1),Variables):-
	ics_scan_body(Body,[],BodyVariables),
	ics_scan_head(Head,BodyVariables,ICSVariables),
	adjust_variable_list(ICSVariables,RepeatedVariables),
	modify_head(RepeatedVariables,Head,Head1,Variables).

quantify_variables([]).
quantify_variables([Variable|MoreVariables]):-
	decide_variable_quantification(Variable,Quantification),
	attach_variable_quantification(Variable,Quantification),
	quantify_variables(MoreVariables).


attach_variable_quantification(variable(VarName,_),Quantification):-
    (get_quant(VarName,_)
        ->	true
        ;   quant(VarName,Quantification)).

% Modified by Federico Chesani
% Date 20060404 1300
split_body([],[[],[],[],[],[],[],[],[]]).
split_body([h(Event,Time)|MoreAtoms],
	   [[h(Event,Time)|More],NH,E,NE,EN,NEN,ABD,O]):-
	!,
	split_body(MoreAtoms,[More,NH,E,NE,EN,NEN,ABD,O]).
split_body([noth(Event,Time)|MoreAtoms],
	   [H,[noth(Event,Time)|More],E,NE,EN,NEN,ABD,O]):-
	!,
	split_body(MoreAtoms,[H,More,E,NE,EN,NEN,ABD,O]).
split_body([e(Event,Time)|MoreAtoms],
	   [H,NH,[e(Event,Time)|More],NE,EN,NEN,ABD,O]):-
	!,
	split_body(MoreAtoms,[H,NH,More,NE,EN,NEN,ABD,O]).
split_body([note(Event,Time)|MoreAtoms],
	   [H,NH,E,[note(Event,Time)|More],EN,NEN,ABD,O]):-
	!,
	split_body(MoreAtoms,[H,NH,E,More,EN,NEN,ABD,O]).
split_body([en(Event,Time)|MoreAtoms],
	   [H,NH,E,NE,[en(Event,Time)|More],NEN,ABD,O]):-
	!,
	split_body(MoreAtoms,[H,NH,E,NE,More,NEN,ABD,O]).
split_body([noten(Event,Time)|MoreAtoms],
	   [H,NH,E,NE,EN,[noten(Event,Time)|More],ABD,O]):-
	!,
	split_body(MoreAtoms,[H,NH,E,NE,EN,More,ABD,O]).
split_body([abd(Event,Time)|MoreAtoms],
	   [H,NH,E,NE,EN,NEN,[abd(Event,Time)|More],O]):-
	!,
	split_body(MoreAtoms,[H,NH,E,NE,EN,NEN,More,O]).
split_body([Atom|MoreAtoms],
	   [H,NH,E,NE,EN,NEN,ABD,[Constraint|More]]):-
	rewrite_constraint(Atom,Constraint1),
	!,
	check_if_restriction(Constraint1,Constraint),
	split_body(MoreAtoms,[H,NH,E,NE,EN,NEN,ABD,More]).
split_body([Atom|MoreAtoms],
	   [H,NH,E,NE,EN,NEN,ABD,[Atom|More]]):-
	split_body(MoreAtoms,[H,NH,E,NE,EN,NEN,ABD,More]).
% End Modification
% Original Version:
/*
split_body([],[[],[],[],[],[],[],[],[]]).
split_body([h(Event,Time)|MoreAtoms],
	   [[h(Event,Time)|More],NH,E,EN,NE,NEN,ABD,O]):-
	!,
	split_body(MoreAtoms,[More,NH,E,EN,NE,NEN,ABD,O]).
split_body([noth(Event,Time)|MoreAtoms],
	   [H,[noth(Event,Time)|More],E,EN,NE,NEN,ABD,O]):-
	!,
	split_body(MoreAtoms,[H,More,E,EN,NE,NEN,ABD,O]).
split_body([e(Event,Time)|MoreAtoms],
	   [H,NH,[e(Event,Time)|More],EN,NE,NEN,ABD,O]):-
	!,
	split_body(MoreAtoms,[H,NH,More,EN,NE,NEN,ABD,O]).
split_body([en(Event,Time)|MoreAtoms],
	   [H,NH,E,[en(Event,Time)|More],NE,NEN,ABD,O]):-
	!,
	split_body(MoreAtoms,[H,NH,E,More,NE,NEN,ABD,O]).
split_body([note(Event,Time)|MoreAtoms],
	   [H,NH,E,EN,[note(Event,Time)|More],NEN,ABD,O]):-
	!,
	split_body(MoreAtoms,[H,NH,E,EN,More,NEN,ABD,O]).
split_body([noten(Event,Time)|MoreAtoms],
	   [H,NH,E,EN,NE,[noten(Event,Time)|More],ABD,O]):-
	!,
	split_body(MoreAtoms,[H,NH,E,EN,NE,More,ABD,O]).
split_body([abd(Event,Time)|MoreAtoms],
	   [H,NH,E,EN,NE,NEN,[abd(Event,Time)|More],O]):-
	!,
	split_body(MoreAtoms,[H,NH,E,EN,NE,NEN,More,O]).
split_body([Atom|MoreAtoms],
	   [H,NH,E,EN,NE,NEN,ABD,[Constraint|More]]):-
	rewrite_constraint(Atom,Constraint1),
	!,
	check_if_restriction(Constraint1,Constraint),
	split_body(MoreAtoms,[H,NH,E,EN,NE,NEN,ABD,More]).
split_body([Atom|MoreAtoms],
	   [H,NH,E,EN,NE,NEN,ABD,[Atom|More]]):-
	split_body(MoreAtoms,[H,NH,E,EN,NE,NEN,ABD,More]).
*/



shuffle_head([],[]).
shuffle_head([Disjunct|MoreDisjuncts],
	     [ShuffledDisjunct|MoreShuffledDisjuncts]):-
	shuffle_disjunct(Disjunct,ShuffledDisjunct),
	shuffle_head(MoreDisjuncts,MoreShuffledDisjuncts).

shuffle_disjunct(Disjunct,ShuffledDisjunct):-
	modify_conjuncts(Disjunct,KeyedDisjunct),
	keysort(KeyedDisjunct,SortedDisjunct),
	remove_keys(SortedDisjunct,ShuffledDisjunct).

remove_keys([],[]).
remove_keys([_-V|KMore],[V|More]):-
	remove_keys(KMore,More).

modify_conjuncts([],[]).
modify_conjuncts([Conjunct|MoreConjuncts],
		 [ModifiedConjunct|MoreModifiedConjuncts]):-
	modify_conjunct(Conjunct,ModifiedConjunct),
	modify_conjuncts(MoreConjuncts,MoreModifiedConjuncts).

modify_conjunct(Conjunct,1-Conjunct):-
	is_abducible(Conjunct),
	!.
modify_conjunct(Conjunct,0-Constraint):-
	rewrite_constraint(Conjunct,Constraint1),
	!,
	check_if_restriction(Constraint1,Constraint).

modify_conjunct(Conjunct,2-Conjunct).

is_abducible(Conjunct):-
	functor(Conjunct,F,N),
	abducible_signature(F,N).

abducible_signature(e,2).
abducible_signature(en,2).
abducible_signature(note,2).
abducible_signature(noten,2).
abducible_signature(abd,2).

% MarcoG 13 Oct 2006: This first clause is necessary because the rewriting of
% ICs is also called by Unfolding, so constraints may be already labelled
% clp_constraint.
rewrite_constraint(clp_constraint(C),clp_constraint(C)):- !.

rewrite_constraint(unif(A,B),clp_constraint(reif_unify(A,B,1))):-
	!.
rewrite_constraint(not_unif(A,B), clp_constraint(reif_unify(A,B,0))):-
	!.
rewrite_constraint(Constraint,clp_constraint(Constraint1)):-
    solver_rewrite_constraint(Constraint,Constraint1).
	

/*
check_if_restriction(Constraint1,Constraint2):-
	contains_universal_variables(Constraint1) ->
	Constraint2=st(Constraint1);
	Constraint2=Constraint1.
*/
check_if_restriction(clp_constraint(st(Arg1)),clp_constraint(st(Arg1))):- !.
check_if_restriction(clp_constraint(st(Arg1, Arg2)),clp_constraint(st(Arg1, Arg2))) :- !.
check_if_restriction(clp_constraint(Constraint1),clp_constraint(Constraint2)):-
	only_universal_variables(Constraint1) ->
	Constraint2=st(Constraint1);
	Constraint2=Constraint1.






/*
contains_universal_variables(Term):-
	term_variables(Term,Variables),
	at_least_one_universal(Variables).

at_least_one_universal([Var]):-
	get_quant(Var,forall),
	!.
at_least_one_universal([_|MoreVars]):-
	at_least_one_universal(MoreVars).
*/

only_universal_variables(Term):-
	term_variables(Term,Variables),
	all_universal(Variables).

all_universal([Var|_]):-
	is_existential(Var),
	!,
	fail.
all_universal([_|MoreVars]):-
	all_universal_1(MoreVars).

all_universal_1([]).
all_universal_1([Var|_]):-
	is_existential(Var),
	!,
	fail.
all_universal_1([_|MoreVars]):-
	all_universal_1(MoreVars).

/*

This ugly code is to implement scope restrictions: in IC

H(p(X)) -> E(q(Y)) OR E(r(Y)),

the two Ys are different varibables.

As usual, naming of predicates and variables is poor.

*/

modify_head(RepeatedVariables,Head,Head1,Variables):-
	modify_head(RepeatedVariables,Head,Head1,[],Variables).

modify_head([],Head,Head,Variables,Variables).

modify_head([RVar|MoreRVars],OldHead,NewHead,OldVars,NewVars):-
	var_modify_head(RVar,OldHead,IntHead,Vars),
	append(OldVars,Vars,IntVars),
	modify_head(MoreRVars,IntHead,NewHead,IntVars,NewVars).


var_modify_head(Var,OldHead,NewHead,Vars):-
	var_to_var_list(Var,Vars),
	var_modify_head(Vars,OldHead,NewHead).

var_modify_head([_],Head,Head):-
	!.
var_modify_head([variable(VarName,_)|MoreVars],OldHead,NewHead):-
	replace_vars_in_head(MoreVars,VarName,OldHead,NewHead).

replace_vars_in_head([],_,Head,Head).
replace_vars_in_head([variable(RName,
			       [occur(head(Disjunct),_)|_])|MoreVars],
		     OName,OldHead,NewHead):-
	replace_var_in_head(OldHead,1,Disjunct,OName,RName,IntHead),
	replace_vars_in_head(MoreVars,OName,IntHead,NewHead).

replace_var_in_head([Disjunct|MoreDisjuncts],N,N,OName,RName,
		    [NewDisjunct|MoreDisjuncts]):-
	!,
	ics_replace(Disjunct,OName,RName,NewDisjunct).
replace_var_in_head([Disjunct|MoreDisjuncts],M,N,OName,RName,
		    [Disjunct|MoreNewDisjuncts]):-
	M1 is M+1,
	replace_var_in_head(MoreDisjuncts,M1,N,OName,RName,
			    MoreNewDisjuncts).


/*

>From a variable(VarName,SplitOccurList) term, create many
variable(NewVarName,SplitOccur) terms. Note that the first new
variable is named as the original.

If a variable appears in the body, it will be the first occurrence.

In this case, any other occurrence of the variable will represent the same variable, because:

- either it is only in the body, which counts as a disjunct;

- or it is also in the head, but then it has the whole IC as scope.

*/

var_to_var_list(Variable,[Variable]):-
	Variable=variable(_,[occur(body,_)|_]),
	!.

var_to_var_list(variable(VarName,OccurList),VarList):-
	split_occurrence_list(OccurList,SplittedOccurList),
	create_var_list(VarName,SplittedOccurList,VarList).


split_occurrence_list([occur(head(N),Type)|MoreOccurs],
		      [[occur(head(N),Type)|MoreOccurs1]|MoreLists]):-
	split_occurrence_list(MoreOccurs,N,[MoreOccurs1|MoreLists]).

split_occurrence_list([],_,[[]]).
split_occurrence_list([occur(head(N),Type)|MoreOccurs],N,
		      [[occur(head(N),Type)|MoreOccurs1]|MoreLists]):-
	!,
	split_occurrence_list(MoreOccurs,N,[MoreOccurs1|MoreLists]).
split_occurrence_list([occur(head(N),Type)|MoreOccurs],_,
		      [[],[occur(head(N),Type)|MoreOccurs1]|MoreLists]):-
	split_occurrence_list(MoreOccurs,N,[MoreOccurs1|MoreLists]).

create_var_list(VarName,[OccurList|MoreOccurLists],
		[variable(VarName,OccurList)|MoreVariables]):-
	create_var_list(MoreOccurLists,MoreVariables).

create_var_list([],[]).
create_var_list([OccurList|MoreOccurLists],
		[variable(_,OccurList)|MoreVariables]):-
	create_var_list(MoreOccurLists,MoreVariables).

/*

First, scan the IC for variables. Returns a list of data structures

variable(VarName,Occurrences)

where:

- VarName is the name of the variable;

- Occurrences is a list of terms of the form 'occur(Where,Type)', where:

--- Where is the part of the IC where it appears (can be 'body' or
'head(N)', where N is the ordinal number of the disjunct of the head
where the variable appears - this is needed to understand if two
variables appear in the same head disjunct);

--- Type is the type of predicate where the variable appears (can be
'h', 'noth', 'e', 'note', 'ne', 'noten', 'other').

If a variables appears in different head disjuncts (and not in the
body), then different variables are created.

*/

ics_scan_body([],Variables,Variables).
ics_scan_body([Conjunct|MoreConjuncts],OldVariables,NewVariables):-
	conjunct_variables(Conjunct,body,ConjunctVariables),
	append(OldVariables,ConjunctVariables,IntVariables),
	ics_scan_body(MoreConjuncts,IntVariables,NewVariables).

ics_scan_head(Head,OldVariables,NewVariables):-
	ics_scan_head(Head,1,OldVariables,NewVariables).

ics_scan_head([],_,Variables,Variables).
ics_scan_head([Disjunct|MoreDisjuncts],DisjunctNumber,
	      OldVariables,NewVariables):-
	ics_scan_disjunct(Disjunct,DisjunctNumber,
			  OldVariables,IntVariables),
	NewDisjunctNumber is DisjunctNumber+1,
	ics_scan_head(MoreDisjuncts,NewDisjunctNumber,
		      IntVariables,NewVariables).

ics_scan_disjunct([],_,Variables,Variables).
ics_scan_disjunct([Conjunct|MoreConjuncts],DisjunctNumber,
		  OldVariables,NewVariables):-
	conjunct_variables(Conjunct,head(DisjunctNumber),ConjunctVariables),
	append(OldVariables,ConjunctVariables,IntVariables),
	ics_scan_disjunct(MoreConjuncts,DisjunctNumber,
			  IntVariables,NewVariables).

conjunct_variables(Conjunct,Where,ConjunctVariables):-
	Conjunct=..[Functor|_],
	functor_type(Functor,Type),
	term_variables(Conjunct,Variables),
	build_variables_list(Variables,Where,Type,ConjunctVariables).

build_variables_list([],_,_,[]).
build_variables_list([Variable|MoreVariables],Where,Type,
		     [variable(Variable,Where,Type)|MoreVarStructs]):-
	build_variables_list(MoreVariables,Where,Type,MoreVarStructs).
	
functor_type(h,h):-
	!.
functor_type(noth,noth):-
	!.
functor_type(e,e):-
	!.
functor_type(note,note):-
	!.
functor_type(en,en):-
	!.
functor_type(noten,noten):-
	!.
functor_type(abd,abd):-
	!.
functor_type(C,clp_constraint):-
    is_clp_functor(C),
	!.
functor_type(_,other).

adjust_variable_list([],[]):-
	!.
adjust_variable_list(Variables,
		     [VarStruct|MoreVarStructs]):-
	get_var_struct(Variables,VarStruct,Remaining),
	adjust_variable_list(Remaining,MoreVarStructs).

get_var_struct([variable(VarName,Where,Type)|MoreVariables],
	       variable(VarName,[occur(Where,Type)|MoreOccurs]),
	       Remaining):-
	get_more_occurs(MoreVariables,VarName,MoreOccurs,Remaining).

get_more_occurs([],_,[],[]).
get_more_occurs([variable(VarName1,Where,Type)|MoreVariables],
		VarName,[occur(Where,Type)|MoreOccurs],Remaining):-
	VarName1==VarName,
	!,
	get_more_occurs(MoreVariables,VarName,MoreOccurs,Remaining).
get_more_occurs([Variable|MoreVariables],VarName,Occurs,
		[Variable|MoreRemaining]):-
	get_more_occurs(MoreVariables,VarName,Occurs,MoreRemaining).

/*

When we have the list of variables in good order, we can decide
their quantification depending on their occurrences.  Representing the
list of variables in this way makes it fairly easy to implement the
syntactic restrictions in D5.

*/

/*

I'm removing this temporarily to allow auxiliary variables for CLP
constraints 

decide_variable_quantification(V,_):-
	illegal(V),
	!,
	fail. %% Some output would be nice here...

*/
decide_variable_quantification(variable(_,OccurList),forall):-
	in_head_and_body(OccurList),
	!.
decide_variable_quantification(variable(_,OccurList),Quantification):-
	first_in_head(OccurList),
	!,
	head_quantify(OccurList,Quantification).
decide_variable_quantification(variable(_,OccurList),Quantification):-
	body_quantify(OccurList,Quantification).
illegal(variable(_,Occurrences)):-
	only_in_others(Occurrences).

only_in_others([]).
only_in_others([occur(_,other)|MoreOccurrences]):-
	only_in_others(MoreOccurrences).


in_head_and_body([occur(body,_)|MoreOccurs]):-
	in_head(MoreOccurs).

in_head([occur(head(_),_)|_]):-
	!.
in_head([_|MoreOccurrences]):-
	in_head(MoreOccurrences).
	
first_in_head([occur(head(_),_)|_]).

head_quantify(OccurList,exists):-
	in_e_or_note_or_abd_or_other(OccurList),
	!.
head_quantify(_,forall).

body_quantify(OccurList,forall):-
	in_h(OccurList),
	!.
body_quantify(OccurList,forall):-
	in_e_or_note_or_abd(OccurList),
	!.
body_quantify(OccurList,forall):-
	only_in_other(OccurList),
	!.
body_quantify(_,exists).

only_in_other([]).
only_in_other([occur(_,other)|Tail]):-
	only_in_other(Tail).
only_in_other([occur(_,clp_constraint)|Tail]):-
	only_in_other(Tail).

in_e_or_note_or_abd([occur(_,e)|_]):-
	!.
in_e_or_note_or_abd([occur(_,note)|_]):-
	!.
in_e_or_note_or_abd([occur(_,abd)|_]):-
	!.
in_e_or_note_or_abd([_|MoreOccurrences]):-
	in_e_or_note_or_abd(MoreOccurrences).


in_e_or_note_or_abd_or_other([occur(_,e)|_]):-
	!.
in_e_or_note_or_abd_or_other([occur(_,note)|_]):-
	!.
in_e_or_note_or_abd_or_other([occur(_,abd)|_]):-
	!.
in_e_or_note_or_abd_or_other([occur(_,other)|_]):-
	!.
in_e_or_note_or_abd_or_other([_|MoreOccurrences]):-
	in_e_or_note_or_abd_or_other(MoreOccurrences).



in_h([occur(_,h)|_]):-
	!.
in_h([_|MoreOccurrences]):-
	in_h(MoreOccurrences).

/*

ics_replace(T,O,R,T1)

T1 is T with O replaced by R.

Isn't there anyhing of the like built-in???

*/

ics_replace(T,O,R,T1):-
	compound(T),
	!,
	T=..[F|Args],
	replace_list(Args,O,R,Args1),
	T1=..[F|Args1].

ics_replace(T,X,Y,Y):-
	T==X,
	!.
ics_replace(T,_,_,T).
	
replace_list([],_,_,[]).
replace_list([H|T],O,R,[H1|T1]):-
	ics_replace(H,O,R,H1),
	replace_list(T,O,R,T1).
