%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: tom.pl,v 1.3 1994/05/31 20:03:48 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(tom , []). 
/*%--------------------------------------

\input{tom_man}

\PL*/

:- export(tom/2).

:- module(tom).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This is a magic cookie for identification. Dear user, don't you worry!

\PL*/

info(filter,"1.1","Translation of multi-modal formulae into clause form").

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A few general options are set...

\PL*/

%:-       get_flag(library_path,OldPath),
%	union(["/u/home/procom/System"],OldPath,FullPath),
%	set_flag(library_path,FullPath).

:-       set_flag(syntax_option,nl_in_quotes).

other_prolog_libs:-
	lib(lists),
	lib(options),
	lib(op_def),
	lib(numbervars).

:-       op(500, fx, $).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now, we define an output option |'Tom:log_file'|. If this is set to |on|, the output of
the filter will be written to a log file, the name of which results from
the original name of the problem. This happens additionally to sending the
output to a stream.
\PL*/
:- define_option('Tom:log_file' = on).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Furthermore, we set a global variable for the identification of the Skolem functions.
\PL*/
:- setval(skolemCounter,1).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Lastly, we declare a
dynamic predicate. This is necessary as we assert the Prolog clauses
of the theory part to the Prolog data base and flush the data base in the end.

\PL*/

:- dynamic constraint/3.

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The global counter |current_depth| is somewhat fictional. This has been done
to give the depth bound some limit.

\PL*/

:- setval(current_depth,10000).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We read the internal language of the system. If you don't like it, then provide
your own. We suggest to use the filter |mpp.pl| prior to this one though
(|tom.pl|, that is).

\PL*/

:- compile('tom_ops.pl').

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate tom/2(+InStream, +OutStream).

This, being the main filter predicate, conforms to the \ProCom{} filter
specification. In a
failure-driven loop clauses are read from the stream |InStream|.
A case split treats all (possible) events (well, the ones I could think of
anyway).

The first part of this clause sets up a frame for the rest. Several options
are set to their default values. The \ProCom{} option |input_filter| is
checked according to the option |'Tom:method'|. Furthermore, the latter
option determines which file to compile for further processing.

\PL*/

tom(InStream,OutStream) :-
	is_option(input_file,InputFileNameAtom),
	set_default_options,
	( is_option('Tom:method',constraints) ->
	    compile('tom_constraints.pl'),
	    is_option(input_filter,AListOfFilters),
	    ( member('constraints',AListOfFilters) -> 
	        true
	    ; writeln(OutStream,"
*** Filter  constraints  not provided. Trying to fix this..."),
	       set_option(input_filter = constraints)
	    )
	; compile('tom_inference.pl')
	),
	( is_option('Tom:log_file') ->
	    concat_atom(['...',InputFileNameAtom,'.pl'],File),
	    set_option('tee:file' = File),
	    is_option(input_filter,AnotherListOfFilters),
	    ( AnotherListOfFilters = [tom, AnyFilter | Tail],
	      ( AnyFilter = tee ->
		  true
	      ; set_option(input_filter = [tom, tee, AnyFilter | Tail])
	      )
	    ; set_option(input_filter = [tom, tee])
	    )
	; true
	),
	setval(skolemCounter,1),
	repeat,
	read(InStream,Clause),
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If we encounter the end of the input file, we are done.
\PL*/
	( Clause = end_of_file ->
	    true

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
It is possible to enter any clause in Prolog format. Thus the user
can specify any kind of formula as an axiom. However, the reader
has to make sure that all conventions are taken care of, especially
the conventions concerning naming etc. The auxiliary predicate is
always called |$Rmod| followed by an underscore and the name of an
accessibility relation. These Prolog predicate names have to be quoted
as there might be problems for the system in understanding names
beginning with the |$| sign.
\begin{description}
\item[Example:] \begin{verbatim}
# modal_axiom_formula(('$Rmod_a'(X + Y) :-
     '$Rmod_a'(X),
     '$Rmod_a'(Y))).\end{verbatim}
\end{description}
This is the formula for the transitivity of the accessibility relation |a|.
No other things are done. We discourage the use of this option.

\PL*/
	; Clause = (# modal_axiom_formula(Axiom)) ->
	   find_type(Axiom,Type),
	   assert(constraint(Type,Axiom,matrix)),
	   fail
      
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Of course, there is the possibility of specifying one of the standard
axioms of the accessibility relation of the modal logic (i.e.\ transitive,
serial etc.) by using the command |# use_axiom_schema|. The name of the
schema  or its (usual) abbreviation, and in either case the sort this axiom
refers to needs to be specified. No quotes are necessary but brackets.

\begin{description}
\item[Example:] \begin{verbatim}
                # modal_axiom_schema(transitive, a).
                \end{verbatim}
%\Xsample{{\tt \# modal\_axiom\_schema(transitive,a).}
In this clause, the transitivity of the accessibility relation |a| is
specified. However, nothing else is done.
\end{description}

\PL*/
        ; Clause = (# modal_axiom_schema(Axiom,Sort)) ->
	   assert_axiom_schema(Axiom,Sort),
	   fail
       
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
A third possibility provides the specification of one of the standard
modal systems. Again, the corresponding sort has to be given. A list of all
the known axiom schemas and theories will be presented later.

If you want to use one of the standard modal logics, we suggest to use this
form as all necessary options are set accordingly.

\Xsample{{\tt \# modal\_standard\_theory(s4,a).}
Here, the logic S4 is used for the accessibity relation |a|. Other possible
logics are KD, KD4, KD5, KD45, KT, KT4, KT5, KT45, and S5.}

At the moment, however, only the logics KD, KT4, and S4 are recognised.
\PL*/
	; Clause = (# modal_standard_theory(Theory, Sort)) ->
	   assert_theory(Theory,Sort),
	   fail

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
In case we don't recognise any of the above commands we just write the
entire line of the input file to the output file. After all, what do
{\em we} know?

\Xsample{{\tt \# this\_is\_a\_comment.} As it says \dots}

\PL*/
	; Clause = (# _) ->
	   writeclause(OutStream,Clause),
	   fail

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
If we have a decent modal formula, we start a series of procedures.
If a normal form is desired, we perform the predicate which was to be
specified in the option |'Tom:normal_form'|. The default for this is the
negation normal form. Then, the formula is translated into a list of clauses.
These clauses are transformed into a Prolog format and put in the Prolog
database, having the type |matrix|.
\PL*/
	; is_option('Tom:normal_form',NormalFormPredicate),
	  ( NormalFormPredicate = off ->
	      Clause = NormalFormula
	  ; Call_NF =.. [NormalFormPredicate, Clause, NormalFormula],
	      call(Call_NF)
	  ),
	  transform(NormalFormula, ClauseList),
	  ( member(Formula, ClauseList),
	    build_clause(Formula,FinalClause),
	    find_type(FinalClause,TypeOfClause),
	    assert(constraint(TypeOfClause,FinalClause,matrix)),
	    fail
	  ; true
	  ),
	  fail

	),
	!,
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Now, the clauses from the Prolog database have to be written to |OutStream|.
This is being carried out by the predicate |write_matrix/1|. The constraints
are treated in the predicate |write_extras/1|. The exact definition of these
two predicates is loaded according to the option |'Tom:method'|. Of course,
the Prolog database is cleared at the end.
\PL*/
	write_matrix(OutStream),
	retract_all(constraint((_ / _),_,matrix)),
	( is_option('Tom:method',constraints) ->
	    write_extras(OutStream)
	; write_extras(InputFileNameAtom)
	),
        retract_all(constraint((_ / _),_,_)).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate transform/2(+Formula, -ClauseList).

The binary predicate |transform| is a top level predicate for the following
predicate of the same name. A number of initial parameters is set.
This predicate has been introduced merely for legibility. Its result is
a list of clauses representing the translated formula. However, the result
is not complete in a sense that parts of the original formula cannot be found
in the clauses as they were put into the theory part.

\PL*/

transform(Formula, ClauseList):-
	transform(Formula,0,[],ClauseList).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\Predicate transform/4(+Formula,
	               ?PathTerm,
                       ?ClassicalVariables,
		       -ClauseList).


To transform the modal formula |Formula| into a first order formula, free of
modal operators.
Hereby, new function symbols will be introduced, using the term representing
the path information as an additional argument.

The |PathTerm| is built during the translation process. |ClassicalVariables|
is a list of the variables of the logic. This list is used for Skolemisation
of existentially quantified variables. These variables can be either
classical variables or variables introduced during translation.

The Skolem counter which is important for a unique naming of the
Skolem functions is a global variable, hence it does not occur in the
arguments of the predicate.

The translation is carried out accordingly to the algorithm presented
in the PhD thesis of F. Debart.

However, the difference to the proposed translation is, that the result of
the predicate is a list of clauses rather than a translated formula. This is
due to the fact that some parts of the translated formula would go into the
theory part. We store these parts in an internal database at the moment we
build them. Another issue is Skolemisation. It was possible to Skolemise
while translating because we keep track of the governing universally
quantified variables. Thus there is no need to produce a prenex normal form.

\PL*/

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{description}
\item[A variable] is easy to translate.
\end{description}
\PL*/
transform(Formula,_PathTerm,_ClassVars,[Formula]):-
	var(Formula),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{description}
\item[The classical connectives] split the translation into two branches,
      the results of which will be chucked together.

      By using the |intersection| predicate, we achieve that the lists of
      variables, |ClassVars1| and |ClassVars2|, contains only the variables
      which occur in the formula to be translated. This has been introduced
      to reduce the complexity of the Skolem functions to be introduced.
\end{description}
\PL*/
transform( and(Formula1,Formula2), PathTerm, ClassVars, Result):-
	!,
	term_variables(Formula1,Variables1),
	term_variables(Formula2,Variables2),
	intersection(Variables1,ClassVars,ClassVars1),
	intersection(Variables2,ClassVars,ClassVars2),
	transform(Formula1,PathTerm,ClassVars1,Intermediate1),
	transform(Formula2,PathTerm,ClassVars2,Intermediate2),
	append(Intermediate1,Intermediate2,Result).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This works similar to the above. It had to be extended by a call of a merging
predicate. This predicate can be specified in the option |'Tom:merging_predicate'|.
The default for this option is |merge_clauses|. Any predicate can be used
as long as it has an arity of three and the arguments are expected to be
clause lists.
\PL*/
transform( or(Formula1,Formula2), PathTerm, ClassVars,Result):-
	!,
	term_variables(Formula1,Variables1),
	term_variables(Formula2,Variables2),
	intersection(Variables1,ClassVars,ClassVars1),
	intersection(Variables2,ClassVars,ClassVars2),
	transform(Formula1,PathTerm,ClassVars1,Clauses1),
	transform(Formula2,PathTerm,ClassVars2,Clauses2),
	is_option('Tom:merging_predicate',MergePredicate),
	Call =.. [MergePredicate, Clauses1, Clauses2, Result],
	call(Call).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{description}
\item[The negation] Here it becomes obvious why a negation normal form is
desirable: the negation does not take into account any possibly occurring
quantifiers and modal operators.
\end{description}
\PL*/
transform(not(Formula),PathTerm,_ClassVars,[not(Result)]):-
	!,
	( var(Formula) ->
	    Result = Formula
	; Formula =.. [AnyPredicate | Arguments],
	  transform_arguments(Arguments, PathTerm, TransformedArguments),
	  ( name(AnyPredicate, [36,82 | _]) ->
	      Result =.. [AnyPredicate | TransformedArguments]
	  ; Result =.. [AnyPredicate, PathTerm | TransformedArguments]
	  )
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{description}
\item[The universal quantifier]
      It is explained in greater detail, because the following clauses
      tackle the problem very similar.

      First of all we find out whether the quantified variable actually
      occurs in the formula. If it doesn't, we simply proceed on the formula.

      If the variable does occur in the formula, we copy the formula to
      achieve a unique variable naming.
      Therefore, we collect all the variables, copy the whole formula,
      and unify nearly all the old and new variables, except for the quantified
      variable. Then, the list of classical variables is extended and the
      rest of the formula ist transformed. We used the newly introduced
      variables to avoid confusion about variable naming.
\end{description}
\PL*/
transform(forall(:(Var, Formula1)),PathTerm,ClassVars, Result):-
	!,
	term_variables(Formula1,VarList),
	( member(Var,VarList) ->
	    copy_term(Formula1 + VarList, NewFormula1 + CopyVarList),
	    unify_partially(VarList,CopyVarList,Var),
	    NewClassVars = [Var | ClassVars],
	    transform(NewFormula1,PathTerm,NewClassVars,Result)
	; transform(Formula1,PathTerm,ClassVars,Result)
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{description}
\item[The existential quantifier]
      works similar. We take into account that we introduce a new skolem
      function (named '|$kolemN|', where N stands for a unique number, taken
      from the global variable |skolemCounter|.

      Otherwise, the same term copying idea as for the universal quantifier
      applies.

      Additionally, the handling of the skolem counter is introduced.
      The value of the global variable |skolemCounter| is read, stored in a
      variable which in turn is used for generating the name of the Skolem
      function. Then, the value of the variable is incremented.
\end{description}
\PL*/
transform(exists(:(Var, Formula1)),PathTerm,ClassVars, Intermediate):-
	!,
	term_variables(Formula1,VarList),
	copy_term(Formula1 + VarList + Var,
	          NewFormula1 + CopyVarList + NewVar),
	unify_partially(VarList,CopyVarList,Var),
	getval(skolemCounter,Value),
	incval(skolemCounter),
	concat_atom(['$kolem',Value],NewFunctionSymbol),
	transform(NewFormula1,PathTerm,ClassVars,Intermediate),
	NewVar =.. [NewFunctionSymbol, PathTerm | ClassVars ].
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{description}
\item[The multi-modal box operator]
      The sort of the modality is indicated by an atom (well, it is
      supposed to anyway) followed by a colon and the formula the modal
      operator refers to.
      As this predicate is understood as being a premise of a conclusion
      (is it a constraint on a universally quantified variable), it has to
      be prepended to the clauses which result from the translation of the
      rest of the formula.
\end{description}
\PL*/
transform(box(:(Sort,Formula1)),PathTerm,ClassVars, Result):-
	!,
	NewPath = PathTerm + X,
	concat_atom(['$Rmod_',Sort],NewSymbol),
	AuxFunction =.. [NewSymbol, X],
	transform(Formula1,NewPath,ClassVars, Intermediate),
	process_premise(Intermediate,Result,AuxFunction).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{description}
\item[The mono-modal box operator]
      applies the same ideas as above. The argument |PathTerm| is extended by
      a newly generated variable, and the rest of the formula will be
      translated.
\end{description}
\PL*/
transform(box(Formula1),PathTerm,ClassVars, Intermediate):-
	!,
	NewPath =.. ['+', PathTerm, _ ],
	transform(Formula1,NewPath,ClassVars, Intermediate).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{description}
\item[The modal diamond operator]
       is basically the same, just mixes the principles of a modal operator
       together with Skolemisation. Suitably enough, the Skolem functions here
       are called |$kolemPath|, thus enabling to distinguish later, where they
       come from.

       As the predicate introduced in this clause is a constraint on an
       existentially quantified variable, it is a conjunct to the rest
       of the formula. As this conjunct forms a clause on its own and
       it consists of the auxiliary predicate only, it belongs to the
       theory part of the problem. Hence, this clause is stored in the
       Prolog database with the type |theory|. If the option |'Tom:method'|
       is set to |inference|, this clause is still part of the matrix.
\end{description}
\PL*/
transform(diamond(:(Sort, Formula1)),PathTerm,ClassVars, Intermediate):-
	!,
	getval(skolemCounter,Value),
	incval(skolemCounter),
	concat_atom(['$kolemPath',Value],NewPathFunctionSymbol),
	NewFunction =.. [NewPathFunctionSymbol, PathTerm | ClassVars],
	NewPath = PathTerm + NewFunction,
	concat_atom(['$Rmod_',Sort],NewSymbol),
	AuxFunction =.. [NewSymbol, NewFunction],
	( is_option('Tom:method',inference) ->
	    assert(constraint((NewSymbol / 1),AuxFunction,matrix))
	; assert(constraint((NewSymbol / 1),AuxFunction, theory))
	),
	transform(Formula1,NewPath,ClassVars, Intermediate).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{description}
\item[The mono-modal diamond operator] is a simplified case of the multi-modal
          operator. Here, simply the |PathTerm| is extended.
\end{description}
\PL*/
transform(diamond(Formula1),PathTerm,ClassVars, Intermediate):-
	!,
	getval(skolemCounter,Value),
	incval(skolemCounter),
	concat_atom(['$kolemPath',Value],NewPathFunctionSymbol),
	NewFunction =.. [NewPathFunctionSymbol , PathTerm | ClassVars],
	NewPath = PathTerm + NewFunction,
	transform(Formula1,NewPath,ClassVars, Intermediate).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\begin{description}
\item[An arbitrary function or predicate symbol (rigid and non-rigid)]
      If we don't find any of the known operators of the language, we
      have to treat the operator in a very boring way: we translate the
      arguments but not the operator symbol.
\end{description}
\PL*/
transform(Formula,PathTerm,_ClassVars, [Result]):-
	Formula =.. [AnyPredicate | ArgList],
	!,
	transform_arguments(ArgList,PathTerm,TransformedArgs),
	( name(AnyPredicate, [36,82 | _]) ->
	    Result =.. [AnyPredicate | TransformedArgs]
	; Result =.. [AnyPredicate , PathTerm | TransformedArgs]
	).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This predicate performs the translation on the arguments of a predicate or
function symbols. The implemented strategy is a recursion on the list of
arguments.
\PL*/
transform_arguments([],_PathTerm,[]).

transform_arguments([H|T],PathTerm,[NH|NT]):-
	transform_single_argument(H,PathTerm,NH),
	transform_arguments(T,PathTerm,NT).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This predicate translates a term of the language. If the term is a variable,
then the result of the translation yields the variable again. If it is a
constant or a function symbol, it has to be examined whether it is flexible
or rigid. In the former case, one argument will be added to the argument list.
\PL*/
transform_single_argument(X,_P,X):-
	var(X),
	!.

transform_single_argument(Term, PathTerm, TransformedTerm):-
	Term =.. [Function | Arguments],
	transform_arguments(Arguments, PathTerm, TransformedArguments),
	( name(Function, [36,82 | _]) ->
	    TransformedTerm =.. [Function | TransformedArguments]
	; TransformedTerm =.. [Function, PathTerm | TransformedArguments]
	). 
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate process_premise/3 (+InClauseList, -OutClauseList, +Premise).

This predicate is used to prepend the clauses from |InCLauseList| with
the |Premise|. If the clause is an implication already, we make the
premise more complex. Otherwise, the new clause becomes an implication.

\PL*/

process_premise([],[],_Constraint).

process_premise([H | T],[NH | NT],Constraint):-
	( H = implies(Premise, Conclusion) ->
	    NH = implies(and(Constraint, Premise), Conclusion)
	; NH = implies(Constraint, H)
	),
	process_premise(T, NT, Constraint).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate unify_partially/3(+VarList, -CopyVarList, +Variable).

This is an auxiliary predicate used to copy lists of veriables,
which may be necessary to avoid erroneous unifications of the systems.

We take the |VarList|. All variables which are identically to |Variable| are fine,
if they are not identical, we unify the corresponding heads of the lists.
This is to unify nearly every old variable with the new ones, {\em but not}
the newly introduced variable.

\PL*/

unify_partially([],[],_).

unify_partially([H|T],[H1|T1],Var):-
	(H == Var ->
	    true
	; H = H1
	),
	unify_partially(T,T1,Var).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate build_clause/2 (+Clause, -PrologClause).

This is a simple but powerful predicate is to transform a |Clause| from
our internal format into a |PrologClause|, a clause in a Prolog like
format.

\PL*/

build_clause(and(F1,F2),(BF1,BF2)):-
	!,
	build_clause(F1,BF1),
	build_clause(F2,BF2).

build_clause(or(F1,F2),(BF1;BF2)):-
	!,
	build_clause(F1,BF1),
	build_clause(F2,BF2).

build_clause(implies(F1,F2),(BF2 :- BF1)):-
	!,
	build_clause(F1,BF1),
	build_clause(F2,BF2).

build_clause(not(F1),-BF1):-
	!,
	build_clause(F1,BF1).

build_clause(F,F).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate find_type/2 (+Clause, -Type).

We attempt to find the main function name and the arity of the head
literal of the clause. This is important for a well ordered output of
all the clauses. We collect function name and arity in a pair called |Type|.
\PL*/

find_type(Variable, (variable / 0)):-
	var(Variable),
	!.

find_type((Head :- _Tail), Type):-
	!,
	find_type(Head, Type).

find_type((Literal1;_Literal2), Type):-
	!,
	find_type(Literal1, Type).

find_type( -(Literal), Type):-
	!,
	find_type(Literal, Type).

find_type(Literal, (Functor / Arity)):-
	functor(Literal,Functor,Arity).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following part contains predicates dealing with the theory not depending
on the particular problem, i.e.\ formula(e).

We provide the opportunity to use a standard axiom schema. As it is
known, there are modal axioms characterising a property of an
accessibility relation.  The name of the property (in teletype font)
can be used to specify this axiom schema.
%For some of the schemas, abbreviations are common.
The following table contains a survey of which
properties and which abbreviations are known to the system:

\makevertother
\begin{center}
\begin{tabular}{l|c|l}
property of the        & permitted & corresponding \\
accessibility relation & abbreviation & modal axiom schema \\
  \hline
{\tt transitive} & {\tt 4} & \( \Box \varphi \to \Box \Box \varphi\)\\
{\tt euclidean} & {\tt 5} & \( \diamond \varphi \to \Box \diamond \varphi\)\\
%{\tt symmetric} & {\tt b} & \( \varphi \to \Box \diamond \varphi\\
%{\tt serial} & {\tt d} & \( \Box \varphi \to \diamond \varphi\)\\
{\tt reflexive} & {\tt t} & \( \Box \varphi \to \varphi\)\\
%{\tt partly\_functional} & {\tt pf} & \( \diamond \varphi \to \Box \varphi\)\\
%{\tt functional} & {\tt f} & \( \diamond \varphi \to \Box \varphi\)\\
%{\tt weakly\_dense } & {\tt wd} & \( \Box \Box \varphi \to \Box \varphi\)\\
%{\tt weakly\_connected} & {\tt wc} & \( \Box((\varphi_2 \wedge \Box \varphi_2) \to \varphi_1) \vee \Box((\varphi_1 \wedge \Box \varphi_1) \to \varphi_2)\)\\
%{\tt connected} & {\tt c} & \( \Box (\Box \varphi_2 \to \varphi_1) \vee \Box(\Box \varphi_1 \to \varphi_2)\)\\
%{\tt weakly\_directed} & {\tt wi} & \( \diamond \Box \varphi \to \Box \diamond \varphi \) 
\end{tabular}
\end{center}
\makevertactive


Together with an axiom schema, the user has to specify which sort this schema
refers to (because we are dealing with multi-modal logics). Thus it is
possible to specify for each sort exactly the accessibility relation we want.

There are two more axiom schemas which can be specified: the {\tt interaction}
schema and the {\tt total} schema. The former states an inclusion relation
between two accessibility relations (hence it is of any interest in a multi-modal case only) and the latter schema states that the relation is total.

It is planned to include many more axioms for greater flexibility of this
module.
\PL*/

assert_axiom_schema(interaction,[H,T]):-
	concat_atom(['$Rmod_',H],Functor1),
	Rel1 =.. [Functor1, Var],
	concat_atom(['$Rmod_',T],Functor2),
	Rel2 =.. [Functor2, Var],
	assert(constraint((Functor2 / 1),(Rel2 :- Rel1),interaction)).

assert_axiom_schema(4,Sort):-
	concat_atom(['$Rmod_',Sort],Functor),
	Rel1 =.. [Functor, Var1],
	Rel2 =.. [Functor, Var2],
	Rel3 =.. [Functor, Var1 + Var2],
	assert(constraint((Functor / 1),(Rel3 :- (Rel1,Rel2)),transitive)).

assert_axiom_schema(transitive,Sort):-
	concat_atom(['$Rmod_',Sort],Functor),
	Rel1 =.. [Functor, Var1],
	Rel2 =.. [Functor, Var2],
	Rel3 =.. [Functor, Var1 + Var2],
	assert(constraint((Functor / 1),(Rel3 :- (Rel1,Rel2)),transitive)).

assert_axiom_schema(5,Sort):-
	concat_atom(['$Rmod_',Sort],Functor),
	Rel1 =.. [Functor1, Var1],
	Rel2 =.. [Functor1, Var2],
	Rel3 =.. [Functor1, Var1 + Var2],
	assert(constraint((Functor / 1),(Rel2 :- (Rel1, Rel3)),euclidean)).

assert_axiom_schema(euclidean,Sort):-
	concat_atom(['$Rmod_',Sort],Functor),
	Rel1 =.. [Functor1, Var1],
	Rel2 =.. [Functor1, Var2],
	Rel3 =.. [Functor1, Var1 + Var2],
	assert(constraint((Functor / 1),(Rel2 :- (Rel1, Rel3)),euclidean)).

%assert_axiom_schema(b,Sort):-
%	concat_atom(['$Rmod_',Sort],Functor),
%	...
%	assert(constraint((Functor / Arity),AxiomClause,Type)).

%assert_axiom_schema(symmetric,Sort):-
%	concat_atom(['$Rmod_',Sort],Functor),
%	...
%	assert(constraint((Functor / Arity),AxiomClause,Type)).

%assert_axiom_schema(d,Sort):-
%	concat_atom(['$Rmod_',Sort],Functor),
%	...
%	assert(constraint((Functor / Arity),AxiomClause,Type)).

%assert_axiom_schema(serial,Sort):-
%	concat_atom(['$Rmod_',Sort],Functor),
%	...
%	assert(constraint((Functor / Arity),AxiomClause,Type)).

assert_axiom_schema(t,Sort):-
	concat_atom(['$Rmod_',Sort],Functor),
	( is_option('Tom:method' ,inference) ->
	    Type = matrix
	; Type = reflexive
	),
	Relation =.. [Functor, 0],
	assert(constraint((Functor / 1),Relation,Type)).

assert_axiom_schema(reflexive,Sort):-
	concat_atom(['$Rmod_',Sort],Functor),
	( is_option('Tom:method' ,inference) ->
	    Type = matrix
	; Type = reflexive
	),
	Relation =.. [Functor, 0],
	assert(constraint((Functor / 1),Relation,Type)).

assert_axiom_schema(total,Sort):-
	concat_atom(['$Rmod_',Sort],Functor),
	( is_option('Tom:method', inference) ->
	    Type = matrix
	; Type = total
	),
	concat_atom(['$kolemPath_',Sort],Constant),
	Relation =.. [Functor, Constant],
	assert(constraint((Functor / 1),Relation,Type)).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
% T : box A imp A
%     reflexivity
%     forall w_1 : w_1 K w_1

% B : A imp box diamond A
%     symmetry
%     forall w_1, w_2 : w_1 K w_2  implies  w_2 K w_1

% 4 : box A imp box box A
%     transitivity
%     forall w_{1,2,3} : w_1 K w_2 and w_2 K w_3  imp  w_1 K w_3

% 5 : diamond A imp box diamond A
%     euclidean
%     forall w_{1,2,3} : w_1 K w_2 and w_1 K w_3  imp  w_2 K w_3

% PF: diamond A imp box A
%     partly functional
%     forall w_{1,2,3} : w_1 K w_2 and w_1 K w_3  imp  w_2 = w_3

% F : diamond A imp box A
%     functional
%     forall w_1 exists! w_2 : w_1 K w_2

% WD: box box A imp box A
%     weakly dense
%     forall w_{1,2} : w_1 K w_2  imp  (exists w_3 : w_1 K w_3  and  w_3 K w_2)

% WC: box((B and box B) imp A) or box((A and box A) imp B)
%     weakly connected
%     forall w_{1,2,3} : w_1 K w_2 and w_1 K w_3  imp  w_2 K w_3  or
%                                                      w_3 K w_2  or
%                                                      w_2 = w_3

% C : box (box B imp A) or box(box A imp B)
%     connected
%     forall w_{1,2,3} : w_1 K w_2 and w_1 K w_3  imp  w_2 K w_3  or  w_3 K w_2

% WI: diamond box A imp box diamond A
%     weakly directed
%     forall w_{1,2,3} : w_1 K w_2 and w_1 K w_3  imp
%                                        (exists w_4 : w_2 K w_4 and w_3 K w_4)
\PL*/

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate assert_theory/3 (+OutStream, +Theory, +Sort).

This is an output predicate providing the opportunity to specify one of the
classical modal theories |Theory| for the sort |Sort|. This theory will be
written to |OutStream|. The following theories are known at the moment:
%{\tt k, k5, k4, kb,
%kd, k45, kdb, kt, kd4, kd5, kb4, ktb, kt4} (also known as {\tt s4}), {\tt kd45,
%kt5} (also known as {\tt s5}). However, only for the theories
{\tt s4} ({\tt kt4}) and {\tt kd}. All the options, such as unification and
path normalisation, are set accordingly.

It is also possible to concatenate the letters of the axioms. The result is an
atom, for instance {\tt k4}, {\tt kb5}, etc. However, the user should bear in
mind that all the necessary options must bet set by himself then.

\PL*/

assert_theory(s4,Sort):-
	set_option('Tom:special_path_term' = normalize_path),
	set_option('Tom:special_unification' = normalize_unify_a1),
	set_option('ProCom::post_link' = ['normalize_unify_a1.pl']),
	assert_axiom_schema(t,Sort),
	assert_axiom_schema(4,Sort).

assert_theory(kt4,Sort):-
	set_option('Tom:special_path_term' = normalize_path),
	set_option('Tom:special_unification' = normalize_unify_a1),
	set_option('ProCom::post_link' = ['normalize_unify_a1.pl']),
	assert_axiom_schema(t,Sort),
	assert_axiom_schema(4,Sort).

assert_theory(s5,Sort):-
%	set_option('Tom:special_path_term' = normalize_path),
%	set_option('Tom:special_unification' = normalize_unify_a1),
%	set_option('ProCom::post_link' = ['normalize_unify_a1.pl']),
	assert_axiom_schema(t,Sort),
	assert_axiom_schema(5,Sort).

assert_theory(kt5,Sort):-
%	set_option('Tom:special_path_term' = normalize_path),
%	set_option('Tom:special_unification' = normalize_unify_a1),
%	set_option('ProCom::post_link' = ['normalize_unify_a1.pl']),
	assert_axiom_schema(t,Sort),
	assert_axiom_schema(5,Sort).

assert_theory(kd,_Sort):-
	set_option('Tom:special_path_term' = off),
	set_option('Tom:special_unification' = off).

%assert_theory(kt,Sort):-
%	set_option('Tom:special_path_term' = off),
%	set_option('Tom:special_unification' = unify_1),
%	set_option('ProCom::post_link' = ['unify_1.pl']),
%	assert_axiom_schema(t,Sort).

%assert_theory(kd4,Sort):-
%	set_option('Tom:special_path_term' = normalize_path),
%	set_option('Tom:special_unification' = normalize_unify_a),
%	set_option('ProCom::post_link' = ['normalize_unify_a.pl']),
%	assert_axiom_schema(4,Sort).

%assert_theory(kd5,Sort):-
%	set_option('Tom:special_path_term' = ????),
%	set_option('Tom:special_unification' = ????),
%	assert_axiom_schema(5,Sort).

%assert_theory(kd45,Sort):-
%	set_option('Tom:special_path_term' = ????),
%	set_option('Tom:special_unification' = ????),
%	assert_axiom_schema(4,Sort),
%	assert_axiom_schema(5,Sort)

assert_theory(Theory,Sort):-
	name(Theory, TheoryStringList),
	assert_theory_aux(TheoryStringList,Sort).

assert_theory_aux([],_Sort).

assert_theory_aux([H | T],Sort):-
	name(AxiomName,[H]),
	assert_axiom_schema(AxiomName, Sort),
	assert_theory_aux(T, Sort).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate set_default_options/0 (none).

This predicate provides the following \ProCom{} options:

\makevertother
\begin{center}
\begin{tabular}{l|l|l}
 & option name & currently recognised values \\ \hline
1. & {\tt 'Tom:method'} & {\tt constraints}, \underline{{\tt inference}} \\
2. & {\tt 'Tom:merging\_predicate'} & {\tt off}, \underline{{\tt
merge\_clauses}} \\
3. & {\tt 'Tom:normal\_form'} & {\tt off}, \underline{{\tt
negation\_normal\_form}} \\
4. & {\tt 'Tom:special\_path\_term'} & {\tt off}, \underline{{\tt
normalize\_path}} \\
5. & {\tt 'Tom:special\_unification'} & {\tt off}, \underline{{\tt
normalize\_unify\_a1}} \\
6. & {\tt 'Tom:log\_file'} & {\tt off}, \underline{{\tt on}} \\
(7.) & ({\tt 'Tom:theory\_optimization'}) & ({\tt off}, \underline{{\tt on}})
\end{tabular}
\end{center}
\makevertactive

The options are examined whether they are defined already. If they are not
defined, they will be defined and set to the default values. Default values are
the logic S4, i.e.\ a path normalization, a A1-unification, a negation normal
form, and the usual merging predicate. This refers to the underlined values in
the table.

The implementation uses a failure driven loop on the list |OptionList| that
contains pairs consisting of an option name and its default value.

In a second loop, the files providing the predicates are loaded.
\PL*/
set_default_options:-
	OptionList = [('Tom:method',inference),
	              ('Tom:merging_predicate',merge_clauses),
	              ('Tom:normal_form',negation_normal_form),
	              ('Tom:special_path_term',normalize_path),
	              ('Tom:special_unification',normalize_unify_a1)],
	( member((OptionName,DefaultValue),OptionList),
	     ( is_option(OptionName,_AnyValue)
	     ; define_option(OptionName = DefaultValue)
	     ),
	     fail
	 ; true
	 ),
	 ( member(Option,['Tom:merging_predicate',
	                  'Tom:normal_form']),
	   is_option(Option,Value),
	   ( Value = off ->
	       true
	   ; concat_atom([Value,'.pl'],FullFileName),
	     compile(FullFileName)
	   ),
	   fail
	 ; true
	 ),
	 ( is_option('Tom:method',constraints) ->
	     define_option('Tom:theory_optimization' = on)
	 ; true
	 ).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following predicate is for testing purposes only.
Its meaning should be evident.

\PL*/
tm_file_to_file(Infile,Outfile) :-
	open(Infile,read,Stream),
	( Outfile = '' ->
	    tom(Stream,output)
	;   open(Outfile,write,OutStream),
	    tom(Stream,OutStream),
	    close(OutStream)
	),
	close(Stream).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */



