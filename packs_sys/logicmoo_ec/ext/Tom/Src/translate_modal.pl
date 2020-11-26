%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: translate_modal.pl,v 1.3 1994/05/31 20:03:48 gerd Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module_interface(translate_modal). /*%--------------------------------------

Written by Zoltan Rigo.\bigskip

This module implements The translation of modal formulae into
first order formulae with terms representing the paths.
This has been done according to the translation proposed in

Francoise Clerin-Debart
Theories equationelles et de contraintes pour la demonstration
automatique en logique multi-modale
PhD Thesis
Caen University, France,
1992

\PL*/
:- export translate_modal/2.

:- begin_module(translate_modal).
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Do we need all of those?

\PL*/

:-      get_flag(library_path,OldPath),
	union(["/u/home/procom/System"],OldPath,FullPath),
	set_flag(library_path,FullPath).

:-	lib(lists),
        lib(options).


/*Pl%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We introduce ihe input language to the system.

The language should agree with the language of the problem
collection to be developed. Similarly, the precedences require some more work.

\PL*/

:- op(1100, xfy, 'implies').
:- op(1000, xfy, 'and').      %    Otter: op(780, , &).
:- op(1050, xfy, 'or').       %           op(790, , |).
:- op( 400,  fy, 'not').      %           op(500, , -).

:- op( 600,  fy, 'forall').   %           op(?, ?, all).
:- op( 600,  fy, 'exists').   %           op(?, ?, exists).

:- op( 600,  fy, 'box').      % 
:- op( 600,  fy, 'diamond').  % (precedence of : is 600)


/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Firstly, we define an output option. This is not yet set into power (25/8/94)
but this is supposed to become a kind of default.
Then we set a global variable for the identification of the Skolem functions.

\PL*/

:- define_option 'translate_modal:output_file' = '...translated_modals.ppp'.

:-setval(skolemCounter,1).


/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate translate_modal/2(+InStream, +OutStream).

This predicate conforms to the \ProCom{} filter specification. In a
failure-driven loop clauses are read form the stream |InStream|.
After transforming the clause into its negated normalform, the modal formula
will be translated.



\PL*/

translate_modal(Stream,OutStream) :-
        is_option('translate_modal:output_file',File),
	open(File,write,CSTStream),
        repeat,
	read(Stream,Clause),
	( Clause = end_of_file ->
	    true

%%%	; Clause = (# _) ->
%%%	   writeclause(OutStream,Clause),
%%%	   fail

	; make_nnf(Clause,0,NormalClause),
	  transform(NormalClause,TranslatedClause),
          normalize_path(TranslatedClause,NormalizedClause),
	  printf(OutStream,"%vQDMw.\n",[NormalizedClause]),
          fail
	),
	close(CSTStream),
	( current_module('modal logic') ->
	    true
	;   create_module('modal logic')
        ),
	call(compile(File),'modal logic'),
	!.
/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate transform/2(+Formula, -TranslatedFormula),

The binary predicate |transform| is a top level predicate for the following
predicate of the same name. A number of initial parameters is set.
This predicate has been introduced merely for legibility.

\PL*/


transform(Formula, TranslatedFormula):-
	transform(0,Formula,[],[],TranslatedFormula).


/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
\Predicate transform/5(?PathTerm,
                       +Formula,
                       ?ClassicalVariables
                       ?PathVariables,
                       -TranslatedFormula),


To transform the |Formula|, which is assumed to be a modal formula in negation
normal form, into a 1st order formula, free of modal operators.
Hereby, new function symbols will be introduced, using the term representing
the path information as an additional argument.

The |PathTerm| is built during the translation process. |ClassicalVariables|
is a list of the variables of the logic, |PathVariables| is a list of the
path variables. 
The skolem counter is a global variable, hence it does not occur in the
arguments of the predicate.

The translation is carried out according to the algorithm presented in the
thesis.

\PL*/

transform(PathTerm,Formula,ClassVars,PathVars,Result):-
    (

/*PL  A variable is really easy to translate... \PL*/

      var(Formula) ->
        Result = Formula


/*PL The classical connectives split the translation into two branches, the
 results of which will be chucked together. \PL*/

     ; Formula =.. [implies, Formula1, Formula2] ->
	make_nnf(not Formula1, 0, NewFormula),
	transform(PathTerm,NewFormula,ClassVars,PathVars,Intermediate1),
	transform(PathTerm,Formula2,ClassVars,PathVars,Intermediate2),
	Result =.. ['or', Intermediate1, Intermediate2]

     ; Formula =.. [and, Formula1, Formula2] ->
	transform(PathTerm,Formula1,ClassVars,PathVars,Intermediate1),
        transform(PathTerm,Formula2,ClassVars,PathVars,Intermediate2),
        Result =.. ['and', Intermediate1, Intermediate2]

     ; Formula =.. [or, Formula1, Formula2] ->
        transform(PathTerm,Formula1,ClassVars,PathVars,Intermediate1),
        transform(PathTerm,Formula2,ClassVars,PathVars,Intermediate2),
        Result =.. ['or', Intermediate1, Intermediate2]

/*PL Here it becomes obvious why a negation normalform is required:
 The negation does not take into account possibly occuring
 quantifiers and modal operators. \PL*/

     ; Formula =.. [not, Formula] ->
        transform(PathTerm,Formula,ClassVars,PathVars,Intermediate),
        Result =.. ['not', Intermediate]


/*PL The universal quantifier.\\

 It is explained in greater detail, because the following clauses
 tackle the problem very similar.

 At first we copy the formula to achieve a unique variable naming.
 Therefore, we collect all the variables, copy the whole formula,
 and unify nearly all the old and new variables, except for the quantified
 variable.\PL*/

     ; Formula =.. [forall, :(Var, Formula1)] ->
        term_variables(Formula1,VarList),
        copy_term(Formula1 + VarList, NewFormula1 + CopyVarList),
        drei(VarList,CopyVarList,Var),

/*PL We extend the list of the classical variables...\PL*/

        NewClassVars = [Var | ClassVars],

/*PL And transform the formula...\\
 In the formula, we use the new variables (variable names) to avoid 
 confusion about variable naming.\PL*/

        transform(PathTerm,NewFormula1,NewClassVars,PathVars,Intermediate),
        Result = Intermediate

/*PL The existential quantifier\\

 ... works similar. We take into account that we introduce a new skolem
 function (named '$kolemN', where N stands for a unique number, taken from
 the global variable skolemCounter.

 Otherwise, the same term copying idea as for the universal quantifier
 applies.

 Additionally, the handling of the skolem counter is introduced.
 The value of the global variables skolemCounter is read, stored in a
 variable which in turn is used for generating the name of the skolem
 function. Then, the value of the variable is incremented.\PL*/

     ; Formula =.. [exists, :(Var, Formula1)] ->
        term_variables(Formula1,VarList),
        copy_term(Formula1 + VarList + Var,NewFormula1 + CopyVarList + NewVar),
        drei(VarList,CopyVarList,Var),
	getval(skolemCounter,Value),
	incval(skolemCounter),
        concat_atom(['$kolem',Value],NewFunctionSymbol),
        append([PathTerm], ClassVars, Arguments),
	transform(PathTerm,NewFormula1,ClassVars,PathVars,Intermediate),
        NewVar =.. [NewFunctionSymbol | Arguments ],
        Result = Intermediate

/*PL The modal box operator\\

... applies the same ideas as above. The term PathTerm is extended by
 a newly generated variable, and the rest of the formula will be translated.
\PL*/

     ; Formula =.. [box, Formula1] ->
        NewPath =.. ['+', PathTerm, X ],
        NewPathVars = [ X | PathVars ],
        transform(NewPath,Formula1,ClassVars,NewPathVars,Intermediate),
        Result = Intermediate

/*PL The modal diamond operator\\
 ... is basically the same, just mixes the principles of a modal operator
 together with skolemization. Suitably enough, the skolem functions here
 are called $kolemPath, thus enabling to distinguish later, where they come
 from.\PL*/

     ; Formula =.. [diamond, Formula1] ->
	getval(skolemCounter,Value),
	incval(skolemCounter),
        concat_atom(['$kolemPath',Value],NewPathFunctionSymbol),
        append([PathTerm], ClassVars, Arguments),
        NewFunction =.. [NewPathFunctionSymbol | Arguments],
        NewPath =.. ['+', PathTerm, NewFunction ],
        transform(NewPath,Formula1,ClassVars,PathVars,Intermediate),
        Result = Intermediate

/*PL If we don't find any of the known operators of the language, we
 have to treat the operator in a very boring way: we translate the
 arguments but not the operator symbol.\PL*/

     ; Formula =.. [OtherFunctor | ArgList] ->
        transform_a_list(PathTerm,ArgList,ClassVars,PathVars,
                      IntermediateArgList),
        Result =.. [OtherFunctor , PathTerm | IntermediateArgList]


     ).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate transform_a_list/5(?PathTerm,
		              +ArgumentList,
                              ?ClassicalVariables
                              ?PathVariables,
                              -TranslatedArgumentList)

It acts as the predicate |transform| but works on a list.

This should be applied to transform the list of arguments of any functor
not known to the system.

The sixth argument from above (|SortInformation|) is omitted as no new
sort information is expected to come up at this level.

\PL*/

transform_a_list(_,[],_,_,[]).

transform_a_list(PathTerm,[H|T],ClassVars,PathVars,Result):-
    transform(PathTerm,H,ClassVars,PathVars,I1),
    transform_a_list(PathTerm,T,ClassVars,PathVars,I2),
    Result = [I1 | I2].

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate make_nnf/3(+Formula, ?Polarity, -NormalizedFormula

As the translation assumes a formula in its negation normalform,
we have to transform the formula before translating it. This has been
done in a rather conventional way, analysing the formula structure and
changing the operators to their duals, according to the polarity.

\PL*/

make_nnf(Formula,Polarity,NormalFormula):-
	( var(Formula) ->
	    ( Polarity = 0 ->
	         NormalFormula = Formula
	    ; Polarity = 1 ->
	         NormalFormula =.. [not, Formula])

	; Formula =.. [implies, Formula1, Formula2] ->
	     make_nnf(not Formula1, Polarity, NormalFormula1),
	     make_nnf(Formula2, Polarity, NormalFormula2),
	     NormalFormula =.. [or, NormalFormula1, NormalFormula2]

	; Formula =.. [and, Formula1, Formula2] ->
	     make_nnf(Formula1, Polarity, NormalFormula1),
	     make_nnf(Formula2, Polarity, NormalFormula2),
	     NormalFormula =.. [and, NormalFormula1, NormalFormula2]

	; Formula =.. [or, Formula1, Formula2] ->
	     make_nnf(Formula1, Polarity, NormalFormula1),
	     make_nnf(Formula2, Polarity, NormalFormula2),
	     NormalFormula =.. [or, NormalFormula1, NormalFormula2]

	; Formula =.. [not, Formula1] ->
	     ( Formula1 =.. [not, NewFormula] ->
		 make_nnf(NewFormula, Polarity, NormalFormula)
	     ; NewPolarity is (Polarity + 1) mod 2,
	       make_nnf(Formula1, NewPolarity, NormalFormula))

	; Formula =.. [forall, :(Var, Formula1)] ->
	     ( Polarity = 0 ->
		 make_nnf(Formula1, Polarity, NewFormula1),
		 NormalFormula =.. [forall, :(Var, NewFormula1)]
	     ; Polarity = 1 ->
	          make_nnf(Formula1, Polarity, NewFormula1),
		  NormalFormula =.. [exists, :(Var, NewFormula1)])

	; Formula =.. [exists, :(Var, Formula1)] ->
	     ( Polarity = 0 ->
		 make_nnf(Formula1, Polarity, NewFormula1),
		 NormalFormula =.. [exists, :(Var, NewFormula1)]
	     ; Polarity = 1 ->
	          make_nnf(Formula1, Polarity, NewFormula1),
		  NormalFormula =.. [forall, :(Var, NewFormula1)])

	; Formula =.. [box, Formula1] ->
	     ( Polarity = 0 ->
		 make_nnf(Formula1, Polarity, NewFormula1),
		 NormalFormula =.. [box, NewFormula1]
	     ; Polarity = 1 ->
	          make_nnf(Formula1, Polarity, NewFormula1),
		  NormalFormula =.. [diamond, NewFormula1])

	; Formula =.. [diamond, Formula1] ->
	     ( Polarity = 0 ->
		 make_nnf(Formula1, Polarity, NewFormula1),
		 NormalFormula =.. [diamond, NewFormula1]
	     ; Polarity = 1 ->
	          make_nnf(Formula1, Polarity, NewFormula1),
		  NormalFormula =.. [box, NewFormula1])

	; Formula = _Anything_else ->
	     ( Polarity = 0 ->
		 NormalFormula = Formula
	     ; Polarity = 1 ->
	         NormalFormula =.. [not, Formula])
	 ).
	  

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate normalize_path/2(+Formula, -NormalizedFormula).

This predicate normalises the path terms in the formula according to the
new convention introduced in Caen. (We co-operate, so this might explain
the mess.)

This code was written by Gilbert Boyreau.

\PL*/
normalize_path(Var,Var2) :-
	var(Var),
	!,
	Var = Var2.
normalize_path(Atom,Atom2) :-
	atomic(Atom),
	!,
	Atom = Atom2.
normalize_path(+(A1,Arg),
	      NormalArg) :-
	A1 == 0,
	!,
	normalize_path(Arg,NormalArg).
normalize_path(+(Arg,A2),
	      NormalArg) :-
	A2 == 0,
	!,
	normalize_path(Arg,NormalArg).
normalize_path(+(A1,C),
	      Normal) :-
	nonvar(A1),
	A1 = +(A,B),
	!,
	normalize_path(+(A,+(B,C)),
			Normal).
normalize_path(+(A,B),
	      +(NormalA,NormalB)) :-
	!,
	normalize_path(A,NormalA),
	normalize_path(B,NormalB).

normalize_path(Term,NormalTerm) :-
	Term =.. [F|Args],
	normalize_path_list(Args,NormalArgs),
	NormalTerm =.. [F|NormalArgs].

normalize_path_list([],[]).
normalize_path_list([H|T],[NormalH|NormalT]) :-
	!,
	normalize_path(H,NormalH),
	normalize_path_list(T,NormalT).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\Predicate drei/3(+VarList, -CopyVarList, +Variable).

This is an auxiliary predicate used to copy lists of veriables,
which may be  necessary to avoid erroneous unifications of the systems.

We take the |VarList|. All variables identical to |Variable| are fine,
if they are not identical, we unify the corresponding heads of the lists.
This is to unify nearly every old variable with the new ones, but not the
newly introduced variable.

\PL*/

drei([],[],_).

drei([H|T],[H1|T1],Var):-
	(H == Var ->
	    true
	; H = H1
        ),
	drei(T,T1,Var).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following predicate is for testing purposes only.

Its meaning should be evident.

\PL*/

tm_file_to_file(Infile,Outfile) :-
	open(Infile,read,Stream),
	( Outfile = '' ->
	    translate_modal(Stream,output)
	;   open(Outfile,write,OutStream),
	    translate_modal(Stream,OutStream),
	    close(OutStream)
        ),
	close(Stream).

/*PL%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

\EndProlog */













