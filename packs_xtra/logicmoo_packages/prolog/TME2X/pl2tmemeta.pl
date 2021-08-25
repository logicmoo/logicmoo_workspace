
%%% Clausal Normalform Conversion.
%%% Version: 1.0
%%% Changes: Initial Version
%%% Date:    1/23/98
%%% Author:  Peter Baumgartner (peter@informatik.uni-koblenz.de)
%%% Description:
%%% Rather straightforward algorithm, but tries Antiprenexing in order to avoid
%%% SKolemfunctions with too many arguments.
%%%
%%% Language for Formulae:
%%% all(Vars,Formula) | ex(Vars,Formula)
%%% there Vars is a list of Prolog Variables, possibly attributed by Metaterms.
%%% Every Variable in a Metaterm must be bound by a governing quantifier.
%%% Formula consists of usual Prolog literals and is made up of the operators
%%% 
%%% ',' (conjunction)  ';'  '->' (implication) '<->' (equivalence), '-' (negation)
%%%
%%% Usual Prolog operator declarations apply. <-> is declared as op(1050,xfy, '<->').
%%%
%%% Variables can be decorated by Prolog metaterms.
%%% Main entry point: clausify/5.

:- module(thman).
% Der EInfachheit halber 

%%%%%% metaterm handlers  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- meta_attribute(eclipse, [unify:unify_sort/2, print:print_sort/2,
%	copy_term:copy_handler_sort/2]).

:- meta_attribute(thman, [unify:unify_sort/2, % print:print_sort/2,
	copy_term:copy_handler_sort/2]).
:- meta_attribute(suspend, [unify:unify_suspend/2]).

copy_handler_sort(Var{Attr}, V) :-
	-?->
	free(V),
	V = Var,
	add_attribute(V,Attr), !.

copy_handler_sort(Var{Attr}, V{A}) :-
	-?->
	meta(V),
	V = Var,
	A = Attr, !.

%% This clause unifies the attributes in case two attributed variables have to be unified
unify_suspend(BoundToVar{BoundToAttr}, Attr) :-
	-?-> !,
%	writeln('unify_suspend_var '), %write(Term), write(' '), writeln(Attr),
	Attr = BoundToAttr.
unify_suspend(_Term, _Attr).

%unify_suspend(_Term, _Attr) :-
%	write(hi).

%% This clause unifies the attributes in case two attributed variables have to be unified
unify_sort(BoundToVar{BoundToAttr}, Attr) :-
	-?-> !,
%	writeln('unify_sort_var '), %write(Term), write(' '), writeln(Attr),
	Attr = BoundToAttr.
unify_sort(_Term, _Attr).

%unify_sort(Term, Attr) :-
%	write('unify_sort '), write(Term), write(' '), writeln(Attr).

% currently there is not much to be done.
%copy_handler_sort(V{Attr}, VC) :-
%	-?->
%	AttrOut = Attr.

%print_sort(_V{Attr}, AttrOut) :-
%	-?->
%	AttrOut = Attr.

%% Ingo's ops:
%% all(Var,_) und ex(Var,_), junktoren  ,;->,<->,contradiction.

:- op(1050,xfy, '<->').
%:- op(1050,xfy, '<-').
%:- op(1050,xfy, '<=>').
%:- op(1050,xfy, '<=').
%:- op(1050,xfy, '=>').
%:- op(1000,xfy, '&').
%:- op(1100,xfy, 'v').


test(Clauses,SkolemDecls) :- 
	clausify((all([Y{set},Z{set}], ((Y=Z) <-> 
	  all(U{el},(in(U,Y) <-> in(U,Z)))))),Clauses,SkolemDecls).
	  

:- set_flag(occur_check, on).
:- set_flag(output_mode, "UPVm").


%%%%%%%%% Conversion workhorse
%% 
%% clausify(F,Clauses,SkolemDecls,SkolemCtr,NewSkolemCtr).
%% 
%% F                 is the predicate logic formula to be converted
%% Clauses           is the resulting list of clauses, each clause is a tuple [Pos,Neg]
%%                   where Pos (Neg) is a prolog list of the positive (negative) lits
%%                   of the clause; literals in Neg are explicitly negated by '-'
%% SkolemDecls       is the list of declarations for the introduced skolem functions
%% SkolemCtr         input parameter, a number, used to name skolem functions
%%                   in the format fsk<SkolemCtr>, fsk<SkolemCtr+1>,...,fsk<NewSkolemCtr-1>
%% NewSkolemCtr      output parameter, gives first unused index.


clausify(F,Clauses,SkolemDecls,SkolemCtr,NewSkolemCtr) :-
	setval(skctr, SkolemCtr),
	clausify(F,Clauses,SkolemDecls),
	getval(skctr, NewSkolemCtr).

clausify(F,Clauses,SkolemDecls) :-
	nnf(F,Fnnf),
	matrix(Fnnf,Matrix),
	cnf(Matrix,Mcnf),
	norm_con(Mcnf,NMcnf), 
	set_matrix(Fnnf,NMcnf,FNMcnf),
	skolemize(FNMcnf,NMcnf,[],S,SkolemDecls), 
	pl_list_con(S,SList),
	split_clauses_dis(SList,Clauses).

matrix(ex(_X,F),R) :-
	matrix(F,R).
matrix(all(_X,F),R) :-
	matrix(F,R).
matrix(R,R).

set_matrix(ex(X,F),NewMatrix,ex(X,R)) :-
	set_matrix(F,NewMatrix,R).
set_matrix(all(X,F),NewMatrix,all(X,R)) :-
	set_matrix(F,NewMatrix,R).
set_matrix(_F,NewMatrix,NewMatrix).

%%% nnf -- put into negation normal form, also normalizes quantified variables as lists
nnf(F,FT) :-
	prennf(F,FP),
	prenex(FP,FT).

prennf(- (- A), NA) :- prennf(A,NA).

prennf(- (A <-> B),  R) :-
	prennf(((-A ; -B) , (A ; B)),R).
%prennf(- (A <=> B),  R) :-
%	prennf(((-A ; -B) , (A ; B)),R).

prennf((A <-> B),  R) :- 
	prennf(((A->B) , (B->A)),R).
%prennf((A <=> B),  R) :- 
%	prennf(((A->B) , (B->A)),R).

prennf(- (A -> B), R) :-
	prennf(- (- A ; B), R).
%prennf(- (A => B), R) :-
%	prennf(- (- A ; B), R).

%prennf(- (B <- A), R) :- 
%	prennf(- (B ; - A), R).
%prennf(- (B <= A), R) :- 
%	prennf(- (B ; - A), R).

prennf((A -> B), R) :-
	prennf((- A ; B), R).
%prennf((A => B), R) :-
%	prennf((- A ; B), R).

%prennf((B <- A), R) :-
%	prennf((B ; - A), R).
%prennf((B <= A), R) :-
%	prennf((B ; - A), R).


prennf(- (A , B), R) :-
	prennf((- A ; - B), R).
%prennf(- (A & B), R) :-
%	prennf((- A ; - B), R).

prennf((A , B), (NA , NB)) :-
	prennf(A, NA),
	prennf(B, NB).
%prennf((A & B), (NA , NB)) :-
%	prennf(A, NA),
%	prennf(B, NB).

prennf(- (A ; B), (NNA , NNB)) :-
	prennf(- A, NNA),
	prennf(- B, NNB).
%prennf(- (A v B), (NNA , NNB)) :-
%	prennf(- A, NNA),
%	prennf(- B, NNB).

prennf((A ; B),  (NA ; NB)) :-
	prennf(A , NA),
	prennf(B , NB).
%prennf((A v B),  (NA ; NB)) :-
%	prennf(A , NA),
%	prennf(B , NB).



prennf(- ex(X,A), all(XVec,NNA)) :-
	make_vector(X,XVec),
	prennf(-A,NNA).
prennf( ex(X,A), ex(XVec,NA)) :-
	make_vector(X,XVec),
	prennf(A,NA).

prennf(- all(X,A), ex(XVec,NNA)) :-
	make_vector(X,XVec),
	prennf(-A,NNA).
prennf( all(X,A), all(XVec,NA)) :-
	make_vector(X,XVec),
	prennf(A,NA).


prennf(- A, - A).

prennf(B, B).


%% works on pre-nnf, assumes variables as vectors
prenex(all(X,F) , all(X,Res)) :-
	prenex(F, Res).

prenex(ex(X,F) , ex(X,Res)) :-
	prenex(F, Res).


prenex((F , G) , R) :-
	prenex(F, FP), 
	prenex(G, GP),
	prenexq((FP, GP), R).

%prenex((F & G) , R) :-
%	prenex(F, FP), 
%	prenex(G, GP),
%	prenexq((FP, GP), R).
	
prenex((F ; G) , R) :-
	prenex(F, FP), 
	prenex(G, GP),
	prenexq((FP ; GP),R).
%prenex((F v G) , R) :-
%	prenex(F, FP), 
%	prenex(G, GP),
%	prenexq((FP ; GP),R).

prenex(F , F ).


prenexq((all(X,F), G) , all(X1,Res)) :-
	new_quantification(all(X,F),all(X1,F1)),
	prenexq((F1,G), Res).

prenexq((G , all(X,F)) , all(X1,Res)) :-
	new_quantification(all(X,F),all(X1,F1)),
	prenexq((G,F1), Res).
%prenexq((G & all(X,F)) , all(X1,Res)) :-
%	new_quantification(all(X,F),all(X1,F1)),
%	prenexq((G,F1), Res).

prenexq((all(X,F); G) , all(X1,Res)) :-
	new_quantification(all(X,F),all(X1,F1)),
	prenexq((F1;G), Res).
%prenexq((all(X,F) v G) , all(X1,Res)) :-
%	new_quantification(all(X,F),all(X1,F1)),
%	prenexq((F1;G), Res).
prenexq((G ; all(X,F)) , all(X1,Res)) :-
	new_quantification(all(X,F),all(X1,F1)),
	prenexq((G;F1), Res).
%prenexq((G v all(X,F)) , all(X1,Res)) :-
%	new_quantification(all(X,F),all(X1,F1)),
%	prenexq((G;F1), Res).

prenexq((ex(X,F), G) , ex(X1,Res)) :-
	new_quantification(ex(X,F),ex(X1,F1)),
	prenexq((F1,G), Res).
prenexq((G , ex(X,F)) , ex(X1,Res)) :-
	new_quantification(ex(X,F),ex(X1,F1)),
	prenexq((G,F1), Res).
%prenexq((G & ex(X,F)) , ex(X1,Res)) :-
%	new_quantification(ex(X,F),ex(X1,F1)),
%	prenexq((G,F1), Res).

prenexq((ex(X,F); G) , ex(X1,Res)) :-
	new_quantification(ex(X,F),ex(X1,F1)),
	prenexq((F1;G), Res).
%prenexq((ex(X,F) v G) , ex(X1,Res)) :-
%	new_quantification(ex(X,F),ex(X1,F1)),
%	prenexq((F1;G), Res).
prenexq((G ; ex(X,F)) , ex(X1,Res)) :-
	new_quantification(ex(X,F),ex(X1,F1)),
	prenexq((G;F1), Res).
%prenexq((G v ex(X,F)) , ex(X1,Res)) :-
%	new_quantification(ex(X,F),ex(X1,F1)),
%	prenexq((G;F1), Res).

%prenexq((F v G) , (F ; G)).
prenexq((F ; G) , (F ; G)).
prenexq((F , G) , (F , G)).
%prenexq((F & G) , (F , G)).


make_vector(A,[A]) :- var(A).
make_vector([],[]) :- !.
make_vector([A|B],[A|B]) :- !.

cnf(- (A <-> B),  R) :-
	cnf(- ((A->B) , (B->A)),R).
%cnf(- (A <=> B),  R) :-
%	cnf(- ((A->B) , (B->A)),R).

cnf((A <-> B),  R) :- 
	cnf(((A->B) , (B->A)),R).
%cnf((A <=> B),  R) :- 
%	cnf(((A->B) , (B->A)),R).

cnf(- (A -> B), R) :-
	cnf(- (- A ; B), R).
%cnf(- (A => B), R) :-
%	cnf(- (- A ; B), R).

%cnf(- (B <- A), R) :- 
%	cnf(- (B ; - A), R).
%cnf(- (B <= A), R) :- 
%	cnf(- (B ; - A), R).

cnf((A -> B), R) :-
	cnf((- A ; B), R).
%cnf((A => B), R) :-
%	cnf((- A ; B), R).

%cnf((B <- A), R) :-
%	cnf((B ; - A), R).
%cnf((B <= A), R) :-
%	cnf((B ; - A), R).

cnf(- (A , B), R) :-
	cnf((- A ;  - B), R).
%cnf(- (A & B), R) :-
%	cnf((- A ;  - B), R).

cnf((A , B), (NA , NB)) :-
	cnf(A, NA),
	cnf(B, NB).
%cnf((A & B), (NA , NB)) :-
%	cnf(A, NA),
%	cnf(B, NB).

cnf(- (A ; B), (NNA , NNB)) :-
	cnf(- A, NNA),
	cnf(- B, NNB).
%cnf(- (A v B), (NNA , NNB)) :-
%	cnf(- A, NNA),
%	cnf(- B, NNB).

cnf((A ; B),  R) :-
	cnf(A , NA),
	cnf(B , NB),
	cnf1((NA ; NB),  R).
%cnf((A v B),  R) :-
%	cnf(A , NA),
%	cnf(B , NB),
%	cnf1((NA ; NB),  R).


cnf(- (- A),  NA) :- cnf(A,NA).

cnf(- A,  - A).

cnf(B,  B).

cnf1( ((AF, AR) ; B) ,  R) :-
	cnf(((AF ; B) , (AR ; B)), R).
%cnf1( ((AF & AR) ; B) ,  R) :-
%	cnf(((AF ; B) , (AR ; B)), R).

cnf1( (A ; (BF, BR)) , R) :-
	cnf(((A ; BF) , (A ; BR)),R).
%cnf1( (A ; (BF & BR)) , R) :-
%	cnf(((A ; BF) , (A ; BR)),R).
cnf1( (A ; B) ,  (A ; B)).

%% works only for CNF:
norm_con(((A , B) , C), (FN , R)) :-
	norm_con((A, B), (F , G)),
	norm_dis(F, FN),
	norm_con((G, C), R).
norm_con((A , B), (NA , NB)) :-
	norm_dis(A, NA),
	norm_con(B, NB).
%norm_con(A,NA) :-
%	norm_dis(A, NA).
norm_con(A, NA) :- norm_dis(A,NA).

norm_dis(((A ; B) ; C), (F ; R)) :-
	norm_dis((A; B), (F ; G)),
	norm_dis((G; C), R).
norm_dis((A ; B), (A ; NB)) :-
	norm_dis(B, NB).
%norm_dis(A,NA) :-
%	norm_dis(A, NA).
norm_dis(A, A).

%%%%%%%%%% Variable utilities

:- setval(varctr,0).
:- setval(skctr,0).

%%% 2nd result is suitable for subst_vector:
newvar_vector([],[],[]).
newvar_vector([X|R],[XN|RN],[X/XN|RNPairs]) :-
	newvar(X,XN), 
	newvar_vector(R,RN,RNPairs).


%newvar(NewVar) :-
%	getval(varctr,S),
%	integer_atom(S,SA),
%	concat_atoms('X',SA,NewVar),
%	incval(varctr).

%is_lowercase(Atom) :-
%	name(Atom,[Code|_]),
%	97 =< Code, %% 'a'
%	Code =< 122. %% 'z'
%
%is_uppercase(Atom) :-
%	name(Atom,[Code|_]),
%	65 =< Code, %% 'A'
%	Code =< 90. %% 'Z'
%
%to_uppercase(Atom,UppercaseAtom) :-
%	name(Atom,[Code|Rest]),
%	UCode is Code-32,
%	name(UppercaseAtom,[UCode|Rest]).

% Peter 15/1/98
% newvar(AttrVar,NewVar)
% AttrVar ist eine attributierte Variable AttrVar = Var{Attr},
% NewVar ist eine neue Variable, aber nur bezueglich Var, Attr ist von AttrVar uebernommen. 
% Also NewVar = Var1{Attr}
% Es ware _falsch_ es anders zu machen, z.b. bei 
% all(X,all(Y{p(X)},q(X,Y))) wird bei Klausifizierung zunaechst
% all(Y{p(X1)},q(X1,Y)) wegen Ersetzung X / X1, dann
% q(X1,Y1{p(X1)}) wegen Ersetzung Y{p(X1)} / Y1{p(X1)}.
% 
newvar(Base{Attr},NewVar) :-
	-?-> !,
% WARNUNG: standard_vars Aufruf fuehrt sogar zu einem vertrackten Fehler
% Das Antipraenexing funktioniert nicht mehr; anscheinend geht in newvar_vector
% was schief. Sehr schmutzig das alles.
%	standard_vars(Base,NV),
	add_attribute(NV,Attr),
	NewVar = NV.

newvar(Base,NewVar) :-
	standard_vars(Base,NewVar).

% Turns a list of Variables into a list of Prolog Atoms of their printnames
% appended by some number
gen_standard_vars([],[]).
gen_standard_vars([V|R],[VRes|RRes]) :-
	get_var_info(V,name,VName),
	concat_atoms(VName,'_',VName1),
	getval(varctr,N),
	incval(varctr),
	integer_atom(N,NA),
	concat_atoms(VName1,NA,VRes),
	gen_standard_vars(R,RRes).


standard_vars(T,TT) :- term_variables(T,V),
% This does not work as expected :-(
% Seems that we do not get the printnames  set  if T is a variable.
	copy_term((T,V),(TT,VV)),
%	retract_all(last_var_name(_,_)),
	gen_standard_vars(V,VVV),
	open("",string,S),
	write(S,VVV),
	seek(S,0),
	read(S,VV),
	close(S),
	!.


%newvar(Base,NewVar) :-
%	( var(Base) ->
%	    %% use Base itself
%	    get_var_info(Base,name,BaseName)
%	  ; is_lowercase(Base) ->
%	    to_uppercase(Base,BaseName)
%	  ; is_uppercase(Base) ->
%	    Base = BaseName
%	  ; concat_atoms('X', Base,BaseName)
%        ),
%	concat_atoms(BaseName,'_',BaseName1),
%	getval(varctr,S),
%	integer_atom(S,SA),
%	concat_atoms(BaseName1,SA,NewVar),
%	incval(varctr).
%
%newvar(Base,NewVar) :-
%	getval(varctr,S),
%	integer_atom(S,SA),
%	concat_atoms('X',Base,H),
%	concat_atoms(H,SA,NewVar),
%	incval(varctr).

%%%%%%%%%%%%%% Skolemisation

skolemfunction(Vars,Res) :-
	getval(skctr,S),
	integer_atom(S,SA),
	concat_atoms('fsk',SA,NewSkF),
	Res =.. [NewSkF|Vars],
	incval(skctr).

/*
 * free_variables_union(Conjunction,X,UVs,GUVs).
 * Conjunction = (C1,...,Cn) is a conjunction of formulae, spearated by ','
 * X is a variable
 * UVs is a list of variables, the universal quantified ones wrt. the existential
 * quantified variable x is to be skolemized
 * Let UVi be the subset of UVs such that UVi is the set of free variables also in Ci
 * Let XUVi be UVi in case X also occurs in Ci, the empty set else.
 * GUVs is the union of all XUVis
 * GUVs is the set of universally quantified variables which are used in the skolem function 
 * for X
 * 
 * In case that X is a sorted variable - i.e. we have X{SORT} -
 * first relativize every member D in Conjunction to -sort(SORT) ; D
 * This is neccessary to skolemize e.g. 
 * 	all(Y{set},ex(Z{prop(Y)},p(Z))).
 * to 	p(fsk2(Y{set})).
 *	gives(fsk2(Y{set}),prop(Y)).
 */

%% Hm, this code works as expected if X is a vector of _one_ variable only,
%% Bit if X is longer ???
free_variables_union((Con1,Con2),X,UVs,GUVs) :-
	sort_annotations(X,XSorts),
	unionq(X,UVs,XUVs),
	free_variables((-sort(XSorts) ; Con1),XUVs,HCon1vars),
	(intersectq(X,HCon1vars,[]) ->
	  free_variables_union(Con2,X,UVs,GUVs)
	; % Here we know that X occurs in Con1
	  free_variables((-sort(XSorts) ; Con1),UVs,Con1vars),
	  free_variables_union(Con2,X,UVs,Con2vars),
	  unionq(Con1vars,Con2vars,GUVs
         )).
free_variables_union(Con,X,UVs,GUVs) :-
	sort_annotations(X,XSorts),
	unionq(X,UVs,XUVs),
	free_variables((-sort(XSorts) ; Con),XUVs,Convars),
	(intersectq(X,Convars,[]) -> 
	   GUVs = []
	;  % Here we know that X occurs in Con
	   free_variables((-sort(XSorts) ; Con),UVs,GUVs)).


%% skolemize works for prenex normal form, currently used for matrix in cnf

skolemize(ex(X,F),Matrix,UVs,Res,SkolemDecls) :-
	free_variables_union(Matrix,X,UVs,GUVs),
	skolemfunction_vector(GUVs,X,XSkFPairs,ThisSkolemDecls),
% old
%	skolemfunction_vector(UVs,X,XSkFPairs),
	subst_vector(XSkFPairs, F, Fsk),
	subst_vector(XSkFPairs, Matrix, Matrixsk),
	skolemize(Fsk,Matrixsk,UVs,Res,FSkolemDecls),
	append(ThisSkolemDecls,FSkolemDecls,SkolemDecls).
skolemize(all(X,F),Matrix,UVs,Res,SkolemDecls) :-
	%% make sure that inner existential quantification will not introduce variables
	%% by a universal quantification within that existential quantifier
%	new_quantification(all(X,F),all(X1,F1)),
	newvar_vector(X,X1,X_X1_Pairs),
	subst_vector(X_X1_Pairs,F,F1),
	subst_vector(X_X1_Pairs,Matrix,Matrix1),
	append(X1,UVs,F1UVs),
	skolemize(F1,Matrix1,F1UVs,Res,SkolemDecls).
%p%

/* %% Old code, where variables are non- vectors
skolemize(ex(X,F),UVs,FS) :-
	skolemfunction(UVs,SkF),
	subst(X/SkF, F, Fsk),
	skolemize(Fsk,UVs,FS).

skolemize(all(X,F),UVs,FS) :-
	%% make sure that inner existential quantification will not introduce variables
	%% by a universal quantification within that existential quantifier
	%%new_quantification(all(X,F),all(X1,F1)),
	newvar(X,Y),
	subst(X/Y,F,F1),
	skolemize(F1,[Y|UVs],FS).
*/
	
%% Down at Matrix:
skolemize(F,_Matrix,_UVs,F,[]).


sort_annotation(V{Sort},Res) :-
	-?->
	meta(V), !,
	Res = Sort.
sort_annotation(_V,none).

sort_annotations([],[]).
sort_annotations([V|R],[VS|RS]) :-
	sort_annotation(V,VS),
	sort_annotations(R,RS).

%%% generate vector of skolemfunctions for binding universal variables UVars
%%% governing EVars; result (3rd arg)  is suitable for subst_vector; last arg 
%%% contains the skolemfunction declarations generated.
skolemfunction_vector(_UVars,[],[],[]).
skolemfunction_vector(UVars,[EVar|RestEVars],[EVar/SkF|RRestEVars],
	[gives(SkF,Sort)|RestSkolemDecls]) :-
	sort_annotation(EVar,Sort),
	skolemfunction(UVars,SkF),
	skolemfunction_vector(UVars,RestEVars,RRestEVars,RestSkolemDecls).


new_quantification(all(X,F),all(X1,F1)) :-
	newvar_vector(X,X1,X_X1_Pairs),
	subst_vector(X_X1_Pairs,F,F1).
new_quantification(ex(X,F),ex(X1,F1)) :-
	newvar_vector(X,X1,X_X1_Pairs),
	subst_vector(X_X1_Pairs,F,F1).

subst_vector([],Formula,Formula).
subst_vector([X/T|R],Formula,Result) :-
	subst(X/T,Formula,H),
	subst_vector(R,H,Result).

subst(X/T,(A <-> B),(SA <-> SB)) :- subst(X/T,A,SA), subst(X/T,B,SB).
subst(X/T,(A  -> B),(SA  -> SB)) :- subst(X/T,A,SA), subst(X/T,B,SB).
%subst(X/T,(A <-  B),(SA  <- SB)) :- subst(X/T,A,SA), subst(X/T,B,SB).
subst(X/T,(A,B),(SA,SB)) :- subst(X/T,A,SA), subst(X/T,B,SB).
subst(X/T,(A;B),(SA;SB)) :- subst(X/T,A,SA), subst(X/T,B,SB).
subst(X/T,- F,- F1) :- subst(X/T,F,F1).
subst(X1/_T,all(X,F),all(X,F)) :- X==X1, !.
subst(X/T,all(Y,F),all(Y,F1)) :-
	X \== Y, subst(X/T,F,F1).
subst(X1/_T,ex(X,F),ex(X,F)) :- X==X1, !.
subst(X/T,ex(Y,F),ex(Y,F1)) :-
	X \== Y, subst(X/T,F,F1).
%% down at literal case:
subst(X/T,L,LS) :-
	L =.. [P|Args],
	subst_list(X/T,Args,SArgs),
	LS =.. [P|SArgs].

subst_list(_X/_T,[],[]).
subst_list(X/T,[S|R],[ST|RR]) :-
	subst_term(X/T,S,ST),
	subst_list(X/T,R,RR).

subst_term((X/T),Y{S},TRes) :-
	-?->
	meta(Y), %% neccessary?
	(X == Y ->
	    TRes = T
	;   subst_term(X/T,S,SRes),
	    TRes = Y,
	    add_attribute(TRes,SRes)).

%% next two cases obsolete???
subst_term(X/T,Y,T) :-
	var(Y), X == Y.
subst_term(X/_T,Y,Y) :-
	var(Y), X \== Y.
subst_term(X/T,Y,T) :-
	atomic(Y), Y == X.
subst_term(X/T,S,ST) :-
	S =.. [F|Args],
	subst_list(X/T,Args,SArgs),
	ST =.. [F|SArgs].



/*
 * free_variables(Formula,Vars,Res)
 * Res is the set of free variables in Formula which are also in Vars
 *
 */
free_variables((A <-> B), UVs, R) :- 
	free_variables(A,UVs, BA),
	free_variables(B,UVs, BB),
	unionq(BA,BB,R).
free_variables((A -> B), UVs, R) :-
	free_variables(A,UVs, BA),
	free_variables(B,UVs, BB),
	unionq(BA,BB,R).
%free_variables((A <- B), UVs, R) :-
%	free_variables(A,UVs, BA),
%	free_variables(B,UVs, BB),
%	unionq(BA,BB,R).
free_variables(-A, UVs, R) :-
	free_variables(A,UVs, R).
free_variables((A , B), UVs, R) :-
	free_variables(A,UVs, BA),
	free_variables(B,UVs, BB),
	unionq(BA,BB,R).
free_variables((A ; B), UVs, R) :-
	free_variables(A,UVs, BA),
	free_variables(B,UVs, BB),
	unionq(BA,BB,R).
free_variables(all(X,F),UVs, R) :-
	free_variables(F,UVs, RF),
	make_vector(X,XVec),
	subtractq(RF,XVec,R).
free_variables(ex(X,F),UVs, R) :-
	free_variables(F,UVs, RF),
	make_vector(X,XVec),
	subtractq(RF,XVec,R).

%% down at literal level
free_variables(B,  UVs, R) :-
	free_variables_term(B,  UVs, R).

free_variables_term(B,  UVs, [B]) :-
	memberq(B,UVs).
free_variables_term(B,  _UVs, []) :-
	(var(B) ; atomic(B)). % a variable which is not in the scope of UVs, or a constant

free_variables_term(T,  UVs, Res) :-
	functor(T,_F,A),
	free_variables_term(T,UVs,A,Res), !.
free_variables_term(_T,_UVs, 0,[]) :- !.
free_variables_term(T,UVs,M,Res) :-
	arg(M,T,TM),
	free_variables_term(TM,UVs,VH),
	N is M-1,
	free_variables_term(T,UVs,N,V1),
	unionq(VH,V1,Res).


%%%%%%%% little helpers 

intersectq([], _, []).

intersectq([Element|Residue], Set, Result) :-
        memberq(Element, Set),
        !,                    
        Result = [Element|Intersection],
        intersectq(Residue, Set, Intersection).

intersectq([_|Rest], Set, Intersection) :-
        intersectq(Rest, Set, Intersection).

unionq([], Set2, Set2).
unionq([Element|Residue], Set, Union) :-
       memberq(Element, Set), !,
       unionq(Residue, Set, Union), !.
unionq([Element|Residue], Set, [Element|Union]) :-
       unionq(Residue, Set, Union).

memberq(HArg,[Arg|_]) :-
        HArg == Arg, !.
memberq(Arg,[_|Tail]) :-
        memberq(Arg,Tail).

pl_list_con( (K, L), [K | PL_L] ) :- pl_list_con(L, PL_L), !.
pl_list_con( K, [K] ).

pl_list_dis( (K; L), [K | PL_L] ) :- pl_list_dis(L, PL_L), !.
pl_list_dis( K, [K] ).

% SPlit a list of clauses using ';' notation to a list of elements [Pos,Neg] 
% where Pos (resp Neg) is the positive (resp. negative) list of literals of each
% clause.
split_clauses_dis([],[]).
split_clauses_dis([F|R],[[FPos,FNeg]|RS]) :-
	pl_list_dis(F,FList),
	split_clause(FList,FPos,FNeg),
	split_clauses_dis(R,RS).

% SPlit clause into list of positive and negative literals 
split_clause([],[],[]).
split_clause([Lit|RestLits],[Lit|ResPos],ResNeg) :-
	is_pos(Lit) , ! ,
	split_clause(RestLits,ResPos,ResNeg), 
	!.
split_clause([Lit|RestLits],ResPos,[Lit|ResNeg]) :-
	%% Lit is negative
%	negate(Lit,NLit),
	split_clause(RestLits,ResPos,ResNeg), !.

is_pos(- _) :- !, fail.
is_pos(~ _) :- !, fail.
is_pos(_).
is_neg(- _).
is_neg(~ _).

%%%%%%%%%%%%%%%%%%% FILE utilities %%%%%%%%%
%% Ingo: all these here are NOT needed by clausify and can be stripped 
%% The main entry point is pl2tme

/*

:- [myread, %%% myread reads a prolog term, but does NOT skip comments
    parser %, %%% not neccessary for clausify
   ].

:- global pl2tme/2.

%% Convert the formualas in InFileName.pl1 to clauses, dumped into file OutFileName.tme
% Both arguments are prolog atoms.

pl2tme(InFileName,OutFileName) :-
	setval(tme_cleanlit,off),
	init_parser,
	concat_atoms(InFileName,'.pl1',InFileNameExt),
	atom_string(InFileNameExt,InFileNameExtString),
	consult_file(InFileNameExtString),
	concat_atoms(OutFileName, '.tme', OutFileNameExt),
	open(OutFileNameExt, write, F),
	printf(F,"%%%% File generated by pl2tme utility.\n%%%% Have a nice proof!\n\n",[]),
	convert_clauses_to_file(F),
	close(F).

clausify_for_peter(F,Clauses) :-
	nnf(F,Fnnf),
	matrix(Fnnf,Matrix),
	cnf(Matrix,Mcnf),
	norm_con(Mcnf,NMcnf), 
	set_matrix(Fnnf,NMcnf,FNMcnf),
	skolemize(FNMcnf,NMcnf,[],S,SkolemDecls), 
	pl_list_con(S,SList),
	append(SList,SkolemDecls,Clauses).



%% split_clause(Clause,Pos,Neg,PosVars,SV).
%% Pos (resp Neg) is the list of positive literals occuring in clause Clause
%% SV is the list of Variables shared by the positive literals
%% PosVars is the list of variables in the Pos lits but not in the Neg lits.
%% 
split_clause(Clause,Pos,Neg,PosVars,SV) :-
	split_clause_(Clause,Pos,Neg,PosLitVarSets),
	unionqlists(PosLitVarSets,HPosVars),
	term_variables(Neg,NegVars),
	subtractq(HPosVars,NegVars,PosVars),
	commonvars(PosLitVarSets,SV), !.

split_clause_([],[],[],[]).
split_clause_([Lit|RestLits],[Lit|ResPos],ResNeg,[LitVars|ResLitVars]) :-
	is_pos(Lit) , ! ,
	term_variables(Lit,LitVars),
	split_clause_(RestLits,ResPos,ResNeg,ResLitVars), 
	!.
split_clause_([Lit|RestLits],ResPos,[NLit|ResNeg],LitVars) :-
	%% Lit is negative
	negate(Lit,NLit),
	split_clause_(RestLits,ResPos,ResNeg,LitVars), !.


pl_list_dis_or_con( (K; L), [K | PL_L] ) :- pl_list_dis_or_con(L, PL_L), !.
pl_list_dis_or_con( (K, L), [K | PL_L] ) :- pl_list_dis_or_con(L, PL_L), !.
pl_list_dis_or_con( K, [K] ).

norm_clause([],[]).
% this is done by norm_dis_taut
%norm_clause([-false|_R],[true]).
%norm_clause([~false|_R],[true]).
%norm_clause([true|_R],[true]).
%norm_clause([false|L],LR) :- !, norm_clause(L,LR).
%norm_clause([-true|L],LR) :- !, norm_clause(L,LR).
%norm_clause([~true|L],LR) :- !, norm_clause(L,LR).
norm_clause([~K|L],[-K|LR]) :- norm_clause(L,LR), !.
norm_clause([K|L],[K|LR]) :- norm_clause(L,LR), !.


skip_clause(F,C,comment) :-
	name(C,[StringCommentSign_ascii|Rest]),
	Comment_sign = '%',
%	getval(comment_sign,Comment_sign),
	name(Comment_sign,[Comment_sign_ascii]),
	(StringCommentSign_ascii = Comment_sign_ascii ->
	    NewC = C %% have the correct comment sign
	%% else replace first char by correct comment sign for output language
	; name(NewC,[Comment_sign_ascii|Rest])), 
	printf(F,"%w\n",[NewC]).
skip_clause(F,C,skip) :-
%	writeq(F,C), nl(F).
	printf(F,"%q.\n",C).



convert_clauses_to_file(F) :-
	retract(oi_clause(C,Type)),
	((Type == comment ; Type == skip) ->
	    skip_clause(F,C,Type) %% Keep the comments
        ; 
%	    convert_general_clause_to_clauses(C,FinalFormula,Clauses),
	    % simplification here - assume that C is in proper format to call clausify
	    % in particular, prolog rules with free variables are no longer allowed
	    [Formula] = C,
	    clausify_for_peter(Formula,Clauses),
	    ( Type == query ->
		Typestring = 'Query formula:'
	    ; Typestring =  'Input formula:'
            ),
%	    printf(F,"%%%% %w %Dw\n",[Typestring,FinalFormula]),
	    printf(F,"%%%% %w %Dmw\n",[Typestring,Formula]),
	    write_clauses(F,Clauses,Type),
	    printf(F,"\n",[])),
	convert_clauses_to_file(F).
convert_clauses_to_file(_F).


write_clauses(_F,[],_Type).
write_clauses(F,[Clause|Rest],Type) :-
	pl_list_dis(Clause,LitList),
	split_clause(LitList,Pos,Neg,_PosVars,_SV),
	write_tme_clause(F,Pos,Neg,Type),
	write_clauses(F,Rest,Type).

%% write out tme clauses:
write_tme_clause(F,[],Neg,input) :-
	printf(F,"false :- ",[]),
	write_tme_litlist(F,',',Neg),
	printf(F,".\n",[]), !.
write_tme_clause(F,Pos,[],_Input_or_query) :-
	%% never write positive clause in  ?- form
	!,
	write_tme_litlist(F,';',Pos),
	printf(F,".\n",[]), !.
write_tme_clause(F,Pos,Neg,input) :-
	write_tme_litlist(F,';',Pos),
	printf(F," :- ",[]),
	write_tme_litlist(F,',',Neg),
	printf(F,".\n",[]), !.

write_tme_clause(F,Pos,Neg,query) :-
	negate_list(Pos,NPos),
	append(NPos,Neg,Q),
	printf(F,"?- ",[]),
	write_tme_litlist(F,',',Q),
	printf(F,".\n",[]), !.

write_tme_litlist(_F,_Sep,[]).
write_tme_litlist(F,Sep,[L|R]) :-
%%% Ingo-servicable part: Hier werden Literale geprinted
%	printf(F,"%UPVmw",[L]),
%	printf(F,"%Dvmw",[L]),
%	printf(F,"%Dmw",[L]),
	printf(F,"%DVmw",[L]),
	( R \== [] ->
	    printf(F,"%mw ",[Sep])
	  ; true),
	write_tme_litlist(F,Sep,R).

negate(- Literal, Literal) :- !.
negate(~ Literal, Literal) :- !.
negate(Literal, - Literal).

negate_list([],[]).
negate_list([H|T],[NH|NT]) :-
	negate(H,NH),
	negate_list(T,NT).


% subtractq(L1, L2, L3)
% L3 = L1 - L2

subtractq([], _, []).
subtractq([Head|L1tail], L2, L3) :-
        memberq(Head, L2),
        !,
        subtractq(L1tail, L2, L3).
subtractq([Head|L1tail], L2, [Head|L3tail]) :-
        subtractq(L1tail, L2, L3tail).

commonvars([],[]).
commonvars([F|R],L) :-
	unionqlists(R,UR),
	intersectq(F,UR,PosLits),
	commonvars(R,R1),
	unionq(PosLits,R1,L).



unionqlists([],[]).
unionqlists([F|R],Res) :-
	unionqlists(R,HRes),
	unionq(F,HRes,Res).


*/