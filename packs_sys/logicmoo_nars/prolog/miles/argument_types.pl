% MODULE argument_types EXPORTS
:- module( argument_types,
           [ argument_types/0,
             type_restriction/2,
             type_equal/2,
             type_equal/4,
             replace_t/4,
             type_sub/2,
             type_of/3,
             types_of/3,
             compare_types/3,
             define_type/0,
             verify_types/0
           ]).


% IMPORTS
:- use_module(home(kb),
                   [get_example/3, get_clause/5, 
                    store_clauses/2,store_clause/4,
                    known/6,delete_clause/1]).
:- use_module(home(lgg),
                   [set_lgg/2]).
:- use_module(home(div_utils),
                   [body2list/2, myforall/2, different_predicates/2,
                    nth_arg/3, remove_v/3, make_unique/2, shares_var/2,
                    mysetof/3]).
:- use_module(home(var_utils),
                   [only_vars/2]).
:- use_module(home(td_basic),
                   [append_body/3]).
:- use_module(home(interpreter),
                   [t_interpreter/2]).
:- use_module(home(show_utils),
                  [show_kb_types/0]).
:- use_module_if_exists(library(subsumes),
                      [variant/2]).
:- use_module_if_exists(library(occurs),
                      [contains_var/2]).
:- use_module_if_exists(library(basics),
                      [member/2]).
:- use_module_if_exists(library(strings),
                      [gensym/2]).

% METAPREDICATES
% none


:- dynamic type_restriction/2.

%***********************************************************************
%*	
%* module: argument_types.pl
%*									
%* author: I.Stahl             date:12/92	
%*									
%* changed:								
%*									
%*									
%* description: algorithm for determining argument types 
%*              results for each predicate p within the pos examples         
%*              in a kb entry 
%*              type_restriction(p(V1,..,Vn),[type1(V1),...,typen(Vn)]) 
%*		
%* 
%* see also:								
%*									
%***********************************************************************


%***********************************************************************
%*									
%* predicate: 	argument_types/0
%*									
%* syntax:	-							
%*									
%* args:	none							
%*									
%*									
%* description: toplevel predicate for determining argument types 
%*              results for each predicate p within the pos examples         
%*              in a kb entry 
%*              type_restriction(p(V1,..,Vn),[type1(V1),...,typen(Vn)]) 
%*		
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%*									
%* see also:								
%*									
%***********************************************************************

argument_types:- 
   mysetof(E,I^get_example(I,E,'+'),Elist), % Elist = [E1,...,En] pos examples
   different_predicates(Elist,Elist1),      % Elist1 = [[E1,..,Em],...]
                                            % list of lists of pos examples with
                                            % the same predicate symbol
   argument_types(Elist1).


argument_types([]).
argument_types([E|R]):-
   argument_types(R),
   arg_types(E).

arg_types([E|R]):-
   functor(E,P,N),
   functor(P1,P,N),
   (   type_restriction(P1,_) ->
       true
   ;   assertz(type_restriction(P1,[]))  % assert a type restriction for the 
                                         % predicate if not already present
   ),
   arg_types(N,[E|R],P,N).




%***********************************************************************
%*									
%* predicate:	arg_types/4
%*									
%* syntax:	arg_types(+Counter,+Examplelist,+Pred_symbol,+Pred_arity)
%*									
%* args:								
%*									
%*									
%* description:								
%* 	for each argument position (1 to Pred_arity) of the predicate Pred_symbol
%* 	the type of the terms occurring at that position is determined.
%* 	If the same type occurred already elsewhere, the old definition is taken
%* 	in order to avoid duplicate type definitions
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%*									
%* see also:								
%*									
%***********************************************************************

arg_types(0,_,_,_):- !.
arg_types(N,EL,P,M):-
   N1 is N - 1,
   arg_types(N1,EL,P,M),
   nth_arg(EL,N,S),
   gensym(type,Type),
   arg_type(S,[],[],CL,Type),
   minimize_cl(CL,Type,Type1),
   adapt_type_restriction(M,P,N,Type1).


%***********************************************************************
%*									
%* predicate:	arg_type/5
%*									
%* syntax: arg_type(+Set_of_Argterms,+Ancestors,+Clause_list,-Clause_list,+Typename)
%*									
%* args:								
%*									
%*									
%* description:								
%* 	Ancestors are all types calling Typename in their definition. Clauselist
%* 	contains all clauses defining Typename
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%*									
%* see also:								
%*									
%***********************************************************************

arg_type(S,Ancestors,CL,CL2,T):-
   different_predicates(S,Slist), % splits the set of Argterms according to 
                                  % different functors, Slist = [[T1,..,Tm],..]

   init_cl(Slist,T,CL0),          % for each set of Argterms in Slist, generate
                                  % a clause head with pred symbol Typename
   append(CL,CL0,CL1),
   refine_cl(Slist,[T|Ancestors],CL0,CL1,CL2). % generate clause bodies 

init_cl([],_,[]).
init_cl([EL|R],T,[(T1:-true)|R1]):-
   init_cl(R,T,R1),
   set_lgg(EL,E),
   T1 =.. [T,E].


%***********************************************************************
%*									
%* predicate:	refine_cl/5							
%*									
%* syntax:	 refine_cl(+Slist,+Ancestors,+Clauses,+Clauselist,-Clauselist)
%*									
%* args:								
%*									
%*									
%* description:								
%* 	for each set of Argterms in Slist and each corresponding clause head 
%* 	in Clauses and Clauselist, add a body literal for each variable in the
%* 	clause head. This body literal may be atom(_),atomic(_),number(_),
%* 	typex(_),where typex is in Ancestors, or typez(_), where typez is a 
%*      new type (recursive call of the algorithm)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%*									
%* see also:								
%*									
%***********************************************************************

refine_cl([],_,_,CL,CL).
refine_cl([S|R],A,[(Head:- _)|R1],CL,CL2):-
   refine_cl(R,A,R1,CL,CL1),
   arg(1,Head,E),
   (   var(E) ->
       test_var_instantiations(E,S,Head,A,CL1,CL2) 
                % if the head argument is a variable, test its instantiations
                % in S and add the corresponding literal to the body of Head 
   ;   functor(E,_,N),
       ref_cl(N,E,S,Head,A,CL1,CL2) 
                % if the head argument is no variable,
                % decompose it, test the variables it contains
                % and add the corresponding literals to the body
                % of Head
   ).


%***********************************************************************
%*									
%* predicate:	ref_cl/7						
%*									
%* syntax:	ref_cl(+Counter,+Argument,+Argterms,+Head,+Ancestors,
%*                     +Clauselist, -Clauselist)
%*									
%* args: 
%*									
%* description:	decompose the head argument and test the variables it contains;
%*              add the corresponding literals to the body of Head
%*									
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%*									
%* see also:								
%*									
%***********************************************************************

ref_cl(0,_,_,_,_,CL,CL):- !.
ref_cl(N,E,S,H,A,CL,CL2):-
   N1 is N - 1,
   ref_cl(N1,E,S,H,A,CL,CL1),
   arg(N,E,X),nth_arg(S,N,Sn),
   (   var(X) ->
       test_var_instantiations(X,Sn,H,A,CL1,CL2)
   ;   functor(X,_,M),
       ref_cl(M,X,Sn,H,A,CL1,CL2)
   ).



%***********************************************************************
%*									
%* predicate:	test_var_instantiation/6
%*									
%* syntax: test_var_instantiations(+Var,+Argterms,+Head,+Ancestors,
%*                                 +Clauselist,-Clauselist)
%*									
%* args:								
%*									
%*									
%* description:								
%* 	Argterms are the instantiations of Var. If all instantiations of Var
%* 	are atoms/number/atomic, the literal atom(Var)/number(Var)/atomic(Var) is
%* 	added to the body of Head in Clauselist. Else if the definition of a typex
%* 	in Ancestors covers all instantiations of Var, typex(Var) is added to the
%* 	body of Head in Clauselist (recursive definition). Else a new symbol typen
%* 	is created, the literal typen(Var) is added to the body of Head in Clauselist
%* 	and a definition of typen is induced by a recursive call of arg_type.
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

test_var_instantiations(X,S,H,_,CL,CL1):-
   myforall(S,atom),!,
   Lit =.. [atom,X],
   add_literal(CL,H,Lit,CL1).

test_var_instantiations(X,S,H,_,CL,CL1):-
   myforall(S,number),!,
   Lit =.. [number,X],
   add_literal(CL,H,Lit,CL1).

test_var_instantiations(X,S,H,_,CL,CL1):-
   myforall(S,atomic),!,
   Lit =.. [atomic,X],
   add_literal(CL,H,Lit,CL1).

test_var_instantiations(X,S,H,A,CL,CL1):-
   test_ancestor(S,A,CL,APred),!,
   Lit =.. [APred,X],
   add_literal(CL,H,Lit,CL1).

test_var_instantiations(X,S,H,A,CL,CL1):-
   gensym(type,T),
   Lit =.. [T,X], 
   add_literal(CL,H,Lit,CL0),
   arg_type(S,A,CL0,CL1,T).


test_ancestor(S,[APred|_],CL,APred):-
   myforall_interpreted(S,APred,CL),!.
test_ancestor(S,[_|R],CL,APred):-
   test_ancestor(S,R,CL,APred).


%***********************************************************************
%*									
%* predicate:	myforall_interpreted/3							
%*									
%* syntax: myforall_interpreted(+Argterms,+Pred,+Clauselist)
%*									
%* args:								
%*									
%*									
%* description:	tests for each argument term T in Argterms whether Pred(T)
%*              follows from Clauselist. For that purpose, a special interpreter
%*              t_interpreter is used that works on Clauselist as program instead
%*              of the knowledge base.						
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%*									
%* see also:								
%*									
%***********************************************************************

myforall_interpreted([],_,_).
myforall_interpreted([E|R],Pred,CL):-
   C =.. [Pred,E],
   t_interpreter(C,CL),
   myforall_interpreted(R,Pred,CL).


%***********************************************************************
%*									
%* predicate:	add_literal/4							
%*									
%* syntax: add_literal(+Clauselist,+Head,+Lit,-Clauselist1)
%*									
%* args:								
%*									
%* description:	adds literal Lit to the clause (Head:- B) within Clauselist
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%*									
%* see also:								
%*									
%***********************************************************************

add_literal([(H:-B)|R],H1, Lit, [(H:- (Lit,B))|R]):-
   H == H1,!.
add_literal([C|R],H,Lit,[C|R1]):-
   add_literal(R,H,Lit,R1).


%***********************************************************************
%*									
%* predicate:	adapt_type_restriction/4
%*									
%* syntax: adapt_type_restriction(+Pred_arity,+Pred_name,+A,+Type)
%*									
%* args:								
%*									
%* description:								
%* 	Type is the type of the Ath Argposition of the predicate Pred_name. The 
%* 	type restriction is type_restriction(p(V1,..,Vn),L). If there is not yet 
%* 	an entry typex(VA) in L, Type(VA) is added to L. Else let the definition
%*	 of typex be typex(Hx1):- Bx1.  and of Type be Type(H1):- B1.
%*                   ...                               ...
%*                   typex(Hxm):- Bxm.                 Type(Ho):- Bo.
%* 	Then we add a new type Tnew(VA) to L with the definition
%*             Tnew(Hx1):- Bx1.                 Tnew(H1):- B1.
%*             ...                              ...
%*             Tnew(Hxm):- Bxm.                 Tnew(Ho):- Bo. 
%* 	The definitions of typex and Type remain.
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************
   
adapt_type_restriction(M,P,N,T):-
   functor(P1,P,M),
   retract(type_restriction(P1,L)),
   arg(N,P1,P1n),
   (   (member(T1,L),T1 =.. [T2,X], X == P1n) ->
       gensym(type,Tnew),
       D =.. [Tnew,P1n],
       remove_v([T1],L,L1),
       assertz(type_restriction(P1,[D|L1])),
       adapt_tr(Tnew,T,T2)
   ;   D =.. [T,P1n],
       assertz(type_restriction(P1,[D|L]))
   ).

adapt_tr(Tnew,T1,T2):-
   functor(HT1,T1,1),functor(HT2,T2,1),
   mysetof((HT1:-B1),I^Clist^(get_clause(I,HT1,B1,Clist,type)),C1),
   mysetof((HT2:-B2),I^Clist^(get_clause(I,HT2,B2,Clist,type)),C2),
   append(C1,C2,C3),
   adapt_tr1(C3,Tnew,C4),
   make_unique(C4,C5), 
   store_clauses(C5,type).

adapt_tr1([],_,[]).
adapt_tr1([(H:-B)|R],T,[(H1:-B)|R1]):-
   adapt_tr1(R,T,R1),
   H =.. [_|Arg], H1 =.. [T|Arg].


%***********************************************************************
%*									
%* predicate:	minimize_cl/3
%*									
%* syntax:	minimize_cl(+CL,+Typename,-Typename)
%*									
%* args:								
%*									
%* description:	CL is the list of clauses defining the type Typename. 
%*	If CL contains definitions that occur already in the database,			
%* 	or if it contains duplicate definitions, it is minimized.	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%*									
%* see also:								
%*									
%***********************************************************************

minimize_cl(CL,Type,Type1):-
   mysetof((H:-B),I^Clist^(get_clause(I,H,B,Clist,type)), Old_types),
   mysetof(T, H^B^R^(member((H:-B),Old_types),H =.. [T|R]), Oldt_names),
   mysetof(T, H^B^R^(member((H:-B),CL),H =.. [T|R]), Newt_names),
   append(Old_types,CL,Clauses),
   minimize_cl(Oldt_names,Newt_names,Clauses,CL,Type,Type1).

minimize_cl([],Newt_names,Clauses,CL,Type,Type1):-
   minim_cl(Newt_names,Clauses,CL,Type,Type1).
minimize_cl([T|R],Newt_names,Clauses,CL,Type,Type2):-
   mysetof(T1,(member(T1,Newt_names),type_equal(T,T1,[T:T1],Clauses)),Tlist),
   replace_t(CL,Tlist,T,CL1),
   make_unique(CL1,CL2),
   remove_v(Tlist,Newt_names,Newt_names1),
   (   member(Type,Tlist) ->
       Type1 = T
   ;   Type1 = Type
   ),
   minimize_cl(R,Newt_names1,Clauses,CL2,Type1,Type2).

minim_cl([],_,CL,Type,Type):- 
   min_cl(CL,CL1),
   store_clauses(CL1,type). % the remaining (minimized) set of clauses is stored
                            % in the database
minim_cl([T|R],Clauses,CL,Type,Type2):-
   mysetof(T1,(member(T1,R),type_equal(T,T1,[T:T1],Clauses)),Tlist),
   replace_t(CL,Tlist,T,CL1),
   make_unique(CL1,CL2),
   remove_v(Tlist,R,R1),
   (   member(Type,Tlist) ->
       Type1 = T
   ;   Type1 = Type
   ),
   minim_cl(R1,Clauses,CL2,Type1,Type2).

min_cl([],[]).
min_cl([(H:-B)|R],[(H:-B1)|R1]):-
   min_cl(R,R1),
   min_cl1(B,B1).

min_cl1((A,true),A):-!.
min_cl1(true,true):- !.
min_cl1((A,B),(A,B1)):-
   min_cl1(B,B1).


%***********************************************************************
%*									
%* predicate:	replace_t/4
%*									
%* syntax:	replace_t(+CL,+List_of_typenames,+Typename,-CL)
%*									
%* args:								
%*									
%* description:	replaces in CL each occurrence of a typename				
%*	in List_of_typenames by Typename						
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

replace_t([],_,_,[]).
replace_t([(H:-_)|R],Tlist,T,R1):-
   H =.. [T1|_], member(T1,Tlist),!,
   replace_t(R,Tlist,T,R1).
replace_t([(H:-B)|R],Tlist,T,[(H:-B1)|R1]):-
   repl_t(B,Tlist,T,B1),
   replace_t(R,Tlist,T,R1).

repl_t((A,B),Tlist,T,(A1,B1)):- !,
   repl_t(A,Tlist,T,A1),
   repl_t(B,Tlist,T,B1).
repl_t(A,Tlist,T,A1):-
   A =.. [T1|R],
   (   member(T1,Tlist) ->
       A1 =.. [T|R]
   ;   A1 = A
   ).


%***********************************************************************************%
%*
%* predicate: type_equal/2
%*
%* syntax: type_equal(+Type_name1,+Type_name2)
%*
%* args: 
%*
%* description: Tests whether the types Type_name1 and Type_name2 are defined 
%* 	        identically
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*									
%**********************************************************************************

type_equal(T,T):- !.
type_equal(T1,T2):-
   mysetof((H1:-B1), I^CL^R^(get_clause(I,H1,B1,CL,type), H1 =.. [T1|R]),Clauses1),
   mysetof((H2:-B2), I^CL^R^(get_clause(I,H2,B2,CL,type), H2 =.. [T2|R]),Clauses2),
   append(Clauses1,Clauses2,Clauses),
   type_equal(T1,T2,[T1:T2],Clauses).

type_equal(T,T,_,_):- !.
type_equal(T1,T2,Ancestors,Clauses):-
   mysetof((H:-B),R^(member((H:-B),Clauses), H =.. [T1|R]),Clist1),
   mysetof((H:-B),R^(member((H:-B),Clauses), H =.. [T2|R]),Clist2),
   compare_clauses(Clist1,Clist2,Clauses,Ancestors).


%***********************************************************************
%*									
%* predicate:	compare_clauses/4						
%*									
%* syntax: compare_clauses(+Clist1,+Clist2,+Clauses12,+Ancestors)
%*									
%* args: Clist1 .. clauses defining Type_name1
%*       Clist2 .. clauses defining Type_name2	
%*       Clauses12 .. all clauses defining Type_name1 and Type_name2
%*	 Ancestors .. types already tested on equality
%*									
%* description: tests whether Type_name1 and Type_name2 are identically
%*              defined by comparing the defining clauses
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%*									
%* see also:								
%*									
%***********************************************************************

compare_clauses([],[],_,_).
compare_clauses([(H1:-B1)|R],CL2,Clauses,Ancestors):-
   find_variant_clause(CL2,H1,CL21,(H2:-B2)),
   arg(1,H1,E1),arg(1,H2,E2),
   comp_clauses(E1,E2,B1,B2,Clauses,Ancestors),
   compare_clauses(R,CL21,Clauses,Ancestors).

comp_clauses(E1,E2,B1,B2,C,A):-
   var(E1),!,
   def_literal(E1,B1,L1),def_literal(E2,B2,L2),
   L1 =.. [T1|_], L2 =.. [T2|_],
   c_clauses(T1,T2,C,A).
comp_clauses(E1,E2,B1,B2,C,A):-
   functor(E1,_,N),
   comp_clauses(N,E1,E2,B1,B2,C,A).
comp_clauses(0,_,_,_,_,_,_):- !.
comp_clauses(N,E1,E2,B1,B2,C,A):-
   N1 is N - 1,
   comp_clauses(N1,E1,E2,B1,B2,C,A),
   arg(N,E1,E1n),arg(N,E2,E2n),
   comp_clauses(E1n,E2n,B1,B2,C,A).

c_clauses(atom,L,_,_):- !, L = atom.
c_clauses(number,L,_,_):- !, L = number.
c_clauses(atomic,L,_,_):- !, L = atomic.
c_clauses(_,L2,_,_):-
   (L2 = atom ; L2 = number ; L2 = atomic),!,
   fail.
c_clauses(T1,T2,C,A):-
   (   (member(T1:T2,A);member(T2:T1,A)) ->
       true
   ;   type_equal(T1,T2,[T1:T2|A],C)
   ).

%***********************************************************************
%*									
%* predicate:	find_variant_clause/4
%*									
%* syntax: find_variant_clause(+CL,+Head,-CL1,-Clause)
%*									
%* args:								
%*									
%* description:	CL1 is CL - Clause, where the head argument of Head and
%*              of the head of Clause are variants
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%*									
%* see also:								
%*									
%***********************************************************************

find_variant_clause([(H2:-B2)|R],H1,R,(H2:-B2)):-
   arg(1,H1,E1),arg(1,H2,E2),
   variant(E1,E2).
find_variant_clause([C|R],H,[C|R1],C1):-
   find_variant_clause(R,H,R1,C1).

%***********************************************************************
%*									
%* predicate:	def_literal/3							
%*									
%* syntax: def_literal(+Var,+Body,-Lit)
%*									
%* args:								
%*									
%* description:	Lit is the literal within Body that defines the type of
%*              Var
%*									
%* example: def_literal(A,(atom(A),list(B)),atom(A))
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

def_literal(X,(A,B),C):- !,
   (   contains_var(X,A) ->
       C = A
   ;   def_literal(X,B,C)
   ).
def_literal(X,A,A):-
   contains_var(X,A),!.
def_literal(X,_,all(X)).

%**********************************************************************************
%*
%*  predicate: type_sub/2
%* 
%*  syntax: type_sub(+Gen,+Spec)
%*
%*  args: Gen, Spec: type names or intermediate type definitions 
%*        t_int(H):- B (cf. type_of).
%*
%*  description: succeeds if the type Gen is more general than
%*  	the type Spec. 
%*
%*  example:
%*
%*  peculiarities: none
%*									
%**********************************************************************************


type_sub(Gen,(H:-B)):-
   type_sub1([(H:-B)],Gen,[]).
type_sub(Gen,Spec):-
   type_sub(Gen,Spec,[Gen:Spec]).



%***********************************************************************
%*									
%* predicate:	 type_sub/3							
%*									
%* syntax:	 type_sub(+Gen,+Spec,+Ancestors)
%*									
%* args:								
%*									
%* description:								
%* 	Ancestors contains the types that have been compared already in order to
%* 	avoid infinite recursion
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************


type_sub(all,_,_):- !. % all is the most general type
type_sub(T1,all,_):- !, T1 = all.    
type_sub(T,T1,_):- T == T1,!.

type_sub(atomic,T,_):-
   !, ( T = atom
      ; T = atomic 
      ; T = number
      ; functor(HT,T,1),
        setof((HT,B),ID^CL^(get_clause(ID,HT,B,CL,type)),TL),
        all_t_in(TL,[atom,number,atomic])
      ).
type_sub(atom,T,_):- 
   !, ( T = atom
      ; functor(HT,T,1),
        setof((HT,B),ID^CL^(get_clause(ID,HT,B,CL,type)),TL),
        all_t_in(TL,[atom])
      ). 
type_sub(number,T,_):- 
   !, ( T = number
      ; functor(HT,T,1),
        setof((HT,B),ID^CL^(get_clause(ID,HT,B,CL,type)),TL),
        all_t_in(TL,[number])
      ).

type_sub(T,atomic,_):- 
   !, ( T = atomic
      ; T = all
      ; functor(HT,T,1),
        setof((HT,B),ID^CL^(get_clause(ID,HT,B,CL,type)),TL),
        all_t_in(TL,[atomic])
      ).
type_sub(T,atom,_):- 
   !, ( T = atom
      ; T = atomic
      ; T = all
      ; functor(HT,T,1),
        setof((HT,B),ID^CL^(get_clause(ID,HT,B,CL,type)),TL),
        all_t_in(TL,[atom,atomic])
      ).
type_sub(T,number,_):- 
   !, ( T = number
      ; T = atomic
      ; T = all
      ; functor(HT,T,1),
        setof((HT,B),ID^CL^(get_clause(ID,HT,B,CL,type)),TL),
        all_t_in(TL,[number,atomic])
      ).


type_sub(TG,TS,A):-
   functor(HTS,TS,1),
   mysetof((HTS:- BS),ID^CL^(get_clause(ID,HTS,BS,CL,type)),CS),
   type_sub1(CS,TG,A).

%***********************************************************************
%*									
%* predicate:	type_sub1/3							
%*									
%* syntax:	type_sub1(+SpecClauses,+Gen,+Ancestors)
%*									
%* args:								
%*									
%* description:								
%* 	for each clause C in SpecClauses defining the more specific type, 
%* 	there must be a clause defining Gen that is more general than C
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

type_sub1([],_,_).
type_sub1([(H:-B)|R],(HG:-BG),A):- !,
   type_sub1(R,(HG:-BG),A),!,
   H =.. [_,Es], HG =.. [_,Es],
   expand_to_type_def(BG,BG1),
   is_type_definition((H:- B)),
   test_type_def(B),   
   is_type_definition((HG:-BG1)),
   test_type_def(BG1),   
   only_vars(Es,EsV),
   type_sub2(EsV,BG1,B,A).
type_sub1([(H:-B)|R],TG,A):-
   type_sub1(R,TG,A),!,
   H =.. [_,Es], HTG =.. [TG,Es],
   get_clause(_,HTG,BG,_,type),
   expand_to_type_def(BG,BG1),
   is_type_definition((H:- B)),
   test_type_def(B),   
   is_type_definition((HTG:-BG1)),
   test_type_def(BG1),   
   only_vars(Es,EsV),
   type_sub2(EsV,BG1,B,A).


%***********************************************************************
%*									
%* predicate:	type_sub2/4						
%*									
%* syntax:	type_sub2(+Varlist,+Genbody,+Specbody,+Ancestors)	
%*									
%* args:								
%*									
%* description:								
%* 	tests for each variable V in Varlist whether the literal defining V in Genbody
%* 	is of a more general type than the literal defining V in Specbody.
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

type_sub2([],_,_,_).
type_sub2([X|R],BG1,B,A):-
   type_sub2(R,BG1,B,A),!,
   def_literal(X,BG1,LG),
   def_literal(X,B,LS),
   LS =.. [TS|_], LG =.. [TG|_],
   (    member(TG:TS,A) ->
        true
   ;    type_sub(TG,TS,[TG:TS|A])
   ).


%***********************************************************************
%*									
%* predicate:	expand_to_type_definition/2
%*									
%* syntax:	expand_to_type_definition(+Body,-Body1)
%*									
%* args:								
%*									
%* description:								
%* 	transform Body to a valid type definition (i.e. each literal is of the 
%* 	form t(X), were t in atom,atomic,number,typex and X is simple), by expanding
%* 	literals containing complex terms
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

expand_to_type_def((A,B),(A,B1)):-
   simple_td(A),
   expand_to_type_def(B,B1).
expand_to_type_def((A,B),B1):-
   \+(simple_td(A)),
   get_clause(_,A,Lits,_,type),
   append_body(Lits,B,B0),
   expand_to_type_def(B0,B1).
expand_to_type_def(A,A):-
   simple_td(A).
expand_to_type_def(A,A1):-
   \+(A = (_,_)), \+(simple_td(A)),
   get_clause(_,A,Lits,_,type),
   expand_to_type_def(Lits,A1).

%***********************************************************************
%*									
%* predicate:	simple_td/1							
%*									
%* syntax:	simple_td(+Lit)						
%*									
%* args:  Lit is a literal within a type definition
%*									
%* description:	succeeds if Lit == true, or Lit = t(X), where X is atomic or
%*         a variable							
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

simple_td(true):- !.
simple_td(A):- A =.. [_,X],simple(X).

%***********************************************************************
%*									
%* predicate:	test_type_definition/1
%*									
%* syntax:	test_type_definition(Body)
%*									
%* args:								
%*									
%* description:	partially evaluates Body. 					
%*              Fails if body contains invalid ground literals
%*              with predicate symbol atom, number or atomic
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

test_type_def((A,B)):- !,
   test_type_def(A),
   test_type_def(B).
test_type_def(A):-
   A =.. [T,X], ground(X),
   ( T =atom; T = atomic; T = number ),!,
   call(A).
test_type_def(_).


%***********************************************************************
%*									
%* predicate:	is_type_definition/1							
%*									
%* syntax:	is_type_definition(+Clause)
%*									
%* args:								
%*									
%* description:								
%* 	succeeds if Clause is a syntactically correct type definition, i.e. only
%* 	atom, atomic, number and typex occur as unary predicate in the body, and
%* 	every variable of Clause occurs once in the head and once in the body of Clause
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

is_type_definition((H:-B)):-
   only_vars(H,Vars),
   is_type_def(Vars,B).

is_type_def([],_).
is_type_def([X|R],B):-
   def_literal(X,B,Lit),!,
   Lit =.. [_,X1],
   X == X1,
   is_type_def(R,B).

all_t_in([],_).
all_t_in([(H,B)|R],L):-
   H =.. [_,X], var(X),
   B =.. [N,X], member(N,L),
   all_t_in(R,L).


%**********************************************************************************
%*
%*  predicate: type_of/3
%* 
%*  syntax: type_of(+Var,+C,-Type)
%*
%*  args: 
%*
%*  description: C is a clause or a literal. Returns the most specific type of Var
%*               within C. If Var does not occur in C or if it occurs at 
%*               positions with incompatible types, type_of returns fail. If Var
%*               is a term that only partially matches the body of a type
%*               definition, a intermediate type definition t_int(Var):- B is
%*               returned
%* 
%*  example:
%*
%*  pecularities:
%*									
%**********************************************************************************

type_of(V,(H:-B),Type):- !,
   type_of(V,H,Type1),
   type_of(V,B,Type2),
   compare_types(Type1,Type2,Type).
type_of(V,(A,B),Type):- !,
   type_of(V,A,Type1),
   type_of(V,B,Type2),
   compare_types(Type1,Type2,Type).
type_of(T,Pred,Type):-
   (   type_restriction(Pred,Plist) ->
       mysetof(Ts,(member(Ts,Plist),contains_var(T,Ts)),TsL),
       type_of1(TsL,T,Type)
   ;   Type = all
   ).
type_of(_,true,all).

type_of1([],_,all).
type_of1([Ts|R],T,Type):- !,
   type_of1(R,T,Type1),
   Ts =.. [Type0,T2],
   (   T2 == T ->
       compare_types(Type1,Type0,Type)
   ;   get_clause(_,Ts,_,[_|CL],type),
       mysetof(Ts1,(member(Ts1:_,CL),contains_var(T,Ts1)),TsL),
       (   TsL = [] ->
           mysetof(Ts2,(member(Ts2,CL),shares_var(T,Ts2)),TsL1),
           H_int =.. [t_int,T],
           kb:body2list(B_int,TsL1),
           compare_types(Type1,(H_int:-B_int),Type)
       ;   type_of1(TsL,T,Type2),
           compare_types(Type1,Type2,Type)
       )
   ).



%**********************************************************************************
%*
%*  predicate: types_of/3
%* 
%*  syntax: types_of(+Varlist,+C,-Typelist)
%*
%*  args: 
%*
%*  description: like type_of for each Var in Varlist.
%*               Varlist = [V1,..,Vn] => Typelist = [V1:T1,..,Vn:Tn]
%*
%*  example:
%*
%*  peculiarities: 
%*									
%**********************************************************************************

types_of([],_,[]).
types_of([V|R],C,[V:T|R1]):-
   type_of(V,C,T),
   types_of(R,C,R1).


%***********************************************************************
%*									
%* predicate:   compare_types/3
%*									
%* syntax:	compare_types(+Type1,+Type2,-Type)
%*									
%* args:	Type1,Type2: types to be compared
%*		Type: the most specific type among type1 and typ2
%*									
%* description:	returns the more specific type
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

compare_types(Type1,Type2,Type):-
   (   type_sub(Type1,Type2) ->
       Type = Type2
   ;   (   type_sub(Type2,Type1) ->
           Type = Type1
       ;   fail
       )
   ).

%***********************************************************************
%*									
%* predicate:  define_type/0 
%*									
%* syntax:	
%*									
%* args:	
%*									
%* description:	allows to define a type restriction for a predicate
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

define_type:-
   show_kb_types,
   read_type_restriction.

read_type_restriction:-
   repeat,
   nl, write('Please enter name and arity of the predicate p/n: '),
   (   (read(P/N),atom(P),integer(N)) ->
       functor(F,P,N),F =.. [P|Args],
       read_type_restriction(Args,1,Alist),
       assert(type_restriction(F,Alist))
   ;   fail
   ).

read_type_restriction([],_,[]).
read_type_restriction([V|R],N,[T|R1]):-
   repeat,
   nl, write('Please enter the type at argument position '),write(N),write(' : '),
   (   (read(TN),atom(TN)) ->
       (  ((H =.. [TN,_], get_clause(_,H,_,_,type));
           member(TN,[atom,number,atomic])) ->
          T =.. [TN,V]
       ;  read_type_definition(TN),
          T =.. [TN,V]
       )
   ;   fail
   ),
   N1 is N + 1,
   read_type_restriction(R,N1,R1).


%***********************************************************************
%*									
%* predicate: read_type_definition/1  
%*									
%* syntax: read_type_definition(+TN)	
%*									
%* args: TN.. name of a type	
%*									
%* description:	allows to enter clauses defining the type TN
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

read_type_definition(TN):-
   nl, write('Type '),write(TN), write(' is undefined. Enter a definition (y/n)? '),
   read(A),
   (   A == y ->
       read_type_def(TN)
   ;   (   A == n ->
           fail
       ;   nl, write('Please enter y or n'),
           read_type_definition(TN)
       )
   ).

read_type_def(TN):-
   nl, write('Please enter the definition of '),write(TN),
   write(' in clausal form. Stop by entering stop.'),nl,
   repeat,
   read(A),
   (   ((A = (H:-_), H =.. [_,_]); (A =.. [_,_])) ->
       store_clause(A,_,type,_),nl,
       fail
   ;   (   A == stop ->
           true
       ;   nl, write('Please enter a clause or stop'),fail
       )
   ).

%***********************************************************************
%*									
%* predicate: verify_types/0  
%*									
%* syntax:	
%*									
%* args:	
%*									
%* description:	checks for the type restrictions in the kb whether
%*              the contain only defined or built-in types. If not,
%*              the user is asked for replacing or defining the unknown
%*              types.
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

verify_types:-
   findall((M,A),type_restriction(M,A),TSet),
   verify_types(TSet).

verify_types([]).
verify_types([(M,A)|R]):-
   verify_types(R),
   verify_types(A,M,A).

verify_types([],_,_).
verify_types([H|R],M,A):-
   verify_types(R,M,A),
   H =.. [T|_],
   findall((H1,B1),(H1 =.. [T,_],known(ID,H1,B1,CL,_,E),
                    delete_clause(ID),
                    assertz(kb:known(ID,H1,B1,CL,type,E))),Tlist),
   (   (Tlist \== [];member(T,[atom,number,atomic])) ->
       true
   ;   nl, write('The type '),write(T),write(' is undefined in '),
       copy_term((M,A),(M1,A1)),numbervars((M1,A1),0,_),
       write(type_restriction(M1,A1)),nl,
       show_kb_types,
       repeat,
       nl, write('Do you want to replace '), write(T), write(' in '),
       write(type_restriction(M1,A1)),write(' (y/n)?'),nl,
       read(An),
       (   An == y ->
           retract(type_restriction(M,A)),
           repeat,
           nl, write('Enter the name of the type replacing '), write(T), write(': '),
           (   (read(T1),atom(T1)) ->
               H1 =.. [T1,_],
               (    get_clause(_,H1,_,_,_) ->
                    vrt(A,T,T1,A2),
                    assert(type_restriction(M,A2))
               ;    nl, write('The type '), write(T1),write(' is undefined.'),
                    vrt1(T1)
               )
           ;   fail
           )
       ;   (   An == n ->
               nl,write('Then you have to define '),write(T),
               read_type_def(T)
           ;   write('Please enter y or n'),fail
           )
       ) ).

vrt([],_,_,[]).
vrt([H|R],T,T1,[H1|R1]):-
   vrt(R,T,T1,R1),
   H =.. [T2,V],
   (   T2 == T ->
       H1 =.. [T1,V]
   ;   H1 = H
   ).


vrt1(T1):-
   nl, write('Do you want to define it (y/n)? '),
   read(Bn),
   (   Bn == y ->
       read_type_def(T1)
   ;   (   Bn == n ->
           fail
       ;   write('Please enter y or n'),
           vrt1(T1)
       )
   ).
                            
                   
