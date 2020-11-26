% MODULE div_utils  EXPORTS

:- module(div_utils,
        [ remove/3,
          sort_by_length/3,
          mysetof/3,
          sum/2,
          efface/3,
          buildpar2/3,
          neg/2,
          contains_duplicates/1,
          contains_identicals/1,
          identical_member/2,
          convert_to_horn_clause/3,
          extract_body/2,
          list_to_struct/2,
          clist_to_prolog/2,
          append_all/2,
          maximum/2,
          myforall/2,
          identical_make_unique/2,
          remove_v/3,
          remove_variant/3,
          make_unique/2,
          variant_mem/2,
          different_predicates/2,
          nth_arg/3,
          split_examples/4,
          shares_var/2,
          body2list/2,
          insert_unique/3,
          insert_unique/4,
          effaceall/3,
          genterm_test/2,
          subset_chk/2,
          best/2,
          subterm_at_position/4,
          part_of_clause/2,
          fak/2,
          fak1/3,
          nueberk/3,
          log2/2,
          log2nueberk/3,
          sum_of_logs/3]).


% METAPREDICATES
:- meta_predicate(mysetof(+,:,-)).


% IMPORTS

:- use_module_if_exists(library(basics),
              [member/2]).
:- use_module_if_exists(library(subsumes),
                      [variant/2]).
:- use_module_if_exists(library(occurs),
                      [sub_term/2,contains_var/2]).
:- use_module_if_exists(library(lists),
              [rev/2]).
:- use_module_if_exists(library(math),
              [log/2]).

%***********************************************************************
%*	
%* module: div_utils.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92	
%*									
%* changed:								
%*									
%* description:	small auxiliary procedures
%*		
%* see also:								
%*									
%***********************************************************************


%***********************************************************************
%*									
%* predicate:	remove/3								
%*									
%* syntax: remove(+I,+L,-L)								
%*									
%* args: I .. number, L ... list of numbers
%*									
%* description:	removes I from L (I occurs at most once in L)
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

remove(_,[],[]).
remove(I,[I|R],R):- !.
remove(I,[J|R],[J|R1]):-
   remove(I,R,R1).

%***********************************************************************
%*									
%* predicate:	sort_by_length/3							
%*									
%* syntax: sort_by_length(+L,+Accu,-Accu)
%*									
%* args: L ... list of lists
%*       Accu ... L sorted increasingly according to the length of sublists
%*									
%* description:	sorts a list of lists increasingly according to 
%*              the length of sublists							
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

sort_by_length([],L,L).
sort_by_length([IXS|R],L,L2):-
   insert_by_length(IXS,L,L1),
   sort_by_length(R,L1,L2).

insert_by_length(X,[Y|R],[Y|R1]):-
   length(X,N),length(Y,N1),
   N > N1,!,
   insert_by_length(X,R,R1).
insert_by_length(X,L,[X|L]).


%***********************************************************************
%*									
%* predicate:	mysetof/3							
%*									
%* syntax: mysetof(+Template,+Generator,-Set)
%*									
%* args: 								
%*									
%* description:	as setof/3, but succeeds with Set = [], if Generator
%*		fails							
%*									
%* example: setof(X, append([1,2,3],X,[4,5]),Set) -> fail
%*          mysetof(X, append([1,2,3],X,[4,5]),Set) -> Set = []
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

mysetof(A,B,C):- setof(A,B,C),!.
mysetof(_,_,[]).


%***********************************************************************
%*									
%* predicate:	sum/2							
%*									
%* syntax: sum(+LN,-S)								
%*									
%* args: LN .. list of numbers, S number
%*									
%* description:	if LN = [I1,..,In], then S = I1 + ... + In
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

sum([I],I).
sum([I|More],C):- sum(More,J), C is I + J.


%***********************************************************************
%*									
%* predicate:	efface/3 (by Clocksin/Mellish)						
%*									
%* syntax: efface(+E,+L,-L)								
%*									
%* args: E .. element of list L								
%*									
%* description:	removes the first element of L that is unifiable with E
%*              from L.							
%*
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

efface(A, [A|L], L) :- !.
efface(A, [B|L], [B|M]) :- efface(A, L, M).


%***********************************************************************
%*									
%* predicate: effaceall/3	
%*									
%* syntax: effacell(+E,+L,-L)								
%*									
%* args: E .. element of list L								
%*									
%* description:	as efface, but allows backtracking
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

effaceall(A, [A|L], L).
effaceall(A, [B|L], [B|M]) :- effaceall(A, L, M).


%***********************************************************************
%*									
%* predicate:	best/2							
%*									
%* syntax: best(+List,-Elem)								
%*									
%* args:								
%*									
%* description:	returns the first element of List, on backtracking
%*              the second etc.
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

best([X|_],X).
best([_|R],X):- best(R,X).


%***********************************************************************
%*									
%* predicate:	buildpar2/3							
%*									
%* syntax: buildpar2(+Lit:M,+CL,-CL1)
%*									
%* args: Lit .. literal, M in {p,n,r}, CL and CL1 clauses in list representation
%*									
%* description:	if M = p then CL1 = [Lit:p|CL] 
%*		else CL1 results from CL by adding Lit:M at the end
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:	
%*
%***********************************************************************

buildpar2(Elem1:p,List2,[Elem1:p|List2]).
buildpar2(ResLit,List2,Parent2) :- append(List2,[ResLit],Parent2).


%************************************************************************
%*
%* predicate: neg/2
%*
%* syntax: neg(+Lit:M,-Lit:M1)
%*
%* args: Lit .... literal, M in {p,n,r}
%*
%* description: switches the mark of the literal, i.e. if M = p then
%*              M1 = n and vice versa
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

neg(F:p, F:n).
neg(F:n, F:p).


%***********************************************************************
%*									
%* predicate:	contains_duplicates/1							
%*									
%* syntax: contains_duplicates(+L)
%*									
%* args: L ... list								
%*									
%* description:	succeeds if L contains two unifiable elements
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

contains_duplicates([H|T]):- member(H,T).
contains_duplicates([_|T]):- contains_duplicates(T).



%***********************************************************************
%*									
%* predicate:	contains_identicals/1							
%*									
%* syntax: contains_identicals(+L)
%*									
%* args: L ... list								
%*									
%* description:	succeeds if L contains two identical (==) elements
%*									
%* example:								
%*									
%***********************************************************************

contains_identicals([H|T]):- contains_var(H,T).
contains_identicals([_|T]):- contains_identicals(T).



%***********************************************************************
%*									
%* predicate: identical_member/2							
%*									
%* syntax: identical_member(+Elem,+List)   
%*									
%* args:  								
%*									
%* description: succeeds if Elem is identically (==) contained  in List	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

identical_member(A,[A1|_]):- A == A1.    
identical_member(A,[_|R]):- identical_member(A,R). 


%***********************************************************************
%*									
%* predicate:	convert_to_horn_clause/3
%*									
%* syntax: convert_to_horn_clause(+PHead,+CL,-HCL)
%*									
%* args: PHead ... preferred head
%*       CL ... general clause in list representation
%*       HCL ... horn clause in list representation
%*									
%* description:	if CL = [H1:p,..,Hn:p,L1:M1,..,Lm:Mm] where Mi in {p,r}
%*              then HCL = [Hj:p,L1:M1,...,Lm:Mm], where Hj is the first
%*                   head in CL unifiable with PHead (if one exists), else
%*                   the first head in CL
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

convert_to_horn_clause(PrefHead,GenClause,HornClause):-
      extract_body(GenClause,Body),      
      !,
      ( member( PrefHead:p, GenClause) -> Head = PrefHead   % if preferred head is among 
      ;                                                     % candidates, select it.
        member( Head:p, GenClause)                          % Else select first candidate.
      ),
      HornClause = [ Head:p | Body ].



%***********************************************************************
%*									
%* predicate:	extract_body/2							
%*									
%* syntax: extract_body(+CL,-CL1)
%*									
%* args: CL .. clause in list representation
%*       CL1 = [...,L:M,...] where M in {p,n} and L in CL
%*									
%* description:	
%*									
%* example:								
%*									
%* peculiarities:			
%*									
%* see also:								
%*									
%***********************************************************************


extract_body([],[]).
extract_body([L:n| Rest], [L:n|Rest1]):- extract_body(Rest,Rest1).
extract_body([L:r| Rest], [L:r|Rest1]):- extract_body(Rest,Rest1).
extract_body([_:p| Rest], Rest1):- extract_body(Rest,Rest1).



%***********************************************************************
%*									
%* predicate: 	list_to_struct/2							
%*									
%* syntax: list_to_struct(+L,-C)
%*									
%* args: L ... list, C ... conjunction of elements of L
%*									
%* description:	if L = [E1,...,En] then C = (E1,..,En)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

list_to_struct([A,B|Rest],(A,Rest1) ):- list_to_struct([B|Rest],Rest1).
list_to_struct([A],A).
list_to_struct([],true).


%***********************************************************************
%*									
%* predicate: clist_to_prolog/2								
%*									
%* syntax: clist_to_prolog(+CL,-C)
%*									
%* args: CL .. Horn clause in list representation
%*       C ... Horn clause in prolog format
%*									
%* description:	convert list format to clause format and vice versa
%*              (should use body2list!!)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

clist_to_prolog([A:p,B|Rest],(A:-Rest1) ):- !,clist_to_prolog([B|Rest],Rest1).
clist_to_prolog([A:p],(A:-true)):-!.
clist_to_prolog([A:n,B|Rest],(A,Rest1) ):- !, clist_to_prolog([B|Rest],Rest1).
clist_to_prolog([A:n],A):-!.
clist_to_prolog([A:r,B|Rest],(A,Rest1) ):- clist_to_prolog([B|Rest],Rest1).
clist_to_prolog([A:r],A).


%***********************************************************************
%*									
%* predicate:	append_all/2							
%*									
%* syntax: append_all(+LL,-L)								
%*									
%* args: LL .. list of lists, L .. list
%*									
%* description:	appends all lists in LL -> L
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

append_all([],[]).
append_all([P|R],R2):-
    append_all(R,R1),
    append(P,R1,R2).



%***********************************************************************
%*									
%* predicate:	maximum/2							
%*									
%* syntax: maximum(+L,-M)								
%*									
%* args: L .. list of numbers, M number
%*									
%* description:	M is the maximum element of L
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

maximum([I],I).

maximum([I|Rest], I):-
        maximum(Rest,J),
        I >= J,!.

maximum([_|Rest],J):-
        maximum(Rest,J),!. 

									
%***********************************************************************
%*									
%* predicate:	myforall/2							
%*									
%* syntax: myforall(+E,+Pred)								
%*									
%* args: E ... argument terms, Pred .. type predicate
%*									
%* description:	calls Pred(e) for each e in E, and succeeds only if 
%*              every call succeeds
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

myforall([],_).
myforall([E|R],Pred):-
   C =.. [Pred,E],
   call(C),
   myforall(R,Pred).


%***********************************************************************
%*									
%* predicate:	identical_make_unique/2	
%*									
%* syntax: identical_make_unique(+L,-L1)
%*									
%* args: L,L1 ... lists								
%*									
%* description: removes all identical duplicates (==) from L
%*
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

identical_make_unique([],[]).
identical_make_unique([X|R],R1):-
   contains_var(X,R),!,
   identical_make_unique(R,R1).   
identical_make_unique([X|R],[X|R1]):-
   identical_make_unique(R,R1). 


%***********************************************************************
%*									
%* predicate: remove_v/3								
%*									
%* syntax: remove_v(+L0,+L,-L1)
%*									
%* args: L0,L,L1 lists								
%*									
%* description:	removes each E in L0  from L if E is identically (==) 
%*              contained  in  L							
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

remove_v(_,[],[]).
remove_v(T,[T1|R],R1):- identical_member(T1,T),!,remove_v(T,R,R1).
remove_v(T,[T1|R],[T1|R1]):- remove_v(T,R,R1).



%***********************************************************************
%*									
%* predicate: remove_variant/3								
%*									
%* syntax: remove_variant(+L0,+L,-L1)
%*									
%* args: L0,L,L1 lists								
%*									
%* description:	removes each E in L0  from L if E is 
%*              contained as variant in  L
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

remove_variant(_,[],[]).
remove_variant(T,[T1|R],R1):- variant_mem(T1,T),!,remove_variant(T,R,R1).
remove_variant(T,[T1|R],[T1|R1]):- remove_variant(T,R,R1).


%***********************************************************************
%*									
%* predicate: make_unique/2								
%*									
%* syntax: make_unique(+L,-L1)								
%*									
%* args: L,L1 .. lists								
%*									
%* description:	removes all duplicates (variant) from L
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

make_unique([],[]).
make_unique([X|R],R1):-
   variant_mem(X,R),!,
   make_unique(R,R1).
make_unique([X|R],[X|R1]):-
   make_unique(R,R1).


%***********************************************************************
%*									
%* predicate: variant_mem/2
%*									
%* syntax: variant_mem(+Elem,+List)
%*									
%* args:								
%*									
%* description:	succeeds if an alphabetical variant of Elem is
%*              contained in List							
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

variant_mem(T,[T1|_]):- variant(T,T1),!.
variant_mem(T,[_|R]):- variant_mem(T,R).


%***********************************************************************
%*									
%* predicate: different_predicates/2
%*									
%* syntax: different_predicates(+L,-LL)
%*									
%* args: L .. list of terms, LL list of lists of terms
%*									
%* description:	for each functor f/n occuring in L, LL contains a list Lf 
%*      consisting of all terms in L with principal functor f/n	
%*									
%* example: L = [f(a,b),f(c,d),h(g)] LL = [[f(a,b),f(c,d)],[h(g)]]
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

different_predicates([],[]).
different_predicates([E|R],[[E|Es]|R2]):-
   functor(E,F,N),
   diff_predicates(R,R1,Es,F,N),
   different_predicates(R1,R2).

diff_predicates([],[],[],_,_).
diff_predicates([E|R],R2,Es2,_,0):- !,
   diff_predicates(R,R1,Es1,_,0),
   (   functor(E,_,0) ->
       R2 = R1, Es2 = [E|Es1]
   ;   R2 = [E|R1], Es2 = Es1
   ).
diff_predicates([E|R],R2,Es2,F,N):-
   diff_predicates(R,R1,Es1,F,N),
   (   functor(E,F,N) ->
       R2 = R1, Es2 = [E|Es1]
   ;   R2 = [E|R1], Es2 = Es1
   ).


%***********************************************************************
%*									
%* predicate: nth_arg/3								
%*									
%* syntax: nth_arg(+E,+N,-Args)								
%*									
%* args: E ... list of terms with principal functor p/n
%*       N =< n	argument position
%*       Args ... list of argument terms
%*									
%* description:	Args = {A | arg(N,P,A) and P in E}
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

nth_arg([],_,[]).
nth_arg([F|R],N,[Argn|R1]):-
   arg(N,F,Argn),
   nth_arg(R,N,R1).


%***********************************************************************
%*									
%* predicate: split_examples/4								
%*									
%* syntax: split_examples(+E,+Term,-P,-N)
%*									
%* args: E,P,N ... list of terms
%*									
%* description: P = {e in E | Term, e unifiable}
%*              N = E - P								
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

split_examples([E1|R],Lgg,P,[E1|M1]):-
   \+(E1 = Lgg),
   split_examples(R,Lgg,P,M1),!.

split_examples([E1|R],Lgg,[E1|P],M1):-
  split_examples(R,Lgg,P,M1),!.

split_examples([],_,[],[]):- !.


%***********************************************************************
%*									
%* predicate:	shares_var/2							
%*									
%* syntax:	shares_var(+T,+Ts)						
%*									
%* args:	T: a term or a clause,Ts: a list of terms or clauses
%*									
%* description:	tests if T shares at least one variable
%*		with the terms in t						
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

shares_var(T,Ts):-
   sub_term(V,T), var(V), contains_var(V,Ts).



%***********************************************************************
%*									
%* predicate:	body2list/2							
%*									
%* syntax:	body2list(?B,?BList)
%*									
%* args:	B: Body of a clause (L1,...,Ln)	
%*		BList: [L1:x,...,Ln:x] where x is in {r,n}
%*									
%* description:	transforms a clause body to a list of its literals
%*		where each literal is augmented by :n (i.e. negative clause literal)
%*		or :r (i.e. recursive goal in the clause body).
%*              works in both directions
%*									
%* example:	(p(x,y),q(z,w)),[(p(x,y):r,q(z,w):r]
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

body2list(B, [L1:n|RestL]) :-
        functor(B,',',2),
        arg(1,B,L1),arg(2,B,RestB),
        body2list(RestB, RestL).
body2list(B, [B:n]) :- !.
body2list(B, [L1:r|RestL]) :-
        functor(B,',',2),
        arg(1,B,L1),arg(2,B,RestB),
        body2list(RestB, RestL).
body2list(B, [B:r]) :- !.



%***********************************************************************
%*									
%* predicate:	insert_unique/3						
%*									
%* syntax: insert_unique(+N,+L,-L1)
%*									
%* args: N .. number, L,L1 sorted lists of numbers
%*									
%* description:	inserts N uniquely in the ascendingly sorted list L
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

insert_unique(I,[I|R],[I|R]):- !.
insert_unique(I,[J|R],[J|R1]):-
   I > J,!,
   insert_unique(I,R,R1).
insert_unique(I,L,[I|L]).



%***********************************************************************
%*									
%* predicate:	insert_unique/4							
%*									
%* syntax: insert_unique(+ID,+A,+L,-L1)
%*									
%* args: ID,A .. numbers, L,L1 = [...,ID:List,...]
%*									
%* description:	inserts A in the sublist identified by ID in L
%*									
%* example: insert_unique(2,5,[1:[5,6],2:[4],3:[9,8]],
%*                            [1:[5,6],2:[5,4],3:[9,8]])
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

insert_unique(I,A,[],[I:[A]]):- !.
insert_unique(I,A,[I:A1|R],[I:[A|A1]|R]):- !.
insert_unique(I,A,[J|R],[J|R1]):-
   insert_unique(I,A,R,R1).


%***********************************************************************
%*									
%* predicate: genterm_test/3	
%*									
%* syntax: genterm_test(+X/T,+Subst)
%*									
%* args: X/T element of a substitution, Subst substitution
%*									
%* description: succeeds if Subst contains a tuple Y/T1 such that
%*      T1 == T. In that case, X and Y are unified.
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

genterm_test(X/T1, [X/T2|_]) :-
        T1 == T2, !.
genterm_test(S, [_|Rest]) :-
        genterm_test(S, Rest).



%***********************************************************************
%*									
%* predicate: subset_chk/2
%*									
%* syntax: subset_chk(+L,+L1)	
%*									
%* args: L, L1 .. lists 								
%*									
%* description:	succeeds, if L is a subset of L1 (without unification)
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************


% subset check uses library 'basics'
subset_chk([],_) :- !.
subset_chk([Elem1|Rest1], List2) :-
        identical_member(Elem1, List2),!,
        subset_chk(Rest1, List2).


%***********************************************************************
%*									
%* predicate:	subterm_at_position/4							
%*									
%* syntax: subterm_at_position(+Term,-Sub,+Pos,-Pos)
%*									
%* args: Term, Sub: Prolog terms
%*       Pos: position of Sub within Term (a list of numbers)
%*									
%* description:	returns a subterm of Term and its position, on backtracking
%*              further subterms
%*									
%* example: ?- subterm_at_position(p(a,[a]),S,[],P).
%*          S = p(a,[a]), P = [];
%*          S = a         P = [1];
%*          S = [a]       P = [2];
%*          S = a         P = [2,1];
%*          S = []        P = [2,2]							
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************
   
subterm_at_position(T,T,P,P1):- rev(P,P1).
subterm_at_position(T,S,P,P1):-
   nonvar(T),
   functor(T,_,N),N > 0,
   subterm_at_position(N,T,S,P,P1).

subterm_at_position(N,T,S,P,P1):-
   N > 0,
   arg(N,T,Tn),
   subterm_at_position(Tn,S,[N|P],P1).
subterm_at_position(N,T,S,P,P1):-
   N > 0,N1 is N - 1,
   subterm_at_position(N1,T,S,P,P1).


%***********************************************************************
%*									
%* predicate: part_of_clause/2	
%*									
%* syntax: part_of_clause(+Term,+Clause)						
%*									
%* args: Term: a prolog term, Clause: a prolog clause
%*									
%* description:	succeeds if Term is a literal within clause, a part
%*              of the clause body or the clause itself
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

part_of_clause(S,B):- 
   S == B.
part_of_clause(S,(H:-B)):- !,
   (   (S == H;S == B) ->
       true
   ;   part_of_clause(S,B)
   ).
part_of_clause(S,(H,B)):- !,
   (   (S == H;S == B) ->
       true
   ;   part_of_clause(S,B)
   ).



%***********************************************************************
%*									
%* predicate: several arithmetic predicates	
%*									
%* syntax: 
%*									
%* args: 
%*									
%* description:	arithmetic predicates used in heuristic measures
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************


fak(X,1):- X =:= 0,!.
fak(N,NF):-
   N1 is N - 1,
   fak(N1,N1F),
   NF is N1F * N.

fak1(N,N,1):- !.
fak1(A,B,C):-
   A1 is A + 1,
   fak1(A1,B,C1),
   C is C1 * A1.

nueberk(N,K,NUK):-
   NK is N - K,
   fak1(NK,N,NKF),
   fak(K,KF),
   NUK is NKF / KF.

log2(X,LX):-
   log(X,LNX),
   log(2,LN2),
   LX is LNX / LN2.

log2nueberk(_,0.0,0.0):- !.
log2nueberk(N,1.0,LN):- log2(N,LN),!.
log2nueberk(N,N,0.0):- !.
log2nueberk(N,K,L):-
   N1 is (N - K) + 1,
   sum_of_logs(N1,N,L1),
   sum_of_logs(1.0,K,L2),
   L is L1 - L2.

sum_of_logs(O,O,LO):- log2(O,LO),!.
sum_of_logs(U,O,L):-
   U < O,!,
   U1 is U + 1,
   sum_of_logs(U1,O,L1),
   log2(U,LU),
   L is L1 + LU.
sum_of_logs(U,O,_):-
   U > O,!,fail.