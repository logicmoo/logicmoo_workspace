% MODULE clause_heads EXPORTS

:- module( clause_heads,
           [clause_heads/0, heads/1, heads/2, heads/3]
         ).

%IMPORTS
:- use_module(home(div_utils),
                   [mysetof/3, different_predicates/2, make_unique/2,
                    variant_mem/2, split_examples/4, insert_unique/3,
                    remove_v/3]).
:- use_module(home(var_utils),
              [terms/4]).
:- use_module(home(interpreter),
                   [proof_path/4]).
:- use_module(home(kb),
                   [get_example/3, get_clause/5, store_clauses/2,delete_clause/1]).
:- use_module(home(argument_types),
                   [type_restriction/2]).
:- use_module(home(lgg),
                   [set_lgg/2]).
:- use_module(home(evaluation),
                   [eval_examples/0]).
:- use_module_if_exists(library(subsumes),
                      [subsumes_chk/2]).
:- use_module_if_exists(library(basics),
                      [member/2]).
:- use_module_if_exists(library(lists),
                      [rev/2]).

% METAPREDICATES
% none

%************************************************************************
%* 
%* module: clause_heads.pl
%*
%* author:      Irene Stahl      date:  13. 10. 1992
%*            
%* changed:   
%*             
%* description: algorithm for determining clause heads
%*              generates database entrys of the form 
%*              known(ID,Head,true,CList,head,_)
%*              
%* see also:    
%*                            
%************************************************************************



%************************************************************************
%*
%* predicate: clause_heads/0
%*
%* syntax:
%*
%* args:
%*
%* description: determines clause heads covering all positive examples in
%*              the kb and asserts them in the kb
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

clause_heads:-
   mysetof(E,I^get_example(I,E,'+'),Elist), % Elist = [E1,..,En] pos examples
   different_predicates(Elist,Elist1), % Elist1 = [[E1,..,Em],...]
                                       % list of lists of pos examples with
                                       % the same predicate symbol
   clause_heads(Elist1).


%************************************************************************
%*
%* predicate: heads/1
%*
%* syntax: heads(-HL)
%*
%* args: HL list of clause heads
%*
%* description: returns list of heads covering all positive examples in
%*              the kb
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

heads(HL):-
   clause_heads,
   mysetof(Head,
           ID^Body^CL^(get_clause(ID,Head,Body,CL,head),delete_clause(ID)),
           HL).


%************************************************************************
%*
%* predicate: heads/2
%*
%* syntax: heads(+Pred,+Arity)
%*
%* args: Pred .. predicate symbol (atom), Arity.. an integer
%*
%* description: determines clause heads covering all positive examples for 
%*              Pred/Arity and asserts them in the kb
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

heads(P,N):-
   functor(E,P,N),
   mysetof(E,I^get_example(I,E,'+'),Elist),
   clause_heads([Elist]).

%************************************************************************
%*
%* predicate: heads/3
%*
%* syntax: heads(+Pred,+Arity,-HL)
%*
%* args: Pred .. predicate symbol, Arity .. integer, HL .. list of heads
%*
%* description: returns list of heads covering all positive examples for
%*              Pred/Arity
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

heads(P,N,HL):-
   functor(E,P,N),
   mysetof(E,I^get_example(I,E,'+'),Elist),
   clause_heads([Elist]),
   functor(Head,P,N),
   mysetof(Head,
           ID^Body^CL^(get_clause(ID,Head,Body,CL,head),delete_clause(ID)),
           HL).

%************************************************************************
%*
%* predicate: clause_heads/1
%*
%* syntax: clause_heads(+ELL)
%*
%* args: ELL = [[E1,..,Em],...] list of lists of pos examples with the
%*       same predicate symbol
%*
%* description: determines for each [E1,..,Em] in ELL clause heads
%*              and asserts them in the knowledge base
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

clause_heads([]).
clause_heads([EL|R]):-
   clause_heads(R),
   clause_heads(EL,Heads),
   make_unique(Heads,Heads1),
   minimize_heads(Heads1,EL,Heads2),
   store_clauses(Heads2,head),
   eval_examples.
   

%************************************************************************
%*
%* predicate: clause_heads/2
%*
%* syntax: clause_heads(+EL,-Heads)
%*
%* args: +EL = [E1,...,Em] positive examples of a predicate p/n
%*       Heads = [H1,..,Hk] heads for p/n covering EL
%*
%* description: determines heads for p/n by determining base heads
%*              and heads for non-base examples according to the 
%*              differing types
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

clause_heads([E|R],Heads):-
   functor(E,P,N),functor(P1,P,N),
   (   type_restriction(P1,Types) ->
       true
   ;   P1 =.. [_|P1A],
       trivial_tr(P1A,Types)
   ),
   bases(N,[E|R],P1,Types,B),
   remove_base_examples(B,[E|R],E1), 
   split_examples_by_types(E1,P1,Types,Hlist),
   make_unique(Hlist,Hlist1),
   best_lgg(Hlist1,[E|R],B,Heads).

trivial_tr([],[]).
trivial_tr([X|R],[T|R1]):-
   trivial_tr(R,R1),
   T =.. [all,X].


%************************************************************************
%*
%* predicate: bases/5
%*
%* syntax: bases(+Count,+E,+P,+Type,-B)
%*
%* args: N .. counter
%*       E .. positive examples for p/n
%*       P, Type .. type restriction of the target predicate p/n
%*       B .. base heads for p/n 
%*
%* description: for each argument position N,
%*                 for each base case at b at that position,
%*                    add lgg({p(..,b,..)|p(..,b,..) in E}) to B
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

bases(0,_,_,_,[]).
bases(N,E,P,Type,B):-
   N1 is N - 1,
   bases(N1,E,P,Type,B1),
   copy_term((P,Type),(P1,Type1)),
   arg(N,P1,P1n),
   member(T,Type1),T =.. [_,X], X == P1n,
   mysetof(Base,I^CL^R^(get_clause(I,T,true,CL,type), T =.. [R,Base]), Bases),
   bases1(Bases,N,E,B1,B).

bases1([],_,_,B,B).
bases1([B|R],N,E,B1,[H|B2]):-
   bases1(R,N,E,B1,B2),
   bases2(E,N,B,Eb),
   set_lgg(Eb,H).

bases2([],_,_,[]).
bases2([E|R],N,B,[E|R1]):-
   arg(N,E,B),!,
   bases2(R,N,B,R1).
bases2([_|R],N,B,R1):-
   bases2(R,N,B,R1).


%************************************************************************
%*
%* predicate: split_examples_by_types/4
%*
%* syntax: split_examples_by_types(+E,+P,+Type,-Heads)
%*
%* args: E ... examles for p/n (without base examples)
%*       P,Type ... type restriction for p/n
%*       Heads ... list [..., H:terms(H),...] of heads for p/n according 
%*                 to different types
%*
%* description: splits examples E according to different argument types
%*        -> ELL list of example lists. For each EL in ELL, lgg(EL) is
%*        added to heads
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

split_examples_by_types(E,P,Types,Heads):-
   split_examples_by_types(Types,P,E,[],Elist),
   construct_heads(Elist,Heads).

split_examples_by_types([],_,_,EL,EL).
split_examples_by_types([T|R],P,E,EL,EL3):-
   split_examples_by_types(R,P,E,EL,EL1),
   mysetof((Ex,Ts),(member(Ex,E),proof_path(Ex,P,T,Ts)),Elist0),
   split_example_list(Elist0,EL2),
   append(EL1,EL2,EL3).


split_example_list([],[]).
split_example_list([(E,Ts)|R],[[E|EL]|R1]):-
   split_elist(R,Ts,EL,R0),
   split_example_list(R0,R1).

split_elist([],_,[],[]).
split_elist([(E,Ts)|R],Ts,[E|R1],R2):-
   split_elist(R,Ts,R1,R2).
split_elist([E|R],Ts,R1,[E|R2]):-
   split_elist(R,Ts,R1,R2).


%************************************************************************
%*
%* predicate: construct_heads/2
%*
%* syntax: construct_heads(+ELL,-Heads)
%*
%* args: ELL ... list of lists of examples
%*       Heads ... list [...,H:terms(H),...] of heads
%*
%* description: for each EL in ELL set H:= lgg(EL), terms(H) terms of H
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

construct_heads([EL|R],[H:Vars|R1]):-
   set_lgg(EL,H),
   functor(H,_,N),
   terms(N,H,[],Vars),
   construct_heads(R,R1).
construct_heads([],[]).


%************************************************************************
%*
%* predicate: best_lgg/4
%*
%* syntax: best_lgg(+ToRefine,+E,+Heads,-Heads)
%*
%* args: ToRefine ... list [...,H:terms(H),...] of heads
%*       E ... examples 
%*       Heads ... resulting heads [...,H,...]
%*
%* description: while ToRefine \= [], 
%*                 add first element H to Heads and
%*                 compute all refinements of H that result from unifying 
%*                 terms within H. Add the refinements to ToRefine.
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

best_lgg([],_,HL,HL).
best_lgg([H:Vars|R],E,HL,HL1):-
   (   variant_mem(H,HL) ->
       best_lgg(R,E,HL,HL1)
   ;   try_to_unify(H,Vars,Vars,E,[],Lp),
       append(Lp,R,R1),
       best_lgg(R1,E,[H|HL],HL1)
   ).


%************************************************************************
%*
%* predicate: try_to_unify/6
%*
%* syntax: try_to_unify(+H,+Terms,+Terms,+E,+Result,-Result)
%*
%* args: H .. head that is to be refined
%*       Terms ... terms(H)
%*       E ... examples
%*       Result ... list [...,H1:terms(H1),...] of refined heads
%*
%* description: for each pair X,Y (X \== Y) in terms(H)
%*                if H[X/Y] covers examples E' in E
%*                then add H1:terms(H1) to result where H1 = lgg(E')
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

try_to_unify(_,[],_,_,L,L).
try_to_unify(H,[X|R],V,E,L,L2):-
   unify_vars(H,X,V,E,L,L1),
   try_to_unify(H,R,V,E,L1,L2).

unify_vars(_,_,[],_,L,L).
unify_vars(H,X,[Y|R],E,L,L2):-
   copy_term((H,X,Y),(H1,X1,Y1)),
   unify_var(H1,X1,Y1,E,L,L1),
   unify_vars(H,X,R,E,L1,L2).

unify_var(_,X,Y,_,L,L):- X == Y,!.
unify_var(H,X,X,E,L0,L1):- !,
   split_examples(E,H,Pos,_),
   (   Pos \== [] ->
       set_lgg(Pos,H1),
       functor(H1,_,N),
       terms(N,H1,[],Vars1),
%       (   Vars1 == [] ->
%           L1 = L0
%       ;   
            L1 = [H1:Vars1|L0]
%       )
   ;   L1 = L0
   ).
unify_var(_,_,_,_,L,L).


%************************************************************************
%*
%* predicate: remove_base_example/3
%*
%* syntax: remove_base_example(+BHeads,+E,-E)
%*
%* args: BHeads ... base heads
%*       E ... examples
%*
%* description: removes all examples covered by base heads in BHeads from E
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

remove_base_examples(B,[E|R],R1):-
   is_base_example(E,B),!,
   remove_base_examples(B,R,R1).
remove_base_examples(B,[E|R],[E|R1]):-
   remove_base_examples(B,R,R1).
remove_base_examples(_,[],[]).


is_base_example(E,[B|_]):-
   subsumes_chk(B,E),!.
is_base_example(E,[_|R]):-
   is_base_example(E,R).


%************************************************************************
%*
%* predicate: minimize_heads/3
%*
%* syntax: minimize_heads(+Heads,+Examples,-Heads)
%*
%* args: Heads.. list of clause heads
%*       Examples... positive examples to be covered by Heads
%*
%* description: minimizes the set of clause heads by first removing general 
%*              redundant heads, then specific redundant heads.
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

minimize_heads(H,EL,H4):-
   sort_heads_theta(H,H1),
   remove_redundant(H1,H1,EL,H2),
   rev(H2,H3),
   remove_redundant(H3,H3,EL,H4).


%************************************************************************
%*
%* predicate: sort_heads_theta/2
%*
%* syntax: sort_heads_theta(+Heads,-Heads)
%*
%* args: Heads.. list of clause heads
%*
%* description: sorts Heads descendingly according to theta-subsumption
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

sort_heads_theta([],[]).
sort_heads_theta([H|R],L):-
   sort_heads_theta(R,L1),
   insert_heads_theta(L1,H,L).

insert_heads_theta([H1|R],H,[H1|R1]):-
   subsumes_chk(H1,H),!,
   insert_heads_theta(R,H,R1).
insert_heads_theta(L,H,[H|L]).


%************************************************************************
%*
%* predicate: remove_redundant/4
%*
%* syntax: remove_redundant(+Heads,+Heads,+Examples,-Heads)
%*
%* args: Heads.. list of clause heads
%*       Examples... positive examples to be covered by Heads
%*
%* description: removes redundant heads from the list Heads
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

remove_redundant([H|R],HL,EL,HL1):-
    remove_v([H],HL,HL0),
    (   heads_cover(HL0,EL) ->
        remove_redundant(R,HL0,EL,HL1)
    ;   remove_redundant(R,HL,EL,HL1)
    ).
remove_redundant([],HL,_,HL).


%************************************************************************
%*
%* predicate: heads_cover/2
%*
%* syntax: heads_cover(+Heads,+Examples)
%*
%* args: Heads.. list of clause heads
%*       Examples... positive examples to be covered by Heads
%*
%* description: tests whether the heads in Heads cover all examples
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

heads_cover(_,[]).
heads_cover([H|R],E):-
   split_examples(E,H,_,E1),
   heads_cover(R,E1).
