/* logic compilation of Definite Clause Translation Grammar rules */

:- op(650,yfx,^^).
:- op(601,xfy,:).
:- op(1150,xfx,::=).
:- op(1175,xfx,<:>).
:- op(1150,xfx,::-).


/*
   The form of a rule is:

   LP ::= RP <:> Args ::- Sem

*/

translate_rule((LP::=[]<:>Sem),H) :- !, 
   t_lp(LP,[],S,S,Sem,H).

translate_rule((LP::=[]),H) :- !, t_lp(LP,[],S,S,[],H).

translate_rule((LP::=RP<:>Sem),(H:-B)):- !,
   t_rp(RP,[],StL,S,SR,B1),
   reverse(StL,RStL),
   t_lp(LP,RStL,S,SR,Sem,H),
   tidy(B1,B).

translate_rule((LP::=RP),(H:-B)):-
   translate_rule((LP::=RP<:>[]),(H:-B)).

t_lp((LP,List),StL,S,SR,Sem,H) :-
   append(List,SR,List2),
   prod_number(Number),
   assert_semantic_rule(Number,LP,StL,Sem),
   add_extra_args([node(LP,StL,Number),S,List2],LP,H).

t_lp(LP,StL,S,SR,Sem,H) :- 
   prod_number(Number),
   assert_semantic_rule(Number,LP,StL,Sem),
   add_extra_args([node(LP,StL,Number),S,SR],LP,H).

t_rp(!,St,St,S,S,!) :- !.

t_rp([],St,[[]|St],S,S1,S=S1) :- !.

t_rp([X],St,[[NX]|St],S,SR,c(S,X,SR)) :-
   char(X,NX).

t_rp([X],St,[[X]|St],S,SR,c(S,X,SR)) :- !.

t_rp([X|R],St,[[NX|NR]|St],S,SR,(c(S,X,SR1),RB)) :- 
   char(X,NX),
   t_rp(R,St,[NR|St],SR1,SR,RB).

t_rp([X|R],St,[[X|R]|St],S,SR,(c(S,X,SR1),RB)) :- !, 
   t_rp(R,St,[R|St],SR1,SR,RB).

t_rp({T},St,St,S,S,T) :- !.

t_rp((T,R),St,StR,S,SR,(Tt,Rt)) :- !,
   t_rp(T,St,St1,S,SR1,Tt),
   t_rp(R,St1,StR,SR1,SR,Rt).

t_rp(T^^N,St,[N|St],S,SR,Tt) :- add_extra_args([N,S,SR],T,Tt).

t_rp(T,St,[St1|St],S,SR,Tt) :- add_extra_args([St1,S,SR],T,Tt).

add_extra_args(L,T,T1) :-
   T=..Tl,
   append(Tl,L,Tl1),
   T1=..Tl1.
 
% append([],L,L) :- !.
% append([X|R],L,[X|R1]) :- append(R,L,R1).

% reverse(X,RX) :- rev1(X,[],RX).
% 
% rev1([],R,R) :- !.
% 
% rev1([X|Y],Z,R) :- rev1(Y,[X|Z],R).

tidy(((P1,P2),P3),Q) :- 
   tidy((P1,(P2,P3)),Q).

tidy((P1,P2),(Q1,Q2)) :- !,
   tidy(P1,Q1),
   tidy(P2,Q2).

tidy(A,A) :- !.

char(X,NX) :-
   integer(X), X < 256, !, name(NX,[X]).

c([X|S],X,S). /* defined as a system predicate */

 
% :- asserta(( term_expansion(T,E) :- translate_rule(T,E) , ! )).
% :- asserta(( term_expansion(T,E) :- process_rule(T,E) , ! )).

grammar(File) :-
   seeing(Old),
   see(File),
   consume,
   seen,
   see(Old).

consume :-
   repeat,
      read(X),
      check_it(X).

check_it(X) :- X = end_of_file, !. 
check_it(X) :- process(X), fail.

process(Grammar) :- (Grammar = (H<:>T); Grammar = (H::=T)), !,
   translate_rule(Grammar,Clause),
   assertz(Clause), !.

process(( :- G)) :- !,         % Execute a command
   G.

process((P :- Q)) :-  !,       % Store a normal clause
   assertz((P :- Q)).

process(P) :-                  % Store a unit clause
   assertz(P).

/*
process_rule(T,E) :-
   translate_rule(T,E),
   !,
   assert(T).
*/

node(NonTerminal,Trees,Index)^^Args :-
   semantic_rule(Index,Args,NonTerminal,Trees). % fast?

/*
get_sem(NonTerminal,Trees,Index,Head) :-
   semantic_rule(Index,NonTerminal,Trees,Head).
*/

prod_number(X) :-
   retract(rule_number(X)),
   X1 is X + 1,
   assert(rule_number(X1)).

:- dynamic rule_number/1.

rule_number(0).

assert_semantic_rule(Number,LP,StL,(Rule,Rules)) :- !, 
   (Rule = (Head ::- Body); Head = Rule, Body = true),
   assert((semantic_rule(Number,Head,LP,StL) :- !,Body)),
   assert_semantic_rule(Number,LP,StL,Rules).

assert_semantic_rule(Number,LP,StL,Rule) :-
   (Rule = (Head ::- Body); Head = Rule, Body = true),
   assert((semantic_rule(Number,Head,LP,StL) :- !,Body)).


