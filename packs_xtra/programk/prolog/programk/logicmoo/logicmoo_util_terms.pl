% ===================================================================
% File 'logicmoo_util_strings.pl'
% Purpose: Common Logicmoo library Functions for Strings
% Maintainers: Douglas Miles/Annie Ogborn/Kino Coursey
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_strings.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
:-module(logicmoo_util_terms,[
         weak_nd_subst/4,
         subst/4
         /*

         %% transparent defaultAssertMt/1,
         assertIfNew/1,
         list_to_conj/2,

         */
   ]).

:-ensure_loaded('../logicmoo/logicmoo_util_library.pl').
:-use_module(library('logicmoo/logicmoo_util_library.pl')).
:-ensure_loaded(library('logicmoo/logicmoo_util_bugger.pl')).

% ===================================================================
% Substitution based on ==
% ===================================================================

% Usage: subst(+Fml,+X,+Sk,?FmlSk)

subst(A,B,C,D):- 
      catch(notrace(nd_subst(A,B,C,D)),_,fail),!.
subst(A,_B,_C,A).

nd_subst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
nd_subst(  P, X,Sk, P1 ) :- functor(P,_,N),nd_subst1( X, Sk, P, N, P1 ).

nd_subst1( _,  _, P, 0, P  ).
nd_subst1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], 
            nd_subst2( X, Sk, Args, ArgS ),
            nd_subst2( X, Sk, [F], [FS] ),  
            P1 =.. [FS|ArgS].

nd_subst2( _,  _, [], [] ).
nd_subst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [Ap|AS] ) :- nd_subst( A,X,Sk,Ap ),nd_subst2( X, Sk, As, AS).
nd_subst2( _X, _Sk, L, L ).




weak_nd_subst(  Var, VarS,SUB,SUB ) :- Var=VarS,!.
weak_nd_subst(        P, X,Sk,        P1 ) :- functor(P,_,N),weak_nd_subst1( X, Sk, P, N, P1 ).

weak_nd_subst1( _,  _, P, 0, P  ).

weak_nd_subst1( X, Sk, P, N, P1 ) :- N > 0, P =.. [F|Args], weak_nd_subst2( X, Sk, Args, ArgS ),
            weak_nd_subst2( X, Sk, [F], [FS] ),
            P1 =.. [FS|ArgS].

weak_nd_subst2( _,  _, [], [] ).
weak_nd_subst2( X, Sk, [A|As], [Sk|AS] ) :- X = A, !, weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( X, Sk, [A|As], [Ap|AS] ) :- weak_nd_subst( A,X,Sk,Ap ),weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( _X, _Sk, L, L ).

