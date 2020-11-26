/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(simple, []).

a(A):-			%direct meta-argument
    call(A).
    
    
b(B):-			%direct unification
    A=B,
    call(A).
    
c(C):-			%some other stuff happens
    a,
    b,
    call(C).
  
d(D):-			%some other stuff happens with or
    a;
    call(D).
    
e(E):-			%meta call in meta argument
    call(call(E)).
    
f(F):-			%again other stuff happens but with non-atoms
    blub(A),
    bla(A),
    call(F).
    
g(G):-			%stuff happens around (not only before the meta-predicate-call
    a,
    call(G),
    b.
 
h(H):-			%indirect unification
    h(H, b) = h(B, _X),
    call(B).
    
h2(H,X):-		%X is also a meta-argument -> h2(0,0)
    bla(H, H) = bla(X, B),
    call(B).    

i(H,X):-		%another arguement X which is not a meta-argument -> i(0,?)
    bla(H, b) = bla(B, X),
    call(B).
    
j(J):-			%call to user-defined meta-predicate
    d(J).
    
k(K):-			%meta-predicate-call in true-branch of a decision
    (	bla(K,a) = bla(D,a)
    ->	call(D)
    ;	fail
    ).
    
:-meta_predicate(l(0,0,?)).
l(L,M,C):-	%(only L not M is meta-argument, even if they would be aliased in the condition)
    (	bla(M,C) = bla(L,a) 	%meta-predicate-call in else-branch of decision
	->	a
	;	call(L)
	).
m(File,Functor,Arity):-    
    file_name_extension(_,Ext,File),
    prolog_file_type(Ext,prolog),
    !,
    functor(Term,Functor,Arity),
    arg(1,Term,File),
    (	catch(call(Term),_,true)
    ;	true
    ).
		
	
z(Z):-			%non_meta
	Z = 4.
	
z1(Z1):-		%non_meta (parameter is only parameter of meta-call)	
	call(bla(Z1)).
	
z2(Z2):-		%non_meta (parameter is only parameter of meta-call bound via term manipulation)
    F =.. [bla,Z2],
    call(F).
    
z3(Z3):-		%non_meta (parameter is only parameter of meta-call combined with unification)
    bla(Z3) = Y,
    call(Y).
    
z4(z3) :- 		%non_meta (paramter is only parameter of meta-call with some term manipulation)
	functor(z3,F, A),
    assert(aT(F, A)).
    


