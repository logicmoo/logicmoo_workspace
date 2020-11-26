/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Günter Kniesel, Tobias Rho (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

% Author: Günter Kniesel & Tobias Rho
% Date:   5. April 2012
% Licence: EPL

%% This module implements a crossreference analysis of the loaded code 
%  (using clause/3 and predicate_property/2). It 
%   - is meta-call-aware (= meta calls are properly handled)  
%   - has clause level granulrity  (= each called CLAUSE is identified)
%
%  USE: 
%
%  Invoke 
%     ?- gen_call_graph.
%  Then use the clauses generated in this module:
%     ?- xref(CallingClauseRef,CallLiteral,ProgramPoint,CalledClauseRef).
% 
%  For many applications call resolution at clause level granularity 
%  might be too detailed. Predicate-level granularity can be easily
%  obtained by calling the internal predicate get_clause/2 via once/1.
%  and asserting the 
%        xref(CallingRef,Literal,ProgramPoint,CalledRef)
%  just for the first referenced clause.  

:- module( pdt_xref_v3, 
         [ gen_call_graph/0
         , xref/4            % (CallingRef,Literal,ProgramPoint,CalledRef)
         ]
         ).
 
% Folgendes Prädikat nutzen wir gar nicht mehr.
% Eigentlich müssten wir es in der vorletzten Klausel
% von process_body__/3 aufrufen. Wir tun es aber nicht.
resolve_call(Call,DeclaringModule,ClauseRef) :- 
	strip_module(Call,Module,Literal),
	functor(Literal,Name,Arity),
	defined_in(Module,Name,Arity,DeclaringModule),
	clause(DeclaringModule:Literal,_Body,ClauseRef).
% Das heisst: Entweder ist dort ein Fehler, oder es 
% funktioniert auch ohne den Aufruf von defined_in/3 
% also so:
%
%resolve_call(Call,DeclaringModule,ClauseRef) :- 
%	strip_module(Call,Module,Literal),
%	%defined_in(Module,Name,Arity,DeclaringModule),
%	clause(Module:Literal,_Body,ClauseRef).
%
%Das ist tatsächlich der Fall:	
%
%?- clause(sub:c(X),_Body,ClauseRef).
%X = 1,
%_Body = true,
%ClauseRef = <clause>(08A640F0) ;
%X = 2,
%_Body = true,
%ClauseRef = <clause>(08A64080) ;
%X = 3,
%_Body = true,
%ClauseRef = <clause>(08A640B8).
% FAZIT: Wenn man nur die referenzierten Klauseln will, reicht clause/3.
% Wenn man hingegen auch das Modul wissen will, aus dem die Klauseln 
% stammen braucht man defined_in. Das (bzw. resolve_call)
% ist also noch einzbauen in unsere Implementierung. 
 
 
 
  	
gen_call_graph :- 
   retractall(xref(_,_,_,_)),  
   defined_in_module(Module,Name,Arity),
      functor(Head,Name,Arity),
      \+ predicate_property(Module:Head, foreign),
      clause_xref(Module,Head,Xref),
      assert(Xref),
   fail.
gen_call_graph.
 
%% clause_xref(+Module,+Head,?Xref) is nondet.
%
clause_xref(Module,Head,Xref):-
   clause(Module:Head,Body,CallingRef),
   process_body(Module,Body,Arc),
   Arc  = arc(            Literal,ProgramPoint,CalledRef),
   Xref = xref(CallingRef,Literal,ProgramPoint,CalledRef).

process_body(Module,Body,Arc) :-
	nb_setval(program_point, 0),
	process_body__(Module,Body,Arc).

%process_body__(Module,(A,B),Arc):-
%	!,
%	( process_body__(Module,A,Arc) 
%	; process_body__(Module,B,Arc)
%    ).
%	
%process_body__(Module,(A;B),Arc):-
%    !,
%	( process_body__(Module,A,Arc) 
%	; process_body__(Module,B,Arc)
%    ).
%   
process_body__(Module,Term,_Arc):-
	( var(Module) 
	; var(Term)
	),
	!,
	fail.


process_body__(_,Module:Term,Arc):-
	!,
	process_body__(Module,Term,Arc).
	 
process_body__(Module,Meta,Arc):-
	is_meta_call(Module:Meta),
	!,
	peel_meta(Module:Meta,Literal),
	process_body__(Module,Literal,Arc).
	

process_body__(Module,Literal,Arc):-
	callable(Literal),
	!,
	inc_program_point,
	get_program_point(Pt),
	get_clause(Module:Literal,Ref),      % refer to each called clause
%	once(get_clause(Module:Literal,Ref),  % refer just to first clause
	Arc=arc(Module:Literal, Pt, Ref).
	

process_body__(_,Literal, arc(Literal, -1, failed)).


is_meta_call(Call):-
	predicate_property(Call, meta_predicate(_)).
	

get_clause(_:'$_variable'(Term,Number),variable(Term,Number)):-
    !.
get_clause(Module:Literal,Ref):-
    predicate_property(Module:Literal, foreign),
    !,
	Ref=foreign.
get_clause(Module:Literal,Ref):-
	\+ clause(Module:Literal,_,_),
	!,
	Ref=undefined.
get_clause(Module:Literal,Ref):-
	clause(Module:Literal,_Body,Ref).


%% peel_meta(+Meta,?Literal) is nondet.
%
peel_meta(Module:Meta,Literal):-
	predicate_property(Module:Meta, meta_predicate(Pattern)),
	inc_program_point,
	meta_argument(Meta,Pattern,Literal).

%% meta_argument(+Meta,+Pattern,?Literal) is nondet.
%
meta_argument(Meta,Pattern,Literal):-
	arg(Nr,Pattern,MetaIndicator),
	number(MetaIndicator),
	arg(Nr,Meta,Argument),
	add_arguments(Argument,MetaIndicator,Literal).
	
%peel_meta(Literal,Literal).


add_arguments(Term,Number,Result) :- 
    var(Term),
    !,
    Result = '$_variable'(Term,Number).
add_arguments(Term,0,Term) :- !.
add_arguments(Term,Number,BiggerTerm) :-
	callable(Term),
	Number > 0,
	Term =.. List,
	length(Postfix,Number),
	append(List,Postfix,BiggerList),
	BiggerTerm =.. BiggerList.


inc_program_point :- 
	nb_getval(program_point, Value),
	Next is Value+1,
	nb_setval(program_point, Next).
	
get_program_point(Value):- 	
	nb_getval(program_point, Value).

	

