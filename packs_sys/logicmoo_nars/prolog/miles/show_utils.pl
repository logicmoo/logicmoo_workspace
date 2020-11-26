% MODULE show_utils EXPORTS
:- module( show_utils, 
	 [ show_kb/0,                   % Show all clauses
           show_ex/0,                   % Show all examples
	   show_clause/1,               % Show one clause
           show_kb_clause/4,
	   show_clauses/1,
	   show_names/0,                % Show all names of predicates
	   show_kb_part/2,              % Show some clauses
           show_kb_types/0,             % displays all available types
           show_type_restrictions/0,
	   print_kb/1,                  % Print all clauses to UNIX-file
           show_heads /0,
           show_bodies/0,
           pp_clause/1,
	   write_list/1]).


% IMPORTS
:- use_module(home(bu_basics),
              [head/3, body/3]).
:- use_module(home(div_utils),
                  [make_unique/2,mysetof/3]).
:- use_module(home(kb),
              [get_clause/5,get_example/3]).
:- use_module(home(argument_types),
                  [type_restriction/2]).
:- use_module_if_exists(library(basics),
              [nonmember/2,member/2]).
% METAPREDICATES
% none


%***********************************************************************
%*	
%* module: show_utils.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92	
%*									
%* changed:								
%*									
%* description:	various diplays predicates
%*		
%* see also:								
%*									
%***********************************************************************



%***********************************************************************
%*									
%* predicate:	show_kb/0							
%*									
%* syntax:	-							
%*									
%* args:	none							
%*									
%* description:	displays all clauses in kb asserted by known
%*
%***********************************************************************

show_kb :- get_clause(I,H,B,_,O),      
           show_kb_clause(I,H,B,O),
           fail.
show_kb :- !.


%***********************************************************************
%*									
%* predicate:	 print_kb/1
%*									
%* syntax:	 print_kb(+ File)						
%*									
%* args: File: name of a file							
%*									
%* description:	prints kb to a file	
%*
%***********************************************************************

print_kb(Filename) :- tell(Filename),
                      show_kb,
                      told.



%***********************************************************************
%*									
%* predicate:	 show_clause/1
%*									
%* syntax:	show_clause(+ ID)							
%*									
%* args:	ID: the ID of a clause							
%*									
%* description:	displays the clause stored with ID
%*									
%***********************************************************************

show_clause(I) :- get_clause(I,H,B,_,O),
                  write(I),write(': '),
                  write('(by '),write(O),write(')'),
                  portray_clause((H:-B)),!.


%***********************************************************************
%*									
%* predicate: 	show_clauses/1							
%*									
%* syntax:      show_clauses(+List_of_clauseIDs)
%*									
%* args:	+List_of_clauseIDs: a list of clause IDs
%*									
%* description:	displays each clause with ID in List_of_clauseIDs
%*									
%***********************************************************************

show_clauses([]) :- !.
show_clauses([Id1|Rest]) :- show_clause(Id1), nl, show_clauses(Rest).


%************************************************************************
%*
%* predicate:    show_kb_clause/4
%*
%* syntax:       show_kb_clause(+I,+H,+B,+O)
%*
%* args:         I: an ID in KB
%*               H: the head of a clause
%*               B: the body of a clause
%*               O: the label of a clause
%*
%* description: displays a clause H:-B, used for xm 
%*
%************************************************************************

show_kb_clause(I,H,B,O):-
	  format('~N~n% Clause ~w (label ~w)~n',[I, O]),
          \+ \+ ((guess_varnames((H:-B)),
                  implode_varnames((H:-B)),
                  portray_clause((H:-B)))), !.


%***********************************************************************
%*									
%* predicate:show_names/0								
%*									
%* syntax:								
%*									
%* args:	none							
%*									
%* description:	lists all predicate names available in the kb
%*									
%***********************************************************************

show_names :- show_names([]).
show_names(Accu) :-
	get_clause(_,H,_,_,_),
	functor(H,Name,_),
	nonmember(Name,Accu),
	format("~10|~a~n",Name), !,
	show_names([Name|Accu]).
show_names(_) :- !.

%************************************************************************
%*
%* predicate: show_kb_part/2
%*
%* syntax:    show_kb_part(+From,+To) 
%*
%* args:      From: the min ID of KB entries to be shown
%*            To:   the max ID of KB entries to  be shown
%*
%* description: shows all clauses with From <= ID <= To
%*									
%************************************************************************

show_kb_part(From,To) :- 
   mysetof(I,H^B^S^O^(get_clause(I,H,B,S,O),
                      From =< I,To >= I),IDL),
   show_clauses(IDL).


%***********************************************************************
%*									
%* predicate:	show_ex/0
%*									
%* syntax:								
%*									
%* args:	none							
%*									
%* description: displays all examples in kb
%*									
%***********************************************************************

show_ex :- get_example(I,F,C),
           write('Example '),write(I),write(': '),
           write(F), write(' -> '), write(C),nl,
           fail.
show_ex :- !.


%***********************************************************************
%*									
%* predicate:  show_heads/0, show_bodies/0						
%*									
%* syntax:								
%*									
%* args:								
%*									
%* description:	displays all intermediate heads/bodies stored by absorption,
%*		saturation,... in the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

show_heads:- head(L,Flag,C), write( head(L,Flag,C)), nl, fail.
show_heads.

show_bodies:- body(L,Flag,C), write( body(L,Flag,C)), nl, fail.
show_bodies. 


%***********************************************************************
%*									
%* predicate:	pp_clause/1							
%*									
%* syntax: pp_clause(+CL)								
%*							
%* args: CL .. clause in list notation
%*									
%* description: displays clause in list notation
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

pp_clause([]).
pp_clause([H:S|Rest]):- write(H:S),nl,pp_clause(Rest).


%***********************************************************************
%*									
%* predicate:	write_list/1							
%*									
%* syntax:	write_list(+List)							
%*									
%* args:	List: a list
%*									
%* description:	displays copy of a  list after instantiating all terms 
%               within the copy by $Var(N)
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

write_list([]).
write_list([X0|R]):-  
   copy_term(X0,X),      
   numbervars(X,0,_),write(X),nl,write_list(R).
write_list(PS:_:_):- write_list(PS).


%***********************************************************************
%*									
%* predicate:   show_kb_types/0
%*									
%* syntax:	
%*									
%* args:	
%*									
%* description:	displays definitions of all types in the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%***********************************************************************

show_kb_types:-
   findall(T:Def,(get_clause(_,H,_,_,type),H =.. [T|_],
                  findall((H1:-B1),(get_clause(_,H1,B1,_,type),H1 =.. [T|_]),Def)),
                 Tlist0),
   make_unique(Tlist0,Tlist),
   nl, write('The following types are defined in the knowledge base:'),nl,
   show_kb_types([atom:[],number:[],atomic:[]|Tlist]).

show_kb_types([]).
show_kb_types([T:Def|R]):-
   nl, write(T),write(':'),nl,
   show_kb_t(Def),
   show_kb_types(R).

show_kb_t([]).
show_kb_t([C|R]):-
   numbervars(C,0,_),
   write(C),
   nl,
   show_kb_t(R).


%***********************************************************************
%*									
%* predicate:   show_type_restrictions/0
%*									
%* syntax:	
%*									
%* args:	
%*									
%* description:	displays all type restrictions in the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

show_type_restrictions:-
   type_restriction(M,A),
   numbervars((M,A),0,_),
   nl,write('type_restriction( '),write(M), write(', '), write(A), write(' )'),
   fail.
show_type_restrictions.